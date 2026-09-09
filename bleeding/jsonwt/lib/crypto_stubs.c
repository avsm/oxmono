/* SPDX-License-Identifier: ISC */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <openssl/obj_mac.h>
#include <openssl/core_names.h>
#include <openssl/ec.h>
#include <openssl/evp.h>
#include <openssl/params.h>

/* OpenSSL owns curve arithmetic and validation. JOSE carries r || s rather
 * than the DER ECDSA signature consumed by EVP_DigestVerify. */
CAMLprim value jsonwt_verify_es256k(value key, value input, value length,
                                 value signature)
{
    if (caml_string_length(key) != 65 ||
        Long_val(length) < 0 ||
        (uintnat)Long_val(length) > caml_string_length(input) ||
        caml_string_length(signature) != 64)
        return Val_false;

    int ok = 0;
    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
    EVP_PKEY *pkey = NULL;
    EVP_MD_CTX *md = EVP_MD_CTX_new();
    ECDSA_SIG *sig = ECDSA_SIG_new();
    BIGNUM *r = BN_bin2bn((const unsigned char *)String_val(signature),
                         32, NULL);
    BIGNUM *s = BN_bin2bn((const unsigned char *)String_val(signature) + 32,
                         32, NULL);
    unsigned char der[80], *cursor = der;
    char group[] = "secp256k1";
    OSSL_PARAM params[] = {
        OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, group, 0),
        OSSL_PARAM_construct_octet_string(OSSL_PKEY_PARAM_PUB_KEY,
                                          (void *)String_val(key), 65),
        OSSL_PARAM_construct_end()
    };
    if (!ctx || !md || !sig || !r || !s) goto done;
    if (ECDSA_SIG_set0(sig, r, s) != 1) goto done;
    r = s = NULL;
    int len = i2d_ECDSA_SIG(sig, &cursor);
    if (len <= 0 || len > (int)sizeof(der)) goto done;
    if (EVP_PKEY_fromdata_init(ctx) != 1 ||
        EVP_PKEY_fromdata(ctx, &pkey, EVP_PKEY_PUBLIC_KEY, params) != 1)
        goto done;
    if (EVP_DigestVerifyInit(md, NULL, EVP_sha256(), NULL, pkey) != 1)
        goto done;
    ok = EVP_DigestVerify(md, der, len,
                         (const unsigned char *)String_val(input),
                         (size_t)Long_val(length)) == 1;
done:
    BN_free(r);
    BN_free(s);
    ECDSA_SIG_free(sig);
    EVP_MD_CTX_free(md);
    EVP_PKEY_free(pkey);
    EVP_PKEY_CTX_free(ctx);
    return Val_bool(ok);
}

/* Decode only SEC1 compressed or uncompressed points on secp256k1.
 * The OCaml key stores the canonical, validated uncompressed form. */
CAMLprim value jsonwt_secp256k1_point(value input)
{
    CAMLparam1(input);
    CAMLlocal2(point, result);
    mlsize_t length = caml_string_length(input);
    if (!((length == 33 && (Byte_u(input, 0) == 2 || Byte_u(input, 0) == 3)) ||
          (length == 65 && Byte_u(input, 0) == 4)))
        CAMLreturn(Val_none);
    unsigned char bytes[65];
    int ok = 0;
    EC_GROUP *group = EC_GROUP_new_by_curve_name(NID_secp256k1);
    EC_POINT *p = group ? EC_POINT_new(group) : NULL;
    if (!group || !p) goto done;
    if (EC_POINT_oct2point(group, p, (const unsigned char *)String_val(input),
                          length, NULL) != 1 ||
        EC_POINT_is_at_infinity(group, p) != 0 ||
        EC_POINT_is_on_curve(group, p, NULL) != 1)
        goto done;
    ok = EC_POINT_point2oct(group, p, POINT_CONVERSION_UNCOMPRESSED,
                           bytes, sizeof(bytes), NULL) == sizeof(bytes);
done:
    EC_POINT_free(p);
    EC_GROUP_free(group);
    if (!ok) CAMLreturn(Val_none);
    point = caml_alloc_initialized_string(sizeof(bytes), (const char *)bytes);
    result = caml_alloc_small(1, 0);
    Field(result, 0) = point;
    CAMLreturn(result);
}

/* EVP_Q_mac creates a fresh context. The input prefix is borrowed directly. */
CAMLprim value jsonwt_hmac(value bits, value key, value input, value length)
{
    CAMLparam4(bits, key, input, length);
    const char *digest;
    size_t expected;
    switch (Long_val(bits)) {
    case 256: digest = "SHA256"; expected = 32; break;
    case 384: digest = "SHA384"; expected = 48; break;
    case 512: digest = "SHA512"; expected = 64; break;
    default: caml_invalid_argument("JSONWT HMAC algorithm");
    }
    if (Long_val(length) < 0 ||
        (uintnat)Long_val(length) > caml_string_length(input))
        caml_invalid_argument("JSONWT HMAC input length");
    unsigned char output[64];
    size_t size = 0;
    if (!EVP_Q_mac(NULL, "HMAC", NULL, digest, NULL,
                   String_val(key), caml_string_length(key),
                   (const unsigned char *)String_val(input),
                   (size_t)Long_val(length), output, sizeof(output), &size) ||
        size != expected)
        caml_failwith("JSONWT OpenSSL HMAC failure");
    CAMLreturn(caml_alloc_initialized_string(size, (const char *)output));
}
