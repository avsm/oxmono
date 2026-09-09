/* SPDX-License-Identifier: ISC */
#include <caml/mlvalues.h>
#include <openssl/core_names.h>
#include <openssl/ec.h>
#include <openssl/evp.h>
#include <openssl/params.h>

/* OpenSSL owns curve arithmetic and validation. JOSE carries r || s rather
 * than the DER ECDSA signature consumed by EVP_DigestVerify. */
CAMLprim value spindle_verify_es256k(value key, value input, value signature)
{
    if (caml_string_length(key) != 33 ||
        caml_string_length(signature) != 64)
        return Val_false;

    int ok = 0;
    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
    EVP_PKEY *pkey = NULL;
    EVP_MD_CTX *md = EVP_MD_CTX_new();
    ECDSA_SIG *sig = ECDSA_SIG_new();
    BIGNUM *r = BN_bin2bn((const unsigned char *)String_val(signature), 32, NULL);
    BIGNUM *s = BN_bin2bn((const unsigned char *)String_val(signature) + 32,
                         32, NULL);
    unsigned char der[80], *cursor = der;
    char group[] = "secp256k1";
    OSSL_PARAM params[] = {
        OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, group, 0),
        OSSL_PARAM_construct_octet_string(OSSL_PKEY_PARAM_PUB_KEY,
                                          (void *)String_val(key), 33),
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
                         caml_string_length(input)) == 1;
done:
    BN_free(r);
    BN_free(s);
    ECDSA_SIG_free(sig);
    EVP_MD_CTX_free(md);
    EVP_PKEY_free(pkey);
    EVP_PKEY_CTX_free(ctx);
    return Val_bool(ok);
}
