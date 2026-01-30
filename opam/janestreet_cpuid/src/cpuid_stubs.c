#include "ocaml_utils.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#ifdef __aarch64__

#include <caml/fail.h>

CAMLprim value cpuid_arbitrary_leaf_and_subleaf(void) { return Val_unit; }

CAMLprim value cpuid_maximum_value_and_brand(value v_record) {
  CAMLparam1(v_record);
  CAMLlocal1(v_brand_string);
  v_brand_string = caml_copy_string("arm");
  Store_field(v_record, 1, v_brand_string);
  CAMLreturn(Val_unit);
}

CAMLprim value amd_cpuid_version_and_feature_information(void) {
  caml_invalid_argument("unavailable on arm");
}

CAMLprim value intel_cpuid_version_and_feature_information(void) {
  caml_invalid_argument("unavailable on arm");
}

CAMLprim value cpuid_extended_feature_flags_subleaf0(void) {
  caml_invalid_argument("unavailable on arm");
}

#else

#include <cpuid.h>
#include <string.h>

// These are the supported levels currently. In the interest of type-sanity in
// OCAML we take each supported "leaf/subleaf" of the actual CPUID command and
// store it into a different kind of record type so that all feature detection
// can be done in OCAML.
//
// All structs must be kept up to date with OCAML records or else bad things.
// All names come from the Intel Software Developer's Manual

#define FIELD(name, width) uint32_t name : width

#define MAXIMUM_VALUE_AND_BRAND_LEAF 0x00
#define VERSION_AND_FEATURE_INFORMATION_LEAF 0x01
#define STRUCTURED_EXTENDED_FEATURE_FLAGS_LEAF 0x07
#define ARCHITECTURAL_PERFORMANCE_MONITORING_LEAF 0x0a

CAMLprim value cpuid_arbitrary_leaf_and_subleaf(value v_eax, value v_ecx) {
  CAMLparam2(v_eax, v_ecx);
  CAMLlocal1(result);

  uint32_t eax, ebx, ecx, edx;
  uint32_t in_eax, in_ecx;

  in_eax = Int_val(v_eax);
  in_ecx = Int_val(v_ecx);

  __cpuid_count(in_eax, in_ecx, eax, ebx, ecx, edx);

  result = caml_alloc_local(4, 0);
  Store_field(result, 0, Val_int(eax));
  Store_field(result, 1, Val_int(ebx));
  Store_field(result, 2, Val_int(ecx));
  Store_field(result, 3, Val_int(edx));

  CAMLreturn(result);
}

CAMLprim value cpuid_maximum_value_and_brand(value v_record) {
  CAMLparam1(v_record);
  CAMLlocal1(v_brand_string);
  // Intel and AMD both switch around their brand labels so I see no issue doing
  // it here.
  union {
    char text[13];
    struct {
      uint32_t ebx;
      uint32_t edx;
      uint32_t ecx;
    } regs;
  } brand;
  uint32_t eax;
  memset(&brand, 0, sizeof(brand));
  __cpuid_count(MAXIMUM_VALUE_AND_BRAND_LEAF, 0, eax, brand.regs.ebx, brand.regs.ecx,
                brand.regs.edx);
  Store_field(v_record, 0, Val_int(eax));
  v_brand_string = caml_copy_string(brand.text);
  Store_field(v_record, 1, v_brand_string);

  CAMLreturn(Val_unit);
}

CAMLprim value amd_cpuid_version_and_feature_information(value v_eax, value v_ebx,
                                                         value v_record) {
  CAMLparam3(v_eax, v_ebx, v_record);
  union {
    struct {
      FIELD(step, 4);
      FIELD(model, 4);
      FIELD(family, 4);
      FIELD(pad1, 4);
      FIELD(emodel, 4);
      FIELD(efamily, 8);
      FIELD(pad2, 4);
    } info;
    uint32_t reg;
  } eax;

  union {
    struct {
      FIELD(brand_index, 8);
      FIELD(clflush_line_size, 8);
      FIELD(max_addressable_logical_processors, 8);
      FIELD(initial_apic_id, 8);
    } info;
    uint32_t reg;
  } ebx;

  uint32_t ecx, edx;

  __cpuid_count(VERSION_AND_FEATURE_INFORMATION_LEAF, 0, eax.reg, ebx.reg, ecx, edx);

  // Store version information for eax
  Store_field(v_eax, 0, Val_int(eax.info.step));
  Store_field(v_eax, 1, Val_int(eax.info.model));
  Store_field(v_eax, 2, Val_int(eax.info.family));
  Store_field(v_eax, 3, Val_int(eax.info.emodel));
  Store_field(v_eax, 4, Val_int(eax.info.efamily));

  // Store the ebx information
  Store_field(v_ebx, 0, Val_int(ebx.info.brand_index));
  Store_field(v_ebx, 1, Val_int(ebx.info.clflush_line_size));
  Store_field(v_ebx, 2, Val_int(ebx.info.max_addressable_logical_processors));
  Store_field(v_ebx, 3, Val_int(ebx.info.initial_apic_id));

  Store_field(v_record, 2, Val_int(ecx));
  Store_field(v_record, 3, Val_int(edx));

  CAMLreturn(Val_unit);
}

CAMLprim value intel_cpuid_version_and_feature_information(value v_eax, value v_ebx,
                                                           value v_record) {
  CAMLparam3(v_eax, v_ebx, v_record);
  union {
    struct {
      FIELD(step, 4);
      FIELD(model, 4);
      FIELD(family, 4);
      FIELD(type, 2);
      FIELD(pad1, 2);
      FIELD(emodel, 4);
      FIELD(efamily, 8);
      FIELD(pad2, 4);
    } info;
    uint32_t reg;
  } eax;

  union {
    struct {
      FIELD(brand_index, 8);
      FIELD(clflush_line_size, 8);
      FIELD(max_addressable_logical_processors, 8);
      FIELD(initial_apic_id, 8);
    } info;
    uint32_t reg;
  } ebx;

  uint32_t ecx, edx;

  __cpuid_count(VERSION_AND_FEATURE_INFORMATION_LEAF, 0, eax.reg, ebx.reg, ecx, edx);

  // Store version information for eax
  Store_field(v_eax, 0, Val_int(eax.info.step));
  Store_field(v_eax, 1, Val_int(eax.info.model));
  Store_field(v_eax, 2, Val_int(eax.info.family));
  Store_field(v_eax, 3, Val_int(eax.info.type));
  Store_field(v_eax, 4, Val_int(eax.info.emodel));
  Store_field(v_eax, 5, Val_int(eax.info.efamily));

  // Store the ebx information
  Store_field(v_ebx, 0, Val_int(ebx.info.brand_index));
  Store_field(v_ebx, 1, Val_int(ebx.info.clflush_line_size));
  Store_field(v_ebx, 2, Val_int(ebx.info.max_addressable_logical_processors));
  Store_field(v_ebx, 3, Val_int(ebx.info.initial_apic_id));

  Store_field(v_record, 2, Val_int(ecx));
  Store_field(v_record, 3, Val_int(edx));

  CAMLreturn(Val_unit);
}

CAMLprim value cpuid_extended_feature_flags_subleaf0(value v_record) {
  CAMLparam1(v_record);

  uint32_t eax, ebx, ecx, edx;

  __cpuid_count(STRUCTURED_EXTENDED_FEATURE_FLAGS_LEAF, 0x0, eax, ebx, ecx, edx);

  Store_field(v_record, 1, Val_int(ebx));
  Store_field(v_record, 2, Val_int(ecx));
  Store_field(v_record, 3, Val_int(edx));

  CAMLreturn(Val_int(eax));
}

#endif
