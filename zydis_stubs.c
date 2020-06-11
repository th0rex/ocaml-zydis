#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <string.h>

#include <Zydis/Zydis.h>

CAMLprim int64_t zydis_get_version() {
  return ZydisGetVersion();
}

value zydis_get_version_byte(value unit) {
  CAMLparam1 (unit);
  CAMLreturn (caml_copy_int64(zydis_get_version()));
}

value zydis_is_feature_enabled_byte(value feature) {
  CAMLparam1 (feature);
  CAMLreturn (Val_bool(ZydisIsFeatureEnabled(Unsigned_long_val(feature)) == ZYAN_STATUS_TRUE));
}

static struct custom_operations zydis_decoder_ops = {
  .identifier                                     = "ZydisDecoder",
  .finalize                                       = NULL,
  .compare                                        = NULL,
  .hash                                           = NULL,
  .serialize                                      = NULL,
  .deserialize                                    = NULL,
  .compare_ext                                    = NULL,
  .fixed_length                                   = NULL,
};

value zydis_decoder_init(value mode, value width) {
  CAMLparam2 (mode, width);
  value decoder = caml_alloc_custom(&zydis_decoder_ops, sizeof(ZydisDecoder), 0, 1);
  // TODO: Technically should check result here
  ZydisDecoderInit(Data_custom_val(decoder), Unsigned_long_val(mode), Unsigned_long_val(width));
  CAMLreturn (decoder);
}

value zydis_decoder_enable(value decoder, value mode, value enabled) {
  CAMLparam3 (decoder, mode, enabled);
  ZydisDecoderEnableMode(
    Data_custom_val(decoder),
    Unsigned_long_val(mode),
    Bool_val(enabled));
  CAMLreturn (Val_unit);
}

static struct custom_operations zydis_insn_ops = {
  .identifier                                  = "ZydisDecodedInstruction",
  .finalize                                    = NULL,
  .compare                                     = NULL,
  .hash                                        = NULL,
  .serialize                                   = NULL,
  .deserialize                                 = NULL,
  .compare_ext                                 = NULL,
  .fixed_length                                = NULL,
};

value make_some(value v) {
  CAMLparam1 (v);
  value some = caml_alloc_small(1, 0);
  Store_field(some, 0, v);
  CAMLreturn (some);
}

void Store_optional_field(value target, size_t idx, size_t val) {
  CAMLparam1 (target);

  if (val == 0) {
    Store_field(target, idx, Val_unit);
  } else {
    Store_field(target, idx, make_some(Val_long(val - 1)));
  }

  CAMLreturn0;
}

static value wrap_decoded_operand(const ZydisDecodedOperand* z_op) {
  CAMLparam0 ();
  CAMLlocal3 (op, kind, imm);

  op = caml_alloc_tuple(9);
  Store_field(op, 0, Val_long(z_op->id));
  Store_field(op, 1, Val_long(z_op->visibility));
  Store_field(op, 2, Val_long(z_op->actions));
  Store_field(op, 3, Val_long(z_op->encoding));
  Store_field(op, 4, Val_long(z_op->size));
  Store_field(op, 5, Val_long(z_op->element_type));
  Store_field(op, 6, Val_long(z_op->element_size));
  Store_field(op, 7, Val_long(z_op->element_count));

  // Create the appropiate kind.
  switch (z_op->type) {
  case ZYDIS_OPERAND_TYPE_UNUSED:
    kind              = Val_long(0);
    break;
  case ZYDIS_OPERAND_TYPE_REGISTER:
    kind              = caml_alloc(1, 0);
    Store_field(kind, 0, Val_long(z_op->reg.value - 1));
    break;
  case ZYDIS_OPERAND_TYPE_MEMORY:
    kind              = caml_alloc(6, 1);
    Store_field(kind, 0, Val_long(z_op->mem.type));
    Store_optional_field(kind, 1, z_op->mem.segment);
    Store_optional_field(kind, 2, z_op->mem.base);
    Store_optional_field(kind, 3, z_op->mem.index);
    Store_field(kind, 4, Val_long(z_op->mem.scale));
    if (z_op->mem.disp.has_displacement) {
      Store_field(kind, 5, make_some(caml_copy_int64(z_op->mem.disp.value)));
    } else {
      Store_field(kind, 5, Val_unit);
    }
    break;
  case ZYDIS_OPERAND_TYPE_POINTER:
    kind = caml_alloc(2, 2);
    Store_field(kind, 0, Val_long(z_op->ptr.segment));
    Store_field(kind, 1, caml_copy_int32(z_op->ptr.offset));
    break;
  case ZYDIS_OPERAND_TYPE_IMMEDIATE:
    kind = caml_alloc(2, 3);
    if (z_op->imm.is_relative) {
      Store_field(kind, 0, Val_long(1));
    } else {
      Store_field(kind, 0, Val_long(0));
    }
    if (z_op->imm.is_signed) {
      imm = caml_alloc(1, 0);
    } else {
      imm = caml_alloc(1, 1);
    }
    Store_field(imm, 0, caml_copy_int64(z_op->imm.value.s));
    Store_field(kind, 1, imm);
    break;
  }

  Store_field(op, 8, kind);

  CAMLreturn (op);
}

static value zydis_decoder_decode_internal(value decoder, value bytes, const size_t offs) {
  CAMLparam2 (decoder, bytes);
  CAMLlocal5 (raw_insn, insn, meta, operands, flags);
  CAMLlocal1 (tmp_flag);

  const size_t len = caml_string_length(bytes);
  if (offs >= len) {
    // TODO: Use Val_none, caml_alloc_some, etc once a stable ocaml version
    // with them gets released.
    return Val_unit;
  }

  // TODO: If we allocate on the stack we save gc pressure in case decoding fails, i.e.
  // if we would only alloc + memcpy in case of success.
  raw_insn                        = caml_alloc_custom(&zydis_insn_ops, sizeof(ZydisDecodedInstruction), 0, 1);
  ZydisDecodedInstruction* z_insn = Data_custom_val(raw_insn);

  ZyanStatus res = ZydisDecoderDecodeBuffer(
    Data_custom_val(decoder),
    String_val(bytes) + offs,
    len - offs,
    z_insn
    );
  if (res != ZYAN_STATUS_SUCCESS) {
    return Val_unit;
  }

  insn = caml_alloc_tuple(16);
  // Store some straight forward things
  Store_field(insn,  0, Val_long(z_insn->machine_mode));
  Store_field(insn,  1, Val_long(z_insn->mnemonic));
  Store_field(insn,  2, Val_long(z_insn->length));
  Store_field(insn,  3, Val_long(z_insn->encoding));
  Store_field(insn,  4, Val_long(z_insn->opcode_map));
  Store_field(insn,  5, Val_long(z_insn->opcode));
  Store_field(insn,  6, Val_long(z_insn->stack_width));
  Store_field(insn,  7, Val_long(z_insn->operand_width));
  Store_field(insn,  8, Val_long(z_insn->address_width));

  // Create operands
  operands = caml_alloc_tuple(z_insn->operand_count);
  for (unsigned i = 0; i < z_insn->operand_count; i++) {
    Store_field(operands, i, wrap_decoded_operand(&z_insn->operands[i]));
  }
  Store_field(insn,  9, operands);

  // Copy attributes
  Store_field(insn, 10, caml_copy_int64(z_insn->attributes));

  // Create flag array
  unsigned flag_count = 0;
  for (unsigned i = 0; i < ZYDIS_CPUFLAG_MAX_VALUE + 1; i++) {
    if (z_insn->accessed_flags[i].action != 0) {
      flag_count += 1;
    }
  }

  flags = caml_alloc_tuple(flag_count);

  for (unsigned i = 0, j = 0; i < ZYDIS_CPUFLAG_MAX_VALUE + 1; i++) {
    if (z_insn->accessed_flags[i].action != 0) {
      tmp_flag = caml_alloc_tuple(2);
      Store_field(tmp_flag, 0, Val_long(i));
      Store_field(tmp_flag, 1, Val_long(z_insn->accessed_flags[i].action - 1));

      Store_field(flags, j++, tmp_flag);
    }
  }
  Store_field(insn, 11, flags);

  // For now we leave out the avx info.
  Store_field(insn, 12, Val_unit);

  // Fill in the meta info.
  meta = caml_alloc_tuple(5);
  Store_field(meta, 0, Val_long(z_insn->meta.category));
  Store_field(meta, 1, Val_long(z_insn->meta.isa_set));
  Store_field(meta, 2, Val_long(z_insn->meta.isa_ext));
  Store_optional_field(meta, 3, z_insn->meta.branch_type);
  Store_optional_field(meta, 4, z_insn->meta.exception_class);

  Store_field(insn, 13, meta);

  // We also leave out the raw info.
  Store_field(insn, 14, Val_unit);
  // Store the raw instruction.
  Store_field(insn, 15, raw_insn);

  value some = caml_alloc_small(1, 0);
  Store_field(some, 0, insn);
  CAMLreturn (some);
}

value zydis_decoder_decode_native(value decoder, value bytes, value offset) {
  CAMLparam3 (decoder, bytes, offset);
  CAMLreturn (zydis_decoder_decode_internal(decoder, bytes, Nativeint_val(offset)));
}

value zydis_decoder_decode_long(value decoder, value bytes, value offset) {
  CAMLparam3 (decoder, bytes, offset);
  CAMLreturn (zydis_decoder_decode_internal(decoder, bytes, Unsigned_long_val(offset)));
}
