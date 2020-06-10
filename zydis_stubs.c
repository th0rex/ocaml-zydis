#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>

#include <Zydis/Zydis.h>

CAMLprim int64_t zydis_get_version() {
  return ZydisGetVersion();
}

CAMLprim value zydis_get_version_byte(value unit) {
  return caml_copy_int64(zydis_get_version());
}

CAMLprim value zydis_is_feature_enabled_byte(value feature) {
  return Val_bool(ZydisIsFeatureEnabled(Unsigned_long_val(feature)) == ZYAN_STATUS_TRUE);
}

static struct custom_operations zydis_decoder_ops = {
	.identifier																			= "ZydisDecoder",
	.finalize																				= NULL,
	.compare																				= NULL,
	.hash																						= NULL,
	.serialize																			= NULL,
	.deserialize																		= NULL,
	.compare_ext																		= NULL,
	.fixed_length																		= NULL,
};

CAMLprim value zydis_decoder_init_byte(value mode, value width) {
  value	decoder = caml_alloc_custom(&zydis_decoder_ops, sizeof(ZydisDecoder), 0, 1);
	ZydisDecoderInit(Data_custom_val(decoder), Unsigned_long_val(mode), Unsigned_long_val(width));
	return decoder;
}

CAMLprim value zydis_decoder_enable_byte(value decoder, value mode, value enabled) {
	ZydisDecoderEnableMode(
		Data_custom_val(decoder),
		Unsigned_long_val(mode),
		Bool_val(enabled));
	return Val_unit;
}

CAMLprim value zydis_decoder_decode_byte(value decoder, value bytes) {
	ZydisDecodedInstruction insn;
	ZydisDecoderDecodeBuffer(
		Data_custom_val(decoder),
		String_val(bytes),
		caml_string_length(bytes),
		&insn);
	// TODO
	return Val_unit;
}
