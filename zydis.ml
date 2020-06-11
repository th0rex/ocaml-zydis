[@@@ocaml.warning "-32"]

module Enums = Enums

open Enums

type zydis_decoder
type zydis_instruction

type immediate =
  | Signed of int64
  | Unsigned of int64

type relative =
  | Absolute
  | Relative

module Operand : sig
  type kind =
    | Unused
    | Reg of register
    | Mem of { typ: memory_operand_type;
               segment: register option;
               base: register option;
               index: register option;
               scale: int;
               displacement: int64 option ;
             }
  (* segment, offset *)
    | Ptr of int * int32
    | Imm of relative * immediate

  type t =
    { id: int;
      visibility: operand_visibility;
      actions: operand_action;
      encoding: operand_encoding;
      bit_size: int;
      el_ty: element_type;
      el_size: int;
      el_count: int;
      kind: kind;
    }
end = struct
  type kind =
    | Unused
    | Reg of register
    | Mem of { typ: memory_operand_type;
               segment: register option;
               base: register option;
               index: register option;
               scale: int;
               displacement: int64 option;
             }
    (* segment, offset *)
    | Ptr of int * int32
    | Imm of relative * immediate

  type t =
    { id: int;
      visibility: operand_visibility;
      actions: operand_action;
      encoding: operand_encoding;
      bit_size: int;
      el_ty: element_type;
      el_size: int;
      el_count: int;
      kind: kind;
    }
end

module Attribute : sig
  type t

  (* zydis attributes *)
  val has_modrm : t -> bool
  val has_sib : t -> bool
  val has_rex : t -> bool
  val has_xop : t -> bool
  val has_vex : t -> bool
  val has_evex : t -> bool
  val has_mvex : t -> bool
  val is_relative : t -> bool
  val is_privileged : t -> bool
  val accesses_flags : t -> bool
  (* conditional reads and writes *)
  val reads_cpu_state : t -> bool
  val writes_cpu_state : t -> bool
  val reads_fpu_state : t -> bool
  val writes_fpu_state : t -> bool
  val reads_xmm_state : t -> bool
  val writes_xmm_state : t -> bool
  val accepts_lock : t -> bool
  val accepts_rep : t -> bool
  val accepts_repe : t -> bool
  val accepts_repne : t -> bool
  val accepts_bnd : t -> bool
  val accepts_xacquire : t -> bool
  val accepts_xrelease : t -> bool
  val accepts_hle_without_lock : t -> bool
  val accepts_branch_hints : t -> bool
  val accepts_segment : t -> bool
  val has_lock : t -> bool
  val has_rep : t -> bool
  val has_repe : t -> bool
  val has_repne : t -> bool
  val has_bnd : t -> bool
  val has_xacquire : t -> bool
  val has_xrelease : t -> bool
  val has_branch_not_taken : t -> bool
  val has_branch_taken : t -> bool
  val has_segment_cs : t -> bool
  val has_segment_ss : t -> bool
  val has_segment_ds : t -> bool
  val has_segment_es : t -> bool
  val has_segment_fs : t -> bool
  val has_segment_gs : t -> bool
  val has_operand_size : t -> bool
  val has_address_size : t -> bool

  (* broader categories from zydis *)
  val has_segment : t -> bool

  (* custom broader categories *)
  val has_branch_hint : t -> bool
  val has_any_rep : t -> bool
  val accepts_any_rep : t -> bool
  (* conditionally might access (i.e. read or write) the state *)
  val touches_cpu_state : t -> bool
  val touches_fpu_state : t -> bool
  val touches_xmm_state : t -> bool
end = struct
  type t = int64 (* TODO: Use Int63.t for better perf, but how do we construct one from c? *)

  open Int64

  let has_modrm x = (logand x 0x01L) <> 0L
  let has_sib x = (logand x 0x02L) <> 0L
  let has_rex x = (logand x 0x04L) <> 0L
  let has_xop x = (logand x 0x08L) <> 0L
  let has_vex x = (logand x 0x10L) <> 0L
  let has_evex x = (logand x 0x20L) <> 0L
  let has_mvex x = (logand x 0x40L) <> 0L
  let is_relative x = (logand x 0x80L) <> 0L
  let is_privileged x = (logand x 0x0100L) <> 0L
  let accesses_flags x = (logand x 0x0010_0000_0000L) <> 0L
  let reads_cpu_state x = (logand x 0x0020_0000_0000L) <> 0L
  let writes_cpu_state x = (logand x 0x0040_0000_0000L) <> 0L
  let reads_fpu_state x = (logand x 0x0080_0000_0000L) <> 0L
  let writes_fpu_state x = (logand x 0x0100_0000_0000L) <> 0L
  let reads_xmm_state x = (logand x 0x0200_0000_0000L) <> 0L
  let writes_xmm_state x = (logand x 0x0400_0000_0000L) <> 0L
  let accepts_lock x = (logand x 0x0200L) <> 0L
  let accepts_rep x = (logand x 0x0400L) <> 0L
  let accepts_repe x = (logand x 0x0800L) <> 0L
  let accepts_repne x = (logand x 0x1000L) <> 0L
  let accepts_bnd x = (logand x 0x2000L) <> 0L
  let accepts_xacquire x = (logand x 0x4000L) <> 0L
  let accepts_xrelease x = (logand x 0x8000L) <> 0L
  let accepts_hle_without_lock x = (logand x 0x0001_0000L) <> 0L
  let accepts_branch_hints x = (logand x 0x0002_0000L) <> 0L
  let accepts_segment x = (logand x 0x0004_0000L) <> 0L
  let has_lock x = (logand x 0x0008_0000L) <> 0L
  let has_rep x = (logand x 0x0010_0000L) <> 0L
  let has_repe x = (logand x 0x0020_0000L) <> 0L
  let has_repne x = (logand x 0x0040_0000L) <> 0L
  let has_bnd x = (logand x 0x0080_0000L) <> 0L
  let has_xacquire x = (logand x 0x0100_0000L) <> 0L
  let has_xrelease x = (logand x 0x0200_0000L) <> 0L
  let has_branch_not_taken x = (logand x 0x0400_0000L) <> 0L
  let has_branch_taken x = (logand x 0x0800_0000L) <> 0L

  let has_segment x = (logand x 0x3_F000_0000L) <> 0L
  let has_segment_cs x = (logand x 0x1000_0000L) <> 0L
  let has_segment_ss x = (logand x 0x2000_0000L) <> 0L
  let has_segment_ds x = (logand x 0x4000_0000L) <> 0L
  let has_segment_es x = (logand x 0x8000_0000L) <> 0L
  let has_segment_fs x = (logand x 0x0001_0000_0000L) <> 0L
  let has_segment_gs x = (logand x 0x0002_0000_0000L) <> 0L

  let has_operand_size x = (logand x 0x0004_0000_0000L) <> 0L
  let has_address_size x = (logand x 0x0008_0000_0000L) <> 0L

  let has_branch_hint x = (logand x 0x0C00_0000L) <> 0L
  let has_any_rep x = (logand x 0x0070_0000L) <> 0L
  let accepts_any_rep x = (logand x 0x1C00L) <> 0L
  let touches_cpu_state x = (logand x 0x0060_0000_0000L) <> 0L
  let touches_fpu_state x = (logand x 0x0180_0000_0000L) <> 0L
  let touches_xmm_state x = (logand x 0x0600_0000_0000L) <> 0L
end

module Instruction : sig
  type avx
  type meta =
    { category: instruction_category;
      isa_set: isaset;
      isa_ext: isaext;
      branch_type: branch_type option;
      exception_class: exception_class option;
    }
  type raw

  type t =
    { mode: machine_mode;
      mnemonic: mnemonic;
      length: int;
      encoding: instruction_encoding;
      opcode_map: opcode_map;
      opcode: int;
      stack_width: int;
      operand_width: int;
      address_width: int;
      operands: Operand.t Array.t;
      attributes: Attribute.t;
      flags: (cpuflag * cpuflag_action) Array.t;
      avx: avx;
      meta: meta;
      raw: raw;
      raw_insn: zydis_instruction;
    }
end = struct
  type avx
  type meta =
    { category: instruction_category;
      isa_set: isaset;
      isa_ext: isaext;
      branch_type: branch_type option;
      exception_class: exception_class option;
    }
  type raw

  type t =
    { mode: machine_mode;
      mnemonic: mnemonic;
      length: int;
      encoding: instruction_encoding;
      opcode_map: opcode_map;
      opcode: int;
      stack_width: int;
      operand_width: int;
      address_width: int;
      operands: Operand.t Array.t;
      attributes: Attribute.t;
      flags: (cpuflag * cpuflag_action) Array.t;
      avx: avx;
      meta: meta;
      raw: raw;
      raw_insn: zydis_instruction;
    }
end

external get_version : unit -> (int64 [@unboxed])
  = "zydis_get_version_byte" "zydis_get_version" [@@noalloc]
external is_feature_enabled : feature -> bool = "zydis_is_feature_enabled_byte"

external zydis_decoder_init : machine_mode -> address_width -> zydis_decoder = "zydis_decoder_init"
external zydis_decoder_enable : zydis_decoder -> machine_mode -> bool -> unit = "zydis_decoder_enable"
external zydis_decoder_decode_long : zydis_decoder -> bytes -> int -> Instruction.t option = "zydis_decoder_decode_long"
external zydis_decoder_decode_native : zydis_decoder -> bytes -> nativeint -> Instruction.t option = "zydis_decoder_decode_native"

module Decoder : sig
  type t

  val create : mode:machine_mode -> width:address_width -> t
  val enable : decoder:t -> mode:machine_mode -> enabled:bool -> unit
  val decode : decoder:t -> ?offset:int -> buffer:bytes -> Instruction.t option
  (* If you need the full precision for the offset, which you
     very likely don’t and also is slower. *)
  val decode_n : decoder:t -> ?offset:nativeint -> buffer:bytes -> Instruction.t option
end = struct
  type t = zydis_decoder

  let create ~mode ~width = zydis_decoder_init mode width
  let enable ~decoder ~mode ~enabled = zydis_decoder_enable decoder mode enabled
  let decode ~decoder ?(offset=0) ~buffer = zydis_decoder_decode_long decoder buffer offset
  let decode_n ~decoder ?(offset=0n) ~buffer = zydis_decoder_decode_native decoder buffer offset
end

let version () =
  let v = get_version() in
  let major = Int64.shift_right v 48 in
  let minor = Int64.(logand (shift_right v 32) 0xFFFFL)in
  let patch = Int64.(logand (shift_right v 16) 0xFFFFL) in
  let build = Int64.logand v 0xFFFFL in
  (major, minor, patch, build)
