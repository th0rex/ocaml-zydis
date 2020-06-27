module Enums = Enums
open Enums

exception InvalidAddressWidth

type zydis_decoder

type zydis_formatter

type zydis_instruction

type immediate = Signed of int64 | Unsigned of int64

type relative = Absolute | Relative

module Operand = struct
  module Action = struct
    type t = int

    let is_read x = x land 0x01 = 0x01

    let is_write x = x land 0x02 = 0x02

    let is_condread x = x land 0x04 = 0x04

    let is_condwrite x = x land 0x08 = 0x08

    let is_read_and_write x = x land 0x03 = 0x03

    let is_condread_and_condwrite x = x land 0x0C = 0x0C

    let is_read_and_condwrite x = x land 0x09 = 0x09

    let is_condread_and_write x = x land 0x06 = 0x06

    let is_any_read x = x land 0x05 <> 0x00

    let is_any_write x = x land 0x0A <> 0x00
  end

  type kind =
    | Unused
    | Reg of register
    | Mem of {
        typ : memory_operand_type;
        segment : register option;
        base : register option;
        index : register option;
        scale : int;
        displacement : int64 option;
      }
    (* segment, offset *)
    | Ptr of int * int32
    | Imm of relative * immediate

  type t = {
    id : int;
    visibility : operand_visibility;
    actions : Action.t;
    encoding : operand_encoding;
    bit_size : int;
    el_ty : element_type;
    el_size : int;
    el_count : int;
    kind : kind;
  }
end

module Attribute = struct
  (* Use Int63.t potentially *)
  type t = int64

  open Int64

  let has_modrm x = logand x 0x01L <> 0L

  let has_sib x = logand x 0x02L <> 0L

  let has_rex x = logand x 0x04L <> 0L

  let has_xop x = logand x 0x08L <> 0L

  let has_vex x = logand x 0x10L <> 0L

  let has_evex x = logand x 0x20L <> 0L

  let has_mvex x = logand x 0x40L <> 0L

  let is_relative x = logand x 0x80L <> 0L

  let is_privileged x = logand x 0x0100L <> 0L

  let accesses_flags x = logand x 0x0010_0000_0000L <> 0L

  let reads_cpu_state x = logand x 0x0020_0000_0000L <> 0L

  let writes_cpu_state x = logand x 0x0040_0000_0000L <> 0L

  let reads_fpu_state x = logand x 0x0080_0000_0000L <> 0L

  let writes_fpu_state x = logand x 0x0100_0000_0000L <> 0L

  let reads_xmm_state x = logand x 0x0200_0000_0000L <> 0L

  let writes_xmm_state x = logand x 0x0400_0000_0000L <> 0L

  let accepts_lock x = logand x 0x0200L <> 0L

  let accepts_rep x = logand x 0x0400L <> 0L

  let accepts_repe x = logand x 0x0800L <> 0L

  let accepts_repne x = logand x 0x1000L <> 0L

  let accepts_bnd x = logand x 0x2000L <> 0L

  let accepts_xacquire x = logand x 0x4000L <> 0L

  let accepts_xrelease x = logand x 0x8000L <> 0L

  let accepts_hle_without_lock x = logand x 0x0001_0000L <> 0L

  let accepts_branch_hints x = logand x 0x0002_0000L <> 0L

  let accepts_segment x = logand x 0x0004_0000L <> 0L

  let has_lock x = logand x 0x0008_0000L <> 0L

  let has_rep x = logand x 0x0010_0000L <> 0L

  let has_repe x = logand x 0x0020_0000L <> 0L

  let has_repne x = logand x 0x0040_0000L <> 0L

  let has_bnd x = logand x 0x0080_0000L <> 0L

  let has_xacquire x = logand x 0x0100_0000L <> 0L

  let has_xrelease x = logand x 0x0200_0000L <> 0L

  let has_branch_not_taken x = logand x 0x0400_0000L <> 0L

  let has_branch_taken x = logand x 0x0800_0000L <> 0L

  let has_segment x = logand x 0x3_F000_0000L <> 0L

  let has_segment_cs x = logand x 0x1000_0000L <> 0L

  let has_segment_ss x = logand x 0x2000_0000L <> 0L

  let has_segment_ds x = logand x 0x4000_0000L <> 0L

  let has_segment_es x = logand x 0x8000_0000L <> 0L

  let has_segment_fs x = logand x 0x0001_0000_0000L <> 0L

  let has_segment_gs x = logand x 0x0002_0000_0000L <> 0L

  let has_operand_size x = logand x 0x0004_0000_0000L <> 0L

  let has_address_size x = logand x 0x0008_0000_0000L <> 0L

  let has_branch_hints x = logand x 0x0C00_0000L <> 0L

  let has_any_rep x = logand x 0x0070_0000L <> 0L

  let accepts_any_rep x = logand x 0x1C00L <> 0L

  let touches_cpu_state x = logand x 0x0060_0000_0000L <> 0L

  let touches_fpu_state x = logand x 0x0180_0000_0000L <> 0L

  let touches_xmm_state x = logand x 0x0600_0000_0000L <> 0L
end

module Instruction = struct
  type avx

  type meta = {
    category : instruction_category;
    isa_set : isaset;
    isa_ext : isaext;
    branch_type : branch_type option;
    exception_class : exception_class option;
  }

  type raw

  type t = {
    mode : machine_mode;
    mnemonic : mnemonic;
    length : int;
    encoding : instruction_encoding;
    opcode_map : opcode_map;
    opcode : int;
    stack_width : int;
    operand_width : int;
    address_width : int;
    operands : Operand.t Array.t;
    attributes : Attribute.t;
    flags : (cpuflag * cpuflag_action) Array.t;
    avx : avx;
    meta : meta;
    raw : raw;
    raw_insn : zydis_instruction;
  }
end

external zydis_decoder_init : machine_mode -> address_width -> zydis_decoder
  = "zydis_decoder_init"

external zydis_decoder_enable : zydis_decoder -> machine_mode -> bool -> unit
  = "zydis_decoder_enable"

external zydis_decoder_decode_long :
  zydis_decoder -> bytes -> int -> Instruction.t option
  = "zydis_decoder_decode_long"

external zydis_decoder_decode_native :
  zydis_decoder -> bytes -> nativeint -> Instruction.t option
  = "zydis_decoder_decode_native"

module Decoder = struct
  type t = zydis_decoder

  let create ~mode ~width =
    match width with
    | 16 -> zydis_decoder_init mode AddressWidth16
    | 32 -> zydis_decoder_init mode AddressWidth32
    | 64 -> zydis_decoder_init mode AddressWidth64
    | _ -> raise InvalidAddressWidth

  let disable decoder mode = zydis_decoder_enable decoder mode false

  let enable decoder mode = zydis_decoder_enable decoder mode true

  let decode decoder buffer offset =
    zydis_decoder_decode_long decoder buffer offset

  let decode_n decoder buffer offset =
    zydis_decoder_decode_native decoder buffer offset

  let set_enabled decoder mode v = zydis_decoder_enable decoder mode v
end

external zydis_formatter_init : formatter_style -> zydis_formatter
  = "zydis_formatter_init"

external zydis_formatter_format_insn :
  zydis_formatter -> zydis_instruction -> int64 -> string
  = "zydis_formatter_format_insn"

module Formatter = struct
  type t = zydis_formatter

  open Instruction

  let create ~style = zydis_formatter_init style

  let format formatter { raw_insn; _ } =
    zydis_formatter_format_insn formatter raw_insn (-1L)

  let format_addr formatter { raw_insn; _ } addr =
    zydis_formatter_format_insn formatter raw_insn addr
end

module type Memory = sig
  type t

  type phys = int
  type virt = int

  val create : bytes -> t

  val phys_of_virt : t -> virt -> phys

  val bytes : t -> bytes

  val load64 : t -> virt -> int64 option
end

module LinearMemory : Memory = struct
  type t = bytes

  type phys = int
  type virt = int

  let create x = x

  let phys_of_virt _ x = x

  let bytes t = t

  let load64 _t _offs = None (* Some (Bytes.get_int64_le x offs) *)
end

module Recursive (M : Memory) = struct
  let compare_int a b =
    let int_of_bool b = Obj.magic b in
    int_of_bool (a > b) - int_of_bool (a < b)

  module IntOrder : Set.OrderedType with type t = int = struct
    type t = int
    let compare = compare_int
  end

  module IntSet = Set.Make (IntOrder)

  type t = {
    d : Decoder.t;
    mem : M.t;
    mutable seen : IntSet.t;
  }

  let create d mem = {d; mem; seen = IntSet.empty}

  let disassemble offs f t =
    let get_imm rip kind = match kind with
      | Operand.Imm (Relative, Signed imm) | Operand.Imm (Relative, Unsigned imm) ->
        Some ((Int64.to_int imm) + rip)
      | Operand.Imm (Absolute, Signed imm) | Operand.Imm (Absolute, Unsigned imm) ->
        Some (Int64.to_int imm)
      | Operand.Mem {base = Some RIP; displacement = Some disp; _} ->
        let offs = (Int64.to_int disp) + rip in
        M.load64 t.mem offs |> Option.map Int64.to_int
      | _ -> None
    in
    let rec go = function
      | [] -> ()
      | x :: xs when IntSet.mem x t.seen -> go xs
      | x :: xs ->
        (* TODO: Keep the set smaller by only adding branch targets. *)
        t.seen <- IntSet.add x t.seen;
        let buffer = M.bytes t.mem in
        let phys = M.phys_of_virt t.mem x in
        match Decoder.decode t.d buffer phys with
        | None -> go xs
        | Some insn ->
          f x insn;
          let rip = x + insn.length in
          match insn.meta.category with
          | CALL -> (match get_imm rip insn.operands.(0).kind with
            | Some imm -> go (imm :: xs)
            | None -> go xs
            )
          | COND_BR -> (match get_imm rip insn.operands.(0).kind with
            | Some imm -> go (rip :: imm :: xs)
            | None -> go (rip :: xs)
            )
          | RET -> go xs
          | _ -> match insn.mnemonic with
            | JMP -> (match get_imm rip insn.operands.(0).kind with
              | Some imm -> go (imm :: xs)
              | None -> go xs
              )
            | _ -> go (rip :: xs)
    in
    go [offs]
end
