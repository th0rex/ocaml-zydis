module Enums = Enums

exception InvalidAddressWidth

type zydis_instruction

type immediate = Signed of int64 | Unsigned of int64

type relative = Absolute | Relative

module Operand : sig
  module Action : sig
    type t

    val is_read : t -> bool

    val is_write : t -> bool

    val is_condread : t -> bool

    val is_condwrite : t -> bool

    val is_read_and_write : t -> bool

    val is_condread_and_condwrite : t -> bool

    val is_read_and_condwrite : t -> bool

    val is_condread_and_write : t -> bool

    val is_any_read : t -> bool

    val is_any_write : t -> bool
  end

  type kind =
    | Unused
    | Reg of Enums.register
    | Mem of {
        typ : Enums.memory_operand_type;
        segment : Enums.register option;
        base : Enums.register option;
        index : Enums.register option;
        scale : int;
        displacement : int64 option;
      }
    (* segment, offset *)
    | Ptr of int * int32
    | Imm of relative * immediate

  type t = private {
    id : int;
    visibility : Enums.operand_visibility;
    actions : Action.t;
    encoding : Enums.operand_encoding;
    bit_size : int;
    el_ty : Enums.element_type;
    el_size : int;
    el_count : int;
    kind : kind;
  }
end

(** Attributes of an instruction *)
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

  (* These are conditional reads and writes, i.e. it *might*
     read or write, but it also might not.
  *)
  val reads_cpu_state : t -> bool

  val writes_cpu_state : t -> bool

  val reads_fpu_state : t -> bool

  val writes_fpu_state : t -> bool

  val reads_xmm_state : t -> bool

  val writes_xmm_state : t -> bool

  (* Whether the instruction accepts the given prefix.
   *)
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

  (* Whether the instruction has the given prefix.
   *)
  val has_lock : t -> bool

  val has_rep : t -> bool

  val has_repe : t -> bool

  val has_repne : t -> bool

  val has_bnd : t -> bool

  val has_xacquire : t -> bool

  val has_xrelease : t -> bool

  val has_branch_not_taken : t -> bool

  val has_branch_taken : t -> bool

  (* Whether the instruction has the given segment.
   *)
  val has_segment_cs : t -> bool

  val has_segment_ss : t -> bool

  val has_segment_ds : t -> bool

  val has_segment_es : t -> bool

  val has_segment_fs : t -> bool

  val has_segment_gs : t -> bool

  val has_operand_size : t -> bool

  val has_address_size : t -> bool

  (* Broader categories from zydis *)
  val has_segment : t -> bool

  (* Custom broader categories *)
  val has_branch_hints : t -> bool

  val has_any_rep : t -> bool

  val accepts_any_rep : t -> bool

  (* Whether the instruction accesses the state in any way, i.e.
     either read or write. Accesses are still conditional and might
     not happen.
  *)
  val touches_cpu_state : t -> bool

  val touches_fpu_state : t -> bool

  val touches_xmm_state : t -> bool
end

module Instruction : sig
  (* Not implemented for now *)
  type avx

  type meta = private {
    category : Enums.instruction_category;
    isa_set : Enums.isaset;
    isa_ext : Enums.isaext;
    branch_type : Enums.branch_type option;
    exception_class : Enums.exception_class option;
  }

  (* Not implemented for now *)
  type raw

  type t = private {
    mode : Enums.machine_mode;
    mnemonic : Enums.mnemonic;
    length : int;
    encoding : Enums.instruction_encoding;
    opcode_map : Enums.opcode_map;
    opcode : int;
    stack_width : int;
    operand_width : int;
    address_width : int;
    operands : Operand.t Array.t;
    attributes : Attribute.t;
    flags : (Enums.cpuflag * Enums.cpuflag_action) Array.t;
    avx : avx;
    meta : meta;
    raw : raw;
    raw_insn : zydis_instruction;
  }
end

module Decoder : sig
  type t

  val create : mode:Enums.machine_mode -> width:int -> t

  val disable : t -> Enums.machine_mode -> unit

  val enable : t -> Enums.machine_mode -> unit

  val decode : t -> bytes -> int -> Instruction.t option
  (** [decode ~decoder ~buffer offset] decodes a single instruction at
      [offset] in the given [buffer].
   *)

  val decode_n : t -> bytes -> nativeint -> Instruction.t option

  val set_enabled : t -> Enums.machine_mode -> bool -> unit
end

module Formatter : sig
  type t

  val create : style:Enums.formatter_style -> t

  (* val set_property : unit  *)
  (* TODO: ^ wrap the properties nicer *)

  val format : t -> Instruction.t -> string

  val format_addr : t -> Instruction.t -> int64 -> string
end

module type Memory = sig
  type t

  type phys = int
  type virt = int

  val create : bytes -> t

  val phys_of_virt : t -> virt -> phys

  (* For the decoder *)
  val bytes : t -> bytes

  val load64 : t -> virt -> int64 option
end

module LinearMemory : Memory

module Recursive (M : Memory) : sig
  type t

  val create : Decoder.t -> M.t -> t

  (* TODO: Maybe allow the callback to control the disassembler. *)
  val disassemble : M.virt -> (M.virt -> Instruction.t -> unit) -> t -> unit
end
