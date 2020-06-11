
# NOTE:

Currently not everything from zydis is exposed, but the more useful (to me)
parts of `ZydisDecodedInstruction` are wrapped in nice to use types.

# Example usage

```ocaml
open Zydis.Enums

let decoder = Zydis.Decoder.create ~mode:LONG_64 ~width:AddressWidth64
let buffer = Bytes.of_string "\x75\x2f"
let insn = Zydis.Decoder.decode ~decoder ~buffer ()

(* If you have a huge buffer you can specify an offset so that you
   don't need to copy the buffer:
 *)
let insn2 = Zydis.Decoder.decode ~decoder ~buffer ~offset:0 ()
```

Results for `insn`:

```ocaml
val insn : Zydis.Instruction.t option =
  Some
   {Zydis.Instruction.mode = LONG_64; mnemonic = JNZ; length = 2;
    encoding = LEGACY; opcode_map = DEFAULT; opcode = 117; stack_width = 64;
    operand_width = 64; address_width = 64;
    operands =
     [|{Zydis.Operand.id = 0; visibility = EXPLICIT; actions = <abstr>;
        encoding = JIMM16; bit_size = 8; el_ty = INT; el_size = 8;
        el_count = 1;
        kind = Zydis.Operand.Imm (Zydis.Relative, Zydis.Signed 47L)};
       {Zydis.Operand.id = 1; visibility = HIDDEN; actions = <abstr>;
        encoding = MODRM_REG; bit_size = 64; el_ty = INT; el_size = 64;
        el_count = 1; kind = Zydis.Operand.Reg RIP};
       {Zydis.Operand.id = 2; visibility = HIDDEN; actions = <abstr>;
        encoding = MODRM_REG; bit_size = 64; el_ty = INT; el_size = 1;
        el_count = 64; kind = Zydis.Operand.Reg RFLAGS}|];
    attributes = <abstr>; flags = [|(ZF, TESTED)|]; avx = <abstr>;
    meta =
     {Zydis.Instruction.category = COND_BR; isa_set = I86; isa_ext = BASE;
      branch_type = Some SHORT; exception_class = None};
    raw = <abstr>; raw_insn = <abstr>}
```