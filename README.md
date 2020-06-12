
# NOTE:

Currently not everything from zydis is exposed, but the more useful (to me)
parts of `ZydisDecodedInstruction` are wrapped in nice to use types.

# Example usage

See also `bin/` for more examples.

```ocaml
open Zydis.Enums

let decoder = Zydis.Decoder.create ~mode:LONG_64 ~width:64
let buffer = Bytes.of_string "\x75\x2f"
(* 0 is the offset into the buffer, so that it doesn't need to be
   copied.
 *)
let insn = Zydis.Decoder.decode decoder buffer 0
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

Results for `"\x62\xf1\x6c\x5f\xc2\x54\x98\x40\x0f"` as a more complex instruction:
```ocaml
val insn : Zydis.Instruction.t option =
  Some
   {Zydis.Instruction.mode = LONG_64; mnemonic = VCMPPS; length = 9;
    encoding = Zydis.Enums.EVEX; opcode_map = OpcodeMap0F; opcode = 194;
    stack_width = 64; operand_width = 32; address_width = 64;
    operands =
     [|{Zydis.Operand.id = 0; visibility = EXPLICIT; actions = <abstr>;
        encoding = MODRM_RM; bit_size = 64; el_ty = INT; el_size = 1;
        el_count = 64; kind = Zydis.Operand.Reg K2};
       {Zydis.Operand.id = 1; visibility = EXPLICIT; actions = <abstr>;
        encoding = DISP8; bit_size = 64; el_ty = INT; el_size = 1;
        el_count = 64; kind = Zydis.Operand.Reg K7};
       {Zydis.Operand.id = 2; visibility = EXPLICIT; actions = <abstr>;
        encoding = IS4; bit_size = 512; el_ty = FLOAT32; el_size = 32;
        el_count = 16; kind = Zydis.Operand.Reg ZMM2};
       {Zydis.Operand.id = 3; visibility = EXPLICIT; actions = <abstr>;
        encoding = Zydis.Enums.OPCODE; bit_size = 32; el_ty = FLOAT32;
        el_size = 32; el_count = 1;
        kind =
         Zydis.Operand.Mem
          {Zydis.Operand.typ = MEM; segment = Some DS; base = Some RAX;
           index = Some RBX; scale = 4; displacement = Some 256L}};
       {Zydis.Operand.id = 4; visibility = EXPLICIT; actions = <abstr>;
        encoding = UIMM16; bit_size = 8; el_ty = CC; el_size = 5; el_count = 1;
        kind = Zydis.Operand.Imm (Zydis.Absolute, Zydis.Unsigned 15L)}|];
    attributes = <abstr>; flags = [||]; avx = <abstr>;
    meta =
     {Zydis.Instruction.category = Zydis.Enums.AVX512; isa_set = AVX512F_512;
      isa_ext = AVX512EVEX; branch_type = None; exception_class = Some E2};
    raw = <abstr>}
```