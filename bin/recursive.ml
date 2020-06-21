open Zydis
open Zydis.Enums

let decoder = Decoder.create ~mode:LONG_64 ~width:64

let formatter = Formatter.create ~style:INTEL

let buffer =
  let file = open_in_bin "/usr/bin/false" in
  let n = in_channel_length file in
  let buffer = Bytes.create n in
  really_input file buffer 0 n;
  close_in file;
  buffer

let disassembler = Recursive.create decoder buffer

let start = 0x2070

let f offs insn =
  Printf.printf "0x%x %s\n" offs (Formatter.format formatter insn)

let () = Recursive.disassemble start f disassembler
