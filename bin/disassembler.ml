open Zydis
open Zydis.Enums

let decoder = Decoder.create ~mode:LONG_64 ~width:64

let formatter = Formatter.create ~style:INTEL

module Disassembler = struct
  type t = int -> Instruction.t option

  open Instruction

  let create = Decoder.decode

  let iter decode f =
    let rec loop offs =
      match decode offs with
      | None -> ()
      | Some ({ length; _ } as insn) ->
          f insn;
          loop (offs + length)
    in
    loop 0
end

let buffer =
  Bytes.of_string
    "\x51\x8D\x45\xFF\x50\xFF\x75\x0C\xFF\x75\
     \x08\xFF\x15\xA0\xA5\x48\x76\x85\xC0\x0F\
     \x88\xFC\xDA\x02\x00"

let d = Disassembler.create decoder buffer

let f i = print_string (Formatter.format formatter i ^ "\n")

let () = Disassembler.iter d f
