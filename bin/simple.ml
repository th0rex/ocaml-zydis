open Zydis
open Zydis.Enums

let decoder = Decoder.create ~mode:LONG_64 ~width:64

let formatter = Formatter.create ~style:INTEL

let buffer = Bytes.of_string "\x75\x2f"

let () =
  print_string
    ( match Decoder.decode ~decoder ~buffer 0 with
    | None -> "failed to decode instruction\n"
    | Some insn -> (Formatter.format ~formatter ~insn) ^ "\n" )
