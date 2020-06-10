[@@@ocaml.warning "-32"]

module Enums = Enums

open Enums

external get_version : unit -> (int64 [@unboxed])
  = "zydis_get_version_byte" "zydis_get_version" [@@noalloc]

external is_feature_enabled : feature -> bool = "zydis_is_feature_enabled_byte"

type zydis_decoder
type zydis_instruction

external zydis_decoder_init : machine_mode -> address_width -> zydis_decoder = "zydis_decoder_init_byte"
external zydis_decoder_enable : zydis_decoder -> machine_mode -> bool -> unit = "zydis_decoder_enable_byte"
external zydis_decoder_decode : zydis_decoder -> bytes -> zydis_instruction = "zydis_decoder_decode_byte"

module Decoder : sig
  type t

  val create : mode:machine_mode -> width:address_width -> t
  val enable : decoder:t -> mode:machine_mode -> enabled:bool -> unit
  val decode : decoder:t -> buffer:bytes -> zydis_instruction
end = struct
  type t = zydis_decoder

  (* TODO: Can we have the decoder be a "member" (value) of this module *)

  let create ~mode ~width = zydis_decoder_init mode width

  let enable ~decoder ~mode ~enabled = zydis_decoder_enable decoder mode enabled

  let decode ~decoder ~buffer = zydis_decoder_decode decoder buffer
end

(** type, offset, size *)
type instruction_segments = (instruction_segment * int * int) Array.t

let version () =
  let v = get_version() in
  let major = Int64.shift_right v 48 in
  let minor = Int64.(logand (shift_right v 32) 0xFFFFL)in
  let patch = Int64.(logand (shift_right v 16) 0xFFFFL) in
  let build = Int64.logand v 0xFFFFL in
  (major, minor, patch, build)
