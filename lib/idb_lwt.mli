(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

include Idb_sigs.DB

(** Store that uses the standard serialization provided by IndexeDB
    (structured clone algorithm):

    https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Structured_clone_algorithm

    This does not work for all OCaml values, e.g., it causes problems
    for OCaml strings. *)
module Unsafe : Idb_sigs.STORE_POLY
  with type key = Js.js_string Js.t
   and type 'a content = 'a Js.t
   and type db := db
   and type store_name := store_name

(** Store that uses the Js_of_ocaml [Json] module for serialization *)
module Json : Idb_sigs.STORE_POLY
  with type key = Js.js_string Js.t
   and type 'a content = 'a
   and type db := db
   and type store_name := store_name

(** Functorial interface *)
module Make (C : Idb_sigs.Js_string_conv) : Idb_sigs.STORE
  with type key = C.key
   and type content = C.content
   and type db := db
   and type store_name := store_name
