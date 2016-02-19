(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

module Make (C : Idb_sigs.Js_string_conv) : Idb_sigs.S
  with type key = C.key
   and type content = C.content

module String : Idb_sigs.S
  with type key = string
   and type content = string
