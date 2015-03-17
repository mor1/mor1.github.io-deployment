(*
 * Copyright (c) 2013 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Mirage

(** [split c s] splits string [s] at every occurrence of character [c] *)
let split c s =
  let rec aux c s ri acc =
    (* half-closed intervals. [ri] is the open end, the right-fencepost.
       [li] is the closed end, the left-fencepost. either [li] is
       + negative (outside [s]), or
       + equal to [ri] ([c] not found in remainder of [s]) ->
         take everything from [ s[0], s[ri] )
       + else inside [s], thus an instance of the separator ->
         accumulate from the separator to [ri]: [ s[li+1], s[ri] )
         and move [ri] inwards to the discovered separator [li]
    *)
    let li = try String.rindex_from s (ri-1) c with Not_found -> -1 in
    if li < 0 || li == ri then (String.sub s 0 ri) :: acc
    else begin
      let len = ri-1 - li in
      let rs = String.sub s (li+1) len in
      aux c s li (rs :: acc)
    end
  in
  aux c s (String.length s) []

let mkfs fs =
  let mode =
    try match String.lowercase (Unix.getenv "FS") with
      | "fat"    -> `Fat
      | "direct" -> `Direct
      | _        -> `Crunch
    with Not_found -> `Crunch
  in
  let fat_ro dir =
    kv_ro_of_fs (fat_of_files ~dir ())
  in
  match mode with
  | `Fat    -> fat_ro fs
  | `Crunch -> crunch fs
  | `Direct -> direct_kv_ro fs

let sitefs = mkfs "../_site"

let https =
  let deploy =
    try match Sys.getenv "DEPLOY" with
      | "1" | "true" | "yes" -> true
      | _ -> false
    with Not_found -> false
  in
  let stack console =
    match deploy with
    | true ->
      let staticip =
        let address = Sys.getenv "ADDR" |> Ipaddr.V4.of_string_exn in
        let netmask = Sys.getenv "MASK" |> Ipaddr.V4.of_string_exn in
        let gateways =
          Sys.getenv "GWS" |> split ':' |> List.map Ipaddr.V4.of_string_exn
        in
        { address; netmask; gateways }
      in
      direct_stackv4_with_static_ipv4 console tap0 staticip

    | false ->
      let net =
        try match Sys.getenv "NET" with
          | "socket" -> `Socket
          | _        -> `Direct
        with Not_found -> `Direct
      in
      let dhcp =
        try match Sys.getenv "DHCP" with
          | "1" | "true" | "yes" -> true
          | _  -> false
        with Not_found -> false
      in
      match net, dhcp with
      | `Direct, false -> direct_stackv4_with_default_ipv4 console tap0
      | `Direct, true  -> direct_stackv4_with_dhcp console tap0
      | `Socket, _     -> socket_stackv4 console [Ipaddr.V4.any]
  in
  let port =
    try match Sys.getenv "PORT" with
      | "" -> 80
      | s  -> int_of_string s
    with Not_found -> 80
  in
  let server = conduit_direct (stack default_console) in
  let mode = `TCP (`Port port) in
  http_server mode server

let main =
  foreign "Server.Main" (console @-> kv_ro @-> http @-> job)

let () =
  register "mortio" [
    main $ default_console $ sitefs $ https
  ]
