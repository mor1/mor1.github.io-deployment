(*
 * Copyright (c) 2013-2016 Richard Mortier <mort@cantab.net>
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

let tls_key =
  let doc = Key.Arg.info
      ~doc:"Enable serving the website over https. Put certificates in tls/"
      ~docv:"BOOL" ~env:"TLS" ["tls"]
  in
  Key.(create "tls" Arg.(opt ~stage:`Configure bool false doc))

let pr_key =
  let doc = Key.Arg.info
      ~doc:"Configuration for running inside a travis PR."
      ~env:"TRAVIS_PULL_REQUEST" ["pr"]
  in
  Key.(create "pr" Arg.(opt ~stage:`Configure (some int) None doc))

let host_key =
  let doc = Key.Arg.info
      ~doc:"Hostname of the unikernel."
      ~docv:"URL" ~env:"HOST" ["host"]
  in
  Key.(create "host" Arg.(opt string "localhost" doc))

let port =
  let doc = Key.Arg.info
      ~doc:"Listening port."
      ~docv:"PORT" ~env:"PORT" ["port"]
  in
  Key.(create "port" Arg.(opt int 80 doc))

let fs_key = Key.(value @@ kv_ro ())
let sitefs = generic_kv_ro ~key:fs_key "../_site"

let secrets_key = Key.(value @@ kv_ro ())
let secrets = generic_kv_ro ~key:secrets_key "./tls"

let keys = Key.([ abstract host_key; abstract port ])
let packages = List.map package [
    "astring";
    "magic-mime";
    "mirage-http";
    "mirage-logs";
    "uri";
  ]

let stack = generic_stackv4 default_network
let http_svr = http_server @@ conduit_direct ~tls:false stack
let https_svr = http_server @@ conduit_direct ~tls:true stack

let http_job =
  foreign ~packages ~keys
    "Unikernel.Http" (http @-> kv_ro @-> job)

let https_job =
  let packages = packages @ [ package ~sublibs:["mirage"] "tls" ] in
  foreign ~packages ~keys ~deps:[abstract nocrypto]
    "Unikernel.Https" (kv_ro @-> http @-> kv_ro @-> job)

let dispatcher =
  if_impl (Key.value tls_key)
    (https_job $ secrets $ https_svr)
    (http_job $ http_svr)

let () =
  register ~packages "mort.io" [
    dispatcher $ sitefs
  ]
