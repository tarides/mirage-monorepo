(*
 * Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS l SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "ipv6" ~doc:"Mirage IPv6"

module Log = (val Logs.src_log src : Logs.LOG)
module I = Ipaddr
module Lwt = struct end

module Make
    (R : Mirage_random.S) =
struct
  type ipaddr = Ipaddr.V6.t
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit

  let pp_ipaddr = Ipaddr.V6.pp

  type t = { ethif : Ethernet.t; clock : Eio.Time.clock; mutable ctx : Ndpv6.context }

  let output t (dst, size, fillf) =
    let buffer = Cstruct.create size in
    let size = fillf buffer in
    let buffer = Cstruct.sub buffer 0 size in
    Ethernet.copy t.ethif dst `IPv6 (Eio.Flow.cstruct_source [ buffer ])

  let output_ign t a = output t a |> ignore

  let start_ticking t u =
    let rec loop u =
      let now = Eio.Time.now t.clock |> Duration.of_f in
      let ctx, outs = Ndpv6.tick ~now t.ctx in
      t.ctx <- ctx;
      let u =
        match (u, Ndpv6.get_ip t.ctx) with
        | None, _ | _, [] -> u
        | Some u, _ ->
            Eio.Promise.resolve u ();
            None
      in
      List.iter (output_ign t) outs;
      (* MCP: replace with propagation *)
      Eio.Time.sleep t.clock 1.;
      loop u
    in
    loop (Some u)

  let mtu t ~dst:_ = Ethernet.mtu t.ethif - Ipv6_wire.sizeof_ipv6

  let write t ?fragment:_ ?ttl:_ ?src dst proto ?(size = 0) headerf bufs =
    let now = Eio.Time.now t.clock |> Duration.of_f in
    (* TODO fragmentation! *)
    let payload = Cstruct.concat bufs in
    let size' = size + Cstruct.length payload in
    let fillf _ip6hdr buf =
      let h_len = headerf buf in
      if h_len > size then (
        Log.err (fun m -> m "provided headerf exceeds size");
        invalid_arg "headerf exceeds size");
      Cstruct.blit payload 0 buf h_len (Cstruct.length payload);
      h_len + Cstruct.length payload
    in
    let ctx, outs = Ndpv6.send ~now t.ctx ?src dst proto size' fillf in
    t.ctx <- ctx;
    (* MCP - it's not totally clear to me that this the right behavior
       for writev. *)
    List.iter (output t) outs

  let input t ~tcp ~udp ~default buf =
    let now = Eio.Time.now t.clock |> Duration.of_f in
    let ctx, outs, actions = Ndpv6.handle ~now ~random:R.generate t.ctx buf in
    t.ctx <- ctx;
    List.iter
      (function
        | `Tcp (src, dst, buf) -> tcp ~src ~dst buf
        | `Udp (src, dst, buf) -> udp ~src ~dst buf
        | `Default (proto, src, dst, buf) -> default ~proto ~src ~dst buf)
      actions;
    (* MCP: replace below w/proper error propagation *)
    List.iter (output_ign t) outs

  let disconnect _ =
    (* TODO *)
    ()

  let src t ~dst = Ndpv6.select_source t.ctx dst
  let get_ip t = Ndpv6.get_ip t.ctx

  let pseudoheader t ?src:source dst proto len =
    let ph = Cstruct.create (16 + 16 + 8) in
    let src = match source with None -> src t ~dst | Some x -> x in
    Ndpv6.ipaddr_to_cstruct_raw src ph 0;
    Ndpv6.ipaddr_to_cstruct_raw dst ph 16;
    Cstruct.BE.set_uint32 ph 32 (Int32.of_int len);
    Cstruct.set_uint8 ph 36 0;
    Cstruct.set_uint8 ph 37 0;
    Cstruct.set_uint8 ph 38 0;
    Cstruct.set_uint8 ph 39 (Ipv6_wire.protocol_to_int proto);
    ph

  let connect ?(no_init = false) ?(handle_ra = true) ?cidr ?gateway ~clock ~sw
     ethif =
    Log.info (fun f -> f "IP6: Starting");
    let now = Eio.Time.now clock |> Duration.of_f in
    let ctx, outs =
      Ndpv6.local ~handle_ra ~now ~random:R.generate (Ethernet.mac ethif)
    in
    let ctx, outs =
      match cidr with
      | None -> (ctx, outs)
      | Some p ->
          let ctx, outs' = Ndpv6.add_ip ~now ctx (Ipaddr.V6.Prefix.address p) in
          let ctx = Ndpv6.add_prefix ~now ctx (Ipaddr.V6.Prefix.prefix p) in
          (ctx, outs @ outs')
    in
    let ctx =
      match gateway with
      | None -> ctx
      | Some ip -> Ndpv6.add_routers ~now ctx [ ip ]
    in
    let t = { ctx; ethif; clock } in
    if no_init then t
    else
      let task, u = Eio.Promise.create ~label:"ipv6.init" () in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Private.Ctf.label "ipv6.connect.tick"; 
        start_ticking t u);
      let buffer = Cstruct.create (Ethernet.mtu ethif) in
      (* call listen until we're good in respect to DAD *)
      let rec ethif_listener ()=
        let noop ~src:_ ~dst:_ _ = () in
        let size = Eio.Flow.read ethif#ipv6 buffer in
        input t ~tcp:noop ~udp:noop ~default:(fun ~proto:_ -> noop) 
          (Cstruct.sub buffer 0 size);
        ethif_listener ()
      in
      Eio.Fiber.any
        [
          (* MCP: replace this error swallowing with proper propagation *)
          (fun () ->
            List.iter (output_ign t) outs |> fun () -> Eio.Promise.await task);
          (fun () -> ethif_listener ());
          (fun () -> Eio.Time.sleep clock 3.);
        ];
      let expected_ips = match cidr with None -> 1 | Some _ -> 2 in
      match get_ip t with
      | ips when List.length ips = expected_ips ->
          Log.info (fun f ->
              f "IP6: Started with %a"
                Fmt.(list ~sep:(any ",@ ") Ipaddr.V6.pp)
                ips);
          t
      | _ -> failwith "IP6 not started, couldn't assign IP addresses"
end
