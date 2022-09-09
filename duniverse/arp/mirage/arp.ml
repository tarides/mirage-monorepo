(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2016 Hannes Mehnert <hannes@mehnert.org>
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

let logsrc = Logs.Src.create "ARP" ~doc:"Mirage ARP handler"

exception Timeout

type 'a or_exn = ('a, exn) result


open Eio.Std

type e = {
  mutable state : (Macaddr.t or_exn Promise.t * Macaddr.t or_exn Promise.u) Arp_handler.t ;
  ethif : Ethernet.t ;
  mutable ticking : bool ;
  clock : Eio.Time.clock ;
}

let probe_repeat_delay = Duration.of_ms 1500 (* per rfc5227, 2s >= probe_repeat_delay >= 1s *)

let output t (arp, destination) =
  let size = Arp_packet.size in
  let buf = Cstruct.create_unsafe size in
  Arp_packet.encode_into arp buf;
  try 
    Ethernet.copy t.ethif destination `ARP (Eio.Flow.cstruct_source [buf])
  with
  | e ->
    Logs.warn ~src:logsrc
      (fun m -> m "exception %s while outputting packet %a to %a"
          (Printexc.to_string e) Arp_packet.pp arp Macaddr.pp destination)

let rec tick t () =
  if t.ticking then begin
    Eio.Time.sleep t.clock (Duration.to_f probe_repeat_delay );
    let state, requests, timeouts = Arp_handler.tick t.state in
    t.state <- state ;
    List.map (fun r () -> output t r) requests
    |> Fiber.all;
    List.iter (fun (_, u) -> Promise.resolve u (Error Timeout)) timeouts ;
    tick t ()
  end

let pp ppf t = Arp_handler.pp ppf t.state

let input t frame =
  let state, out, wake = Arp_handler.input t.state frame in
  t.state <- state ;
  (match out with
    | None -> ()
    | Some pkt -> output t pkt) ;
  match wake with
  | None -> ()
  | Some (mac, (_, u)) -> 
    Promise.resolve u (Ok mac)

let get_ips t = Arp_handler.ips t.state

let create ?ipaddr t =
  let mac = Arp_handler.mac t.state in
  let state, out = Arp_handler.create ~logsrc ?ipaddr mac in
  t.state <- state ;
  match out with
  | None -> ()
  | Some x -> output t x

let add_ip t ipaddr =
  match Arp_handler.ips t.state with
  | [] -> create ~ipaddr t
  | _ ->
    let state, out, wake = Arp_handler.alias t.state ipaddr in
    t.state <- state ;
    output t out ;
    match wake with
    | None -> ()
    | Some (_, u) -> Promise.resolve u (Ok (Arp_handler.mac t.state))

let init_empty mac =
  let state, _ = Arp_handler.create ~logsrc mac in
  state

let set_ips t = function
  | [] ->
    let mac = Arp_handler.mac t.state in
    let state = init_empty mac in
    t.state <- state
  | ipaddr::xs ->
    create ~ipaddr t ;
    List.iter (add_ip t) xs

let remove_ip t ip =
  let state = Arp_handler.remove t.state ip in
  t.state <- state 

let query t ip =
  let merge = function
    | None -> 
      Promise.create ~label:"ARP response" ()
    | Some a -> a
  in
  let state, res = Arp_handler.query t.state ip merge in
  t.state <- state ;
  match res with
  | Arp_handler.RequestWait (pkt, (tr, _)) -> 
    (output t pkt;
    match 
      Promise.await tr
    with
    | Ok v -> v
    | Error exn -> raise exn)
  | Arp_handler.Wait (t, _) -> 
    (match 
      Promise.await t
    with
    | Ok v -> v
    | Error exn -> raise exn)
  | Arp_handler.Mac m -> 
    m

let connect ~sw ~clock ethif =
  let mac = Ethernet.mac ethif in
  let state = init_empty mac in
  let t = { ethif; state; ticking = true; clock} in
  Fiber.fork ~sw (tick t);
  t

let disconnect t =
  t.ticking <- false

  
type t = <
  pp: unit Fmt.t;
  get_ips: Ipaddr.V4.t list;
  set_ips: Ipaddr.V4.t list -> unit;
  remove_ip: Ipaddr.V4.t -> unit;
  add_ip: Ipaddr.V4.t -> unit;
  query: Ipaddr.V4.t -> Macaddr.t;
  Eio.Flow.sink;
>

let chunk_cs = Cstruct.create 10000

let fallback_copy source flow = 
  try
    while true do
      let got = Eio.Flow.read source chunk_cs in
      input flow (Cstruct.sub chunk_cs 0 got)
    done
  with End_of_file -> ()

let copy_with_rsb rsb flow =
  try
    rsb @@ fun cstruct ->
    input flow (Cstruct.concat cstruct);
    Cstruct.lenv cstruct
  with
  | End_of_file -> ()

let connect ~sw ~clock ethif =
  let v = connect ~sw ~clock ethif in
  object (self : t)
    method pp = Fmt.const pp v
    method get_ips = get_ips v
    method set_ips = set_ips v
    method remove_ip = remove_ip v
    method add_ip = add_ip v
    method query = query v

    method probe _ = None

    method copy (source : #Eio.Flow.source) =
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> 
            copy_with_rsb rsb v
        | _ :: xs -> aux xs
        | [] -> fallback_copy source v
      in
      aux (Eio.Flow.read_methods source)


  end

let query t = t#query