type proto = [ `ARP | `IPv4 | `IPv6 ]

let pp_proto ppf = function
  | `ARP -> Fmt.string ppf "ARP"
  | `IPv4 -> Fmt.string ppf "IPv4"
  | `IPv6 -> Fmt.string ppf "IPv6"

type t = {
  source : Macaddr.t;
  destination : Macaddr.t;
  ethertype : proto;
}

type error = string

let pp fmt t =
  Format.fprintf fmt "%a -> %a: %a" Macaddr.pp t.source
    Macaddr.pp t.destination pp_proto t.ethertype

let equal {source; destination; ethertype} q =
  (Macaddr.compare source q.source) = 0 &&
  (Macaddr.compare destination q.destination) = 0 &&
  Ethernet_wire.(compare (ethertype_to_int ethertype) (ethertype_to_int q.ethertype)) = 0

module Unmarshal = struct

  let of_cstruct frame =
    let open Ethernet_wire in
    if Cstruct.length frame >= sizeof_ethernet then
      match get_ethernet_ethertype frame |> int_to_ethertype with
      | None -> Error (Printf.sprintf "unknown ethertype 0x%x in frame"
                                (get_ethernet_ethertype frame))
      | Some ethertype ->
        let payload = Cstruct.shift frame sizeof_ethernet
        and source = Macaddr.of_octets_exn (copy_ethernet_src frame)
        and destination = Macaddr.of_octets_exn (copy_ethernet_dst frame)
        in
        Ok ({ destination; source; ethertype;}, payload)
    else
      Error "frame too small to contain a valid ethernet header"
end

module Marshal = struct
  let check_len buf =
    if Ethernet_wire.sizeof_ethernet > Cstruct.length buf then
      Error "Not enough space for an Ethernet header"
    else Ok ()

  let unsafe_fill t buf =
    let open Ethernet_wire in
    set_ethernet_dst (Macaddr.to_octets t.destination) 0 buf;
    set_ethernet_src (Macaddr.to_octets t.source) 0 buf;
    set_ethernet_ethertype buf (ethertype_to_int t.ethertype);
    ()

  let into_cstruct t buf =
    Result.map (fun () -> unsafe_fill t buf) (check_len buf)

  let make_cstruct t =
    let buf = Cstruct.create Ethernet_wire.sizeof_ethernet in
    unsafe_fill t buf;
    buf
end
