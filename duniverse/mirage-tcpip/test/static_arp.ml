module Make(E : Ethernet.S) = struct
  module A = Arp.Make(E)
  (* generally repurpose A, but substitute input and query, and add functions
     for adding/deleting entries *)

  type t = {
    base : A.t;
    table : (Ipaddr.V4.t, Macaddr.t) Hashtbl.t;
  }

  let add_ip t = A.add_ip t.base
  let remove_ip t = A.remove_ip t.base
  let set_ips t = A.set_ips t.base
  let get_ips t = A.get_ips t.base

  let pp ppf t =
    let print ip entry =
      Fmt.pf ppf "IP %a : MAC %a" Ipaddr.V4.pp ip Macaddr.pp entry
    in
    Hashtbl.iter print t.table

  let connect ~sw ~clock e = 
    let base = A.connect ~clock ~sw e in
    { base; table = (Hashtbl.create 7) }

  let disconnect t = A.disconnect t.base

  let query t ip =
    match Hashtbl.mem t.table ip with
    | false -> raise Arp.Timeout
    | true -> (Hashtbl.find t.table ip)

  let input t buffer =
    (* disregard responses, but reply to queries *)
    let open Arp_packet in
    match decode buffer with
    | Ok arp when arp.operation = Request -> A.input t.base buffer
    | Ok _ -> ()
    | Error e ->
      Format.printf "Arp decoding failed %a" pp_error e

  let add_entry t ip mac =
    Hashtbl.add t.table ip mac
end
