let read file =
  let open Unix in
  let size = (stat file).st_size in
  let buf  = Bytes.create size in
  let rec loop pos len fd =
    let n = read fd buf pos len in
    if n > 0 then loop (pos + n) (len - n) fd
  in
  let fd = Unix.openfile file Unix.[O_RDONLY] 0 in
  loop 0 size fd;
  Unix.close fd;
  Bigstringaf.of_string (Bytes.to_string buf) ~off:0 ~len:size

let bench name n_iters p data =
  let stat0 = Gc.stat () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n_iters do
    match Angstrom.(parse_bigstring ~consume:All) p data with
    | Ok _ -> ()
    | Error err -> failwith err
  done;
  let t1 = Unix.gettimeofday () in
  let stat1 = Gc.stat () in
  let time = t1 -. t0 in
  let n_iters = float n_iters in
  let minor_words = (stat1.minor_words -. stat0.minor_words) /. n_iters in
  let major_words = (stat1.major_words -. stat0.major_words) /. n_iters in
  let minor_collections = float (stat1.minor_collections - stat0.minor_collections) /. n_iters in
  let major_collections = float (stat1.major_collections - stat0.major_collections) /. n_iters in
  Printf.printf "%-7s %6.2fms %7.0fk %8.0f %7.0f %7.0f\n"
    name
    (1000. *. time /. n_iters)
    (minor_words/.1000.)
    major_words
    minor_collections
    major_collections

let slow =
  Angstrom.(many any_char)

let skip =
  Angstrom.(skip_many any_char)

let chars =
  Angstrom.(fix (fun p ->
      any_char >>= fun _ ->
      at_end_of_input >>= function
      | true -> return ()
      | false -> p
    ))

let () =
  let http_get = read "benchmarks/data/http-requests.txt.100" in
  let parse_http = Angstrom.skip_many RFC2616.request in
  Printf.printf "Name        Time  MinWrds  MajWrds  MinCol  MajCol\n";
  bench "http"  100 parse_http http_get;
  bench "count" 10  chars http_get;
  bench "space" 10  slow  http_get;
  bench "skip"  10  skip  http_get
