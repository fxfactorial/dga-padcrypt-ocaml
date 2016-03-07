(* If using utop then uncomment next line *)
(* #require "nocrypto.unix, calendar,hex, stringext" *)

module D = CalendarLib.Date

let tlds = [|"com";"co.uk"; "de";"org";"net";"eu";"info";"online";
            "co"; "cc"; "website"|]

let tlds_count = Array.length tlds
let nr_domains = 24 * 3
let digit_mapping = "abcdnfolmk"

let seed_string ~date i =
  Printf.sprintf "%d-%d-%d|%d"
    (D.day_of_month date)
    (D.month date |> D.int_of_month)
    (D.year date)
    i

let domain_generate date =
  let rec helper count accum =
    if count = nr_domains then accum
    else seed_string ~date count :: helper (count + 1) accum
  in
  helper 0 []

let () =
  Nocrypto_entropy_unix.initialize ();
  CalendarLib.Date.today ()
  |> domain_generate
  |> List.iter begin fun date_stamp ->
    let hashed =
      (Cstruct.of_string date_stamp)
      |> Nocrypto.Hash.digest `SHA256
      |> Hex.of_cstruct |> function `Hex s -> s
    in
    let domain =
      String.sub hashed 3 16
      |> Stringext.to_list
      |> List.map
        (function '0'..'9' as hh ->
          Char.code hh - 48 |> String.get digit_mapping
                | o -> o)
      |> Stringext.of_list
    in
    let tld_index =
      String.get hashed (String.length hashed - 1)
      |> Printf.sprintf "0x%c"
      |> int_of_string
      |> fun tld -> if tld >= tlds_count then 0 else tld
    in
    Printf.sprintf "%s.%s" domain (Array.get tlds tld_index)
    |> print_endline
  end
