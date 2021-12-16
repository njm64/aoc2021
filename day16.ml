open Base
open Stdio

type packet =
  | Literal of int
  | Sum of packet list
  | Product of packet list
  | Minimum of packet list
  | Maximum of packet list
  | GreaterThan of (packet * packet)
  | LessThan of (packet * packet)
  | Equal of (packet * packet)

let rec eval p =
  match p with
  | Literal n -> n
  | Sum lst -> eval_list lst ( + )
  | Product lst -> eval_list lst ( * )
  | Minimum lst -> eval_list lst Int.min
  | Maximum lst -> eval_list lst Int.max
  | GreaterThan (a, b) -> eval_cmp a b ( > )
  | LessThan (a, b) -> eval_cmp a b ( < )
  | Equal (a, b) -> eval_cmp a b ( = )

and eval_list lst f = List.map lst ~f:eval |> List.reduce_exn ~f
and eval_cmp a b f = if f (eval a) (eval b) then 1 else 0

let parse_hex s =
  let len = String.length s / 2 in
  let buf = Bytes.make len (Char.of_int_exn 0) in
  for i = 0 to len - 1 do
    let n = "0x" ^ String.sub s ~pos:(i * 2) ~len:2 |> Int.of_string in
    Bytes.set buf i (Char.of_int_exn n)
  done;
  buf

let parse_input lines = List.hd_exn lines |> String.strip |> parse_hex

type reader = {
  buf : Bytes.t;
  mutable pos : int;
}

let reader_of_buf buf = { buf; pos = 0 }

let read_bit r =
  let byte_offset = r.pos / 8 in
  let bit_offset = 7 - (r.pos % 8) in
  r.pos <- r.pos + 1;
  let mask = 1 lsl bit_offset in
  let b = Bytes.get r.buf byte_offset |> Char.to_int in
  (b land mask) lsr bit_offset

let read_bits r bits =
  let n = ref 0 in
  for _ = 1 to bits do
    n := (!n lsl 1) lor read_bit r
  done;
  !n

let rec get_version_sum r =
  let v = ref (read_bits r 3) in
  let t = read_bits r 3 in
  (if t = 4 then
   let ok = ref 1 in
   while !ok = 1 do
     ok := read_bit r;
     ignore (read_bits r 4)
   done
  else
    let length_type_id = read_bit r in
    if length_type_id = 0 then
      let end_pos = r.pos + read_bits r 15 in
      while r.pos < end_pos do
        v := !v + get_version_sum r
      done
    else
      let packet_count = read_bits r 11 in
      for _ = 1 to packet_count do
        v := !v + get_version_sum r
      done);
  !v

let rec parse_packet r =
  let _v = read_bits r 3 in
  let t = read_bits r 3 in
  match t with
  | 0 -> Sum (parse_list r)
  | 1 -> Product (parse_list r)
  | 2 -> Minimum (parse_list r)
  | 3 -> Maximum (parse_list r)
  | 4 -> Literal (parse_literal r)
  | 5 -> GreaterThan (parse_pair r)
  | 6 -> LessThan (parse_pair r)
  | 7 -> Equal (parse_pair r)
  | _ -> failwith "Unexpected packet type"

and parse_literal r =
  let n = ref 0 in
  let ok = ref 1 in
  while !ok = 1 do
    ok := read_bit r;
    n := (!n lsl 4) lor read_bits r 4
  done;
  !n

and parse_list r =
  let lst = ref [] in
  let length_type_id = read_bit r in
  (if length_type_id = 0 then
   let end_pos = r.pos + read_bits r 15 in
   while r.pos < end_pos do
     lst := parse_packet r :: !lst
   done
  else
    let packet_count = read_bits r 11 in
    for _ = 1 to packet_count do
      lst := parse_packet r :: !lst
    done);
  List.rev !lst

and parse_pair r =
  match parse_list r with
  | [ a; b ] -> (a, b)
  | _ -> failwith "Expected 2 packets"

let run () =
  let buf = In_channel.read_lines "input/day16.txt" |> parse_input in

  (* Part 1 *)
  let vs = reader_of_buf buf |> get_version_sum in
  printf "Version sum: %d\n" vs;

  (* Part 2 *)
  let value = reader_of_buf buf |> parse_packet |> eval in
  printf "Value: %d\n" value
