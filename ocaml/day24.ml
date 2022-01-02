open Base

(* After disassembling the input, it seems that the program consists of 18
   instructions per digit. The code for each digit looks something like this,
   except the constants 10 and 13 below vary, and the division by 26 is
   sometimes a division by 1, which is a nop.

   w = input
   z = z / 26
   if (z % 26) - 10 == w then x = 0 else x = 1
   z = z * (25 * x + 1) + ((w + 13) * x)

   The only dependency between each phase is the Z register. So, we execute the
   last phase with all digits, and find out which input Z values give us an
   output value of zero. Store these input Z values in a set, and use them as
   the success criteria for the previous digit. Repeat for all digits, and we
   have enough information to find the highest/lowest digit at each place. *)

type register =
  | RegW
  | RegX
  | RegY
  | RegZ

type operand =
  | Register of register
  | Immediate of int

type opcode =
  | Inp of register
  | Add of register * operand
  | Mul of register * operand
  | Div of register * operand
  | Mod of register * operand
  | Eql of register * operand

type input = opcode list

type alu = {
  input : int list;
  w : int;
  x : int;
  y : int;
  z : int;
}

let parse_register s =
  match s with
  | "w" -> RegW
  | "x" -> RegX
  | "y" -> RegY
  | "z" -> RegZ
  | _ -> failwith "Invalid register"

let parse_operand s =
  try Register (parse_register s) with _ -> Immediate (Int.of_string s)

let parse_instruction line =
  let tokens = String.split line ~on:' ' in
  match tokens with
  | [ "inp"; a ] -> Inp (parse_register a)
  | [ "add"; a; b ] -> Add (parse_register a, parse_operand b)
  | [ "mul"; a; b ] -> Mul (parse_register a, parse_operand b)
  | [ "div"; a; b ] -> Div (parse_register a, parse_operand b)
  | [ "mod"; a; b ] -> Mod (parse_register a, parse_operand b)
  | [ "eql"; a; b ] -> Eql (parse_register a, parse_operand b)
  | _ -> failwith "Invalid instruction"

let store alu r v =
  match r with
  | RegW -> { alu with w = v }
  | RegX -> { alu with x = v }
  | RegY -> { alu with y = v }
  | RegZ -> { alu with z = v }

let get_reg alu r =
  match r with
  | RegW -> alu.w
  | RegX -> alu.x
  | RegY -> alu.y
  | RegZ -> alu.z

let get_val alu v =
  match v with
  | Register r -> get_reg alu r
  | Immediate n -> n

let execute alu instruction =
  match instruction with
  | Inp r -> (
      match alu.input with
      | n :: input -> store { alu with input } r n
      | _ -> failwith "Input error")
  | Add (a, b) -> get_reg alu a + get_val alu b |> store alu a
  | Mul (a, b) -> get_reg alu a * get_val alu b |> store alu a
  | Div (a, b) -> get_reg alu a / get_val alu b |> store alu a
  | Mod (a, b) -> get_reg alu a % get_val alu b |> store alu a
  | Eql (a, b) ->
      let n = if get_reg alu a = get_val alu b then 1 else 0 in
      store alu a n

let parse_input lines = List.map lines ~f:parse_instruction

let instructions_for_phase instructions n =
  let instructions_per_phase = 18 in
  let i = List.drop instructions (n * instructions_per_phase) in
  List.take i instructions_per_phase

let run_phase instructions d z =
  let alu = { x = 0; y = 0; z; w = 0; input = [ d ] } in
  let ret = List.fold instructions ~init:alu ~f:execute in
  ret.z

(* Calculate the set of valid Z input values for phase p,
   given the set of valid Z output values, zs *)
let calc_zs instructions p zs =
  let phase = instructions_for_phase instructions p in
  List.range 1 10
  |> List.map ~f:(fun d ->
         Sequence.range 0 10000
         |> Sequence.filter ~f:(fun z -> Set.mem zs (run_phase phase d z))
         |> Sequence.to_list)
  |> List.concat
  |> Set.of_list (module Int)

(* Find the best digit for phase p *)
let best_digit instructions p z zs criteria =
  let digits =
    match criteria with
    | `highest -> List.range 9 1 ~stop:`inclusive ~stride:(-1)
    | `lowest -> List.range 1 9 ~stop:`inclusive
  in
  let instructions = instructions_for_phase instructions p in
  List.find_map_exn digits ~f:(fun d ->
      let z = run_phase instructions d z in
      if Set.mem zs z then Some (d, z) else None)

let calc instructions zsets criteria =
  let rec step i z acc =
    if i = 14 then acc
    else
      let d, z = best_digit instructions i z zsets.(i) criteria in
      step (i + 1) z ((acc * 10) + d)
  in
  step 0 0 0

(* Calculate the set of valid Z input values for each phase *)
let calc_zsets instructions =
  let zsets = Array.create ~len:14 (Set.empty (module Int)) in
  zsets.(13) <- Set.of_list (module Int) [ 0 ];
  for i = 12 downto 0 do
    zsets.(i) <- calc_zs instructions (i + 1) zsets.(i + 1)
  done;
  zsets

let part1 instructions =
  let zsets = calc_zsets instructions in
  calc instructions zsets `highest

let part2 instructions =
  let zsets = calc_zsets instructions in
  calc instructions zsets `lowest
