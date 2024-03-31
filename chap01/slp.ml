type id = string

type binop =
  | Plus
  | Minus
  | Times
  | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

(* Source code:
    a := 5 + 3;
    b := (print(a, a-1), 10 * a);
    print(b)
*)
let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

let rec maxargs = function
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e) -> maxargs_from_exp e
  | PrintStm explist -> max (List.length explist) (maxargs_from_explist explist)

and maxargs_from_exp = function
  | IdExp _ | NumExp _ -> 0
  | OpExp (e1, _, e2) -> max (maxargs_from_exp e1) (maxargs_from_exp e2)
  | EseqExp (s, e) -> max (maxargs s) (maxargs_from_exp e)

and maxargs_from_explist = function
  | [] -> 0
  | hd :: tl -> max (maxargs_from_exp hd) (maxargs_from_explist tl)

let rec lookup table id =
  match table with
  | [] -> 0
  | (name, value) :: tl ->
      if name == id then
        value
      else
        lookup tl id

(* stm * table -> table *)
let rec interpStm stm table =
  match stm with
  | CompoundStm (s1, s2) -> interpStm s2 (interpStm s1 table)
  | AssignStm (name, e) ->
      let value, new_table = interpExp e table in
      (name, value) :: new_table (* update *)
  | PrintStm explist ->
      let ret =
        List.fold_left
          (fun acc e ->
            let value, new_table = interpExp e acc in
            print_int value;
            print_char ' ';
            new_table)
          table explist
      in
      print_newline ();
      ret

(* exp * table -> int * table *)
and interpExp exp table =
  match exp with
  | IdExp id -> (lookup table id, table)
  | NumExp x -> (x, table)
  | OpExp (e1, op, e2) -> (
      let v1, t1 = interpExp e1 table in
      let v2, t2 = interpExp e2 t1 in
      match op with
      | Plus -> (v1 + v2, t2)
      | Minus -> (v1 - v2, t2)
      | Times -> (v1 * v2, t2)
      | Div -> (v1 / v2, t2))
  | EseqExp (s, e) -> interpExp e (interpStm s table)

(* Tests *)
let _ = Div (* To avoid unused-constructor warning *)
let _ = print_int (maxargs prog)
let _ = print_newline ()
let table = interpStm prog []

(* Check the table *)
let _ =
  List.iter
    (fun x ->
      print_string (fst x);
      print_char '=';
      print_int (snd x);
      print_newline ())
    table
