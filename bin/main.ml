(* Look and Say sequence https://oeis.org/A005150 *)

let rec seq_of_int n cont =
  if n >= 10 then
    seq_of_int (n / 10) 
      (Seq.cons (n mod 10)
         cont)
  else
    Seq.cons n cont
  
let rec werber n =
  if n = 1 then Seq.return 1
  else
    let prev = werber (n - 1) in
    match prev () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (h, t) ->
       let rec aux current count seq =
         match seq () with
         | Seq.Cons (h, t) when h = current -> aux current (count + 1) t
         | Seq.Cons (h, t) -> seq_of_int count (fun () -> Seq.Cons (current, (aux h 1 t)))
         | Seq.Nil -> seq_of_int count (Seq.return current) in
       aux h 1 t

let () =
  let n = int_of_string Sys.argv.(1) in
  let print_digit =
    let zero = Char.code '0' in
    fun n -> print_char (Char.chr (zero + n)) in
  Seq.iter print_digit (werber n);
  Printf.printf "%!"
    
