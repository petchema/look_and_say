(* Look and Say sequence https://oeis.org/A005150 *)

let append_int buffer n =
  if n < 10 then
    Buffer.add_char buffer (Char.chr (Char.code '0' + n))
  else
    (* unused unless you use digits >= 4 in seed *)
    Printf.bprintf buffer "%d" n

let maxblocksize = 1024

type reader = {
    mutable block : Buffer.t;
    mutable index : int;
    mutable rest : Buffer.t Seq.t
  }

let rec reader_get reader =
  if reader.index < Buffer.length reader.block then
    let c = Buffer.nth reader.block reader.index in
    reader.index <- reader.index + 1;
    Some c
  else
    match reader.rest () with
    | Seq.Nil -> None
    | Seq.Cons (h, t) ->
       reader.block <- h;
       reader.index <- 0;
       reader.rest <- t;
       reader_get reader
                 
let rec look_and_say n =
  if n = 1 then
    let buffer1 = Buffer.create 1 in
    Buffer.add_char buffer1 '1';
    Seq.return buffer1
  else
    let prev = look_and_say (n - 1) in
    match prev () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (h, t) ->
       let reader = { block = h; index = 0; rest = t } in
       let rec aux current count out_buffer =
         match reader_get reader with
         | Some c when c = current ->
            aux current (count + 1) out_buffer
         | Some c ->
            append_int out_buffer count;
            Buffer.add_char out_buffer current;
            if Buffer.length out_buffer <= maxblocksize - 4 then
              aux c 1 out_buffer
            else
              (* need to use fun () -> Seq.Cons here instead of Seq.cons to delay computation. Meh *)
              fun () -> Seq.Cons (out_buffer, aux c 1 (Buffer.create maxblocksize))
         | None ->
            append_int out_buffer count;
            Buffer.add_char out_buffer current;
            Seq.return out_buffer in
       match reader_get reader with
       | None -> Seq.empty
       | Some c ->
          aux c 1 (Buffer.create maxblocksize)

let () =
  let n = int_of_string Sys.argv.(1) in
  Seq.iter (Buffer.output_buffer stdout) (look_and_say n);
  Printf.printf "%!"
    
