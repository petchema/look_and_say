(* Look and Say sequence https://oeis.org/A005150 *)

module StringSequence = struct
  type t = Buffer.t Seq.t

  let of_string s =
    let buffer = Buffer.create (String.length s) in
    Buffer.add_string buffer s;
    Seq.return buffer
end

module Consumer = struct
  type t = {
    mutable block : Buffer.t;
    mutable index : int;
    mutable rest : Buffer.t Seq.t
  }

  let create ss =
    match ss () with
    | Seq.Nil -> {
        block = Buffer.create 0;
        index = 0;
        rest = Seq.empty
      }
    | Seq.Cons (h, t) -> {
        block = h;
        index = 0;
        rest = t
      }

  let rec get consumer =
    if consumer.index < Buffer.length consumer.block then
      let c = Buffer.nth consumer.block consumer.index in
      consumer.index <- consumer.index + 1;
      Some c
    else
      match consumer.rest () with
      | Seq.Nil -> None
      | Seq.Cons (h, t) ->
         consumer.block <- h;
         consumer.index <- 0;
         consumer.rest <- t;
         get consumer
end
                
let maxblocksize = 1024 * 16


let append_int buffer n =
  if n < 10 then
    Buffer.add_char buffer (Char.chr (Char.code '0' + n))
  else
    (* unused unless you use digits >= 4 in seed *)
    Printf.bprintf buffer "%d" n
                 
let rec look_and_say n =
  if n = 1 then
    let buffer1 = Buffer.create 1 in
    Buffer.add_char buffer1 '1';
    Seq.return buffer1
  else
    let prev = look_and_say (n - 1) in
    let consumer = Consumer.create prev in
    match Consumer.get consumer with
    | None -> Seq.empty
    | Some c ->
       let rec aux current count out_buffer =
         match Consumer.get consumer with
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
       aux c 1 (Buffer.create maxblocksize)
       
let () =
  let n = int_of_string Sys.argv.(1) in
  Seq.iter (Buffer.output_buffer stdout) (look_and_say n);
  Printf.printf "%!"
    
