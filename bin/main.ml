(* Look and Say sequence https://oeis.org/A005150 *)

module StringSequence = struct
  type t = Buffer.t Seq.t

  let of_string s =
    let buffer = Buffer.create (String.length s) in
    Buffer.add_string buffer s;
    Seq.return buffer

  let output s channel =
    Seq.iter (Buffer.output_buffer channel) s
end

module Consumer = struct
  type t = {
    mutable block : Buffer.t;
    mutable index : int;
    mutable rest : StringSequence.t
  }

  let create seq =
    match seq () with
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

module Producer = struct
  type t = {
      mutable buffer: Buffer.t
    }

  let create () = {
      buffer = Buffer.create maxblocksize
    }

  let add_pair producer c n =
    let add_int producer n =
      if n < 10 then
        Buffer.add_char producer.buffer (Char.chr (Char.code '0' + n))
      else
        (* unused unless you use digits >= 4 in seed *)
        Printf.bprintf producer.buffer "%d" n in

    let add_char producer c =
      Buffer.add_char producer.buffer c in
    
    add_int producer n;
    add_char producer c

  let push producer cont =
    if Buffer.length producer.buffer <= maxblocksize - 4 then
      cont ()
    else
      let full_buffer = producer.buffer in
      producer.buffer <- Buffer.create maxblocksize;
      (* need to use fun () -> Seq.Cons here instead of Seq.cons to delay computation. Meh *)
      fun () -> Seq.Cons (full_buffer, cont ())
    
  let flush producer =
    Seq.return producer.buffer

  let empty =
    Seq.empty
    
  let of_string s =
    StringSequence.of_string s
end
                
                 
let rec look_and_say n =
  if n = 1 then
    Producer.of_string "1"
  else
    let prev = look_and_say (n - 1) in
    let consumer = Consumer.create prev in
    match Consumer.get consumer with
    | None -> Producer.empty
    | Some c ->
       let producer = Producer.create () in
       let rec aux current count =
         match Consumer.get consumer with
         | Some c when c = current ->
            aux current (count + 1)
         | Some c ->
            Producer.add_pair producer current count;
            Producer.push producer
              (fun () -> aux c 1)
         | None ->
            Producer.add_pair producer current count;
            Producer.flush producer in
       aux c 1
       
let () =
  let n = int_of_string Sys.argv.(1) in
  StringSequence.output (look_and_say n) stdout;
  print_newline ()
    
