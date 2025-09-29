module P = C_wrapper.Parquet_reader

type t = P.t

let create = P.create
let next t = P.next t |> Option.map (fun x -> (Obj.magic x : Table.t))
let close = P.close

let iter_batches ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename ~f =
  let t = P.create ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename in
  Fun.protect
    ~finally:(fun () -> close t)
    (fun () ->
      let rec loop_read () =
        match next t with
        | None -> ()
        | Some table ->
          f table;
          loop_read ()
      in
      loop_read ())

let fold_batches
    ?use_threads
    ?column_idxs
    ?mmap
    ?buffer_size
    ?batch_size
    filename
    ~init
    ~f
  =
  let t = P.create ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename in
  Fun.protect
    ~finally:(fun () -> close t)
    (fun () ->
      let rec loop_read acc =
        match next t with
        | None -> acc
        | Some table -> f acc table |> loop_read
      in
      loop_read init)

let schema filename = P.schema filename |> Schema.of_c_wrapper
let schema_and_num_rows filename =
  let schema, rows = P.schema_and_num_rows filename in
  Schema.of_c_wrapper schema, rows
let table ?only_first ?use_threads ?column_idxs filename =
  P.table ?only_first ?use_threads ?column_idxs filename |> (fun x -> (Obj.magic x : Table.t))