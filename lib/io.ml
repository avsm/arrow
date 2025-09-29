(** Arrow I/O operations *)

let unknown_suffix filename =
  failwith (Printf.sprintf
    "cannot infer the file format from suffix %s (supported suffixes are csv/json/feather/parquet)"
    filename)

let get_file_extension filename =
  try
    let dot_index = String.rindex filename '.' in
    let extension = String.sub filename (dot_index + 1) (String.length filename - dot_index - 1) in
    Some extension
  with Not_found -> None

let column_names_to_indexes col_names schema =
  let col_set = List.fold_left (fun acc name ->
    if List.mem name acc then acc else name :: acc
  ) [] col_names in
  let rec loop acc i = function
    | [] -> List.rev acc
    | child :: rest ->
      let col_name = child.C_wrapper.Schema.name in
      let new_acc = if List.mem col_name col_set then i :: acc else acc in
      loop new_acc (i + 1) rest
  in
  loop [] 0 schema.C_wrapper.Schema.children

let indexes columns ~filename ~schema_fn =
  match columns with
  | `Indexes indexes -> indexes
  | `Names col_names ->
    let schema = schema_fn filename in
    column_names_to_indexes col_names schema

(* Forward declarations for modules *)
module CSV = struct
  let read filename =
    let wrapper_table = C_wrapper.Table.read_csv filename in
    (Obj.magic wrapper_table : Table.t)
  let write _table _filename = failwith "CSV writing not yet implemented"
end

module JSON = struct
  let read filename =
    let wrapper_table = C_wrapper.Table.read_json filename in
    (Obj.magic wrapper_table : Table.t)
  let write _table _filename = failwith "JSON writing not yet implemented"
end

module Feather = struct
  (* Internal Feather reader implementation *)
  let schema filename =
    C_wrapper.C.Feather_reader.schema filename |> C_wrapper.Schema.of_c |> Schema.of_c_wrapper

  let table ?(column_idxs = []) filename =
    let column_idxs = Ctypes.CArray.of_list Ctypes.int column_idxs in
    let t = C_wrapper.C.Feather_reader.read_table
      filename
      (Ctypes.CArray.start column_idxs)
      (Ctypes.CArray.length column_idxs)
    in
    Gc.finalise C_wrapper.C.Table.free t;
    (Obj.magic t : Table.t)

  let read ?columns filename =
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:(fun fn -> schema fn |> Schema.to_c_wrapper))
      | None -> None
    in
    table ?column_idxs filename

  let write ?chunk_size ?compression table filename =
    let wrapper_table = (Obj.magic table : C_wrapper.Table.t) in
    C_wrapper.Table.write_feather ?chunk_size ?compression wrapper_table filename
end

module ParquetIO = struct
  type compression = Parquet.compression
  type metadata = Parquet.file_metadata

  let schema = Parquet.read_schema

  let read ?columns filename =
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:(fun fn ->
         schema fn |> Schema.to_c_wrapper))
      | None -> None
    in
    Parquet.read_table ?column_idxs filename

  let write ?chunk_size ?compression table filename =
    let compression = match compression with
      | Some c ->
        (* Convert Parquet.compression to Compression.t *)
        (match c with
         | (Parquet.Uncompressed : Parquet.compression) -> Compression.None
         | (Parquet.Snappy : Parquet.compression) -> Compression.Snappy
         | (Parquet.Gzip _ : Parquet.compression) -> Compression.Gzip
         | (Parquet.Brotli _ : Parquet.compression) -> Compression.Brotli
         | (Parquet.Lz4 : Parquet.compression) -> failwith "LZ4 compression not supported for Parquet files"
         | (Parquet.Zstd _ : Parquet.compression) -> Compression.Zstd
         | (Parquet.Lzo : Parquet.compression) -> failwith "LZO compression not supported in Arrow")
      | None -> Compression.Snappy
    in
    let wrapper_table = (Obj.magic table : C_wrapper.Table.t) in
    C_wrapper.Table.write_parquet ?chunk_size ~compression wrapper_table filename

  let read_batches ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename ~f =
    Parquet.read_batches ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename ~f

  let metadata filename =
    let reader = Parquet.Reader.open_file filename in
    let meta = Parquet.Reader.metadata reader in
    Parquet.Reader.close reader;
    meta

  module Reader = Parquet.Reader
  module Writer = Parquet.Writer
end

(* Alias for compatibility *)
module Parquet = ParquetIO

let schema filename =
  match get_file_extension filename with
  | Some "csv" -> CSV.read filename |> Table.schema
  | Some "json" -> JSON.read filename |> Table.schema
  | Some "feather" -> Feather.schema filename
  | Some "parquet" -> Parquet.schema filename
  | Some _ | None -> unknown_suffix filename

let read ?columns filename =
  match get_file_extension filename with
  | Some "csv" -> CSV.read filename
  | Some "json" -> JSON.read filename
  | Some "feather" -> Feather.read ?columns filename
  | Some "parquet" -> Parquet.read ?columns filename
  | Some _ | None -> unknown_suffix filename

let write ?chunk_size ?compression table filename =
  match get_file_extension filename with
  | Some "csv" -> failwith "CSV writing not yet supported"
  | Some "json" -> failwith "JSON writing not yet supported"
  | Some "feather" -> Feather.write ?chunk_size ?compression table filename
  | Some "parquet" ->
    (* Convert Compression.t to parquet compression *)
    let parquet_compression = match compression with
      | Some Compression.None -> Some (Uncompressed : Parquet.compression)
      | Some Compression.Snappy -> Some (Snappy : Parquet.compression)
      | Some Compression.Gzip -> Some (Gzip (Some 6) : Parquet.compression)
      | Some Compression.Brotli -> Some (Brotli (Some 6) : Parquet.compression)
      | Some Compression.Zstd -> Some (Zstd 3 : Parquet.compression)
      | Some (Compression.Lz4 | Compression.Lz4_raw) -> failwith "LZ4 compression not supported for Parquet files"
      | None -> None
    in
    Parquet.write ?chunk_size ?compression:parquet_compression table filename
  | Some _ | None -> unknown_suffix filename