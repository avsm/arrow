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
      let col_name = child.Wrapper.Schema.name in
      let new_acc = if List.mem col_name col_set then i :: acc else acc in
      loop new_acc (i + 1) rest
  in
  loop [] 0 schema.Wrapper.Schema.children

let indexes columns ~filename ~schema_fn =
  match columns with
  | `Indexes indexes -> indexes
  | `Names col_names ->
    let schema = schema_fn filename in
    column_names_to_indexes col_names schema

let schema filename =
  match get_file_extension filename with
  | Some "csv" -> Table.read_csv filename |> Table.schema
  | Some "json" -> Table.read_json filename |> Table.schema
  | Some "feather" -> Wrapper.Feather_reader.schema filename
  | Some "parquet" -> Parquet.read_schema filename
  | Some _ | None -> unknown_suffix filename

let read ?columns filename =
  match get_file_extension filename with
  | Some "csv" -> Table.read_csv filename
  | Some "json" -> Table.read_json filename
  | Some "feather" ->
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:Wrapper.Feather_reader.schema)
      | None -> None
    in
    Wrapper.Feather_reader.table ?column_idxs filename
  | Some "parquet" ->
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:Parquet.read_schema)
      | None -> None
    in
    Parquet.read_table ?column_idxs filename
  | Some _ | None -> unknown_suffix filename

let write ?chunk_size ?compression table filename =
  match get_file_extension filename with
  | Some "csv" -> failwith "CSV writing not yet supported"
  | Some "json" -> failwith "JSON writing not yet supported"
  | Some "feather" -> Table.write_feather ?chunk_size ?compression table filename
  | Some "parquet" -> Table.write_parquet ?chunk_size ?compression table filename
  | Some _ | None -> unknown_suffix filename

module CSV = struct
  let read = Table.read_csv
  let write _table _filename = failwith "CSV writing not yet implemented"
end

module JSON = struct
  let read = Table.read_json
  let write _table _filename = failwith "JSON writing not yet implemented"
end

module Parquet = struct
  type compression = Parquet.compression
  type metadata = Parquet.file_metadata

  let schema = Parquet.read_schema

  let read ?columns filename =
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:Parquet.read_schema)
      | None -> None
    in
    Parquet.read_table ?column_idxs filename

  let write ?chunk_size ?compression table filename =
    let compression = match compression with
      | Some c ->
        (* Convert Parquet.compression to Compression.t *)
        (match c with
         | Parquet.Uncompressed -> Compression.None
         | Parquet.Snappy -> Compression.Snappy
         | Parquet.Gzip _ -> Compression.Gzip
         | Parquet.Brotli _ -> Compression.Brotli
         | Parquet.Lz4 -> failwith "LZ4 compression not supported for Parquet files"
         | Parquet.Zstd _ -> Compression.Zstd
         | Parquet.Lzo -> failwith "LZO compression not supported in Arrow")
      | None -> Compression.Snappy
    in
    Table.write_parquet ?chunk_size ~compression table filename

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

module Feather = struct
  let schema = Wrapper.Feather_reader.schema

  let read ?columns filename =
    let column_idxs = match columns with
      | Some cols -> Some (indexes cols ~filename ~schema_fn:Wrapper.Feather_reader.schema)
      | None -> None
    in
    Wrapper.Feather_reader.table ?column_idxs filename

  let write ?chunk_size ?compression table filename =
    Table.write_feather ?chunk_size ?compression table filename
end