(** Apache Parquet columnar file format support for OCaml *)

(** {1 Types} *)

type compression =
  | Uncompressed
  | Snappy
  | Gzip of int option
  | Brotli of int option
  | Lz4
  | Zstd of int
  | Lzo

type encoding =
  | Plain
  | Dictionary
  | Run_length
  | Bit_packed
  | Delta_binary_packed
  | Delta_length_byte_array
  | Delta_byte_array
  | Rle_dictionary

type statistics = {
  null_count : int64 option;
  distinct_count : int64 option;
  min : string option;
  max : string option;
}

type column_metadata = {
  path : string list;
  type_name : string;
  encodings : encoding list;
  compression : compression;
  num_values : int64;
  total_uncompressed_size : int64;
  total_compressed_size : int64;
  data_page_offset : int64;
  index_page_offset : int64 option;
  dictionary_page_offset : int64 option;
  statistics : statistics option;
}

type row_group_metadata = {
  num_rows : int64;
  total_byte_size : int64;
  columns : column_metadata list;
  sorting_columns : int list option;
}

type file_metadata = {
  version : int;
  schema_json : string;
  num_rows : int64;
  row_groups : row_group_metadata list;
  created_by : string option;
  key_value_metadata : (string * string) list;
}

type writer_version = V1 | V2

type writer_properties = {
  compression : compression;
  compression_level : int option;
  dictionary_enabled : bool;
  dictionary_page_size : int64;
  data_page_size : int64;
  write_batch_size : int64;
  max_row_group_length : int64;
  writer_version : writer_version;
  created_by : string;
  enable_statistics : bool;
}

let default_writer_properties = {
  compression = Snappy;
  compression_level = None;
  dictionary_enabled = true;
  dictionary_page_size = Int64.mul 1024L 1024L;  (* 1MB *)
  data_page_size = Int64.mul 1024L 1024L;  (* 1MB *)
  write_batch_size = 1024L;
  max_row_group_length = Int64.mul 1024L 1024L;  (* 1M rows *)
  writer_version = V2;
  created_by = "ocaml-parquet";
  enable_statistics = true;
}

type reader_properties = {
  use_memory_map : bool;
  buffer_size : int;
  prefetch_row_groups : bool;
  verify_checksums : bool;
}

let default_reader_properties = {
  use_memory_map = true;
  buffer_size = 8192;
  prefetch_row_groups = false;
  verify_checksums = false;
}

type logical_type =
  | No_logical_type
  | String
  | Map
  | List
  | Enum
  | Decimal of { scale : int; precision : int }
  | Date
  | Time of { is_adjusted_utc : bool; unit : [ `Millis | `Micros | `Nanos ] }
  | Timestamp of { is_adjusted_utc : bool; unit : [ `Millis | `Micros | `Nanos ] }
  | Integer of { bit_width : int; is_signed : bool }
  | Json
  | Bson
  | Uuid

type physical_type =
  | Boolean
  | Int32
  | Int64
  | Int96
  | Float
  | Double
  | Byte_array
  | Fixed_len_byte_array of int

type repetition =
  | Required
  | Optional
  | Repeated

type schema_node = {
  name : string;
  physical_type : physical_type option;
  logical_type : logical_type option;
  repetition : repetition;
  children : schema_node list;
}

(** {1 Pretty Printers} *)

let pp_compression fmt = function
  | Uncompressed -> Format.fprintf fmt "Uncompressed"
  | Snappy -> Format.fprintf fmt "Snappy"
  | Gzip None -> Format.fprintf fmt "Gzip"
  | Gzip (Some level) -> Format.fprintf fmt "Gzip(%d)" level
  | Brotli None -> Format.fprintf fmt "Brotli"
  | Brotli (Some level) -> Format.fprintf fmt "Brotli(%d)" level
  | Lz4 -> Format.fprintf fmt "Lz4"
  | Zstd level -> Format.fprintf fmt "Zstd(%d)" level
  | Lzo -> Format.fprintf fmt "Lzo"

let pp_encoding fmt = function
  | Plain -> Format.fprintf fmt "Plain"
  | Dictionary -> Format.fprintf fmt "Dictionary"
  | Run_length -> Format.fprintf fmt "Run_length"
  | Bit_packed -> Format.fprintf fmt "Bit_packed"
  | Delta_binary_packed -> Format.fprintf fmt "Delta_binary_packed"
  | Delta_length_byte_array -> Format.fprintf fmt "Delta_length_byte_array"
  | Delta_byte_array -> Format.fprintf fmt "Delta_byte_array"
  | Rle_dictionary -> Format.fprintf fmt "Rle_dictionary"

let pp_int64 fmt n = Format.fprintf fmt "%Ld" n

let pp_option pp fmt = function
  | None -> Format.fprintf fmt "None"
  | Some x -> Format.fprintf fmt "Some(%a)" pp x

let pp_statistics fmt stats =
  Format.fprintf fmt "{null_count=%a; distinct_count=%a; min=%a; max=%a}"
    (pp_option pp_int64) stats.null_count
    (pp_option pp_int64) stats.distinct_count
    (pp_option Format.pp_print_string) stats.min
    (pp_option Format.pp_print_string) stats.max

let pp_list pp fmt list =
  Format.fprintf fmt "[@[<hov>%a@]]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp) list

let pp_column_metadata fmt meta =
  Format.fprintf fmt "{path=%a; type_name=%s; encodings=%a; compression=%a; \
    num_values=%Ld; total_uncompressed_size=%Ld; total_compressed_size=%Ld; \
    data_page_offset=%Ld; index_page_offset=%a; dictionary_page_offset=%a; \
    statistics=%a}"
    (pp_list Format.pp_print_string) meta.path
    meta.type_name
    (pp_list pp_encoding) meta.encodings
    pp_compression meta.compression
    meta.num_values
    meta.total_uncompressed_size
    meta.total_compressed_size
    meta.data_page_offset
    (pp_option pp_int64) meta.index_page_offset
    (pp_option pp_int64) meta.dictionary_page_offset
    (pp_option pp_statistics) meta.statistics

let pp_row_group_metadata fmt (meta : row_group_metadata) =
  Format.fprintf fmt "{num_rows=%Ld; total_byte_size=%Ld; columns=%a; sorting_columns=%a}"
    meta.num_rows
    meta.total_byte_size
    (pp_list pp_column_metadata) meta.columns
    (pp_option (pp_list Format.pp_print_int)) meta.sorting_columns

let pp_pair pp_k pp_v fmt (k, v) =
  Format.fprintf fmt "(%a, %a)" pp_k k pp_v v

let pp_file_metadata fmt meta =
  Format.fprintf fmt "{version=%d; schema_json=%s; num_rows=%Ld; row_groups=%a; \
    created_by=%a; key_value_metadata=%a}"
    meta.version
    meta.schema_json
    meta.num_rows
    (pp_list pp_row_group_metadata) meta.row_groups
    (pp_option Format.pp_print_string) meta.created_by
    (pp_list (pp_pair Format.pp_print_string Format.pp_print_string)) meta.key_value_metadata

let pp_writer_version fmt = function
  | V1 -> Format.fprintf fmt "V1"
  | V2 -> Format.fprintf fmt "V2"

let pp_physical_type fmt = function
  | Boolean -> Format.fprintf fmt "Boolean"
  | Int32 -> Format.fprintf fmt "Int32"
  | Int64 -> Format.fprintf fmt "Int64"
  | Int96 -> Format.fprintf fmt "Int96"
  | Float -> Format.fprintf fmt "Float"
  | Double -> Format.fprintf fmt "Double"
  | Byte_array -> Format.fprintf fmt "Byte_array"
  | Fixed_len_byte_array n -> Format.fprintf fmt "Fixed_len_byte_array(%d)" n

let pp_time_unit fmt = function
  | `Millis -> Format.fprintf fmt "Millis"
  | `Micros -> Format.fprintf fmt "Micros"
  | `Nanos -> Format.fprintf fmt "Nanos"

let pp_logical_type fmt = function
  | No_logical_type -> Format.fprintf fmt "No_logical_type"
  | String -> Format.fprintf fmt "String"
  | Map -> Format.fprintf fmt "Map"
  | List -> Format.fprintf fmt "List"
  | Enum -> Format.fprintf fmt "Enum"
  | Decimal { scale; precision } -> Format.fprintf fmt "Decimal{scale=%d; precision=%d}" scale precision
  | Date -> Format.fprintf fmt "Date"
  | Time { is_adjusted_utc; unit } ->
      Format.fprintf fmt "Time{is_adjusted_utc=%b; unit=%a}" is_adjusted_utc pp_time_unit unit
  | Timestamp { is_adjusted_utc; unit } ->
      Format.fprintf fmt "Timestamp{is_adjusted_utc=%b; unit=%a}" is_adjusted_utc pp_time_unit unit
  | Integer { bit_width; is_signed } ->
      Format.fprintf fmt "Integer{bit_width=%d; is_signed=%b}" bit_width is_signed
  | Json -> Format.fprintf fmt "Json"
  | Bson -> Format.fprintf fmt "Bson"
  | Uuid -> Format.fprintf fmt "Uuid"

let pp_repetition fmt = function
  | Required -> Format.fprintf fmt "Required"
  | Optional -> Format.fprintf fmt "Optional"
  | Repeated -> Format.fprintf fmt "Repeated"

let rec pp_schema_node fmt node =
  Format.fprintf fmt "{name=%s; physical_type=%a; logical_type=%a; repetition=%a; children=%a}"
    node.name
    (pp_option pp_physical_type) node.physical_type
    (pp_option pp_logical_type) node.logical_type
    pp_repetition node.repetition
    (pp_list pp_schema_node) node.children

(** {1 FFI} *)

module C = Parquet_bindings.C (Parquet_bindings_generated)

(** {1 Reader} *)

module Reader = struct
  type t = C.ParquetReader.t

  let open_file ?(properties = default_reader_properties) filename =
    C.ParquetReader.open_file
      filename
      properties.use_memory_map
      properties.buffer_size
      properties.verify_checksums

  let metadata t =
    let num_rows = C.ParquetReader.num_rows t in
    let num_row_groups = C.ParquetReader.num_row_groups t in
    let version = C.file_version t in
    let created_by = C.created_by t in

    (* Convert compression from int *)
    let compression_from_int = function
      | 0 -> Uncompressed
      | 1 -> Snappy
      | 2 -> Gzip None  (* Can't get level from metadata *)
      | 3 -> Brotli None  (* Can't get level from metadata *)
      | 4 -> Lz4
      | 5 -> Lzo
      | 6 -> Zstd 3  (* Default ZSTD level since we can't get level from metadata *)
      | _ -> Snappy
    in

    (* Convert encoding from int *)
    let encoding_from_int = function
      | 0 -> Plain
      | 2 -> Run_length
      | 3 -> Bit_packed
      | 4 -> Delta_binary_packed
      | 5 -> Delta_length_byte_array
      | 6 -> Delta_byte_array
      | 8 -> Rle_dictionary
      | _ -> Plain
    in

    let row_groups = List.init num_row_groups (fun rg_idx ->
      let rg_num_rows = C.row_group_num_rows t rg_idx in
      let rg_byte_size = C.row_group_total_byte_size t rg_idx in
      let num_cols = C.row_group_num_columns t rg_idx in

      let columns = List.init num_cols (fun col_idx ->
        let path_str = C.column_path t rg_idx col_idx in
        let path = if path_str = "" then [] else String.split_on_char '.' path_str in
        let comp = C.column_compression t rg_idx col_idx in
        let enc = C.column_encoding t rg_idx col_idx in
        let has_stats = C.column_has_statistics t rg_idx col_idx in

        let statistics = if has_stats then
          let null_count = C.column_null_count t rg_idx col_idx in
          let distinct_count = C.column_distinct_count t rg_idx col_idx in
          Some {
            null_count = if null_count < 0L then None else Some null_count;
            distinct_count = if distinct_count < 0L then None else Some distinct_count;
            min = None;  (* Would need binary data extraction *)
            max = None;  (* Would need binary data extraction *)
          }
        else None in

        {
          path;
          type_name = "";  (* Would need schema info *)
          encodings = [encoding_from_int enc];
          compression = compression_from_int comp;
          num_values = C.column_num_values t rg_idx col_idx;
          total_uncompressed_size = C.column_uncompressed_size t rg_idx col_idx;
          total_compressed_size = C.column_compressed_size t rg_idx col_idx;
          data_page_offset = 0L;  (* Would need more C++ functions *)
          index_page_offset = None;
          dictionary_page_offset = None;
          statistics;
        }
      ) in

      {
        num_rows = rg_num_rows;
        total_byte_size = rg_byte_size;
        columns;
        sorting_columns = None;
      }
    ) in

    {
      version;
      schema_json = "{}";  (* Would need schema serialization *)
      num_rows;
      row_groups;
      created_by;
      key_value_metadata = [];  (* Would need C++ function *)
    }

  let schema t =
    let schema_ptr = C.ParquetReader.schema t in
    let num_columns = C.schema_num_columns schema_ptr in

    (* Convert physical type from int *)
    let physical_type_from_int = function
      | 0 -> Some Boolean
      | 1 -> Some Int32
      | 2 -> Some Int64
      | 3 -> Some Int96
      | 4 -> Some Float
      | 5 -> Some Double
      | 6 -> Some Byte_array
      | 7 -> Some (Fixed_len_byte_array 0)  (* Size would need separate call *)
      | _ -> None
    in

    (* Convert time unit from int *)
    let time_unit_from_int = function
      | 0 -> `Millis
      | 1 -> `Micros
      | 2 -> `Nanos
      | _ -> `Millis
    in

    (* Convert logical type from int *)
    let logical_type_from_int schema_ptr i = function
      | 0 -> Some String
      | 1 -> Some Map
      | 2 -> Some List
      | 3 -> Some Enum
      | 4 ->
          let scale = C.schema_column_decimal_scale schema_ptr i in
          let precision = C.schema_column_decimal_precision schema_ptr i in
          Some (Decimal { scale; precision })
      | 5 -> Some Date
      | 6 ->
          let unit = time_unit_from_int (C.schema_column_time_unit schema_ptr i) in
          let is_adjusted_utc = C.schema_column_time_is_adjusted_utc schema_ptr i in
          Some (Time { is_adjusted_utc; unit })
      | 7 ->
          let unit = time_unit_from_int (C.schema_column_time_unit schema_ptr i) in
          let is_adjusted_utc = C.schema_column_time_is_adjusted_utc schema_ptr i in
          Some (Timestamp { is_adjusted_utc; unit })
      | 8 -> Some (Integer { bit_width = 32; is_signed = true })  (* Would need params *)
      | 9 -> Some Json
      | 10 -> Some Bson
      | 11 -> Some Uuid
      | _ -> Some No_logical_type
    in

    (* Convert repetition from int *)
    let repetition_from_int = function
      | 0 -> Required
      | 1 -> Optional
      | 2 -> Repeated
      | _ -> Required
    in

    (* Build column nodes *)
    let columns = List.init num_columns (fun i ->
      let name = C.schema_column_name schema_ptr i in
      let phys_type = C.schema_column_physical_type schema_ptr i in
      let log_type = C.schema_column_logical_type schema_ptr i in
      let rep = C.schema_column_repetition schema_ptr i in
      {
        name;
        physical_type = physical_type_from_int phys_type;
        logical_type = logical_type_from_int schema_ptr i log_type;
        repetition = repetition_from_int rep;
        children = [];  (* Leaf nodes have no children *)
      }
    ) in

    (* Return root schema with columns as children *)
    {
      name = "schema";
      physical_type = None;
      logical_type = None;
      repetition = Required;
      children = columns;
    }

  let num_rows t = C.ParquetReader.num_rows t

  let num_row_groups t = C.ParquetReader.num_row_groups t

  let row_group_metadata t idx =
    let num_row_groups = C.ParquetReader.num_row_groups t in
    if idx < 0 || idx >= num_row_groups then
      invalid_arg "row_group_metadata: index out of bounds";

    (* Get full metadata and extract the specific row group *)
    let meta = metadata t in
    List.nth meta.row_groups idx

  let close t = C.ParquetReader.close t
end

(** {1 Writer} *)

module Writer = struct
  type t = C.ParquetWriter.t

  let compression_to_int = function
    | Uncompressed -> 0
    | Snappy -> 1
    | Gzip _ -> 2
    | Brotli _ -> 3
    | Lz4 -> 4
    | Zstd _ -> 6
    | Lzo -> 5

  let compression_level = function
    | Gzip (Some level) -> level
    | Gzip None -> 6  (* Default gzip level *)
    | Brotli (Some level) -> level
    | Brotli None -> 1  (* Default brotli level *)
    | Zstd level -> level
    | _ -> 0

  let create ?(properties = default_writer_properties) filename schema =
    (* For now, ignore the schema parameter and pass nullptr *)
    let _ = schema in
    C.ParquetWriter.create
      filename
      (compression_to_int properties.compression)
      (compression_level properties.compression)
      properties.dictionary_enabled
      properties.dictionary_page_size
      properties.data_page_size
      properties.write_batch_size
      properties.max_row_group_length
      (match properties.writer_version with V1 -> 1 | V2 -> 2)
      properties.created_by
      properties.enable_statistics
      Ctypes.null

  let schema t =
    (* Writer schema - would need proper implementation *)
    let _ = C.ParquetWriter.schema t in
    {
      name = "root";
      physical_type = None;
      logical_type = None;
      repetition = Required;
      children = [];  (* Would need to track the schema passed at creation *)
    }

  let close t =
    (* Writer close - returns basic metadata *)
    let _ = C.ParquetWriter.close t in
    {
      version = 2;
      schema_json = "{}";  (* Would need to serialize the schema *)
      num_rows = 0L;  (* Would need to track rows written *)
      row_groups = [];  (* Would need to track row groups *)
      created_by = Some "ocaml-parquet";
      key_value_metadata = [];
    }
end

(** {1 Utilities} *)

let version () = C.version ()

let validate_file filename =
  try
    let reader = Reader.open_file filename in
    let _ = Reader.metadata reader in
    Reader.close reader;
    Ok ()
  with
  | e -> Error (Printexc.to_string e)

let print_metadata ?(verbose = false) filename =
  let reader = Reader.open_file filename in
  let meta = Reader.metadata reader in
  Printf.printf "Parquet file: %s\n" filename;
  Printf.printf "Version: %d\n" meta.version;
  Printf.printf "Total rows: %Ld\n" meta.num_rows;
  Printf.printf "Row groups: %d\n" (List.length meta.row_groups);
  (match meta.created_by with
  | Some creator -> Printf.printf "Created by: %s\n" creator
  | None -> ());
  if verbose then begin
    Printf.printf "Schema: %s\n" meta.schema_json;
    List.iteri (fun i (rg : row_group_metadata) ->
      Printf.printf "  Row group %d: %Ld rows, %Ld bytes\n"
        i rg.num_rows rg.total_byte_size
    ) meta.row_groups
  end;
  Reader.close reader

let print_schema filename =
  let reader = Reader.open_file filename in
  let schema = Reader.schema reader in
  Format.printf "%a@." pp_schema_node schema;
  Reader.close reader

(* High-level table reading functions *)
let read_table ?only_first ?use_threads ?column_idxs filename =
  Parquet_reader.table ?only_first ?use_threads ?column_idxs filename

let read_schema filename =
  Parquet_reader.schema filename

let read_batches ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename ~f =
  Parquet_reader.iter_batches ?use_threads ?column_idxs ?mmap ?buffer_size ?batch_size filename ~f