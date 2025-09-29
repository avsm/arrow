(** Apache Parquet columnar file format support for OCaml *)

(** {1 Compression Codecs} *)

type compression =
  | Uncompressed
  | Snappy
  | Gzip of int option  (** Gzip with optional compression level (1-9, default 6) *)
  | Brotli of int option  (** Brotli with optional compression level (0-11, default 1) *)
  | Lz4
  | Zstd of int  (** Zstd with compression level (1-22, default 3) *)
  | Lzo

(** {1 Encodings} *)

type encoding =
  | Plain
  | Dictionary
  | Run_length
  | Bit_packed
  | Delta_binary_packed
  | Delta_length_byte_array
  | Delta_byte_array
  | Rle_dictionary

(** {1 Statistics} *)

type statistics = {
  null_count : int64 option;
  distinct_count : int64 option;
  min : string option;  (** Min value as bytes *)
  max : string option;  (** Max value as bytes *)
}

(** {1 Column Metadata} *)

type column_metadata = {
  path : string list;  (** Column path for nested schemas *)
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

(** {1 Row Group Metadata} *)

type row_group_metadata = {
  num_rows : int64;
  total_byte_size : int64;
  columns : column_metadata list;
  sorting_columns : int list option;  (** Column indices for sorting *)
}

(** {1 File Metadata} *)

type file_metadata = {
  version : int;  (** Parquet format version *)
  schema_json : string;  (** JSON representation of the schema *)
  num_rows : int64;
  row_groups : row_group_metadata list;
  created_by : string option;  (** Creator library/version *)
  key_value_metadata : (string * string) list;
}

(** {1 Writer Properties} *)

type writer_version = V1 | V2

type writer_properties = {
  compression : compression;
  compression_level : int option;  (** For codecs that support levels *)
  dictionary_enabled : bool;
  dictionary_page_size : int64;  (** Bytes *)
  data_page_size : int64;  (** Target page size in bytes *)
  write_batch_size : int64;  (** Rows per batch *)
  max_row_group_length : int64;  (** Max rows per row group *)
  writer_version : writer_version;
  created_by : string;
  enable_statistics : bool;
}

val default_writer_properties : writer_properties
(** Default writer properties with reasonable defaults *)

(** {1 Reader Options} *)

type reader_properties = {
  use_memory_map : bool;  (** Memory-map the file if possible *)
  buffer_size : int;  (** Read buffer size *)
  prefetch_row_groups : bool;  (** Prefetch row groups for better performance *)
  verify_checksums : bool;  (** Verify page checksums *)
}

val default_reader_properties : reader_properties

(** {1 Schema Types} *)

(** Parquet logical types *)
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

(** Parquet physical types *)
type physical_type =
  | Boolean
  | Int32
  | Int64
  | Int96  (** Deprecated, used for legacy timestamps *)
  | Float
  | Double
  | Byte_array
  | Fixed_len_byte_array of int

(** Field repetition type *)
type repetition =
  | Required
  | Optional
  | Repeated

(** Schema node *)
type schema_node = {
  name : string;
  physical_type : physical_type option;  (** None for group nodes *)
  logical_type : logical_type option;
  repetition : repetition;
  children : schema_node list;  (** Empty for primitive types *)
}

(** {1 Reading} *)

module Reader : sig
  type t
  (** Reader handle *)

  val open_file : ?properties:reader_properties -> string -> t
  (** Open a Parquet file for reading *)

  val metadata : t -> file_metadata
  (** Get file metadata *)

  val schema : t -> schema_node
  (** Get the file schema *)

  val num_rows : t -> int64
  (** Total number of rows in the file *)

  val num_row_groups : t -> int
  (** Number of row groups *)

  val row_group_metadata : t -> int -> row_group_metadata
  (** Get metadata for a specific row group *)

  (** Column reading will be in the Arrow bridge module *)

  val close : t -> unit
  (** Close the reader and free resources *)
end

(** {2 High-level Table Reading} *)

val read_table
  :  ?only_first:int
  -> ?use_threads:bool
  -> ?column_idxs:int list
  -> string
  -> Table.t
(** Read a Parquet file into an Arrow table.
    @param only_first Read only the first n rows
    @param use_threads Use multiple threads for reading
    @param column_idxs Column indices to read (None means all columns) *)

val read_schema : string -> Schema.t
(** Get the Arrow schema from a Parquet file *)

val read_batches
  :  ?use_threads:bool
  -> ?column_idxs:int list
  -> ?mmap:bool
  -> ?buffer_size:int
  -> ?batch_size:int
  -> string
  -> f:(Table.t -> unit)
  -> unit
(** Read a Parquet file in batches, calling the given function for each batch *)

(** {1 Writing} *)

module Writer : sig
  type t
  (** Writer handle *)

  val create : ?properties:writer_properties -> string -> schema_node -> t
  (** Create a new Parquet file writer *)

  val schema : t -> schema_node
  (** Get the schema being written *)

  (** Actual writing will be in the Arrow bridge module *)

  val close : t -> file_metadata
  (** Close the writer and return final metadata *)

  (** Internal functions exposed for testing *)
  val compression_to_int : compression -> int
  (** Convert compression type to integer for FFI *)

  val compression_level : compression -> int
  (** Extract compression level from compression type *)
end

(** {1 Utilities} *)

val version : unit -> string
(** Get the Parquet library version *)

val validate_file : string -> (unit, string) result
(** Validate a Parquet file structure without fully reading it *)

val print_metadata : ?verbose:bool -> string -> unit
(** Print file metadata to stdout *)

val print_schema : string -> unit
(** Print file schema to stdout *)

(** {1 Pretty Printers} *)

val pp_compression : Format.formatter -> compression -> unit
val pp_encoding : Format.formatter -> encoding -> unit
val pp_statistics : Format.formatter -> statistics -> unit
val pp_column_metadata : Format.formatter -> column_metadata -> unit
val pp_row_group_metadata : Format.formatter -> row_group_metadata -> unit
val pp_file_metadata : Format.formatter -> file_metadata -> unit
val pp_writer_version : Format.formatter -> writer_version -> unit
val pp_logical_type : Format.formatter -> logical_type -> unit
val pp_physical_type : Format.formatter -> physical_type -> unit
val pp_repetition : Format.formatter -> repetition -> unit
val pp_schema_node : Format.formatter -> schema_node -> unit