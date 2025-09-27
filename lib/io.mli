(** Arrow I/O operations - auto-detect format by file extension *)

(** Read a table from a file, auto-detecting format from extension *)
val read : ?columns:[ `Indexes of int list | `Names of string list ] -> string -> Table.t

(** Write a table to a file, auto-detecting format from extension *)
val write : ?chunk_size:int -> ?compression:Compression.t -> Table.t -> string -> unit

(** Get schema from a file, auto-detecting format from extension *)
val schema : string -> Wrapper.Schema.t

(** {1 Format-specific submodules} *)

module CSV : sig
  val read : string -> Table.t
  val write : Table.t -> string -> unit
end

module JSON : sig
  val read : string -> Table.t
  val write : Table.t -> string -> unit
end

module Parquet : sig
  (** Simple read/write operations *)
  val read : ?columns:[ `Indexes of int list | `Names of string list ] -> string -> Table.t
  val write : ?chunk_size:int -> ?compression:Parquet.compression -> Table.t -> string -> unit
  val schema : string -> Wrapper.Schema.t

  (** Batch reading for large files *)
  val read_batches
    :  ?use_threads:bool
    -> ?column_idxs:int list
    -> ?mmap:bool
    -> ?buffer_size:int
    -> ?batch_size:int
    -> string
    -> f:(Table.t -> unit)
    -> unit

  (** Parquet-specific types and operations *)
  type compression = Parquet.compression
  type metadata = Parquet.file_metadata

  val metadata : string -> metadata

  module Reader = Parquet.Reader
  module Writer = Parquet.Writer
end

module Feather : sig
  val read : ?columns:[ `Indexes of int list | `Names of string list ] -> string -> Table.t
  val write : ?chunk_size:int -> ?compression:Compression.t -> Table.t -> string -> unit
  val schema : string -> Wrapper.Schema.t
end