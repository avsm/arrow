(** {1 OCaml Apache Arrow Library}

    This library provides OCaml bindings to Apache Arrow, a columnar memory format
    designed for efficient data processing and interchange. Arrow enables zero-copy
    reads, vectorized computation, and seamless data sharing between different
    languages and systems.

    {2 Quick Start}

    The library is organized around a few key concepts:
    - {b Tables}: Collections of named columns with equal lengths
    - {b Columns}: Homogeneous arrays of a single data type
    - {b Schemas}: Descriptions of table structure
    - {b Builders}: Incremental construction of columns and tables
    - {b I/O}: Reading and writing various file formats (Parquet, CSV, JSON, Feather)

    {3 Basic Example}

    {[
      open Arrow

      (* Create a table from arrays *)
      let table = Table.create [
        Table.col [| 1; 2; 3 |] Table.Int "id";
        Table.col [| "Alice"; "Bob"; "Charlie" |] Table.Utf8 "name";
        Table.col [| 25.5; 30.0; 35.5 |] Table.Float "score";
      ]

      (* Write to Parquet file *)
      let () = IO.write table "data.parquet"

      (* Read back with column selection *)
      let table2 = IO.read ~columns:(`Names ["id"; "name"]) "data.parquet"

      (* Access columns *)
      let ids = Table.read table2 Table.Int ~column:(`Name "id")
      let names = Table.read table2 Table.Utf8 ~column:(`Name "name")
    ]}

    {3 Working with Builders}

    For incremental or programmatic table construction:

    {[
      (* Create builders for each column *)
      let int_builder = Builder.Int64.create ()
      let string_builder = Builder.String.create ()

      (* Append values *)
      Builder.Int64.append int_builder 42L;
      Builder.Int64.append_null int_builder;
      Builder.String.append string_builder "hello";

      (* Convert to table *)
      let arrays = [
        Builder.Int64.finish int_builder;
        Builder.String.finish string_builder;
      ]
    ]}

    {3 File I/O}

    The library supports multiple file formats:

    {[
      (* Auto-detect format from extension *)
      let table = IO.read "data.parquet"
      let table = IO.read "data.csv"
      let table = IO.read "data.json"

      (* Format-specific operations *)
      let table = IO.Parquet.read "data.parquet"
      let schema = IO.Parquet.schema "data.parquet"

      (* Batch processing for large files *)
      IO.Parquet.read_batches "large.parquet" ~batch_size:1000
        ~f:(fun batch -> process_batch batch)

      (* Write with compression *)
      IO.Parquet.write table "output.parquet"
        ~compression:(Parquet.Gzip (Some 6))
    ]}

    {3 Note on Compression}

    There are two compression types in the library:
    - {!Compression.t}: Simple compression type without levels (used by Table functions)
    - {!type:Parquet.compression}: Parquet-specific with compression levels

    When using {!Table.write_parquet}, compression levels are not supported.
    For fine-grained control over compression levels, use {!IO.Parquet.write} with
    {!type:Parquet.compression} instead.

    {2 Module Organization}

    The library is organized into several modules, presented here in order of
    typical usage:
*)

(** {2 Core Data Structures} *)

(** Tables - The primary data structure for working with columnar data *)
module Table = Table

(** Column operations - Reading and manipulating table columns *)
module Column = Column

(** Schema information for tables and columns *)
module Schema = Wrapper.Schema

(** {2 Data Construction} *)

(** Builder - Incremental construction of arrays and tables *)
module Builder = Builder

(** {2 File I/O} *)

(** High-level I/O operations with automatic format detection *)
module IO = Io

(** Direct Parquet file format support with advanced features *)
module Parquet = Parquet

(** {2 Data Types and Utilities} *)

(** Arrow data types and type descriptors *)
module Type = Type

(** Time-related types (Date, Time_ns, Span, etc.) *)
module Time = Time

(** Compression codecs for file I/O *)
module Compression = Compression

(** Validity bitmasks for nullable data *)
module Valid = Valid

(** {2 Version Information} *)

(** Library version string *)
val version : string

(** {2 Internal Modules}

    These modules are exposed for advanced use cases but most users should
    use the higher-level modules above:

    - Wrapper: Low-level FFI bindings (not exposed)
    - Parquet_reader: Internal Parquet reading (use Parquet module instead)
*)
