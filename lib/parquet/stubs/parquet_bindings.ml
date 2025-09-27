open Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open F

  (* Reader type and functions *)
  module ParquetReader = struct
    type t = unit ptr
    let t : t typ = ptr void

    let open_file = foreign "parquet_open_file"
      (string @-> bool @-> int @-> bool @-> returning t)

    let metadata = foreign "parquet_metadata"
      (t @-> returning (ptr void))

    let schema = foreign "parquet_get_schema"
      (t @-> returning (ptr void))

    let num_rows = foreign "parquet_num_rows"
      (t @-> returning int64_t)

    let num_row_groups = foreign "parquet_num_row_groups"
      (t @-> returning int)

    let row_group_metadata = foreign "parquet_row_group_metadata"
      (t @-> int @-> returning (ptr void))

    let close = foreign "parquet_close_reader"
      (t @-> returning void)
  end

  (* Writer type and functions *)
  module ParquetWriter = struct
    type t = unit ptr
    let t : t typ = ptr void

    let create = foreign "parquet_create_writer"
      (string @-> int @-> int @-> bool @-> int64_t @-> int64_t @-> int64_t @->
       int64_t @-> int @-> string @-> bool @-> ptr void @-> returning t)

    let schema = foreign "parquet_writer_schema"
      (t @-> returning (ptr void))

    let close = foreign "parquet_close_writer"
      (t @-> returning (ptr void))
  end

  (* Metadata extraction functions *)
  let file_version = foreign "parquet_file_version"
    (ptr void @-> returning int)

  let created_by = foreign "parquet_created_by"
    (ptr void @-> returning string_opt)

  let row_group_num_rows = foreign "parquet_row_group_num_rows"
    (ptr void @-> int @-> returning int64_t)

  let row_group_total_byte_size = foreign "parquet_row_group_total_byte_size"
    (ptr void @-> int @-> returning int64_t)

  let row_group_num_columns = foreign "parquet_row_group_num_columns"
    (ptr void @-> int @-> returning int)

  let column_path = foreign "parquet_column_path"
    (ptr void @-> int @-> int @-> returning string)

  let column_num_values = foreign "parquet_column_num_values"
    (ptr void @-> int @-> int @-> returning int64_t)

  let column_compressed_size = foreign "parquet_column_compressed_size"
    (ptr void @-> int @-> int @-> returning int64_t)

  let column_uncompressed_size = foreign "parquet_column_uncompressed_size"
    (ptr void @-> int @-> int @-> returning int64_t)

  let column_compression = foreign "parquet_column_compression"
    (ptr void @-> int @-> int @-> returning int)

  let column_encoding = foreign "parquet_column_encoding"
    (ptr void @-> int @-> int @-> returning int)

  let column_has_statistics = foreign "parquet_column_has_statistics"
    (ptr void @-> int @-> int @-> returning bool)

  let column_null_count = foreign "parquet_column_null_count"
    (ptr void @-> int @-> int @-> returning int64_t)

  let column_distinct_count = foreign "parquet_column_distinct_count"
    (ptr void @-> int @-> int @-> returning int64_t)

  (* Schema inspection functions *)
  let schema_num_columns = foreign "parquet_schema_num_columns"
    (ptr void @-> returning int)

  let schema_column_name = foreign "parquet_schema_column_name"
    (ptr void @-> int @-> returning string)

  let schema_column_physical_type = foreign "parquet_schema_column_physical_type"
    (ptr void @-> int @-> returning int)

  let schema_column_logical_type = foreign "parquet_schema_column_logical_type"
    (ptr void @-> int @-> returning int)

  let schema_column_repetition = foreign "parquet_schema_column_repetition"
    (ptr void @-> int @-> returning int)

  let schema_column_max_definition_level = foreign "parquet_schema_column_max_definition_level"
    (ptr void @-> int @-> returning int)

  let schema_column_max_repetition_level = foreign "parquet_schema_column_max_repetition_level"
    (ptr void @-> int @-> returning int)

  let schema_to_string = foreign "parquet_schema_to_string"
    (ptr void @-> returning string)

  let schema_column_path = foreign "parquet_schema_column_path"
    (ptr void @-> int @-> returning string)

  let schema_column_field_id = foreign "parquet_schema_column_field_id"
    (ptr void @-> int @-> returning int)

  let schema_column_type_length = foreign "parquet_schema_column_type_length"
    (ptr void @-> int @-> returning int)

  let schema_column_decimal_scale = foreign "parquet_schema_column_decimal_scale"
    (ptr void @-> int @-> returning int)

  let schema_column_decimal_precision = foreign "parquet_schema_column_decimal_precision"
    (ptr void @-> int @-> returning int)

  let schema_column_time_unit = foreign "parquet_schema_column_time_unit"
    (ptr void @-> int @-> returning int)

  let schema_column_time_is_adjusted_utc = foreign "parquet_schema_column_time_is_adjusted_utc"
    (ptr void @-> int @-> returning bool)

  (* Utility functions *)
  let version = foreign "parquet_version"
    (void @-> returning string)
end