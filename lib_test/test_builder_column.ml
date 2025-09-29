(** Comprehensive tests for Builder.Column interface covering all data types *)
open Arrow
open Test_fixtures

(** {2 Basic Column Builder Tests} *)

let test_int8_column_builder () =
  let builder = Builder.Column.Int8.create () in

  (* Test basic operations *)
  Builder.Column.Int8.append builder 127;
  Builder.Column.Int8.append builder (-128);
  Builder.Column.Int8.append builder 0;
  Builder.Column.Int8.append_null builder;

  Alcotest.(check int) "Int8 builder length" 4 (Builder.Column.Int8.length builder);
  Alcotest.(check int) "Int8 builder null count" 1 (Builder.Column.Int8.null_count builder);

  (* Test append_opt *)
  let opt_builder = Builder.Column.Int8.create () in
  Builder.Column.Int8.append_opt opt_builder (Some 42);
  Builder.Column.Int8.append_opt opt_builder None;
  Builder.Column.Int8.append_opt opt_builder (Some (-50));

  Alcotest.(check int) "Int8 optional length" 3 (Builder.Column.Int8.length opt_builder);
  Alcotest.(check int) "Int8 optional null count" 1 (Builder.Column.Int8.null_count opt_builder);

  (* Test append_many *)
  let many_builder = Builder.Column.Int8.create () in
  let values = [|1; 2; 3; 4; 5|] in
  Builder.Column.Int8.append_many many_builder values;

  Alcotest.(check int) "Int8 many values length" 5 (Builder.Column.Int8.length many_builder);
  Alcotest.(check int) "Int8 many values null count" 0 (Builder.Column.Int8.null_count many_builder)

let test_int16_column_builder () =
  let builder = Builder.Column.Int16.create () in

  Builder.Column.Int16.append builder 32767;
  Builder.Column.Int16.append builder (-32768);
  Builder.Column.Int16.append builder 0;
  Builder.Column.Int16.append_null builder;

  Alcotest.(check int) "Int16 builder length" 4 (Builder.Column.Int16.length builder);
  Alcotest.(check int) "Int16 builder null count" 1 (Builder.Column.Int16.null_count builder);

  (* Test edge cases *)
  let edge_builder = Builder.Column.Int16.create () in
  let edge_values = [|32767; -32768; 0; 1; -1|] in
  Builder.Column.Int16.append_many edge_builder edge_values;

  Alcotest.(check int) "Int16 edge values length" 5 (Builder.Column.Int16.length edge_builder)

let test_int32_column_builder () =
  let builder = Builder.Column.Int32.create () in

  Builder.Column.Int32.append builder Int32.max_int;
  Builder.Column.Int32.append builder Int32.min_int;
  Builder.Column.Int32.append builder 0l;
  Builder.Column.Int32.append_null ~n:2 builder;

  Alcotest.(check int) "Int32 builder length" 5 (Builder.Column.Int32.length builder);
  Alcotest.(check int) "Int32 builder null count" 2 (Builder.Column.Int32.null_count builder);

  (* Test with sample data *)
  let sample_builder = Builder.Column.Int32.create () in
  let sample_data = sample_int32s ~size:10 () in
  Builder.Column.Int32.append_many sample_builder sample_data;

  Alcotest.(check int) "Int32 sample data length" 10 (Builder.Column.Int32.length sample_builder)

let test_int64_column_builder () =
  let builder = Builder.Column.Int64.create () in

  Builder.Column.Int64.append builder Int64.max_int;
  Builder.Column.Int64.append builder Int64.min_int;
  Builder.Column.Int64.append builder 0L;
  Builder.Column.Int64.append_null builder;

  Alcotest.(check int) "Int64 builder length" 4 (Builder.Column.Int64.length builder);
  Alcotest.(check int) "Int64 builder null count" 1 (Builder.Column.Int64.null_count builder);

  (* Test with sample data *)
  let sample_builder = Builder.Column.Int64.create () in
  let sample_data = sample_int64s ~size:15 () in
  Builder.Column.Int64.append_many sample_builder sample_data;

  Alcotest.(check int) "Int64 sample data length" 15 (Builder.Column.Int64.length sample_builder)

let test_uint8_column_builder () =
  let builder = Builder.Column.UInt8.create () in

  Builder.Column.UInt8.append builder 255;
  Builder.Column.UInt8.append builder 0;
  Builder.Column.UInt8.append builder 128;
  Builder.Column.UInt8.append_null builder;

  Alcotest.(check int) "UInt8 builder length" 4 (Builder.Column.UInt8.length builder);
  Alcotest.(check int) "UInt8 builder null count" 1 (Builder.Column.UInt8.null_count builder);

  (* Test boundary values *)
  let boundary_builder = Builder.Column.UInt8.create () in
  let boundary_values = [|0; 1; 127; 128; 255|] in
  Builder.Column.UInt8.append_many boundary_builder boundary_values;

  Alcotest.(check int) "UInt8 boundary values length" 5 (Builder.Column.UInt8.length boundary_builder)

let test_uint16_column_builder () =
  let builder = Builder.Column.UInt16.create () in

  Builder.Column.UInt16.append builder 65535;
  Builder.Column.UInt16.append builder 0;
  Builder.Column.UInt16.append builder 32768;
  Builder.Column.UInt16.append_null builder;

  Alcotest.(check int) "UInt16 builder length" 4 (Builder.Column.UInt16.length builder);
  Alcotest.(check int) "UInt16 builder null count" 1 (Builder.Column.UInt16.null_count builder)

let test_uint32_column_builder () =
  let builder = Builder.Column.UInt32.create () in

  Builder.Column.UInt32.append builder (-1l); (* max uint32 as signed int32 *)
  Builder.Column.UInt32.append builder 0l;
  Builder.Column.UInt32.append builder 2147483647l;
  Builder.Column.UInt32.append_null builder;

  Alcotest.(check int) "UInt32 builder length" 4 (Builder.Column.UInt32.length builder);
  Alcotest.(check int) "UInt32 builder null count" 1 (Builder.Column.UInt32.null_count builder)

let test_uint64_column_builder () =
  let builder = Builder.Column.UInt64.create () in

  Builder.Column.UInt64.append builder (-1L); (* max uint64 as signed int64 *)
  Builder.Column.UInt64.append builder 0L;
  Builder.Column.UInt64.append builder Int64.max_int;
  Builder.Column.UInt64.append_null builder;

  Alcotest.(check int) "UInt64 builder length" 4 (Builder.Column.UInt64.length builder);
  Alcotest.(check int) "UInt64 builder null count" 1 (Builder.Column.UInt64.null_count builder)

let test_float_column_builder () =
  let builder = Builder.Column.Float.create () in

  Builder.Column.Float.append builder 3.14;
  Builder.Column.Float.append builder (-2.71);
  Builder.Column.Float.append builder 0.0;
  Builder.Column.Float.append builder Float.infinity;
  Builder.Column.Float.append builder Float.neg_infinity;
  Builder.Column.Float.append_null builder;

  Alcotest.(check int) "Float builder length" 6 (Builder.Column.Float.length builder);
  Alcotest.(check int) "Float builder null count" 1 (Builder.Column.Float.null_count builder);

  (* Test with sample data *)
  let sample_builder = Builder.Column.Float.create () in
  let sample_data = sample_floats ~size:12 () in
  Builder.Column.Float.append_many sample_builder sample_data;

  Alcotest.(check int) "Float sample data length" 12 (Builder.Column.Float.length sample_builder)

let test_float64_column_builder () =
  let builder = Builder.Column.Float64.create () in

  Builder.Column.Float64.append builder Float.max_float;
  Builder.Column.Float64.append builder Float.min_float;
  Builder.Column.Float64.append builder 0.0;
  Builder.Column.Float64.append builder 1.23456789;
  Builder.Column.Float64.append_null ~n:2 builder;

  Alcotest.(check int) "Float64 builder length" 6 (Builder.Column.Float64.length builder);
  Alcotest.(check int) "Float64 builder null count" 2 (Builder.Column.Float64.null_count builder)

let test_boolean_column_builder () =
  let builder = Builder.Column.Boolean.create () in

  Builder.Column.Boolean.append builder true;
  Builder.Column.Boolean.append builder false;
  Builder.Column.Boolean.append builder true;
  Builder.Column.Boolean.append_null builder;

  Alcotest.(check int) "Boolean builder length" 4 (Builder.Column.Boolean.length builder);
  Alcotest.(check int) "Boolean builder null count" 1 (Builder.Column.Boolean.null_count builder);

  (* Test with sample data *)
  let sample_builder = Builder.Column.Boolean.create () in
  let sample_data = sample_bools ~size:20 () in
  Builder.Column.Boolean.append_many sample_builder sample_data;

  Alcotest.(check int) "Boolean sample data length" 20 (Builder.Column.Boolean.length sample_builder)

let test_string_column_builder () =
  let builder = Builder.Column.String.create () in

  Builder.Column.String.append builder "hello";
  Builder.Column.String.append builder "world";
  Builder.Column.String.append builder "";
  Builder.Column.String.append builder "🚀 unicode";
  Builder.Column.String.append_null builder;

  Alcotest.(check int) "String builder length" 5 (Builder.Column.String.length builder);
  Alcotest.(check int) "String builder null count" 1 (Builder.Column.String.null_count builder);

  (* Test with sample data *)
  let sample_builder = Builder.Column.String.create () in
  let sample_data = sample_strings ~size:8 () in
  Builder.Column.String.append_many sample_builder sample_data;

  Alcotest.(check int) "String sample data length" 8 (Builder.Column.String.length sample_builder);

  (* Test with very long string *)
  let long_builder = Builder.Column.String.create () in
  let long_string = String.make 10000 'x' in
  Builder.Column.String.append long_builder long_string;
  Builder.Column.String.append long_builder "short";

  Alcotest.(check int) "String long data length" 2 (Builder.Column.String.length long_builder)

(** {2 Temporal Column Builder Tests} *)

let test_date32_column_builder () =
  let builder = Builder.Column.Date32.create () in

  Builder.Column.Date32.append builder 0l; (* epoch *)
  Builder.Column.Date32.append builder 18000l; (* days since epoch *)
  Builder.Column.Date32.append builder (-365l); (* before epoch *)
  Builder.Column.Date32.append_null builder;

  Alcotest.(check int) "Date32 builder length" 4 (Builder.Column.Date32.length builder);
  Alcotest.(check int) "Date32 builder null count" 1 (Builder.Column.Date32.null_count builder);

  (* Test with sequential dates *)
  let seq_builder = Builder.Column.Date32.create () in
  let seq_dates = Array.init 30 (fun i -> Int32.of_int (18000 + i)) in
  Builder.Column.Date32.append_many seq_builder seq_dates;

  Alcotest.(check int) "Date32 sequential length" 30 (Builder.Column.Date32.length seq_builder)

let test_date64_column_builder () =
  let builder = Builder.Column.Date64.create () in

  Builder.Column.Date64.append builder 0L; (* epoch *)
  Builder.Column.Date64.append builder 1609459200000L; (* 2021-01-01 *)
  Builder.Column.Date64.append builder (-86400000L); (* before epoch *)
  Builder.Column.Date64.append_null builder;

  Alcotest.(check int) "Date64 builder length" 4 (Builder.Column.Date64.length builder);
  Alcotest.(check int) "Date64 builder null count" 1 (Builder.Column.Date64.null_count builder)

let test_time32_column_builder () =
  let builder = Builder.Column.Time32.create () in

  Builder.Column.Time32.append builder 0l; (* midnight *)
  Builder.Column.Time32.append builder 3600l; (* 1 hour *)
  Builder.Column.Time32.append builder 86399l; (* end of day - 1 second *)
  Builder.Column.Time32.append_null builder;

  Alcotest.(check int) "Time32 builder length" 4 (Builder.Column.Time32.length builder);
  Alcotest.(check int) "Time32 builder null count" 1 (Builder.Column.Time32.null_count builder)

let test_time64_column_builder () =
  let builder = Builder.Column.Time64.create () in

  Builder.Column.Time64.append builder 0L; (* midnight *)
  Builder.Column.Time64.append builder 3600000000L; (* 1 hour in microseconds *)
  Builder.Column.Time64.append builder 86399999999L; (* end of day - 1 microsecond *)
  Builder.Column.Time64.append_null builder;

  Alcotest.(check int) "Time64 builder length" 4 (Builder.Column.Time64.length builder);
  Alcotest.(check int) "Time64 builder null count" 1 (Builder.Column.Time64.null_count builder)

let test_timestamp_column_builder () =
  let builder = Builder.Column.Timestamp.create () in

  Builder.Column.Timestamp.append builder 0L; (* epoch *)
  Builder.Column.Timestamp.append builder 1609459200000000000L; (* 2021-01-01 in nanoseconds *)
  Builder.Column.Timestamp.append builder 1234567890000000000L; (* specific timestamp *)
  Builder.Column.Timestamp.append_null builder;

  Alcotest.(check int) "Timestamp builder length" 4 (Builder.Column.Timestamp.length builder);
  Alcotest.(check int) "Timestamp builder null count" 1 (Builder.Column.Timestamp.null_count builder)

let test_duration_column_builder () =
  let builder = Builder.Column.Duration.create () in

  Builder.Column.Duration.append builder 0L; (* zero duration *)
  Builder.Column.Duration.append builder 1000000000L; (* 1 second in nanoseconds *)
  Builder.Column.Duration.append builder 3600000000000L; (* 1 hour in nanoseconds *)
  Builder.Column.Duration.append builder (-1000000000L); (* negative duration *)
  Builder.Column.Duration.append_null builder;

  Alcotest.(check int) "Duration builder length" 5 (Builder.Column.Duration.length builder);
  Alcotest.(check int) "Duration builder null count" 1 (Builder.Column.Duration.null_count builder)

(** {2 Edge Case Tests} *)

let test_empty_column_builders () =
  let test_empty_builder name create length_fn null_count_fn =
    let builder = create () in
    Alcotest.(check int) (name ^ " empty length") 0 (length_fn builder);
    Alcotest.(check int) (name ^ " empty null count") 0 (null_count_fn builder)
  in

  test_empty_builder "Int8" Builder.Column.Int8.create Builder.Column.Int8.length Builder.Column.Int8.null_count;
  test_empty_builder "Int16" Builder.Column.Int16.create Builder.Column.Int16.length Builder.Column.Int16.null_count;
  test_empty_builder "Int32" Builder.Column.Int32.create Builder.Column.Int32.length Builder.Column.Int32.null_count;
  test_empty_builder "Int64" Builder.Column.Int64.create Builder.Column.Int64.length Builder.Column.Int64.null_count;
  test_empty_builder "UInt8" Builder.Column.UInt8.create Builder.Column.UInt8.length Builder.Column.UInt8.null_count;
  test_empty_builder "UInt16" Builder.Column.UInt16.create Builder.Column.UInt16.length Builder.Column.UInt16.null_count;
  test_empty_builder "UInt32" Builder.Column.UInt32.create Builder.Column.UInt32.length Builder.Column.UInt32.null_count;
  test_empty_builder "UInt64" Builder.Column.UInt64.create Builder.Column.UInt64.length Builder.Column.UInt64.null_count;
  test_empty_builder "Float" Builder.Column.Float.create Builder.Column.Float.length Builder.Column.Float.null_count;
  test_empty_builder "Float64" Builder.Column.Float64.create Builder.Column.Float64.length Builder.Column.Float64.null_count;
  test_empty_builder "Boolean" Builder.Column.Boolean.create Builder.Column.Boolean.length Builder.Column.Boolean.null_count;
  test_empty_builder "String" Builder.Column.String.create Builder.Column.String.length Builder.Column.String.null_count;
  test_empty_builder "Date32" Builder.Column.Date32.create Builder.Column.Date32.length Builder.Column.Date32.null_count;
  test_empty_builder "Date64" Builder.Column.Date64.create Builder.Column.Date64.length Builder.Column.Date64.null_count;
  test_empty_builder "Time32" Builder.Column.Time32.create Builder.Column.Time32.length Builder.Column.Time32.null_count;
  test_empty_builder "Time64" Builder.Column.Time64.create Builder.Column.Time64.length Builder.Column.Time64.null_count;
  test_empty_builder "Timestamp" Builder.Column.Timestamp.create Builder.Column.Timestamp.length Builder.Column.Timestamp.null_count;
  test_empty_builder "Duration" Builder.Column.Duration.create Builder.Column.Duration.length Builder.Column.Duration.null_count

let test_all_nulls_column_builders () =
  (* Test builders with only null values *)
  let int_builder = Builder.Column.Int64.create () in
  let string_builder = Builder.Column.String.create () in
  let float_builder = Builder.Column.Float64.create () in
  let bool_builder = Builder.Column.Boolean.create () in

  Builder.Column.Int64.append_null ~n:10 int_builder;
  Builder.Column.String.append_null ~n:5 string_builder;
  Builder.Column.Float64.append_null ~n:7 float_builder;
  Builder.Column.Boolean.append_null ~n:3 bool_builder;

  Alcotest.(check int) "All nulls int length" 10 (Builder.Column.Int64.length int_builder);
  Alcotest.(check int) "All nulls int null count" 10 (Builder.Column.Int64.null_count int_builder);

  Alcotest.(check int) "All nulls string length" 5 (Builder.Column.String.length string_builder);
  Alcotest.(check int) "All nulls string null count" 5 (Builder.Column.String.null_count string_builder);

  Alcotest.(check int) "All nulls float length" 7 (Builder.Column.Float64.length float_builder);
  Alcotest.(check int) "All nulls float null count" 7 (Builder.Column.Float64.null_count float_builder);

  Alcotest.(check int) "All nulls bool length" 3 (Builder.Column.Boolean.length bool_builder);
  Alcotest.(check int) "All nulls bool null count" 3 (Builder.Column.Boolean.null_count bool_builder)

let test_large_column_builders () =
  (* Test with larger datasets *)
  let size = 1000 in
  let int_builder = Builder.Column.Int32.create () in
  let string_builder = Builder.Column.String.create () in

  (* Generate large datasets *)
  let large_ints = sequential_ints 1 size in
  let large_strings = Array.init size (fun i -> Printf.sprintf "item_%04d" i) in

  Builder.Column.Int32.append_many int_builder (Array.map Int32.of_int large_ints);
  Builder.Column.String.append_many string_builder large_strings;

  Alcotest.(check int) "Large int builder length" size (Builder.Column.Int32.length int_builder);
  Alcotest.(check int) "Large string builder length" size (Builder.Column.String.length string_builder);

  Alcotest.(check int) "Large int builder null count" 0 (Builder.Column.Int32.null_count int_builder);
  Alcotest.(check int) "Large string builder null count" 0 (Builder.Column.String.null_count string_builder)

let test_mixed_nulls_and_values () =
  (* Test builders with mixed null and non-null values *)
  let float_builder = Builder.Column.Float64.create () in
  let string_builder = Builder.Column.String.create () in

  (* Alternate between values and nulls *)
  for i = 1 to 20 do
    if i mod 3 = 0 then (
      Builder.Column.Float64.append_null float_builder;
      Builder.Column.String.append_null string_builder;
    ) else (
      Builder.Column.Float64.append float_builder (float_of_int i);
      Builder.Column.String.append string_builder (string_of_int i);
    )
  done;

  Alcotest.(check int) "Mixed float length" 20 (Builder.Column.Float64.length float_builder);
  Alcotest.(check int) "Mixed string length" 20 (Builder.Column.String.length string_builder);

  let expected_nulls = 20 / 3 in (* Every 3rd element is null *)
  Alcotest.(check int) "Mixed float null count" expected_nulls (Builder.Column.Float64.null_count float_builder);
  Alcotest.(check int) "Mixed string null count" expected_nulls (Builder.Column.String.null_count string_builder)

let test_batch_null_operations () =
  (* Test batch null operations with different counts *)
  let builder = Builder.Column.Int32.create () in

  Builder.Column.Int32.append builder 1l;
  Builder.Column.Int32.append_null ~n:5 builder;
  Builder.Column.Int32.append builder 2l;
  Builder.Column.Int32.append_null ~n:3 builder;
  Builder.Column.Int32.append builder 3l;
  Builder.Column.Int32.append_null builder; (* single null *)

  let expected_length = 1 + 5 + 1 + 3 + 1 + 1 in
  let expected_nulls = 5 + 3 + 1 in

  Alcotest.(check int) "Batch nulls length" expected_length (Builder.Column.Int32.length builder);
  Alcotest.(check int) "Batch nulls count" expected_nulls (Builder.Column.Int32.null_count builder)

(** {2 Test Suite} *)

let () =
  let open Alcotest in
  run "Builder.Column tests" [
    "integer_types", [
      test_case "Int8 column builder" `Quick test_int8_column_builder;
      test_case "Int16 column builder" `Quick test_int16_column_builder;
      test_case "Int32 column builder" `Quick test_int32_column_builder;
      test_case "Int64 column builder" `Quick test_int64_column_builder;
      test_case "UInt8 column builder" `Quick test_uint8_column_builder;
      test_case "UInt16 column builder" `Quick test_uint16_column_builder;
      test_case "UInt32 column builder" `Quick test_uint32_column_builder;
      test_case "UInt64 column builder" `Quick test_uint64_column_builder;
    ];
    "floating_point", [
      test_case "Float column builder" `Quick test_float_column_builder;
      test_case "Float64 column builder" `Quick test_float64_column_builder;
    ];
    "other_types", [
      test_case "Boolean column builder" `Quick test_boolean_column_builder;
      test_case "String column builder" `Quick test_string_column_builder;
    ];
    "temporal_types", [
      test_case "Date32 column builder" `Quick test_date32_column_builder;
      test_case "Date64 column builder" `Quick test_date64_column_builder;
      test_case "Time32 column builder" `Quick test_time32_column_builder;
      test_case "Time64 column builder" `Quick test_time64_column_builder;
      test_case "Timestamp column builder" `Quick test_timestamp_column_builder;
      test_case "Duration column builder" `Quick test_duration_column_builder;
    ];
    "edge_cases", [
      test_case "Empty column builders" `Quick test_empty_column_builders;
      test_case "All nulls column builders" `Quick test_all_nulls_column_builders;
      test_case "Large column builders" `Quick test_large_column_builders;
      test_case "Mixed nulls and values" `Quick test_mixed_nulls_and_values;
      test_case "Batch null operations" `Quick test_batch_null_operations;
    ];
  ]