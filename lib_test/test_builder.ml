(* Builder module tests using external Arrow interface *)
open Arrow

let test_basic_builders () =
  (* Test basic builder functionality *)
  let int_builder = Builder.NativeInt.create () in
  let string_builder = Builder.String.create () in
  let float_builder = Builder.Double.create () in

  (* Append some values *)
  Builder.NativeInt.append int_builder 42;
  Builder.NativeInt.append int_builder 100;
  Builder.NativeInt.append_null int_builder;

  Builder.String.append string_builder "hello";
  Builder.String.append string_builder "world";
  Builder.String.append_null string_builder;

  Builder.Double.append float_builder 1.5;
  Builder.Double.append float_builder 2.7;
  Builder.Double.append_null float_builder;

  (* Check lengths and null counts *)
  Alcotest.(check int) "Int builder length" 3 (Builder.NativeInt.length int_builder);
  Alcotest.(check int) "Int builder null count" 1 (Builder.NativeInt.null_count int_builder);

  Alcotest.(check int) "String builder length" 3 (Builder.String.length string_builder);
  Alcotest.(check int) "String builder null count" 1 (Builder.String.null_count string_builder);

  Alcotest.(check int) "Float builder length" 3 (Builder.Double.length float_builder);
  Alcotest.(check int) "Float builder null count" 1 (Builder.Double.null_count float_builder)

let test_optional_builders () =
  (* Test builders with optional values *)
  let int_builder = Builder.Int64.create () in
  let string_builder = Builder.String.create () in

  Builder.Int64.append_opt int_builder (Some 123L);
  Builder.Int64.append_opt int_builder None;
  Builder.Int64.append_opt int_builder (Some 456L);

  Builder.String.append_opt string_builder (Some "test");
  Builder.String.append_opt string_builder None;
  Builder.String.append_opt string_builder (Some "value");

  Alcotest.(check int) "Int64 optional length" 3 (Builder.Int64.length int_builder);
  Alcotest.(check int) "Int64 optional null count" 1 (Builder.Int64.null_count int_builder);

  Alcotest.(check int) "String optional length" 3 (Builder.String.length string_builder);
  Alcotest.(check int) "String optional null count" 1 (Builder.String.null_count string_builder)

let test_numeric_builders () =
  (* Test various numeric builder types *)
  let int32_builder = Builder.Int32.create () in
  let float_builder = Builder.Float.create () in
  let bool_builder = Builder.Boolean.create () in

  Builder.Int32.append int32_builder 100l;
  Builder.Int32.append int32_builder (-50l);
  Builder.Int32.append_null int32_builder;

  Builder.Float.append float_builder 3.14;
  Builder.Float.append float_builder (-2.71);
  Builder.Float.append_null float_builder;

  Builder.Boolean.append bool_builder true;
  Builder.Boolean.append bool_builder false;
  Builder.Boolean.append_null bool_builder;

  Alcotest.(check int) "Int32 builder length" 3 (Builder.Int32.length int32_builder);
  Alcotest.(check int) "Float builder length" 3 (Builder.Float.length float_builder);
  Alcotest.(check int) "Boolean builder length" 3 (Builder.Boolean.length bool_builder);

  Alcotest.(check int) "Int32 null count" 1 (Builder.Int32.null_count int32_builder);
  Alcotest.(check int) "Float null count" 1 (Builder.Float.null_count float_builder);
  Alcotest.(check int) "Boolean null count" 1 (Builder.Boolean.null_count bool_builder)

let test_small_integer_builders () =
  (* Test smaller integer types *)
  let int8_builder = Builder.Int8.create () in
  let int16_builder = Builder.Int16.create () in
  let uint8_builder = Builder.UInt8.create () in
  let uint16_builder = Builder.UInt16.create () in

  Builder.Int8.append int8_builder 127;
  Builder.Int8.append int8_builder (-128);
  Builder.Int8.append_null int8_builder;

  Builder.Int16.append int16_builder 32767;
  Builder.Int16.append int16_builder (-32768);
  Builder.Int16.append_null int16_builder;

  Builder.UInt8.append uint8_builder 255;
  Builder.UInt8.append uint8_builder 0;
  Builder.UInt8.append_null uint8_builder;

  Builder.UInt16.append uint16_builder 65535;
  Builder.UInt16.append uint16_builder 0;
  Builder.UInt16.append_null uint16_builder;

  (* Check all builders work correctly *)
  Alcotest.(check int) "Int8 length" 3 (Builder.Int8.length int8_builder);
  Alcotest.(check int) "Int16 length" 3 (Builder.Int16.length int16_builder);
  Alcotest.(check int) "UInt8 length" 3 (Builder.UInt8.length uint8_builder);
  Alcotest.(check int) "UInt16 length" 3 (Builder.UInt16.length uint16_builder);

  Alcotest.(check int) "Int8 null count" 1 (Builder.Int8.null_count int8_builder);
  Alcotest.(check int) "Int16 null count" 1 (Builder.Int16.null_count int16_builder);
  Alcotest.(check int) "UInt8 null count" 1 (Builder.UInt8.null_count uint8_builder);
  Alcotest.(check int) "UInt16 null count" 1 (Builder.UInt16.null_count uint16_builder)

let test_large_integer_builders () =
  (* Test larger integer types *)
  let uint32_builder = Builder.UInt32.create () in
  let uint64_builder = Builder.UInt64.create () in

  Builder.UInt32.append uint32_builder (-1l); (* max uint32 as signed int32 *)
  Builder.UInt32.append uint32_builder 0l;
  Builder.UInt32.append_null uint32_builder;

  Builder.UInt64.append uint64_builder Int64.max_int;
  Builder.UInt64.append uint64_builder 0L;
  Builder.UInt64.append_null uint64_builder;

  Alcotest.(check int) "UInt32 length" 3 (Builder.UInt32.length uint32_builder);
  Alcotest.(check int) "UInt64 length" 3 (Builder.UInt64.length uint64_builder);

  Alcotest.(check int) "UInt32 null count" 1 (Builder.UInt32.null_count uint32_builder);
  Alcotest.(check int) "UInt64 null count" 1 (Builder.UInt64.null_count uint64_builder)

let test_datetime_builders () =
  (* Test datetime-related builders *)
  let date32_builder = Builder.Date32.create () in
  let date64_builder = Builder.Date64.create () in
  let time32_builder = Builder.Time32.create () in
  let time64_builder = Builder.Time64.create () in
  let timestamp_builder = Builder.Timestamp.create () in
  let duration_builder = Builder.Duration.create () in

  (* Add some temporal values *)
  Builder.Date32.append date32_builder 18000l; (* days since epoch *)
  Builder.Date32.append_null date32_builder;

  Builder.Date64.append date64_builder 1609459200000L; (* milliseconds since epoch *)
  Builder.Date64.append_null date64_builder;

  Builder.Time32.append time32_builder 3600l; (* seconds since midnight *)
  Builder.Time32.append_null time32_builder;

  Builder.Time64.append time64_builder 3600000000L; (* microseconds since midnight *)
  Builder.Time64.append_null time64_builder;

  Builder.Timestamp.append timestamp_builder 1609459200000000000L; (* nanoseconds since epoch *)
  Builder.Timestamp.append_null timestamp_builder;

  Builder.Duration.append duration_builder 1000000000L; (* nanoseconds duration *)
  Builder.Duration.append_null duration_builder;

  (* Verify all datetime builders work *)
  Alcotest.(check int) "Date32 length" 2 (Builder.Date32.length date32_builder);
  Alcotest.(check int) "Date64 length" 2 (Builder.Date64.length date64_builder);
  Alcotest.(check int) "Time32 length" 2 (Builder.Time32.length time32_builder);
  Alcotest.(check int) "Time64 length" 2 (Builder.Time64.length time64_builder);
  Alcotest.(check int) "Timestamp length" 2 (Builder.Timestamp.length timestamp_builder);
  Alcotest.(check int) "Duration length" 2 (Builder.Duration.length duration_builder);

  Alcotest.(check int) "Date32 null count" 1 (Builder.Date32.null_count date32_builder);
  Alcotest.(check int) "Date64 null count" 1 (Builder.Date64.null_count date64_builder);
  Alcotest.(check int) "Time32 null count" 1 (Builder.Time32.null_count time32_builder);
  Alcotest.(check int) "Time64 null count" 1 (Builder.Time64.null_count time64_builder);
  Alcotest.(check int) "Timestamp null count" 1 (Builder.Timestamp.null_count timestamp_builder);
  Alcotest.(check int) "Duration null count" 1 (Builder.Duration.null_count duration_builder)

let test_builder_batch_nulls () =
  (* Test adding multiple nulls at once *)
  let int_builder = Builder.NativeInt.create () in
  let string_builder = Builder.String.create () in

  Builder.NativeInt.append int_builder 1;
  Builder.NativeInt.append_null ~n:3 int_builder;
  Builder.NativeInt.append int_builder 2;

  Builder.String.append string_builder "start";
  Builder.String.append_null ~n:2 string_builder;
  Builder.String.append string_builder "end";

  Alcotest.(check int) "Int builder with batch nulls length" 5 (Builder.NativeInt.length int_builder);
  Alcotest.(check int) "Int builder with batch nulls null count" 3 (Builder.NativeInt.null_count int_builder);

  Alcotest.(check int) "String builder with batch nulls length" 4 (Builder.String.length string_builder);
  Alcotest.(check int) "String builder with batch nulls null count" 2 (Builder.String.null_count string_builder)

let test_empty_builders () =
  (* Test that empty builders work correctly *)
  let empty_int = Builder.NativeInt.create () in
  let empty_string = Builder.String.create () in
  let empty_float = Builder.Double.create () in

  Alcotest.(check int) "Empty int builder length" 0 (Builder.NativeInt.length empty_int);
  Alcotest.(check int) "Empty string builder length" 0 (Builder.String.length empty_string);
  Alcotest.(check int) "Empty float builder length" 0 (Builder.Double.length empty_float);

  Alcotest.(check int) "Empty int builder null count" 0 (Builder.NativeInt.null_count empty_int);
  Alcotest.(check int) "Empty string builder null count" 0 (Builder.String.null_count empty_string);
  Alcotest.(check int) "Empty float builder null count" 0 (Builder.Double.null_count empty_float)

type person = { name : string; age : int; score : float option }

let test_row_based_building () =
  (* Test the row-based builder functionality *)
  let people = [|
    { name = "Alice"; age = 30; score = Some 95.5 };
    { name = "Bob"; age = 25; score = None };
    { name = "Charlie"; age = 35; score = Some 88.0 };
  |] in

  let cols =
    Builder.col ~name:"name" Table.Utf8 (fun p -> p.name) @
    Builder.col ~name:"age" Table.Int (fun p -> p.age) @
    Builder.col_opt ~name:"score" Table.Float (fun p -> p.score) in

  let table = Builder.array_to_table cols people in

  Alcotest.(check int) "Row-based table rows" 3 (Table.num_rows table);

  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in
  let scores = Table.read_opt table Table.Float ~column:(`Name "score") in

  Alcotest.(check (array string)) "Row-based names" [| "Alice"; "Bob"; "Charlie" |] names;
  Alcotest.(check (array int)) "Row-based ages" [| 30; 25; 35 |] ages;
  Alcotest.(check (array (option (float 1e-6)))) "Row-based scores" [| Some 95.5; None; Some 88.0 |] scores

let () =
  let open Alcotest in
  run "Builder tests" [
    "basic", [
      test_case "Basic builders" `Quick test_basic_builders;
      test_case "Optional builders" `Quick test_optional_builders;
      test_case "Empty builders" `Quick test_empty_builders;
    ];
    "numeric", [
      test_case "Numeric builders" `Quick test_numeric_builders;
      test_case "Small integer builders" `Quick test_small_integer_builders;
      test_case "Large integer builders" `Quick test_large_integer_builders;
    ];
    "temporal", [
      test_case "Datetime builders" `Quick test_datetime_builders;
    ];
    "advanced", [
      test_case "Batch nulls" `Quick test_builder_batch_nulls;
      test_case "Row-based building" `Quick test_row_based_building;
    ];
  ]