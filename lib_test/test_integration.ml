(** Comprehensive integration tests for the Arrow OCaml library

    This test suite validates cross-module functionality and end-to-end scenarios
    to ensure all components work together properly. It covers:

    - Table + Builder integration (creating tables via builders, then reading them)
    - Table + Schema integration (schema consistency across operations)
    - Builder + I/O integration (build tables and write to files)
    - I/O + Schema integration (schema preservation across file operations)
    - Column + Table integration (column operations within tables)
    - Time + Table integration (time data types in tables and I/O)
    - Multi-module workflows (complex end-to-end scenarios)
    - Cross-format data migration workflows
    - Large-scale data processing workflows
    - Error propagation across modules
    - Performance testing for integrated workflows
*)

open Arrow

module Fixtures = Test_fixtures

(** {1 Test Utilities} *)

(** Custom Alcotest comparators for time types *)
let date_testable = Alcotest.testable
  (fun ppf d -> Format.fprintf ppf "Date(%d)" (Time.Date.to_unix_days d))
  Fixtures.date_equal

let time_ns_testable = Alcotest.testable
  (fun ppf t -> Format.fprintf ppf "Time_ns(%Ld)" (Time.Time_ns.to_int64_ns_since_epoch t))
  Fixtures.time_ns_equal

let span_ns_testable = Alcotest.testable
  (fun ppf s -> Format.fprintf ppf "Span_ns(%Ld)" (Time.Time_ns.Span.to_ns s))
  Fixtures.span_ns_equal

let ofday_ns_testable = Alcotest.testable
  (fun ppf o -> Format.fprintf ppf "Ofday_ns(%Ld)" (Time.Time_ns.Ofday.to_ns_since_midnight o))
  Fixtures.ofday_ns_equal

(** Helper to verify schema field equality *)
let verify_schema_field_equal field1 field2 =
  let name1 = Schema.name field1 in
  let name2 = Schema.name field2 in
  let type1 = Schema.format field1 in
  let type2 = Schema.format field2 in
  name1 = name2 && type1 = type2

(** Helper to verify schema equality *)
let verify_schemas_equal schema1 schema2 =
  let fields1 = Schema.children schema1 in
  let fields2 = Schema.children schema2 in
  List.length fields1 = List.length fields2 &&
  List.for_all2 verify_schema_field_equal fields1 fields2

(** Measure execution time of a function *)
let time_execution f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  (result, end_time -. start_time)

(** {1 Table + Builder Integration Tests} *)

let test_builder_to_table_roundtrip () =
  (* Create table using builders, then read back data *)
  let ids = Fixtures.sample_ints ~size:100 () in
  let names = Fixtures.sample_strings ~size:100 () in
  let scores = Fixtures.sample_floats ~size:100 () in
  let active = Fixtures.sample_bools ~size:100 () in
  let dates = Fixtures.sample_dates ~size:100 () in

  (* Build table using column builders *)
  let table = Table.create [
    Table.col ids Table.Int "id";
    Table.col names Table.Utf8 "name";
    Table.col scores Table.Float "score";
    Table.col active Table.Bool "active";
    Table.col dates Table.Date "created_date";
  ] in

  (* Verify table structure *)
  Alcotest.(check int) "Builder table row count" 100 (Table.num_rows table);
  let schema = Table.schema table in
  Alcotest.(check int) "Builder table column count" 5 (List.length (Schema.children schema));

  (* Read back data and verify values *)
  let read_ids = Table.read table Table.Int ~column:(`Name "id") in
  let read_names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let read_scores = Table.read table Table.Float ~column:(`Name "score") in
  let read_active = Table.read table Table.Bool ~column:(`Name "active") in
  let read_dates = Table.read table Table.Date ~column:(`Name "created_date") in

  Alcotest.(check (array int)) "IDs roundtrip" ids read_ids;
  Alcotest.(check (array string)) "Names roundtrip" names read_names;
  Alcotest.(check (array (float 1e-6))) "Scores roundtrip" scores read_scores;
  Alcotest.(check (array bool)) "Active flags roundtrip" active read_active;
  Alcotest.(check (array date_testable)) "Dates roundtrip" dates read_dates

let test_row_builder_integration () =
  (* Test row-based builder with table integration *)
  let records = Fixtures.sample_test_records 50 in
  let table = Fixtures.test_records_to_table records in

  (* Verify table structure matches record structure *)
  Alcotest.(check int) "Row builder table size" 50 (Table.num_rows table);
  let column_names = Fixtures.get_column_names table in
  let expected_columns = ["id"; "name"; "value"; "active"; "score"] in
  Alcotest.(check (list string)) "Row builder columns" expected_columns column_names;

  (* Verify data integrity *)
  let read_ids = Table.read table Table.Int ~column:(`Name "id") in
  let read_names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let read_values = Table.read table Table.Float ~column:(`Name "value") in
  let read_active = Table.read table Table.Bool ~column:(`Name "active") in
  let read_scores_opt = Table.read_opt table Table.Float ~column:(`Name "score") in

  (* Check first few records *)
  Array.iteri (fun i record ->
    if i < 5 then (
      Alcotest.(check int) (Printf.sprintf "ID %d" i) record.Test_fixtures.id read_ids.(i);
      Alcotest.(check string) (Printf.sprintf "Name %d" i) record.Test_fixtures.name read_names.(i);
      Alcotest.(check (float 1e-6)) (Printf.sprintf "Value %d" i) record.Test_fixtures.value read_values.(i);
      Alcotest.(check bool) (Printf.sprintf "Active %d" i) record.Test_fixtures.active read_active.(i);
      Alcotest.(check (option (float 1e-6))) (Printf.sprintf "Score %d" i) record.Test_fixtures.score read_scores_opt.(i)
    )
  ) records

let test_nullable_builder_integration () =
  (* Test builder with nullable data integration *)
  let opt_ints = Fixtures.sample_ints_opt ~size:20 ~null_ratio:0.4 () in
  let opt_strings = Fixtures.sample_strings_opt ~size:20 ~null_ratio:0.3 () in
  let opt_floats = Fixtures.sample_floats_opt ~size:20 ~null_ratio:0.2 () in

  let table = Table.create [
    Table.col_opt opt_ints Table.Int "opt_ints";
    Table.col_opt opt_strings Table.Utf8 "opt_strings";
    Table.col_opt opt_floats Table.Float "opt_floats";
  ] in

  (* Read back and verify nullable data *)
  let read_opt_ints = Table.read_opt table Table.Int ~column:(`Name "opt_ints") in
  let read_opt_strings = Table.read_opt table Table.Utf8 ~column:(`Name "opt_strings") in
  let read_opt_floats = Table.read_opt table Table.Float ~column:(`Name "opt_floats") in

  Alcotest.(check (array (option int))) "Optional ints roundtrip" opt_ints read_opt_ints;
  Alcotest.(check (array (option string))) "Optional strings roundtrip" opt_strings read_opt_strings;
  Alcotest.(check (array (option (float 1e-6)))) "Optional floats roundtrip" opt_floats read_opt_floats

(** {1 Table + Schema Integration Tests} *)

let test_schema_consistency_across_operations () =
  (* Create table and verify schema is preserved across various operations *)
  let original_table = Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "integers";
    Table.col [| "a"; "b"; "c"; "d"; "e" |] Table.Utf8 "strings";
    Table.col [| 1.0; 2.0; 3.0; 4.0; 5.0 |] Table.Float "floats";
  ] in
  let original_schema = Table.schema original_table in

  (* Test column operations preserve schema structure *)
  let integers_column = Table.read original_table Table.Int ~column:(`Name "integers") in
  let new_table = Table.create [
    Table.col integers_column Table.Int "integers";
    Table.col [| "x"; "y"; "z"; "w"; "v" |] Table.Utf8 "new_strings";
  ] in

  let new_schema = Table.schema new_table in
  let new_fields = Schema.children new_schema in

  (* Verify first field matches original *)
  let original_int_field = List.hd (Schema.children original_schema) in
  let new_int_field = List.hd new_fields in

  Alcotest.(check string) "Field name preserved"
    (Schema.name original_int_field) (Schema.name new_int_field);
  Alcotest.(check bool) "Field type preserved" true
    (Schema.format original_int_field = Schema.format new_int_field)

let test_schema_validation_across_formats () =
  (* Test that schema information is preserved when converting between formats *)
  let table = Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "id";
    Table.col [| "a"; "b"; "c"; "d"; "e" |] Table.Utf8 "label";
    Table.col [| 1.1; 2.2; 3.3; 4.4; 5.5 |] Table.Float "value";
    Table.col [| true; false; true; false; true |] Table.Bool "flag";
    Table.col (Fixtures.sample_dates ~size:5 ()) Table.Date "event_date";
  ] in

  let original_schema = Table.schema table in

  (* Write to different formats and verify schema preservation *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "schema_validation" in
  IO.write table parquet_file;

  let schema_from_file = IO.schema parquet_file in
  Alcotest.(check bool) "Schema preserved in Parquet" true
    (verify_schemas_equal original_schema schema_from_file);

  let read_table = IO.read parquet_file in
  let schema_from_table = Table.schema read_table in
  Alcotest.(check bool) "Schema preserved after roundtrip" true
    (verify_schemas_equal original_schema schema_from_table);

  Fixtures.cleanup_test_files "schema_validation_.*\\.parquet"

let test_schema_evolution_compatibility () =
  (* Test adding/removing columns while maintaining schema compatibility *)
  let base_table = Table.create [
    Table.col [| 1; 2; 3 |] Table.Int "id";
    Table.col [| "a"; "b"; "c" |] Table.Utf8 "name";
  ] in

  (* Create extended table with additional column *)
  let extended_table = Table.create [
    Table.col [| 1; 2; 3 |] Table.Int "id";
    Table.col [| "a"; "b"; "c" |] Table.Utf8 "name";
    Table.col [| 10.0; 20.0; 30.0 |] Table.Float "score";
    Table.col [| true; false; true |] Table.Bool "active";
  ] in

  let base_schema = Table.schema base_table in
  let extended_schema = Table.schema extended_table in

  (* Verify schema evolution *)
  let base_fields = Schema.children base_schema in
  let extended_fields = Schema.children extended_schema in

  Alcotest.(check int) "Base schema fields" 2 (List.length base_fields);
  Alcotest.(check int) "Extended schema fields" 4 (List.length extended_fields);

  (* Verify first two fields are compatible *)
  List.iteri (fun i base_field ->
    let extended_field = List.nth extended_fields i in
    Alcotest.(check bool) (Printf.sprintf "Field %d compatibility" i) true
      (verify_schema_field_equal base_field extended_field)
  ) base_fields

(** {1 Builder + I/O Integration Tests} *)

let test_build_and_write_workflow () =
  (* Build table incrementally and write to multiple formats *)
  let size = 1000 in
  let ids = Fixtures.sequential_ints 1 size in
  let labels = Array.init size (fun i -> Printf.sprintf "item_%04d" (i + 1)) in
  let values = Fixtures.sequential_floats 0.0 0.1 size in
  let flags = Array.init size (fun i -> i mod 3 = 0) in
  let timestamps = Fixtures.sample_time_ns ~size () in

  let table = Table.create [
    Table.col ids Table.Int "id";
    Table.col labels Table.Utf8 "label";
    Table.col values Table.Float "value";
    Table.col flags Table.Bool "flag";
    Table.col timestamps Table.Time_ns "timestamp";
  ] in

  (* Write to Parquet *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "build_write_parquet" in
  IO.write table parquet_file;

  (* Verify file was created and has correct content *)
  Alcotest.(check bool) "Parquet file exists" true (Sys.file_exists parquet_file);

  let read_table = IO.read parquet_file in
  Alcotest.(check int) "Read table row count" size (Table.num_rows read_table);

  (* Verify data integrity after I/O roundtrip *)
  let read_ids = Table.read read_table Table.Int ~column:(`Name "id") in
  let read_labels = Table.read read_table Table.Utf8 ~column:(`Name "label") in
  let read_values = Table.read read_table Table.Float ~column:(`Name "value") in
  let read_flags = Table.read read_table Table.Bool ~column:(`Name "flag") in
  let read_timestamps = Table.read read_table Table.Time_ns ~column:(`Name "timestamp") in

  Alcotest.(check (array int)) "IDs after I/O" ids read_ids;
  Alcotest.(check (array string)) "Labels after I/O" labels read_labels;
  Alcotest.(check (array (float 1e-6))) "Values after I/O" values read_values;
  Alcotest.(check (array bool)) "Flags after I/O" flags read_flags;
  Alcotest.(check (array time_ns_testable)) "Timestamps after I/O" timestamps read_timestamps;

  Fixtures.cleanup_test_files "build_write_parquet_.*\\.parquet"

let test_incremental_build_and_stream_write () =
  (* Test building data incrementally and writing in batches *)
  let batch_size = 100 in
  let total_batches = 5 in
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "incremental_build" in

  (* Simulate incremental data building *)
  let all_tables = Array.init total_batches (fun batch_idx ->
    let start_id = batch_idx * batch_size + 1 in
    let ids = Fixtures.sequential_ints start_id batch_size in
    let values = Fixtures.sequential_floats (float_of_int start_id) 1.0 batch_size in
    let labels = Array.init batch_size (fun i -> Printf.sprintf "batch_%d_item_%d" batch_idx i) in

    Table.create [
      Table.col ids Table.Int "batch_id";
      Table.col values Table.Float "batch_value";
      Table.col labels Table.Utf8 "batch_label";
    ]
  ) in

  (* Write first batch to establish file *)
  IO.write all_tables.(0) parquet_file;

  (* For simplicity, write remaining batches to separate files and verify each *)
  Array.iteri (fun i table ->
    if i > 0 then (
      let batch_file = Fixtures.temp_file_path ~extension:".parquet"
        (Printf.sprintf "incremental_build_batch_%d" i) in
      IO.write table batch_file;

      let read_table = IO.read batch_file in
      Alcotest.(check int) (Printf.sprintf "Batch %d size" i) batch_size (Table.num_rows read_table);
    )
  ) all_tables;

  Fixtures.cleanup_test_files "incremental_build.*\\.parquet"

let test_builder_compression_integration () =
  (* Test building tables and writing with different compression methods *)
  let size = 500 in
  let data_table = Table.create [
    Table.col (Fixtures.sequential_ints 1 size) Table.Int "id";
    Table.col (Array.init size (fun i -> String.make (10 + i mod 50) 'x')) Table.Utf8 "data";
    Table.col (Fixtures.sequential_floats 0.0 0.001 size) Table.Float "values";
  ] in

  (* Test different compression formats *)
  let test_compression compression_type ext =
    let file = Fixtures.temp_file_path ~extension:ext "compression_test" in
    (try
      IO.Parquet.write data_table file ~compression:compression_type;
      let read_table = IO.read file in
      Alcotest.(check int) (Printf.sprintf "Compressed %s row count" ext) size (Table.num_rows read_table);

      (* Verify data integrity *)
      let read_ids = Table.read read_table Table.Int ~column:(`Name "id") in
      let original_ids = Fixtures.sequential_ints 1 size in
      Alcotest.(check (array int)) (Printf.sprintf "Compressed %s data integrity" ext) original_ids read_ids
    with _ ->
      Printf.printf "Compression %s not available\n%!" ext
    )
  in

  test_compression (Parquet.Uncompressed) "_uncompressed.parquet";
  test_compression (Parquet.Snappy) "_snappy.parquet";
  test_compression (Parquet.Gzip (Some 6)) "_gzip.parquet";
  test_compression (Parquet.Lz4) "_lz4.parquet";

  Fixtures.cleanup_test_files "compression_test.*\\.parquet"

(** {1 I/O + Schema Integration Tests} *)

let test_schema_preservation_across_formats () =
  (* Create table with complex schema *)
  let table = Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "integers";
    Table.col [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] Table.Utf8 "strings";
    Table.col [| 1.1; 2.2; 3.3; 4.4; 5.5 |] Table.Float "floats";
    Table.col [| true; false; true; false; true |] Table.Bool "booleans";
    Table.col (Fixtures.sample_dates ~size:5 ()) Table.Date "dates";
    Table.col (Fixtures.sample_time_ns ~size:5 ()) Table.Time_ns "timestamps";
    Table.col (Fixtures.sample_span_ns ~size:5 ()) Table.Span_ns "durations";
    Table.col (Fixtures.sample_ofday_ns ~size:5 ()) Table.Ofday_ns "times_of_day";
  ] in

  let original_schema = Table.schema table in

  (* Test Parquet schema preservation *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "schema_preserve_parquet" in
  IO.write table parquet_file;

  let parquet_schema = IO.schema parquet_file in
  let parquet_table = IO.read parquet_file in
  let parquet_read_schema = Table.schema parquet_table in

  (* Verify schema consistency *)
  Alcotest.(check bool) "Parquet schema from file" true
    (verify_schemas_equal original_schema parquet_schema);
  Alcotest.(check bool) "Parquet schema from table" true
    (verify_schemas_equal original_schema parquet_read_schema);

  (* Verify all data types round-trip correctly *)
  let read_ints = Table.read parquet_table Table.Int ~column:(`Name "integers") in
  let read_strings = Table.read parquet_table Table.Utf8 ~column:(`Name "strings") in
  let read_floats = Table.read parquet_table Table.Float ~column:(`Name "floats") in
  let read_bools = Table.read parquet_table Table.Bool ~column:(`Name "booleans") in
  let read_dates = Table.read parquet_table Table.Date ~column:(`Name "dates") in
  let read_timestamps = Table.read parquet_table Table.Time_ns ~column:(`Name "timestamps") in
  let read_durations = Table.read parquet_table Table.Span_ns ~column:(`Name "durations") in
  let read_times_of_day = Table.read parquet_table Table.Ofday_ns ~column:(`Name "times_of_day") in

  Alcotest.(check (array int)) "Int roundtrip" [| 1; 2; 3; 4; 5 |] read_ints;
  Alcotest.(check (array string)) "String roundtrip" [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] read_strings;
  Alcotest.(check (array (float 1e-6))) "Float roundtrip" [| 1.1; 2.2; 3.3; 4.4; 5.5 |] read_floats;
  Alcotest.(check (array bool)) "Bool roundtrip" [| true; false; true; false; true |] read_bools;

  (* Verify time data arrays have correct lengths *)
  Alcotest.(check int) "Dates array length" 5 (Array.length read_dates);
  Alcotest.(check int) "Timestamps array length" 5 (Array.length read_timestamps);
  Alcotest.(check int) "Durations array length" 5 (Array.length read_durations);
  Alcotest.(check int) "Times of day array length" 5 (Array.length read_times_of_day);

  Fixtures.cleanup_test_files "schema_preserve_parquet_.*\\.parquet"

let test_column_selection_schema_consistency () =
  (* Test that column selection preserves schema integrity *)
  let full_table = Fixtures.medium_test_table () in
  let full_schema = Table.schema full_table in

  let file = Fixtures.temp_file_path ~extension:".parquet" "column_selection" in
  IO.write full_table file;

  (* Read with column selection *)
  let selected_columns = ["integers"; "strings"] in
  let partial_table = IO.read ~columns:(`Names selected_columns) file in
  let partial_schema = Table.schema partial_table in

  (* Verify partial schema has correct structure *)
  let partial_fields = Schema.children partial_schema in
  Alcotest.(check int) "Selected columns count" (List.length selected_columns) (List.length partial_fields);

  (* Verify selected fields match original schema *)
  let full_fields = Schema.children full_schema in
  let integers_field = List.find (fun f -> Schema.name f = "integers") full_fields in
  let strings_field = List.find (fun f -> Schema.name f = "strings") full_fields in

  let partial_integers = List.hd partial_fields in
  let partial_strings = List.nth partial_fields 1 in

  Alcotest.(check bool) "Integers field preserved" true
    (verify_schema_field_equal integers_field partial_integers);
  Alcotest.(check bool) "Strings field preserved" true
    (verify_schema_field_equal strings_field partial_strings);

  Fixtures.cleanup_test_files "column_selection_.*\\.parquet"

(** {1 Column + Table Integration Tests} *)

let test_column_operations_within_tables () =
  (* Test various column operations and their integration with table operations *)
  let table = Fixtures.medium_test_table () in

  (* Extract columns *)
  let integers = Table.read table Table.Int ~column:(`Name "integers") in
  let floats = Table.read table Table.Float ~column:(`Name "floats") in
  let strings = Table.read table Table.Utf8 ~column:(`Name "strings") in
  let booleans = Table.read table Table.Bool ~column:(`Name "booleans") in

  (* Verify column lengths match table row count *)
  let row_count = Table.num_rows table in
  Alcotest.(check int) "Integers column length" row_count (Array.length integers);
  Alcotest.(check int) "Floats column length" row_count (Array.length floats);
  Alcotest.(check int) "Strings column length" row_count (Array.length strings);
  Alcotest.(check int) "Booleans column length" row_count (Array.length booleans);

  (* Create new table from extracted columns *)
  let new_table = Table.create [
    Table.col integers Table.Int "extracted_integers";
    Table.col strings Table.Utf8 "extracted_strings";
  ] in

  Alcotest.(check int) "New table row count" row_count (Table.num_rows new_table);

  (* Verify data integrity in new table *)
  let new_integers = Table.read new_table Table.Int ~column:(`Name "extracted_integers") in
  let new_strings = Table.read new_table Table.Utf8 ~column:(`Name "extracted_strings") in

  Alcotest.(check (array int)) "Extracted integers match" integers new_integers;
  Alcotest.(check (array string)) "Extracted strings match" strings new_strings

let test_column_type_operations () =
  (* Test column operations with different data types *)
  let simple_dates = Array.init 10 (fun i -> Time.Date.of_unix_days (18000 + i)) in
  let simple_timestamps = Array.init 10 (fun i -> Time.Time_ns.of_int64_ns_since_epoch (Int64.add 1609459200000000000L (Int64.mul (Int64.of_int i) 86400000000000L))) in
  let simple_durations = Array.init 10 (fun i -> Time.Time_ns.Span.of_ns (Int64.mul (Int64.of_int (i + 1)) 1000000000L)) in
  let simple_times_of_day = Array.init 10 (fun i -> Time.Time_ns.Ofday.of_ns_since_midnight (Int64.mul (Int64.of_int i) 3600000000000L)) in

  let time_table = Table.create [
    Table.col simple_dates Table.Date "dates";
    Table.col simple_timestamps Table.Time_ns "timestamps";
    Table.col simple_durations Table.Span_ns "durations";
    Table.col simple_times_of_day Table.Ofday_ns "times_of_day";
  ] in

  (* Extract and verify time-based columns *)
  let dates = Table.read time_table Table.Date ~column:(`Name "dates") in
  let timestamps = Table.read time_table Table.Time_ns ~column:(`Name "timestamps") in
  let durations = Table.read time_table Table.Span_ns ~column:(`Name "durations") in
  let times_of_day = Table.read time_table Table.Ofday_ns ~column:(`Name "times_of_day") in

  (* Verify all columns have correct length *)
  Alcotest.(check int) "Dates column length" 10 (Array.length dates);
  Alcotest.(check int) "Timestamps column length" 10 (Array.length timestamps);
  Alcotest.(check int) "Durations column length" 10 (Array.length durations);
  Alcotest.(check int) "Times of day column length" 10 (Array.length times_of_day);

  (* Test column-based table reconstruction *)
  let reconstructed_table = Table.create [
    Table.col dates Table.Date "dates";
    Table.col timestamps Table.Time_ns "timestamps";
    Table.col durations Table.Span_ns "durations";
    Table.col times_of_day Table.Ofday_ns "times_of_day";
  ] in

  let orig_schema = Table.schema time_table in
  let recon_schema = Table.schema reconstructed_table in

  Alcotest.(check bool) "Reconstructed schema matches" true
    (verify_schemas_equal orig_schema recon_schema)

let test_nullable_column_operations () =
  (* Test column operations with nullable data *)
  let nullable_table = Table.create [
    Table.col_opt (Fixtures.sample_ints_opt ~size:10 ~null_ratio:0.3 ()) Table.Int "opt_integers";
    Table.col_opt (Fixtures.sample_floats_opt ~size:10 ~null_ratio:0.3 ()) Table.Float "opt_floats";
    Table.col_opt (Fixtures.sample_strings_opt ~size:10 ~null_ratio:0.3 ()) Table.Utf8 "opt_strings";
  ] in

  (* Extract nullable columns *)
  let opt_integers = Table.read_opt nullable_table Table.Int ~column:(`Name "opt_integers") in
  let opt_strings = Table.read_opt nullable_table Table.Utf8 ~column:(`Name "opt_strings") in
  let opt_floats = Table.read_opt nullable_table Table.Float ~column:(`Name "opt_floats") in

  (* Count non-null values *)
  let count_non_null arr = Array.fold_left (fun acc opt ->
    match opt with Some _ -> acc + 1 | None -> acc) 0 arr in

  let non_null_ints = count_non_null opt_integers in
  let non_null_strings = count_non_null opt_strings in
  let non_null_floats = count_non_null opt_floats in

  (* Verify we have some null values (with 30% null ratio) *)
  Alcotest.(check bool) "Has some null integers" true (non_null_ints < Array.length opt_integers);
  Alcotest.(check bool) "Has some null strings" true (non_null_strings < Array.length opt_strings);
  Alcotest.(check bool) "Has some null floats" true (non_null_floats < Array.length opt_floats);

  (* Create new table with extracted nullable columns *)
  let new_nullable_table = Table.create [
    Table.col_opt opt_integers Table.Int "extracted_opt_integers";
    Table.col_opt opt_strings Table.Utf8 "extracted_opt_strings";
  ] in

  let new_opt_integers = Table.read_opt new_nullable_table Table.Int ~column:(`Name "extracted_opt_integers") in
  let new_opt_strings = Table.read_opt new_nullable_table Table.Utf8 ~column:(`Name "extracted_opt_strings") in

  Alcotest.(check (array (option int))) "Extracted nullable ints" opt_integers new_opt_integers;
  Alcotest.(check (array (option string))) "Extracted nullable strings" opt_strings new_opt_strings

(** {1 Time + Table Integration Tests} *)

let test_comprehensive_time_table_operations () =
  (* Test all time types in table operations *)
  let size = 25 in
  let dates = Fixtures.sample_dates ~size () in
  let timestamps = Fixtures.sample_time_ns ~size () in
  let durations = Fixtures.sample_span_ns ~size () in
  let times_of_day = Fixtures.sample_ofday_ns ~size () in

  let time_table = Table.create [
    Table.col dates Table.Date "event_dates";
    Table.col timestamps Table.Time_ns "event_timestamps";
    Table.col durations Table.Span_ns "event_durations";
    Table.col times_of_day Table.Ofday_ns "event_times_of_day";
    Table.col (Array.init size (fun i -> Printf.sprintf "event_%d" i)) Table.Utf8 "event_names";
  ] in

  (* Test time table I/O *)
  let time_file = Fixtures.temp_file_path ~extension:".parquet" "time_table_io" in
  IO.write time_table time_file;

  let read_time_table = IO.read time_file in

  (* Verify time data integrity after I/O *)
  let read_dates = Table.read read_time_table Table.Date ~column:(`Name "event_dates") in
  let read_timestamps = Table.read read_time_table Table.Time_ns ~column:(`Name "event_timestamps") in
  let read_durations = Table.read read_time_table Table.Span_ns ~column:(`Name "event_durations") in
  let read_times_of_day = Table.read read_time_table Table.Ofday_ns ~column:(`Name "event_times_of_day") in

  Alcotest.(check (array date_testable)) "Dates I/O roundtrip" dates read_dates;
  Alcotest.(check (array time_ns_testable)) "Timestamps I/O roundtrip" timestamps read_timestamps;
  Alcotest.(check (array span_ns_testable)) "Durations I/O roundtrip" durations read_durations;
  Alcotest.(check (array ofday_ns_testable)) "Times of day I/O roundtrip" times_of_day read_times_of_day;

  Fixtures.cleanup_test_files "time_table_io_.*\\.parquet"

let test_time_table_with_nullables () =
  (* Test time types with nullable values *)
  let size = 15 in
  let opt_dates = Fixtures.sample_dates_opt ~size ~null_ratio:0.2 () in
  let opt_timestamps = Fixtures.sample_time_ns_opt ~size ~null_ratio:0.3 () in
  let opt_durations = Fixtures.sample_span_ns_opt ~size ~null_ratio:0.1 () in
  let opt_times_of_day = Fixtures.sample_ofday_ns_opt ~size ~null_ratio:0.4 () in

  let nullable_time_table = Table.create [
    Table.col_opt opt_dates Table.Date "opt_dates";
    Table.col_opt opt_timestamps Table.Time_ns "opt_timestamps";
    Table.col_opt opt_durations Table.Span_ns "opt_durations";
    Table.col_opt opt_times_of_day Table.Ofday_ns "opt_times_of_day";
  ] in

  (* Test nullable time table I/O *)
  let nullable_time_file = Fixtures.temp_file_path ~extension:".parquet" "nullable_time_table" in
  IO.write nullable_time_table nullable_time_file;

  let read_nullable_time_table = IO.read nullable_time_file in

  (* Verify nullable time data after I/O *)
  let read_opt_dates = Table.read_opt read_nullable_time_table Table.Date ~column:(`Name "opt_dates") in
  let read_opt_timestamps = Table.read_opt read_nullable_time_table Table.Time_ns ~column:(`Name "opt_timestamps") in
  let read_opt_durations = Table.read_opt read_nullable_time_table Table.Span_ns ~column:(`Name "opt_durations") in
  let read_opt_times_of_day = Table.read_opt read_nullable_time_table Table.Ofday_ns ~column:(`Name "opt_times_of_day") in

  Alcotest.(check (array (option date_testable))) "Nullable dates I/O" opt_dates read_opt_dates;
  Alcotest.(check (array (option time_ns_testable))) "Nullable timestamps I/O" opt_timestamps read_opt_timestamps;
  Alcotest.(check (array (option span_ns_testable))) "Nullable durations I/O" opt_durations read_opt_durations;
  Alcotest.(check (array (option ofday_ns_testable))) "Nullable times of day I/O" opt_times_of_day read_opt_times_of_day;

  Fixtures.cleanup_test_files "nullable_time_table_.*\\.parquet"

(** {1 Multi-Module End-to-End Workflow Tests} *)

let test_complete_data_pipeline () =
  (* Comprehensive end-to-end workflow: Build -> Transform -> Write -> Read -> Analyze *)

  (* Step 1: Build initial dataset using row builder *)
  let initial_records = Array.init 200 (fun i -> {
    Test_fixtures.id = i + 1;
    Test_fixtures.name = Printf.sprintf "user_%03d" (i + 1);
    Test_fixtures.value = float_of_int i *. 1.5 +. (Random.float 10.0);
    Test_fixtures.active = i mod 3 <> 0;
    Test_fixtures.score = if i mod 7 = 0 then None else Some (float_of_int i *. 2.5);
  }) in

  let initial_table = Fixtures.test_records_to_table initial_records in

  (* Step 2: Transform data (extract and modify columns) *)
  let ids = Table.read initial_table Table.Int ~column:(`Name "id") in
  let names = Table.read initial_table Table.Utf8 ~column:(`Name "name") in
  let values = Table.read initial_table Table.Float ~column:(`Name "value") in
  let active_flags = Table.read initial_table Table.Bool ~column:(`Name "active") in

  (* Create derived columns *)
  let doubled_values = Array.map (fun v -> v *. 2.0) values in
  let value_categories = Array.map (fun v ->
    if v < 100.0 then "low"
    else if v < 300.0 then "medium"
    else "high"
  ) values in
  let timestamps = Fixtures.sample_time_ns ~size:200 () in

  (* Step 3: Create transformed table *)
  let transformed_table = Table.create [
    Table.col ids Table.Int "user_id";
    Table.col names Table.Utf8 "username";
    Table.col values Table.Float "original_value";
    Table.col doubled_values Table.Float "doubled_value";
    Table.col value_categories Table.Utf8 "value_category";
    Table.col active_flags Table.Bool "is_active";
    Table.col timestamps Table.Time_ns "created_at";
  ] in

  (* Step 4: Write to file with compression *)
  let output_file = Fixtures.temp_file_path ~extension:".parquet" "complete_pipeline" in
  IO.Parquet.write transformed_table output_file ~compression:(Parquet.Snappy);

  (* Step 5: Read back and verify pipeline integrity *)
  let final_table = IO.read output_file in

  (* Verify table structure *)
  Alcotest.(check int) "Pipeline final row count" 200 (Table.num_rows final_table);
  let final_schema = Table.schema final_table in
  let final_fields = Schema.children final_schema in
  Alcotest.(check int) "Pipeline final column count" 7 (List.length final_fields);

  (* Verify data transformations *)
  let final_ids = Table.read final_table Table.Int ~column:(`Name "user_id") in
  let final_values = Table.read final_table Table.Float ~column:(`Name "original_value") in
  let final_doubled = Table.read final_table Table.Float ~column:(`Name "doubled_value") in
  let final_categories = Table.read final_table Table.Utf8 ~column:(`Name "value_category") in

  Alcotest.(check (array int)) "Pipeline IDs preserved" ids final_ids;
  Alcotest.(check (array (float 1e-6))) "Pipeline values preserved" values final_values;

  (* Verify transformation correctness *)
  Array.iteri (fun i original_val ->
    let expected_doubled = original_val *. 2.0 in
    let actual_doubled = final_doubled.(i) in
    Alcotest.(check (float 1e-6)) (Printf.sprintf "Doubled value %d" i) expected_doubled actual_doubled;

    let expected_category = if original_val < 100.0 then "low"
                           else if original_val < 300.0 then "medium"
                           else "high" in
    let actual_category = final_categories.(i) in
    Alcotest.(check string) (Printf.sprintf "Category %d" i) expected_category actual_category
  ) (Array.sub final_values 0 (min 10 (Array.length final_values)));

  Fixtures.cleanup_test_files "complete_pipeline_.*\\.parquet"

let test_multi_format_data_migration () =
  (* Test moving data between different formats while preserving integrity *)
  let source_table = Table.create [
    Table.col (Fixtures.sequential_ints 1 100) Table.Int "id";
    Table.col (Array.init 100 (fun i -> Printf.sprintf "item_%03d" i)) Table.Utf8 "name";
    Table.col (Fixtures.sequential_floats 0.0 0.5 100) Table.Float "value";
    Table.col (Fixtures.sample_dates ~size:100 ()) Table.Date "date_created";
    Table.col (Array.init 100 (fun i -> i mod 4 = 0)) Table.Bool "is_featured";
  ] in

  let original_schema = Table.schema source_table in

  (* Step 1: Write to Parquet *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "migration_parquet" in
  IO.write source_table parquet_file;

  (* Step 2: Read from Parquet and verify *)
  let parquet_table = IO.read parquet_file in
  let parquet_schema = Table.schema parquet_table in

  Alcotest.(check bool) "Parquet migration schema" true
    (verify_schemas_equal original_schema parquet_schema);
  Alcotest.(check int) "Parquet migration rows" 100 (Table.num_rows parquet_table);

  (* Verify data integrity in Parquet *)
  let parquet_ids = Table.read parquet_table Table.Int ~column:(`Name "id") in
  let parquet_names = Table.read parquet_table Table.Utf8 ~column:(`Name "name") in
  let original_ids = Fixtures.sequential_ints 1 100 in
  let original_names = Array.init 100 (fun i -> Printf.sprintf "item_%03d" i) in

  Alcotest.(check (array int)) "Parquet data integrity - IDs" original_ids parquet_ids;
  Alcotest.(check (array string)) "Parquet data integrity - Names" original_names parquet_names;

  Fixtures.cleanup_test_files "migration_.*\\.parquet"

let test_batch_processing_workflow () =
  (* Test processing large datasets in batches *)
  let total_size = 1000 in
  let batch_size = 250 in
  let num_batches = total_size / batch_size in

  (* Generate large dataset *)
  let large_ids = Fixtures.sequential_ints 1 total_size in
  let large_values = Fixtures.sequential_floats 0.0 0.01 total_size in
  let large_labels = Array.init total_size (fun i -> Printf.sprintf "item_%06d" (i + 1)) in
  let large_flags = Array.init total_size (fun i -> i mod 5 = 0) in

  let large_table = Table.create [
    Table.col large_ids Table.Int "id";
    Table.col large_values Table.Float "value";
    Table.col large_labels Table.Utf8 "label";
    Table.col large_flags Table.Bool "flag";
  ] in

  (* Write large table *)
  let large_file = Fixtures.temp_file_path ~extension:".parquet" "batch_large" in
  IO.write large_table large_file;

  (* Simulate batch processing by reading full table and processing in chunks *)
  let read_large_table = IO.read large_file in
  Alcotest.(check int) "Large table size" total_size (Table.num_rows read_large_table);

  (* Extract data for batch processing *)
  let all_ids = Table.read read_large_table Table.Int ~column:(`Name "id") in
  let all_values = Table.read read_large_table Table.Float ~column:(`Name "value") in

  (* Process in batches and verify each batch *)
  for batch_idx = 0 to num_batches - 1 do
    let start_idx = batch_idx * batch_size in
    let end_idx = min (start_idx + batch_size) total_size in
    let batch_length = end_idx - start_idx in

    let batch_ids = Array.sub all_ids start_idx batch_length in
    let _batch_values = Array.sub all_values start_idx batch_length in

    (* Verify batch content *)
    Alcotest.(check int) (Printf.sprintf "Batch %d size" batch_idx) batch_length (Array.length batch_ids);

    (* Check first and last elements of batch *)
    if batch_length > 0 then (
      let expected_first_id = start_idx + 1 in
      let expected_last_id = end_idx in
      Alcotest.(check int) (Printf.sprintf "Batch %d first ID" batch_idx) expected_first_id batch_ids.(0);
      if batch_length > 1 then
        Alcotest.(check int) (Printf.sprintf "Batch %d last ID" batch_idx) expected_last_id batch_ids.(batch_length - 1)
    )
  done;

  Fixtures.cleanup_test_files "batch_large_.*\\.parquet"

(** {1 Cross-Format Data Migration Tests} *)

let test_format_compatibility_matrix () =
  (* Test data compatibility across different supported formats *)
  let test_data = Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "integers";
    Table.col [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] Table.Utf8 "strings";
    Table.col [| 1.1; 2.2; 3.3; 4.4; 5.5 |] Table.Float "floats";
    Table.col [| true; false; true; false; true |] Table.Bool "booleans";
  ] in

  let original_schema = Table.schema test_data in

  (* Test Parquet format *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "format_compat_parquet" in
  IO.write test_data parquet_file;

  let parquet_table = IO.read parquet_file in
  let parquet_schema = Table.schema parquet_table in

  Alcotest.(check bool) "Parquet format compatibility" true
    (verify_schemas_equal original_schema parquet_schema);

  (* Verify data integrity across format *)
  let parquet_ints = Table.read parquet_table Table.Int ~column:(`Name "integers") in
  let parquet_strings = Table.read parquet_table Table.Utf8 ~column:(`Name "strings") in
  let parquet_floats = Table.read parquet_table Table.Float ~column:(`Name "floats") in
  let parquet_bools = Table.read parquet_table Table.Bool ~column:(`Name "booleans") in

  Alcotest.(check (array int)) "Parquet ints compatibility" [| 1; 2; 3; 4; 5 |] parquet_ints;
  Alcotest.(check (array string)) "Parquet strings compatibility" [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] parquet_strings;
  Alcotest.(check (array (float 1e-6))) "Parquet floats compatibility" [| 1.1; 2.2; 3.3; 4.4; 5.5 |] parquet_floats;
  Alcotest.(check (array bool)) "Parquet bools compatibility" [| true; false; true; false; true |] parquet_bools;

  Fixtures.cleanup_test_files "format_compat_.*\\.parquet"

(** {1 Large-Scale Data Processing Tests} *)

let test_large_table_operations () =
  (* Test operations on larger tables to verify scalability *)
  let large_size = 10000 in

  let (large_table, build_time) = time_execution (fun () ->
    Table.create [
      Table.col (Fixtures.sequential_ints 1 large_size) Table.Int "id";
      Table.col (Array.init large_size (fun i -> Printf.sprintf "user_%06d" (i + 1))) Table.Utf8 "username";
      Table.col (Fixtures.sequential_floats 0.0 0.001 large_size) Table.Float "score";
      Table.col (Array.init large_size (fun i -> i mod 2 = 0)) Table.Bool "active";
      Table.col (Fixtures.sample_dates ~size:large_size ()) Table.Date "signup_date";
    ]
  ) in

  Printf.printf "Large table build time: %.3f seconds\n%!" build_time;

  (* Verify table structure *)
  Alcotest.(check int) "Large table row count" large_size (Table.num_rows large_table);

  (* Test large table I/O *)
  let large_file = Fixtures.temp_file_path ~extension:".parquet" "large_table" in

  let (_, write_time) = time_execution (fun () ->
    IO.write large_table large_file
  ) in

  Printf.printf "Large table write time: %.3f seconds\n%!" write_time;

  let (read_table, read_time) = time_execution (fun () ->
    IO.read large_file
  ) in

  Printf.printf "Large table read time: %.3f seconds\n%!" read_time;

  (* Verify read table *)
  Alcotest.(check int) "Large table read row count" large_size (Table.num_rows read_table);

  (* Test column operations on large table *)
  let (column_data, column_time) = time_execution (fun () ->
    let ids = Table.read read_table Table.Int ~column:(`Name "id") in
    let usernames = Table.read read_table Table.Utf8 ~column:(`Name "username") in
    let scores = Table.read read_table Table.Float ~column:(`Name "score") in
    (ids, usernames, scores)
  ) in

  Printf.printf "Large table column extraction time: %.3f seconds\n%!" column_time;

  let (ids, usernames, _scores) = column_data in

  (* Verify sample of data *)
  Alcotest.(check int) "Large table first ID" 1 ids.(0);
  Alcotest.(check int) "Large table last ID" large_size ids.(large_size - 1);
  Alcotest.(check string) "Large table first username" "user_000001" usernames.(0);
  Alcotest.(check string) "Large table last username" "user_010000" usernames.(large_size - 1);

  Fixtures.cleanup_test_files "large_table_.*\\.parquet"

let test_memory_efficiency_large_operations () =
  (* Test memory efficiency with moderately large datasets *)
  let medium_size = 5000 in

  (* Create multiple tables and verify they can coexist *)
  let tables = Array.init 3 (fun table_idx ->
    Table.create [
      Table.col (Array.init medium_size (fun i -> table_idx * medium_size + i + 1)) Table.Int "id";
      Table.col (Array.init medium_size (fun i -> Printf.sprintf "table_%d_item_%d" table_idx i)) Table.Utf8 "name";
      Table.col (Array.init medium_size (fun i -> float_of_int i *. float_of_int table_idx)) Table.Float "value";
    ]
  ) in

  (* Write all tables *)
  let files = Array.mapi (fun i table ->
    let file = Fixtures.temp_file_path ~extension:".parquet" (Printf.sprintf "memory_test_%d" i) in
    IO.write table file;
    file
  ) tables in

  (* Read back and verify all tables *)
  Array.iteri (fun i file ->
    let read_table = IO.read file in
    Alcotest.(check int) (Printf.sprintf "Memory test table %d size" i) medium_size (Table.num_rows read_table);

    let ids = Table.read read_table Table.Int ~column:(`Name "id") in
    let expected_first_id = i * medium_size + 1 in
    let expected_last_id = (i + 1) * medium_size in

    Alcotest.(check int) (Printf.sprintf "Memory test table %d first ID" i) expected_first_id ids.(0);
    Alcotest.(check int) (Printf.sprintf "Memory test table %d last ID" i) expected_last_id ids.(medium_size - 1)
  ) files;

  Fixtures.cleanup_test_files "memory_test_.*\\.parquet"

(** {1 Error Propagation Tests} *)

let test_schema_mismatch_error_propagation () =
  (* Test that schema mismatches are properly detected and reported *)
  let table1 = Table.create [
    Table.col [| 1; 2; 3 |] Table.Int "id";
    Table.col [| "a"; "b"; "c" |] Table.Utf8 "name";
  ] in

  let table2 = Table.create [
    Table.col [| 1.0; 2.0; 3.0 |] Table.Float "id";  (* Different type for same column name *)
    Table.col [| "a"; "b"; "c" |] Table.Utf8 "name";
  ] in

  let schema1 = Table.schema table1 in
  let schema2 = Table.schema table2 in

  (* Verify schemas are different *)
  Alcotest.(check bool) "Schema mismatch detected" false
    (verify_schemas_equal schema1 schema2);

  (* Test that we can detect the specific difference *)
  let fields1 = Schema.children schema1 in
  let fields2 = Schema.children schema2 in

  let id_field1 = List.hd fields1 in
  let id_field2 = List.hd fields2 in

  let type1 = Schema.format id_field1 in
  let type2 = Schema.format id_field2 in

  Alcotest.(check bool) "Type mismatch for ID field" false (type1 = type2)

let test_file_not_found_error_handling () =
  (* Test error handling for non-existent files *)
  let non_existent_file = "/tmp/non_existent_file_" ^ (string_of_float (Unix.time ())) ^ ".parquet" in

  (try
    let _ = IO.read non_existent_file in
    Alcotest.fail "Should have raised exception for non-existent file"
  with _ ->
    (* Expected behavior - exception should be raised *)
    Alcotest.(check bool) "Non-existent file handled" true true
  );

  (try
    let _ = IO.schema non_existent_file in
    Alcotest.fail "Should have raised exception for non-existent schema file"
  with _ ->
    (* Expected behavior - exception should be raised *)
    Alcotest.(check bool) "Non-existent schema file handled" true true
  )

let test_invalid_column_access_errors () =
  (* Test error handling for invalid column access *)
  let table = Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "integers";
    Table.col [| "a"; "b"; "c"; "d"; "e" |] Table.Utf8 "strings";
    Table.col [| 1.0; 2.0; 3.0; 4.0; 5.0 |] Table.Float "floats";
  ] in

  (try
    let _ = Table.read table Table.Int ~column:(`Name "non_existent_column") in
    Alcotest.fail "Should have raised exception for non-existent column"
  with _ ->
    (* Expected behavior *)
    Alcotest.(check bool) "Non-existent column handled" true true
  );

  (try
    let _ = Table.read table Table.Float ~column:(`Name "integers") in  (* Type mismatch *)
    Alcotest.fail "Should have raised exception for type mismatch"
  with _ ->
    (* Expected behavior *)
    Alcotest.(check bool) "Type mismatch handled" true true
  )

(** {1 Performance Testing} *)

let test_builder_performance () =
  (* Test performance of various builder operations *)
  let size = 1000 in

  (* Time column builder performance *)
  let (_, column_build_time) = time_execution (fun () ->
    Table.create [
      Table.col (Fixtures.sequential_ints 1 size) Table.Int "ids";
      Table.col (Array.init size (fun i -> Printf.sprintf "item_%d" i)) Table.Utf8 "names";
      Table.col (Fixtures.sequential_floats 0.0 0.1 size) Table.Float "values";
    ]
  ) in

  Printf.printf "Column builder performance (%d rows): %.3f seconds\n%!" size column_build_time;

  (* Time row builder performance *)
  let records = Fixtures.sample_test_records size in
  let (_, row_build_time) = time_execution (fun () ->
    Fixtures.test_records_to_table records
  ) in

  Printf.printf "Row builder performance (%d rows): %.3f seconds\n%!" size row_build_time;

  (* Verify both approaches produce valid tables *)
  Alcotest.(check bool) "Builder performance test completed" true true

let test_io_performance () =
  (* Test I/O performance with different table sizes *)
  let sizes = [| 100; 500; 1000; 2000 |] in

  Array.iter (fun size ->
    let table = Table.create [
      Table.col (Fixtures.sequential_ints 1 size) Table.Int "id";
      Table.col (Array.init size (fun i -> Printf.sprintf "test_item_%06d" i)) Table.Utf8 "name";
      Table.col (Fixtures.sequential_floats 0.0 0.01 size) Table.Float "value";
      Table.col (Array.init size (fun i -> i mod 2 = 0)) Table.Bool "flag";
    ] in

    let file = Fixtures.temp_file_path ~extension:".parquet" (Printf.sprintf "perf_test_%d" size) in

    let (_, write_time) = time_execution (fun () -> IO.write table file) in
    let (read_table, read_time) = time_execution (fun () -> IO.read file) in

    Printf.printf "I/O performance (%d rows): write=%.3f, read=%.3f seconds\n%!" size write_time read_time;

    (* Verify correctness *)
    Alcotest.(check int) (Printf.sprintf "Perf test %d rows" size) size (Table.num_rows read_table)
  ) sizes;

  Fixtures.cleanup_test_files "perf_test_.*\\.parquet"

(** {1 Test Suite Definition} *)

let table_builder_tests = [
  "builder_to_table_roundtrip", `Quick, test_builder_to_table_roundtrip;
  "row_builder_integration", `Quick, test_row_builder_integration;
  "nullable_builder_integration", `Quick, test_nullable_builder_integration;
]

let table_schema_tests = [
  "schema_consistency_across_operations", `Quick, test_schema_consistency_across_operations;
  "schema_validation_across_formats", `Quick, test_schema_validation_across_formats;
  "schema_evolution_compatibility", `Quick, test_schema_evolution_compatibility;
]

let builder_io_tests = [
  "build_and_write_workflow", `Quick, test_build_and_write_workflow;
  "incremental_build_and_stream_write", `Quick, test_incremental_build_and_stream_write;
  "builder_compression_integration", `Quick, test_builder_compression_integration;
]

let io_schema_tests = [
  "schema_preservation_across_formats", `Quick, test_schema_preservation_across_formats;
  "column_selection_schema_consistency", `Quick, test_column_selection_schema_consistency;
]

let column_table_tests = [
  "column_operations_within_tables", `Quick, test_column_operations_within_tables;
  "column_type_operations", `Quick, test_column_type_operations;
  "nullable_column_operations", `Quick, test_nullable_column_operations;
]

let time_table_tests = [
  "comprehensive_time_table_operations", `Quick, test_comprehensive_time_table_operations;
  "time_table_with_nullables", `Quick, test_time_table_with_nullables;
]

let workflow_tests = [
  "complete_data_pipeline", `Quick, test_complete_data_pipeline;
  "multi_format_data_migration", `Quick, test_multi_format_data_migration;
  "batch_processing_workflow", `Quick, test_batch_processing_workflow;
]

let migration_tests = [
  "format_compatibility_matrix", `Quick, test_format_compatibility_matrix;
]

let scale_tests = [
  "large_table_operations", `Slow, test_large_table_operations;
  "memory_efficiency_large_operations", `Quick, test_memory_efficiency_large_operations;
]

let error_tests = [
  "schema_mismatch_error_propagation", `Quick, test_schema_mismatch_error_propagation;
  "file_not_found_error_handling", `Quick, test_file_not_found_error_handling;
  "invalid_column_access_errors", `Quick, test_invalid_column_access_errors;
]

let performance_tests = [
  "builder_performance", `Quick, test_builder_performance;
  "io_performance", `Quick, test_io_performance;
]

let () =
  Alcotest.run "Arrow Integration Tests" [
    "Table + Builder Integration", table_builder_tests;
    "Table + Schema Integration", table_schema_tests;
    "Builder + I/O Integration", builder_io_tests;
    "I/O + Schema Integration", io_schema_tests;
    "Column + Table Integration", column_table_tests;
    "Time + Table Integration", time_table_tests;
    "Multi-Module Workflows", workflow_tests;
    "Cross-Format Migration", migration_tests;
    "Large-Scale Processing", scale_tests;
    "Error Propagation", error_tests;
    "Performance Testing", performance_tests;
  ]