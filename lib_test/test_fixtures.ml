(** Comprehensive test fixtures module for Arrow OCaml library tests

    This module provides:
    - Sample data generators for all Arrow data types
    - Standard test tables with known data patterns
    - File path helpers for test files in build directory
    - Comparison helpers for floating point and time values
    - Common test data patterns (sequential, random, edge values)
*)

open Arrow

(** {2 Utility Functions} *)

(** Generate a random seed based on current time for reproducible tests *)
let get_test_seed () =
  Random.self_init ();
  let seed = Random.int 1000000 in
  Random.init seed;
  seed

(** Create a temporary filename in the _build/test/ directory *)
let temp_file_path ?(extension = ".arrow") prefix =
  let test_dir = "_build/test" in
  let () =
    try Unix.mkdir "_build" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ();
    try Unix.mkdir test_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  let timestamp = string_of_float (Unix.time ()) in
  let filename = Printf.sprintf "%s_%s%s" prefix timestamp extension in
  Filename.concat test_dir filename

(** Clean up test files by pattern *)
let cleanup_test_files pattern =
  let test_dir = "_build/test" in
  try
    let files = Sys.readdir test_dir in
    Array.iter (fun file ->
      if Str.string_match (Str.regexp pattern) file 0 then
        try Unix.unlink (Filename.concat test_dir file)
        with _ -> ()
    ) files
  with Sys_error _ -> ()

(** {2 Comparison Helpers} *)

(** Floating point comparison with tolerance *)
let float_equal ?(tolerance = 1e-6) a b =
  abs_float (a -. b) < tolerance

(** Array floating point comparison with tolerance *)
let float_array_equal ?(tolerance = 1e-6) arr1 arr2 =
  Array.length arr1 = Array.length arr2 &&
  Array.for_all2 (float_equal ~tolerance) arr1 arr2

(** Optional floating point comparison *)
let float_option_equal ?(tolerance = 1e-6) opt1 opt2 =
  match opt1, opt2 with
  | None, None -> true
  | Some a, Some b -> float_equal ~tolerance a b
  | _ -> false

(** Array of optional floating point comparison *)
let float_option_array_equal ?(tolerance = 1e-6) arr1 arr2 =
  Array.length arr1 = Array.length arr2 &&
  Array.for_all2 (float_option_equal ~tolerance) arr1 arr2

(** Time comparison helpers *)
let time_ns_equal t1 t2 =
  Time.Time_ns.to_int64_ns_since_epoch t1 = Time.Time_ns.to_int64_ns_since_epoch t2

let span_ns_equal s1 s2 =
  Time.Time_ns.Span.to_ns s1 = Time.Time_ns.Span.to_ns s2

let ofday_ns_equal o1 o2 =
  Time.Time_ns.Ofday.to_ns_since_midnight o1 = Time.Time_ns.Ofday.to_ns_since_midnight o2

let date_equal d1 d2 =
  Time.Date.to_unix_days d1 = Time.Date.to_unix_days d2

(** {2 Data Generators} *)

(** Generate sample integers with edge cases *)
let sample_ints ?(size = 10) () =
  let base_values = [| 0; 1; -1; 42; -42; Int.max_int; Int.min_int |] in
  let random_values = Array.init (max 0 (size - Array.length base_values))
    (fun _ -> Random.int 1000 - 500) in
  let all_values = Array.append base_values random_values in
  if Array.length all_values > size then Array.sub all_values 0 size else all_values

(** Generate sample 32-bit integers *)
let sample_int32s ?(size = 10) () =
  let base_values = [| 0l; 1l; -1l; 42l; -42l; Int32.max_int; Int32.min_int |] in
  let random_values = Array.init (max 0 (size - Array.length base_values))
    (fun _ -> Int32.of_int (Random.int 1000 - 500)) in
  let all_values = Array.append base_values random_values in
  if Array.length all_values > size then Array.sub all_values 0 size else all_values

(** Generate sample 64-bit integers *)
let sample_int64s ?(size = 10) () =
  let base_values = [| 0L; 1L; -1L; 42L; -42L; Int64.max_int; Int64.min_int |] in
  let random_values = Array.init (max 0 (size - Array.length base_values))
    (fun _ -> Int64.of_int (Random.int 1000 - 500)) in
  let all_values = Array.append base_values random_values in
  if Array.length all_values > size then Array.sub all_values 0 size else all_values

(** Generate sample floats with edge cases *)
let sample_floats ?(size = 10) () =
  let base_values = [| 0.0; 1.0; -1.0; 3.14159; -2.71828; Float.max_float; Float.min_float;
                      Float.infinity; Float.neg_infinity |] in
  let random_values = Array.init (max 0 (size - Array.length base_values))
    (fun _ -> Random.float 1000.0 -. 500.0) in
  let all_values = Array.append base_values random_values in
  if Array.length all_values > size then Array.sub all_values 0 size else all_values

(** Generate sample strings *)
let sample_strings ?(size = 10) () =
  let base_values = [| ""; "hello"; "world"; "test"; "arrow"; "ocaml"; "🚀"; "café";
                      "very long string with lots of text to test string handling" |] in
  let random_values = Array.init (max 0 (size - Array.length base_values))
    (fun i -> Printf.sprintf "random_%d" i) in
  let all_values = Array.append base_values random_values in
  if Array.length all_values > size then Array.sub all_values 0 size else all_values

(** Generate sample booleans *)
let sample_bools ?(size = 10) () =
  Array.init size (fun i -> i mod 2 = 0)

(** Generate sample dates *)
let sample_dates ?(size = 10) () =
  let base_days = [| 0; 1; 365; 18000; 18500; 19000 |] in
  let random_days = Array.init (max 0 (size - Array.length base_days))
    (fun _ -> Random.int 20000) in
  let all_days = Array.append base_days random_days in
  let days = if Array.length all_days > size then Array.sub all_days 0 size else all_days in
  Array.map Time.Date.of_unix_days days

(** Generate sample timestamps *)
let sample_time_ns ?(size = 10) () =
  let base_timestamps = [| 0L; 1234567890000000000L; 1609459200000000000L;
                          1640995200000000000L |] in
  let random_timestamps = Array.init (max 0 (size - Array.length base_timestamps))
    (fun _ -> Int64.of_float (Random.float 2000000000.0 *. 1000000000.0)) in
  let all_timestamps = Array.append base_timestamps random_timestamps in
  let timestamps = if Array.length all_timestamps > size then Array.sub all_timestamps 0 size else all_timestamps in
  Array.map Time.Time_ns.of_int64_ns_since_epoch timestamps

(** Generate sample time spans *)
let sample_span_ns ?(size = 10) () =
  let base_spans_ns = [| 0L; 1000000000L; 3600000000000L; 86400000000000L |] in
  let random_spans = Array.init (max 0 (size - Array.length base_spans_ns))
    (fun _ -> Int64.mul (Int64.of_int (Random.int 86400)) 1000000000L) in
  let all_spans = Array.append base_spans_ns random_spans in
  let spans_ns = if Array.length all_spans > size then Array.sub all_spans 0 size else all_spans in
  Array.map Time.Time_ns.Span.of_ns spans_ns

(** Generate sample time of day *)
let sample_ofday_ns ?(size = 10) () =
  let base_ofdays_ns = [| 0L; 3600000000000L; 43200000000000L; 82800000000000L |] in
  let random_ofdays = Array.init (max 0 (size - Array.length base_ofdays_ns))
    (fun _ -> Int64.of_int (Random.int 86400 * 1000000000)) in
  let all_ofdays = Array.append base_ofdays_ns random_ofdays in
  let ofdays_ns = if Array.length all_ofdays > size then Array.sub all_ofdays 0 size else all_ofdays in
  Array.map Time.Time_ns.Ofday.of_ns_since_midnight ofdays_ns

(** {2 Optional Data Generators} *)

(** Make some elements of an array optional (introduce nulls) *)
let make_optional ?(null_ratio = 0.3) arr =
  Array.mapi (fun _i x -> if Random.float 1.0 < null_ratio then None else Some x) arr

(** Generate sample optional integers *)
let sample_ints_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_ints ~size ())

(** Generate sample optional 32-bit integers *)
let sample_int32s_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_int32s ~size ())

(** Generate sample optional 64-bit integers *)
let sample_int64s_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_int64s ~size ())

(** Generate sample optional floats *)
let sample_floats_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_floats ~size ())

(** Generate sample optional strings *)
let sample_strings_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_strings ~size ())

(** Generate sample optional booleans *)
let sample_bools_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_bools ~size ())

(** Generate sample optional dates *)
let sample_dates_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_dates ~size ())

(** Generate sample optional timestamps *)
let sample_time_ns_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_time_ns ~size ())

(** Generate sample optional time spans *)
let sample_span_ns_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_span_ns ~size ())

(** Generate sample optional time of day *)
let sample_ofday_ns_opt ?(size = 10) ?(null_ratio = 0.3) () =
  make_optional ~null_ratio (sample_ofday_ns ~size ())

(** {2 Standard Test Tables} *)

(** Create a small test table (10 rows) with all supported data types *)
let small_test_table () =
  Table.create [
    Table.col (sample_ints ~size:10 ()) Table.Int "integers";
    Table.col (sample_floats ~size:10 ()) Table.Float "floats";
    Table.col (sample_strings ~size:10 ()) Table.Utf8 "strings";
    Table.col (sample_bools ~size:10 ()) Table.Bool "booleans";
    Table.col (sample_dates ~size:10 ()) Table.Date "dates";
    Table.col (sample_time_ns ~size:10 ()) Table.Time_ns "timestamps";
    Table.col (sample_span_ns ~size:10 ()) Table.Span_ns "spans";
    Table.col (sample_ofday_ns ~size:10 ()) Table.Ofday_ns "ofdays";
  ]

(** Create a medium test table (100 rows) *)
let medium_test_table () =
  Table.create [
    Table.col (sample_ints ~size:100 ()) Table.Int "integers";
    Table.col (sample_floats ~size:100 ()) Table.Float "floats";
    Table.col (sample_strings ~size:100 ()) Table.Utf8 "strings";
    Table.col (sample_bools ~size:100 ()) Table.Bool "booleans";
  ]

(** Create an empty test table with all column types *)
let empty_test_table () =
  Table.create [
    Table.col [||] Table.Int "integers";
    Table.col [||] Table.Float "floats";
    Table.col [||] Table.Utf8 "strings";
    Table.col [||] Table.Bool "booleans";
    Table.col [||] Table.Date "dates";
    Table.col [||] Table.Time_ns "timestamps";
    Table.col [||] Table.Span_ns "spans";
    Table.col [||] Table.Ofday_ns "ofdays";
  ]

(** Create a single-row test table *)
let single_row_test_table () =
  Table.create [
    Table.col [| 42 |] Table.Int "integers";
    Table.col [| 3.14 |] Table.Float "floats";
    Table.col [| "hello" |] Table.Utf8 "strings";
    Table.col [| true |] Table.Bool "booleans";
    Table.col [| Time.Date.of_unix_days 18000 |] Table.Date "dates";
    Table.col [| Time.Time_ns.of_int64_ns_since_epoch 1234567890000000000L |] Table.Time_ns "timestamps";
    Table.col [| Time.Time_ns.Span.of_ns 1000000000L |] Table.Span_ns "spans";
    Table.col [| Time.Time_ns.Ofday.of_ns_since_midnight 3600000000000L |] Table.Ofday_ns "ofdays";
  ]

(** Create a test table with nulls/optional values *)
let nullable_test_table () =
  Table.create [
    Table.col_opt (sample_ints_opt ~size:10 ~null_ratio:0.3 ()) Table.Int "opt_integers";
    Table.col_opt (sample_floats_opt ~size:10 ~null_ratio:0.3 ()) Table.Float "opt_floats";
    Table.col_opt (sample_strings_opt ~size:10 ~null_ratio:0.3 ()) Table.Utf8 "opt_strings";
    Table.col_opt (sample_bools_opt ~size:10 ~null_ratio:0.3 ()) Table.Bool "opt_booleans";
    Table.col_opt (sample_dates_opt ~size:10 ~null_ratio:0.3 ()) Table.Date "opt_dates";
    Table.col_opt (sample_time_ns_opt ~size:10 ~null_ratio:0.3 ()) Table.Time_ns "opt_timestamps";
    Table.col_opt (sample_span_ns_opt ~size:10 ~null_ratio:0.3 ()) Table.Span_ns "opt_spans";
    Table.col_opt (sample_ofday_ns_opt ~size:10 ~null_ratio:0.3 ()) Table.Ofday_ns "opt_ofdays";
  ]

(** {2 Edge Case Tables} *)

(** Create a table with extreme values *)
let edge_values_table () =
  Table.create [
    Table.col [| Int.max_int; Int.min_int; 0 |] Table.Int "extreme_ints";
    Table.col [| Float.max_float; Float.min_float; 0.0; Float.infinity; Float.neg_infinity |] Table.Float "extreme_floats";
    Table.col [| ""; "very long string " ^ (String.make 1000 'x') |] Table.Utf8 "extreme_strings";
    Table.col [| true; false |] Table.Bool "all_bools";
  ]

(** {2 Data Patterns} *)

(** Create sequential integer data *)
let sequential_ints start length =
  Array.init length (fun i -> start + i)

(** Create sequential floats with step *)
let sequential_floats start step length =
  Array.init length (fun i -> start +. (float_of_int i) *. step)

(** Create repeated pattern data *)
let repeated_pattern pattern times =
  let pattern_len = Array.length pattern in
  Array.init (times * pattern_len) (fun i -> pattern.(i mod pattern_len))

(** Create geometric sequence *)
let geometric_sequence start ratio length =
  let rec generate acc current n =
    if n <= 0 then Array.of_list (List.rev acc)
    else generate (current :: acc) (current *. ratio) (n - 1)
  in
  generate [] start length

(** {2 Table Builders with Patterns} *)

(** Create a table with sequential data pattern *)
let sequential_table size =
  Table.create [
    Table.col (sequential_ints 1 size) Table.Int "sequence";
    Table.col (sequential_floats 0.0 0.1 size) Table.Float "linear";
    Table.col (Array.init size (fun i -> Printf.sprintf "item_%03d" i)) Table.Utf8 "labels";
    Table.col (Array.init size (fun i -> i mod 2 = 0)) Table.Bool "even";
  ]

(** Create a table with random data (reproducible with seed) *)
let random_table ?(seed = 12345) size =
  Random.init seed;
  Table.create [
    Table.col (Array.init size (fun _ -> Random.int 1000)) Table.Int "random_ints";
    Table.col (Array.init size (fun _ -> Random.float 100.0)) Table.Float "random_floats";
    Table.col (Array.init size (fun i -> if Random.bool () then Printf.sprintf "rand_%d" i else "")) Table.Utf8 "random_strings";
    Table.col (Array.init size (fun _ -> Random.bool ())) Table.Bool "random_bools";
  ]

(** Create a table with repeating patterns *)
let pattern_table pattern_size total_size =
  let base_pattern_int = Array.init pattern_size (fun i -> i + 1) in
  let base_pattern_str = Array.init pattern_size (fun i -> Printf.sprintf "pat_%d" i) in
  let repeats = (total_size + pattern_size - 1) / pattern_size in

  Table.create [
    Table.col (Array.sub (repeated_pattern base_pattern_int repeats) 0 total_size) Table.Int "pattern_ints";
    Table.col (Array.sub (repeated_pattern base_pattern_str repeats) 0 total_size) Table.Utf8 "pattern_strings";
  ]

(** {2 Utility Functions for Testing} *)

(** Verify table has expected number of rows and columns *)
let verify_table_dimensions table ~expected_rows ~expected_columns =
  let actual_rows = Table.num_rows table in
  let schema = Table.schema table in
  let actual_columns = List.length (Schema.children schema) in
  actual_rows = expected_rows && actual_columns = expected_columns

(** Extract all column names from a table *)
let get_column_names table =
  let schema = Table.schema table in
  List.map (fun field -> Schema.name field) (Schema.children schema)

(** Check if table contains a specific column name *)
let has_column table column_name =
  List.mem column_name (get_column_names table)

(** Create a simple test record type for row-based building *)
type test_record = {
  id: int;
  name: string;
  value: float;
  active: bool;
  score: float option;
}

(** Generate sample test records *)
let sample_test_records size =
  Array.init size (fun i ->
    {
      id = i + 1;
      name = Printf.sprintf "record_%d" (i + 1);
      value = float_of_int i *. 1.5;
      active = i mod 2 = 0;
      score = if i mod 3 = 0 then None else Some (float_of_int i *. 10.0);
    }
  )

(** Create table from test records using row-based builder *)
let test_records_to_table records =
  let cols =
    Builder.Row.col ~name:"id" Table.Int (fun r -> r.id) @
    Builder.Row.col ~name:"name" Table.Utf8 (fun r -> r.name) @
    Builder.Row.col ~name:"value" Table.Float (fun r -> r.value) @
    Builder.Row.col ~name:"active" Table.Bool (fun r -> r.active) @
    Builder.Row.col_opt ~name:"score" Table.Float (fun r -> r.score) in
  Builder.Row.array_to_table cols records