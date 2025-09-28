(* Table module tests using external Arrow interface *)
open Arrow

let test_table_creation_basic () =
  (* Test basic table creation with multiple data types *)
  let table = Table.create [
    Table.col [| "v1"; "v2"; "v3" |] Table.Utf8 "strings";
    Table.col [| 1; 2; 3 |] Table.Int "integers";
    Table.col [| 1.1; 2.2; 3.3 |] Table.Float "floats";
    Table.col [| true; false; true |] Table.Bool "booleans";
  ] in

  Alcotest.(check int) "Table has correct number of rows" 3 (Table.num_rows table);

  (* Read back the columns *)
  let strings = Table.read table ~column:(`Name "strings") Table.Utf8 in
  let integers = Table.read table ~column:(`Name "integers") Table.Int in
  let floats = Table.read table ~column:(`Name "floats") Table.Float in
  let booleans = Table.read table ~column:(`Name "booleans") Table.Bool in

  Alcotest.(check (array string)) "Strings column" [| "v1"; "v2"; "v3" |] strings;
  Alcotest.(check (array int)) "Integers column" [| 1; 2; 3 |] integers;
  Alcotest.(check (array (float 1e-6))) "Floats column" [| 1.1; 2.2; 3.3 |] floats;
  Alcotest.(check (array bool)) "Booleans column" [| true; false; true |] booleans

let test_table_optional_columns () =
  (* Test table creation with optional/nullable columns *)
  let table = Table.create [
    Table.col_opt [| Some 1; None; Some 3 |] Table.Int "opt_integers";
    Table.col_opt [| Some "a"; Some "b"; None |] Table.Utf8 "opt_strings";
    Table.col_opt [| None; Some 2.5; Some 3.5 |] Table.Float "opt_floats";
  ] in

  Alcotest.(check int) "Optional table has correct rows" 3 (Table.num_rows table);

  let opt_integers = Table.read_opt table ~column:(`Name "opt_integers") Table.Int in
  let opt_strings = Table.read_opt table ~column:(`Name "opt_strings") Table.Utf8 in
  let opt_floats = Table.read_opt table ~column:(`Name "opt_floats") Table.Float in

  Alcotest.(check (array (option int))) "Optional integers" [| Some 1; None; Some 3 |] opt_integers;
  Alcotest.(check (array (option string))) "Optional strings" [| Some "a"; Some "b"; None |] opt_strings;
  Alcotest.(check (array (option (float 1e-6)))) "Optional floats" [| None; Some 2.5; Some 3.5 |] opt_floats

let test_table_concatenation () =
  (* Test table concatenation *)
  let table1 = Table.create [
    Table.col [| "a"; "b" |] Table.Utf8 "letters";
    Table.col [| 1; 2 |] Table.Int "numbers";
  ] in

  let table2 = Table.create [
    Table.col [| "c"; "d" |] Table.Utf8 "letters";
    Table.col [| 3; 4 |] Table.Int "numbers";
  ] in

  let concatenated = Table.concatenate [table1; table2] in

  Alcotest.(check int) "Concatenated table rows" 4 (Table.num_rows concatenated);

  let letters = Table.read concatenated ~column:(`Name "letters") Table.Utf8 in
  let numbers = Table.read concatenated ~column:(`Name "numbers") Table.Int in

  Alcotest.(check (array string)) "Concatenated letters" [| "a"; "b"; "c"; "d" |] letters;
  Alcotest.(check (array int)) "Concatenated numbers" [| 1; 2; 3; 4 |] numbers

let test_table_slicing () =
  (* Test table slicing *)
  let table = Table.create [
    Table.col [| 0; 1; 2; 3; 4; 5 |] Table.Int "sequence";
    Table.col [| "a"; "b"; "c"; "d"; "e"; "f" |] Table.Utf8 "labels";
  ] in

  let sliced = Table.slice table ~offset:2 ~length:3 in

  Alcotest.(check int) "Sliced table rows" 3 (Table.num_rows sliced);

  let sequence = Table.read sliced ~column:(`Name "sequence") Table.Int in
  let labels = Table.read sliced ~column:(`Name "labels") Table.Utf8 in

  Alcotest.(check (array int)) "Sliced sequence" [| 2; 3; 4 |] sequence;
  Alcotest.(check (array string)) "Sliced labels" [| "c"; "d"; "e" |] labels

let test_table_column_operations () =
  (* Test adding and manipulating columns *)
  let table = Table.create [
    Table.col [| "x"; "y"; "z" |] Table.Utf8 "initial";
  ] in

  (* Get a column from the table *)
  let column = Table.get_column table "initial" in

  (* Create a new table and add the column to it *)
  let table2 = Table.create [
    Table.col [| 1; 2; 3 |] Table.Int "numbers";
  ] in

  let table3 = Table.add_column table2 "copied" column in

  Alcotest.(check int) "Table with added column rows" 3 (Table.num_rows table3);

  let numbers = Table.read table3 ~column:(`Name "numbers") Table.Int in
  let copied = Table.read table3 ~column:(`Name "copied") Table.Utf8 in

  Alcotest.(check (array int)) "Numbers column unchanged" [| 1; 2; 3 |] numbers;
  Alcotest.(check (array string)) "Copied column correct" [| "x"; "y"; "z" |] copied

let test_table_time_types () =
  (* Test time-related column types *)
  let dates = [|
    Time.Date.of_unix_days 18000;
    Time.Date.of_unix_days 18001;
    Time.Date.of_unix_days 18002;
  |] in

  let times = [|
    Time.Time_ns.of_int64_ns_since_epoch 1234567890000000000L;
    Time.Time_ns.of_int64_ns_since_epoch 1234567891000000000L;
    Time.Time_ns.of_int64_ns_since_epoch 1234567892000000000L;
  |] in

  let spans = [|
    Time.Time_ns.Span.of_ns 1000000000L;
    Time.Time_ns.Span.of_ns 2000000000L;
    Time.Time_ns.Span.of_ns 3000000000L;
  |] in

  let ofdays = [|
    Time.Time_ns.Ofday.of_ns_since_midnight 3600000000000L;
    Time.Time_ns.Ofday.of_ns_since_midnight 7200000000000L;
    Time.Time_ns.Ofday.of_ns_since_midnight 10800000000000L;
  |] in

  let table = Table.create [
    Table.col dates Table.Date "dates";
    Table.col times Table.Time_ns "times";
    Table.col spans Table.Span_ns "spans";
    Table.col ofdays Table.Ofday_ns "ofdays";
  ] in

  Alcotest.(check int) "Time types table rows" 3 (Table.num_rows table);

  let read_dates = Table.read table ~column:(`Name "dates") Table.Date in
  let read_times = Table.read table ~column:(`Name "times") Table.Time_ns in
  let read_spans = Table.read table ~column:(`Name "spans") Table.Span_ns in
  let read_ofdays = Table.read table ~column:(`Name "ofdays") Table.Ofday_ns in

  Alcotest.(check int) "Dates array length" 3 (Array.length read_dates);
  Alcotest.(check int) "Times array length" 3 (Array.length read_times);
  Alcotest.(check int) "Spans array length" 3 (Array.length read_spans);
  Alcotest.(check int) "Ofdays array length" 3 (Array.length read_ofdays)

let test_table_schema () =
  (* Test table schema access *)
  let table = Table.create [
    Table.col [| 1; 2; 3 |] Table.Int "id";
    Table.col [| "a"; "b"; "c" |] Table.Utf8 "name";
    Table.col [| 1.5; 2.5; 3.5 |] Table.Float "value";
  ] in

  let schema = Table.schema table in
  let field_names = List.map (fun field -> field.Schema.name) schema.Schema.children in

  Alcotest.(check int) "Schema field count" 3 (List.length schema.Schema.children);
  Alcotest.(check (list string)) "Schema field names" ["id"; "name"; "value"] field_names

let test_table_empty () =
  (* Test empty table creation *)
  let table = Table.create [
    Table.col [||] Table.Int "empty_int";
    Table.col [||] Table.Utf8 "empty_string";
  ] in

  Alcotest.(check int) "Empty table rows" 0 (Table.num_rows table);

  let empty_ints = Table.read table ~column:(`Name "empty_int") Table.Int in
  let empty_strings = Table.read table ~column:(`Name "empty_string") Table.Utf8 in

  Alcotest.(check int) "Empty int array" 0 (Array.length empty_ints);
  Alcotest.(check int) "Empty string array" 0 (Array.length empty_strings)

let () =
  let open Alcotest in
  run "Table tests" [
    "creation", [
      test_case "Basic table creation" `Quick test_table_creation_basic;
      test_case "Optional columns" `Quick test_table_optional_columns;
      test_case "Empty table" `Quick test_table_empty;
    ];
    "operations", [
      test_case "Table concatenation" `Quick test_table_concatenation;
      test_case "Table slicing" `Quick test_table_slicing;
      test_case "Column operations" `Quick test_table_column_operations;
    ];
    "types", [
      test_case "Time types" `Quick test_table_time_types;
      test_case "Schema access" `Quick test_table_schema;
    ];
  ]