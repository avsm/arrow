(* Column module tests using external Arrow interface *)
open Arrow

let create_test_table () =
  (* Create a test table with various column types *)
  Table.create [
    Table.col [| 1; 2; 3; 4; 5 |] Table.Int "integers";
    Table.col [| 1.1; 2.2; 3.3; 4.4; 5.5 |] Table.Float "floats";
    Table.col [| "a"; "b"; "c"; "d"; "e" |] Table.Utf8 "strings";
    Table.col_opt [| Some 10; None; Some 30; None; Some 50 |] Table.Int "opt_integers";
    Table.col_opt [| Some "x"; Some "y"; None; Some "z"; None |] Table.Utf8 "opt_strings";
    Table.col_opt [| Some 1.5; None; Some 3.5; Some 4.5; None |] Table.Float "opt_floats";
  ]

let test_column_reading_by_name () =
  let table = create_test_table () in

  (* Test reading columns by name *)
  let integers = Column.read_int table ~column:(`Name "integers") in
  let floats = Column.read_float table ~column:(`Name "floats") in
  let strings = Column.read_utf8 table ~column:(`Name "strings") in

  Alcotest.(check (array int)) "Read integers by name" [| 1; 2; 3; 4; 5 |] integers;
  Alcotest.(check (array (float 1e-6))) "Read floats by name" [| 1.1; 2.2; 3.3; 4.4; 5.5 |] floats;
  Alcotest.(check (array string)) "Read strings by name" [| "a"; "b"; "c"; "d"; "e" |] strings

let test_column_reading_by_index () =
  let table = create_test_table () in

  (* Test reading columns by index *)
  let integers = Column.read_int table ~column:(`Index 0) in
  let floats = Column.read_float table ~column:(`Index 1) in
  let strings = Column.read_utf8 table ~column:(`Index 2) in

  Alcotest.(check (array int)) "Read integers by index" [| 1; 2; 3; 4; 5 |] integers;
  Alcotest.(check (array (float 1e-6))) "Read floats by index" [| 1.1; 2.2; 3.3; 4.4; 5.5 |] floats;
  Alcotest.(check (array string)) "Read strings by index" [| "a"; "b"; "c"; "d"; "e" |] strings

let test_optional_column_reading () =
  let table = create_test_table () in

  (* Test reading optional columns *)
  let opt_integers = Column.read_int_opt table ~column:(`Name "opt_integers") in
  let opt_strings = Column.read_utf8_opt table ~column:(`Name "opt_strings") in
  let opt_floats = Column.read_float_opt table ~column:(`Name "opt_floats") in

  Alcotest.(check (array (option int))) "Read optional integers"
    [| Some 10; None; Some 30; None; Some 50 |] opt_integers;
  Alcotest.(check (array (option string))) "Read optional strings"
    [| Some "x"; Some "y"; None; Some "z"; None |] opt_strings;
  Alcotest.(check (array (option (float 1e-6)))) "Read optional floats"
    [| Some 1.5; None; Some 3.5; Some 4.5; None |] opt_floats

let test_bigarray_column_reading () =
  let table = create_test_table () in

  (* Test reading columns as bigarrays *)
  let int_ba = Column.read_i64_ba table ~column:(`Name "integers") in
  let float_ba = Column.read_f64_ba table ~column:(`Name "floats") in

  Alcotest.(check int) "Int bigarray length" 5 (Bigarray.Array1.dim int_ba);
  Alcotest.(check int) "Float bigarray length" 5 (Bigarray.Array1.dim float_ba);

  (* Check some values *)
  Alcotest.(check int64) "Int bigarray first value" 1L (Bigarray.Array1.get int_ba 0);
  Alcotest.(check (float 1e-6)) "Float bigarray first value" 1.1 (Bigarray.Array1.get float_ba 0)

let test_bigarray_optional_reading () =
  let table = create_test_table () in

  (* Test reading optional columns as bigarrays *)
  let opt_int_ba, opt_int_valid = Column.read_i64_ba_opt table ~column:(`Name "opt_integers") in
  let opt_float_ba, opt_float_valid = Column.read_f64_ba_opt table ~column:(`Name "opt_floats") in

  Alcotest.(check int) "Optional int bigarray length" 5 (Bigarray.Array1.dim opt_int_ba);
  Alcotest.(check int) "Optional float bigarray length" 5 (Bigarray.Array1.dim opt_float_ba);

  Alcotest.(check int) "Optional int valid length" 5 (Valid.length opt_int_valid);
  Alcotest.(check int) "Optional float valid length" 5 (Valid.length opt_float_valid);

  (* Check validity bitset *)
  Alcotest.(check bool) "First optional int is valid" true (Valid.get opt_int_valid 0);
  Alcotest.(check bool) "Second optional int is invalid" false (Valid.get opt_int_valid 1);
  Alcotest.(check bool) "Third optional int is valid" true (Valid.get opt_int_valid 2)

let test_time_column_reading () =
  (* Create a table with time types *)
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

  (* Test reading time columns *)
  let read_dates = Column.read_date table ~column:(`Name "dates") in
  let read_times = Column.read_time_ns table ~column:(`Name "times") in
  let read_spans = Column.read_span_ns table ~column:(`Name "spans") in
  let read_ofdays = Column.read_ofday_ns table ~column:(`Name "ofdays") in

  Alcotest.(check int) "Dates array length" 3 (Array.length read_dates);
  Alcotest.(check int) "Times array length" 3 (Array.length read_times);
  Alcotest.(check int) "Spans array length" 3 (Array.length read_spans);
  Alcotest.(check int) "Ofdays array length" 3 (Array.length read_ofdays)

let test_time_optional_column_reading () =
  (* Create a table with optional time types *)
  let opt_dates = [|
    Some (Time.Date.of_unix_days 18000);
    None;
    Some (Time.Date.of_unix_days 18002);
  |] in

  let opt_times = [|
    Some (Time.Time_ns.of_int64_ns_since_epoch 1234567890000000000L);
    Some (Time.Time_ns.of_int64_ns_since_epoch 1234567891000000000L);
    None;
  |] in

  let table = Table.create [
    Table.col_opt opt_dates Table.Date "opt_dates";
    Table.col_opt opt_times Table.Time_ns "opt_times";
  ] in

  (* Test reading optional time columns *)
  let read_opt_dates = Column.read_date_opt table ~column:(`Name "opt_dates") in
  let read_opt_times = Column.read_time_ns_opt table ~column:(`Name "opt_times") in

  Alcotest.(check int) "Optional dates array length" 3 (Array.length read_opt_dates);
  Alcotest.(check int) "Optional times array length" 3 (Array.length read_opt_times);

  (* Check that None values are preserved *)
  match read_opt_dates.(1) with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for second date"

let test_bitset_reading () =
  (* Create a table with boolean columns for bitset testing *)
  let bool_col1 = [| true; false; true; false; true |] in
  let bool_col2 = [| true; true; false; false; true |] in

  let table = Table.create [
    Table.col bool_col1 Table.Bool "bool_col1";
    Table.col bool_col2 Table.Bool "bool_col2";
  ] in

  (* Test reading validity bitsets *)
  let bitset1 = Column.read_bitset table ~column:(`Name "bool_col1") in
  let bitset2 = Column.read_bitset table ~column:(`Name "bool_col2") in

  Alcotest.(check int) "Bitset 1 length" 5 (Valid.length bitset1);
  Alcotest.(check int) "Bitset 2 length" 5 (Valid.length bitset2);

  (* Check validity patterns *)
  Alcotest.(check bool) "bitset1[0] valid" true (Valid.get bitset1 0);
  Alcotest.(check bool) "bitset1[1] invalid" false (Valid.get bitset1 1);
  Alcotest.(check bool) "bitset1[2] valid" true (Valid.get bitset1 2);

  Alcotest.(check bool) "bitset2[0] valid" true (Valid.get bitset2 0);
  Alcotest.(check bool) "bitset2[2] invalid" false (Valid.get bitset2 2)

let test_bitset_with_optional_reading () =
  (* Create a table with boolean column for bitset testing *)
  let bool_data = [| true; false; true; true; false |] in

  let table = Table.create [
    Table.col bool_data Table.Bool "bool_col";
  ] in

  (* Test reading bitsets with optional data *)
  let bitset_data, bitset_valid = Column.read_bitset_opt table ~column:(`Name "bool_col") in

  Alcotest.(check int) "Bitset data length" 5 (Valid.length bitset_data);
  Alcotest.(check int) "Bitset valid length" 5 (Valid.length bitset_valid)

let () =
  let open Alcotest in
  run "Column tests" [
    "basic_reading", [
      test_case "Read columns by name" `Quick test_column_reading_by_name;
      test_case "Read columns by index" `Quick test_column_reading_by_index;
      (* test_case "Read int32 columns" `Quick test_int32_column_reading; *)
    ];
    "optional_reading", [
      test_case "Read optional columns" `Quick test_optional_column_reading;
    ];
    "bigarray_reading", [
      test_case "Read as bigarrays" `Quick test_bigarray_column_reading;
      test_case "Read optional as bigarrays" `Quick test_bigarray_optional_reading;
    ];
    "time_reading", [
      test_case "Read time columns" `Quick test_time_column_reading;
      test_case "Read optional time columns" `Quick test_time_optional_column_reading;
    ];
    "validity_reading", [
      test_case "Read validity bitsets" `Quick test_bitset_reading;
      test_case "Read bitsets with optional" `Quick test_bitset_with_optional_reading;
    ];
  ]
