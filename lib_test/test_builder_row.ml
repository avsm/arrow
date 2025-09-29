(** Comprehensive tests for Builder.Row interface *)
open Arrow
open Test_fixtures

(** {2 Test Data Types} *)

type simple_person = {
  name: string;
  age: int;
  active: bool;
}

type employee = {
  id: int;
  name: string;
  department: string;
  salary: float;
  active: bool;
  hire_date: int32; (* days since epoch *)
  score: float option;
}

type product = {
  id: int;
  name: string;
  price: float option;
  in_stock: bool;
  category: string option;
  created_at: int64; (* timestamp in nanoseconds *)
}

type all_types_record = {
  int_val: int;
  float_val: float;
  bool_val: bool;
  string_val: string;
  date_val: Time.Date.t;
  time_val: Time.Time_ns.t;
  span_val: Time.Time_ns.Span.t;
  ofday_val: Time.Time_ns.Ofday.t;
  opt_int: int option;
  opt_float: float option;
  opt_string: string option;
  opt_bool: bool option;
}

(** {2 Basic Row Builder Tests} *)

let test_simple_row_builder () =
  let people = [|
    { name = "Alice"; age = 30; active = true };
    { name = "Bob"; age = 25; active = false };
    { name = "Charlie"; age = 35; active = true };
  |] in

  let cols =
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"age" Table.Int (fun (p : simple_person) -> p.age) @
    Builder.Row.col ~name:"active" Table.Bool (fun (p : simple_person) -> p.active) in

  let table = Builder.Row.array_to_table cols people in

  Alcotest.(check int) "Simple row builder rows" 3 (Table.num_rows table);

  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in
  let actives = Table.read table Table.Bool ~column:(`Name "active") in

  Alcotest.(check (array string)) "Simple row names" [| "Alice"; "Bob"; "Charlie" |] names;
  Alcotest.(check (array int)) "Simple row ages" [| 30; 25; 35 |] ages;
  Alcotest.(check (array bool)) "Simple row actives" [| true; false; true |] actives

let test_employee_row_builder () =
  let employees = [|
    { id = 1; name = "Alice Smith"; department = "Engineering"; salary = 75000.0;
      active = true; hire_date = 18000l; score = Some 95.5 };
    { id = 2; name = "Bob Johnson"; department = "Marketing"; salary = 65000.0;
      active = true; hire_date = 18100l; score = None };
    { id = 3; name = "Charlie Brown"; department = "Engineering"; salary = 80000.0;
      active = false; hire_date = 17900l; score = Some 88.0 };
  |] in

  let cols =
    Builder.Row.col ~name:"id" Table.Int (fun (e : employee) -> e.id) @
    Builder.Row.col ~name:"name" Table.Utf8 (fun (e : employee) -> e.name) @
    Builder.Row.col ~name:"department" Table.Utf8 (fun (e : employee) -> e.department) @
    Builder.Row.col ~name:"salary" Table.Float (fun (e : employee) -> e.salary) @
    Builder.Row.col ~name:"active" Table.Bool (fun (e : employee) -> e.active) @
    Builder.Row.col ~name:"hire_date" Table.Date (fun (e : employee) -> Time.Date.of_unix_days (Int32.to_int e.hire_date)) @
    Builder.Row.col_opt ~name:"score" Table.Float (fun (e : employee) -> e.score) in

  let table = Builder.Row.array_to_table cols employees in

  Alcotest.(check int) "Employee table rows" 3 (Table.num_rows table);

  let ids = Table.read table Table.Int ~column:(`Name "id") in
  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let departments = Table.read table Table.Utf8 ~column:(`Name "department") in
  let salaries = Table.read table Table.Float ~column:(`Name "salary") in
  let actives = Table.read table Table.Bool ~column:(`Name "active") in
  let scores = Table.read_opt table Table.Float ~column:(`Name "score") in

  Alcotest.(check (array int)) "Employee IDs" [| 1; 2; 3 |] ids;
  Alcotest.(check (array string)) "Employee names"
    [| "Alice Smith"; "Bob Johnson"; "Charlie Brown" |] names;
  Alcotest.(check (array string)) "Employee departments"
    [| "Engineering"; "Marketing"; "Engineering" |] departments;
  Alcotest.(check (array (float 1e-6))) "Employee salaries"
    [| 75000.0; 65000.0; 80000.0 |] salaries;
  Alcotest.(check (array bool)) "Employee actives" [| true; true; false |] actives;
  Alcotest.(check (array (option (float 1e-6)))) "Employee scores"
    [| Some 95.5; None; Some 88.0 |] scores

let test_product_row_builder () =
  let products = [|
    { id = 1; name = "Laptop"; price = Some 1299.99; in_stock = true;
      category = Some "Electronics"; created_at = 1609459200000000000L };
    { id = 2; name = "Book"; price = Some 19.99; in_stock = true;
      category = Some "Books"; created_at = 1609545600000000000L };
    { id = 3; name = "Unknown Item"; price = None; in_stock = false;
      category = None; created_at = 1609632000000000000L };
  |] in

  let cols =
    Builder.Row.col ~name:"id" Table.Int (fun (p : product) -> p.id) @
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : product) -> p.name) @
    Builder.Row.col_opt ~name:"price" Table.Float (fun (p : product) -> p.price) @
    Builder.Row.col ~name:"in_stock" Table.Bool (fun (p : product) -> p.in_stock) @
    Builder.Row.col_opt ~name:"category" Table.Utf8 (fun (p : product) -> p.category) @
    Builder.Row.col ~name:"created_at" Table.Time_ns
      (fun (p : product) -> Time.Time_ns.of_int64_ns_since_epoch p.created_at) in

  let table = Builder.Row.array_to_table cols products in

  Alcotest.(check int) "Product table rows" 3 (Table.num_rows table);

  let ids = Table.read table Table.Int ~column:(`Name "id") in
  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let prices = Table.read_opt table Table.Float ~column:(`Name "price") in
  let in_stocks = Table.read table Table.Bool ~column:(`Name "in_stock") in
  let categories = Table.read_opt table Table.Utf8 ~column:(`Name "category") in

  Alcotest.(check (array int)) "Product IDs" [| 1; 2; 3 |] ids;
  Alcotest.(check (array string)) "Product names" [| "Laptop"; "Book"; "Unknown Item" |] names;
  Alcotest.(check (array (option (float 1e-6)))) "Product prices"
    [| Some 1299.99; Some 19.99; None |] prices;
  Alcotest.(check (array bool)) "Product stock" [| true; true; false |] in_stocks;
  Alcotest.(check (array (option string))) "Product categories"
    [| Some "Electronics"; Some "Books"; None |] categories

(** {2 All Types Row Builder Test} *)

let test_all_types_row_builder () =
  let records = [|
    {
      int_val = 127;
      float_val = 3.14;
      bool_val = true;
      string_val = "test_string";
      date_val = Time.Date.of_unix_days 18000;
      time_val = Time.Time_ns.of_int64_ns_since_epoch 1609459200000000000L;
      span_val = Time.Time_ns.Span.of_ns 1000000000L;
      ofday_val = Time.Time_ns.Ofday.of_ns_since_midnight 3600000000000L;
      opt_int = Some 42; opt_float = Some 1.23; opt_string = Some "optional"; opt_bool = Some false;
    };
    {
      int_val = -128;
      float_val = -1.0;
      bool_val = false;
      string_val = "another_test";
      date_val = Time.Date.of_unix_days 0;
      time_val = Time.Time_ns.of_int64_ns_since_epoch 0L;
      span_val = Time.Time_ns.Span.of_ns 0L;
      ofday_val = Time.Time_ns.Ofday.of_ns_since_midnight 0L;
      opt_int = None; opt_float = None; opt_string = None; opt_bool = None;
    };
  |] in

  let cols =
    Builder.Row.col ~name:"int" Table.Int (fun (r : all_types_record) -> r.int_val) @
    Builder.Row.col ~name:"float" Table.Float (fun (r : all_types_record) -> r.float_val) @
    Builder.Row.col ~name:"bool" Table.Bool (fun (r : all_types_record) -> r.bool_val) @
    Builder.Row.col ~name:"string" Table.Utf8 (fun (r : all_types_record) -> r.string_val) @
    Builder.Row.col ~name:"date" Table.Date (fun (r : all_types_record) -> r.date_val) @
    Builder.Row.col ~name:"time" Table.Time_ns (fun (r : all_types_record) -> r.time_val) @
    Builder.Row.col ~name:"span" Table.Span_ns (fun (r : all_types_record) -> r.span_val) @
    Builder.Row.col ~name:"ofday" Table.Ofday_ns (fun (r : all_types_record) -> r.ofday_val) @
    Builder.Row.col_opt ~name:"opt_int" Table.Int (fun (r : all_types_record) -> r.opt_int) @
    Builder.Row.col_opt ~name:"opt_float" Table.Float (fun (r : all_types_record) -> r.opt_float) @
    Builder.Row.col_opt ~name:"opt_string" Table.Utf8 (fun (r : all_types_record) -> r.opt_string) @
    Builder.Row.col_opt ~name:"opt_bool" Table.Bool (fun (r : all_types_record) -> r.opt_bool) in

  let table = Builder.Row.array_to_table cols records in

  Alcotest.(check int) "All types table rows" 2 (Table.num_rows table);

  (* Verify some key columns *)
  let ints = Table.read table Table.Int ~column:(`Name "int") in
  let strings = Table.read table Table.Utf8 ~column:(`Name "string") in
  let opt_ints = Table.read_opt table Table.Int ~column:(`Name "opt_int") in
  let opt_strings = Table.read_opt table Table.Utf8 ~column:(`Name "opt_string") in

  Alcotest.(check (array int)) "All types ints" [| 127; -128 |] ints;
  Alcotest.(check (array string)) "All types strings" [| "test_string"; "another_test" |] strings;
  Alcotest.(check (array (option int))) "All types opt_ints" [| Some 42; None |] opt_ints;
  Alcotest.(check (array (option string))) "All types opt_strings" [| Some "optional"; None |] opt_strings

(** {2 Edge Case Tests} *)

let test_empty_row_array () =
  let empty_people : simple_person array = [||] in
  let cols =
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"age" Table.Int (fun (p : simple_person) -> p.age) in

  let table = Builder.Row.array_to_table cols empty_people in

  Alcotest.(check int) "Empty row array table rows" 0 (Table.num_rows table);

  (* Verify we can still read from empty table *)
  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in

  Alcotest.(check (array string)) "Empty array names" [||] names;
  Alcotest.(check (array int)) "Empty array ages" [||] ages

let test_single_row () =
  let single_person = [| { name = "Solo"; age = 42; active = true } |] in
  let cols =
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"age" Table.Int (fun (p : simple_person) -> p.age) @
    Builder.Row.col ~name:"active" Table.Bool (fun (p : simple_person) -> p.active) in

  let table = Builder.Row.array_to_table cols single_person in

  Alcotest.(check int) "Single row table rows" 1 (Table.num_rows table);

  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in
  let actives = Table.read table Table.Bool ~column:(`Name "active") in

  Alcotest.(check (array string)) "Single row names" [| "Solo" |] names;
  Alcotest.(check (array int)) "Single row ages" [| 42 |] ages;
  Alcotest.(check (array bool)) "Single row actives" [| true |] actives

let test_many_optional_values () =
  let data_with_many_nulls = Array.init 20 (fun i ->
    {
      id = i;
      name = Printf.sprintf "person_%d" i;
      department = if i mod 3 = 0 then "Engineering" else if i mod 3 = 1 then "Sales" else "Marketing";
      salary = if i mod 5 = 0 then 0.0 else float_of_int (50000 + i * 1000);
      active = i mod 2 = 0;
      hire_date = Int32.of_int (18000 + i);
      score = if i mod 4 = 0 then None else Some (float_of_int (80 + i mod 20));
    }
  ) in

  let cols =
    Builder.Row.col ~name:"id" Table.Int (fun (e : employee) -> e.id) @
    Builder.Row.col ~name:"name" Table.Utf8 (fun (e : employee) -> e.name) @
    Builder.Row.col ~name:"department" Table.Utf8 (fun (e : employee) -> e.department) @
    Builder.Row.col ~name:"salary" Table.Float (fun (e : employee) -> e.salary) @
    Builder.Row.col ~name:"active" Table.Bool (fun (e : employee) -> e.active) @
    Builder.Row.col_opt ~name:"score" Table.Float (fun (e : employee) -> e.score) in

  let table = Builder.Row.array_to_table cols data_with_many_nulls in

  Alcotest.(check int) "Many optionals table rows" 20 (Table.num_rows table);

  let scores = Table.read_opt table Table.Float ~column:(`Name "score") in
  let null_count = Array.fold_left (fun acc opt -> if Option.is_none opt then acc + 1 else acc) 0 scores in
  let expected_nulls = 20 / 4 in (* Every 4th element is None *)

  Alcotest.(check int) "Many optionals null count" expected_nulls null_count

let test_large_row_dataset () =
  (* Test with larger dataset *)
  let size = 1000 in
  let large_dataset = Array.init size (fun i ->
    { name = Printf.sprintf "person_%04d" i; age = 20 + (i mod 50); active = i mod 2 = 0 }
  ) in

  let cols =
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"age" Table.Int (fun (p : simple_person) -> p.age) @
    Builder.Row.col ~name:"active" Table.Bool (fun (p : simple_person) -> p.active) in

  let table = Builder.Row.array_to_table cols large_dataset in

  Alcotest.(check int) "Large dataset rows" size (Table.num_rows table);

  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in

  Alcotest.(check int) "Large dataset names length" size (Array.length names);
  Alcotest.(check int) "Large dataset ages length" size (Array.length ages);

  (* Verify first and last entries *)
  Alcotest.(check string) "Large dataset first name" "person_0000" names.(0);
  Alcotest.(check string) "Large dataset last name" "person_0999" names.(size - 1);
  Alcotest.(check int) "Large dataset first age" 20 ages.(0);
  Alcotest.(check int) "Large dataset last age" (20 + (size - 1) mod 50) ages.(size - 1)

(** {2 String Edge Cases} *)

let test_string_edge_cases () =
  let string_test_data = [|
    { name = ""; age = 1; active = true };  (* empty string *)
    { name = "normal"; age = 2; active = false };
    { name = "🚀🌟🎉"; age = 3; active = true };  (* unicode *)
    { name = String.make 1000 'x'; age = 4; active = false };  (* very long string *)
    { name = "line1\nline2\ttab"; age = 5; active = true };  (* special characters *)
  |] in

  let cols =
    Builder.Row.col ~name:"name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"age" Table.Int (fun (p : simple_person) -> p.age) @
    Builder.Row.col ~name:"active" Table.Bool (fun (p : simple_person) -> p.active) in

  let table = Builder.Row.array_to_table cols string_test_data in

  Alcotest.(check int) "String edge cases rows" 5 (Table.num_rows table);

  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let ages = Table.read table Table.Int ~column:(`Name "age") in

  Alcotest.(check string) "Empty string" "" names.(0);
  Alcotest.(check string) "Normal string" "normal" names.(1);
  Alcotest.(check string) "Unicode string" "🚀🌟🎉" names.(2);
  Alcotest.(check string) "Long string" (String.make 1000 'x') names.(3);
  Alcotest.(check string) "Special chars string" "line1\nline2\ttab" names.(4);

  Alcotest.(check (array int)) "String edge cases ages" [| 1; 2; 3; 4; 5 |] ages

(** {2 Column Name and Schema Tests} *)

let test_column_names () =
  let people = [| { name = "Alice"; age = 30; active = true } |] in
  let cols =
    Builder.Row.col ~name:"person_name" Table.Utf8 (fun (p : simple_person) -> p.name) @
    Builder.Row.col ~name:"person_age" Table.Int (fun (p : simple_person) -> p.age) @
    Builder.Row.col ~name:"is_active" Table.Bool (fun (p : simple_person) -> p.active) in

  let table = Builder.Row.array_to_table cols people in
  let column_names = get_column_names table in

  Alcotest.(check (list string)) "Custom column names"
    ["person_name"; "person_age"; "is_active"] column_names;

  (* Test reading with custom names *)
  let names = Table.read table Table.Utf8 ~column:(`Name "person_name") in
  let ages = Table.read table Table.Int ~column:(`Name "person_age") in
  let actives = Table.read table Table.Bool ~column:(`Name "is_active") in

  Alcotest.(check (array string)) "Custom names data" [| "Alice" |] names;
  Alcotest.(check (array int)) "Custom ages data" [| 30 |] ages;
  Alcotest.(check (array bool)) "Custom actives data" [| true |] actives

(** {2 Integration with Test Fixtures} *)

let test_with_test_fixtures () =
  (* Use test_fixtures to create data *)
  let test_records = sample_test_records 10 in
  let table = test_records_to_table test_records in

  Alcotest.(check int) "Test fixtures table rows" 10 (Table.num_rows table);

  let ids = Table.read table Table.Int ~column:(`Name "id") in
  let names = Table.read table Table.Utf8 ~column:(`Name "name") in
  let values = Table.read table Table.Float ~column:(`Name "value") in
  let actives = Table.read table Table.Bool ~column:(`Name "active") in
  let scores = Table.read_opt table Table.Float ~column:(`Name "score") in

  (* Verify first record *)
  Alcotest.(check int) "Fixtures first ID" 1 ids.(0);
  Alcotest.(check string) "Fixtures first name" "record_1" names.(0);
  Alcotest.(check (float 1e-6)) "Fixtures first value" 0.0 values.(0);
  Alcotest.(check bool) "Fixtures first active" true actives.(0);
  Alcotest.(check (option (float 1e-6))) "Fixtures first score" None scores.(0);

  (* Verify last record *)
  Alcotest.(check int) "Fixtures last ID" 10 ids.(9);
  Alcotest.(check string) "Fixtures last name" "record_10" names.(9);
  Alcotest.(check (float 1e-6)) "Fixtures last value" 13.5 values.(9);
  Alcotest.(check bool) "Fixtures last active" false actives.(9)

(** {2 Test Suite} *)

let () =
  let open Alcotest in
  run "Builder.Row tests" [
    "basic_functionality", [
      test_case "Simple row builder" `Quick test_simple_row_builder;
      test_case "Employee row builder" `Quick test_employee_row_builder;
      test_case "Product row builder" `Quick test_product_row_builder;
    ];
    "comprehensive_types", [
      test_case "All types row builder" `Quick test_all_types_row_builder;
    ];
    "edge_cases", [
      test_case "Empty row array" `Quick test_empty_row_array;
      test_case "Single row" `Quick test_single_row;
      test_case "Many optional values" `Quick test_many_optional_values;
      test_case "Large row dataset" `Quick test_large_row_dataset;
      test_case "String edge cases" `Quick test_string_edge_cases;
    ];
    "metadata", [
      test_case "Column names" `Quick test_column_names;
    ];
    "integration", [
      test_case "With test fixtures" `Quick test_with_test_fixtures;
    ];
  ]