(* Valid module tests using external Arrow interface *)
open Arrow

let bitset_to_string bitset =
  let chars = ref [] in
  for i = Valid.length bitset - 1 downto 0 do
    chars := (if Valid.get bitset i then '1' else '0') :: !chars
  done;
  String.concat "" (List.map (String.make 1) !chars)

let test_valid_creation () =
  (* Test creation of validity bitsets *)
  let len = 10 in
  let valid_all = Valid.create_all_valid len in
  let valid_none = Valid.create len in

  Alcotest.(check int) "All valid length" len (Valid.length valid_all);
  Alcotest.(check int) "None valid length" len (Valid.length valid_none);

  (* Check all valid bitset *)
  for i = 0 to len - 1 do
    Alcotest.(check bool) (Printf.sprintf "All valid bit %d" i) true (Valid.get valid_all i)
  done;

  (* Check none valid bitset *)
  for i = 0 to len - 1 do
    Alcotest.(check bool) (Printf.sprintf "None valid bit %d" i) false (Valid.get valid_none i)
  done;

  Alcotest.(check string) "All valid string" "1111111111" (bitset_to_string valid_all);
  Alcotest.(check string) "None valid string" "0000000000" (bitset_to_string valid_none)

let test_valid_operations () =
  (* Test setting and getting validity bits *)
  let len = 8 in
  let valid = Valid.create len in

  (* Set some bits to valid *)
  Valid.set_valid valid 1;
  Valid.set_valid valid 3;
  Valid.set_valid valid 5;
  Valid.set_invalid valid 7; (* Should already be invalid, but test the function *)

  let expected = "01010100" in
  Alcotest.(check string) "Pattern setting" expected (bitset_to_string valid);

  (* Test num_true and num_false *)
  Alcotest.(check int) "Num true" 3 (Valid.num_true valid);
  Alcotest.(check int) "Num false" 5 (Valid.num_false valid)

let test_valid_from_array () =
  (* Test creating validity bitset from boolean array *)
  let bool_array = [| true; false; true; true; false |] in
  let valid = Valid.from_array bool_array in

  Alcotest.(check int) "From array length" 5 (Valid.length valid);
  Alcotest.(check string) "From array pattern" "10110" (bitset_to_string valid);

  (* Check individual bits *)
  Alcotest.(check bool) "Bit 0 true" true (Valid.get valid 0);
  Alcotest.(check bool) "Bit 1 false" false (Valid.get valid 1);
  Alcotest.(check bool) "Bit 2 true" true (Valid.get valid 2);
  Alcotest.(check bool) "Bit 3 true" true (Valid.get valid 3);
  Alcotest.(check bool) "Bit 4 false" false (Valid.get valid 4);

  Alcotest.(check int) "From array num true" 3 (Valid.num_true valid);
  Alcotest.(check int) "From array num false" 2 (Valid.num_false valid)

let test_valid_edge_cases () =
  (* Test edge cases *)

  (* Empty bitset *)
  let empty = Valid.create 0 in
  Alcotest.(check int) "Empty bitset length" 0 (Valid.length empty);
  Alcotest.(check int) "Empty bitset num_true" 0 (Valid.num_true empty);
  Alcotest.(check int) "Empty bitset num_false" 0 (Valid.num_false empty);
  Alcotest.(check string) "Empty bitset string" "" (bitset_to_string empty);

  (* Single bit *)
  let single_true = Valid.create_all_valid 1 in
  let single_false = Valid.create 1 in

  Alcotest.(check string) "Single true" "1" (bitset_to_string single_true);
  Alcotest.(check string) "Single false" "0" (bitset_to_string single_false);

  (* Large bitset *)
  let large = Valid.create_all_valid 64 in
  Alcotest.(check int) "Large bitset length" 64 (Valid.length large);
  Alcotest.(check int) "Large bitset all true" 64 (Valid.num_true large);
  Alcotest.(check int) "Large bitset no false" 0 (Valid.num_false large)

let test_valid_modifications () =
  (* Test various modification patterns *)
  let len = 12 in
  let valid = Valid.create len in

  (* Set alternating pattern *)
  for i = 0 to len - 1 do
    if i mod 2 = 0 then Valid.set_valid valid i
  done;

  Alcotest.(check string) "Alternating pattern" "101010101010" (bitset_to_string valid);
  Alcotest.(check int) "Alternating num_true" 6 (Valid.num_true valid);
  Alcotest.(check int) "Alternating num_false" 6 (Valid.num_false valid);

  (* Flip some bits *)
  Valid.set_invalid valid 0;
  Valid.set_invalid valid 4;
  Valid.set_valid valid 1;
  Valid.set_valid valid 3;

  Alcotest.(check string) "Modified pattern" "011100101010" (bitset_to_string valid);
  Alcotest.(check int) "Modified num_true" 6 (Valid.num_true valid);
  Alcotest.(check int) "Modified num_false" 6 (Valid.num_false valid)

let test_valid_from_mixed_array () =
  (* Test with mixed true/false patterns *)
  let patterns = [
    ([| true; true; true |], "111", 3, 0);
    ([| false; false; false |], "000", 0, 3);
    ([| true; false; true; false |], "1010", 2, 2);
    ([| false; true; false; true; false |], "01010", 2, 3);
  ] in

  List.iteri (fun test_idx (bool_array, expected_str, expected_true, expected_false) ->
    let valid = Valid.from_array bool_array in
    Alcotest.(check string)
      (Printf.sprintf "Pattern %d string" test_idx)
      expected_str
      (bitset_to_string valid);
    Alcotest.(check int)
      (Printf.sprintf "Pattern %d num_true" test_idx)
      expected_true
      (Valid.num_true valid);
    Alcotest.(check int)
      (Printf.sprintf "Pattern %d num_false" test_idx)
      expected_false
      (Valid.num_false valid)
  ) patterns

let test_valid_boundary_access () =
  (* Test boundary conditions for get/set operations *)
  let len = 10 in
  let valid = Valid.create len in

  (* Test first and last positions *)
  Valid.set_valid valid 0;
  Valid.set_valid valid (len - 1);

  Alcotest.(check bool) "First bit set" true (Valid.get valid 0);
  Alcotest.(check bool) "Last bit set" true (Valid.get valid (len - 1));
  Alcotest.(check bool) "Middle bit not set" false (Valid.get valid (len / 2));

  Alcotest.(check string) "Boundary pattern" "1000000001" (bitset_to_string valid);
  Alcotest.(check int) "Boundary num_true" 2 (Valid.num_true valid);
  Alcotest.(check int) "Boundary num_false" 8 (Valid.num_false valid)

let test_valid_integration_with_table () =
  (* Test validity bitsets in the context of table operations *)
  (* Create boolean columns that represent validity patterns *)
  let int_valid_pattern = [| true; false; true; false; true |] in
  let string_valid_pattern = [| true; true; false; false; true |] in

  let table = Table.create [
    Table.col int_valid_pattern Table.Bool "int_validity";
    Table.col string_valid_pattern Table.Bool "string_validity";
  ] in

  (* Read validity bitsets *)
  let int_valid = Column.read_bitset table ~column:(`Name "int_validity") in
  let string_valid = Column.read_bitset table ~column:(`Name "string_validity") in

  Alcotest.(check int) "Int validity length" 5 (Valid.length int_valid);
  Alcotest.(check int) "String validity length" 5 (Valid.length string_valid);

  Alcotest.(check string) "Int validity pattern" "10101" (bitset_to_string int_valid);
  Alcotest.(check string) "String validity pattern" "11001" (bitset_to_string string_valid);

  Alcotest.(check int) "Int valid count" 3 (Valid.num_true int_valid);
  Alcotest.(check int) "String valid count" 3 (Valid.num_true string_valid);

  Alcotest.(check int) "Int invalid count" 2 (Valid.num_false int_valid);
  Alcotest.(check int) "String invalid count" 2 (Valid.num_false string_valid)

let test_valid_all_scenarios () =
  (* Test comprehensive validity scenarios *)

  (* All valid scenario *)
  let all_valid_table = Table.create [
    Table.col [| true; true; true |] Table.Bool "all_valid";
  ] in
  let all_valid_bitset = Column.read_bitset all_valid_table ~column:(`Name "all_valid") in
  Alcotest.(check string) "All valid scenario" "111" (bitset_to_string all_valid_bitset);
  Alcotest.(check int) "All valid count" 3 (Valid.num_true all_valid_bitset);

  (* All invalid scenario *)
  let all_invalid_table = Table.create [
    Table.col [| false; false; false |] Table.Bool "all_invalid";
  ] in
  let all_invalid_bitset = Column.read_bitset all_invalid_table ~column:(`Name "all_invalid") in
  Alcotest.(check string) "All invalid scenario" "000" (bitset_to_string all_invalid_bitset);
  Alcotest.(check int) "All invalid count" 0 (Valid.num_true all_invalid_bitset);

  (* Mixed scenario *)
  let mixed_table = Table.create [
    Table.col [| true; false; true; false; false; true |] Table.Bool "mixed";
  ] in
  let mixed_bitset = Column.read_bitset mixed_table ~column:(`Name "mixed") in
  Alcotest.(check string) "Mixed scenario" "101001" (bitset_to_string mixed_bitset);
  Alcotest.(check int) "Mixed valid count" 3 (Valid.num_true mixed_bitset);
  Alcotest.(check int) "Mixed invalid count" 3 (Valid.num_false mixed_bitset)

let () =
  let open Alcotest in
  run "Valid tests" [
    "creation", [
      test_case "Validity creation" `Quick test_valid_creation;
      test_case "From boolean array" `Quick test_valid_from_array;
      test_case "From mixed arrays" `Quick test_valid_from_mixed_array;
    ];
    "operations", [
      test_case "Basic operations" `Quick test_valid_operations;
      test_case "Modifications" `Quick test_valid_modifications;
      test_case "Boundary access" `Quick test_valid_boundary_access;
    ];
    "edge_cases", [
      test_case "Edge cases" `Quick test_valid_edge_cases;
    ];
    "integration", [
      test_case "Integration with tables" `Quick test_valid_integration_with_table;
      test_case "All validity scenarios" `Quick test_valid_all_scenarios;
    ];
  ]