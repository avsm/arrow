open Cmdliner

(** Common options *)
let file_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"File to process")

let verbose_flag =
  Arg.(value & flag & info ["v"; "verbose"] ~doc:"Enable verbose output")

(** Parquet schema command *)
let parquet_schema_cmd =
  let doc = "Display the schema of a Parquet file" in
  let man = [
    `S Manpage.s_description;
    `P "Display the schema structure of a Parquet file, including:";
    `P "- Field names and types";
    `P "- Nested structure";
    `P "- Physical and logical types";
    `P "- Repetition levels";
  ] in

  let schema_run file verbose =
    try
      let reader = Arrow.Parquet.Reader.open_file file in
      let schema = Arrow.Parquet.Reader.schema reader in

      if verbose then
        Printf.printf "File: %s\n\n" file;

      Format.printf "Schema:\n%a@." Arrow.Parquet.pp_schema_node schema;

      Arrow.Parquet.Reader.close reader;
      `Ok ()
    with
    | e ->
      `Error (false, Printf.sprintf "Failed to read schema: %s" (Printexc.to_string e))
  in

  let term = Term.(ret (const schema_run $ file_arg $ verbose_flag)) in
  let info = Cmd.info "schema" ~doc ~man in
  Cmd.v info term


(** Parquet info command - unified info and metadata *)
let parquet_info_cmd =
  let doc = "Display information and metadata about a Parquet file" in
  let man = [
    `S Manpage.s_description;
    `P "Display comprehensive information about a Parquet file including:";
    `P "- File size and version";
    `P "- Row and column counts";
    `P "- Compression types and ratios";
    `P "- Creator information";
    `P "- Row group details (with --verbose)";
    `P "- Column metadata and statistics (with --verbose)";
    `P "- Key-value metadata (with --verbose)";
  ] in

  let info_run file verbose =
    try
      (* Get file size *)
      let stats = Unix.stat file in
      let size_mb = float_of_int stats.st_size /. (1024. *. 1024.) in

      let reader = Arrow.Parquet.Reader.open_file file in
      let meta = Arrow.Parquet.Reader.metadata reader in
      let schema = Arrow.Parquet.Reader.schema reader in

      (* Count leaf columns *)
      let rec count_columns node =
        match node.Arrow.Parquet.physical_type with
        | Some _ -> 1
        | None -> List.fold_left (fun acc child -> acc + count_columns child) 0 node.children
      in
      let num_columns = count_columns schema in

      Printf.printf "File: %s\n" file;
      Printf.printf "Size: %.2f MB\n" size_mb;
      Printf.printf "Parquet version: %d\n" meta.version;
      Printf.printf "Total rows: %Ld\n" meta.num_rows;
      Printf.printf "Columns: %d\n" num_columns;
      Printf.printf "Row groups: %d\n" (List.length meta.row_groups);

      (match meta.created_by with
      | Some creator -> Printf.printf "Created by: %s\n" creator
      | None -> ());

      (* Show compression types used *)
      let compressions = List.fold_left (fun acc (rg : Arrow.Parquet.row_group_metadata) ->
        List.fold_left (fun acc2 (col : Arrow.Parquet.column_metadata) ->
          if List.mem col.compression acc2 then acc2
          else col.compression :: acc2
        ) acc rg.columns
      ) [] meta.row_groups in

      Printf.printf "Compression types: ";
      List.iter (fun c -> Format.printf "%a " Arrow.Parquet.pp_compression c) compressions;
      Printf.printf "\n";

      (* Calculate compression ratio *)
      let total_uncompressed = List.fold_left (fun acc rg ->
        Int64.add acc (List.fold_left (fun acc2 col ->
          Int64.add acc2 col.Arrow.Parquet.total_uncompressed_size
        ) 0L rg.Arrow.Parquet.columns)
      ) 0L meta.row_groups in

      let total_compressed = List.fold_left (fun acc rg ->
        Int64.add acc (List.fold_left (fun acc2 col ->
          Int64.add acc2 col.Arrow.Parquet.total_compressed_size
        ) 0L rg.Arrow.Parquet.columns)
      ) 0L meta.row_groups in

      if total_uncompressed > 0L then begin
        let ratio = 1.0 -. (Int64.to_float total_compressed /. Int64.to_float total_uncompressed) in
        Printf.printf "Compression ratio: %.1f%%\n" (ratio *. 100.0)
      end;

      if verbose then begin
        Printf.printf "\nRow Group Details:\n";
        List.iteri (fun i (rg : Arrow.Parquet.row_group_metadata) ->
          Printf.printf "  Row Group %d:\n" i;
          Printf.printf "    Rows: %Ld\n" rg.num_rows;
          Printf.printf "    Total bytes: %Ld\n" rg.total_byte_size;
          Printf.printf "    Columns: %d\n" (List.length rg.columns);

          List.iter (fun (col : Arrow.Parquet.column_metadata) ->
            Format.printf "      - %s (%s, %a)\n"
              (String.concat "." col.path)
              col.type_name
              Arrow.Parquet.pp_compression col.compression;

            Printf.printf "        Compressed: %Ld bytes\n" col.total_compressed_size;
            Printf.printf "        Uncompressed: %Ld bytes\n" col.total_uncompressed_size;
            Printf.printf "        Encodings: ";
            List.iter (fun e -> Format.printf "%a " Arrow.Parquet.pp_encoding e) col.encodings;
            Printf.printf "\n"
          ) rg.columns
        ) meta.row_groups;

        if meta.key_value_metadata <> [] then begin
          Printf.printf "\nKey-Value Metadata:\n";
          List.iter (fun (k, v) ->
            Printf.printf "  %s: %s\n" k v
          ) meta.key_value_metadata
        end
      end;

      Arrow.Parquet.Reader.close reader;
      `Ok ()
    with
    | Unix.Unix_error(err, _, _) ->
      `Error (false, Printf.sprintf "Failed to access file: %s" (Unix.error_message err))
    | e ->
      `Error (false, Printf.sprintf "Failed to read file info: %s" (Printexc.to_string e))
  in

  let term = Term.(ret (const info_run $ file_arg $ verbose_flag)) in
  let info = Cmd.info "info" ~doc ~man in
  Cmd.v info term

(** Read command - read Parquet file using Arrow reader *)
let read_cmd =
  let limit_opt =
    Arg.(value & opt (some int) None & info ["n"; "limit"] ~docv:"N"
         ~doc:"Limit output to first N rows")
  in

  let columns_opt =
    Arg.(value & opt (some string) None & info ["c"; "columns"] ~docv:"COLS"
         ~doc:"Comma-separated list of columns to display")
  in

  let format_opt =
    let doc = Arg.info ["f"; "format"] ~docv:"FORMAT"
              ~doc:"Output format (csv, json, table, debug)" in
    Arg.(value & opt string "table" doc)
  in

  let doc = "Read and display contents of a Parquet file" in
  let man = [
    `S Manpage.s_description;
    `P "Read a Parquet file using the Arrow reader and display its contents.";
    `P "This uses the Arrow-Parquet integration to read the file into Arrow memory format.";
    `P "Multiple output formats are supported for different use cases.";
  ] in

  let read_run file limit columns format verbose =
    try
      if verbose then
        Printf.printf "Reading file: %s\n\n" file;

      (* Determine row limit *)
      let row_limit = match limit with
        | None -> 100  (* Default to 100 rows *)
        | Some n -> n
      in

      (* Read table with limit *)
      let table = Arrow.Parquet.read_table file ~only_first:row_limit in
      let num_rows = Arrow.Table.num_rows table in

      if verbose then
        Printf.printf "Loaded %d rows from file\n\n" num_rows;

      (* Handle column selection if specified *)
      let selected_columns = match columns with
        | None -> None
        | Some cols -> Some (String.split_on_char ',' cols)
      in

      (* Display based on format *)
      begin match format with
      | "debug" ->
          (* Use Arrow debug string *)
          let debug_str = Arrow.Table.to_string_debug table in
          Printf.printf "%s\n" debug_str

      | "csv" ->
          (* CSV output - simplified for now *)
          Printf.printf "# CSV output (simplified):\n";
          Printf.printf "# Note: Full CSV export requires implementing column iteration\n";
          let debug_str = Arrow.Table.to_string_debug table in
          Printf.printf "%s\n" debug_str

      | "json" ->
          (* JSON output - simplified for now *)
          Printf.printf "{\n";
          Printf.printf "  \"rows\": %d,\n" num_rows;
          Printf.printf "  \"format\": \"arrow\",\n";
          Printf.printf "  \"note\": \"Full JSON serialization pending\",\n";

          (* Try to extract some data *)
          begin try
            match selected_columns with
            | Some [col] ->
                let data = Arrow.Column.read_utf8 table ~column:(`Name col) in
                Printf.printf "  \"%s_sample\": [\n" col;
                let sample_size = min 5 (Array.length data) in
                for i = 0 to sample_size - 1 do
                  Printf.printf "    \"%s\"%s\n"
                    (String.escaped data.(i))
                    (if i < sample_size - 1 then "," else "")
                done;
                Printf.printf "  ]\n"
            | _ ->
                Printf.printf "  \"data\": \"Use --columns to specify a single column for JSON sample\"\n"
          with _ ->
            Printf.printf "  \"data\": \"Unable to extract column data\"\n"
          end;
          Printf.printf "}\n"

      | "table" | _ ->
          (* Default table format using debug string *)
          Printf.printf "Table with %d rows:\n\n" num_rows;
          let debug_str = Arrow.Table.to_string_debug table in
          Printf.printf "%s\n" debug_str
      end;

      `Ok ()
    with
    | e ->
      `Error (false, Printf.sprintf "Failed to read file: %s" (Printexc.to_string e))
  in

  let term = Term.(ret (const read_run $ file_arg $ limit_opt $ columns_opt
                         $ format_opt $ verbose_flag)) in
  let info = Cmd.info "read" ~doc ~man in
  Cmd.v info term


(** Convert command - convert between formats *)
let convert_cmd =
  let input_arg =
    Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT"
         ~doc:"Input file to convert")
  in

  let output_arg =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"OUTPUT"
         ~doc:"Output file path")
  in

  let compression_opt =
    let doc = Arg.info ["compression"] ~docv:"TYPE"
              ~doc:"Output compression (none, snappy, gzip, brotli, lz4, zstd)" in
    Arg.(value & opt string "snappy" doc)
  in

  let doc = "Convert between Arrow and Parquet formats" in
  let man = [
    `S Manpage.s_description;
    `P "Convert files between different Arrow/Parquet formats.";
    `P "Can also be used to recompress Parquet files with different compression.";
  ] in

  let convert_run input output compression verbose =
    try
      if verbose then
        Printf.printf "Converting %s to %s\n" input output;

      (* Parse compression *)
      let compression_type = match String.lowercase_ascii compression with
        | "none" | "uncompressed" -> Arrow.Parquet.Uncompressed
        | "snappy" -> Arrow.Parquet.Snappy
        | "gzip" -> Arrow.Parquet.Gzip None
        | "brotli" -> Arrow.Parquet.Brotli None
        | "lz4" -> Arrow.Parquet.Lz4
        | "zstd" -> Arrow.Parquet.Zstd 3
        | _ -> Arrow.Parquet.Snappy
      in

      (* Read input file *)
      let table = Arrow.Parquet.read_table input in
      let num_rows = Arrow.Table.num_rows table in

      if verbose then
        Printf.printf "Read %d rows from input file\n" num_rows;

      (* Write with new compression *)
      Arrow.IO.Parquet.write table output ~compression:compression_type;

      Printf.printf "✓ Successfully converted file\n";
      Printf.printf "  Input: %s\n" input;
      Printf.printf "  Output: %s\n" output;
      Printf.printf "  Rows: %d\n" num_rows;
      Printf.printf "  Compression: %s\n" compression;

      `Ok ()
    with
    | e ->
      `Error (false, Printf.sprintf "Failed to convert file: %s" (Printexc.to_string e))
  in

  let term = Term.(ret (const convert_run $ input_arg $ output_arg
                         $ compression_opt $ verbose_flag)) in
  let info = Cmd.info "convert" ~doc ~man in
  Cmd.v info term

(** Version command *)
let version_cmd =
  let doc = "Display version information" in
  let man = [
    `S Manpage.s_description;
    `P "Display version information for oarrow and the Arrow library.";
  ] in

  let version_run () =
    Printf.printf "oarrow version 1.0.0\n";
    Printf.printf "Arrow library version: %s\n" Arrow.version;
    Printf.printf "OCaml Arrow bindings for Apache Arrow and Parquet\n";
    `Ok ()
  in

  let term = Term.(ret (const version_run $ const ())) in
  let info = Cmd.info "version" ~doc ~man in
  Cmd.v info term

(** Main command *)
let main_cmd =
  let doc = "OCaml Arrow and Parquet utility" in
  let man = [
    `S Manpage.s_description;
    `P "$(b,oarrow) is a command-line utility for working with";
    `P "Apache Arrow and Parquet files in OCaml.";
    `P "";
    `P "Features:";
    `P "- Read and inspect Parquet files";
    `P "- Display file metadata and schemas";
    `P "- Convert between formats with different compression";
    `P "";
    `P "Use $(b,oarrow COMMAND --help) for detailed help on specific commands.";
    `S Manpage.s_commands;
    `S "COMMON OPERATIONS";
    `P "$(b,oarrow info file.parquet) - Display file information and metadata";
    `P "$(b,oarrow schema file.parquet) - Display file schema";
    `P "$(b,oarrow read file.parquet -n 10) - Read first 10 rows";
    `P "$(b,oarrow convert input.parquet output.parquet --compression gzip) - Convert with new compression";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/mtelvers/arrow/issues";
  ] in

  let info = Cmd.info "oarrow" ~version:"1.0.0" ~doc ~man in
  Cmd.group info [
    parquet_schema_cmd;
    parquet_info_cmd;
    read_cmd;
    convert_cmd;
    version_cmd;
  ]

let () =
  exit (Cmd.eval main_cmd)