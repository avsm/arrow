open Configurator.V1

let () =
  main ~name:"arrow-parquet" (fun c ->
    let default_parquet_libs = [] in

    (* Try pkg-config first - prefer parquet which includes arrow *)
    let t = match Pkg_config.get c with
      | None -> { Pkg_config.libs = []; cflags = [] }
      | Some pc ->
        let parquet_conf = Pkg_config.query pc ~package:"parquet" in
        match parquet_conf with
        | Some p ->
          (* Parquet pkg-config already includes Arrow dependencies *)
          {
            libs = p.libs @ ["-lstdc++"];
            cflags = p.cflags
          }
        | None ->
          (* Fall back to trying just Arrow *)
          let arrow_conf = Pkg_config.query pc ~package:"arrow" in
          match arrow_conf with
          | Some a ->
            {
              libs = a.libs @ default_parquet_libs @ ["-lstdc++"];
              cflags = a.cflags
            }
          | None ->
            { Pkg_config.libs = []; cflags = [] }
    in

    Flags.write_sexp "c_flags.sexp" t.cflags;
    Flags.write_sexp "c_library_flags.sexp" t.libs
  )
