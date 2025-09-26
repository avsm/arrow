val schema : string -> Wrapper.Schema.t
val table : ?columns:[ `Indexes of int list | `Names of string list ] -> string -> Table.t