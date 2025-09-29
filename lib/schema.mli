(** High-level abstraction over Arrow schema structures *)

(** Schema flags for controlling field behavior *)
module Flags : sig
  (** Abstract type for schema flags *)
  type t

  (** No flags set *)
  val none : t

  (** Combine multiple flags *)
  val all : t list -> t

  (** Create flag indicating field is nullable *)
  val nullable : t

  (** Create flag indicating dictionary ordering *)
  val dictionary_ordered : t

  (** Create flag indicating map keys are sorted *)
  val map_keys_sorted : t

  (** Check if field is nullable *)
  val is_nullable : t -> bool

  (** Check if dictionary is ordered *)
  val is_dictionary_ordered : t -> bool

  (** Check if map keys are sorted *)
  val is_map_keys_sorted : t -> bool
end

(** Abstract schema type *)
type t

(** Create a schema field *)
val create :
  format:Type.t ->
  name:string ->
  ?metadata:(string * string) list ->
  ?flags:Flags.t ->
  ?children:t list ->
  unit -> t

(** Get the format/type of a schema field *)
val format : t -> Type.t

(** Get the name of a schema field *)
val name : t -> string

(** Get the metadata of a schema field *)
val metadata : t -> (string * string) list

(** Get the flags of a schema field *)
val flags : t -> Flags.t

(** Get the children of a schema field *)
val children : t -> t list

(** Convert schema to string representation *)
val to_string : t -> string

(** {2 Internal conversion functions for C interop} *)

(** Convert from C_wrapper.Schema.t - for internal use only *)
val of_c_wrapper : C_wrapper.Schema.t -> t

(** Convert to C_wrapper.Schema.t - for internal use only *)
val to_c_wrapper : t -> C_wrapper.Schema.t