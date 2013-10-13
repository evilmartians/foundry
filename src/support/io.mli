open Unicode.Std

val open_in   : latin1s -> in_channel
val open_out  : latin1s -> out_channel

val input_all : in_channel -> string

val input_file  : latin1s -> string
val output_file : latin1s -> string -> unit
