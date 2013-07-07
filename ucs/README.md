ucslib
======

Small Unicode library for OCaml.

To use the Udb module, first build the library and program, then run the
program `make_udb /usr/share/unicode/UnicodeData.txt udb_data.ml`. Install the
`unicode-data` package if you are using Debian or Ubuntu. The produced file
should then be put into the `library` directory, replacing the dummy file.
After a subsequent recompilation, the Udb functions will be useful.
