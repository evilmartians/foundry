OUTPUT_FORMAT(elf32-littlearm)
ENTRY(_startup)

SECTIONS {
	.vectors : {
		*(.vectors)
	} > flash

	.text : {
		*(.text .text.*)

		. = ALIGN(4);

		*(.rodata .rodata.*)

		. = ALIGN(4);

		__text_end__ = .;
	} > flash

	.data : AT (__text_end__) {
		__ram_begin__  = .;
		__data_begin__ = .;

		*(.data .data.*)

		. = ALIGN(4);

		__data_end__ = .;
	} > ram

	. = ALIGN(4);

	.bss : {
		__bss_begin__ = .;

		*(.bss* COMMON)

		. = ALIGN(4);

		__bss_end__ = .;
	} > ram

	__stack_end__ = ADDR(.data) + RAMSIZE;

	/DISCARD/ : {
		*(.comment)
		*(.ARM.attributes)
	}
}
