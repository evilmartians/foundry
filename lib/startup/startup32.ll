@__text_end__   = external global i32
@__data_begin__ = external global i32
@__data_end__   = external global i32
@__bss_begin__  = external global i32
@__bss_end__    = external global i32

declare void @main()

define void @_startup() {
entry:
  br label %load_data

load_data:
  %pdata.src = phi i32* [ @__text_end__,   %entry ], [ %pdata.src.inc, %load_data ]
  %pdata.dst = phi i32* [ @__data_begin__, %entry ], [ %pdata.dst.inc, %load_data ]
  %data.word = load i32* %pdata.src, align 4
  store i32 %data.word, i32* %pdata.dst, align 4

  %data.src      = ptrtoint i32* %pdata.src to i32
  %data.src.inc  = add i32 %data.src, 4
  %pdata.src.inc = inttoptr i32 %data.src.inc to i32*

  %data.dst      = ptrtoint i32* %pdata.dst to i32
  %data.dst.inc  = add i32 %data.dst, 4
  %pdata.dst.inc = inttoptr i32 %data.dst.inc to i32*

  %data.done = icmp uge i32 %data.dst.inc, ptrtoint(i32* @__data_end__ to i32)
  br i1 %data.done, label %clear_bss, label %load_data

clear_bss:
  %pbss = phi i32* [ @__bss_begin__, %load_data ], [ %pbss.inc, %clear_bss ]
  store i32 0, i32* %pbss, align 4

  %bss      = ptrtoint i32* %pbss to i32
  %bss.inc  = add i32 %bss, 4
  %pbss.inc = inttoptr i32 %bss.inc to i32*

  %bss.done = icmp uge i32 %bss.inc, ptrtoint(i32* @__bss_end__ to i32)
  br i1 %bss.done, label %call_main, label %clear_bss

call_main:
  call void @main()
  br label %loop

loop:
  br label %loop
}
