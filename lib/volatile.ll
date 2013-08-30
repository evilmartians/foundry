define i32 @foundry.mem_loadv.a1.i32(i32*) alwaysinline {
  %2 = load volatile i32* %0, align 1
  ret i32 %2
}

define void @foundry.mem_storev.a1.i32(i32*, i32) alwaysinline {
  store volatile i32 %1, i32* %0, align 1
  ret void
}

define i32 @foundry.mem_loadv.a2.i32(i32*) alwaysinline {
  %2 = load volatile i32* %0, align 2
  ret i32 %2
}

define void @foundry.mem_storev.a2.i32(i32*, i32) alwaysinline {
  store volatile i32 %1, i32* %0, align 2
  ret void
}

define i32 @foundry.mem_loadv.a4.i32(i32*) alwaysinline {
  %2 = load volatile i32* %0, align 4
  ret i32 %2
}

define void @foundry.mem_storev.a4.i32(i32*, i32) alwaysinline {
  store volatile i32 %1, i32* %0, align 4
  ret void
}
