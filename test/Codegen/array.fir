; RUN: %foundry_gen %s -o %t
; RUN: llvm-dis %t -o - | %file_check -check-prefix LLASM %s
; RUN: lli %t | %file_check -check-prefix LLI %s

; LLASM-DAG: @[[ary:[0-9]+]] = global %array { i32 5, i32 2, i8* bitcast ([5 x i32]* @[[elems]] to i8*) }
; LLASM-DAG: @[[elems:[0-9]+]] = global [5 x i32] [i32 1, i32 2, i32 undef, i32 undef, i32 undef]
@a = storage(5 x unsigned(32)) [
  unsigned(32) 1,
  unsigned(32) 2
]

@main = function () -> unsigned(32) {
entry:
; LLASM: %cap = load i32* getelementptr inbounds (%array* @[[ary]], i32 0, i32 0)
; LLI: [DEBUG: 0x00000005]
  %cap = unsigned(32) primitive "ary_capa" (array(unsigned(32)) @a)
  primitive "debug" (%cap)

; LLASM: %len = load i32* getelementptr inbounds (%array* @[[ary]], i32 0, i32 1)
; LLI: [DEBUG: 0x00000002]
  %len = unsigned(32) primitive "ary_length" (array(unsigned(32)) @a)
  primitive "debug" (%len)

  %snd = unsigned(32) primitive "ary_get" (array(unsigned(32)) @a, unsigned(32) 1)
; LLI: [DEBUG: 0x00000002]
  primitive "debug" (%snd)

  unsigned(32) primitive "ary_set" (array(unsigned(32)) @a, unsigned(32) 0, unsigned(32) 42)
  %fst = unsigned(32) primitive "ary_get" (array(unsigned(32)) @a, unsigned(32) 0)
; LLI: [DEBUG: 0x0000002a]
  primitive "debug" (%fst)

  return unsigned(32) 0
}
