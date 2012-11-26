module Foundry
  module AST::Prepare
    class DumpIR < AST::Processor
      def on_dump_ir(node)
        proc, = node.children
        code, = proc.children

        p code

        s(:proc_call, proc, s(:array), s(:nil))
      end
    end
  end
end