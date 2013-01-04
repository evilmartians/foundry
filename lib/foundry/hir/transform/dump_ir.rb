module Foundry
  class HIR::Transform::DumpIR < HIR::Processor
    def on_dump_ir(node)
      proc, = node.children
      code, = proc.children

      p code

      s(:apply, proc, s(:tuple), s(:nil))
    end
  end
end