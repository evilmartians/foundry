module Foundry
  module Transform::RubyParser::Calls
    def on_iter(node)
      send, block_args, *block_body = node.children

      if block_body.empty?
        block_body = [ s(:nil) ]
      end

      block = node.updated(:lambda, [
        block_args, *block_body
      ])

      receiver, name, args = process(send).children

      send.updated(:send, [
        receiver, name, args,
        process(block)
      ])
    end

    def on_call(node)
      receiver, name, *args = node.children

      receiver = s(:self) if receiver.nil?

      if args.length > 0 &&
           args.last.type == :block_pass
        block, = args.last.children
        args   = args[0..-2]
      else
        block  = s(:nil)
      end

      process(node.updated(:send, [
        receiver,
        s(:symbol, name),
        s(:tuple, *args),
        block
      ]))
    end

    def on_attrasgn(node)
      process(node.updated(:call))
    end
  end
end