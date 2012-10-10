require_relative 'backtrace_item'

module Foundry
  class InterpreterError < StandardError
    attr_reader :inner_exception
    attr_reader :nested_inner_backtrace

    def initialize(interpreter, inner_exception)
      @interpreter     = interpreter
      @inner_exception = inner_exception
      @nested_inner_backtrace = interpreter.collect_backtrace
    end

    def inner_backtrace
      @nested_inner_backtrace.flatten
    end

    def interleave_backtraces(skip_last=0, &block)
      host_index     = 0
      host_backtrace = backtrace[0..-skip_last]

      @nested_inner_backtrace.each do |nested_parts|
        nested_parts.each do |target_line|
          while host_index < host_backtrace.size
            host_line = host_backtrace[host_index]
            host_index += 1

            yield host_line, true

            if [ 'with_scope', 'evaluate' ].find \
                  { |target| match_method?(host_line, target) }
              break
            end
          end

          yield target_line, false
        end
      end

      while host_index < host_backtrace.size
        yield backtrace[host_index], true
        host_index += 1
      end
    end

    protected

    def match_method?(line, method)
      interp_path = File.expand_path('../interpreter.rb', __FILE__)
      line =~ /#{interp_path}:\d+:in `#{method}'/
    end
  end
end