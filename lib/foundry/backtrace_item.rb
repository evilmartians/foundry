module Foundry
  class BacktraceItem
    attr_reader :file, :line, :function

    def initialize(file, line, function)
      @file, @line, @function = file, line, function
    end

    def to_s
      "#{@file}:#{@line}:in `#{@function}'"
    end
  end
end