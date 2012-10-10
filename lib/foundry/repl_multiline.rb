require 'irb/ruby-lex'
require 'stringio'

# Derived from https://github.com/janlelis/ripl-multi_line/blob/master/lib/ripl/multi_line/irb.rb,
# distributed under MIT license.

module Foundry
  module REPLMultiline
    class << self
      attr_reader :scanner
    end

    # create scanner and patch it to our needs
    @scanner = RubyLex.new

    def @scanner.multiline?
      initialize_input
      @continue = false
      @found_eof = false

      while line = lex
        @line << line
        @continue = false
      end

      !!( !@found_eof or @ltype or @continue or @indent > 0 )
    end

    def @scanner.lex
      until (((tk = token).kind_of?(RubyLex::TkNL) || tk.kind_of?(RubyLex::TkEND_OF_SCRIPT)) &&
          !@continue or tk.nil?)
        #p tk
        #p @lex_state
        #p self
      end

      @found_eof = true if tk.kind_of?(RubyLex::TkEND_OF_SCRIPT)
      line = get_readed

      if line == "" and tk.kind_of?(RubyLex::TkEND_OF_SCRIPT) || tk.nil?
        nil
      else
        line
      end
    end

    def self.multiline?(string)
      self.scanner.set_input StringIO.new(string + "\0")
      self.scanner.multiline?
    end
  end
end