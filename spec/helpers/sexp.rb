def s
  @sexp_generator ||= Object.new.tap do |obj|
    def obj.[](*args)
      AST::Node.from_simple_sexp(args)
    end
  end
end