def s
  @sexp_generator ||= Object.new.tap do |obj|
    def obj.[](*args)
      AST::Node.from_sexp_without_location(args)
    end
  end
end