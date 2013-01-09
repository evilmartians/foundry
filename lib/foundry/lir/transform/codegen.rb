module Foundry
  class LIR::Transform::Codegen
    def run(translator)
      @llvm  = translator.llvm_module

      @types = Hash.new { |h, type|  h[type]  = emit_type(type)   }
      @data  = Hash.new { |h, datum| h[datum] = emit_datum(datum) }

      bootstrap_types

      translator.each_function do |func|
        emit_function(func)
      end if false

      @llvm.dump
      #@llvm.verify
    end

    def bootstrap_types
      emit_datum(VI::NIL,   'nil')
      emit_datum(VI::TRUE,  'true')
      emit_datum(VI::FALSE, 'false')
    end

    def emit_class_body_type(klass)
      if klass.is_a?(VI::SingletonClass)
        if klass.object.is_a?(VI::Class)
          llvm_name = name("s", klass.object)
        else
          llvm_name = "s.#{klass.__id__}"
        end
      else
        llvm_name = name(nil, klass)
      end

      unless (llvm_ty = @llvm.types[llvm_name])
        llvm_ty = LLVM::Type.struct([], false, llvm_name)
        llvm_ty.element_types = [
            (emit_class_body_type(klass.superclass) unless klass.superclass.nil?),
        ].compact
      end

      llvm_ty
    end

    def emit_type(type, entity_name=nil)
      case type
      when LIR.void
        LLVM::Type.void

      when Monotype
        klass         = type.klass
        llvm_body_ty  = emit_class_body_type(klass)

        llvm_imp_name = "i.#{llvm_body_ty.name}"

        unless (llvm_imp_ty = @llvm.types[llvm_imp_name])
          llvm_imp_ty = LLVM::Type.struct([], false, llvm_imp_name)

          if klass == VI::Class
            llvm_klass_ref_ty = llvm_imp_ty.pointer
          else
            llvm_klass_ref_ty = @types[Monotype.of(VI::Class)].pointer
          end

          llvm_imp_ty.element_types = [
              llvm_klass_ref_ty.pointer,
              llvm_body_ty,
          ]
        end

        llvm_imp_ty

      else
        raise RuntimeError, "unable to lower type #{type.inspect}"
      end
    end

    def emit_datum(object, name=nil)
      if object.is_a?(VI::Class) && !object.name.nil?
        name = name(nil, object)
      end

      if name && (datum = @llvm.globals[name])
        datum
      else
        if object.singleton_class_defined?
          klass      = object.singleton_class
          klass_name = "S." + (name || object.__id__.to_s)
          llvm_ty    = emit_type(Monotype.of(klass), klass_name)
        else
          klass      = object.class
          llvm_ty    = @types[Monotype.of(klass)]
        end

        datum = @llvm.globals.add llvm_ty, name

        datum.initializer = LLVM::ConstantStruct.const([
            emit_datum(klass, klass_name)
        ])

        datum
      end
    end

    def emit_function(func)
      arguments_ty = func.arguments.map(&:type).map { |ty| @types[ty] }
      return_ty    = @types[func.return_type]

      @values     = Hash.new { |h, k| raise RuntimeError, "unmapped val #{k.inspect}" }
      @phi_fixups = {}

      llvm_func = @llvm.functions.add(func.name, arguments_ty, return_ty)

      func.arguments.each_with_index do |arg, index|
        @values[arg] = llvm_func.params[index]
      end

      func.each_basic_block do |block|
        llvm_block = llvm_func.basic_blocks.append(block.name)

        llvm_block.build do |builder|
          block.each_instruction do |insn|
            emit_code(builder, insn)
          end
        end
      end
    end

    def emit_code(builder, insn)
      p builder, insn
    end

    protected

    def name(prefix, entity, entity_name=nil)
      name = entity.name

      if name.nil?
        name = entity_name
      end

      if name.nil?
        name = entity.__id__
      else
        name = name.to_s.gsub(/::/, '.')
      end

      if prefix.nil?
        name.to_s
      else
        "#{prefix}.#{name}"
      end
    end
  end
end