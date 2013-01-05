require 'spec_helper'

describe 'HIR::SexpBuilder' do
  include Foundry::HIR::SexpBuilder

  it 'builds non-nested consts' do
    s_const('Foundry').should ==
      s(:const_ref, s(:var, :Cref), :Foundry)
  end

  it 'builds nested consts' do
    s_const('A::B::C').should ==
      s(:const_fetch,
        s(:const_fetch,
          s(:const_ref, s(:var, :Cref), :A),
          :B),
        :C)
  end

  it 'builds toplevel consts' do
    s_const('::A').should ==
      s(:const_fetch,
        s(:const_base),
        :A)
  end

  it 'builds calls' do
    s_send(s(:foo), :bar, s(:integer, 1), s(:string, "2")).should ==
      s(:send,
        s(:foo),
        s(:symbol, :bar),
        s(:tuple, s(:integer, 1), s(:string, "2")),
        s(:nil))
  end
end