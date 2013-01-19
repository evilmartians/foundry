module STM32::F1
  class RCCUnit
    include Registers

    register 0x00, :CR, align: 1 do |r|
      r.flag     :hsion,   :rw
      r.flag     :hsirdy,  :r
      r.reserved 1
      r.field    :hsitrim, :rw, 5
      r.field    :hsical,  :r,  8
      r.flag     :hseon,   :rw
      r.flag     :hserdy,  :r
      r.flag     :hsebyp,  :rw
      r.flag     :csson,   :rw
      r.reserved 4
      r.flag     :pllon,   :rw
      r.flag     :pllrdy,  :r
      r.reserved 6
    end
  end
end