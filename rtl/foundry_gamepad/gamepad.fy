class Gamepad
  KEY_LEFT  = 0
  KEY_RIGHT = 2
  KEY_UP    = 3
  KEY_DOWN  = 6
  KEY_B     = 8
  KEY_A     = 10

  BUZZ      = 7

  def @lcd : GamepadLCD

  def initialize
    @lcd = GamepadLCD.new
  end

  def setup
    RCC.ICSCR = RCC.ICSCR.set_msirange(0b110)

    RCC.AHBENR = RCC.AHBENR.
          set_gpioaen(true)

    GPIOA.as_input(KEY_LEFT,  pull_down: true).
          as_input(KEY_RIGHT, pull_down: true).
          as_input(KEY_UP,    pull_down: true).
          as_input(KEY_DOWN,  pull_down: true).
          as_input(KEY_B,     pull_down: true).
          as_input(KEY_A,     pull_down: true).
          as_output(BUZZ)

    @lcd.setup

    self
  end

  def lcd
    @lcd
  end

  def up_pressed?
    GPIOA.get(KEY_UP)
  end

  def down_pressed?
    GPIOA.get(KEY_DOWN)
  end

  def left_pressed?
    GPIOA.get(KEY_LEFT)
  end

  def right_pressed?
    GPIOA.get(KEY_RIGHT)
  end

  def a_pressed?
    GPIOA.get(KEY_A)
  end

  def b_pressed?
    GPIOA.get(KEY_B)
  end

  def delay(n) : (\self, Unsigned(32)) -> Nil
    n.times((_) { RCC.AHBENR }) # silly
  end
end
