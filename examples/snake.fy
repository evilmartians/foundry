class PRNG
  def @next : Unsigned(32)

  def initialize(seed)
    @next = seed
  end

  def next(max)
    @next = @next * 1103515245 + 12345
    @next % max
  end
end

Random = PRNG.new(123u32)

class Snake
  WIDTH  = 64
  HEIGHT = 32

  CLEAR  = 0u8
  UP     = 1u8
  DOWN   = 2u8
  LEFT   = 3u8
  RIGHT  = 4u8
  FOOD   = 5u8

  class Field
    def @cells : Array(Unsigned(8))

    def initialize
      @cells = Array(Unsigned(8)).new(reserve: WIDTH * HEIGHT)
    end

    def at(x:, y:)
      @cells[y * WIDTH + x]
    end

    def at=(x:, y:, status)
      @cells[y * WIDTH + x] = status
    end
  end

  def @field  : Field
  def @head_x : Unsigned(32)
  def @head_y : Unsigned(32)
  def @tail_x : Unsigned(32)
  def @tail_y : Unsigned(32)

  def @paused : Boolean
  def @game_over : Boolean
  def @game_over_jiffies : Unsigned(32)

  def initialize
    @field  = Field.new

    self.reset
  end

  def reset
    @game_over = false
    @game_over_jiffies = 0u32
    @paused = false

    @head_y = @tail_y = 0u32
    @head_x = 9u32
    @tail_x = 0u32

    HEIGHT.times((y) do
      WIDTH.times((x) do
        @field.at(x:, y:) = CLEAR
      end)
    end)

    10.times((x) do
      @field.at(x:, y: 0) = RIGHT
    end)

    self.place_food
  end

  def head
    @field.at(x: @head_x, y: @head_y)
  end

  def head=(status)
    @field.at(x: @head_x, y: @head_y) = status
  end

  def tail
    @field.at(x: @tail_x, y: @tail_y)
  end

  def tail=(status)
    @field.at(x: @tail_x, y: @tail_y) = status
  end

  def game_over
    @game_over = true
    @game_over_jiffies = 0
  end

  def place_food
    let mut placed = false
    while !placed
      let x = Random.next(WIDTH)
      let y = Random.next(HEIGHT)
      if @field.at(x:, y:) == CLEAR
        @field.at(x:, y:) = FOOD
        placed = true
      end
    end
  end

  def move_snake
    let head_dir = self.head

    if head_dir == LEFT
      if @head_x == 0
        self.game_over
      else
        @head_x -= 1
      end
    elsif head_dir == RIGHT
      if @head_x == WIDTH - 1
        self.game_over
      else
        @head_x += 1
      end
    elsif head_dir == UP
      if @head_y == 0
        self.game_over
      else
        @head_y -= 1
      end
    elsif head_dir == DOWN
      if @head_y == HEIGHT - 1
        self.game_over
      else
        @head_y += 1
      end
    end

    if self.head != FOOD
      let tail_dir = self.tail
      self.tail = CLEAR

      if tail_dir == LEFT
        @tail_x -= 1
      elsif tail_dir == RIGHT
        @tail_x += 1
      elsif tail_dir == UP
        @tail_y -= 1
      elsif tail_dir == DOWN
        @tail_y += 1
      end
    else
      self.place_food
    end

    if self.head != CLEAR && self.head != FOOD
      self.game_over
    else
      self.head = head_dir
    end

    nil
  end

  def draw(display)
    HEIGHT.times((y) do
      WIDTH.times((x) do
        let mut on = @field.at(x:, y:) != CLEAR
        if @game_over && @game_over_jiffies % 2 == 0
          on = !on
        end

        display.rectangle(x: x * 2, y: y * 2, w: 2, h: 2, on)
      end)
    end)
  end

  def tick
    if !@game_over
      if !@paused
        self.move_snake
      end
    else
      @game_over_jiffies += 1
      nil
    end
  end

  def handle_left
    @field.at(x: @head_x, y: @head_y) = LEFT
  end

  def handle_right
    @field.at(x: @head_x, y: @head_y) = RIGHT
  end

  def handle_up
    @field.at(x: @head_x, y: @head_y) = UP
  end

  def handle_down
    @field.at(x: @head_x, y: @head_y) = DOWN
  end

  def handle_a
    if @game_over
      @game_over = false
      self.reset
    end
  end

  def handle_b
    @paused = !@paused
  end
end

Board = Gamepad.new
LCD   = Board.lcd
Game  = Snake.new

def main
  Board.setup

  RCC.enable_hse
  RCC.enable_pll(div: 0b01, mul: 0b0001, use_hse: true)
  RCC.switch(0b11)

  let mut jiffies = 0u32

  while true
    if jiffies % 4 == 0
      Game.tick
    end

    if Board.left_pressed?
      Game.handle_left
    elsif Board.right_pressed?
      Game.handle_right
    elsif Board.up_pressed?
      Game.handle_up
    elsif Board.down_pressed?
      Game.handle_down
    elsif Board.a_pressed?
      Game.handle_a
    elsif Board.b_pressed?
      Game.handle_b
    end

    LCD.clear
    Game.draw(LCD)
    LCD.refresh

    jiffies += 1
  end
end
