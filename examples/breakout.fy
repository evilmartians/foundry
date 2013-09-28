class Breakout
  DISPLAY_WIDTH  = 128
  DISPLAY_HEIGHT = 64
  BALL_SIZE      = 2
  PADDLE_Y       = 60
  PADDLE_HEIGHT  = 2
  BRICK_WIDTH    = 8u32
  BRICK_HEIGHT   = 2u32
  BRICKS_HORZ    = 14u32
  BRICKS_VERT    = 5u32

  class Brick
    def @x : Unsigned(32)
    def @y : Unsigned(32)
    def @active : Boolean

    def initialize(x:, y:)
      @x = x
      @y = y
      @active = true
    end

    def x
      @x
    end

    def y
      @y
    end

    def active?
      @active
    end

    def active=(is_active)
      @active = is_active
    end
  end

  def @paddle_w  : Unsigned(32)
  def @paddle_x  : Unsigned(32)

  def @on_paddle : Boolean
  def @ball_x    : Unsigned(32)
  def @ball_y    : Unsigned(32)
  def @ball_vx   : Unsigned(32)
  def @ball_vy   : Unsigned(32)

  def @bricks    : Array(Brick)

  def initialize
    @paddle_w = 12u32
    @paddle_x = (DISPLAY_WIDTH - @paddle_w) / 2u32

    @on_paddle = true
    @ball_x = @ball_vx = @ball_y = @ball_vy = 0u32

    @bricks = Array(Brick).new(reserve: BRICKS_HORZ * BRICKS_VERT)
    BRICKS_VERT.times((y) do
      BRICKS_HORZ.times((x) do
        @bricks[y * BRICKS_HORZ + x] =
            Brick.new(x: 1 + x * (BRICK_WIDTH + 1),
                      y: y * (BRICK_HEIGHT + 1))
      end)
    end)
  end

  def handle_left
    if @paddle_x > 0
      @paddle_x -= 1
    end

    self
  end

  def handle_right
    if @paddle_x + @paddle_w < DISPLAY_WIDTH
      @paddle_x += 1
    end

    self
  end

  def handle_up
    if @on_paddle
      @on_paddle = false
      @ball_x  = @paddle_x + @paddle_w / 2 - 1
      @ball_y  = PADDLE_Y - BALL_SIZE
      @ball_vx = 1
      @ball_vy = -1
    end

    self
  end

  def tick
    if !@on_paddle
      @ball_x += @ball_vx
      @ball_y += @ball_vy
    end

    @bricks.each((brick) do
      if @ball_y == brick.y + BRICK_HEIGHT &&
            @ball_x + BALL_SIZE > brick.x &&
            @ball_x < brick.x + BRICK_WIDTH &&
            brick.active?
        brick.active = false
        @ball_vy = -@ball_vy
      end
      nil
    end)

    if @ball_x == 0 || @ball_x + BALL_SIZE == DISPLAY_WIDTH
      @ball_vx = -@ball_vx
    end

    if @ball_y == 0
      @ball_vy = -@ball_vy
    elsif @ball_y + BALL_SIZE == PADDLE_Y &&
            @ball_x + BALL_SIZE > @paddle_x &&
            @ball_x < @paddle_x + @paddle_w
      @ball_vy = -@ball_vy
    elsif @ball_y == DISPLAY_HEIGHT - BALL_SIZE
      self.game_over
    end

    self
  end

  def game_over
    @on_paddle = true

    @bricks.each((brick) do
      brick.active = true
    end)

    self
  end

  def draw(display)
    # paddle
    display.rectangle(x: @paddle_x, y: PADDLE_Y,
                      w: @paddle_w, h: PADDLE_HEIGHT, true)

    # ball
    if @on_paddle
      display.rectangle(x: @paddle_x + @paddle_w / 2 - 1, y: PADDLE_Y - BALL_SIZE,
                        w: BALL_SIZE, h: BALL_SIZE, true)
    else
      display.rectangle(x: @ball_x, y: @ball_y,
                        w: BALL_SIZE, h: BALL_SIZE, true)
    end

    # bricks
    @bricks.each((brick) do
      if brick.active?
        display.rectangle(x: brick.x, y: brick.y,
                          w: BRICK_WIDTH, h: BRICK_HEIGHT, true)
      end
    end)

    self
  end
end

Board = Gamepad.new
LCD   = Board.lcd
Game  = Breakout.new

def main
  Board.setup

  let mut jiffies = 0u32

  while true
    if jiffies % 2 == 0
      Game.tick
    end

    if Board.left_pressed? || Board.a_pressed?
      Game.handle_left
    elsif Board.right_pressed? || Board.b_pressed?
      Game.handle_right
    elsif Board.up_pressed?
      Game.handle_up
    end

    LCD.clear
    Game.draw(LCD)
    LCD.refresh

    jiffies += 1
  end
end
