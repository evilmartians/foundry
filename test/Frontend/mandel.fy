# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: **********************
# CHECK: *************+++******
# CHECK: ***********+++..+*****
# CHECK: **********+++.  ++****
# CHECK: *********++ .    .+***
# CHECK: *******+++..      .***
# CHECK: *****++....        +**
# CHECK: ***+++.            +**
# CHECK: ***+..            .+**
# CHECK: ***+..            .+**
# CHECK: ***+++.            +**
# CHECK: *****++....        +**
# CHECK: *******+++..      .***
# CHECK: *********++ .    .+***
# CHECK: **********+++.  ++****
# CHECK: ***********+++..+*****
# CHECK: *************+++******
# CHECK: **********************

class Signed
  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def >(other)
    invokeprimitive int_sgt(self, other)
  end

  def <(other)
    invokeprimitive int_slt(self, other)
  end

  def -@
    invokeprimitive int_sub(0s32, self)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def >>(other)
    invokeprimitive int_ashr(self, other)
  end
end

def putchar(chr) : (\a, Unsigned(32)) -> Unsigned(32)
  invokeprimitive external(:putchar, chr)
end

# mandelbrot converted to 24.8 fixed point

def printdensity(d)
  if d > 8s32
    self.putchar(32u32)
  elsif d > 4s32
    self.putchar(46u32)
  elsif d > 2s32
    self.putchar(43u32)
  else
    self.putchar(42u32)
  end
end

def mandelconverger(real, imag, iters, creal, cimag)
  let real_sq = (real * real) >> 8s32
  let imag_sq = (imag * imag) >> 8s32
  if iters > 255s32 or real_sq + imag_sq > 1024s32 # 0x00000400
    iters
  else
    self.mandelconverger(real_sq - imag_sq + creal,
                         2s32 * ((real * imag) >> 8s32) + cimag,
                         iters + 1s32, creal, cimag)
  end
end

def mandelconverge(real, imag)
  self.mandelconverger(real, imag, 0s32, real, imag)
end

def mandelhelp(xmin, xmax, xstep, ymin, ymax, ystep)
  let mut y = ymin
  while y < ymax
    let mut x = xmin
    while x < xmax
      self.printdensity(self.mandelconverge(x, y))
      x += xstep
    end
    self.putchar(10u32) # "\n"
    y += ystep
  end
end

def mandel(realstart, imagstart, realmag, imagmag)
  self.mandelhelp(realstart, realstart + realmag * 22s32, realmag,
                  imagstart, imagstart + imagmag * 18s32, imagmag)
end

def main
  # mandel(-2.3, -1.3, 0.05, 0.07)
  # mandel(-0x0000024c, -0x0000014c, 0x00000ccc, 0x000011eb)
  self.mandel(-588s32, -332s32, 39s32, 39s32)
end
