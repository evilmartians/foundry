class USBEndpointUnit < Unit
  class USB_ADDR < Register(16)
    self.field(:addr,       :rw, offset: 0, width: 15)
  end

  class USB_COUNT_TX < Register(16)
    self.field(:count,      :rw, offset: 0,  width: 9)
  end

  class USB_COUNT_RX < Register(16)
    self.field(:count,      :r,  offset: 0,  width: 9)
    self.field(:num_block,  :rw, offset: 10, width: 5)
    self.flag(:bl_size,     :rw, offset: 15)
  end

  self.register(:ADDR_TX,   :rw, offset: 0x00, align: 4, impl: USB_ADDR)
  self.register(:COUNT_TX,  :rw, offset: 0x04, align: 4, impl: USB_COUNT_TX)
  self.register(:ADDR_RX,   :rw, offset: 0x08, align: 4, impl: USB_ADDR)
  self.register(:COUNT_RX,  :rw, offset: 0x0C, align: 4, impl: USB_COUNT_RX)

  def @buffer_base : Unsigned(32)

  def initialize(base, buffer_base:)
    @base = base # super(base)

    @buffer_base = buffer_base
  end

  def setup_rx(offset:, length:)
    let big_blocks = length > 62
    let num_blocks = (if big_blocks then length / 32
                      else               length / 2  end)

    # Offset is USB-local.
    self.ADDR_RX  = self.ADDR_RX.set_addr(offset)
    self.COUNT_RX = self.COUNT_RX.set_bl_size(big_blocks).
                                  set_num_block(num_blocks)

    if big_blocks
      num_blocks * 32
    else
      num_blocks * 2
    end
  end

  def setup_tx(offset:, length:)
    # Offset is USB-local.
    self.ADDR_TX  = self.ADDR_TX.set_addr(offset)
    self.COUNT_TX = self.COUNT_TX.set_count(length)

    length
  end

  def rx_buffer : (\self) -> ROMemory(align: 4, width: 2, stride: 2)
    let {addr: offset}       = self.ADDR_RX
    let {bl_size, num_block} = self.COUNT_RX

    let address = @buffer_base + offset << 1
    let length  = (if bl_size then num_block * 32
                   else            num_block * 2  end)

    invokeprimitive fat_alloc(address, length)
  end

  def tx_buffer : (\self) -> RWMemory(align: 4, width: 2, stride: 2)
    let {addr: offset}  = self.ADDR_RX
    let {count: length} = self.COUNT_TX

    let address = @buffer_base + offset << 1

    invokeprimitive fat_alloc(address, length)
  end
end

class USBUnit < Unit
  class USB_CNTR < Register(16)
    self.flag(:fres,    :rw, offset: 0)
    self.flag(:pdwn,    :rw, offset: 1)
    self.flag(:lp_mode, :rw, offset: 2)
    self.flag(:fsusp,   :rw, offset: 3)
    self.flag(:resume,  :rw, offset: 4)
    self.flag(:esofm,   :rw, offset: 8)
    self.flag(:sofm,    :rw, offset: 9)
    self.flag(:resetm,  :rw, offset: 10)
    self.flag(:suspm,   :rw, offset: 11)
    self.flag(:wkupm,   :rw, offset: 12)
    self.flag(:errm,    :rw, offset: 13)
    self.flag(:pmaovrm, :rw, offset: 14)
    self.flag(:ctrm,    :rw, offset: 15)
  end

  class USB_ISTR < Register(16)
    self.field(:ep_id,  :r,     offset: 0, width: 4)
    self.flag(:dir,     :r,     offset: 4)
    self.flag(:esof,    :rc_w0, offset: 8)
    self.flag(:sof,     :rc_w0, offset: 9)
    self.flag(:reset,   :rc_w0, offset: 10)
    self.flag(:susp,    :rc_w0, offset: 11)
    self.flag(:wkup,    :rc_w0, offset: 12)
    self.flag(:err,     :rc_w0, offset: 13)
    self.flag(:pmaovr,  :rc_w0, offset: 14)
    self.flag(:ctr,     :r,     offset: 15)
  end

  class USB_FNR < Register(16)
    self.field(:fn,     :r,     offset: 0,  width: 11)
    self.field(:lsof,   :r,     offset: 11, width: 2)
    self.flag(:lck,     :r,     offset: 13)
    self.flag(:rxdm,    :r,     offset: 14)
    self.flag(:rxdp,    :r,     offset: 15)
  end

  class USB_DADDR < Register(16)
    self.field(:addr,   :rw,    offset: 0,  width: 7)
    self.flag(:ef,      :rw,    offset: 8)
  end

  class USB_EPR < Register(16)
    self.field(:ea,       :rw,    offset: 0,  width: 4)
    self.field(:stat_tx,  :t,     offset: 4,  width: 2)
    self.flag(:dtog_tx,   :t,     offset: 6)
    self.flag(:ctr_tx,    :rc_w0, offset: 7)
    self.flag(:ep_kind,   :rw,    offset: 8)
    self.field(:ep_type,  :rw,    offset: 9,  width: 2)
    self.flag(:setup,     :r,     offset: 11)
    self.field(:stat_rx,  :t,     offset: 12, width: 2)
    self.flag(:dtog_rx,   :t,     offset: 14)
    self.flag(:ctr_rx,    :rc_w0, offset: 15)

    def set_stat_tx(stat_tx)
      self.toggle_stat_tx(self.stat_tx ^ stat_tx)
    end

    def set_stat_rx(stat_rx)
      self.toggle_stat_rx(self.stat_rx ^ stat_rx)
    end
  end

  class USB_BTABLE < Register(16)
    self.field(:btable,   :rw,    offset: 3,  width: 13)
  end

  self.registers(:EPR,    :rwc, offset: 0x00, count: 8, align: 4, impl: USB_EPR,
                                invariant_set:   0b1000_0000_1000_0000,
                                invariant_clear: 0b0111_0000_0111_0000)
  self.register(:CNTR,    :rw,  offset: 0x40, align: 4, impl: USB_CNTR)
  self.register(:ISTR,    :rwc, offset: 0x44, align: 4, impl: USB_ISTR,
                                invariant_set:   0b0111_1111_0000_0000)
  self.register(:FNR,     :r,   offset: 0x48, align: 4, impl: USB_FNR)
  self.register(:DADDR,   :rw,  offset: 0x4C, align: 4, impl: USB_DADDR)
  self.register(:BTABLE,  :rw,  offset: 0x50, align: 4, impl: USB_BTABLE)

  def @buffer_base : Unsigned(32)

  def initialize(base, buffer_base:)
    super(base)

    @buffer_base = buffer_base
  end

  def address
    self.DADDR.addr
  end

  def address=(address)
    self.DADDR = self.DADDR.set_addr(address)
  end

  def endpoint(num)
    # USBEndpointUnit.new(@buffer_base + (self.BTABLE.btable + num) << 3,
    #                     buffer_base: @buffer_base)
    USBEndpointUnit.new(@buffer_base + num << 3,
                        buffer_base: @buffer_base)
  end

  def power_up
    # Power up the USB peripheral.
    self.CNTR = self.CNTR.set_pdwn(false)

    # Wait tSTARTUP=1uS
    48_000u32.times((_) { self.CNTR })

    # Deassert USB reset.
    self.CNTR = self.CNTR.set_fres(false)

    # Clear any spurious interrupt condition.
    self.ISTR = self.clear_ISTR.
                     clear_esof.clear_sof.clear_reset.clear_susp.
                     clear_wkup.clear_err.clear_pmaovr

    # Enable interrupt on correct transfer.
    self.CNTR = self.CNTR.set_ctrm(true).set_resetm(true)

    # Enable USB function on address 0.
    self.DADDR = self.DADDR.set_ef(true).set_addr(0)

    self
  end

  def setup_control(tx_length:, rx_length:)
    # Set up the control endpoint.
    self.EPR(0) = self.EPR(0).set_ea(0).         # endpoint 0
                              set_ep_type(0b01). # control
                              set_stat_rx(0b11)  # ACK RX

    # Set up control endpoint buffers.
    let ep0 = self.endpoint(0)

    # One endpoint descriptor is 4 word = 8 byte long. There are
    # 8 of them.
    let mut offset = 8u16 * 8u16
    offset += ep0.setup_rx(offset:, length: rx_length)
    offset += ep0.setup_tx(offset:, length: tx_length)

    # Return token and endpoint.
    [{ id: 1u16, offset }, ep0]
  end

  def setup_endpoint(token, address:, kind:, tx_length:, rx_length:)
    # Unpack the sequencing data.
    let {id, mut offset} = token

    # Set up the endpoint. If {rx,tx}_length is zero, mark the corresponding
    # OUT/IN endpoint as DISABLED, otherwise as VALID/NAK.
    let stat_rx  = (if rx_length == 0 then 0b00 else 0b10 end)
    let stat_tx  = (if tx_length == 0 then 0b00 else 0b10 end)
    self.EPR(id) = self.EPR(id).set_ea(address).
                                set_ep_type(kind).
                                set_stat_rx(stat_rx).
                                set_stat_tx(stat_tx)

    # Set up endpoint buffers.
    let ep = self.endpoint(id)

    offset += ep.setup_rx(offset:, length: rx_length)
    offset += ep.setup_tx(offset:, length: tx_length)

    # Return token and endpoint.
    [{ id: id + 1, offset }, ep]
  end

  def handle_interrupt(device)
    if self.ISTR.ctrm
      # A transfer has successfully finished. Record where it happened.
      let {ep_id}                 = self.ISTR
      let {ctr_tx, ctr_rx, setup} = self.EPR(ep_id)

      if ctr_rx
        # Mark RX endpoint as NAK.
        self.rx_status(ep_id) = 0b10

        if ep_id == 0 && setup
          # ep_id = 0 always corresponds to EP0, i.e. control endpoint.
          device.handle_setup
        else
          device.handle_rx(ep_id)
        end
      end

      if ctr_tx
        # Mark TX endpoint as NAK.
        self.tx_status(ep_id) = 0b10

        device.handle_tx(ep_id)
      end

      # Clear TX/RX completion flags.
      self.EPR(ep_id) = self.clear_EPR(ep_id).
                             clear_ctr_tx.clear_ctr_rx
    end

    nil
  end

  def rx_status=(ep_id, status)
    self.EPR(ep_id) = self.EPR(ep_id).set_stat_rx(status)
  end

  def tx_status=(ep_id, status)
    self.EPR(ep_id) = self.EPR(ep_id).set_stat_tx(status)
  end
end
