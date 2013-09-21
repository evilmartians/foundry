class USBDevicePipe
  def @device    : USBDevice
  def @endpoint  : Unsigned(32)
  def @rx_buffer : ROMemory
  def @tx_buffer : RWMemory

  def initialize(device, endpoint, rx_buffer:, tx_buffer:)
    @device    = device
    @endpoint  = endpoint
    @rx_buffer = rx_buffer
    @tx_buffer = tx_buffer
  end

  def rx_available
  end

  def tx_ack
  end

and class USBDeviceControlPipe < USBDevicePipe
  TO_HOST           = 1
  TO_DEVICE         = 0

  TYPE_STANDARD     = 0
  TYPE_CLASS        = 1
  TYPE_VENDOR       = 2

  RECP_DEVICE       = 0
  RECP_INTERFACE    = 1
  RECP_ENDPOINT     = 2
  RECP_OTHER        = 3

  GET_STATUS        = 0x00
  CLEAR_FEATURE     = 0x01
  SET_FEATURE       = 0x03
  SET_ADDRESS       = 0x05
  GET_DESCRIPTOR    = 0x06
  SET_DESCRIPTOR    = 0x07
  GET_CONFIGURATION = 0x08
  SET_CONFIGURATION = 0x09
  GET_INTERFACE     = 0x0A
  SET_INTERFACE     = 0x11
  SYNCH_FRAME       = 0x12

  def initialize(device, rx_buffer:, tx_buffer:)
    super(device, 0, rx_buffer:, tx_buffer:)
  end

  def setup
    let [< # bmRequestType,
           recipient : u5, kind : u2, direction : u1,
           # bRequest,     wValue,      wIndex,      wLength
           request   : u8, value : u16, index : u16, length : u16,
           *
        >] = @rx_buffer

    let packet = { recipient:, direction:, request:, length:, value:, index: }

    case kind
    when TYPE_STANDARD
      self.setup_standard(packet)
    when TYPE_CLASS
      self.setup_class(packet)
    when TYPE_VENDOR
      self.setup_vendor(packet)
    end

    nil
  end

  def setup_standard(pkt)
    case pkt.recipient
    when RECP_DEVICE
      case [pkt.direction, pkt.request, pkt.length]
      when [TO_HOST,   GET_STATUS,        2]
        @tx_buffer.fill([< 0 : u16 >])
        @device.tx_available(@endpoint)

      when [TO_DEVICE, CLEAR_FEATURE |
                       SET_FEATURE,       0]
        # No supported features.

      when [TO_DEVICE, SET_ADDRESS,       0]
        @device.address = value

      when [TO_HOST,   GET_DESCRIPTOR,    _]
        ...

      when [TO_DEVICE, SET_DESCRIPTOR,    _]
        # Not supported.

      when [TO_HOST,   GET_CONFIGURATION, 1]
        let id = @device.configuration_id
        @tx_buffer.fill([< id : u8, 0 : u8 >])
        @device.tx_available(@endpoint)

      when [TO_DEVICE, SET_CONFIGURATION, 0]
        @device.configuration_id = (value.narrow : Unsigned(8))

      else
        # Invalid request.
      end

    when RECP_INTERFACE
      case [pkt.direction, pkt.request, pkt.length]
      when [TO_HOST,   GET_STATUS,     2]
        @tx_buffer.fill([< 0 : u16 >])
        @device.tx_available(@endpoint)

      when [TO_DEVICE, CLEAR_FEATURE |
                       SET_FEATURE,    0]
        # No defined features.

      when [TO_HOST,   GET_INTERFACE,  1]
        # Not supported.

      when [TO_DEVICE, SET_INTERFACE,  0]
        # Not supported.

      else
        # Invalid request.
      end

    when RECP_ENDPOINT
      case [pkt.direction, pkt.request, pkt.length]
      when [TO_HOST,   GET_STATUS,     2]
        @tx_buffer.fill([< 0 : u16 >])
        @device.tx_available(@endpoint)

      when [TO_DEVICE, CLEAR_FEATURE |
                       SET_FEATURE,    0]
        # No supported features.

      when [TO_HOST,   SYNCH_FRAME,    2]
        # Not supported.

      else
        # Invalid request.
      end
    else
      # Invalid request.
    end
  end

  def setup_class(pkt)
  end

  def setup_vendor(pkt)
  end

and class USBDevice(\unit)
  def @unit         : \unit
  def @control_pipe : USBDeviceControlPipe

  def initialize(unit)
    @unit         = unit
    @control_pipe = USBDeviceControlPipe.new(self, unit.rx_buffer(0), unit.tx_buffer(0))
  end

  def handle_interrupt
    @unit.handle_interrupt(self)
  end

  def setup_ready
    @control_pipe.setup
  end

  def rx_ready(endpoint_id)
    if endpoint == 0
      @control_pipe.rx_available
    end
  end

  def tx_ready(endpoint_id)
    if endpoint == 0
      @control_pipe.tx_ack
    end
  end

  def address=(address)
    @unit.address = address
  end

  def rx_ack(endpoint_id)
    @unit.rx_status(endpoint_id) = 0b11
  end

  def tx_available(endpoint_id)
    @unit.tx_status(endpoint_id) = 0b11
  end
end
