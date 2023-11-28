namespace ue2_emu
{
    class MmioRequest
    {
        public bool IsRead;
        public int Address;
        public byte Value;
        public bool Handled = false;

        public void Write(byte value)
        {
            Handled = true;
            Value = value;
        }

        public byte Read()
        {
            Handled = true;
            return Value;
        }

        public static MmioRequest Read(int addr)
        {
            return new MmioRequest
            {
                IsRead = true,
                Address = addr,
                Value = 0
            };
        }

        public static MmioRequest Write(int addr, byte value)
        {
            return new MmioRequest
            {
                IsRead = false,
                Address = addr,
                Value = value
            };
        }

        public bool IsWrite() => !IsRead;
    }
}