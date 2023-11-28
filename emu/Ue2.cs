namespace ue2_emu
{
    class Ue2
    {
        public const int PC_MAX = 0xfff;
        byte[] _memory;
        Dictionary<int, Action<MmioRequest>> _mmio = new Dictionary<int, Action<MmioRequest>>();
        bool _isHalted;
        OpCode op;
        int addr;


        public int pc;
        public int ir;
        
        public bool z;
        public bool c;
        public int rra;
        public int rrp;

        public int nextpc;

        public Ue2()
        {
            _memory = new byte[PC_MAX + 1];
            nextpc = 0;
        }

        public void Load(byte[] data, int at)
        {
            Array.Copy(data, 0, _memory, at, data.Length);
        }

        public void SetMmio(int addr, Action<MmioRequest> callback)
        {
            _mmio[addr] = callback;
        }

        public void Step()
        {
            pc = nextpc;
            ir = Read16(nextpc);
            nextpc += 2;
            nextpc &= PC_MAX;
            op = (OpCode)(ir >> 12);
            addr = ir & PC_MAX;
            switch (op)
            {
                case OpCode.BZ:
                    if (z) nextpc = addr & PC_MAX;
                    break;
                case OpCode.BL:
                    if (c) nextpc = addr & PC_MAX;
                    break;
                case OpCode.LDA:
                    rra = addr & 0xff;
                    break;
                case OpCode.LDL:
                    rra = Read8(addr);
                    break;
                case OpCode.LDP:
                    rra = Read8(rrp);
                    break;
                case OpCode.STL:
                    Write8(addr, (byte)rra);
                    break;
                case OpCode.STP:
                    Write8(rrp, (byte)rra);
                    break;
                case OpCode.LRP:
                    rrp = addr;
                    break;
                case OpCode.INP:
                    rrp++;
                    if (rrp >= PC_MAX)
                    {
                        rrp = 0;
                    }
                    break;
                case OpCode.SCF:
                    c = (addr & 1) !=0;
                    z = (addr & 2) !=0;
                    break;
                case OpCode.ADC:
                    rra = rra + (c ? 1 : 0) + Read8(addr);
                    SetCarry(ref rra);
                    SetZero(rra);
                    z = rra == 0;
                    break;
                case OpCode.CMP:
                    int tmp = rra + Read8(addr);
                    SetCarry(ref tmp);
                    SetZero(tmp);
                    break;
                case OpCode.SRL:
                    rra <<= 1;
                    SetCarry(ref rra);
                    SetZero(rra);
                    break;
                case OpCode.NAND:
                    rra = (~(rra & Read8(addr))) & 0xff;
                    SetZero(rra);
                    break;
                case OpCode.ORI:
                    rra = ((rra | Read8(addr))) & 0xff;
                    SetZero(rra);
                    break;
                case OpCode.ORE:
                    rra = ((rra ^ Read8(addr))) & 0xff;
                    SetZero(rra);
                    break;
            }
        }

        void SetCarry(ref int value)
        {
            if (value > 255)
            {
                value -= 256;
                c = true;
            }
            else
            {
                c = false;
            }
        }

        void SetZero(int value)
        {
            z = value == 0;
        }

        public byte Read8(int addr)
        {
            addr &= PC_MAX;
            if (_mmio.TryGetValue(addr, out var cb))
            {
                var req = MmioRequest.Read(addr);
                cb(req);
                if (req.Handled)
                {
                    return req.Value;
                }
            }
            return _memory[addr];
        }

        void Write8(int addr, byte value)
        {
            addr &= PC_MAX;
            if (_mmio.TryGetValue(addr, out var cb))
            {
                var req = MmioRequest.Write(addr, value);
                cb(req);
                if (req.Handled)
                {
                    return;
                }
            }
            _memory[addr] = value;
        }

        public int Read16(int addr)
        {
            int val = (Read8(addr) << 8) | Read8(addr + 1);
            return val;
        }

        public bool IsHalted() => _isHalted;

        public void Halt() => _isHalted = true;

        public void Reset()
        {
            Array.Fill<byte>(_memory, 0);
            _isHalted = false;
            op = default;
            addr = default;
            pc = default;
            ir = default;
            z = default;
            c = default;
            rra = default;
            rrp = default;
            nextpc = default;
        }
    }
}