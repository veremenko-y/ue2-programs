using System.Collections.Concurrent;
using System.Diagnostics;
using System.Reflection.Emit;
using System.Security.Cryptography;
using System.Text.RegularExpressions;

namespace ue2_emu
{
    internal class Program
    {
        const int HALT = 0xfff;
        const int RXREADY = 0xffe;
        const int RX = 0xffd;
        const int TX = 0xffc;
        static bool trace = false;
        static int breakpoint = -1;
        static string path = null;

        static void Main(string[] args)
        {
            Console.CancelKeyPress += (sender, e) =>
            {
                if (trace == false)
                {
                    trace = true;
                    e.Cancel = true;
                }
            };

            ConcurrentQueue<char> rxbuf = new ConcurrentQueue<char>();
            var ue2 = new Ue2();
            ue2.SetMmio(HALT, req =>
            {
                if (req.IsWrite())
                {
                    req.Handled = true;
                    ue2.Halt();
                }
            });
            ue2.SetMmio(RXREADY, req =>
            {
                if (req.IsRead)
                {
                    req.Write((byte)(rxbuf.Count > 0 ? 1 : 0));
                }
            });
            ue2.SetMmio(RX, req =>
            {
                if (req.IsRead)
                {
                    if (rxbuf.TryDequeue(out char result))
                    {
                        req.Write((byte)result);
                    }
                }
            });
            ue2.SetMmio(TX, req =>
            {
                if (req.IsWrite())
                {
                    char c = (char)req.Read();
                    if (c == '\r')
                    {
                        c = '\n';
                    }
                    Console.Write(c);
                }
            });
            if (args.Length > 0)
            {
                path = args[0];
            }
            var labels = new Dictionary<int, string>();
            Load(ue2, labels, path);
            var lastTs = Stopwatch.GetTimestamp();
            while (true)
            {
                if (ue2.IsHalted())
                {
                    Console.WriteLine("Comuter halted.");
                    trace = true;
                }
                if (trace == true)
                {
                    Console.WriteLine();
                    Console.Write("debug$ ");
                    string line = Console.ReadLine();
                    var arg = line?.Split(' ') ?? Array.Empty<string>();
                    if (line == "?")
                    {
                        Console.WriteLine("UE2 Debugger");
                        Console.WriteLine("? - this help");
                        Console.WriteLine("reset - reset computer, reload program from file");
                        Console.WriteLine("load <path> - load program from file");
                        Console.WriteLine("s - execute instruction, print state");
                        Console.WriteLine("c - continue execution");
                        Console.WriteLine("b <hex> - set breakpoint");
                        Console.WriteLine("b <nothex> - remove breakpoint");
                        Console.WriteLine("d <N> - disassemble N lines from PC");
                        Console.WriteLine("r <addr:hex> [<count:integer>] - show content of memory starting at addr");
                        Console.WriteLine();
                        Console.WriteLine("Press Ctrl^C during execution to enter debugger");

                    }
                    else if (line == "reset")
                    {
                        ue2.Reset();
                        Load(ue2, labels, path);
                    }
                    else if (arg.Length > 1 && arg[0] == "load")
                    {
                        path = arg[1];
                        Load(ue2, labels, path);
                    }
                    else if (line == "s")
                    {
                        ue2.Step();
                        Console.WriteLine($"${ue2.pc:X03}: {Disasm(ue2.ir, labels).PadRight(24)} ({GetStateString(ue2)})");
                    }
                    else if (line == "c")
                    {
                        trace = false;
                    }
                    else if (line?.StartsWith("b") == true)
                    {
                        if (arg.Length == 2)
                        {
                            try
                            {
                                breakpoint = Convert.ToInt32(arg[1], 16);
                            }
                            catch
                            {
                                breakpoint = -1;
                                Console.WriteLine("breakpoint removed");
                            }
                        }
                    }
                    else if (line?.StartsWith("d") == true)
                    {
                        if (arg.Length >= 2 && int.TryParse(arg[1], out int count))
                        {
                            for (int i = 0; i < count; i++)
                            {
                                Console.WriteLine($"${ue2.pc + (i * 2):X03}: {Disasm(ue2.Read16(ue2.pc + (i * 2)), labels)}");
                            }
                        }
                    }
                    else if (line?.StartsWith("r") == true)
                    {
                        if (arg.Length >= 2)
                        {
                            int addr = -1;
                            try
                            {
                                addr = Convert.ToInt32(arg[1], 16);
                            }
                            catch
                            {
                                var l = labels.FirstOrDefault(i => i.Value == arg[1]);
                                if (l.Value != null)
                                {
                                    addr = l.Key;
                                }
                                else
                                {
                                    Console.WriteLine("Address or label not found");
                                }
                            }
                            if (addr >= 0)
                            {
                                var value = ue2.Read8(addr);
                                Console.Write($"{addr:X03}: {value:X02}");
                                if (arg.Length >= 3 && int.TryParse(arg[2], out int count))
                                {
                                    int until = addr++ + count;
                                    for (; addr < until; addr++)
                                    {
                                        if ((addr & 0xf) == 0)
                                        {
                                            Console.WriteLine();
                                            Console.Write($"{addr:X03}:");
                                        }
                                        value = ue2.Read8(addr);
                                        Console.Write($" {value:X02}");
                                    }
                                }
                                Console.WriteLine();
                            }
                        }
                    }
                }
                else
                {
                    if (ue2.nextpc == breakpoint)
                    {
                        Console.WriteLine();
                        Console.WriteLine($"break at ${breakpoint:X03}");
                        trace = true;
                        continue;
                    }
                    if (Console.KeyAvailable)
                    {
                        var key = Console.ReadKey(true);
                        var c = char.ToUpper(key.KeyChar);
                        rxbuf.Enqueue(c);
                    }
                    var ts = Stopwatch.GetTimestamp();
                    var elapsed = ts - lastTs;
                    var diff = (elapsed / (double)Stopwatch.Frequency * 1000.0d);
                    //if (diff > 1)
                    {
                        ue2.Step();
                        lastTs = ts;
                    }
                }
            }
        }

        static void Load(Ue2 ue2, Dictionary<int, string> labels, string path, int at = 0)
        {
            if (path == null || File.Exists(path) == false)
            {
                trace = true;
                labels.Clear();
                ue2.Reset();
                if (path == null)
                {
                    Console.WriteLine("Program not loaded. Computer reset.");
                }
                else
                {
                    Console.WriteLine($"File '{path}' not found.");
                }
                return;
            }
            var program = File.ReadAllBytes(path);
            ue2.Load(program, at);
            var labelPath = Path.Combine(
                Path.GetDirectoryName(path),
                Path.GetFileNameWithoutExtension(path) + ".labels");
            labels.Clear();
            if (File.Exists(labelPath))
            {
                var rg = new Regex(@"^.*?([0-9A-F]+)\s+(.*?)$");
                File.ReadAllLines(labelPath)
                    .Select(i => rg.Match(i))
                    .Select(i => (Addr: Convert.ToInt32(i.Groups[1].Value, 16), Name: i.Groups[2].Value.Substring(1)))
                    .ToList()
                    .ForEach(i => labels[i.Addr] = i.Name);
            }
        }

        static string GetStateString(Ue2 ue2)
        {
            return $"A: ${ue2.rra:X02} P: ${ue2.rrp:X03} Z: {(ue2.z ? 1 : 0)}, C: {(ue2.c ? 1 : 0)}";
        }

        static string Disasm(int ir, Dictionary<int, string> labels)
        {
            var op = (OpCode)(ir >> 12);
            var addr = ir & Ue2.PC_MAX;
            string addrStr = $"${addr:X03}";
            if (labels.TryGetValue(addr, out string label))
            {
                addrStr = $"{label}";
            }
            string irStr = $"[${ir:X04}]";
            switch (op)
            {
                case OpCode.SRL:
                case OpCode.INP:
                case OpCode.STP:
                case OpCode.LDP:
                    addrStr = $"{(addr != 0 ? "<invalid>" : "")}";
                    break;
                case OpCode.SCF:
                    var c = (addr & 1) != 0;
                    var z = (addr & 2) != 0;
                    addrStr = $"{(z ? "Z" : "")} {(z & c ? " | " : "")} {(c ? "C" : "")}";
                    break;
            }
            return $"{op.ToString().ToLower().PadRight(4)} {addrStr.PadRight(16)} {irStr}";
        }
    }
}