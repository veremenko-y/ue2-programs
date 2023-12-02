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
        static bool openDebugger = false;
        static int breakpoint = -1;
        static int speed = 50_000;
        static bool isContinue = true;
        static string path = null;
        static int instrCount = 0;
        static Ue2 ue2;
        static List<(string label, int addr)> labels = new List<(string label, int addr)>();
        static Queue<char> rxbuf = new Queue<char>();
        static int elapsedInstr = 0;
        static Stopwatch totalElapsed = new Stopwatch();
        // static List<string> dbgHistory = new List<string>();
        // static int dbgHistoryIndex = -1;
        // static bool dbgHistoryEnabled = true;


        static void Main(string[] args)
        {
            Console.CancelKeyPress += (sender, e) =>
            {
                if (trace == false)
                {
                    openDebugger = true;
                    e.Cancel = true;
                }
            };

            ue2 = new Ue2();
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
                    if (rxbuf.Count > 0 && rxbuf.TryDequeue(out char result))
                    {
                        req.Write((byte)result);
                    }
                    else
                    {
                        req.Write(0);
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
            Load(path);
            var lastTs = Stopwatch.GetTimestamp();
            totalElapsed.Start();
            while (true)
            {
                if (ue2.IsHalted())
                {
                    Console.WriteLine("Comuter halted.");
                    trace = true;
                }
                if (openDebugger == true)
                {
                    openDebugger = false;
                    WriteTrace();
                    trace = true;
                }
                if (trace == true)
                {
                    Console.WriteLine();
                    Console.Write("debug$ ");
                    string line = Console.ReadLine();
                    ExecuteDebugger(line);
                }
                else
                {
                    if (ue2.nextpc == breakpoint && isContinue == false)
                    {
                        totalElapsed.Stop();
                        Console.WriteLine();
                        Console.WriteLine($"break at ${breakpoint:X03}");
                        WriteTrace();

                        trace = true;
                        continue;
                    }
                    if (isContinue == true)
                    {
                        elapsedInstr = 0;
                        totalElapsed.Restart();
                        isContinue = false;
                    }
                    if (Console.KeyAvailable)
                    {
                        var key = Console.ReadKey(true);
                        var c = char.ToUpper(key.KeyChar);
                        rxbuf.Enqueue(c);
                    }
                    var ts = Stopwatch.GetTimestamp();
                    var elapsed = ts - lastTs;
                    var diff = elapsed / (double)Stopwatch.Frequency * speed;
                    if (diff > 1 || speed < 0)
                    {
                        ue2.Step();
                        instrCount++;
                        elapsedInstr++;
                        lastTs = ts;
                    }
                }
            }
        }

        static void ExecuteDebugger(string line)
        {
            var arg = line?.Split(' ') ?? Array.Empty<string>();
            if (line == "?")
            {
                Console.WriteLine("UE2 Debugger");
                Console.WriteLine("? - this help");
                Console.WriteLine("reset - reset computer, reload program from file");
                Console.WriteLine("load <path> - load program from file");
                Console.WriteLine("s | Enter - execute instruction, print state");
                Console.WriteLine("c - continue execution");
                Console.WriteLine("b <hex|label> - set breakpoint");
                Console.WriteLine("b <nothex> - remove breakpoint");
                Console.WriteLine("d [<hex|label]] <N> - disassemble N lines from PC or address");
                Console.WriteLine("speed <N> - set speed of emulator in HZ");
                Console.WriteLine("r <addr:hex> [<count:integer>] - show content of memory starting at addr");
                Console.WriteLine();
                Console.WriteLine("Press Ctrl^C during execution to enter debugger");

            }
            else if (arg[0] == "echo")
            {
                Console.WriteLine(string.Join(' ', arg.Skip(1)));
            }
            else if (line == "reset")
            {
                Load(path);
                Console.WriteLine($"Reloaded `{path}`.");
            }
            else if (arg.Length > 1 && arg[0] == "load")
            {
                path = arg[1];
                Load(path);
            }
            else if (line == "" || line == "s")
            {
                ue2.Step();
                instrCount++;
                WriteTrace();
            }
            else if (arg[0] == "c")
            {
                trace = false;
                isContinue = true;
                if (arg.Length > 1)
                {
                    breakpoint = ue2.pc + 2;
                }
            }
            else if (arg[0] == "cn")
            {
                trace = false;
                breakpoint = breakpoint + 2;
                 ue2.Step();
                instrCount++;
                WriteTrace();
            }
            else if (arg[0] == "speed")
            {
                if (arg.Length == 2)
                {
                    if (!int.TryParse(arg[1], out speed))
                    {
                        speed = -1;
                        Console.WriteLine("Running at max speed");
                    }
                    else
                    {
                        Console.WriteLine($"Set speed to {speed} Hz");
                    }
                }
                else
                {
                    Console.WriteLine($"Current speed: {speed} Hz");
                }
            }
            else if (line?.StartsWith("b") == true)
            {
                if (arg.Length == 2)
                {
                    int addr = ParseAddrOrLabel(arg[1]);
                    if (addr >= 0)
                    {
                        breakpoint = addr;
                        Console.WriteLine($"breakpoint set at ${breakpoint:X03}");
                    }
                    else
                    {
                        breakpoint = -1;
                        Console.WriteLine("breakpoint removed");
                    }
                }
                else
                {
                    Console.WriteLine($"breakpoint set at ${breakpoint:X03}");
                }
            }
            else if (line?.StartsWith("d") == true)
            {
                int start = ue2.pc;
                int count = 0;
                bool success = true;
                if (arg.Length == 2)
                {
                    success = int.TryParse(arg[1], out count);
                }
                else if (arg.Length == 3)
                {
                    start = ParseAddrOrLabel(arg[1]);
                    success = int.TryParse(arg[2], out count) & start >= 0;
                }
                if (success)
                {
                    for (int i = 0; i < count; i++)
                    {
                        Console.WriteLine($"${start + (i * 2):X03}: {Disasm(ue2.Read16(start + (i * 2)))}");
                    }
                }
                else
                {
                    Console.WriteLine("Invalid arguments.");
                }
            }
            else if (arg[0] == "rx")
            {
                if (arg.Length == 2)
                {
                    rxbuf.Enqueue(arg[1][0]);
                }
            }
            else if (line?.StartsWith("r") == true)
            {
                if (arg.Length >= 2)
                {
                    int addr = ParseAddrOrLabel(arg[1]);
                    if (addr < 0)
                    {
                        Console.WriteLine("Address or label not found");
                    }
                    else
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
            else
            {
                var dbgScript = Path.Combine(
                    Path.GetDirectoryName(path),
                    arg[0] + ".dbgs");
                if (File.Exists(dbgScript))
                {
                    var lines = File.ReadAllLines(dbgScript);
                    foreach (var l in lines)
                    {
                        ExecuteDebugger(l);
                    }
                }
            }
        }

        static void WriteTrace()
        {
            Console.WriteLine($"${ue2.pc:X03}: {Disasm(ue2.ir).PadRight(24)} " +
                $"({GetStateString(ue2)})\t" +
                $"[{instrCount} cycles; since last break {totalElapsed.ElapsedMilliseconds / 1000.0:0.000} " +
                $"seconds or {elapsedInstr} cycles or " +
                $"{ToHzString((double)elapsedInstr / (totalElapsed.ElapsedMilliseconds / 1000.0))}]");
        }

        static string ToHzString(double value)
        {
            if (value > 1000)
            {
                return $"{value / 1000:0.00} kHz";
            }
            else if (value > 1000_000)
            {
                return $"{value / 1000_000:0.00} MHz";
            }
            else
            {
                return $"{value:0.00} Hz";
            }
        }

        static int ParseAddrOrLabel(string value)
        {
            int addr = -1;
            try
            {
                addr = Convert.ToInt32(value, 16);
            }
            catch
            {
                var l = labels.FirstOrDefault(i => i.label == value);
                if (l.label != null)
                {
                    addr = l.addr;
                }
            }
            return addr;
        }

        static void Load(string path, int at = 0)
        {
            labels.Clear();
            instrCount = 0;
            elapsedInstr = 0;
            totalElapsed.Reset();
            rxbuf.Clear();
            ue2.Reset();
            if (path == null || File.Exists(path) == false)
            {
                trace = true;

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
            if (File.Exists(labelPath))
            {
                var rg = new Regex(@"^.*?([0-9A-F]+)\s+(.*?)$");
                labels = File.ReadAllLines(labelPath)
                    .Select(i => rg.Match(i))
                    .Select(i => (
                        Name: i.Groups[2].Value.Substring(1),
                        Convert.ToInt32(i.Groups[1].Value, 16)))
                    .ToList();
            }
        }

        static string GetStateString(Ue2 ue2)
        {
            return $"A: ${ue2.rra:X02} P: ${ue2.rrp:X03} Z: {(ue2.z ? 1 : 0)}, C: {(ue2.c ? 1 : 0)}";
        }

        static string Disasm(int ir)
        {
            var op = (OpCode)(ir >> 12);
            var addr = ir & Ue2.PC_MAX;
            string addrStr = $"${addr:X03}";
            var label = labels.FirstOrDefault(i => i.addr == addr);
            if (label.label != null)
            {
                addrStr = $"{label.label}";
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