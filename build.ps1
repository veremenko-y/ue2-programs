$curPath = Get-Location
$env:path += ";$curPath\cc65\"
$programsOutPath = "out"


"# Compiling UE14500 programs"
$programs = Get-ChildItem "src/*.s" | Select-Object -ExpandProperty Name
mkdir -p "${programsOutPath}" 2> $null
foreach ($program in $programs)
{    
    $p = [System.IO.Path]::GetFileNameWithoutExtension($program)
    "Compiling $p"
    Invoke-Expression "ca65 -g src/$p.s -o ${programsOutPath}/$p.o -l ${programsOutPath}/$p.lst --list-bytes 0"
    Invoke-Expression "ld65 -o ${programsOutPath}/$p.bin -Ln ${programsOutPath}/$p.labels -m ${programsOutPath}/$p.map -C sdk/ue2.cfg ${programsOutPath}/$p.o"

    $size = (Get-ChildItem ${programsOutPath}/$p.bin).Length
 
    "Size: {0} bytes" -f ($Size)
}

"DONE"