#!/bin/bash
curPath=$(pwd)
outPath="$curPath/out"

for program in src/*.s; do
    p=$(basename -- "$program")
    p="${p%.*}"
    echo "Compiling $p"

    ca65 -g src/$p.s -o ${outPath}/$p.o -l ${outPath}/$p.lst --list-bytes 0
    ld65 -o ${outPath}/$p.bin -Ln ${outPath}/$p.labels -m ${outPath}/$p.map -C sdk/ue2.cfg ${outPath}/$p.o

    size=$(stat --printf="%s" $outPath/$p.bin)
    echo -e "\tSize: $size bytes"
done
echo "DONE"