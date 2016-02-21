#!/bin/bash
echo "read.cs => read.exe"
dmcs -lib:./IntXLib/lib/ -r:./IntXLib/lib/net20/IntXLib.dll read.cs
echo "gen.cs => gen.exe"
dmcs -lib:./IntXLib/lib/ -r:./IntXLib/lib/net20/IntXLib.dll gen.cs
