#!/bin/bash
export MONO_PATH=$(pwd)/IntXLib/lib/net20/:$MONO_PATH
echo "rsa_charp.cs => rsa_csharp.exe"
dmcs -lib:./IntXLib/lib/ -r:./IntXLib/lib/net20/IntXLib.dll rsa_csharp.cs
echo "keygen_csharp.cs => keygen_csharp.exe"
dmcs -lib:./IntXLib/lib/ -r:./IntXLib/lib/net20/IntXLib.dll keygen_csharp.cs
