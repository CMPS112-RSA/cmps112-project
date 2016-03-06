#!/bin/bash

export MONO_PATH=$PWD/csharp/IntXLib/lib/net20/:$MONO_PATH
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

echo "Building C implementation."
mkdir -p c/build
cd c/build
cmake ..
make
cd ../..

echo "Building C# implementation."
cd csharp
./build.sh
cd ..

echo "Building Haskell implementation."
cd haskell
make
cd ..
