#!/usr/bin/env bash
# Build script for DEM_to_UnTRIM (macOS/Linux), equivalent of compile.bat

set -euo pipefail

cd "$(dirname "$0")"

echo ""
echo "========================================"
echo "DEM_to_UnTRIM - Build Script"
echo "========================================"
echo ""

if ! command -v cmake >/dev/null 2>&1; then
    echo "ERROR: CMake is not installed or not in PATH" >&2
    echo "Install it with: brew install cmake" >&2
    exit 1
fi

echo "Checking for Fortran compiler..."
if command -v gfortran >/dev/null 2>&1; then
    FC_COMPILER=gfortran
elif command -v ifort >/dev/null 2>&1; then
    FC_COMPILER=ifort
elif command -v ifx >/dev/null 2>&1; then
    FC_COMPILER=ifx
else
    echo "ERROR: No Fortran compiler found in PATH" >&2
    echo "Install gfortran with: brew install gcc" >&2
    exit 1
fi
echo "- Fortran compiler found: $FC_COMPILER"

GENERATOR_ARGS=()
if command -v ninja >/dev/null 2>&1; then
    GENERATOR_ARGS=(-G Ninja)
fi

echo ""
echo "Configuring project with CMake..."
cmake -S . -B build \
    ${GENERATOR_ARGS[@]+"${GENERATOR_ARGS[@]}"} \
    -DCMAKE_Fortran_COMPILER="$FC_COMPILER" \
    -DCMAKE_BUILD_TYPE=Release

echo ""
echo "Building project..."
cmake --build build --parallel

echo ""
echo "Copying executable to project root..."
cp build/bin/DEM_to_UnTRIM ./DEM_to_UnTRIM

echo ""
echo "========================================"
echo "Build completed successfully!"
echo "========================================"
echo "Executable locations:"
echo "- build/bin/DEM_to_UnTRIM"
echo "- ./DEM_to_UnTRIM"
echo ""
