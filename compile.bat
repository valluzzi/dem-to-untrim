@echo off
REM Batch script to compile DEM_to_UnTRIM using CMake and Intel Fortran

setlocal enabledelayedexpansion

echo.
echo ========================================
echo DEM_to_UnTRIM - Build Script
echo ========================================
echo.

REM Check if CMake is installed
where cmake >nul
if !errorlevel! neq 0 (
    echo ERROR: CMake is not installed or not in PATH
    echo Please install CMake from https://cmake.org/download/
    exit /b 1
)

REM Detect and setup Intel Fortran
echo Checking for Fortran compiler...
where ifort >nul
if !errorlevel! equ 0 (
    echo - Intel Fortran Compiler ^(ifort^) found
    goto setup_intel
)

where gfortran >nul
if !errorlevel! equ 0 (
    echo - GNU Fortran Compiler ^(gfortran^) found
    goto setup_mingw
)

echo ERROR: No Fortran compiler found in PATH
echo.
echo Please install one of:
echo 1. Intel oneAPI Fortran: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
echo 2. MinGW/Gfortran: https://www.mingw-w64.org/
echo.
echo After installation, add the compiler bin directory to your PATH and run this script again.
exit /b 1

:setup_intel
REM Setup Intel oneAPI environment
if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
    echo Initializing Intel oneAPI environment...
    call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" >nul 2>&1
) else if exist "C:\Program Files\Intel\oneAPI\setvars.bat" (
    echo Initializing Intel oneAPI environment...
    call "C:\Program Files\Intel\oneAPI\setvars.bat" >nul 2>&1
)
goto configure

:setup_mingw
echo MinGW/Gfortran found, proceeding with build...
goto configure

:configure
REM Create build directory if it doesn't exist
if not exist "build" (
    echo.
    echo Creating build directory...
    mkdir build
)

REM Change to build directory
cd build

REM Run CMake configuration
echo.
echo Configuring project with CMake...
echo.

REM For Intel Fortran, explicitly set the compiler
cmake .. -DCMAKE_Fortran_COMPILER=ifort -G "Ninja" -DCMAKE_BUILD_TYPE=Release 2>nul
if !errorlevel! equ 0 (
    echo Configured with Ninja generator
    goto build
)

cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_BUILD_TYPE=Release 2>nul
if !errorlevel! equ 0 (
    echo Configured with default generator
    goto build
)

cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Release 2>nul
if !errorlevel! equ 0 (
    echo Configured with Ninja generator
    goto build
)

cmake .. -DCMAKE_BUILD_TYPE=Release 2>nul
if !errorlevel! equ 0 (
    echo Configured with default generator
    goto build
)

echo ERROR: CMake configuration failed
cd ..
exit /b 1

:build
REM Build the project
echo.
echo Building project...
cmake --build . --config Release

if !errorlevel! neq 0 (
    echo ERROR: Build failed
    cd ..
    exit /b 1
)

REM Return to root directory
cd ..

REM Copy executable to root directory
echo.
echo Copying executable to project root...
copy build\bin\DEM_to_UnTRIM.exe DEM_to_UnTRIM.exe >nul
if !errorlevel! equ 0 (
    echo Successfully copied DEM_to_UnTRIM.exe to project root
)

echo.
echo ========================================
echo Build completed successfully!
echo ========================================
echo.
echo Executable locations:
echo - build\bin\DEM_to_UnTRIM.exe
echo - .\DEM_to_UnTRIM.exe
echo.

endlocal
exit /b 0

