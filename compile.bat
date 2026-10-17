@echo off
REM Batch script to compile DEM_to_UnTRIM using CMake and Intel Fortran

setlocal enabledelayedexpansion

echo.
echo ========================================
echo DEM_to_UnTRIM - Build Script
echo ========================================
echo.

REM Check if CMake is installed
where cmake >nul 2>&1
if !errorlevel! neq 0 (
    echo ERROR: CMake is not installed or not in PATH
    echo Please install CMake from https://cmake.org/download/
    exit /b 1
)

REM Detect and setup Intel Fortran
echo Checking for Fortran compiler...

REM ifort on Windows needs the MSVC linker and rc from VS Build Tools, so load them before oneAPI
if exist "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat" (
    call "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat" >nul 2>&1
)

REM Load the Intel oneAPI environment so ifort is on PATH if installed
if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
    echo Initializing Intel oneAPI environment...
    call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" >nul 2>&1
) else if exist "C:\Program Files\Intel\oneAPI\setvars.bat" (
    echo Initializing Intel oneAPI environment...
    call "C:\Program Files\Intel\oneAPI\setvars.bat" >nul 2>&1
)

REM Fallback in case setvars did not put ifort on PATH
where ifort >nul 2>&1
if !errorlevel! neq 0 if exist "C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin\ifort.exe" (
    set "PATH=C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin;!PATH!"
)

where ifort >nul 2>&1
if !errorlevel! equ 0 (
    echo - Intel Fortran Compiler ^(ifort^) found
    goto setup_intel
)

where gfortran >nul 2>&1
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
REM Ninja bundled with VS Build Tools, the generator that works with ifort
if exist "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\Common7\IDE\CommonExtensions\Microsoft\CMake\Ninja\ninja.exe" (
    set "PATH=C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\Common7\IDE\CommonExtensions\Microsoft\CMake\Ninja;!PATH!"
)
REM CMake only identifies the Intel compiler when its test source is preprocessed
set "FC_FLAGS=/fpp"
for /f "delims=" %%i in ('where ifort') do set "FC_PATH=%%i"
set "FC_PATH=!FC_PATH:\=/!"
goto configure

:setup_mingw
set "FC_FLAGS="
for /f "delims=" %%i in ('where gfortran') do set "FC_PATH=%%i"
set "FC_PATH=!FC_PATH:\=/!"
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

REM Use Ninja when installed, otherwise NMake (the VS generator needs devenv, which Build Tools lacks)
set "GEN=NMake Makefiles"
where ninja >nul 2>&1 && set "GEN=Ninja"
cmake .. -G "!GEN!" -DCMAKE_Fortran_COMPILER="%FC_PATH%" -DCMAKE_Fortran_FLAGS="%FC_FLAGS%" -DCMAKE_BUILD_TYPE=Release
if !errorlevel! neq 0 (
    echo ERROR: CMake configuration failed
    cd ..
    exit /b 1
)
echo Configured with !GEN! generator

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

