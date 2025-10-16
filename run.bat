@echo off
echo Starting Haskell Melody Generator...

REM Check if Stack is installed; if not, install it
where stack >nul 2>nul
if %errorlevel% neq 0 (
    echo Stack not found. Downloading and installing...
    powershell -Command "Invoke-WebRequest -Uri 'https://get.haskellstack.org/stable/windows-x86_64-installer.exe' -OutFile 'stack-installer.exe'; Start-Process 'stack-installer.exe' -ArgumentList '/S' -Wait; del stack-installer.exe"
    set PATH=%PATH%;%APPDATA%\local\Programs\stack\x86_64-windows-ghc-9.4.8\bin
)

REM === Setup VirtualMIDISynth ===
echo Setting up MIDI synthesizer (VirtualMIDISynth)...
where VirtualMIDISynth.exe >nul 2>nul
if %errorlevel% neq 0 (
    echo VirtualMIDISynth not found. Downloading and installing...
    powershell -Command "Invoke-WebRequest -Uri 'https://coolsoft.altervista.org/en/virtualmidisynth/files/VirtualMIDISynth.msi' -OutFile 'vms-installer.msi'; msiexec /i 'vms-installer.msi' /quiet /norestart; del vms-installer.msi"
    echo Restart may be required after VMS installation. Please restart and run again.
    pause
    exit /b
)

REM Copy GeneralUser-GS.sf2 to C:\SoundFonts
if not exist "C:\SoundFonts\GeneralUser-GS.sf2" (
    echo Copying GeneralUser-GS.sf2 to C:\SoundFonts...
    mkdir C:\SoundFonts >nul 2>nul
    copy SoundFonts\GeneralUser-GS.sf2 C:\SoundFonts\GeneralUser-GS.sf2 >nul
)

REM Configure VMS via registry to load GeneralUser-GS.sf2
reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth\SoundFonts" /v "0" /t REG_SZ /d "C:\SoundFonts\GeneralUser-GS.sf2" /f >nul 2>nul
reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth" /v "EnableHardwareMixing" /t REG_DWORD /d 1 /f >nul 2>nul
reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth" /v "PreloadSoundfont" /t REG_DWORD /d 1 /f >nul 2>nul

REM Set VMS as default MIDI device
reg add "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Multimedia\MIDIMap" /v "szPname" /t REG_SZ /d "VirtualMIDISynth #1" /f >nul 2>nul

echo MIDI setup complete. If no sound, open VirtualMIDISynth and confirm SF2 is loaded.

REM Update snapshots, setup GHC, and build Haskell project
stack update
stack setup
stack clean --full
stack build

REM Run the program
echo.
echo Running with 5 notes...
stack run -- generateMelody 5
echo.
echo Done! Press any key to exit.
pause >nul