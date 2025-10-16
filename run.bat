@echo off
echo Starting Haskell Melody Generator...

REM Check if Stack is installed; if not, install it
echo Checking for Stack...
where stack >nul 2>nul
if %errorlevel% neq 0 (
    echo Stack not found. Downloading and installing...
    powershell -Command "Invoke-WebRequest -Uri 'https://get.haskellstack.org/stable/windows-x86_64-installer.exe' -OutFile 'stack-installer.exe'; Start-Process 'stack-installer.exe' -ArgumentList '/S' -Wait; del stack-installer.exe"
    if errorlevel 1 (
        echo Error: Failed to install Stack. Please install manually from https://get.haskellstack.org/ and run again.
        pause
        exit /b
    )
    set "PATH=%PATH%;%APPDATA%\local\Programs\stack\x86_64-windows-ghc-9.4.8\bin"
) else (
    echo Stack found. Continuing...
)

REM === Setup VirtualMIDISynth ===
echo Setting up MIDI synthesizer (VirtualMIDISynth)...
set "VMS_INSTALLED=false"

REM Check for VMS in your installation path
echo Checking C:\Program Files\VirtualMIDISynth...
if exist "C:\Program Files\VirtualMIDISynth\VirtualMIDISynth.exe" (
    set "VMS_INSTALLED=true"
    echo VirtualMIDISynth found in C:\Program Files\VirtualMIDISynth.
) else (
    echo Not found in C:\Program Files\VirtualMIDISynth.
)

REM Check in PATH for VirtualMIDISynth.exe
echo Checking PATH for VirtualMIDISynth.exe...
where VirtualMIDISynth.exe >nul 2>nul
if %errorlevel% == 0 (
    set "VMS_INSTALLED=true"
    echo VirtualMIDISynth.exe found in PATH.
)

if "%VMS_INSTALLED%"=="true" (
    echo VirtualMIDISynth is already installed. Skipping installation...
) else (
    echo VirtualMIDISynth not found. Downloading and installing...
    powershell -Command "$url = 'https://coolsoft.altervista.org/download/CoolSoft_VirtualMIDISynth_2.13.9.exe?tckt=1760637392'; try { Invoke-WebRequest -Uri $url -OutFile 'vms-installer.exe'; Start-Process 'vms-installer.exe' -ArgumentList '/S' -Wait; del 'vms-installer.exe' } catch { Write-Host 'Download failed. https://coolsoft.altervista.org/download/CoolSoft_VirtualMIDISynth_2.13.9.exe?tckt=1760637392'; pause }"
    if errorlevel 1 (
        echo Installation failed. Please install VirtualMIDISynth manually from https://coolsoft.altervista.org/en/download/CoolSoft_VirtualMIDISynth_2.13.9.exe and run again.
        pause
        exit /b
    )
    echo Restart may be required after VMS installation. Please restart and run again if needed.
    pause
)

REM Copy or verify GeneralUser-GS.sf2 to C:\SoundFonts\GeneralUser-GS
echo Verifying GeneralUser-GS.sf2 in C:\SoundFonts\GeneralUser-GS...
if not exist "C:\SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2" (
    mkdir C:\SoundFonts\GeneralUser-GS >nul 2>nul
    echo Checking for GeneralUser-GS.sf2 in project: SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2
    if exist "SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2" (
        copy "SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2" "C:\SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2" >nul
        if errorlevel 1 (
            echo Warning: Failed to copy GeneralUser-GS.sf2. Please copy manually to C:\SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2.
            pause
        ) else (
            echo GeneralUser-GS.sf2 copied successfully.
        )
    ) else (
        echo Warning: GeneralUser-GS.sf2 not found in SoundFonts\GeneralUser-GS folder. Please copy it to C:\SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2 or download from http://www.schristiancollins.com/generaluser.php.
        pause
        exit /b
    )
) else (
    echo GeneralUser-GS.sf2 already exists in C:\SoundFonts\GeneralUser-GS.
)

REM Configure VMS via registry to load GeneralUser-GS.sf2 (if VMS installed)
if "%VMS_INSTALLED%"=="true" (
    echo Configuring VirtualMIDISynth...
    reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth\SoundFonts" /v "0" /t REG_SZ /d "C:\SoundFonts\GeneralUser-GS\GeneralUser-GS.sf2" /f >nul 2>nul
    reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth" /v "EnableHardwareMixing" /t REG_DWORD /d 1 /f >nul 2>nul
    reg add "HKEY_CURRENT_USER\Software\CoolSoft\VirtualMIDISynth" /v "PreloadSoundfont" /t REG_DWORD /d 1 /f >nul 2>nul
    reg add "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Multimedia\MIDIMap" /v "Default" /t REG_SZ /d "VirtualMIDISynth" /f >nul 2>nul
    if errorlevel 1 (
        echo Warning: Failed to configure VirtualMIDISynth registry. Please configure manually.Open VirtualMIDISynth, open SoundFonds tab. Press "+" and navigate to the folder where this program is stored.
        echo  Open SoundFonts foulder then open GeneralUser-GS foulder and press to the file named GeneralUser-GS.sf2. Press button "open". And button "ok". All done
        pause
    ) else (
        echo VMS configuration updated.
    )
) else (
    echo Warning: VirtualMIDISynth not installed. MIDI configuration skipped.
    pause
)

echo MIDI setup complete. If no sound, open VirtualMIDISynth and confirm SF2 is loaded.

REM Update snapshots, setup GHC, and build Haskell project
echo Building Haskell project...
stack update
stack setup
stack clean --full
stack build
if errorlevel 1 (
    echo Build failed. Check errors above and resolve issues.
    pause
    exit /b
)

REM Request number of notes from user

echo ===============================================================================================================
echo ===============================================Enter number====================================================
echo ===============================================================================================================
set /p number=Enter the number of notes (e.g., 5): 
if "%number%"=="" (
    echo Error: No input. Using default value. Will be played 5.
    set "number=5"
)

REM Run the program with the specified number of notes
echo Running with %number% notes...
stack run -- generateMelody %number%
if errorlevel 1 (
    echo Run failed. Check errors above and resolve issues.
    pause
    exit /b
)

echo Done! Program completed successfully.
pause
