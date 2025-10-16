@echo off
echo Starting Haskell Melody Generator...

REM Проверяем, установлен ли Stack. Если нет — скачиваем и устанавливаем
where stack >nul 2>nul
if %errorlevel% neq 0 (
    echo Stack not found. Downloading and installing...
    powershell -Command "Invoke-WebRequest -Uri 'https://get.haskellstack.org/stable/windows-x86_64-installer.exe' -OutFile 'stack-installer.exe'; Start-Process 'stack-installer.exe' -ArgumentList '/S' -Wait; del stack-installer.exe"
    set PATH=%PATH%;%APPDATA%\local\Programs\stack\x86_64-windows-ghc-9.4.5\bin
)

REM Обновляем зависимости и строим проект
stack setup
stack build

REM Запускаем программу с примерами
echo.
echo Running with 5 notes...
stack run -- generateMelody 5
echo.
echo Done! Press any key to exit.
pause >nul