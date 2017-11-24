@echo off

set    zip=ckongpt2.zip
set ifiles=7.5d+8.5e+9.5h+10.5k+11.5l+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+12.5n+6.11n+5.11l+4.11k+3.11h+2.11c+1.11a+2.11c+1.11a+14.5s+13.5p
set  ofile=a.ckong.rom

rem =====================================
setlocal ENABLEDELAYEDEXPANSION

set pwd=%~dp0
echo.
echo.

if EXIST %zip% (

	!pwd!7za x -otmp %zip%
	if !ERRORLEVEL! EQU 0 ( 
		cd tmp

		copy /b/y %ifiles% !pwd!%ofile%
		if !ERRORLEVEL! EQU 0 ( 
			echo.
			echo ** done **
			echo.
			echo Copy "%ofile%" into root of SD card
		)
		cd !pwd!
		rmdir /s /q tmp
	)

) else (

	echo Error: Cannot find "%zip%" file
	echo.
	echo Put "%zip%", "7za.exe" and "%~nx0" into the same directory
)

echo.
echo.
pause
