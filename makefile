main.exe:	main.obj math.obj graphics.obj
	tlink main + math + graphics;
main.obj:	main.asm
	tasm main;
math.obj:	math.asm
	tasm math;
graphics.obj:	graphics.asm
	tasm graphics;