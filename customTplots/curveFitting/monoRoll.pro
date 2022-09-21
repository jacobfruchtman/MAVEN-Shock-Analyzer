function monoRoll,aj,bi,dim,mono,Fore
			a2=aj
			monend=mono[aj:bi]
			print,'mono[a2]=',mono[a2]
			if NOT alwaysOn(monend) then begin
				print,'monorolling'
;				while mono[aj] eq 1 do aj++
				while mono[a2] eq 1 and Fore[a2] ne 0 do a2++
				;aj=aj
			endif 
		print,'[before,after]=',[aj,a2]

	return,a2
end
