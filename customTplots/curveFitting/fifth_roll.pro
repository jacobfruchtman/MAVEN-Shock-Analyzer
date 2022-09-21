function fifth_roll,dim,aj,bi,yBS

;	b0=bi-dim
;	ypBS=yBS[dim:bi]
	if yBS[max([aj-3*60,dim])] -yBS[dim] gt 10 then begin

		aaj=aj
		for i=0,aj-dim do begin


			if yBS[aj-i] gt yBS[aj] then aaj=aj-i

		endfor
	aj=aaj
	endif
	
	return,aj
end

