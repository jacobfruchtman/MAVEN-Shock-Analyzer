function extremaroll,dim,aj,bi,extr,rhod,Fore
	maxdisplace=15*60
	maxdist=6*60
	if dim gt 0 then bhalf=mean([dim,aj]) else bhalf=0
	;print,numel(extr)-bi
	;print,numel(extr)-bhalf
	extrp=extr[bhalf:bi]
	indxs=findgen(bi-bhalf)+bhalf
	GGp=where(extrp ne 0 and Fore[bhalf:bi] eq 1,gcount)

	if gcount le 0 then return, aj


	GG=GGp+bhalf
	exG=extrp[GGp]
	aj0=aj
	el0=-1

	print,"aj0=",aj0
	for el=0, gcount-1 do begin
		if exG[el] lt 0 then continue
		aj0=GG[el]
		el0=el
		
	endfor
	print,"aj0=",aj0
	if aj eq aj0 then return,aj
	aj00=aj0
	aj0n=aj0
	naj0n=aj0
	if gcount gt 1 then begin
		el22=el0
		for el2=el0+1,gcount-1 do begin


			if exG[el2] lt 0 then break


			if GG[el2]-aj00 gt maxdisplace then break


			if rhod[el2] gt 0.0 then begin
				aj0=GG[el2]
				el22=el2
			endif
		endfor

		for el2=el22+1,gcount-1 do begin

			r=el2-el22
	
			sn=(-1)^r
			tsn=sign(exG[el2])
			if sn ne tsn then break
			if naj0n-aj0 gt maxdist then break
			if GG[el2]-aj00 gt maxdisplace then break

			if tsn eq -1 then naj0n=GG[el2]
			if tsn eq 1 then aj0=GG[el2]
			;
		endfor
	endif
	print,"aj0=",aj0
	return, aj0
end
