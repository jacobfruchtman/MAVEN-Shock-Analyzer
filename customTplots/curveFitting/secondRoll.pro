function secondRoll,aj,bi,hill,dobug
	if dobug gt 1 then print,'numel(hill)=',numel(hill),', bi=',bi
	if dobug gt 1 then print, 'aj=',aj
	;hills=hill[0:bi]
	N=numel(hill)
	ball=aj


	peakHeight=hill[ball]
	
	peakloc=aj
	lasth=0
	lastloc=aj
	highestHeight=peakHeight
	highestloc=aj
	
	onPeak=0
	peakStart=aj
	peakend=aj
	for i=aj, bi-1 do begin
		;if i ge bi then break
		if (hill[i-1] lt hill[i]) and (hill[i+1] le hill[i]) then begin
			onPeak=1
			peakStart=i
		endif
	
		if ((hill[i-1] le hill[i]) and (hill[i+1] lt hill[i])) and onPeak then begin
			onPeak=0
			peakEnd=i
			peakloc=mean([peakStart,peakEnd])
			peakHeight=hill[i]

			if peakHeight gt highestHeight then begin
				highestHeight=peakHeight
				highestLoc=peakloc
			endif

			if peakHeight lt highestHeight then return,highestloc ; we keep rolling up and down peaks until we reach one higher than the next. That one should be the midpoint of the jump
		endif
		

	endfor
	return, aj

end
