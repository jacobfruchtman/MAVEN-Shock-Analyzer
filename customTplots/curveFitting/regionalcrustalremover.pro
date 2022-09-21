pro regionalcrustalremover,foot,aj,bi,neh,peh,teh
	bi0=bi
	print,"neh[foot],teh[foot],peh[foot]=",neh[foot],teh[foot],peh[foot]

	pmax=max(peh[foot+1:bi],pmloc)
	nmax=max(neh[foot+1:bi],nmloc)
	tmax=max(teh[foot+1:bi],tmloc)
	frange=findgen(bi-foot-1)+foot+1
	nmloc+=foot+1
;	if pmax lt (aj+bi)/2 then st=pmax else st=foot+1
	st=nmloc
	print,foot,st,bi,pmax
	;hLL=where(finite(peh[st:bi]) eq 1 and finite(neh[st:bi]) eq 1 and finite(teh[st:bi]) eq 1 and neh[foot] lt neh[st:bi] and teh[foot] lt teh[st:bi] and peh[foot] lt peh[st:bi],lcount)
	hLL=where(finite(neh[st:bi]) eq 1  and neh[foot] lt neh[st:bi],lcount)
	;print,"new hLL at time_string(xs[hLL[-1]+foot+1])=timestring(xs["+strtrim(hLL[-1],2)+"+"+strtrim(foot,2)+"+1])=",time_string(xs[hLL[-1]+foot+1])
	;if hLL[-1]+foot+1 gt aj+2*60 and hLL[-1] ne -1 then bi=hLL[-1]+foot+1
	if hLL[-1] ne -1 and hLL[-1]+st gt st+60 and hLL[-1]+st gt aj+60 then bi=hLL[-1]+st
	;print,"new bi at ",time_string(xs[hLL[-1]+foot+1])
	;if 0 and (max(ymm[foot:bi-1],mxlc) eq ymm[bi-1] or mxlc+foot ge bi-20) and B20d[bi-1] gt 0 then begin

	;	GG=where(mono[aj:bi-1] eq 0,gcount)
	;	glast=GG[-1]
	;	if glast ne -1 and glast gt 3*60 then begin
	;		bi=glast
	;		return		

	;	endif
	;	extrlocs=EXTREMA(B20d[aj:bi-1],maxima=maxima,minima=minima)
	;	minima=temporary(minima)+aj
	;	maxima=temporary(maxima)+aj
	;	extrlocs=temporary(extrlocs)+aj

	;	HH=where(ymm[minima] lt ymm[bi-1] )
	;	KK=where(minima lt maxima[-1],kcount)
	;	if B20d[minima[-1]] gt 0 and B20d[minima[-1]] lt B20d[-1] and numel(minima) gt 1 and kcount gt 0 then HH=intersect(temporary(HH),temporary(KK))

	;	bi=minima[HH[-1]]
	;endif
	return
end
