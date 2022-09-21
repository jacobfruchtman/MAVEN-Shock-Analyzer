pro lsmaxcorrtest,invdt=invdt,xmxs=xmxs,ymxs=ymxs
	get_data,'Ls',data=datLS
	get_data,'Ls',data=datD
	plt='test'
	Dy=datD.y
	N=numel(Dy)
	rounds=1000
	Xmxs=fltarr(rounds)
	Ymxs=fltarr(rounds)
	LS=datLS.y

	for kk=0,rounds-1 do begin
		Dy=randomu(seed,N)

				;LS=randomu(seed2,N)*360.
		;p11=scatterplot(LS,Dy)


	if not keyword_set(invdt) then	invdt=1
	DEGRANGE=findgen(360*invdt)/invdt
	rCORRS=fltarr(360*invdt)
	
	for i=0,360*invdt-1 do begin
		ofst=DEGRANGE[i]
		dustdist=180-Abs(180-abs(ofst-LS))
		r=correlate(dustdist,Dy)
		if keyword_set(rank) then r=R_CORRELATE(dustdist,Dy,KENDALL=KENDALL,zd=zd,probd=probd)
		rCORRS[i]=r[0]
	endfor
	mincorr=min(rCORRS,mnlc) ;;r should be a negative value
	maxcorr=max(rCORRS,mxlc) ;;r should be a negative value
	mxofst=DEGRANGE[mxlc]
	mnofst=DEGRANGE[mnlc]

	Xmxs[kk]=mxofst
	Ymxs[kk]=maxcorr
	endfor
	
	
	

	p33=scatterplot(Xmxs,Ymxs)
	h_enso = HISTOGRAM(Xmxs, BINSIZE=5, LOCATIONS=binvals)
	histoplot = PLOT(binvals,h_enso,/hist)

	;p33.save,'Documents/correlation_of_'+plt+'_and_variable_offset.png'
	;p33.close
	;p1=scatterplot(xdustdist,Dy,xtitle='xdustdist=180-Abs(180-abs('+strtrim(mxofst,2)+'-LS))')
	;p2=scatterplot(ndustdist,Dy,xtitle='ndustdist=180-Abs(180-abs('+strtrim(mnofst,2)+'-LS))')
end
