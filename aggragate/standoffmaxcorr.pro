pro standoffmaxcorr
	get_data,'Ls',data=datLS
	get_data,'lmavenNorm',data=datD
	
	Dy=datD.y
	LS=datLS.y
	DEGRANGE=findgen(360*2)/2.
	rCORRS=fltarr(720)
	
	for i=0,720-1 do begin
		ofst=DEGRANGE[i]
		dustdist=180-Abs(180-abs(ofst-LS))
		r=correlate(dustdist,Dy)
		rCORRS[i]=r
	endfor
	mincorr=min(rCORRS,mnlc) ;;r should be a negative value
	maxcorr=max(rCORRS,mxlc) ;;r should be a negative value
	mxofst=DEGRANGE[mxlc]
	mnofst=DEGRANGE[mnlc]
	print,"mxofst,maxcorr=",mxofst,maxcorr
	print,"mnofst,mincorr=",mnofst,mincorr
	xdustdist=180-Abs(180-abs(mxofst-LS))
	ndustdist=180-Abs(180-abs(mnofst-LS))
	p1=scatterplot(xdustdist,Dy,xtitle='xdustdist=180-Abs(180-abs('+strtrim(mxofst,2)+'-LS))')
	p2=scatterplot(ndustdist,Dy,xtitle='ndustdist=180-Abs(180-abs('+strtrim(mnofst,2)+'-LS))')
end
