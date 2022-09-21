pro lsmaxcorr,plt,invdt=invdt,rank=rank,KENDALL=KENDALL,subset=subset
	get_data,'Ls',data=datLS
	get_data,plt,data=datD
	tplot_element,8,'y',Mfms
	ww=where(Mfms gt 4)
	Dy=datD.y[ww]
	LS=datLS.y[ww]
	
	if keyword_set(subset) then begin
		get_data,subset,data=datsub
		subflag=datsub.y
		sub=where(subflag eq 1)		
		Dy=Dy[sub]
		LS=LS[sub]
	endif
	if not keyword_set(invdt) then	invdt=1
	DEGRANGE=findgen(360*invdt)/invdt
	if ~keyword_set(rank) then begin
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
	;print,plt
	print,"using LSdist=180-Abs(180-abs(LS-LS_Offset)), then calculating correlation with "+plt
	if keyword_set(rank) then begin
		if keyword_set(KENDALL) then print,'using kendall rank correlation' else print,'using spearman rank correlation'
	endif else print,'using pearson coorelation'
		if keyword_Set(sub) then print,'for the subset:'+subset
	print,'find:'
	print,"maxcorr=",maxcorr,'at LS_Offset=',mxofst;"mxofst,maxcorr=",mxofst,maxcorr
	print,"mincorr=",mincorr,'at LS_Offset=',mnofst
	;print,;"mnofst,mincorr=",mnofst,mincorr
	xdustdist=180-Abs(180-abs(mxofst-LS))
	ndustdist=180-Abs(180-abs(mnofst-LS))
	endif else begin
	rCORRS=fltarr(360*invdt,4)
	
	for i=0,360*invdt-1 do begin
		ofst=DEGRANGE[i]
		dustdist=180-Abs(180-abs(ofst-LS))
		r=correlate(dustdist,Dy)
		if keyword_set(rank) then r=R_CORRELATE(dustdist,Dy,KENDALL=KENDALL,zd=zd,probd=probd)
		rCORRS[i,0]=r[0]
		rCORRS[i,1]=r[1]
		rCORRS[i,2]=zd
		rCORRS[i,3]=probd
	endfor
	
	mincorr=min(rCORRS[*,0],mnlc) ;;r should be a negative value
	maxcorr=max(rCORRS[*,0],mxlc) ;;r should be a negative value
	mxofst=DEGRANGE[mxlc]
	mnofst=DEGRANGE[mnlc]
	mxzd=rCORRS[mxlc,2]
	mnzd=rCORRS[mnlc,2]
	mxprobd=rCORRS[mxlc,3]
	mnprobd=rCORRS[mnlc,3]
	;print,plt
	print,"using LSdist=180-Abs(180-abs(LS-LS_Offset)), then calculating correlation with "+plt
	if keyword_set(rank) then begin
		if keyword_set(KENDALL) then print,'using kendall rank correlation' else print,'using spearman rank correlation'
	endif else print,'using pearson coorelation'
		if keyword_Set(sub) then print,'for the subset:'+subset
	print,'find:'
	print,"maxcorr=",maxcorr,'at LS_Offset=',mxofst,", ";"mxofst,maxcorr=",mxofst,maxcorr
	print,"mincorr=",mincorr,'at LS_Offset=',mnofst
	;print,;"mnofst,mincorr=",mnofst,mincorr
	;xdustdist=180-Abs(180-abs(mxofst-LS))
	;ndustdist=180-Abs(180-abs(mnofst-LS))
	
	
	endelse

	dustdist=180-Abs(180-abs(mxofst-LS))
	p22=scatterplot(dustdist,Dy)
	
	p33=scatterplot(findgen(360*invdt)/invdt, rCorrs[*,0],xtitle='$L_S$ offset [deg]',ytitle='r('+plt+',$\Delta L_S from offset$)')
	p33.save,'Documents/correlation_of_'+plt+'_and_variable_offset.png'
	p33.close
	;p1=scatterplot(xdustdist,Dy,xtitle='xdustdist=180-Abs(180-abs('+strtrim(mxofst,2)+'-LS))')
	;p2=scatterplot(ndustdist,Dy,xtitle='ndustdist=180-Abs(180-abs('+strtrim(mnofst,2)+'-LS))')
end
