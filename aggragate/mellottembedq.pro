pro mellottembedq,xplt,mname,imdems,xcoord,ycoord,ar=ar,dims=dims
if not keyword_set(ar) then ar=2.

dire="Documents/Plots/CombinedPlots/"

	get_data,"ThetaNBn",data=datTH

	Theta=datTH.y

	QPERP=where(Theta ge 45,COMPLEMENT=QPAR)
	
	ALL=where(Theta eq Theta)

	subsets=list(Qperp,Qpar,ALL)

	get_data,xplt,data=datx
	get_data,'overshootAmplitude',data=daty
	overamp=daty.y

	xvals=datx.y
	yvals=overamp
	binsize=datx.binsize[0]
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]
	Title=YT+" vs "+XT
	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	plotFName=yfnm+"_vs_"+xfnm+".png"

	medname='Median Line'
	maxval=max(xvals)
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals ge mlow[zzz] and xvals lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]


	mednamepar='Qpar Median Line'
	maxval=max(xvals[QPAR])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals[QPAR] ge mlow[zzz] and xvals[QPAR] lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals[QPAR] )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlowpar2=mlow[nonzero]
				mhighpar2=mhigh[nonzero]
				medovershootpar2=medovershoot[nonzero]

	mednameperp='Qperp Median Line'
	maxval=max(xvals[QPERP])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals[QPERP] ge mlow[zzz] and xvals[QPERP] lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals[QPERP] )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlowperp2=mlow[nonzero]
				mhighperp2=mhigh[nonzero]
				medovershootperp2=medovershoot[nonzero]


	for ccnt=0,1 do begin
		if ccnt eq 0 then continue
	p1=scatterplot(xvals,yvals,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='lime',xcolor=!color.dark_slate_blue,ycolor=!color.dark_slate_blue)
	p1Qperp=scatterplot(xvals[QPERP],yvals[QPERP],xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='lime',name='Qperp overshoot amplitude',xcolor=!color.dark_slate_blue,ycolor=!color.dark_slate_blue,/over)
	p1Qpar=scatterplot(xvals[QPAR],yvals[QPAR],xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color=!color.medium_orchid,name='Qpar overshoot amplitude',xcolor=!color.dark_slate_blue,ycolor=!color.dark_slate_blue,/over)
	
						
		if ccnt eq 1 then begin
			p2=scatterplot([1.,1.]*xcoord,[1.,1.]*ycoord,SYMBOL='.', /SYM_FILLED, $
					sym_color='black',name="Mellott's 25 Hz res data",/over,sym_size=2)
			medname='Our '+medname
			;p1.name='Our 1Hz res data'
			p1Qperp.name='Our Quasiperp 1Hz res data'
			p1Qpar.name='Our Quasipar 1Hz res data'

		endif		
				medplotperp=plot((mlowperp2+mhighperp2)/2.0, medovershootpar2,color='green',name='Qperp median line',/overplot)
				medplotpar=plot((mlowpar2+mhighpar2)/2.0, medovershootpar2,color='purple',name='Qpar median line',/overplot)
				print,numel(mlowpar2),numel(mhighpar2),numel(medovershootpar2)
				;medplot3=plot((mlowpar2+mhighpar2)/2.0, medovershootpar2,color='red',name=medname,/overplot)
				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color='grey',name=medname,/overplot)

		

	if ccnt eq 0 then	ccf = LEGEND(target=[p1Qperp,p1Qpar,medplotpar,medplotperp,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
	if ccnt eq 1 then begin
		ccf = LEGEND(target=[p2,p1Qperp,p1Qpar,medplotpar,medplotperp,medplot1], font_size=7,transparency=80,/relative,pos=[.95,.95])
	plotFName=yfnm+"_vs_"+xfnm+"_with_Mellott2.png"
		
		;p1.name='Our 1 Hz res data'
		medplot1.name='our median line'
		medplotpar.name='our Qpar median line'
		medplotperp.name='our Qperp median line'
		;pxrange=p1.xrange
		p1Qperp.name='Our Quasiperp 1Hz res data'
		p1Qpar.name='Our Quasipar 1Hz res data'
		print,imdems
	im1=image(mname,/over,$
		image_dimensions=imdems,$
		aspect_ratio=ar)

		ccf.pos=[1.,1.]
	endif 

	;dims=p1.dimensions
	ccf.save,dire+"qparqperp_"+plotFName, BORDER=10, $
  				 RESOLUTION=400
	p1.close
	endfor
	print,"max("+xfnm+")=",max(xvals,maxloc),", at t=",time_string(datx.x[maxloc])
end
