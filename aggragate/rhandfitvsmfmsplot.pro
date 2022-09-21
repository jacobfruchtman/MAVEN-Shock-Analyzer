pro rhandfitvsmfmsplot
	yplt='overshootAmplitude'
	imdems=[6.005,2.5+.5*76/125.]
	xplt='FM2'
	mname='Documents/MELLOTT_A23.png'
	ar=2.
	xcoord=.7
	ycoord=.05
	;return
	shockaggragateplotter,xplt,yplt,$
	domedian=5,ar=ar,dims=dims
	mellottembed,xplt,mname,imdems,xcoord,ycoord,ar=ar,dims=dims
	mellottembedQ,xplt,mname,imdems,xcoord,ycoord,ar=ar,dims=dims
	

	xcoord=.52
	ycoord=.099
	mname='Documents/MellotBetaEtrans2.png'
	xplt='beta_electron'
	ar=2.
	imdems=[7+118./117.9,2.5]
shockaggragateplotter,xplt,yplt,$
		domedian=5,ar=ar,dims=dims
	mellottembed,xplt,mname,imdems,xcoord,ycoord,ar=ar
	mellottembedQ,xplt,mname,imdems,xcoord,ycoord,ar=ar
	

	xplt='MachAlfven'
	imdems=[14+2*112./117.7,2.5+.5*36/142.]
	mname='Documents/MellotAlfvenTrans3.png'
	ar=5.
	xcoord=2.
	ycoord=.01
shockaggragateplotter,xplt,yplt,$
		domedian=5,ar=ar,dims=dims
	mellottembed,xplt,mname,imdems,xcoord,ycoord,ar=ar
	mellottembedQ,xplt,mname,imdems,xcoord,ycoord,ar=ar
	
	;return
	dire="Documents/Plots/CombinedPlots/"


	get_data,'overshootAmplitude',data=daty
	overamp=daty.y
	yfnm=daty.fn[0]
	yvals=overamp
	yttl=daty.ytitle[0]
	YT=daty.YN[0]
	if 0 then begin
	get_data,'beta_electron',data=datx
	FM=datx.y
	


	xvals=FM

	binsize=datx.binsize[0]
	XT=datx.YN[0]


	xttl=datx.ytitle[0]
	Title=YT+" vs "+XT

	xfnm=datx.fn[0]
	plotFName=yfnm+"_vs_"+xfnm+".png"

	medname='Median Line'
maxval=max(xvals)
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	;[7+118./117.9,2.5]
 					 w = where(xvals ge mlow[zzz] and xvals lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
	for ccnt=0,1 do begin
		if ccnt eq 0 then continue
	p1=scatterplot(xvals,yvals,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='lime',name='overshoot amplitude',xcolor=!color.dark_slate_blue,ycolor=!color.dark_slate_blue)
	;p2=scatterplot(2.,0.01,SYMBOL='.', /SYM_FILLED, $
						
		if ccnt eq 1 then begin
			p2=scatterplot([.52,.52],[.099,0.099],SYMBOL='.', /SYM_FILLED, $
					sym_color='black',name="Mellott's 25 Hz res data",/over,sym_size=2)
			medname='Our '+medname
			p1.name='Our 1Hz res data'
		endif		
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color='green',name=medname,/overplot)

		

	if ccnt eq 0 then	ccf = LEGEND(target=[p1,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
	if ccnt eq 1 then begin
		ccf = LEGEND(target=[p2,p1,medplot1], font_size=7,transparency=80,/relative,pos=[.95,.95])
	plotFName=yfnm+"_vs_"+xfnm+"_with_Mellott.png"
		
		p1.name='Our 1 Hz res data'
		medplot1.name='our median line'
		
	f3name='Documents/MellotBetaEtrans.png'
	im1=image(f3name,/over,$
		image_dimensions=[7+118./117.9,2.5],$;[14+2*112./117.7,2.0+.5*201/142.],$;[16,2.5+.5*36/142.],$
		aspect_ratio=2.)
	;p2=scatterplot(2.,0.01,SYMBOL='.', /SYM_FILLED, $
	;					sym_color='black',name="Mellott's 25 Hz res data",sym_size=2.5*p1.sym_size,/over)
		;ccf = LEGEND(target=[p1,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
		ccf.pos=[1.,1.]
	endif 
	ccf.save,dire+"green_"+plotFName, BORDER=10, $
  				 RESOLUTION=400
	p1.close
	endfor
	return

	

	;;;;;;;;;;;ALFVEN



	get_data,'MachAlfven',data=datx
	get_data,'overshootAmplitude',data=daty
	FM=datx.y
	
	overamp=daty.y

	xvals=FM
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
	for ccnt=0,1 do begin
	p1=scatterplot(xvals,yvals,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='lime',name='overshoot amplitude',xcolor=!color.dark_slate_blue,ycolor=!color.dark_slate_blue)
	;p2=scatterplot(2.,0.01,SYMBOL='.', /SYM_FILLED, $
						
		if ccnt eq 1 then begin
			p2=scatterplot([2.,2+22./53],[.01,0.1*4/25],SYMBOL='.', /SYM_FILLED, $
					sym_color='black',name="Mellott's 25 Hz res 			data",/over,sym_size=2)
			medname='Our '+medname
			p1.name='Our 1Hz res data'
		endif		
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color='green',name=medname,/overplot)

		

	if ccnt eq 0 then	ccf = LEGEND(target=[p1,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
	if ccnt eq 1 then begin
		ccf = LEGEND(target=[p2,p1,medplot1], font_size=7,transparency=80,/relative,pos=[.95,.95])
	plotFName=yfnm+"_vs_"+xfnm+"_with_Mellott.png"
		
		p1.name='Our 1 Hz res data'
		medplot1.name='our median line'
		
	f3name='Documents/MellotAlfvenTrans2.png'
	im1=image(f3name,/over,$
		image_dimensions=[14+2*112./117.7,2.5+.5*36/142.],$;[14+2*112./117.7,2.0+.5*201/142.],$;[16,2.5+.5*36/142.],$
		aspect_ratio=5.)
	;p2=scatterplot(2.,0.01,SYMBOL='.', /SYM_FILLED, $
	;					sym_color='black',name="Mellott's 25 Hz res data",sym_size=2.5*p1.sym_size,/over)
		;ccf = LEGEND(target=[p1,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
		ccf.pos=[1.,1.]
	endif 
	ccf.save,dire+"green_"+plotFName, BORDER=10, $
  				 RESOLUTION=400
	p1.close
	endfor
	return
	get_data,'FM2',data=datx
	get_data,'overshootAmplitude',data=daty
	FM=datx.y
	
	overamp=daty.y

	xvals=FM
	yvals=overamp
	binsize=datx.binsize[0]
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]
	Title=YT+" vs "+XT
	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	plotFName=yfnm+"_vs_"+xfnm+".png";".eps"
	for ccnt=0,1 do begin

	p1=scatterplot(xvals,yvals,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='green',name='overshoot amplitude')
		ps=p1.position 
		xl=ps[0]
		xr=ps[2]
		yb=ps[1]
		yt=ps[3]
		rH=yt-yb
		rW=xr-xl
		ytv=p1.ytickvalues
		xtv=p1.xtickvalues
rrW=rW/(max(xtv)-min(xtv))
		rrH=rH/(max(ytv)-min(ytv))
				;maxval=max(xvals)
				;mlow = findgen(maxval/binsize+1)*binsize
				;mhigh = mlow + binsize
				;medovershoot = fltarr(maxval/binsize+1)
				;for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
; 					 w = where(xvals ge mlow[zzz] and xvals lt mhigh[zzz],nw)
 ; 					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals )[w])
;				endfor
;				nonzero=where(medovershoot ne 0.0,zzz)
;				mlow2=mlow[nonzero]
;				mhigh2=mhigh[nonzero]
;				medovershoot2=medovershoot[nonzero]
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

;				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.lime,name="Median",/overplot)
		medliner,xvals,yvals,binsize,mlow2,mhigh2,medovershoot2
medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.lime,name="Median",/overplot)
	if ccnt eq 0 then begin
	ccf = LEGEND(target=[p1,medplot1], font_size=7,transparency=80,/relative,pos=[1.,1.])
	 ccf.save,dire+"green_"+plotFName, BORDER=10, RESOLUTION=400
	endif
	if ccnt eq 1 then begin
		ps2=medplot1.position 
		xl2=ps[0]
		xr2=ps[2]
		xtv2=medplot1.xtickvalues
		print,ps
		print,ps2

		print,''
		print,xtv
		print,xtv2
		;yb=ps[1]
		;yt=ps[3]
		;rH=yt-yb
		;rW=xr-xl
		rW2=xr2-xl2
		rrW2=rW2/(max(xtv2)-min(xtv2))
		;ytv=p1.ytickvalues
		;xtv=p1.xtickvalues
		p1.name='our overshoot amplitude'
		medplot1.name='our median'
		;rrW=rW/(max(xtv)-min(xtv))
		;rrH=rH/(max(ytv)-min(ytv))
		f3name='Documents/MellottquartersecTRANSPARENT3.png'
		
		xxl=xl2+0.;xl2-rrW2;0;xl
		xxr=6.*rrW2;-.75*rrW2;+xl
		yyb=yb
		yyt=rrH*(2.5+.5*35/54)+yb              
		pos3=[xxl,yyb,xxr,yyt] 
		;im1=image(f3name,/current,position=pos3)
		im1=image(f3name,/over,image_dimensions=[6,2.5+.5*35/54],aspect_ratio=2,name="Mellott's data")
		ccf = LEGEND(target=[p1,medplot1,im1], font_size=7,transparency=80,/relative,pos=[1.,1.])
		p1.title=''
		plotFName=yfnm+"_vs_"+xfnm+"withMellott.png"
		ccf.save,dire+"green_"+plotFName, BORDER=10, RESOLUTION=400
	endif

	p1.close
	endfor
	return
	endif
	get_data,"Mfms",data=datMfms
	get_data,"B2B1fit",data=datB2B1fit
	get_data,"B2B1_RH",data=datB2B1RH
	
	B2B1_Fit=datB2B1fit.y
	Mfms=datMfms.y
	B2B1_RH=datB2B1RH.y

	binsize=1.
	xvals=Mfms
	

	p1=scatterplot(Mfms,B2B1_Fit,xtitle='$M_{fms}$',ytitle='$B_{downstream}/B_{upstream}$',title='Magnetic Jump vs Fast Mach Number',$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='black',name='Fit ratio',/over)
	p2=scatterplot(Mfms,B2B1_RH,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='red',name='RH calculated ratio',/over)
	yvals=B2B1_Fit
	
				maxval=max(Mfms)
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median FIT",/overplot,thick=2)
		yvals=B2B1_RH
		maxval=max(Mfms)
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot2=plot((mlow2+mhigh2)/2.0, medovershoot2,color="dark orange",name="Median RH",/overplot,thick=2)

	ccf = LEGEND(target=[p1,p2,medplot1,medplot2], font_size=7,transparency=80,/relative,pos=[0.35,1.]);[p10,p11,p12,p13])
	ccf.save,dire+"FIT_AND_RH_VS_MFMS.png", BORDER=10, $
  				 RESOLUTION=400

	p1.close

	get_data,"ThetaNBn",data=datTH

	xvals=datTH.y
	binsize=datTH.binsize[0]
p1=scatterplot(xvals,B2B1_Fit,xtitle=datTH.ytitle,ytitle='$B_{downstream}/B_{upstream}$',title='Magnetic Jump vs Fast Mach Number',$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='black',name='Fit ratio',/over)
	p2=scatterplot(xvals,B2B1_RH,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='red',name='RH calculated ratio',/over)
	yvals=B2B1_Fit
	
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median FIT",/overplot,thick=2)
		yvals=B2B1_RH
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot2=plot((mlow2+mhigh2)/2.0, medovershoot2,color="dark orange",name="Median RH",/overplot,thick=2)

	ccf = LEGEND(target=[p1,p2,medplot1,medplot2], font_size=7,transparency=80,/relative,pos=[0.35,1.]);[p10,p11,p12,p13])
	ccf.save,dire+"FIT_AND_RH_VS_TH.png", BORDER=10, $
  				 RESOLUTION=400

	p1.close

get_data,"beta",data=datBeta

	xvals=datBeta.y
	binsize=datBeta.binsize[0]
p1=scatterplot(xvals,B2B1_Fit,xtitle=datTH.ytitle,ytitle='$B_{downstream}/B_{upstream}$',title='Magnetic Jump vs Fast Mach Number',$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='black',name='Fit ratio',/over)
	p2=scatterplot(xvals,B2B1_RH,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='red',name='RH calculated ratio',/over)
	yvals=B2B1_Fit
	
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot1=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median FIT",/overplot,thick=2)
		yvals=B2B1_RH
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
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot2=plot((mlow2+mhigh2)/2.0, medovershoot2,color="dark orange",name="Median RH",/overplot,thick=2)

	ccf = LEGEND(target=[p1,p2,medplot1,medplot2], font_size=7,transparency=80,/relative,pos=[0.35,1.]);[p10,p11,p12,p13])
	ccf.save,dire+"FIT_AND_RH_VS_BETA.png", BORDER=10, $
  				 RESOLUTION=400

	p1.close
	

end

	
