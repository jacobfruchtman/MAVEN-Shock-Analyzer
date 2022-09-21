pro sesoverplot,mov=mov
	dire="Documents/Plots/CombinedPlots/"
	R_mars=3389.5

	seasonColors=list("green","red","goldenrod","blue")
	seasonColors2=list("spring_green","orange_red","gold","deep_sky_blue")
	seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")

	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0,numspring)
					summerIndices=where(datseason.y eq 1,numsummer)
					autumnIndices=where(datseason.y eq 2,numautumn)				
					winterIndices=where(datseason.y eq 3,numwinter)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)

	get_data,"FM",data=datFM
	get_data,'MachAlfven',data=datMA
	get_data,14,data=datBeta
	get_data,'overshootAmplitude',data=datOA
	
	tplot_element,'Quasipar','y',Quasipar
	wQpar=where(Quasipar eq 1)
	tplot_element,'Quasiperp','y',Quasiperp
	wQperp=where(Quasiperp eq 1)

	
	OverAmp=datOA.y
	;B2B1_RH=datB2B1RH.y
	help,datFM
FM=datFM.y
Theta=datMA.y
Beta1=datBeta.y
    ww = WINDOW(DIMENSIONS=[700,1000])

 

	;;;;ORIENTATION: (LAYOUT=[2,4,X]

	;;; [p10,p20]  ==[M season,M RH]      :: [2,4,1] , [2,4,2]

	;;; [p30,p40]  ==[beta season,beta RH] :: [2,4,3] , [2,4,4]

	;;; [p50,p60]  ==[theta season,theta RH] :: [2,4,5] , [2,4,6]

	TitleSeason="Seasonal Binning"
	TitleRH="Mellott Comparison"

	;;;;FM season

	;;;original position= [0.035803571,0.79396875 ,0.46562500 ,0.95603125];[0.59218750 ,0.75846354 ,0.94531250, 0.93046875]

	;;update pos[1] to 0.695
	yt1=.98
	yb1=yt1-.20
	xl1=.1
	xr1=0.46562500
	npos1=[xl1,yb1 ,xr1 ,yt1];[0.092187501  ,  yb1      ,   0.44531250  ,   yt1]

	datY=datOA
	datX=datFM


	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	N=numel(datx.x)
	plotlist1=list()
			p10=scatterplot(x,y,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				/curr,layout=[2,4,1],pos=npos1)
	;KK=0
			;xsub=X[seasonsubIndices[KK]]
			;ysub=Y[seasonsubIndices[KK]]
						
			;plotlist1.add,scatterplot(xsub,ysub,$
			;	SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
			;	sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,1],pos=npos1)
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,1])

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			plotlist1.add,plot((mlow+mhigh)/2.0, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,1]);,pos=[0.092187501  , 
			
			xmed=(mlow+mhigh)/2.0
			wBig=where(xmed gt 5.5)
			
			plotlist1.add,errorplot(xmed[wBig], medovershoot[wBig],errorbars[*,wBig],color=seasonColors[KK],errorbar_color=seasonColors3[KK],name="Median "+seasonNames0[KK],/overplot) 
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist1.add,plot((mlow+mhigh)/2.0, medovershoot,color=!color.slate_grey,name="Median",/overplot,title=Title,layout=[2,4,1]);,pos=[0.092187501  ,    0.695   ,   0.44531250  ,    0.93046875])
	plotlist1[0].title=TitleSeason
	plotlist1[0].ytitle="$M_{fms}/M_{crit}$ plots $\n\n$"+yttl
	plotlist1[0].xtitle=xttl

	;cff=legend(target=plotlist1, font_size=4,transparency=99.999,orient=0,/relative,pos=[0,1],VERTICAL_SPACING=.001)
	;;;M RH


	;;original2 =[0.56866071, 0.80496875  ,0.96562500, 0.91346250]

	xl2=0.59218750
	xr2=0.96562500
	npos2=[xl2 ,yb1 ,xr2, yt1];[0.59218750 ,yb1 ,0.94531250, yt1]
	;p20=scatterplot(X,OverAmp,xtitle='$M_{fms}/Mcrit$',ytitle=yttl,title=TitleRH,$
	;					SYMBOL='dot', /SYM_FILLED,sym_size=.05,name="our 1 Hz data", $
	;					sym_color='lime green',/current,layout=[2,4,2],pos=npos2)
	p20=scatterplot(X[wQperp],OverAmp[wQperp],xtitle='$M_{fms}/Mcrit$',ytitle=yttl,title=TitleRH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05,name="our 1 Hz data", $
						sym_color='dodger blue',/current,layout=[2,4,2],pos=npos2)
	p21=scatterplot(X[wQpar],OverAmp[wQpar],$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
						sym_color='orange',/over)
	
	
	medliner,X[wQperp],OverAmp[wQperp],binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,thick=.5,layout=[2,4,2],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot2=plot((mlow+mhigh)/2.0, medovershoot,color=!color.medium_blue, name="Median",/over,thick=1,layout=[2,4,2])
	medliner,X[wQpar],OverAmp[wQpar],binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,thick=.5,layout=[2,4,2],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot3=plot((mlow+mhigh)/2.0, medovershoot,color=!color.red, name="Median",/over,thick=1,layout=[2,4,2])
	
	medliner,X,OverAmp,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,thick=.5,layout=[2,4,2],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	;medplot1=plot((mlow+mhigh)/2.0, medovershoot,color=!color.green, name="Median",/over,thick=1,layout=[2,4,2])
	medplot1=plot((mlow+mhigh)/2.0, medovershoot,color=!color.dim_grey, name="Median",/over,thick=1,layout=[2,4,2])
	

	if keyword_set(mov) then begin
		movingmedian, x,overamp,binsize,xmed,ymed,xmed2,ybin=.25
		b=sort(xmed2)
		xmed2=xmed2[b]
		xmed=xmed[b]
		ymed=ymed[b]
		mmedplot1=plot(xmed, ymed,color='gold', name="Median",/over,thick=1,layout=[2,4,2])
	endif
	
	;,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	f3name='Documents/MELLOTT_A23.png'
	im1=image(f3name,/over,$
		image_dimensions=[7+118./117.9,2.5],$;[14+2*112./117.7,2.0+.5*201/142.],$;[16,2.5+.5*36/142.],$
		aspect_ratio=2.)
	print,p20.sym_size
	print,plotlist1[0].position
	print,p20.position
;cRH1 = LEGEND(target=[p20,medplot1], font_size=4,transparency=100,orient=0,/relative,pos=[xl2,1],VERTICAL_SPACING=.005)
	;return
	;;;BETA Season
	;;original pos= 0.092187501      0.42513021      0.44531250      0.59635417
	;;original pos2= 0.035803571      0.54396875      0.46562500      0.70603125
	yt2=0.73603125;.619;0.59635417
	yb2=yt2-.20;.24
	npos3=[xl1 ,yb2,xr1,yt2];[0.1 ,yb2,0.44531250,yt2]

	datX=datBeta
	X=datx.y
	logx=alog10(X)
	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	plotlist2=list()
	KK=0
	xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist2.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,3],pos=npos3,/xlog)
	FOR KK=1,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist2.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,3])

	endfor
	FOR KK=0,3 do begin
			xsub=alog10(X[seasonsubIndices[KK]])
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,.25,mlow,mhigh,medovershoot,errorbars
			plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,3],thick=.5);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])
	endfor
	medliner,logX,Y,.25,mlow,mhigh,medovershoot,errorbars
	plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=!color.slate_grey,name="Median",/overplot,title=Title,layout=[2,4,3]);,pos=[0.1 ,0.35 ,0.44531250,0.59635417],thick=.5)


	plotlist2[0].ytitle="$\beta$ plots $\n\n$"+yttl
	plotlist2[0].xtitle=xttl

	;;BETA RH
	;;original pos=      0.59218750      0.42513021      0.94531250      0.59635417
	npos4=[xl2,yb2,xr2,yt2];[0.59218750  ,yb2,  0.94531250   ,   yt2]
	;p40=scatterplot(X,OverAmp,xtitle=xttl,ytitle=yttl,$
	;					SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
	;					sym_color='lime green',name='our 1Hz data',/current,layout=[2,4,4],pos=npos4,xrange=[0,10])
	p40=scatterplot(X[wQperp],OverAmp[wQperp],xtitle=xttl,ytitle=yttl,$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
						sym_color='dodger blue',name='our 1Hz data',/current,layout=[2,4,4],pos=npos4,xrange=[0,10])
	p41=scatterplot(X[wQpar],OverAmp[wQpar],$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
						sym_color='orange',/over)
	medliner,X[wQperp],OverAmp[wQperp],binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,thick=.5,layout=[2,4,4],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot4=plot((mlow+mhigh)/2.0, medovershoot,color=!color.medium_blue, name="Median",/over,thick=1,layout=[2,4,4])
	medliner,X[wQpar],OverAmp[wQpar],binsize,mlow,mhigh,medovershoot,errorbars

	medplot5=plot((mlow+mhigh)/2.0, medovershoot,color=!color.red, name="Median",/over,thick=1)
	medliner,X,OverAmp,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot3=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median FIT",/overplot,thick=.5,layout=[2,4,4],ERRORBAR_THICK=.25,ERRORBAR_COLOR='dim_grey')
;medplot3=plot((mlow+mhigh)/2.0, medovershoot,color=!color.green,name="Median",/overplot,layout=[2,4,4])
	medplot3=plot((mlow+mhigh)/2.0, medovershoot,color=!color.dim_grey,name="Median",/overplot)
	if keyword_set(mov) then begin
		movingmedian, x,overamp,binsize,xmed,ymed,xmed2
		b=sort(xmed2)
		xmed2=xmed2[b]
		ymed=ymed[b]
		mmedplot1=plot(xmed2, ymed,color='gold', name="Median",/over,thick=1,layout=[2,4,2])
	endif
	;p2=scatterplot([.52,.52],[.099,0.099],SYMBOL='.', /SYM_FILLED, $
	;				sym_color='black',name="Mellott's 25 Hz res data",/over,sym_size=2)
	f3name='Documents/MellotBetaEtrans.png'
	im1=image(f3name,/over,$
		image_dimensions=[7+118./117.9,2.5],$;[14+2*112./117.7,2.0+.5*201/142.],$;[16,2.5+.5*36/142.],$
		aspect_ratio=2.)
	print,p40.sym_size
	print,plotlist2[0].position
	print,p40.position
	p4pos=p40.pos
	p4h=p4pos[3]-p4pos[2]
	p4adj=yt2-p4pos[3]
	print,'p4pos=',p4pos
	print,'yt2=',yt2
	print,'p4h=',p4h
	print,'p4adj=',p4adj

	;;theta season
	;;original season pos=0.056250000     0.070312500      0.44531250      0.26302083
	;;original 2 =0.035803571      0.29396875      0.46562500      0.45603125

      	;;original RH pos=0.59218750     0.091796875      0.94531250      0.26302083

	yt3=0.48603125;.31;0.26302083
	yb3=yt3-.20;0.070312500
	npos5=[xl1,yb3,xr1,yt3];[.1,yb3,0.44531250,yt3]
	datX=datMA
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	plotlist3=list()
	KK=0
xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,5],pos=npos5)

	FOR KK=1,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,5])

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			plotlist3.add,plot((mlow+mhigh)/2.0, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,5],thick=.5)
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist3.add,plot((mlow+mhigh)/2.0, medovershoot,color=!color.slate_grey,name="Median",/overplot,title=Title,layout=[2,4,5],thick=.5)

	plotlist3[0].ytitle="$\theta_{NB_1}$ plots $\n\n$"+yttl
	plotlist3[0].xtitle=xttl	
	;;THETA RH
	npos6=[xl2,yb3,xr2,yt3];[0.59218750,yb3,0.94531250,  yt3]
	;p60=scatterplot(X,OverAmp,xtitle=xttl,ytitle=yttl,$
	;					SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
	;					sym_color='lime green',name='Fit ratio',/current,layout=[2,4,6],pos=npos6)
	p60=scatterplot(X[wQperp],OverAmp[wQperp],xtitle=xttl,ytitle=yttl,$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
						sym_color='dodger blue',name='our 1Hz data',/current,layout=[2,4,6],pos=npos6)
	p61=scatterplot(X[wQpar],OverAmp[wQpar],$
						SYMBOL='dot', /SYM_FILLED,sym_size=.05, $
						sym_color='orange',/over)
	ar=5.
	xcoord=2.
	ycoord=.01

	medliner,X[wQperp],OverAmp[wQperp],binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,thick=.5,layout=[2,4,4],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot8=plot((mlow+mhigh)/2.0, medovershoot,color=!color.medium_blue, name="Median",/over,thick=1)
	medliner,X[wQpar],OverAmp[wQpar],binsize,mlow,mhigh,medovershoot,errorbars

	medplot9=plot((mlow+mhigh)/2.0, medovershoot,color=!color.red, name="Median",/over,thick=1,layout=[2,4,6])


	medliner,X,OverAmp,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot5=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median FIT",/over,thick=.5,layout=[2,4,6],ERRORBAR_THICK=.25,ERRORBAR_COLOR='dim_grey')

	medplot5=plot((mlow+mhigh)/2.0, medovershoot,color=!color.dim_grey,name="Median FIT",/over,layout=[2,4,6])

		
	if keyword_set(mov) then begin
		movingmedian, x,overamp,binsize,xmed,ymed,xmed2,ybin=.25
		b=sort(xmed2)
		xmed2=xmed2[b]
		xmed=xmed[b]
		ymed=ymed[b]
		mmedplot1=plot(xmed2, ymed,color='gold', name="Median",/over,thick=1,layout=[2,4,2])
	endif
	p2=scatterplot([1.,1.]*xcoord,[1.,1.]*ycoord,SYMBOL='.', /SYM_FILLED, $
					sym_color='black',name="Mellott's 25 Hz res data",/over,sym_size=2)
		imdems=[14+2*112./117.7,2.5+.5*36/142.]
	mname='Documents/MellotAlfvenTrans3.png'
	im1=image(mname,/over,$
		image_dimensions=imdems,$
		aspect_ratio=ar)
	;medplot6.save,dire+'B2B1_varPlots.eps'

	print,p60.sym_size
	print,plotlist3[0].position
	print,p60.position

	;medplot6.close

	get_data,'Mflow',data=datX

	;return

	;;
	get_data,'Ls',data=datLs
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	npos7=[xl1,.04,xr1,.23]
	plotlist4=list()
	p70=scatterplot(x,y,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				/curr,pos=npos7);/curr,layout=[2,4,1],pos=npos7
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist4.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			plotlist4.add,plot((mlow+mhigh)/2.0, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,7],thick=.5);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])
	endfor
	;endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist4.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,    0.695   ,   0.44531250  ,    0.93046875])
	plotlist4[0].ytitle='$M_{flow}$ plot$\n\n$'+yttl
	plotlist4[0].xtitle=xttl
	;plotlist4[0].title='Overshoot Amplitude vs $M_{flow}$'

	(plotlist4[-1]).save,dire+'Over_varPlots2.png'
	plotlist4[-1].close
	print,'numspring=',numspring
	print,'numsummer=',numsummer
	print,'numautumn=',numautumn
	print,'numwinter=',numwinter
	return
	datX=datLs
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	npos7=[xl1,.07,xr2,.20]
	plotlist4=list()
	p70=scatterplot(x,y,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				/curr,pos=npos7);/curr,layout=[2,4,1],pos=npos7
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist4.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor

	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist4.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,thick=.5);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist4.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,    0.695   ,   0.44531250  ,    0.93046875])
	plotlist4[0].ytitle=yttl
	plotlist4[0].xtitle=xttl
	plotlist4[0].title='Overshoot Amplitude vs Ls'

	(plotlist4[-1]).save,dire+'Over_varPlots3.png'
	plotlist4[-1].close
	
	print,'numspring=',numspring
	print,'numsummer=',numsummer
	print,'numautumn=',numautumn
	print,'numwinter=',numwinter
	
end
