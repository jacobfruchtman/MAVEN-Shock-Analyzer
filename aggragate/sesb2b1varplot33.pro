pro sesb2b1varplot33
	dire="Documents/Plots/CombinedPlots/"
	fname=dire+'B2B1_varPlots.png'
	
	R_mars=3389.5

	seasonColors=list("green","red","goldenrod","blue")
	seasonColorsH=list(!color.green,!color.red,!color.goldenrod,!color.blue)
	seasonColors2=list("spring_green","orange_red","gold","deep_sky_blue")
	seasonColors2H=list(!color.spring_green,!color.orange_red,!color.gold,!color.deep_sky_blue)
	seasonColors3=list("forest_green","firebrick","tan","steel_blue")
	seasonColors3H=list(!color.forest_green,!color.firebrick,!color.tan,!color.steel_blue)

	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)

	get_data,"Mfms",data=datMfms
	get_data,"ThetaNBn",data=datTH
	get_data,"beta",data=datBeta
	get_data,"B2B1fit",data=datB2B1fit
	get_data,"B2B1_RH",data=datB2B1RH
	
	B2B1_Fit=datB2B1fit.y
	B2B1_RH=datB2B1RH.y

Mfms=datMfms.y
Theta=datTH.y
Beta1=datBeta.y
   ; ww = WINDOW(DIMENSIONS=[700,1000])

	set_plot,'ps',/interpolate
		device, filename = 'testtest.eps', /color, bits_per_pixel=8, /encapsulated, xsize=7, ysize=10, set_font='Helvetica'
	device, filename = dire+'B2B1_varPlots42.eps', /color, bits_per_pixel=8, /encapsulated, xsize=14, ysize=20, set_font='Helvetica'
	!p.multi=[0,2,4]

	;;;;ORIENTATION: (LAYOUT=[2,4,X]

	;;; [p10,p20]  ==[M season,M RH]      :: [2,4,1] , [2,4,2]

	;;; [p30,p40]  ==[beta season,beta RH] :: [2,4,3] , [2,4,4]

	;;; [p50,p60]  ==[theta season,theta RH] :: [2,4,5] , [2,4,6]

	TitleSeason="Seasonal Binning"
	TitleRH="Rankine-Hugoniot Comparison"

	;;;;M season

	;;;original position= [0.035803571,0.79396875 ,0.46562500 ,0.95603125];[0.59218750 ,0.75846354 ,0.94531250, 0.93046875]

	;;update pos[1] to 0.695
	yt1=.95
	yb1=yt1-.20
	xl1=.1
	xr1=0.46562500
	npos1=[xl1,yb1 ,xr1 ,yt1];[0.092187501  ,  yb1      ,   0.44531250  ,   yt1]

	datY=datB2B1fit
	datX=datMfms


	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	N=numel(datx.x)
	plotlist1=list()
	loadct,39
	plot,x,y,psym=3,symsize=.02,title=TitleSeason,ytitle="$M_{fms}$ plots !C!C"+yttl,xtitle=xttl
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColors2H[KK]
			oplot,xsub,ysub,psym=3,color=1,symsize=.02

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColorsH[KK]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=(mlow+mhigh)/2.0
			wBig=where(medx ge 4.5)
			oplot,medx, medovershoot,color=1
			monocolortable,seasonColors3H[KK]
			errplot,medx[wBig], medovershoot[wBig]-errorbars[0,wBig],medovershoot[wBig]+errorbars[1,wBig],color=1
			
	endfor

	loadct,39                                   
	axis,color=0


	;;;M RH


	;;original2 =[0.56866071, 0.80496875  ,0.96562500, 0.91346250]

	xl2=0.59218750
	xr2=0.96562500
	npos2=[xl2 ,yb1 ,xr2, yt1];[0.59218750 ,yb1 ,0.94531250, yt1]
	plot,X,B2B1_Fit,xtitle='$M_{fms}$',ytitle='$B_{downstream}/B_{upstream}$',title=TitleRH,$
						psym=3,symsize=.02,color=1;,pos=npos2
				monocolortable,!color.blue
	oplot,X,B2B1_Fit,color=1,$
						psym=3,symsize=.02;,pos=npos2
	monocolortable,!color.dark_orange
	oplot,X,B2B1_RH,psym=3,symsize=.02,color=1
	
	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	
	monocolortable,!color.dark_slate_blue
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.medium_blue
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1
	
	

	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars


	monocolortable,!color.red
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.crimson
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1


	help,B2B1_RH
	help,mlow
	help,mhigh
	help,medovershoot
	;medplot2=plot((mlow+mhigh)/2.0, medovershoot,color="red",name="Median RH",/over,layout=[2,4,2]);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	loadct,39                                   
	axis,color=0
	;device,/close
	;set_plot,'x'
	;return
	;;;BETA Season
	;;original pos= 0.092187501      0.42513021      0.44531250      0.59635417
	;;original pos2= 0.035803571      0.54396875      0.46562500      0.70603125
	yt2=0.70603125;.619;0.59635417
	yb2=yt2-.20;.24
	npos3=[xl1 ,yb2,xr1,yt2];[0.1 ,yb2,0.44531250,yt2]

	datX=datBeta
	X=datx.y;alog(datx.y)

	XT=datx.YN[0]
	xttl=datx.ytitle[0];'log$(\beta)$';datx.ytitle[0]
	binsize=.2;datx.binsize[0]
	plotlist2=list()
	KK=0
	
	plot,x,y,psym=3,symsize=.01,title=TitleSeason,ytitle="$\beta$ plots !C!C"+yttl,xtitle=xttl,/xlog
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColors2H[KK]
			oplot,xsub,ysub,psym=3,color=1,symsize=.01

	endfor
	FOR KK=0,3 do begin
			xsub=alog10(X[seasonsubIndices[KK]])
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColorsH[KK]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=10.^((mlow+mhigh)/2.0)
			wBig=where(medx ge 4.5)
			oplot,medx, medovershoot,color=1
			;monocolortable,seasonColors3H[KK]
			;errplot,medx[wBig], medovershoot[wBig]-errorbars[0,wBig],medovershoot[wBig]+errorbars[1,wBig],color=1
			
	endfor
	
			;xsub=X[seasonsubIndices[KK]]
			;ysub=Y[seasonsubIndices[KK]]
						
			;plotlist2.add,scatterplot(xsub,ysub,$
			;	SYMBOL='dot', /SYM_FILLED,sym_size=.01,/xlog, $
			;	sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,3],pos=npos3)
	;FOR KK=1,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
		;	ysub=Y[seasonsubIndices[KK]]
						
		;	plotlist2.add,scatterplot(xsub,ysub,$
		;		SYMBOL='dot', /SYM_FILLED,sym_size=.01, $
		;		sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,3])

	;endfor
	;FOR KK=0,3 do begin
	;		xsub=alog10(X[seasonsubIndices[KK]])
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			
			
	;		plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,3]);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])
	;endfor
	;medliner,alog10(X),Y,binsize,mlow,mhigh,medovershoot,errorbars
	;plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=!color.dark_slate_blue,name="Median",/overplot,title=Title,layout=[2,4,3],xrange=[.1,20]);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])


	loadct,39                                   
	axis,color=0

	;;BETA RH
	plot,X,B2B1_Fit,xtitle=xttl,ytitle='$B_{downstream}/B_{upstream}$',$
						psym=3,symsize=.02,/xlog;,pos=npos2
				monocolortable,!color.blue
	oplot,X,B2B1_Fit,color=1,$
						psym=3,symsize=.02;,pos=npos2
	monocolortable,!color.dark_orange
	oplot,X,B2B1_RH,psym=3,symsize=.02,color=1
	
	medliner,alog10(X),B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	
	monocolortable,!color.dark_slate_blue
	oplot,10.^((mlow+mhigh)/2.0), medovershoot,color=1
	monocolortable,!color.medium_blue
	errplot,10.^((mlow+mhigh)/2.0), medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1
	
	

	medliner,alog10(X),B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars


	monocolortable,!color.red
	oplot,10.^((mlow+mhigh)/2.0), medovershoot,color=1
	monocolortable,!color.crimson
	errplot,10.0^((mlow+mhigh)/2.0), medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1


	
	loadct,39                                   
	axis,color=0
	
	
	
	
	;;theta season
	;;original season pos=0.056250000     0.070312500      0.44531250      0.26302083
	;;original 2 =0.035803571      0.29396875      0.46562500      0.45603125

      	;;original RH pos=0.59218750     0.091796875      0.94531250      0.26302083

	yt3=0.45603125;.31;0.26302083
	yb3=yt3-.20;0.070312500
	npos5=[xl1,yb3,xr1,yt3];[.1,yb3,0.44531250,yt3]
	datX=datTH
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	plotlist3=list()
	KK=0
	plot,x,y,psym=3,symsize=.02,title=TitleSeason,ytitle="$\theta_{BN}$ plots!C!C"+yttl,xtitle=xttl,xrange=[0,90]
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColors2H[KK]
			oplot,xsub,ysub,psym=3,color=1,symsize=.02

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColorsH[KK]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=(mlow+mhigh)/2.0
			wBig=where(medx ge 4.5)
			oplot,medx, medovershoot,color=1
			;monocolortable,seasonColors3H[KK]
			;errplot,medx[wBig], medovershoot[wBig]-errorbars[0,wBig],medovershoot[wBig]+errorbars[1,wBig],color=1
			
	endfor

	loadct,39                                   
	axis,color=0

	;;THETA RH
	plot,X,B2B1_Fit,xtitle='$\theta_{BN}$ [deg]',ytitle='$B_{downstream}/B_{upstream}$',xrange=[0,90],$
						psym=3,symsize=.02,color=1;,pos=npos2
				monocolortable,!color.blue
	oplot,X,B2B1_Fit,color=1,$
						psym=3,symsize=.02;,pos=npos2
	monocolortable,!color.dark_orange
	oplot,X,B2B1_RH,psym=3,symsize=.02,color=1
	
	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	
	monocolortable,!color.dark_slate_blue
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.medium_blue
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1
	
	

	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars


	monocolortable,!color.red
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.crimson
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1


	loadct,39                                   
	axis,color=0

	;!p.multi=0
	;device,/close
	;set_plot,'x'
	;return

	get_data,75,data=datMA

	datX=datMA
	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	npos7=[xl1,.05,xr1,.22]
	
	plot,x,y,psym=3,symsize=.02,title=TitleSeason,ytitle="$M_{A}$ plots!C!C"+yttl,xtitle=xttl
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColors2H[KK]
			oplot,xsub,ysub,psym=3,color=1,symsize=.02

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			monocolortable,seasonColorsH[KK]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=(mlow+mhigh)/2.0
			wBig=where(medx ge 4.5)
			oplot,medx, medovershoot,color=1
			;monocolortable,seasonColors3H[KK]
			;errplot,medx[wBig], medovershoot[wBig]-errorbars[0,wBig],medovershoot[wBig]+errorbars[1,wBig],color=1
			
	endfor

	loadct,39                                   
	axis,color=0




	
	
	
	;plotlist4[0].ytitle="$M_A$ plots $\n\n$"+yttl
	;plotlist4[0].xtitle=xttl
	;plotlist4[0].title='Magnetic Jump vs Ls'


	npos8=[xl2 ,.05 ,xr2, .22];[0.59218750 ,yb1 ,0.94531250, yt1]
	plot,X,B2B1_Fit,xtitle='$M_A$',ytitle='$B_{downstream}/B_{upstream}$',$
						psym=3,symsize=.02,color=1;,pos=npos2
				monocolortable,!color.blue
	oplot,X,B2B1_Fit,color=1,$
						psym=3,symsize=.02;,pos=npos2
	monocolortable,!color.dark_orange
	oplot,X,B2B1_RH,psym=3,symsize=.02,color=1
	
	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	
	monocolortable,!color.dark_slate_blue
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.medium_blue
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1
	
	

	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars


	monocolortable,!color.red
	oplot,(mlow+mhigh)/2.0, medovershoot,color=1
	monocolortable,!color.crimson
	errplot,(mlow+mhigh)/2.0, medovershoot-errorbars[0,*],medovershoot+errorbars[1,*],color=1


	loadct,39                                   
	axis,color=0
	!p.multi=0
	device,/close
	set_plot,'x'

	;(plotlist4[-1]).save,dire+'B2B1_varPlots.png',res=700
	;medplot2.save,dire+'B2B1_varPlots4.eps';,res=600
	;print,'done'
	;plotlist1[-1].close
end
