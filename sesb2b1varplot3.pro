pro sesb2b1varplot3
	dire="Documents/Plots/CombinedPlots/"
	R_mars=3389.5

	seasonColors=list("green","red","goldenrod","blue")
	seasonColors2=list("spring_green","orange_red","gold","deep_sky_blue")
	seasonColors3=list("forest_green","firebrick","tan","steel_blue")

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
    ww = WINDOW(DIMENSIONS=[350,500])

 

	;;;;ORIENTATION: (LAYOUT=[2,4,X]

	;;; [p10,p20]  ==[M season,M RH]      :: [2,4,1] , [2,4,2]

	;;; [p30,p40]  ==[beta season,beta RH] :: [2,4,3] , [2,4,4]

	;;; [p50,p60]  ==[theta season,theta RH] :: [2,4,5] , [2,4,6]

	TitleSeason='';"Seasonal Binning"
	TitleRH='';"Rankine-Hugoniot Comparison"

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

	ytAMB='';'$B_{downstream}/B_{upstream}$'
	Y=daty.y
	YT=daty.YN[0]
	yttl='';daty.ytitle[0]
	X=datx.y

	XT=datx.YN[0]
	xttl='';datx.ytitle[0]
	binsize=datx.binsize[0]
	N=numel(datx.x)
	plotlist1=list()
			p10=scatterplot(x,y,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
				/curr,layout=[2,4,1],pos=npos1)
	;KK=0
			;xsub=X[seasonsubIndices[KK]]
			;ysub=Y[seasonsubIndices[KK]]
						
			;plotlist1.add,scatterplot(xsub,ysub,$
			;	SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
			;	sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,1],pos=npos1)
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,1])

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=(mlow+mhigh)/2.0
			wBig=where(medx ge 4.5)
			plotlist1.add,plot(medx, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,1]);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
			plotlist1.add,errorplot(medx[wBig], medovershoot[wBig],errorbars[*,wBig],color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,1],errorbar_color=seasonColors3[KK]);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	medx=(mlow+mhigh)/2.0
			;wBig=where(medx ge 4.5)
	plotlist1.add,plot(medx, medovershoot,color=!color.dark_slate_grey,name="Median",/overplot,title=Title,layout=[2,4,1]);,pos=[0.092187501  ,    0.695   ,   0.44531250  ,    0.93046875])
	;plotlist1.add,errorplot(medx[wBig], medovershoot[wBig],errorbars[*,wBig],color=dark_slate_grey,name="Median",/overplot,layout=[2,4,1]);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	plotlist1[0].title=TitleSeason
	;plotlist1[0].ytitle="$M_{fms}$ plots $\n\n$"+yttl
	plotlist1[0].xtitle=xttl

	;cff=legend(target=plotlist1, font_size=4,transparency=99.999,orient=0,/relative,pos=[0,1],VERTICAL_SPACING=.001)
	;;;M RH


	;;original2 =[0.56866071, 0.80496875  ,0.96562500, 0.91346250]

	xl2=0.59218750
	xr2=0.96562500
	npos2=[xl2 ,yb1 ,xr2, yt1];[0.59218750 ,yb1 ,0.94531250, yt1]
	p20=scatterplot(X,B2B1_Fit,xtitle=xttl,ytitle=ytAMB,title=TitleRH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='blue',name='Fit ratio',/current,layout=[2,4,2],pos=npos2)
	p21=scatterplot(X,B2B1_RH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='dark orange',name='RH calculated ratio',/over,layout=[2,4,2]);,pos= [0.59218750 ,0.695,0.94531250, 0.93046875])

	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,layout=[2,4,2],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	;medplot1=plot((mlow+mhigh)/2.0, medovershoot,color=!color.dark_slate_blue, name="Median FIT",/over,layout=[2,4,2]);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue, name="Median FIT",/over,layout=[2,4,2],errorbar_color='medium blue');,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars
;	medplot2=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/over,layout=[2,4,2],ERRORBAR_THICK=.3,ERRORBAR_COLOR='peru');,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	help,B2B1_RH
	help,mlow
	help,mhigh
	help,medovershoot
	;medplot2=plot((mlow+mhigh)/2.0, medovershoot,color="red",name="Median RH",/over,layout=[2,4,2]);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot2=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/over,layout=[2,4,2],errorbar_color='crimson');,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	print,p20.sym_size
	print,plotlist1[0].position
	print,p20.position
;cRH1 = LEGEND(target=[p20,p21,medplot1,medplot2], font_size=4,transparency=100,orient=0,/relative,pos=[xl2,1],VERTICAL_SPACING=.005)
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
	xttl='';datx.ytitle[0];'log$(\beta)$';datx.ytitle[0]
	binsize=.2;datx.binsize[0]
	plotlist2=list()
	KK=0
	xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist2.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1,/xlog, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,3],pos=npos3)
	FOR KK=1,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist2.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,3])

	endfor
	FOR KK=0,3 do begin
			xsub=alog10(X[seasonsubIndices[KK]])
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			
			
			plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,3]);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])
	endfor
	medliner,alog10(X),Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist2.add,plot(10.^((mlow+mhigh)/2.0), medovershoot,color=!color.dark_slate_blue,name="Median",/overplot,title=Title,layout=[2,4,3],xrange=[.1,20]);,pos=[0.1 ,0.35 ,0.44531250,0.59635417])


	;plotlist2[0].ytitle="$\beta$ plots $\n\n$"+yttl
	;plotlist2[0].xtitle=xttl

	;;BETA RH
	;;original pos=      0.59218750      0.42513021      0.94531250      0.59635417
	npos4=[xl2,yb2,xr2,yt2];[0.59218750  ,yb2,  0.94531250   ,   yt2]
	p40=scatterplot(X,B2B1_Fit,xtitle=xttl,ytitle=ytAMB,/xlog,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='blue',name='Fit ratio',/current,layout=[2,4,4],pos=npos4)
	p41=scatterplot(X,B2B1_RH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='dark orange',name='RH calculated ratio',/over,layout=[2,4,4]);,pos=[0.59218750  ,0.35,  0.94531250   ,   0.59635417])

	medliner,alog10(X),B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot3=errorplot(10.^((mlow+mhigh)/2.0), medovershoot,errorbars,color=!color.dark_slate_blue,name="Median FIT",/overplot,layout=[2,4,4],ERRORBAR_THICK=.25,ERRORBAR_COLOR='dim_grey')
medplot3=errorplot(10.^((mlow+mhigh)/2.0), medovershoot,errorbars,color=!color.dark_slate_blue,name="Median FIT",/overplot,layout=[2,4,4],errorbar_color='medium blue')
	medliner,alog10(X),B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot4=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/overplot,layout=[2,4,4],ERRORBAR_THICK=.25,ERRORBAR_COLOR='peru')
	;medplot4=plot(10.^((mlow+mhigh)/2.0), medovershoot,color="red",name="Median RH",/overplot,layout=[2,4,4])
	medplot4=errorplot(10.^((mlow+mhigh)/2.0), medovershoot,errorbars,color="red",name="Median RH",/overplot,layout=[2,4,4],errorbar_color='crimson',xrange=[.1,20])
	print,p40.sym_size
	print,plotlist2[0].position
	print,p40.position
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
	xttl='';datx.ytitle[0]
	binsize=datx.binsize[0]
	plotlist3=list()
	KK=0
xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/curr,layout=[2,4,5],pos=npos5,xrange=[0,90])

	FOR KK=1,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1, $
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over,layout=[2,4,5])

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			plotlist3.add,plot((mlow+mhigh)/2.0, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,5])
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist3.add,plot((mlow+mhigh)/2.0, medovershoot,color=!color.dark_slate_blue,name="Median",/overplot,title=Title,layout=[2,4,5])

	;plotlist3[0].ytitle="$\theta_{NB_1}$ plots $\n\n$"+yttl
	;plotlist3[0].xtitle=xttl	
	;;THETA RH
	npos6=[xl2,yb3,xr2,yt3];[0.59218750,yb3,0.94531250,  yt3]
	
	p60=scatterplot(X,B2B1_Fit,xtitle=xttl,ytitle=ytAMB,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='blue',name='Fit ratio',/current,layout=[2,4,6],pos=npos6,xrange=[0.,90.])
	p61=scatterplot(X,B2B1_RH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='dark orange',name='RH calculated ratio',/over,layout=[2,4,6]);,pos=[0.59218750,0.070312500,0.94531250,  0.26302083])

	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot5=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue,name="Median FIT",/over,layout=[2,4,6],ERRORBAR_THICK=.25,ERRORBAR_COLOR='dim_grey')
	medplot5=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue,name="Median FIT",errorbar_color='medium blue',/over,layout=[2,4,6])
	
	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot6=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/over,layout=[2,4,6],ERRORBAR_THICK=.25,ERRORBAR_COLOR='peru')
	medplot6=plot((mlow+mhigh)/2.0, medovershoot,color="red",name="Median RH",/over,layout=[2,4,6])
	;medplot6.save,dire+'B2B1_varPlots.eps'

	print,p60.sym_size
	print,plotlist3[0].position
	print,p60.position

	;medplot6.close

	get_data,75,data=datMA

	datX=datMA
	X=datx.y

	XT=datx.YN[0]
	xttl='';datx.ytitle[0]
	binsize=datx.binsize[0]
	npos7=[xl1,.05,xr1,.22]
	plotlist4=list()
	p70=scatterplot(x,y,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
				/curr,pos=npos7);/curr,layout=[2,4,1],pos=npos7
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist4.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
			medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
			medx=(mlow+mhigh)/2.0
			wBig=where(medx ge 4.5)
			plotlist4.add,plot(medx, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
			;plotlist1.add,errorplot(medx[wBig], medovershoot[wBig],errorbars[*,wBig],color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,layout=[2,4,1],errorbar_color=seasonColors3[KK]);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	medx=(mlow+mhigh)/2.0
			;wBig=where(medx ge 4.5)
	plotlist4.add,plot(medx, medovershoot,color=!color.dark_slate_grey,name="Median",/overplot)	
	;FOR KK=0,3 do begin
	;		xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
						
	;		plotlist4.add,scatterplot(xsub,ysub,$
	;			SYMBOL='dot', /SYM_FILLED,sym_size=1 ,$
	;			sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)
;
	;endfor
	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist4.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	
	;plotlist4[0].ytitle="$M_A$ plots $\n\n$"+yttl
	plotlist4[0].xtitle=xttl
	;plotlist4[0].title='Magnetic Jump vs Ls'


	npos8=[xl2 ,.05 ,xr2, .22];[0.59218750 ,yb1 ,0.94531250, yt1]
	p20=scatterplot(X,B2B1_Fit,xtitle=xttl,ytitle=ytAMB,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='blue',name='Fit ratio',/current,pos=npos8)
	p21=scatterplot(X,B2B1_RH,$
						SYMBOL='dot', /SYM_FILLED,sym_size=1, $
						sym_color='dark orange',name='RH calculated ratio',/over);,pos= [0.59218750 ,0.695,0.94531250, 0.93046875])

	medliner,X,B2B1_Fit,binsize,mlow,mhigh,medovershoot,errorbars
	;medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue,ERRORBAR_COLOR='dim_grey', name="Median FIT",/over,layout=[2,4,2],ERRORBAR_THICK=.25);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medplot1=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_blue,errorbar_color='medium blue', name="Median FIT",/over);,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	medliner,X,B2B1_RH,binsize,mlow,mhigh,medovershoot,errorbars
;	medplot2=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/over,layout=[2,4,2],ERRORBAR_THICK=.3,ERRORBAR_COLOR='peru');,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])
	;help,B2B1_RH
	;help,mlow
	;help,mhigh
	;help,medovershoot
	medplot2=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color="red",name="Median RH",/over,errorbar_color='crimson');,pos= [0.59218750 ,0.695 ,0.94531250, 0.93046875])

	;print,p20.sym_size
	;print,plotlist1[0].position
	;print,p20.position


	;(plotlist4[-1]).save,dire+'B2B1_varPlots.png',res=700
	medplot2.save,dire+'B2B1_varPlots4.eps';,res=600
	print,'done'
	plotlist1[-1].close
end
