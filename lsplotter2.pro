pro lsplotter2
		dire="Documents/Plots/CombinedPlots/"
	seasonColors=list("green","red","goldenrod","blue")
	seasonColors2=list("spring_green","orange_red","gold","blue")
	seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")

	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
	get_data,'Ls',data=datLS
	LS=datLS.y
	get_data,'OverAmpvarMeanRatio',data=datAVAR
	Avar=datAvar.y

	dire="Documents/Plots/CombinedPlots/"
	R_mars=3389.5

	get_data,'X_MSO',data=datXmso
	get_data,'RHO_MSO',data=datRmso
	get_data,'overshootAmplitude',data=datA
	get_data,'Ls',data=datLs
	get_data,'Mflow',data=datM

	Mflow=datM.y
	datM.YN[0]='$M_{ms1}$'
	datA.Ytitle= 'A=($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$'
	datLs.YN='$L_S$'
	OA=datA.y
	t=datM.x
	minA=min(OA,minloc)
	print,"min(OA)=",minA,", at ", time_string(t[minloc])
	tsort=sort(t)
	Msort=sort(Mflow)
	Mflow0=Mflow[Msort]
	OA0=OA[Msort]
	t0=t[Msort]
	weight=1.0/OA0
	curveType='expofit'
	MM=[.1,.66,1.38,-.26]
	expofit,Mflow0,MM,F
	;p3=plot(Mflow0,F,'g-')
	use1=1
	yfit=curvefit(Mflow0, OA0, weight, MM, SIGMA, FUNCTION_NAME='expofit',status=status,CHISQ=CHISQ)
	;F=.1*exp(.35*(Mflow+.1))+.1
	MM2=[.1,.35,-.1,.1]
	expofit,Mflow0,MM,F
	yfit2=curvefit(Mflow0, OA0, weight, MM2, SIGMA, FUNCTION_NAME='expofit2',status=status,CHISQ=CHISQ2)
	print,'CHISQ,CHISQ2=',CHISQ,",",CHISQ2	
	p1=scatterplot(Mflow0,OA0,xtitle='$M_{flow}$',ytitle=datA.Ytitle,sym_size=.2)

	sm0=string(MM[0],format='%0.2f')
	sm1=string(MM[1],format='%0.2f')
	sm2=string(MM[2],format='%0.2f')
	sm3=string(MM[3],format='%0.2f')
	name1=sm0+'(M_flow-'+sm1+')^{'+sm2+'}+'+sm3 +' , CHI^2='+string(CHISQ,format='%0.3f')
	tm0=string(MM2[0],format='%0.2f')
	tm1=string(MM2[1],format='%0.2f')
	tm2=string(MM2[2],format='%0.2f')
	tm3=string(MM2[3],format='%0.2f')
	name2=tm0+'*exp('+tm2+'(M_flow-'+tm2+'))+'+tm3 +' , CHI^2='+string(CHISQ2,format='%0.3f')
	p2=plot(Mflow0,yfit,'b-',/over,name='fit1='+name1)
	p3=plot(Mflow0,yfit2,'g-',/over,name='fit2='+name2)
	
	MM3=[1,0,1,0.]
	yfit3=curvefit(yfit,OA0 , weight, MM3, SIGMA, FUNCTION_NAME='expofit',status=status3,CHISQ=CHISQ3)
	print,MM3
	p25=plot(Mflow0,yfit3,'-',color='brown',/over,name='MM3')
	MM4=[1,0,1,0.]	
yfit4=curvefit(yfit2,OA0 , weight, MM4, SIGMA, FUNCTION_NAME='expofit',status=status4,CHISQ=CHISQ4)

	MM5=[1,.1,-MM[3],MM[3]]	
	yfit5=curvefit(yfit,OA0 , weight, MM5, SIGMA, FUNCTION_NAME='expofit2',status=status5,CHISQ=CHISQ5)

	print,MM4	
	p22=plot(Mflow0,yfit^2,'-',color='cyan',/over,name='fit^2')
	p35=plot(Mflow0,yfit4,'-',color='orange',/over,name='MM4')
	p32=plot(Mflow0,yfit2^2,'m-',/over,name='fit2^2')
	p66=plot(Mflow0,yfit5,color='red',/over,name='MM5')
	ll=legend(target=[p2,p3])
	ll.save,Dire+'MflowFits.png',res=600
	p3.close
	
	p4=scatterplot(OA0,yfit,sym_color='blue',xtitle='A',ytitle='fits',name=name1,sym_size=.1)
	p5=scatterplot(OA0,yfit2,sym_color='green',/over,name=name2,sym_size=.1)
	p6=scatterplot(OA0,yfit3,sym_color='brown',/over,name='MM3',sym_size=.1)

	p10=scatterplot(OA0,yfit5,sym_color='red',/over,name='MM5',sym_size=.1)
	p8=scatterplot(OA0,yfit4,sym_color='orange',/over,name='MM4',sym_size=.1)
		p7=scatterplot(OA0,yfit^2,sym_color='cyan',/over,name=name2,sym_size=.1)
	ll=legend(target=[p4,p5])
		pp1=plot(findgen(50)/10.,findgen(50)/10.,'b-',/over)
	ll.save,Dire+'MflowFits_Vs_A.png',res=600
	p4.close
	if CHISQ gt CHISQ2 then begin
	yfit=yfit2
	use1=0

	MM=MM2
	;p1=scatterplot(Mflow0,OA0,/over)

	;p2=plot(Mflow0,yfit,'b-',/over)
	;p4=scatterplot(Mflow0,OA0/yfit)
	;return
	endif
;	help,yfit
	;p1=scatterplot(Mflow0,OA0,/over)
	;p2=plot(Mflow0,yfit,'b-',/over)
	;p4=scatterplot(Mflow0,OA0/yfit)
	tsort=sort(t0)
	taeq=array_equal(t,t0[tsort])
	Aaeq=array_equal(OA,OA0[tsort])
	print,taeq
	print,Aaeq
	ttdt=total(t-t0[tsort])
	Adt=total(OA-OA0[tsort])
	print,ttdt
	print,Adt
	if ~taeq or ~Aaeq then return
	yfit=yfit[tsort]
	print,total(Mflow-Mflow0[tsort])
	print,total(OA-OA0[tsort])
	;p5=scatterplot(Mflow,OA/yfit)
	print,MM
	print,CHISQ

	Asort=sort(OA)

	A1=OA[Asort]
	tA=t[Asort]

	;	for i=0,20 do print,A1[i],", at ", time_string(tA[i])	
	;	print,'====='
	;for i=0,20 do print,A1[-i],", at ", time_string(tA[-i])
	;return
	AM=OA/yfit
	pp=scatterplot(OA,AM)
	;pp1=plot(findgen(50)/10.,findgen(50)/10.,'b-',/over)
	;pp=scatterplot(OA,yfit,sym_color='red')
	;pp1=plot(findgen(50)/10.,findgen(50)/10.,'b-',/over)
	;pp2=scatterplot(Mflow,AM)
	;return
	sm0=string(MM[0],format='%0.2f')
	sm1=string(MM[1],format='%0.2f')
	sm2=string(MM[2],format='%0.2f')
	sm3=string(MM[3],format='%0.2f')
	if use1 then denstr='$('+sm0+'(M_{flow}-'+sm1+')^{'+sm2+'}+'+sm3+')$' else denstr='$('+sm0+'exp('+sm2+'(M_{flow}-'+sm2+'))+'+sm3+')$'
	denstr=denstr.replace('-+','-')
	denstr=denstr.replace('+-','-')
	denstr=denstr.replace('--','+')
	numerstr='(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ '
	yt=numerstr+denstr
	;'(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ $(.1(M_{flow}-.79)^{1.36}-.028)$'
	
	store_data,'overshootAmplitudeVsMflow',data={x: t ,y: AM ,ytitle:[yt], YN:[ "$M_{flow}$ normalized A" ], fn:[ "Normalized_OvershootHeightvMflow" ], binsize:[ .1 ], radian:[0],degree:[0]}


	;;;;;;;;;

    	ww = WINDOW(DIMENSIONS=[1000,1000])
	xl1= 0.083359375+.02
	xr1=0.94;6531250;+.02
	datX=datLS
	datY=datA
	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]

	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=datx.binsize[0]
	;npos7=[xl1,.07,xr2,.20]
	plotlist0=list()
	
	get_data,43,data=datJ
	J=datJ.y
	jtitle=datJ.ytitle
	p0=scatterplot(x,J,xtitle=xttl,ytitle=jtitle,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				layout=[1,4,1],/curr,xrange=[0,360])
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=J[seasonsubIndices[KK]]
						
			plotlist0.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor
	
	medliner,X,J,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist0.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,   

	p0pos=p0.pos
	p0pos[0]=xl1
	p0pos[2]=xr1
	p0pos[1]-=.025;+.05
	p0pos[3]+=.015;+.05;375
	p0.pos=p0pos
	
	plotlist1=list()
	p10=scatterplot(x,y,xtitle=xttl,ytitle=yttl,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				layout=[1,4,2],/curr,xrange=[0,360]);,pos=npos7
	p1pos=p10.pos
	p1pos[0]=xl1
	p1pos[2]=xr1
	p1pos[1]-=.02;+.05
	p1pos[3]+=.02;+.05;375
	p10.pos=p1pos
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor

	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist1.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,thick=.5);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist1.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,   


	;;;Avar
	get_data,'overshootAmplitudeVsMflow',data=datAM
	datY=datAM
	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	plotlist2=list()
	p20=scatterplot(x,y,/curr,xtitle=xttl,ytitle=yttl,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				layout=[1,4,3],/ylog,xrange=[0,360]);,pos=npos7
	p2pos=p20.pos
	p2pos[0]=xl1
	p2pos[2]=xr1
	p2pos[1]-=.02;+.05
	p2pos[3]+=.02;+.05;375
	p20.pos=p2pos
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist2.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	endfor

	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,thick=.5);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,   

	;;;;Mflow normalized

	datY=datAM
Y=daty.y
	lY=alog(datY.y)
	XT=datx.YN[0]
	YT=daty.YN[0]
	;sml=where(Y lt 10)
	;X=X[sml]
	;Y=Y[sml]
	;ly=ly[sml]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	binsize=10.;datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	;Title=subT+YT+" vs "+XT

	;pos30=[      0.11562500   ,   0.12109375  ,    0.42187500   ,   0.40117188]
	pos3=[ 0.098687500   ,  0.083359375 ,     0.96531250     , 0.26422396]
	
	p3=scatterplot(X,OA,xtitle=xttl,ytitle=yttl,SYMBOL='.', /SYM_FILLED,sym_size=.75,layout=[1,4,4],/curr,xrange=[0,360],yrange=[0,4],sym_color='light slate grey');,yrange=[0,4],sym_color='light slate grey',ytext_color='goldenrod',ycolor='goldenrod');,/ylog,pos=pos3)
xl1= 0.083359375+.02
	xr1=0.94;6531250+.02
	;pos3=p3.pos
	plotlist3=list()
	p3pos=p3.pos
	p3pos[0]=xl1
	p3pos[2]=xr1
	p3pos[1]-=.015;+.05
	p3pos[3]+=.025;+.05;375
	p3.pos=p3pos
	if 0 then begin
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
						
		plotlist3.add,scatterplot(xsub,ysub,sym_size=.05,$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors2[KK],Name=seasonNames[KK],/overplot)
	endfor
	endif
	pNOR=scatterplot(X,Y,SYMBOL='.', /SYM_FILLED,sym_size=.75,/OVER,sym_color='gold',ytext_color='orange');,yrange=[0,4]);,/ylog,pos=pos3)
	medliner,X,OA,binsize,mlow,mhigh,medovershoot,errorbars
	peuNN=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_slate_grey,errorbar_color=!color.dark_slate_grey,name="Median",/overplot)
	
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	;plotlist3.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.dark_goldenrod,name="Median",/overplot)
	peNOR=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.red,errorbar_color=!color.red,name="Normalized Median",/overplot)
	
	raxis=axis('Y',LOCATION='right',TITLE=datA.ytitle,text_color='slate grey',target=peNOR)
	;(plotlist3[-1]).save,dire+'LsPlots2.png'
	raxis.save,dire+'LsPlots2.png'
	print,pos3
	p10.close
	;plotlist3[-1].save,Dire+'LS'
	;plotlist3[-1].close
	
	return
	pUNN=scatterplot(X,datA.y,SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color='steel_blue')
	pNOR=scatterplot(X,datAM.y,SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				sym_color='gold',yrange=[0,4],/over)
	medliner,X,datA.y,binsize,mlow,mhigh,medovershoot,errorbars
	peUN=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.blue,name="Median",/overplot,title=Title,thick=.5,errorbar_color='medium blue');,layout=[2,4,1]);,pos=[0.092187501  ,   
medliner,X,datAM.y,binsize,mlow,mhigh,medovershoot,errorbars
	peNOR=errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.orange,name="Median",/overplot,title=Title,thick=.5,errorbar_color='tan',yrange=[0,4]);,layout=[2,4,1]);,pos=[0.092187501  ,   
end
