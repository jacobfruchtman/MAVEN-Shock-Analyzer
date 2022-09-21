pro solarplot
	mn=.65
	mx=1.2
	m0=(mx-mn)/2*(-1)
	m3=(mx+mn)/2
	get_data,'tjul',data=datT
	T=datT.y
	get_data,45,data=datA
	A=datA.y
	jlen=julday(12,1,2019)-julday(12,1,2008)
	m1=(jlen)/(2.*!pi);11*365./(2*!pi)

	m2=julday(4,1,2014);julday(12,1,2008);julday(4,1,2014)-5.5*365

	MM=[m0,m1,m2,m3]
	print,MM
	weights=1./A
	cosfit2,T,MM,F
	p1=scatterplot(T,A,sym_size=.1,xtickunits="time",xtitle='time of crossing',ytitle='A')
	p3=plot(T,F,color='green',/over,name='guess fit')

	;Asmooth=smooth(A,180)
	movingmedian, t,A,90,xmed,ymed,xmed2
	p7=plot(T,ymed,color='red',/over,name='ymed')
	movingmedian, t,A,90,xmed,ymen,xmed2,/men
	p8=plot(T,ymen,color='blue',/over,name='ymen')
	yfit=curvefit(T,A,weights,MM,status=status,chisq=chisq, $
FUNCTION_NAME='cosfit2',fita=[1,1,1,1])
	m20=(mx-mn)/(T[-1]-T[0])
	MM2=[m20,T[0],1.,mn]
	yfit2=curvefit(T,A,weights,MM2,status=status2,chisq=chisq2, $
FUNCTION_NAME='expofit',fita=[1,1,1,1]) 
	print,status,status2
	print,chisq,chisq2
	print,MM
	print,MM[1]*2*!pi/365.
	print,MM2
	print,correlate(A,F)
	print,correlate(A,yfit)
	print,correlate(A,yfit2)
	p2=plot(T,yfit,color='gold',/over,name='best fit')
	p9=plot(T,yfit2,color='grey',/over,name='best fit2')
	ll=legend(target=[p3,p2,p7,p8,p9])
	p2.save,'SolarCycle_fit.png'
	p2.close

	;p5=scatterplot(T,A/F,sym_color='red',sym_size=.5)
	;p5=scatterplot(A,A/F,sym_color='red',sym_size=.5)
	;p5=scatterplot(T,A/yfit,sym_size=.5)
	;return
	caldat,MM[2],month,day,year
	
	n1=MM[1]*2*!pi/365.
	sm0=string(MM[0],format='%0.2f')
	sm1=string(n1,format='%0.2f')
	sm2='['+timestamp(day=day,month=month,year=year)+']'    ;string(MM[2],format='%0.2f')
	sm3=string(MM[3],format='%0.2f')
	denstr=sm0+'cos(2$\pi$ (t-'+sm2+')/('+sm1+' years))+'+sm3
	denstr=denstr.replace('-+','-')
	denstr=denstr.replace('+-','-')
	denstr=denstr.replace('--','+')
	
	m22=MM[2]-n1*365./2
	tm0=string(MM[0],format='%0.2f')
	tm1=string(n1,format='%0.2f')
	caldat,m22,month2,day2,year2
	tm2='['+timestamp(day=day2,month=month2,year=year2)+']'    ;string(MM[2],format='%0.2f')
	tm3=string(MM[3],format='%0.2f')
	denstr2=tm0+'cos(2$\pi$ (t-'+tm2+')/('+tm1+' years)-$\pi$)+'+tm3
	denstr2=denstr2.replace('-+','-')
	denstr2=denstr2.replace('+-','-')
	denstr2=denstr2.replace('--','+')
	
	
	numerstr='(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ '
	yt=numerstr+'('+denstr+')'

	store_data,'solar_cycle-cos',data={x:datT.x,y:yfit,ytitle:denstr,YN:'Solar Cycle cos',fn:'SolarCycle',binsize:[1],radian:[0],degree:[0],vec:[0]}

	store_data,'overshootAmplitudeVsolarCycle',data={x:datT.x,y:A/yfit,ytitle:yt,YN:'Solar Cycle normalized A',fn:'AvsSolarCycle',binsize:[1],radian:[0],degree:[0],vec:[0]}

	print,A[0],yfit[0],A[0]/yfit[0],(A/yfit)[0]
	mn=min(A,mnlc)
	print,A[mnlc],yfit[mnlc],A[mnlc]/yfit[mnlc],(A/yfit)[mnlc]
	mn=max(A,mnlc)
	print,A[mnlc],yfit[mnlc],A[mnlc]/yfit[mnlc],(A/yfit)[mnlc]
	return

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



	ww = WINDOW(DIMENSIONS=[1000,1200])
	tjstart=julday(1,1,2014)
	tjend=julday(12,30,2019)
	
	xrange=[tjstart,tjend]
	
	xl1= 0.083359375
	xr1=0.96531250
	datX=datT
	datY=datA
	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]

	X=datx.y

	XT=datx.YN[0]
	xttl=datx.ytitle[0]
	binsize=180;365;180;150;datx.binsize[0]
	;npos7=[xl1,.07,xr2,.20]
	plotlist1=list()
	p10=scatterplot(x,y,xtitle=xttl,ytitle=yttl,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.2 ,$
				layout=[1,3,1],/curr,xtickunits='time');,pos=npos7
	p1pos=p10.pos
	p1pos[0]=xl1
	p1pos[2]=xr1
	p1pos[1]-=.06
	p1pos[3]+=.03;375
	p10.pos=p1pos
	
	FOR KK=0,3 do begin
			xsub=X[seasonsubIndices[KK]]
			ysub=Y[seasonsubIndices[KK]]
						
			plotlist1.add,scatterplot(xsub,ysub,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.2 ,$
				sym_color=seasonColors[KK],Name=seasonNames0[KK],/over)

	endfor

	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist1.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,thick=.5);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist1.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5,xrange=xrange);,layout=[2,4,1]);,pos=[0.092187501  ,   


	;;;Avar
	;get_data,'overshootAmplitudeVsMflow',data=datAM
	;datY=datAM
	;Y=daty.y
	;YT=daty.YN[0]
	;yttl=daty.ytitle[0]
	plotlist2=list()
	p20=scatterplot(x,y,/curr,xtitle=xttl,ytitle=yttl,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.2 ,$
				layout=[1,3,2],xtickunits='time');,pos=npos7
	p2pos=p20.pos
	p2pos[0]=xl1
	p2pos[2]=xr1
	p2pos[1]-=.045
	p2pos[3]+=.045;375
	p20.pos=p2pos
	;FOR KK=0,3 do begin
	;		xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
						
	;		plotlist2.add,scatterplot(xsub,ysub,$
	;			SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
	;			sym_color=seasonColors2[KK],Name=seasonNames0[KK],/over)

	;endfor

	;FOR KK=0,3 do begin
		;	xsub=X[seasonsubIndices[KK]]
	;		ysub=Y[seasonsubIndices[KK]]
	;		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
	;		plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot,thick=.5);,pos=[0.092187501  ,    0.695  ,   0.44531250  ,    0.93046875])
	;endfor
	;medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	;plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,title=Title,thick=.5);,layout=[2,4,1]);,pos=[0.092187501  ,   
	p2=plot(T,yfit,color='gold',/over,name='best fit')
	ll=text(.09,.6,'solar fit='+denstr+'$\n$         ='+denstr2,color='gold',layout=[1,3,2],FONT_STYLE='bold')
	;;;;Mflow normalized
	get_data,'overshootAmplitudeVsMflow',data=datAM
	datY=datAM
	Y=daty.y
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
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
	;binsize=150.;datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	;Title=subT+YT+" vs "+XT
	xl1= 0.083359375
	xr1=0.96531250
	;pos30=[      0.11562500   ,   0.12109375  ,    0.42187500   ,   0.40117188]
	pos3=[ 0.098687500   ,  0.083359375 ,     0.96531250     , 0.26422396]
	p3=scatterplot(X,Y,xtitle=xttl,ytitle=yttl,SYMBOL='.', /SYM_FILLED,sym_size=.1,layout=[1,3,3],/curr,pos=pos3,xtickunits='time');,yrange=[0,4]);,/ylog)

	pos3=p3.pos
	plotlist3=list()
		p3pos=p3.pos
	p3pos[0]=xl1
	p3pos[2]=xr1
	p3pos[1]-=.030
	p3pos[3]+=.060;375
	p3.pos=p3pos
	
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
						
		plotlist3.add,scatterplot(xsub,ysub,sym_size=.2,$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors[KK],Name=seasonNames[KK],/overplot)
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist3.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,xrange=xrange)

	(plotlist3[-1]).save,dire+'SolarPlots.png'
	print,pos3
	p10.close
	;plotlist3[-1].save,Dire+'LS'

end
