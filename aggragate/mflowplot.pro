pro mflowplot
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
yfit=curvefit(Mflow0, OA0, weight, MM, SIGMA, FUNCTION_NAME='expofit',status=status,CHISQ=CHISQ)
help,yfit
;p1=scatterplot(Mflow0,OA0,/over)
;p2=plot(Mflow0,yfit,'b-',/over)
;p4=scatterplot(Mflow0,OA0/yfit)
tsort=sort(t0)
yfit=yfit[tsort]
print,total(Mflow-Mflow0[tsort])
print,total(OA-OA0[tsort])
;p5=scatterplot(Mflow,OA/yfit)
print,MM
print,CHISQ

Asort=sort(OA)

A1=OA[Asort]
tA=t[Asort]

for i=0,20 do print,A1[i],", at ", time_string(tA[i])
print,'====='
for i=0,20 do print,A1[-i],", at ", time_string(tA[-i])
;return
AM=OA/yfit


store_data,'overshootAmplitudeVsMflow',data={x: t ,y: AM ,ytitle:[ '(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ $(.1(M_{flow}+.67)^{1.40}-.074)$' ], YN:[ "$M_{flow}$ normalized A" ], fn:[ "Normalized_OvershootHeightvMflow" ], binsize:[ .1 ], radian:[0],degree:[0]}

	get_data,'overshootAmplitudeVsMflow',data=datAM

	;;;[A vs Mflow : Mflow Projection]
	;;;[        AM vs LS             ]


	;;;A vs Mflow

	datX=datM
	datY=datA 
	seasonColors=list("green","red","orange","blue")
	seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
	seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")
	
	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	X=datx.y
	Y=daty.y
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	binsize=datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)

	get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)

					seasonsubIndices=list(springIndices,summerIndices,$	
					autumnIndices,winterIndices)
	Title=subT+YT+" vs "+XT
	p1=scatterplot(X,Y,xtitle=xttl,ytitle=yttl,title=Title,SYMBOL='.', /SYM_FILLED,sym_size=.1,layout=[2,2,1])

	plotlist1=list()
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
						
		plotlist1.add,scatterplot(xsub,ysub,sym_size=.1,$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors2[KK],Name=seasonNames[KK],/overplot)
	endfor
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
		medliner,xsub,ysub,binsize,mlow,mhigh,medovershoot,errorbars
		plotlist1.add,plot((mlow+mhigh)/2.0, medovershoot,color=seasonColors[KK],name="Median "+seasonNames0[KK],/overplot)
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist1.add,plot((mlow+mhigh)/2.0, medovershoot,color=!color.slate_grey,name="Median",/overplot)


	;;;Projection

	datX=datXmso
	datY=datRmso
	zttl=datM.ytitle[0]
	Z=datM.y
	zbyt=bytscl(Z)
	zmin=min(Z)
	zmax=max(Z)
	X=datx.y
	Y=daty.y
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]
	p2=scatterplot(X,Y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				/curr,layout=[2,2,2],RGB_TABLE=72)
	p2.aspect_ratio=1

	cb2=colorbar(ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl,target=p2);,pos=cposlist[lnum-1]););pos=cpos1)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,layout=[2,2,2],target=p2)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p2)

	;;; vs Ls

	datY=datAM
	datX=datLs
	X=datx.y
	Y=alog(daty.y)
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl='log('+daty.ytitle[0]+')'
	xttl=datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	binsize=10.;datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	Title=subT+YT+" vs "+XT
	;pos30=[      0.11562500   ,   0.12109375  ,    0.42187500   ,   0.40117188]
	pos3=[      0.13562500   ,   0.12109375  ,    0.9   ,   0.40117188]
	p1=scatterplot(X,Y,xtitle=xttl,ytitle=yttl,title=Title,SYMBOL='.', /SYM_FILLED,sym_size=.1,layout=[2,2,3],/curr,pos=pos3)

	pos3=p1.pos
	plotlist2=list()
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
						
		plotlist2.add,scatterplot(xsub,ysub,sym_size=.05,$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors2[KK],Name=seasonNames[KK],/overplot)
	endfor
	medliner,X,Y,binsize,mlow,mhigh,medovershoot,errorbars
	plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,thick=.5)

	(plotlist2[-1]).save,dire+'MflowPlots.eps'
	print,pos3
	p1.close
	return
	;;;ZOOMED IN for abs(AM) lt 100


	W=where(abs(Y) le 20)
	;X=X[W]
	;Y=Y[W]
seasonsubIndices=list(intersect(springIndices,W),intersect(summerIndices,W),$	
					intersect(autumnIndices,W),intersect(winterIndices,W))
	p4=scatterplot(X[W],Y[W],xtitle=xttl,ytitle=yttl,title='Zoomed to show abs(Y)<20',SYMBOL='.', /SYM_FILLED,sym_size=.1,layout=[2,2,4],/curr)

	pos4=p4.pos
	plotlist2=list()
	for KK=0,3 do begin
		xsub=X[seasonsubIndices[KK]]
		ysub=Y[seasonsubIndices[KK]]
						
		plotlist2.add,scatterplot(xsub,ysub,sym_size=.05,$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors2[KK],Name=seasonNames[KK],/overplot)
	endfor
	medliner,X[W],Y[W],binsize,mlow,mhigh,medovershoot,errorbars
	plotlist2.add,errorplot((mlow+mhigh)/2.0, medovershoot,errorbars,color=!color.slate_grey,name="Median",/overplot,thick=.5)

	(plotlist2[-1]).save,dire+'MflowPlots.eps'
	print,pos4

end
