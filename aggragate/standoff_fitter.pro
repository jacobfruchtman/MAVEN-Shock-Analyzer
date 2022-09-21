pro standoff_fitter
		dire="Documents/Plots/CombinedPlots/"
	seasonColors=list("green","red","goldenrod","blue")
	seasonColors2=list("spring_green","orange_red","gold","blue")
	seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")

	seasonNames0=list("Spring","Summer","Autumn","Winter")
	;seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
	get_data,'Ls',data=datLS
	LS=datLS.y
	get_data,32,data=datY
	OA=datY.y


	R_mars=3389.5

	get_data,'Mflow',data=datM

	Mflow=datM.y
	datM.YN[0]='$M_{ms1}$'
	;datY.Ytitle= '$(\Delta/R_{Mars})$'
	datLs.YN='$\Delta/R_{Mars}$'
	yt='$(\Delta/R_{Mars})$'
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
	MM=[-.2,0,1.,3.2]
	expofit,Mflow0,MM,F
	;p3=plot(Mflow0,F,'g-')
	use1=1
	yfit=curvefit(Mflow0, OA0, weight, MM, SIGMA, FUNCTION_NAME='expofit',status=status,CHISQ=CHISQ)
	;F=.1*exp(.35*(Mflow+.1))+.1
	MM2=[55.714,-1.23,0.52,1.5685]
	expofit,Mflow0,MM,F
	yfit2=curvefit(Mflow0, OA0, weight, MM2, SIGMA, FUNCTION_NAME='expofit2',status=status,CHISQ=CHISQ2)
	print,'CHISQ,CHISQ2=',CHISQ,",",CHISQ2	
	p1=scatterplot(Mflow0,OA0,xtitle='$M_{flow}$',ytitle=datY.Ytitle,sym_size=.2)

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
	p2=plot(Mflow0,yfit,'b-',/over,name=name1)
	p3=plot(Mflow0,yfit2,'g-',/over,name=name2)

	ll=legend(target=[p2,p3])
	ll.save,Dire+'Standoff_vs_MflowFits.png',res=600
	p3.close
	
	p4=scatterplot(Mflow0,OA0/yfit,sym_color='blue')
	p5=scatterplot(Mflow0,OA0/yfit2,sym_color='green')
	if CHISQ gt CHISQ2 then begin
	yfit=yfit2
	use1=0

	MM=MM2
	;p1=scatterplot(Mflow0,OA0,/over)

	;p2=plot(Mflow0,yfit,'b-',/over)

	;return
	endif
;	help,yfit
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

	;	for i=0,20 do print,A1[i],", at ", time_string(tA[i])	
	;	print,'====='
	;for i=0,20 do print,A1[-i],", at ", time_string(tA[-i])
	;return
	AM=OA/yfit
	sm0=string(MM[0],format='%0.2f')
	sm1=string(MM[1],format='%0.2f')
	sm2=string(MM[2],format='%0.2f')
	sm3=string(MM[3],format='%0.2f')
	if use1 then denstr='$('+sm0+'(M_{flow}-'+sm1+')^{'+sm2+'}+'+sm3+')$' else denstr='$('+sm0+'exp('+sm2+'(M_{flow}-'+sm2+'))+'+sm3+')$'
	denstr=denstr.replace('-+','-')
	denstr=denstr.replace('+-','-')
	denstr=denstr.replace('--','+')
	numerstr='$(\Delta/R_{Mars})\n$ ----------------------$\n$ '
	yt=numerstr+denstr
	;'(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ $(.1(M_{flow}-.79)^{1.36}-.028)$'
	
	store_data,'StandoffVsMflow',data={x: t ,y: AM ,ytitle:[yt], YN:[ "$M_{flow}$ normalized $\Delta/R_{Mars}$" ], fn:[ "Normalized_StandoffvMflow" ], binsize:[ .1 ], radian:[0],degree:[0]}
end
