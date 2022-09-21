pro overbetaefit

	dire="Documents/Plots/CombinedPlots/"
	get_Data,14,data=datX
	bta=datX.y
	get_data,45,data=datA
	A=datA.y
	t=datA.x
	tsort=sort(t)
	Btasort=sort(bta)
	bta0=bta[btasort]

	
	A0=A[btasort]

	ppp=scatterplot(alog(bta),A)
	ppp.save,dire+'A_vs_logBetaE.png'
	;return
	help,A0
	help,bta0
	m2=0.
	m0=0.40652 *Exp(.955511*m2) 
	m1=.955511;.18
	;m2=-1;-1.35
	m3=0.24375;1.5
	MM=[m0,m1,m2,m3]
	help,MM
	print,MM
	lbta0=alog(bta0)
	lbta=alog(bta)
	weights=1./A0
	alogfit,lbta0,MM,F
	p1=scatterplot(lbta,A,sym_size=.1,xtitle='log($\beta$)',ytitle='A')
	;p3=plot(lbta0,F,color='green',/over,name='guess fit')
	yfit=curvefit(lbta0,A0,weights,MM,status=status,chisq=chisq, $
FUNCTION_NAME='expofit2') 

	MM2=[.2,-1.125,2,.396875]
	yfit2=curvefit(lbta0, A0, weight, MM2, SIGMA, FUNCTION_NAME='expofit',status=status2,CHISQ=CHISQ2)

	tm0=string(MM2[0],format='%0.2f')
	tm1=string(MM2[1],format='%0.2f')
	tm2=string(MM2[2],format='%0.2f')
	tm3=string(MM2[3],format='%0.2f')

	print,MM
;	print,MM[1]*2*!pi/365.
	sm0=string(MM[0],format='%0.2f')
	sm1=string(MM[1],format='%0.2f')
	sm2=string(MM[2],format='%0.2f')
	sm3=string(MM[3],format='%0.2f')

	name2=sm0+'(log(beta)-'+sm1+')^{'+sm2+'}+'+sm3 +' , CHI^2='+string(CHISQ2,format='%0.3f')
	name1=tm0+'*exp('+tm2+'(log(beta)-'+tm2+'))+'+tm3 +' , CHI^2='+string(CHISQ,format='%0.3f')

	denstr='('+sm0+'log('+sm1+'($\beta$-'+sm2+')+'+sm3
	denstr=denstr.replace('-+','-')
	denstr=denstr.replace('+-','-')
	denstr=denstr.replace('--','+')
	;p2=plot(lbta0,yfit,color='gold',/over,name='best fit')
	p2=plot(lbta0,yfit,'b-',/over,name=name1)
	p3=plot(lbta0,yfit2,'g-',/over,name=name2)
	t3=text(.1,.9,denstr,/over)
	ll=legend(target=[p3,p2])
	p2.save,Dire+'AvslogBetaE_fit.png'

	;alogfit,bta0,MM,F
	pp4=scatterplot(bta0,A)
	pp5=plot(bta0,yfit,'b-',/over,name=name1)
	pp5=plot(bta0,yfit2,'g-',/over,name=name2)
	return
	return

	p5=scatterplot(bta0,A0/F,sym_color='red',sym_size=.5)
	p5=scatterplot(bta0,A0/yfit,sym_size=.5)

	
	
	
	
	numerstr='(($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$)$\n$ ----------------------$\n$ '
	yt=numerstr+denstr
	yfit=yfit[tsort]
	;store_data,'solar_cycle-cos',data={x:datT.x,y:yfit,ytitle:denstr,YN:'Solar Cycle cos',fn:'SolarCycle',binsize:[1],radian:[0],degree:[0],vec:[0]}

	store_data,'overshootAmplitudeVsolarCycle',data={x:datX.x,y:A/yfit,ytitle:yt,YN:'$\beta_e$ normalized A',fn:'AvsBeta',binsize:[1],radian:[0],degree:[0],vec:[0]}

end
