pro manfitting2,plt1,plt2,curveType,MM,fita=fita,aggtplot=aggtplot
	get_data,plt1,data=dat1
	x=dat1.y
	t=dat1.x
	get_data,plt2,data=dat2
	y=dat2.y
	tsort=sort(t)
	
	Xsort=sort(X)
	X0=X[Xsort]
	Y0=Y[Xsort]
	t0=t[Xsort]
	weight=1.0/Y0
	;curveType='expofit'
	;MM=[.1,.66,2,-.26]
	;F1=4.11*exp(X0)-4.1
	MM0=MM
	if curveType eq 'expofit' then expofit,X0,MM,F else expofit2,X0,MM,F
	
	;p3=plot(x0,F1,'g-',/over)
	use1=1
	p3=plot(x0,F,color='orange',/over)
	p1=scatterplot(x0,y0,sym_size=.5,/over)
	;return
	yfit=curvefit(x0, y0, weight, MM0, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ,fita=fita,itmax=100)
	p1=scatterplot(x0,y0,sym_size=.5,/over)
	p2=plot(x0,yfit,color='green',/over)
	
	print,CHISQ,status
	print,MM0
	N=numel(yfit)
	p4=scatterplot(X0,Y0/yfit,sym_size=.5,sym_color='green',/xlog)
	p5=scatterplot(X0,Y0/F,sym_size=.5,sym_color='orange',/xlog)
	bn=(Y0/yfit)[(N-1)/20]
	if keyword_set(aggtplot) then begin

		sm0=string(MM0[0],format='%0.2f')
		sm1=string(MM0[1],format='%0.2f')
		sm2=string(MM0[2],format='%0.2f')
		sm3=string(MM0[3],format='%0.2f')
		if curveType eq 'expofit' then denstr='$('+sm0+'('+plt1+'-'+sm1+')^{'+sm2+'}+'+sm3+')$' else denstr='$('+sm0+'exp('+sm2+'('+plt1+'-'+sm2+'))+'+sm3+')$'
		denstr=denstr.replace('-+','-')
		denstr=denstr.replace('+-','-')
		denstr=denstr.replace('--','+')
		denstr=denstr.replace('-0)','')
		denstr=denstr.replace('+0)','')
		denstr=denstr.replace('(1.00(','((')
		ytitle=dat2.ytitle +"$!C$-----------------$!C$"+denstr
		yfit=yfit[tsort]
		Y2=Y/yfit
		dat={x:t,y:Y2,ytitle:ytitle,YN:dat2.yn+' normalized',fn:dat2.fn+'-Over'+dat1.fn+'fit',binsize:bn,radians:0,degree:0}
		store_data,aggtplot,data=dat
	endif
end
