pro randomorderer
	tplot_element,'season','y',season
	tplot_element,45,'y',A
	lst1=list()
	lst2=list() 
	w=where(season eq 3 or season eq 1,ns)
	A=A[w]
	nsu=1146
	nwi=698
	for i=0,nwi do begin
		nss=nwi+nsu
		rnd=floor(nss*(randomu(seed)))
		el=A[rnd]
		ww=where(A ne el)
		A=A[ww]
		lst1.add,el
		nwi--
	endfor
	
	WI=lst1.toarray()
	SU=A
	hw = HISTOGRAM(WI, LOCATIONS=wbin,binsize=.2)
	p1=plot(wbin,hw/(698.),/hist,color='blue')
	hs = HISTOGRAM(SU, LOCATIONS=sbin,binsize=.2)
	p2=plot(sbin,hs/(1146.),/hist,color='red',/over)
	rs=rs_test(SU,WI,UX=U1,UY=U2)
	print,rs
	print,U1
	print,U2
end
