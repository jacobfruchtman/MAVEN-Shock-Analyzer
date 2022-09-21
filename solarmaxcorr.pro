pro solarmaxcorr,plt=plt
	dire="Documents/Plots/CombinedPlots/"
	if not keyword_set(plt) then plt='up'
	get_data,plt,data=dat
	
	t=dat.x
	y=dat.y
	
	mn=10.7
	mx=11.1
	st=time_double('2014-04-01')
	en=time_double('2014-04-30')
	rng=(en-st)/(24*3600.)
	krange=(mx-mn)*100/2.
	;;r , tstart ,kk
	N=numel(t)
	corrs=fltarr(rng*50,3)
	for i=0,rng-1 do begin
		tstart=st+i*(24*3600.)
		for k=1070./2,1110/2-1 do begin
			kk=k/50.
			per=!pi*2/(kk*365*24*3600.)
			x=-1*cos(per*(t-tstart))
			r=abs(correlate(x,y))
			corrs[i,0]=r;[r,tstart,kk]
			corrs[i,1]=tstart
			corrs[i,2]=kk
		endfor
	endfor
	for i=0,rng*50-1 do print,corrs[i,0],corrs[i,1],corrs[i,2]
	rs=corrs[*,0]
	tstarts=corrs[*,1]
	pers=corrs[*,2]
	mxcorr=max(rs,mxlc)
	beststart=tstarts[mxlc]
	bestper=pers[mxlc]
	per=!pi*2/(bestper*365*24*3600.)
	xx=-1*cos(per*(t-beststart))
	print,"best correlation r=",mxcorr," when period=",bestper," yrs and solar maximum at ",time_string(beststart)
	p1=scatterplot(t,y ,xtitle='time',sym_size=.1,xtickunits="time",ytitle=dat.ytitle)
	p11=scatterplot(t,xx ,xtitle='time',sym_size=.1,xtickunits="time",ytitle='cycle')
	p2=scatterplot(xx,y,ytitle=dat.ytitle)
	p2.xtitle='-1*cos($2\pi$(t-['+time_string(beststart)+'])/('+strtrim(bestper,2)+' yrs)) , r='+string(mxcorr,format='%0.3f')
	p2.save,dire+dat.fn+'_solarcycleFit.png'
	p2.close
	store_data,plt+'_solarcycletest',data={x:t,y:xx,ytitle:'-1*cos($2\pi$(t-['+time_string(beststart)+'])/('+strtrim(bestper,2)+' yrs))',YN:'solar cosine',fn:dat.fn+'_solarcos',binsize:[.1],radian:[0],degree:[0],vec:[0]}
	
end
