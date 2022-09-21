function plotcorrel2string,plt1,plt2,rank=rank,sub=sub,xlog=xlog,ylog=ylog
	r=plotcorrelate(plt1,plt2,rank=rank,sub=sub)
	;r=correl2string(x,y,rank=rank,sub=sub)
	return,string(r[0],format='%0.3g')
end
function correl2string2,xval,yval,subset=subset,rank=rank,kendall=kendall,rr=rr,xlog=xlog,ylog=ylog
	
	xx=xval
	yy=yval
	if keyword_set(xlog) then xx=alog(xx)
	if keyword_set(ylog) then yy=alog(yy)
	if keyword_set(subset) then begin
		get_data,subset,data=datS
		wsub=where(datS.y eq 1)
		xx=xx[wsub]
		yy=yy[wsub]
	endif
	if keyword_set(rank) then r=(r_correlate(xx,yy,kendall=kendall))[0] else r=correlate(xx,yy)
	rr=r
	return,string(r,format='%0.3f')
end

function correaggraline,plt1,plt2,param1=param1,param2=param2,shortline=shortline,corrs=corrs,sep=sep,full=full,conic=conic,xlog=xlog,ylog=ylog
	;paramlist1=list('$M_{fms}$','$\theta_{NB}$','$\beta$','proton $\beta$','electron $\beta$','Sol distance',$
	;'standoff $\Delta$','SZA','$\theta_{NV}$','RH prediction','$\Delta L_S$ from $L_S=270^\circ$','$M_{Alfven}$','M_{ms1}')
	;paramtplot=[8,10,12,13,14,35,32,98,80,51,149,75,84]
	;linelist1=list()
	;plt2=43
	if not keyword_set(sep) then sep='&'
	get_data,plt1,data=dat1
	get_data,plt2,data=dat2
	IF NOT KEYWORD_SET(param1) then param1=plt1	
	IF NOT KEYWORD_SET(param2) then param2=plt2
	x=dat1.y
	y=dat2.y
	rlist=list()
	srlist=list()
	rstring=param1
	klist=list()
	keys='param1'
	constr=""
	if keyword_set(conic) then constr="Conic"
	;keys=param
	;for i=0,numel(paramtplot)-1 do begin
	numr=4		
		;r=correlate(x,y)
		srlist.add,correl2string2(x,y,rr=rr,xlog=xlog,ylog=ylog)
		rlist.add,rr
		srlist.add,correl2string2(x,y,/rank,rr=rr,xlog=xlog,ylog=ylog)
		rlist.add,rr
		if not keyword_set(shortline) then begin
			if keyword_set(full) then begin
					;klist.add,param2
					rstring=rstring+sep+param2
			endif
			srlist.add,correl2string2(x,y,sub='autumn',rr=rr,xlog=xlog,ylog=ylog)
			klist.add,'autumn'
			rlist.add,rr
			srlist.add,correl2string2(x,y,sub='winter',rr=rr,xlog=xlog,ylog=ylog)
			klist.add,'winter'
			rlist.add,rr
			srlist.add,correl2string2(x,y,sub='spring',rr=rr,xlog=xlog,ylog=ylog)
			klist.add,'spring'
			rlist.add,rr
			srlist.add,correl2string2(x,y,sub='summer',rr=rr,xlog=xlog,ylog=ylog)
			klist.add,'summer'
			rlist.add,rr
			numr=8		

		endif else rstring=rstring+sep+param2
		if rstring ne param1 then keys+=',param2
		keys+=':: r  , rho  '
		
		srlist.add,correl2string2(x,y,sub='Quasiperp'+constr,rr=rr,xlog=xlog,ylog=ylog)
		klist.add,'Qperp'
		rlist.add,rr		
		srlist.add,correl2string2(x,y,sub='Quasipar'+constr,rr=rr,xlog=xlog,ylog=ylog)
		rlist.add,rr
		klist.add,'Qpar'
		foreach el,klist do keys+=','+el
		foreach el,srlist do rstring=rstring+sep+el
		corrs=rlist
		if keyword_set(full) then print,keys
		return,rstring
	end
