function plotcorrelate,xplt,yplt,rank=rank,KENDALL=KENDALL,subset=subset,d=d,xlog=xlog,ylog=ylog,probd=probd,zd=zd,xinv=xinv,yinv=yinv

get_data,xplt,data=datx
	get_data,yplt,data=daty
	;help,datx.y
	x=datx.y
	y=daty.y
	if keyword_set(subset) then begin
		get_data,subset,data=datsub
		subflag=datsub.y
		sub=where(subflag eq 1)		
		x=x[sub]
		y=y[sub]
	endif
	if keyword_set(xinv) then x=1./x
	if keyword_set(yinv) then y=1./y
	if keyword_set(xlog) then x=alog(x)
	if keyword_set(ylog) then y=alog(y)
	
	if keyword_set(rank) then return,R_CORRELATE(x,y,d=d,probd=probd,zd=zd)
	return,correlate(x,y)
end
