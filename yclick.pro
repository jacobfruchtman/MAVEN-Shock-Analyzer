pro yclick,x,y,inds=inds,plt=plt,nan=nan
	
	ctime,t,vname=vname
	;help,t
	get_data,vname[0],data=dat
	xs=dat.x
	ys=dat.y
	REP=0
	N=N_elements(t)
	;ys=dat.y
	if keyword_set(plt) then begin

			get_data,plt,data=dat2
			xs=dat2.x
			ys=dat2.y
	endif else if N_elements(t) ne 1 and not arrayconst(vname) then REP=1
	inds=fltarr(N)
	x=fltarr(N)
	y=fltarr(N)
	dx=xs[1]-xs[0]
	for j=0, N_elements(t)-1 do begin
		print,j
		if j gt 0 and rep eq 1 then begin
			get_data,vname[j],data=dat
			xs=dat.x
			ys=dat.y

		endif
		if keyword_set(nan) then begin
			b=where(finite(ys) ne 1)
			xs[b]=!VALUES.F_NAN
		endif
		print,numel(xs),numel(dat.x),numel(ys),numel(dat.y)
		minloc=(where(min(abs(xs-t[j])) eq abs(xs-t[j]),nw))[0]
		;print,minloc
		y[j]=ys[minloc] 
		x[j]=xs[minloc] ;;for some reason, the wrong value is saved here. Why?
		;print,xs[minloc]-x[j],xs[minloc]-xs[0],x[j]-xs[0],t-xs[0]
		inds[j]=minloc
		;print,xs[minloc]-xs[minloc-1]
		;print,time_string(xs[minloc])
		;print,time_string(xs[minloc-1])
		;print,time_string(x[j])
		;xm=x[j]-(xs[minloc]-xs[minloc-1])
		;print,time_string(xm)
	endfor

end
