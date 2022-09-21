
function subextrema,intrvl,xs, ys,ys2=ys2,doPlot=doPlot,scale=scale

	if not keyword_set(scale) then scale=4.
	imin=intrvl[1]
	if intrvl[1] eq intrvl[0] then imin=mean([imin,intrvl[2]])
	yp=ys[imin:intrvl[3]]
	xp=xs[imin:intrvl[3]]
	x00=xs-xs[0]
	extr=EXTREMA(yp)

	extr=extr[where(intrvl[3]-extr gt 60)]
	ye0=yp[extr]
	g=where(abs(ye0) gt max(ye0)/scale)

	ms=extr[g]
	
	

	if keyword_set(doPlot) then begin
		xms=xp[ms]
		yms=yp[ms]

		p1=plot(xp,yp)

		p2=scatterplot(xms,yms,SYMBOL='star', /SYM_FILLED,sym_color="red", /overplot) 


		if keyword_set(ys2) then begin
			yp2=ys2[imin:intrvl[3]]
			y2ms=yp2[ms]
			p1=plot(xp,yp2)

			p2=scatterplot(xms,y2ms,SYMBOL='star', /SYM_FILLED,sym_color="red", /overplot) 
		endif
	endif
		

	return,ms+imin
end

pro intervalextremafinder,plt,plt2=plt2,doPlot=doPlot,scale=scale

	get_data,"shockInterval",data=datInt

	get_data,plt,data=dat

	if keyword_set(plt2) then begin

		get_data,plt2,data=dat2

		ys2=dat2.y
		nys2=reverse(ys2)
	endif
	ys=dat.y
	xs=dat.x
	nys=reverse(ys)*(-1)

	extremaf=fltarr(numel(ys))
	extremab=fltarr(numel(ys))
	fl=datInt.forwardList
	bl=datInt.backwardList
	help,fl
	help,bl
	;print,fl
	;print,bl
	;print, size(fl,/dim) 
	;print, size(bl,/dim) 
	Nf=numel(fl[*,0])
	Nb=numel(bl[*,0])

	

	if size(fl,/n_dim) ne 1  then begin
		for i=0,Nf-1 do begin

		intrvl=fl[i,*]
		catch,error_status
		if error_status ne 0 then begin
			;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG

			break
		endif
		extrms=subextrema(intrvl,xs, ys,ys2=ys2,doPlot=doPlot,scale=scale)

		foreach el,extrms do if fl[i,3] -el gt 60 then extremaf[el]=sign(ys[el])

		endfor
	endif

	if size(bl,/n_dim) ne 1 then begin
	for i=0,Nb-1 do begin

		intrvl=bl[i,*]
		catch,error_status
		if error_status ne 0 then begin
			;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG

			break
		endif
		extrms=subextrema(intrvl,xs, nys,ys2=nys2,doPlot=doPlot,scale=scale)

		foreach el,extrms do if bl[i,3] -el gt 60 then extremab[el]=sign(nys[el])

	endfor
	endif
	extremab=reverse(extremab)*(-1)

	store_data,plt+"_extrema_inbound",data={x:xs,y:extremaf,ytitle:"extrema"}
	
	store_data,plt+"_extrema_outbound",data={x:xs,y:extremab,ytitle:"extrema"}

	store_data,plt+"_extrema",data={x:xs,y:extremaf+extremab,ytitle:"extrema"}


end 
	
