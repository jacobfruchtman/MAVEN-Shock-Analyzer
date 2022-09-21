pro fracdiffchangeplot,plt,newname=newname,zeroNAN=zeroNAN,dt=dt
	if not keyword_set(dt) then dt=1
	if not keyword_set(newname) then newname=plt+'_fdiffchange'
	get_data,plt,data=dat

	ys=dat.y
	N=numel(ys)
	if keyword_set(zeroNAN) then for i=0,N-1 do if ys[i] eq 0 then ys[i]=!VALUES.F_NAN
	yl=shift(ys,-dt)
	yr=shift(ys,dt)
	fyr=fracdiff(ys,yr)
	store_data,newname+'_right',data={x:dat.x,y:fyr}
	fyl=fracdiff(ys,yl)
	store_data,newname+'_left',data={x:dat.x,y:fyl}
	if dt eq 1 then begin
	ylh=fltarr(N)
	for i=0,N-1 do ylh[i]=(yl[i]+ys[i])/2.0
	yrh=fltarr(N)
	for i=0,N-1 do yrh[i]=(yr[i]+ys[i])/2.0
	endif else begin
		ylh=shift(ys,-dt/2.)

		yrh=shift(ys,dt/2.)


	endelse
	fym=fracdiff(yl,yr)
	store_data,newname+'_mid',data={x:dat.x,y:fym}
	fymh=fracdiff(ylh,yrh)
	store_data,newname+'_midhalf',data={x:dat.x,y:fymh}
end
