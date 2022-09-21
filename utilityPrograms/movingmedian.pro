pro movingmedian, x,y,xbin,xmed,ymed,xmed2,ybin=ybin,std=std,ster=ster,yhist=yhist,quartiles=quartiles,geo=geo,grid=grid,men=men

	N=numel(x)
	xmed=x;nanarr(N)
	ymed=nanarr(N)
	xmed2=nanarr(N)
	yhist=nanarr(N)
	std=fltarr(N)
	ster=fltarr(N)
	quartiles=fltarr(N,2)
	
	for i=0,N-1 do begin

		tx=x[i]
		ty=y[i]
		
		w=where(abs(x-tx) le 0.5*xbin,nw)
		if keyword_set(ybin) then yhist[i]=total(abs(ty-y[w]) lt 0.5 *ybin)
		if nw ge 2 then begin
			;if i mod 100 eq 0 then print,i
			xw=x[w]
			yw=y[w]
			if keyword_set(geo) and keyword_set(ybin) then begin
				val=geometricmedian(xw,yw,[1,1],grid=grid)
				mx=val[0]
				my=val[1]
			
			
			endif else begin
				if not keyword_set(men) then begin
				mx=median(xw,/even)
				my=median(yw,/even)
				endif else begin
					mx=mean(xw)
					my=mean(yw)
				endelse
			endelse
			xmed2[i]=mx
			ymed[i]=my
			std[i]=stddev(yw)
			ster[i]=standarderror(yw)	
			;;TODO: quartiles			
		endif
		
	endfor
end

