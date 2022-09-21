function movinghist,x,y,xbin,ybin

	N=n_elements(x)

	yhist=fltarr(N)
	for i=0,N-1 do begin

		tx=x[i]
		ty=y[i]
		w=where( sqrt( (tx-x)^2 /xbin^2 +(ty-y)^2 /ybin^2) le 1,nw)
		yhist[i]=nw 
	endfor

	return,yhist
end
