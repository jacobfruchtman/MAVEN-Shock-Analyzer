pro extremathreshplot,plt,bound

	get_data,plt,data=dat

	y=dat.y

	GG=where(abs(y) ge bound,gcount)
	z=y*0.0
	N=size(z,/n_el)
	for el=0,gcount-1 do begin
		i=GG[el]

		istart=max([0,i-10])
		iend=min([N-1,i+10])
		if abs(y[i]) eq max(abs(y[istart:iend])) then z[i]=y[i]
	endfor

	dat.y=z

	store_data,plt+"_extrema",data=dat

end
