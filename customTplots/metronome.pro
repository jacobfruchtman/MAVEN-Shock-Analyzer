pro metronome

	get_data,'mvn_swica_density',data=dat

	xs=dat.x
	ys=0.0* dat.y

	N=size(xs,/n_el)


	for i=0, N-1 do begin
		if(i mod 10 eq 0) then ys[i]+=1
		if(i mod 100 eq 0) then ys[i]+=1
		if(i mod 1000 eq 0) then ys[i]+=1
		if(i mod 10000 eq 0) then ys[i]+=1
	endfor
	dat.y=ys
	dat.ytitle='metonome'
	store_data,'metro',data=dat
end
