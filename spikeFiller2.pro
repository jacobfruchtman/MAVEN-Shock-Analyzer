pro spikeFiller2, flg,side=side
	
	if not keyword_set(side) then side="end"

	
	if side ne "" then sd="_"+sd else sd=side

	get_data,flg,data=datFlag

	get_data,'sublengths_inbound'+sd,data=datin
	get_data,'sublengths_outbound'+sd,data=datout
	
	yf=datFlag.y

	N=numel(yf)

	yin=datin.y
	yout=datout.y

	ylen=yin+yout

	GG=where(yf ne 0,gcount)
	z=yin
	for i=1,N-1 do z[i]=0
	for el=0, gcount-1 do begin

		i=GG[el]
		yi=yf[i]
		iStart=i-1; where the sublength starts
		iEnd=i  ;where the sublength ends

		while ylen[iStart] ne 0 do istart--
		while ylen[iEnd] ne 0 do iEnd++
		;print,"[istart,iEnd]=",[istart,iEnd]
		z[max([iStart,0]):min([N,iEnd])-1]=yi
	endfor

	datFlag.y=z
	store_data,flg+"_flattened",data=datFlag

end
