pro extremablock,plt,trigplot,newName,ytitle=ytitle

	if not keyword_set(newName) then newName=plt+"_extremaBlock"
	if not keyword_set(ytitle) then ytitle="(dBu-dBd)*blockwidth !C [nT s]"
	get_data,plt,data=dat
	get_data,trigplot,data=dattrig

	y=dat.y

	;help,dattrig
	trig=dattrig.y
	N=numel(y)
	NAN=!VALUES.F_NAN
	z=nanarr(N)
	wherefin=where(trig ne 0 and finite(trig) eq 1,complement=wherenot)
	foreach el, wherenot do trig[el]=0
	;foreach el, wherenot do y[el]=0
	if wherefin[0] eq -1 then store_data,newName,data={x:dat.x,y:z}
	firstfin=(wherefin)[0]
	finalfin=wherefin[-1]
	starton=firstfin
	endon=firstfin
	ison=1
	ii=finalfin
	for i=firstfin,finalfin do begin
		if (ison xor trig[i] ne 0) eq 0 and i ne finalfin then continue
		if ~ison and trig[i] ne 0 then begin
			starton=i
			ison=1
			continue
		endif
		if ison and trig[i] eq 0 then begin
			ii=i-1
			ymax=max(abs(y[starton:ii]))
			for j=starton,ii do z[j]=ymax*trig[ii]
			ison=0
			continue
		endif
		if ison and i eq finalfin then begin
			ymax=max(abs(y[starton:i]))
			for j=starton,ii do z[j]=ymax*trig[i]
		endif
	endfor

	store_data,newName,data={x:dat.x,y:z,ytitle:ytitle}

	end
