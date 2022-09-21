function hallstage2loop,B,med,DD,AA,ii,cond1

	aj=AA[ii]
	wd=where(aj-DD gt 0,dc)
	if dc eq 0 then dim=max([0,aj-60*60]) else dim=max([(DD[wd])[-1],aj-60*60])
	
	for i=dim,min([aj+30,N-1-10*60]) do begin
		i2=i+5*60
		if ~cond1[i] then continue
		yboxmax=max(B[i:i2],mxlc)
		mxlc+=i
		cond2=1*( alog10(yboxmax/B[i]) gt 0.6)
		if ~cond2 then continue
		i3=mxlc+5*60
		y3=B[mxlc,i3]
		cond3=1*( total(y3 gt yboxmax) gt total(y3 lt yboxmax))
		if ~cond3 then continue  
	
		return,[i,i3]	
	endfor
	
	
	return,[-1,-1];shock
end
