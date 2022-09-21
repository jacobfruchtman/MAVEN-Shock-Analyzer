function hallstage2loop,B,med,DD,AA,ii,cond1,conds=conds
	N=numel(med)
	aj=AA[ii]
	wd=where(aj-DD gt 0,dc)
	if dc eq 0 then dim=max([0,aj-60*60]) else dim=max([(DD[wd])[-1],aj-60*60])
	conds=0
	for i=dim,min([aj+30,N-2-10*60]) do begin
		i2=i+5*60
		if ~cond1[i] then continue
		yboxmax=max(B[i:i2],mxlc)
		mxlc+=i
		conds=[1,1]
		cond2=1*( alog10(yboxmax/B[i]) gt 0.6)
		conds=[1,alog10(yboxmax/B[i])]
		if ~cond2 then continue

		i3=min([mxlc+5*60,N-1])
		;print,mxlc,i3,numel(B)
		y3=B[mxlc:i3]
		cond3=1*( total(y3 ge yboxmax) ge total(y3 lt yboxmax))
		conds=[2,total(y3 ge yboxmax)/total(y3 lt yboxmax)]
		if ~cond3 then continue  
		conds=[3,total(y3 ge yboxmax)/total(y3 lt yboxmax)]
		return,[i,i3]	
	endfor
	
	
	return,[-1,-1];shock
end


pro hallstage2

	get_data,'B_median',data=datB
	Bmed=datB.y
	get_data,'hall_1st_cond',data=datcond1
	cond1=datcond1.y
	get_data,'hallwind',data=dathw
	hmed=dathw.y
	
	get_data,'wind_shift_left',data=datWSL
	get_data,'wind_shift_right',data=datWSR

	AA=where(datWSL.y eq 1,acount)
	DD=where(datWSR.y eq 1,dcount)
	N=numel(Bmed)
	yy=fltarr(N)
	yflgs=fltarr(N,2)
	help,Bmed
	help,hmed
	help,DD
	help,AA
	help,cond1
	for i=0,acount-1 do begin
		val=hallstage2loop(Bmed,hmed,DD,AA,i,cond1,conds=conds)
		for k=val[0],val[1] do yy[k]=1
		yflgs[val[0],*]=val[0]
		yflgs[val[1],*]=val[1]
		print,yy[val[0]],val[0],val[1],conds
	endfor
	
	
	nBmed=reverse(Bmed)
	nhmed=reverse(hmed)
	ncond1=reverse(cond1)
	nAA=where(reverse(datWSR.y) eq 1,acount)
	nDD=where(reverse(datWSL.y) eq 1,dcount)
	nyy=fltarr(N)
	nyflgs=fltarr(N,2)
	for i=0,acount-1 do begin
		val=hallstage2loop(nBmed,nhmed,nDD,nAA,i,ncond1,conds=conds)
		if val[0] eq -1 then continue
		for k=val[0],val[1] do yy[k]=1
		yflgs[val[0],*]=val[0]
		yflgs[val[1],*]=val[1]
		print,yy[val[0]],val[0],val[1],conds
	endfor
	
	nyy=reverse(nyy)
	nyflgs=reverse(nyflgs)
	store_data,'hallshockreg_inbound',data={x:datB.x,y:yy,ytitle:'hall stage2 region inbound'}
	store_data,'hallshockflags_inbound',data={x:datB.x,y:yflgs,ytitle:'hall stage2 flags inbound'}	
	options,'hallshockflags_inbound','colors',['y','b']


	store_data,'hallshockreg_outbound',data={x:datB.x,y:nyy,ytitle:'hall stage2 region outbound'}
	store_data,'hallshockflags_outbound',data={x:datB.x,y:nyflgs,ytitle:'hall stage2 flags outbound'}	
	options,'hallshockflags_outbound','colors',['y','b']
	
	yy+=nyy
	yflgs+=nyflgs

	store_data,'hallshockreg',data={x:datB.x,y:yy,ytitle:'hall stage2 region'}
	store_data,'hallshockflags',data={x:datB.x,y:yflgs,ytitle:'hall stage2 flags'}	
	options,'hallshockflags','colors',['y','b']


end
