pro shockcoordcalc2,plt,newname=newname
	get_data,"shocks",data=dats

	if total(dats.y) eq 0 then return
	get_data,plt,data=dat,alim=lim
	if not keyword_set(newname) then newname=plt+'_NIF2'
	;DownstreamMeasurer,'mvn_B_1sec_MAVEN_MSO',newname='Bd_vector'
	;get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
	;get_data,'Bd_vector',data=datBdv
	get_data,'Shock_Normal_AVG',data=datSN
	;Bdv=datBdv.y
	SN=datSN.y

	xs=dats.x
	GG=where(dats.y eq 1,gcount)
	if gcount eq 0 then return
	
	ys=dat.y


	get_data,"shock_locs",data=datsl;{x:xs,y:shockLocs,ytitle:'shock location'}

	shockloc=datsl.y
	N=numel(xs)

	ynew=nanarr(N,3)
	yframe=ynew
	
	get_data,'uplocs',data=datui
	ui=datui.y
	
	for i=0,N-1 do begin
		if shockloc[i] eq -1 then continue
		ishock=shockloc[i]
		yy=ys[i,*]
		st=min(ui[ishock,*])
		en=min(ui[ishock,*])
		N_shock=SN[ishock,*];fltarr(3)
		yuavg=[mean(ys[st:en,0]),mean(ys[st:en,1]),mean(ys[st:en,2])]
		vec=crossprod(N_shock,crossprod(yuavg,N_shock))
		for k=0,2 do begin
				yframe[i,k]=vec[k]
				ynew[i,k]=yy[k]-vec[k]
		endfor
	endfor


	dat.y=ynew

	store_data,newname,data=dat,dlim=lim
	;options,newname,'LABELS',['x','y','z']
	options,newname,'LABFLAG',1

	options,newname,'colors',['r','g','b']
	
	
		dat.y=ynew

	store_data,newname+'_frame',data=dat,dlim=lim
	;options,newname,'LABELS',['x','y','z']
	options,newname+'_frame','LABFLAG',1

	options,newname+'_frame','colors',['r','g','b']
end
