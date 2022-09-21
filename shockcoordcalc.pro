pro shockcoordcalc,plt,newname=newname
	get_data,"shocks",data=dats

	if total(dats.y) eq 0 then return
		get_data,plt,data=dat,alim=lim
	if not keyword_set(newname) then newname=plt+'_NIF'
	;DownstreamMeasurer,'mvn_B_1sec_MAVEN_MSO',newname='Bd_vector'
	get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
	get_data,'Bd_vector',data=datBdv
	get_data,'Shock_Normal_AVG',data=datSN
	Bdv=datBdv.y
	SN=datSN.y

	xs=dats.x
	GG=where(dats.y eq 1,gcount)

	
	ys=dat.y


	get_data,"shock_locs",data=datsl;{x:xs,y:shockLocs,ytitle:'shock location'}

	shockloc=datsl.y
	N=numel(xs)

	ynew=nanarr(N,3)

	for i=0,N-1 do begin
	if shockloc[i] eq -1 then continue
	ishock=shockloc[i]
	N_shock=fltarr(3)
	BD=fltarr(3)
	N_Bd=fltarr(3)
	;N_3=fltarr(3)
	y=fltarr(3)
	for j=0,2 do begin
		N_shock[j]=SN[ishock,j]
		BD[j]=Bdv[ishock,j]
		
		y[j]=ys[i,j]
		if newname eq 'POS_NIF' then y[j]-=ys[ishock,j]
	endfor
	N_Bd=BD-vecdotproduct(BD,N_shock)*N_shock
	N_Bd/=norm(N_Bd)
	N_3=transpose(vecCrossProduct(N_shock,N_Bd))
	z=[vecdotproduct(y,N_shock),vecdotproduct(y,N_Bd),vecdotproduct(y,N_3)]
	for j=0,2 do ynew[i,j]=z[j]
	endfor


	dat.y=ynew

	store_data,newname,data=dat,dlim=lim
	options,newname,'LABELS',['N','B_d','3rd']
	options,newname,'LABFLAG',1

	options,newname,'colors',['r','g','b']
end
