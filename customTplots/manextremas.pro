pro manextremas,plt=plt,name=name
	
	if not keyword_set(plt) then plt='mvn_B_1sec_MAVEN_MSO_Mag'
	
	get_data,plt,data=dat
	get_data,'BbutterDeriv',data=datD
	if not keyword_set(name) then name='BbutterDeriv';plt
	get_data,'st_flags',data=datst
	get_data,'en_flags',data=daten
	ys=dat.y
	yD=datD.y
	yD20=yD;smooth(yD,10)
	sts=where(datst.y ne 0)
	ens=where(daten.y ne 0)
	N=n_elements(ys)
	z=fltarr(N)
	extrs=fltarr(N)
	mxs=fltarr(N)
	mns=fltarr(N)
	inmxs=mxs
	outmxs=mxs
	inmns=mns
	outmns=mns
	inextrs=mns
	outextrs=mns
	numcross=n_elements(sts)
	print,sts

	infirstflips=fltarr(N)
	outfirstflips=infirstflips
	print,''
	print,ens
	print,''

	DD=fltarr(N)
	nDD=fltarr(N)
	for i=0,numcross-1 do begin
		st=sts[i]
		en=ens[i]
		yst=ys[st]
		yen=ys[en]
		yb=min([yst,yen])
		yt=max([yst,yen])
		print,st,en
		ymx=max(yD[st:en],mxlc)
		mxlc+=st
		
		ymn=min(yD[st:en],mnlc)
		mnlc+=st
		if yst lt yen then begin
			inmxs[mxlc]=ymx
			inmns[mnlc]=ymn
			stp=st
			DD[st]=1
			while(yD20[stp] le 0 or stp lt mxlc) do stp++
			flp=(where(yD20[stp:*] le 0))[0]
			infirstflips[flp+stp]=1
		endif
		if yst gt yen then begin
			outmxs[mxlc]=ymx
			outmns[mnlc]=ymn
			enp=en
			nDD[en]=1
			while(yD20[enp] ge 0 or enp gt mnlc) do enp--
			flp=(where(yD20[0:enp] ge 0))[-1]
			outfirstflips[flp]=1

		endif

	endfor

	mxs=inmxs+outmxs
	mns=inmns+outmns
	inextrs=inmxs+inmns
	outextrs=outmxs+outmns
	extrs=inextrs+outextrs
	enttrgs=inmxs+outmns
	lvtrgs=inmns+outmxs
	
	datD.y=mxs
	store_data,name+'_maxs',data=datD
		dat.y=inmxs
	store_data,name+'_inmaxs',data=datD
	datD.y=outmxs
	store_data,name+'_outmaxs',data=datD
	datD.y=mns
	store_data,name+'_mins',data=datD
	datD.y=inextrs
	store_data,name+'_inextrema',data=datD
	datD.y=outextrs;mxs
	store_data,name+'_outextrema',data=datD
	datD.y=extrs
	store_data,name+'_extrema',data=datD
	datD.y=enttrgs
	store_data,name+'_enterflags',data=datD
	datD.y=lvtrgs
	store_data,name+'_leaveflags',data=datD
	datD.y=infirstflips+outfirstflips
	store_data,name+'_firstflips',data=datD

	datD.y=DD
	datD.ytitle='DD_flag'
	store_data,name+'_DD_flags',data=datD
	datD.y=nDD
	datD.ytitle='bDD_flag'
	store_data,name+'_bDD_flags',data=datD
end
