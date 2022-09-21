pro butterback
	get_data,'BbutterDeriv_DD_flags',data=datDD
	get_data,'BbutterDeriv_bDD_flags',data=datbDD
	get_data,'BbutterDSmoothed20',data=datBbd
	get_Data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
	B=datB.y
	Bs=smooth(B,10)
	nBs=reverse(Bs)
	Bbd=datBbd.y
	nBbd=-1*reverse(Bbd)
	DD=where(datDD.y ne 0,dcount)
	bDD=where(datbDD.y ne 0,ndcount)
	nDD=where(reverse(datbDD.y) ne 0,ndcount)
	N=numel(Bbd)
	BB=fltarr(N)
	nBB=fltarr(N)

	for i=0,dcount-1 do begin

		dim=DD[i]
		reachedpeak=0
		mx=Bbd[dim]
		tB=Bs[dim]
		for j=dim+1,N-1 do begin
			
			if total(j eq bDD) eq 1 then break
			bi=j
			if ~reachedpeak and Bbd[j] gt mx then begin
				mx=Bbd[j]
				continue
			endif
			if ~reachedpeak and Bbd[j] lt mx and Bs[j]-tB gt .5 and nBs[j]/tB gt 1.1 then begin
				reachedpeak=1
				continue
			endif
			if reachedpeak and Bbd[j] lt mx then begin
				;mx=Bbd[j]
				continue
			endif
			if reachedpeak and Bbd[j] ge mx then begin
				bi--
				break
			endif
		endfor
		BB[bi]=1

	endfor
	for i=0,ndcount-1 do begin

		dim=DD[i]
		reachedpeak=0
		mx=nBbd[dim]
		tB=nBs[dim]
		for j=dim+1,N-1 do begin
			if total(j eq N-1-DD) eq 1 then break
			bi=j
			if ~reachedpeak and nBbd[j] gt mx then begin
				mx=nBbd[j]
				continue
			endif
			if ~reachedpeak and nBbd[j] lt mx  and nBs[j]-tB gt .5 and nBs[j]/tB gt 1.1 then begin
				reachedpeak=1
				continue
			endif
			if reachedpeak and nBbd[j] lt mx then begin
				;mx=Bbd[j]
				continue
			endif
			if reachedpeak and nBbd[j] ge mx then begin
				bi--
				break
			endif
		endfor
		nBB[bi]=1

	endfor
	bBB=reverse(nBB)
	store_data,'BbutterDeriv_BB_flags',data={x:datDD.x,y:BB,ytitle:"BB flags"}
	store_data,'BbutterDeriv_bBB_flags',data={x:datDD.x,y:bBB,ytitle:"bBB flags"}
end
