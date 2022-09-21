pro cyclotronaverager,plt=plt,newNames=newNames,onlyFirst=onlyFirst

	if not keyword_set(plt) then plt="mvn_B_1sec_MAVEN_MSO_Mag"
	get_data,"proton_cyclotron_period",data=datTau
	;if not keyword_set(newName) then newName=plt+"_modal_average"
	get_data,plt,data=dat

	ys=dat.y
	tau=datTau.y
	N=numel(tau)
	rad=tau/2.0

	firstHarm=tau
	secondHarm=rad*3
	thirdHarm=rad*4

	zstd=fltarr(N); we want to find out when the magnetic field stops being monochromatic. 
	zmean=zstd
	zstdn=zstd

	nzstd=zstd
	nzmean=zstd
	nzstdn=zstd

	harms=fltarr(N,3)

	harms[*,0]=firstHarm ; the first
	harms[*,1]=secondHarm
	harms[*,2]=thirdHarm
	nHarms=reverse(harms)
		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'bDD_flag',data=datbDD

		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock


		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan, COMPLEMENT=nH_C, NCOMPLEMENT=nhcount_c); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn, COMPLEMENT=nK_C, NCOMPLEMENT=nkcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn, COMPLEMENT=nL_C, NCOMPLEMENT=nlcount_c); end of  negative  shock

		if DD[0] gt AA[0] then begin
			DD=[0,DD]
			ddn++
		endif
		if nDD[0] gt nAA[0] then begin
			nDD=[0,nDD]
			nddn++
		endif
		if BB[-1] lt AA[-1] then begin
			BB=[BB,N-1]
			bbn++
		endif
		if BB[0] lt AA[0] then begin
			BB=BB[1:*]
			bbn--
		endif

		if nBB[0] lt nAA[0] then begin
			nBB=nBB[1:*]
			nbbn--
		endif
		if nBB[-1] lt nAA[-1] then begin
			nBB=[nBB,N-1]
			nbbn++
		endif

	z=fltarr(N,3)

	zout=z

	numlooped=0

	fstlst=list()

	if keyword_set(onlyFirst) then begin

		zz=fltarr(N)
		for i=0, N-1 do begin
			if tau[i] ne 0 then  begin
				beg=max([i-tau[i]/2,0])
				fin=min([i+tau[i]/2,N-1])
				zz[i]=mean(ys[beg:fin],/NAN)
				;numlooped++
			endif
			;;zstd[i]=stddev(z[i,*])
			;zmean[i]=mean(z[i,*])
			;zstdn[i]=zstd[i]/zmean[i]
		endfor
		if not keyword_set(newNames) then begin
			newNames=plt+"_cycloperiod"
		
		endif

		store_data,newNames,data={x:dat.x,y:zz}

		return
	endif
	;for el=0, aan-1 do begin
		;dim=DD[el]
		;bi=BB[el]
		;aj=AA[el]
		;dhalf=mean([dim+10,aj])
		;for i=dim, bi do begin
			;if tau[i] ne 0 then  begin
				;for j=0,2 do begin
					;beg=max([i-harms[i,j],0])
					;fin=min([i+harms[i,j],N-1])
					;z[i,j]=mean(ys[beg:fin],/NAN)
					;numlooped++
				;endfor
			;endif
			;zstd[i]=stddev(z[i,*])
			;zmean[i]=mean(z[i,*])
			;zstdn[i]=zstd[i]/zmean[i]
		;endfor
	;endfor
	for i=0, N-1 do begin
		if tau[i] ne 0 then  begin
			for j=0,2 do begin
				beg=max([i-harms[i,j],0])
				fin=min([i+harms[i,j],N-1])
				z[i,j]=mean(ys[beg:fin],/NAN)
				numlooped++
			endfor
		endif
		zstd[i]=stddev(z[i,*])
		zmean[i]=mean(z[i,*])
		zstdn[i]=zstd[i]/zmean[i]
	endfor
	print,numlooped
	print,max(zstd)
	if not keyword_set(newNames) then begin

		newnames=strarr(3)
		 for i=0,2 do newNames[i]=plt
		newNames+=["_1st","_2nd","3rd"]+"_modal_average"
	endif

	store_data,newNames[0],data={x:dat.x,y:z[*,0]}
	options,newNames[0],'colors','g'
	store_data,newNames[1],data={x:dat.x,y:z[*,1]}
	options,newNames[1],'colors','r'
	store_data,newNames[2],data={x:dat.x,y:z[*,2]}
	options,newNames[2],'colors','c'
	store_data,plt+"_modal_mean",data={x:dat.x,y:zmean,ytitle:"mean of modal averages [nT]"}
	store_data,plt+"_modal_stddev",data={x:dat.x,y:zstd,ytitle:"stddev [nT]"}
	store_data,plt+"_modal_stddev_normalized",data={x:dat.x,y:zstdn,ytitle:"stddev/mean"}
end
