pro crustalremover
	get_data,'B_meanMode_smoothedmonoRegion',data=datmono
	get_data,'B_meanMode_smoothedbmonoRegion',data=datbmono
	get_data,"B20deriv",data=datB20d
	get_data,"B_meanMode_smoothed",data=datmm
	get_data,'AA_flag',data=datAA
	get_data,'bAA_flag',data=datbAA
	get_data,'BB_flag',data=datBB
	get_data,'bBB_flag',data=datbBB
	get_data,'CC_flag',data=datCC
	get_data,'bCC_flag',data=datbCC
	get_data,'DD_flag',data=datDD
	get_data,'bDD_flag',data=datbDD
	get_data,"foot_start_inbound",data=datfi
	get_data,"foot_start_outbound",data=datfo

	get_data,"PE_half",data=datpeh
	get_data,"te_half",data=datteh
	get_data,"ne_half",data=datneh

	;get_data,"PE_10sec",data=datpe10
	;get_data,"te_10sec",data=datte10
	;get_data,"ne_10sec",data=datne10

	peh=datpeh.y
	neh=datneh.y
	teh=datteh.y

	;peh=datpe10.y
	;neh=datne10.y
	;teh=datte10.y

	nneh=reverse(neh)
	nteh=reverse(teh)
	npeh=reverse(peh)

	mono=datmono.y
	bmono=REVERSE(datbmono.y)

	B20d=datB20d.y
	nB20d=REVERSE(B20d)*(-1)
	ymm=datmm.y
	nymm=reverse(ymm)
	N=numel(ymm)
	newBB=fltarr(N)
	newbBB=fltarr(N)

	;xs=datmono.x
	;nxs=reverse(xs)

		FF = WHERE(datfi.y ne 0, ffn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of inbound foot
		
		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock

		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock

		nFF = WHERE(REVERSE(datfo.y) ne 0, nffn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan, COMPLEMENT=nH_C, NCOMPLEMENT=nhcount_c); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn, COMPLEMENT=nK_C, NCOMPLEMENT=nkcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn, COMPLEMENT=nL_C, NCOMPLEMENT=nlcount_c); end of  negative  shock
		

		doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
	aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
	;dboffsetb=1*(nBB[1] lt nDD[0])
	;daoffsetb=1*(nAA[1] lt nDD[0])

		doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

			backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
		if numel(BB) gt 1 then boffsetb=1*(BB[1] lt DD[0]) else  boffsetb=0
		if numel(AA) gt 1 then aoffsetb=1*(AA[1] lt DD[0]) else aoffsetb=0
			;boffsetb=1*(BB[1] lt DD[0])
			;aoffsetb=1*(AA[1] lt DD[0])
		fullIter=min([ddn,aan,bbn])
		nfullIter=min([nddn,naan,nbbn])
;;;;;;;;;;;INTBOUND
		for i=1 ,fullIter-1 do begin
			j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			k=i-1+doffsetf

			aj=AA[j]
			bi=BB[i]
			dim=DD[k]
			
			;cln=CC[l+1]

			foot=FF[(where(FF gt dim and FF lt bi))[-1]]

			regionalcrustalremover,foot,aj,bi,neh,peh,teh
			BB[i]=bi
			newBB[bi]=1
		endfor


		bi=BB[0]
		d0=DD[0]
		;dim=0
		if d0 lt bi then dim=d0 else dim=0 
		aoffsetf=1*(BB[0] lt AA[0])
		if(aoffsetf eq 0) then begin
			i=0
			aj=AA[0]
			foot=FF[(where(FF gt dim and FF lt bi))[-1]]
			regionalcrustalremover,foot,aj,bi,neh,peh,teh
			BB[i]=bi
			newBB[bi]=1
		endif else begin
			regionalcrustalremover,0  ,  0,bi,neh,peh,teh
			BB[i]=bi
			newBB[bi]=1
		endelse

;;;;;;;;;;;OUTBOUND

		for i=1 ,nfullIter-1 do begin
			j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			k=i-1+doffsetb

			aj=nAA[j]
			bi=nBB[i]
			dim=nDD[k]


			foot=nFF[(where(nFF gt dim and nFF lt bi))[-1]]

			regionalcrustalremover,foot,aj,bi,nneh,npeh,nteh
			nBB[i]=bi
			newbBB[bi]=1
		endfor


		bi=nBB[0]
		d0=nDD[0]
		;dim=0
		if d0 lt bi then dim=d0 else dim=0 
		aoffsetb=1*(nBB[0] lt nAA[0])
		if(aoffsetb eq 0) then begin
			print,"aoffsetb eq 0"
			i=0
			aj=nAA[0]
			foot=nFF[(where(nFF ge dim and nFF lt bi))[-1]]
			regionalcrustalremover,foot,aj,bi,nneh,npeh,nteh
			nBB[i]=bi
			newbBB[bi]=1
		endif else begin
			regionalcrustalremover,0,  0  ,bi,nneh,npeh,nteh
			nBB[i]=bi
			newbBB[bi]=1
		endelse

		newbBB=REVERSE(newbBB)

		datBB.y=newBB
		datbBB.y=newbBB
		store_data,'BB_flag',data=datBB
		store_data,'bBB_flag',data=datbBB	

end
