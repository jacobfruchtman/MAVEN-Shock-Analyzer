pro secondorderfootstart

	
		get_data,'foot_start_inbound',data=datfin
		get_data,'foot_start_outbound',data=datfout

		get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_stddev',data=datSTD

		STD=datSTD.y
		nSTD=REVERSE(STD)
		fin=datfin.y

		FFI=where(fin eq 1, fincount)




		fout=reverse(datfout.y)
		x=datSTD.x
		nx=reverse(x)
				N=numel(x)

		FFO=where(fout eq 1, foutcount)
		if (total(STD[FFI] gt .4) eq 0)  and (total(nSTD[FFO] gt .4) eq 0) then return
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
		fullIter=min([ddn,aan,bbn])
		nfullIter=min([nddn,naan,nbbn])
		;for i=0, fullIter-1 do print, "[DD,FFI,AA,BB][",i,"]=",[DD[i],FFI[i],AA[i],BB[i]]

		;for i=0, fullIter-1 do print, "[nDD,FFO,nAA,nBB][",i,"]=",[nDD[i],FFO[i],nAA[i],nBB[i]]

		get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datB

		B=datB.y
		nB=REVERSE(B)
		N=numel(B)

		for el=0, fincount-1 do begin
			while 1 do begin
				i=FFI[el]
				tt=x2Greg(x[i],/strformat)
				print,"i,tt,STD[i]=",i,tt,STD[i]
				if STD[i] lt .5 then break
				
				

			endwhile
		endfor

end
