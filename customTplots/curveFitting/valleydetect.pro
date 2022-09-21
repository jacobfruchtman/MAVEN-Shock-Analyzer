pro monolength,dhalf,aj,bi,Bau,z,z2,z3

		lastup=dhalf
	
		lastdown=dhalf
		numup=0
		numdown=0
		;if Bau[dhalf] then lastup=dhalf else lastdown=dhalf
		ab=0
		if Bau[dhalf] then ab=1


		z[dhalf]=0
		z2[dhalf]=0
		for i=dhalf+1,bi-1 do begin
			if (ab eq 0) and Bau[i] then begin
				ab=1
				lastup=i

				z2[lastdown:i-1]-=numdown
				if numdown le 25 then begin
					z3[lastdown]=-1
					z3[i]=1 
				endif
				numup=0
				;numdown=0
			endif

			if ( ab) and (Bau[i] eq 0) then begin
				ab=0
				lastdown=i
				z2[lastup:i-1]+=numup

				;if numdown le 30 then z3[lastdown]=-1 
				numdown=0
				;numup=0
			endif

			if ab and Bau[i] then begin
				numup++
				z[i]=numup
				z2[i]=numup
			endif
			if (0 eq ab) and (0 eq  Bau[i]) then begin
				numdown--
				z[i]=numdown
				z2[i]=numdown
			endif
			
		endfor
	

end

pro valleydetect

		;get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datB
		get_data,'B_above-up_regions_cleaned',data=datBau

		;B=datB.y

		;nB=REVERSE(B)

		Bau=datBau.y
		nBau=REVERSE(Bau)

		x=datBau.x

		N=numel(x)
		z=fltarr(N)
		z2=z
		z3=z
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

		;;FRONT SIDE

		for el=0,aan-1 do begin

			dim=DD[el]
			aj=AA[el]
			bi=BB[el]

			dhalf=mean([dim+10,aj])
			monolength,dhalf,aj,bi,Bau,z,z2,z3

		endfor

		z=reverse(z)
		z2=reverse(z2)
		z3=reverse(z3)
		for el=0,naan-1 do begin

			dim=nDD[el]
			aj=nAA[el]
			bi=nBB[el]

			dhalf=mean([dim+10,aj])
			monolength,dhalf,aj,bi,nBau,z,z2,z3

		endfor
		z=reverse(z)
		z2=reverse(z2)
		z3=reverse(z3)


		store_data,"B_valleyEnd",data={x:x,y:z,ytitle:"indices into valley or hill"}
		store_data,"B_valleyAdjusted",data={x:x,y:z2,ytitle:"indices into valley or hill+length"}
		store_data,"B_valleyFlags",data={x:x,y:z3,ytitle:"B_valleyFlags"}
end
