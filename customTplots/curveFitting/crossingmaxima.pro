pro crossingmaxima,plt,extrema=extrema,fromdim=fromdim,newName=newName,antisymmetric=antisymmetric,userawflag=userawflag

	get_data,plt,data=dat

	ys=dat.y
	xs=dat.x
	N=numel(xs)

	nys=reverse(ys)
	if keyword_set(antisymmetric) then nys*=-1
	z=fltarr(N)
	nz=z
	;if keyword_set(extrema) then ys=abs(ys)
	if not keyword_set(newName) then newName=plt+"_maxima"

	if not keyword_set(userawflag) then begin
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
		if BB[0] lt AA[0] and numel(BB) gt 1 then begin
			if  numel(nBB) gt 1 then BB=BB[1:*] else aan=0
			bbn--
		endif

		if nBB[0] lt nAA[0] then begin
			if numel(nBB) gt 1 then nBB=nBB[1:*] else naan=0
			nbbn--
		endif
		if nBB[-1] lt nAA[-1] then begin
			nBB=[nBB,N-1]
			nbbn++
		endif




endif else begin

	get_data,'ascend_end_interpolated',data=datae
	get_data,'ascend_begin_interpolated',data=datab
	get_data,'descend_end_interpolated',data=datde
	get_data,'descend_begin_interpolated',data=datdb

	yae=datae.y
	yab=datab.y
	yde=datde.y
	ydb=datdb.y
	store_data,'ascend_end_refined',data={x:xs,y:yae*0.0}
	store_data,'ascend_begin_refined',data={x:xs,y:yae*0.0};datab
	store_data,'descend_end_refined',data={x:xs,y:yae*0.0};datde
	store_data,'descend_begin_refined',data={x:xs,y:yae*0.0};datdb
	;	
	DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

	AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

	CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
	BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock

	nDD=WHERE(reverse(ydb) ne 0, nddn); beginnging of shock 
	nCC=WHERE(reverse(yde) ne 0, nccn); beginnging of shock 
	nBB=WHERE(reverse(yab) ne 0, nbbn); beginnging of shock 
	nAA=WHERE(reverse(yae) ne 0, naan); beginnging of shock 

		;if numel(AA) gt 1 and numel(BB) gt 1 and DD[0] ne -1 and BB[1] lt DD[0] 
		
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
		if BB[0] lt AA[0] and numel(BB) gt 1 then begin
			if  numel(BB) gt 1 then BB=BB[1:*] else aan=0
			bbn--
		endif

		if nBB[0] lt nAA[0] then begin
			if numel(nBB) gt 1 then nBB=nBB[1:*] else naan=0
			nbbn--
		endif
		if nBB[-1] lt nAA[-1] then begin
			nBB=[nBB,N-1]
			nbbn++
		endif


		for el=0,numel(AA)-1 do begin
			if el eq aan then break
			print,el,numel(AA),aan
			aj=AA[el]

			bi=BB[el]
			dim=DD[el]
			
	
			if dim gt aj and el gt 0 then begin
				ldim=DD[el-1]
				lbi=BB[el-1]
				laj=AA[el-1]
				if bi-aj gt lbi-laj then begin
					newAAlocs=where(AA ne laj,aan)
					if aan gt 0 then AA=AA[newAAlocs] else AA=-1
					newBBlocs=where(BB ne lbi,bbn)
					if bbn gt 0 then BB=BB[newBBlocs] else BB=-1
				endif else begin
					newAAlocs=where(AA ne aj,aan)
					if aan gt 0 then AA=AA[newAAlocs] else AA=-1
					newBBlocs=where(BB ne bi,bbn)
					if bbn gt 0 then BB=BB[newBBlocs] else BB=-1

				endelse
				if aan le 0 then continue

				if DD[0] gt AA[0] then begin
					DD=[0,DD]
					ddn++
				endif
		

				if BB[-1] lt AA[-1] then begin
					BB=[BB,N-1]
					bbn++
				endif
				if BB[0] lt AA[0] and numel(BB) gt 1 then begin
					if  numel(BB) gt 1 then BB=BB[1:*] else aan=0
					bbn--
				endif
				el--
				aj=nAA[el]
				bi=nBB[el]
				dim=nDD[el]
			endif
		endfor
		print,"outbound"
		for el=0,numel(nAA)-1 do begin
			print,el,numel(nAA),naan
			if el eq naan then break
			aj=nAA[el]

			bi=nBB[el]
			dim=nDD[el]
			print,"[dim,aj,bi]=",[dim,aj,bi]
	
			if dim gt aj and el gt 0 then begin
				ldim=nDD[el-1]
				lbi=nBB[el-1]
				laj=nAA[el-1]
				if bi-aj gt lbi-laj then begin
					newAAlocs=where(nAA ne laj,naan)
					if naan gt 0 then nAA=nAA[newAAlocs] else nAA=-1
					newBBlocs=where(BB ne lbi,nbbn)
					if nbbn gt 0 then nBB=nBB[newBBlocs] else nBB=-1
				endif else begin
					newAAlocs=where(nAA ne aj,naan)
					if naan gt 0 then nAA=nAA[newAAlocs] else nAA=-1
					newBBlocs=where(nBB ne bi,nbbn)
					if nbbn gt 0 then nBB=nBB[newBBlocs] else nBB=-1
					aj=laj
					bi=lbi
				endelse
				if naan le 0 then continue

				if nDD[0] gt nAA[0] then begin
					nDD=[0,nDD]
					nddn++
				endif
		

				if nBB[-1] lt nAA[-1] then begin
				nBB=[nBB,N-1]
				nbbn++
				endif
				if nBB[0] lt nAA[0] and numel(nBB) gt 1 then begin
					if  numel(nBB) gt 1 then nBB=nBB[1:*] else naan=0
					nbbn--
				endif

				el--
				aj=nAA[el]
				bi=nBB[el]
				dim=nDD[el]
			endif
			print,"[el,dim,aj,bi,naan]=",[el,dim,aj,bi,naan]
		endfor

		if aan gt 0 then begin
		FF=fltarr(aan)
		for el=0,aan-1 do FF[el]=(2*AA[el]+DD[el])/3
		endif else FF=[DD[0]]
		if naan gt 0 then begin
		nFF=fltarr(naan)
		for el=0,naan-1 do nFF[el]=(2*nAA[el]+nDD[el])/3
		endif else nFF=[nDD[0]]
endelse
		print,numel(FF),aan
		for el=0,min([aan,numel(FF)])-1 do begin
			aj=AA[el]
			dim=DD[el]
			print,el
			foot=FF[el]
			bi=BB[el]			
			if keyword_set(fromdim) then st=dim else st=foot
			reg=ys[st:bi]
			if keyword_set(extrema) then reg=abs(reg)
			mx=max(reg,mxloc)
			z[mxloc+st]=ys[st+mxloc]
		endfor




		for el=0,naan-1 do begin
			aj=nAA[el]
			dim=nDD[el]
			foot=nFF[el]
			bi=nBB[el]			
			if keyword_set(fromdim) or foot gt bi then st=dim else st=foot
			print,"[dim,st,foot,aj,bi]=",[dim,st,foot,aj,bi]
			reg=nys[st:bi]
			if keyword_set(extrema) then reg=abs(reg)
			mx=max(reg,mxloc)
			nz[mxloc+st]=nys[st+mxloc]
		endfor

		nz=reverse(nz)
		if keyword_set(antisymmetric) then nz*=-1
		dat.y=z+nz
		store_data,newName,data=dat
end

