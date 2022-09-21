function locallocate,foot,aj,bi,ymm,B5d,B20d,B20da,B20dex,fore,B5ex,xs

		
		inregion=where(fore eq 1)
		if inregion[0] eq -1 then return,aj;-1
		firstval=inregion[0]
		lastval=inregion[-1]
		EXT=where(B20dex ne 0 and B20dex gt .02,nex)
		EXT5=where(B5ex ne 0,nex5)

		
		BEXT=B20dex[EXT]
		firstreg=0;(where(EXT ge firstval))[0]
		print,firstreg
		print,EXT[firstreg]
		print,firstval
		lastreg=EXT[firstreg]
		lastmax=BEXT[firstreg]
		lastvalreg=EXT[firstreg]
		lasrvalB=Bext[firstreg]
		topB=BEXT[firstreg]
		topreg=EXT[firstreg]
		lastBp=0
		lastp=-1
		topreg=EXT[firstreg]
		lastvBp=0
		lastvp=-1
		N=numel(B20dex)

		print,"fore starts at:",x2greg(xs[firstval],/str)

		print,numel(B5d),firstval,lastreg+5
		wpre5=(where(EXT5 lt lastreg ))[-1]
		if wpre5 eq -1 then pre5=firstval else pre5=EXT5[wpre5]
		print,wpre5
		print,"pre5 at ",x2greg(xs[pre5],/str)

		for  i=firstreg+1,nex-1 do begin
			el=EXT[i]
			Baex=B20dex[el]
			
			if Baex ge lastmax then begin;and el lt lastval then begin

				pre5=(where(EXT5 lt el and B5d[EXT5] ne max(B5d[lastreg:el])     ))[-1]
				if pre5 eq -1 then pre5=firstval else pre5=EXT5[pre5]
				lastreg=el
				lastmax=Baex
				topreg=el

				if B20d[el] gt 0 then begin
					lastBp=B20d[el]
					lastp=el
					topB=Baex
				endif
				if el lt lastval then begin
					lastvalreg=el
					lastvalB=Baex
					if B20d[el] gt 0 then begin
						lastvBp=B20d[el]
						lastvp=el

					endif

				endif
				continue
			endif
			if Baex lt lastmax and el gt firstval then begin;and (B20d[lastmax] gt 0) then begin
				print,"pre5    at ",x2greg(xs[pre5],/str)
				print,"lastreg at ",x2greg(xs[lastreg],/str)
				print,"el      at ",x2greg(xs[el],/str)
				post5=(where(EXT5 gt el+1))[0]
				if post5 eq -1 then post5=min([N-1,el+10]) else post5=EXT5[post5]+1 ;else post5 =mean([el,post5])
				print,"pre5,lastreg,el,post5=",pre5,lastreg,el,post5
				print,"post5  at ",x2greg(xs[post5],/str)
				;B5max=max(B5d[pre5:post5],maxloc)
				;maxloc+=lastreg
				;return, maxloc+foot
				maxlocs5=WHERE(B5d[pre5:post5] eq max(B5d[pre5:post5]))+pre5
				print,"maxlocs5[0]  at ",x2greg(xs[maxlocs5[0]],/str)
				if numel(maxlocs5) eq 1 then return,maxlocs5[0]+foot
				maxlocs2=WHERE(B20d[pre5:post5] eq max(B20d[pre5:post5]))+pre5
				m52=intersect(maxlocs5,maxlocs2)
				if size(m52,/dim) gt 0 then return, median(m52)+foot
				if maxlocs2[0] gt maxlocs5[-1] then return,maxlocs5[-1]+foot else return,maxlocs5[0]+foot 
			endif
		endfor

		return,aj;-1
end



pro shocklocatetest


		get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datB




		get_data,"B_meanMode_smoothed",data=datmm

		get_data,'B5-3d10',data=datB5d
		get_data,'B20deriv',data=datB20d

		get_data,'B20deriv_abs',data=datB20da
		get_data,'B20deriv_abs_extrema',data=datB20dex
		get_data,'B5-3d10_abs_extrema',data=datB5ex
		xs=datB.x

		nxs=reverse(xs)

		ys=datB.y
		ymm=datmm.y
		nymm=REVERSE(ymm)
		B5d=datB5d.y
		nB5d=reverse(B5d)*(-1)

		B20d=datB20d.y
		nB20d=reverse(B20d)*(-1)

		B20da=datB20da.y
		nB20da=reverse(B20da)
		B20dex=datB20dex.y
		nB20dex=reverse(B20dex)

		B5ex=datB5ex.y
		nB5ex=reverse(B5ex)
		N=numel(xs)

		z=fltarr(N)
		nz=fltarr(N)

		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'bDD_flag',data=datbDD
		get_data,'forecountdown',data=datFore
		get_data,"foot_start_inbound",data=datfsi
		get_data,"foot_start_outbound",data=datfso
		fore=datFore.y
		nfore=REVERSE(fore)

		get_data,'DD_effective_flag',data=dateDD
		get_data,'bDD_effective_flag',data=datebDD

		FF = WHERE(datfsi.y ne 0, ffn); beginnings of inbound ramps
		nFF = WHERE(REVERSE(datfso.y) ne 0, nffn);beginnings of outbound ramps

		DD = WHERE(datDD.y ne 0, ddn);defines the boundaries in  forward direction. beginning  of negative shock
		eDD = WHERE(dateDD.y ne 0, eddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn); end of  negative  shock

		enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn);defines the boundaries in  forward direction. beginning  of negative shock
		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn); end of  negative  shock
		



		if DD[0] gt AA[0] then begin
			DD=[0,DD]
			ddn++
		endif
		if nDD[0] gt nAA[0] then begin
			nDD=[0,nDD]
			nddn++
		endif

		if eDD[0] gt AA[0] then begin
			eDD=[0,eDD]
			eddn++
		endif
		if enDD[0] gt nAA[0] then begin
			enDD=[0,enDD]
			enddn++
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
		print,"DD:"
		print,DD
		print,"FF:"
		print,FF
		print,"AA:"
		print,AA
		print,"BB:"
		print,BB


		for i=0,aan-1 do begin
			print,"~~~~~~~~~"
			print,"i=",i
		 	aj=AA[i]
			bi=BB[(where(aj lt BB))[0]]
			dim=DD[(where(aj gt DD))[-1]]
			edim=eDD[(where(aj gt eDD))[-1]]
			wherefoot=(where(FF lt aj and FF ge dim))[-1]
			if wherefoot eq -1 then foot=mean([dim,aj]) else foot=FF[wherefoot]
			sbound=max([foot,edim])
			if sbound eq dim then sbound=mean([dim,aj])
			print,"dim,edim,foot,sbound,aj,bi=",[dim,edim,foot,sbound,aj,bi]
			loc=locallocate(sbound,aj,bi,ymm[sbound:bi],B5d[sbound:bi],B20d[sbound:bi],B20da[sbound:bi],B20dex[sbound:bi],fore[sbound:bi],B5ex[sbound:bi],xs[sbound:bi])
			print,aj,loc,abs(aj-loc)
			if loc eq -1 then continue
			z[loc]=1
		endfor
		;z=reverse(z)
		print,"OUTBOUND"
		for i=0,naan-1 do begin
			print,"~~~~~~~~~"
			print,"i=",i
		 	aj=nAA[i]
			bi=nBB[(where(aj lt nBB))[0]]
			dim=nDD[(where(aj gt nDD))[-1]]
			edim=enDD[(where(aj gt enDD))[-1]]
			wherefoot=(where(nFF lt aj and nFF ge dim))[-1]
			if wherefoot eq -1 then foot=mean([dim,aj]) else foot = nFF[wherefoot]
			sbound=max([foot,edim])
			if sbound eq dim then sbound=mean([dim,aj])
			print,"dim,edim,foot,sbound,aj,bi=",[dim,edim,foot,sbound,aj,bi]
			loc=locallocate(sbound,aj,bi,nymm[sbound:bi],nB5d[sbound:bi],nB20d[sbound:bi],nB20da[sbound:bi],nB20dex[sbound:bi],nfore[sbound:bi],nB5ex[sbound:bi],xs[sbound:bi])
			print,aj,loc,abs(aj-loc)
			if loc eq -1 then continue
			z[loc]=1
		endfor


		nz=reverse(nz)

		store_data,"shock_guess_inbound",data={x:xs,y:z,ytitle:"Shock !C Flag"}
		store_data,"shock_guess_outbound",data={x:xs,y:z,ytitle:"Shock !C Flag"}
		store_data,"shock_guess",data={x:xs,y:z+nz,ytitle:"Shock !C Flag"}


end
