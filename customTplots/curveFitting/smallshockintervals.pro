pro smallshockintervals



	get_data,'ascend_end_interpolated',data=datae
	get_data,'ascend_begin_interpolated',data=datab

	get_data,'descend_end_interpolated',data=datde
	get_data,'descend_begin_interpolated',data=datdb

	get_data,'denjump_trigger',data=datdj
	get_data,'denjumpdown_trigger',data=datdjd

	get_data,'denfdiff',data=datdenfdiff

	denfdiff=datdenfdiff.y
	ndenfdiff=reverse(denfdiff)
	yae=datae.y
	yab=datab.y
	yde=datde.y
	ydb=datdb.y
	N=n_elements(yae)
	indexes=findgen(N)
	dj=datdj.y
	djd=datdjd.y

	ndj=reverse(djd)
	ndjd=reverse(dj)

	JU= WHERE(datdj.y ne 0,jcount)
	JD= WHERE(datdjd.y ne 0,kcount)

	DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

	AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

	CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
	BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock





	for j=0,aan-1 do begin

			aj=AA[j]
			lastd=(where(DD lt aj))[-1]
			if lastd ne -1 then dim=DD[lastd] else dim=0
			if denfdiff[aj] ge .3 then begin
				aj0=aj
				while denfdiff[aj] ge .3 and aj-dim gt 2*60 do aj--
				AA[j]=aj
				ydb[aj0]=0
				ydb[aj]=1
			endif

			nextb=(where(BB gt aj))[0]
			if nextb ne -1 then bi=BB[nextb] else begin
						bi=N-1
					;	yde[bi]=1
			endelse
			print,aj,bi
			if total(djd[aj:bi]) lt 1 or total(dj[aj:bi]) lt 2 then continue

			ck=JU[(where(JU gt aj))[0]]
			first=1
			while 1 do begin
				if ck+1 ge bi then break
				if total(djd[ck+1:bi]) lt 1 or total(dj[ck+1:bi]) lt 1 then break
				cn=JU[(where(JU gt ck+1))[0]]
				nextd=(where(JD gt ck+1 and JD lt cn))[0]
				if nextd eq -1 then break else dm=JD[nextd]

				newb=(dm+ck)/2

				if newb-ck lt 5*60 or cn-dm lt 5*60 or dm-newb lt 5*60 then break
				if first then begin
					;ydb[aj]=0
					;ydb[ck]=1

				endif
				first=0
				yae[dm+1]=1

				yde[newb-1]=1
				ydb[cn]=1
				yab[newb+1]=1
				DD = WHERE(yae ne 0, ddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

				AA = WHERE(ydb ne 0, aan); beginnging of shock 

				CC = WHERE(yab ne 0, ccn) ;end of negative shock.  defines boundaries in  negative time  direction
				BB = WHERE(yde ne 0, bbn); end of  negative  shock
				ck=cn
			endwhile
			 


	endfor

	nDD=N-1-reverse(AA)
	nAA=N-1-reverse(DD)
	nBB=N-1-reverse(CC)
	nCC=N-1-reverse(BB)


	nJU= N-1-reverse(JD)
	nJD= N-1-reverse(JU)

	nddn=aan
	nbbn=ccn
	nccn=bbn
	naan=ddn

	nyae=reverse(ydb) ;;nDD ==AA
	nyab=reverse(yde) ;;nCC ==BB
	nyde=reverse(yab)
	nydb=reverse(yae)

		for j=0,naan-1 do begin

			aj=nAA[j]
			lastd=(where(nDD lt aj))[-1]
			if lastd ne -1 then dim=nDD[lastd] else dim=0
			if ndenfdiff[aj] ge .3 then begin
				aj0=aj
				while ndenfdiff[aj] ge .3 and aj-dim gt 2*60 do aj--
				nAA[j]=aj
				nydb[aj0]=0
				ydb[aj]=1
			endif
			nextb=(where(nBB gt aj))[0]
			if nextb ne -1 then bi=nBB[nextb] else begin

					 bi=N-1
					;nyde[bi]=1
			endelse
			if total(ndjd[aj:bi]) lt 1 or total(ndj[aj:bi]) lt 2 then continue

			ck=nJU[(where(nJU gt aj))[0]]
			first=1
			while 1 do begin
				if ck+1 ge bi then break
				if total(ndjd[ck+1:bi]) lt 1 or total(ndj[ck+1:bi]) lt 1 then break
				cn=nJU[(where(nJU gt ck+1))[0]]
				nextd=(where(nJD gt ck+1 and nJD lt cn))[0]
				if nextd eq -1 then break else dm=nJD[nextd]
				newb=(dm+ck)/2
				if newb-ck lt 5*60 or cn-dm lt 5*60 or dm-newb lt 5*60 then break
				if first then begin
					;nydb[aj]=0
					;nydb[ck]=1

				endif
				first=0
				nyae[dm+1]=1

				nyde[newb-1]=1
				nydb[cn]=1
				nyab[newb+1]=1
				
				;if total(nydb[ck-10:ck+10]) eq 0 then nyab[ck]=1

				nDD = WHERE(nyae ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

				nAA = WHERE(nydb ne 0, naan); beginnging of shock 

				nCC = WHERE(nyab ne 0, nccn) ;end of negative shock.  defines boundaries in  negative time  direction
				nBB = WHERE(nyde ne 0, nbbn); end of  negative  shock
				ck=cn
			endwhile
			 


	endfor

	ddn=naan
	bbn=nccn
	ccn=nbbn
	aan=nddn

	yae=reverse(nydb) ;;nDD ==AA
	yab=reverse(nyde) ;;nCC ==BB
	yde=reverse(nyab)
	ydb=reverse(nyae)

	datae.y=yae
	datab.y=yab
	datde.y=yde
	datdb.y=ydb

	store_data,'ascend_end_interpolated2',data=datae
	store_data,'ascend_begin_interpolated2',data=datab

	store_data,'descend_end_interpolated2',data=datde
	store_data,'descend_begin_interpolated2',data=datdb

end
