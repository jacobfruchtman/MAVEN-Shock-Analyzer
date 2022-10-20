pro downstreamcalc
		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout


		if total(datin.y + datout.y) eq 0 then return
	del_data,"overshoot*"


	get_data,"B20deriv",data=datB20d

	B20d=datB20d.y
	nB20d=(-1)*REVERSE(B20d)

	get_data,"Franken_fitted_inbound",data=datFFI;{x:xs,y:zin,ytitle:datin.ytitle,inups:inups,indowns:indowns,imins:inimins,imaxs:inimaxs,chis:inChis}

	get_data,"Franken_fitted_outbound",data=datFFO;{x:xs,y:zout,ytitle:datin.ytitle,outups:outups,outdowns:outdowns,imins:outimins,imaxs:outimaxs,chis:outChis}
	
	get_data,"shocks_inbound",data=datsi;{x:xs,y:inshocks,ytitle:'shock'}
	get_data,"shocks_outbound",data=datso;{x:xs,y:outshocks,ytitle:'shock'}
	;get_data,"B_above-up_regions_cleaned",data=datBau
	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
	get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_mean',data=datmm
	get_data,'sublengths_inbound',data=datsubli;{x:xs,y:insublengths,ytitle:'flag'}
	get_data,'sublengths_inbound_begin',data=datsublib;{x:xs,y:insubbegs,ytitle:'flag'}
	get_data,'sublengths_inbound_end',data=datsublie;{x:xs,y:insubends,ytitle:'flag'}

	get_data,'sublengths_outbound',data=datsublo;{x:xs,y:outsublengths,ytitle:'flag'}
	get_data,'sublengths_outbound_begin',data=datsublob;{x:xs,y:outsubbegs,ytitle:'flag'}
	get_data,'sublengths_outbound_end',data=datsubloe;{x:xs,y:outsubends,ytitle:'flag'}

	get_data,"shock_locs_inbound",data=datSLI;{x:xs,y:inShockLocs,ytitle:'shock location'}
	get_data,"shock_locs_outbound",data=datSLO;{x:xs,y:outShockLocs,ytitle:'shock location'}

	get_data,'rho10deriv',data=datrhod10
	get_data,'rho30deriv',data=datrhod
	get_data,'v30deriv',data=datvd
	vd=datvd.y
	nvd=REVERSE(vd) *(-1.0)
	rhod=datrhod.y
	nrhod=(-1)*REVERSE(rhod)

	rhod10=datrhod10.y
	nrhod10=(-1)*REVERSE(rhod10)

	get_data,"underdownstream",data=datud
	get_data,"overdownstream",data=datOD
	BB=datB.y
	nBB=REVERSE(BB)
	get_data,"proton_cyclotron_period",data=datpt


	get_data,"downmmdev",data=datdmd
	get_data,"mmdblockNeg_trigger",data=datmmtrig
	get_data,"mmdblocksize",data=datmmblock
	get_data,"mvn_swe_spec_temp_interpolated",data=datte

	

	t_e=datte.y


	dmd=datdmd.y

	ndmd=reverse(dmd)

	mmdtrig=datmmtrig.y
	nmmdtrig=reverse(mmdtrig)

	xs=datpt.x
	pt=datpt.y
	npt=REVERSE(pt)

	ud=datud.y
	OD=datOD.y
	nud=REVERSE(ud)
	nOD=REVERSE(OD)

	mmblock=datmmblock.y
	nmmblock=reverse(mmblock)

	maxMMD=6
	iwidths=1./(datFFI.MMs[*,1])
	owidths=reverse(1./(datFFO.MMs[*,1]))
	mm=datmm.y

	

	nmm=reverse(mm)

	mmd=datFFI.downs -mm
	fmmd=abs(mmd)/datFFI.downs
	nmmd=REVERSE(datFFO.downs) -nmm
	nfmmd=nmmd/REVERSE(datFFO.downs)
	slii=datSLI.y

	sloo=REVERSE(datSLO.y)


	mmda60=smooth(abs(smooth(mmd,60)),60)

	nmmda60=smooth(abs(smooth(nmmd,60)),60)

	subli=datsubli.y
	sublib=datsublib.y
	sublie=datsublie.y

	sublo=REVERSE(datsublo.y)
	sublob=REVERSE(datsublob.y)
	subloe=REVERSE(datsubloe.y)

	;Bau=datBau.y
	;nBau=REVERSE(Bau)

	;get_data,"B_1stModalAverage",data=datB1
	;get_data,"B_2ndModalAverage",data=datB2
	;get_data,"B_3rdModalAverage",data=datB3

	inchis=datFFI.chis
	outchis=REVERSE(datFFO.chis)



	;B1=datB1.y
	;B2=datB2.y
	;B3=datB3.y

	;nB1=REVERSE(B1)

	;nB2=REVERSE(B2)

	;nB3=REVERSE(B3)

	Fdi=datFFI.indowns
	Fdo=REVERSE(datFFO.outdowns)

	FFI=datFFI.y
	FFO=REVERSE(datFFO.y)

	BfdiffI=fracdiff(FFI,mm)
	smallfracdiffI=onofflength(BfdiffI lt .1)
	BfdiffO=fracdiff(FFO,nmm)
	smallfracdiffO=onofflength(BfdiffO lt .1)
	
	BfdiffI=fracdiff(FFI,mm)
	smallerfracdiffI=onofflength(fmmd le .1)


	BfdiffO=fracdiff(FFO,nmm)
	smallfracdiffO=onofflength(BfdiffO lt .1)
	BfdiffO=fracdiff(FFO,nmm)
	smallerfracdiffO=onofflength(nfmmd lt .1)

	N=size(FFO,/n_el)

	overregion=fltarr(N)
	noverregion=fltarr(N)
	overshoot=fltarr(N)
	novershoot=fltarr(N)

	underregion=fltarr(N)
	nunderregion=fltarr(N)
	undershoot=fltarr(N)
	nundershoot=fltarr(N)


	downtestinterval=fltarr(N)
	ndowntestinterval=fltarr(N)

	downBmag=fltarr(N)
	ndownBmag=fltarr(N)

	dsi=fltarr(N,2)
	dso=dsi
	Bms=fltarr(N)
	ioend=Bms
	ioendloc=fltarr(N)-1
	ooend=fltarr(N)
	ooendloc=fltarr(N)-1

	iuend=fltarr(N)
	iuendloc=fltarr(N)-1
	ouend=fltarr(N)
	ouendloc=fltarr(N)-1

	si=datsi.y
	so=REVERSE(datso.y)
	sli=where(si ne 0,icount)
	slo=where(so ne 0,ocount)
	;want to use Franken_fitted, Bau and modal averages to identify end of overshoot. If predicted end of oveshoot occurs when Bau[i] eq 0, then find
	; next time modal averages drop below Franken_fitted and Bau[i] eq 1
	allimaxs=datFFI.imaxs
	allimins=datFFI.imins
	allomaxs=REVERSE(datFFO.imaxs)
	allomins=REVERSE(datFFO.imins)
	allomaxs0=datFFO.imaxs
	allomins0=datFFO.imins

	imaxs=allimaxs[sli]
	imins=datFFI.imins[sli]
	omins=N-1-allomaxs[slo]
	omaxs=N-1-allomins[slo]

	inups=datFFI.inups
	indowns=datFFI.indowns
	outups=REVERSE(datFFO.outups)
	outdowns=REVERSE(datFFO.outdowns)
	for el=0, icount-1 do begin
		;;print,"===================="
		;;print,"el=",el
		ishock=sli[el]
		imax=imaxs[el]
		imin=imins[el]
		wid=iwidths[ishock]*!pi/2
		
		pp=pt[ishock]
		;t=max([6*pp,120]);pt[ishock]
		;t=min([max([100,6*pp]),120])
		t=pp
		while round(t) le 60 do t+=pp	
		over=OD[ishock]
		startunder=0
		endunder=0
		suloc=ishock
		savedU=0
		mmdmean=mmd[(ishock+imax)/2]
		overstart=ishock
		mono=1
		down=indowns[ishock]
		maxnegblock=min(mmblock[ishock:imax])

		IF (bfdiffI[imax-1] ge .25 and max(smallfracdiffI[ishock:imax]) gt 5*60.) then begin
			imaxt=(where(smallfracdiffI[ishock:imax] eq	max(smallfracdiffI[ishock:imax],mxcnt)))[-1]+ishock
				;;BEYOND THIS POINT, THE B-FIELD DIVERGES / BECOMES ILL BEHAVED ENOUGH FROM THE FIT SIGNIFICANTLY ENOUGH THAT IT CAN BE NEGLECTED OUT OF HAND
				for kk=imaxt+1,min([imax,N-1]) do begin
 					allimaxs[kk]=-1
					FFI[kk]=0
					slii[kk]=-1

					subli[kk]=0
					sublib[kk]=0
					sublie[kk]=0
					;indowns[kk]=0
					;inups[kk]=0
				endfor
				allimaxs[imin:imaxt]=imaxt
				imax=imaxt
			imaxs[el]=imaxt	

		endif else begin
			IF (fmmd[imax-1] ge .20 and max(smallerfracdiffI[ishock:imax]) ge 5*60.) then begin
			imaxt=(where(smallerfracdiffI[ishock:imax] eq	max(smallerfracdiffI[ishock:imax],mxcnt)))[-1]+ishock
				;;BEYOND THIS POINT, THE B-FIELD DIVERGES / BECOMES ILL BEHAVED ENOUGH FROM THE FIT SIGNIFICANTLY ENOUGH THAT IT CAN BE NEGLECTED OUT OF HAND
				for kk=imaxt+1,min([imax,N-1]) do begin
 					allimaxs[kk]=-1
					FFI[kk]=0
					slii[kk]=-1

					subli[kk]=0
					sublib[kk]=0
					sublie[kk]=0
					;indowns[kk]=0
					;inups[kk]=0
				endfor
				allimaxs[imin:imaxt]=imaxt
				imax=imaxt
			imaxs[el]=imaxt	

		endif


		endelse

		if ishock ge imax-t or total(finite(BB[ishock:imax]) eq 0) ne 0 or (maxnegblock lt 0 and (-1.0*maxnegblock/(imax-ishock) gt .4 or -1* maxnegblock ge 10*60  )  ) then begin
			;;In this case, there's no way we can find an overshoot or downstream interval. Everything will be blow up if we try to conduct measurements on it so no point in measuring. Just throw the crossing out.
			FFI[imin:imax]=0
			allimaxs[imin:imax]=-1
			;indowns[imin:imax]=0
			;inups[imin:imax]=0
			allimins[imin:imax]=-1
			inchis[imin:imax]=0
			si[ishock]=0
			slii[imin:imax]=-1

			subli[imin:imax]=0
			sublib[imin:imax]=0
			sublie[imin:imax]=0

			continue
		endif




		i0d8=.8*(imax)+.2*(ishock)
		bound=max([i0d8,imax-t])
		fullmax=max(BB[ishock:imax-t],fullmaxloc)

		

		if max(abs(mmd[imax-5:imax])) gt maxMMD or BB[imax] eq max(BB[ishock:imax]) or imax eq N then begin
			imaxt=max([(where(abs(mmd[ishock:imax]) le maxMMD))[-1],min([t+1,imax-ishock])])+ishock;imaxt=(where(abs(mmd[ishock:imax]) le maxMMD,blw10))[-1]+ishock
			if 0 and imaxt-1 lt imax and imaxt-t gt ishock then begin
				for kk=imaxt,min([imax,N-1]) do begin
 					allimaxs[kk]=-1
					FFI[kk]=0
					slii[kk]=-1

					subli[kk]=0
					sublib[kk]=0
					sublie[kk]=0
					;indowns[kk]=0
					;inups[kk]=0
				endfor
				allimaxs[imin:imaxt]=imaxt
				
				imax=imaxt
				imaxs[el]=imaxt
			endif	
		endif

		whereBAD=WHERE(mmdtrig[ishock:imax] ne 0)
		jj=ishock
		if whereBad[0] eq 0 then begin
				;jj=ishock
				while mmdtrig[jj] ne 0 and jj lt imax-1 do jj++
				whereBAD=WHERE(mmdtrig[jj:imax] ne 0)
			endif
		imax0=imax
		if whereBad[0] ne 0 and (whereBad[-1]+jj eq imax ) then begin
				while whereBad[-1]+jj eq imax and imax gt ishock and whereBad[-1] ne -1 do begin
					imax--
					if numel(whereBad) gt 1 then whereBad=temporary(whereBad[0:-2]) else $
					if numel(whereBad) eq 1 then whereBad=whereBad[0] else whereBad = -1			
				endwhile

				if ishock ge imax0-t then begin

				FFI[imin:imax0]=0
				allimaxs[imin:imax0]=-1
				;indowns[imin:imax0]=0
				;inups[imin:imax0]=0
				allimins[imin:imax0]=-1
				inchis[imin:imax0]=0
				si[ishock]=0
				slii[imin:imax0]=-1
	
				subli[imin:imax0]=0
				sublib[imin:imax0]=0
				sublie[imin:imax0]=0

				continue

			endif

				for kk=imax,min([imax0,N-1]) do begin
 					allimaxs[kk]=-1
					FFI[kk]=0
					slii[kk]=-1

					subli[kk]=0
					sublib[kk]=0
					sublie[kk]=0
					indowns[kk]=0
					inups[kk]=0
				endfor
				allimaxs[imin:imax0]=imax
				
				;imax=imaxt
				imaxs[el]=imax

		endif

		if whereBad[0] ne -1 then begin
			

			whereBad=temporary(whereBAD)+jj
			pbool=max(abs(mmdtrig[whereBAD])) ge 3
			qbool=whereBad[0] lt imax-2*pp
			rbool=total(mmdtrig[whereBAD]) *1.0 /(imax-ishock) gt .1
			;sbool=total(mmdtrig[whereBAD]) *1.0 /(imax-ishock) lt .5
			if ((~pbool or rbool) and (pbool or ( qbool and  rbool))) then begin
				FFI[imin:imax]=0
			allimaxs[imin:imax]=-1

			allimins[imin:imax]=-1
			inchis[imin:imax]=0
			si[ishock]=0
			slii[imin:imax]=-1

			subli[imin:imax]=0
			sublib[imin:imax]=0
			sublie[imin:imax]=0

			;indowns[imin:imax]=0
			;inups[imin:imax]=0


			endif
			continue
		endif

		if (imax-ishock)/wid lt 2. then wid=0
 
		;t=min([max([100,6*pp]),120])
		quasipar=1
		for i=ishock,imax-t do begin

			
			;if over and total(dmd[ishock:i-1] gt 0) eq 0 and total(dmd[i:imax] lt 0) eq 0 then begin
				
			numabove=total(dmd[i+1:imax] gt 0)
			numbelow=total(dmd[i+1:imax] lt 0)
				

			;endif

			if rhod[i] gt 0 then continue
			if i +2 gt imax-t   then break
			if dmd[i] lt dmd[i-1] then mono=0
			if dmd[i] ge dmd[i-1] and mono then continue
			quasipar=0
			if over and (total(B20d[ishock:i]  lt 0) eq 0 or max(BB[ishock:i]) lt down or i-ishock lt wid*.75) then continue 
			;if over and (~ B1a or ~B3a) and Bau[i] then startunder=1
			if over and ud[i] gt 1 and i-ishock gt pp then begin
				;;;print,"under"
				if ~startunder then suloc=i
				startunder=1
				continue
			endif 
			if startunder and  ud[i] lt 0 then begin;total(nud[i:i+6] lt 0) eq 7 then begin;and ud[i] lt 0 then begin
				;;;print,"ud[i] lt 0"
				over=0
				startunder=0
				suloc=ishock
				continue
			endif
			if startunder and  ud[i] eq 0 then begin;total(ud[i:i+3] eq 0) eq 4  then begin;and ud[i] eq 0 then begin
				;;;print,"ud[i] eq 0"
				overmax=max(BB[ishock:suloc],maxloc)
				downmax=max(BB[i:min([i+t,imax])],dmaxloc)
				thusmax=max(BB[ishock:min([i+t,imax])],thusmaxloc)
				;if ((i-ishock)/(imax-ishock) lt .05 and downmax eq thusmax) or ( (i-ishock)/(imax-ishock) lt .1 and  mean(BB[i:i+t])-down gt 4 and numbelow gt 60 )  then begin
				if (((i-ishock)/(imax-ishock) lt .05 or i-ishock lt wid*.65 ) and downmax eq thusmax) then begin
					over=1
					startunder=0
					continue
				endif
				;if (maxloc)/(imax-ishock) gt .8 then begin
				;	vex=EXTREMA(vd[ishock:imax-t])
				;	lvex=vex[-1]+ishock
				;	dex=extrema(mmda60[ishock:lvex],minim)
				;	closest=min
				;endif


				;;print,"suloc,i=",suloc,i
				while max(mm[i:min([i+t,imax])])-max(mm[ishock:suloc]) gt 2 and max(mm[i:min([i+t,imax])]) eq max(mm[ishock:min([i+t,imax])]) and i-suloc gt 1 do i--
				rmax=max(BB[suloc:i],rloc)-1
				lmax=max(BB[ishock:suloc],lloc)
				if rmax gt lmax and 1.*(rloc+suloc-ishock)/(imax-ishock) lt .5 and imax-i gt t+30 then begin
					suloc=i
					startunder=1
					continue
				endif
				
				st=i
				if st lt .7*ishock+.3*imax then st=.7*ishock+.3*imax
				en=min([st+t,imax])
				if en ge imax and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .75 or fracdiff(mean(BB[st:en]),down) gt .40 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) then begin
					while st gt mean([suloc,i]) and st ge .3*(imax-ishock)+ishock  and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .7 or fracdiff(mean(BB[st:en]),down) gt .3 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) do begin
								st--
								en--
							endwhile
						print,time_string(xs[st])

						endif
				dd=[st,en]
				;dd=[i,min([i+t,imax])]
				;;print,"dd=",dd
				for j=mean([imin,ishock]),imax do dsi[j,*]=dd
				;dsi[mean([imin,ishock]):imax,*]=[i,min([i+t,N-1])]
				;mx=max(ud[suloc:i-1],maxloc)
				ioend[suloc]=1
				ioendloc[imin:imax]=suloc
				iuend[i]=1
				iuendloc[imin:imax]=i
				savedU=1
				;for j=i,min([i+t,imax]) do downBmag[j]=BB[j]
				for j=st,en do downtestinterval[j]=1
				for j=ishock,suloc do overregion[j]=1
				for j=ishock,suloc do overshoot[j]=BB[j]
				for j=suloc,i do underregion[j]=1
				for j=suloc,i do undershoot[j]=BB[j]
				;;print,"savedU v1"
				break
			endif
			if OD[i] gt 0 and ~over then begin
				;;;print,"over"
				over=1
				overstart=i
			endif	
		endfor
		;;print,"[over,under,savedU]=",[over,startunder,savedU]
		if savedU then continue
			if  (~quasipar or ~startunder) and (stddev(BB[ishock:imax-t]) lt 1.1  or max(BB[ishock:imax])-mean(BB[ishock:imax]) lt 2 or (over and ud[i] lt max(ud[overstart:i])))  then begin;if 0 and ~quasipar and stddev(BB[ishock:imax-t]) lt 1.1 then begin

			;;print,"stddev(BB[ishock:imax-t])",stddev(BB[ishock:imax-t])
			startunder=0
			suloc=ishock+2*pp
			mono=1
			for k=suloc,imax-t do begin
				;print,suloc
				if ~startunder and (max(nBB[ishock:k]) lt down or k-ishock lt wid) then continue
				if dmd[k] lt dmd[k-1] then mono=0
				if dmd[k] ge dmd[k-1] and mono then continue

				if k +2 gt imax-t then break
				if ~ ((rhod[k-1] gt rhod[k]) and (rhod[k+1] gt rhod[k])) and ~startunder then continue
				startunder=1
					if ((rhod[k-1] ge rhod[k]) and (rhod[k+1] ge rhod[k])) and ~startunder and imax-k gt t+30  then begin
						suloc=k
					
						if ((rhod[k-1] gt rhod[k]) and (rhod[k+1] gt rhod[k])) and ~startunder then continue
				endif
				;suloc=i
				
				if ~ ((rhod[k-1] lt rhod[k]) and (rhod[k+1] lt rhod[k])) and imax-k gt t+30 then continue

				;if max(BB[suloc:k]) gt max(BB[ishock:suloc]) then begin
				;	suloc=k
				;	startunder=1
				;	continue
				;endif
				print,suloc,k
				rmax=max(BB[suloc:k],rloc)-1
				lmax=max(BB[ishock:suloc],lloc)
				if rmax gt lmax and 1.*(rloc+suloc-ishock)/(imax-ishock) lt .5 and imax-k gt t+30  then begin
							suloc=k
						startunder=1
							continue
				endif

				rmax=max(mm[suloc:k],rloc)-3
				lmax=max(mm[ishock:suloc],lloc)
				if rmax gt lmax and 1.*(rloc+suloc-ishock)/(imax-ishock) lt .3 and imax-k gt t+30 then begin
							suloc=k
							startunder=1
							continue
				endif

				;;print,"quasi"
				ioend[suloc]=1
				ioendloc[imin:imax]=suloc
				;k=mean([suloc,k])
				;while mm[min([k+t,imax])]-max(mm[ishock:suloc]) gt 2 and mm[min([k+t,imax])] eq max(mm[ishock:min([k+t,imax])]) do k--
				;dd=[k,min([k+t,imax])]
				st=k
				if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
				en=min([st+t,imax])
				if en ge imax and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .75 or fracdiff(mean(BB[st:en]),down) gt .40 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) then begin
					while st gt mean([suloc,k]) and st ge .3*(imax-ishock)+ishock  and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .7 or fracdiff(mean(BB[st:en]),down) gt .3 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) do begin
								st--
								en--
							endwhile
						print,time_string(xs[st])

						endif
				dd=[st,en]
				;;print,"dd=",dd
				for j=mean([imin,ishock]),imax do dsi[j,*]=dd
				for j=ishock,suloc do overregion[j]=1
				for j=ishock,suloc do overshoot[j]=BB[j]
				iuend[k]=1
				iuendloc[imin:imax]=k
				for j=suloc,k do underregion[j]=1
				for j=suloc,k do undershoot[j]=BB[j]
				for j=st,en do downtestinterval[j]=1
				;for j=i,min([i+t,imax]) do downBmag[j]=BB[j]
				savedU=1
				;;print,"savedU v2"
				break
			endfor

		endif
		;;print,"[over,under,savedU]=",[over,startunder,savedU]
		if startunder and ~savedU   then begin
				;;print,"startunder and ~savedU"
				suloc=2*pp+ishock
				if   stddev(BB[suloc:imax-t]) lt 1.1  then begin
					;;print,"stddev(BB[suloc:imax-t]) lt 1.1"
					for k=suloc,imax-t-1 do begin
						if k +2 gt imax-t then break
						if total(B20d[ishock:k]  lt 0) eq 0 then continue 
						if ~ ((rhod10[k-1] lt rhod10[k]) and (rhod10[k+1] lt rhod10[k])) then continue
					;	if max(BB[suloc:k])-1 gt max(BB[ishock:suloc]) then begin
						rmax=max(BB[suloc:k],rloc)-1
						lmax=max(BB[ishock:suloc],lloc)
						if rmax gt lmax and 1.*(rloc+suloc-ishock)/(imax-ishock) lt .5 and imax-k gt t+30 then begin
							suloc=k
						;startunder=1
							continue
						endif


						;k=mean([suloc,k])
						st=k
						if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
						en=min([st+t,imax])
						if en ge imax and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .75 or fracdiff(mean(BB[st:en]),down) gt .40 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) then begin
							while st gt mean([suloc,k]) and st ge .3*(imax-ishock)+ishock  and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .7 or fracdiff(mean(BB[st:en]),down) gt .3 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) do begin
								st--
								en--
							endwhile
						print,time_string(xs[st])

						endif
						dd=[st,en]
					;	dd=[k,min([k+t,imax])]
						;;print,"~savedU, dd=",dd
						for j=mean([imin,ishock]),imax do dsi[j,*]=dd
						savedU=1
						;;print,"savedU v3"
				for j=ishock,suloc do overregion[j]=1
				for j=ishock,suloc do overshoot[j]=BB[j]
				for j=suloc,k do underregion[j]=1
				for j=suloc,k do undershoot[j]=BB[j]
				for j=st,en do downtestinterval[j]=1
				ioend[suloc]=1
				ioendloc[imin:imax]=suloc
				iuend[i]=1
				iuendloc[imin:imax]=i
				;for j=k,min([k+t,imax]) do downBmag[j]=BB[j]
						break
					endfor

				endif 
				downmax=max(ud[suloc:imax-t],umaxloc)
				bmax=max(ud[ishock:suloc],maxloc)
				if not savedU and ((downmax le bmax +3))  then begin
					;;print,"not saved"
				;if mmd[imax] gt 10 then startunder=0 else begin
					ioend[suloc]=1
					ioendloc[imin:imax]=suloc
					q=max(ud[suloc:imax-t],maxloc)
					i=umaxloc+suloc
					iuend[i]=1
					;dd=[i,min([i+t,imax])]
					st=i
					if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
					en=min([st+t,imax])
					if en ge imax and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .75 or fracdiff(mean(BB[st:en]),down) gt .40 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) then begin
						while st gt mean([suloc,i]) and st gt .3*(imax-ishock)+ishock and mean(BB[st:en]) lt down and (mean(BB[st:en])/down lt .7 or fracdiff(mean(BB[st:en]),down) gt .3 or mean(BB[st:en]) +4 lt down or mm[en] + 4 lt down) do begin
							st--
							en--
						endwhile
						print,time_string(xs[st])

					endif				
					dd=[st,en]
					;;print,"dd=",dd
				for j=mean([imin,ishock]),imax do dsi[j,*]=dd
					iuendloc[imin:imax]=umaxloc+suloc
				for j=ishock,suloc do overregion[j]=1
				for j=ishock,suloc do overshoot[j]=BB[j]
				for j=suloc,umaxloc+suloc do underregion[j]=1
				for j=suloc,umaxloc+suloc do undershoot[j]=BB[j]
				for j=st,en do downtestinterval[j]=1
				;for j=i,min([i+t,imax]) do downBmag[j]=BB[j]
					savedU=1
					;;print,"savedU v4"
				endif
				;endelse
		endif
		;;print,"[over,under,savedU]=",[over,startunder,savedU]
		if 0 and imax-suloc lt pp*3 and ((~startunder or ~over) and ~savedU) and stddev(BB[.5*ishock+.5*imax:imax])*2 lt abs(median(BB[.5*ishock+.5*imax:imax])-down) then begin;~startunder then begin
			FFI[imin:imax]=0
			allimaxs[imin:imax]=-1

			allimins[imin:imax]=-1
			inchis[imin:imax]=0
			si[ishock]=0
			slii[imin:imax]=-1

			subli[imin:imax]=0
			sublib[imin:imax]=0
			sublie[imin:imax]=0

			;indowns[imin:imax]=0
			;inups[imin:imax]=0
			
		endif
	endfor
	;;;print,"[over,under,savedU]=",[over,startunder,savedU]
	datFFI.y=FFI
	datFFI.chis=inchis
	;datFFI.imins=allimins
	;datFFI.imaxs=allimaxs
	datSLI.y=slii
	datsi.y=si
	datsubli.y=subli
	datsublib.y=sublib
	datsublie.y=sublie
	datFFI.inups=inups
	datFFI.indowns=indowns
	store_data,"Franken_fitted_inbound",data=datFFI
	store_data,"shocks_inbound",data=datsi
	store_data,"shock_locs_inbound",data=datSLI
	store_data,'sublengths_inbound',data=datsubli
	store_data,'sublengths_inbound_begin',data=datsublib
	store_data,'sublengths_inbound_end',data=datsublie

	store_data,'overshoot_region_inbound',data={x:xs,y:overregion,ytitle:"overshoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'overshoot_inbound',data={x:xs,y:overshoot,ytitle:"overshoot [nT]"}


	;store_data,'undershoot_region_inbound',data={x:xs,y:underregion,ytitle:"undershoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'undershoot_inbound',data={x:xs,y:undershoot,ytitle:"undershoot [nT]"}
	;options,'undershoot_inbound','colors','c'
	store_data,'downstream_interval_inbound',data={x:xs,y:downtestinterval,ytitle:"downstream measure region"}

	print,'~~~~~~~~~~~~~'
	print,"outbound side:"
	print,'~~~~~~~~~~~~~'
	for el=0, ocount-1 do begin
		print,"el=",el
		ishock=slo[el]
		imax=max([omaxs[el],omins[el]])
		imin=min([omaxs[el],omins[el]]);omins[el]
		i=ishock
		print,time_string(xs[N-1-ishock])

		smallfracdiffO=onofflength(BfdiffO lt .1)
		BfdiffO=fracdiff(FFO,nmm)
		smallerfracdiffO=onofflength(nfmmd lt .1)

		;B1a=1*(nB1[ishock] gt Fdo[ishock])
		;B2a=1*(nB2[ishock] gt Fdo[ishock])
		;B3a=1*(nB3[ishock] gt Fdo[ishock
		pp=npt[ishock]
		t=pp*ceil(60/pp)
		;while round(t) le 60 do t+=pt	
		;t=min([6*pp,120]);t=120;npt[ishock]
		;t=min([max([60,6*pp]),120])
		over=nOD[ishock]
		startunder=0
		endunder=0
		suloc=ishock
		savedU=0
		wid=owidths[ishock]
		overstart=ishock
		down=outdowns[ishock]
		maxnegblock=min(nmmblock[ishock:imax])
		;nmmd[imin:imax]=nmm[imin:imax]-down
		;nfmmd[imin:imax]=nmmd[imin:imax]
		;BfdiffO[imin:imax]=2*abs(down-nmm[imin:imax])/(down+nmm[imin:imax])
		if outdowns[ishock] ne outdowns[imax] and outdowns[ishock] ne 0 then begin

			imaxt=(where(outdowns[ishock:imax] eq down))[-1]+ishock
			for kk=imaxt+1,min([imax,N-1]) do begin
				allomins[kk]=-1

				FFO[kk]=0
				sloo[kk]=-1
				outdowns[kk]=0
				sublo[kk]=0
				sublob[kk]=0
				subloe[kk]=0
				;outdowns[kk]=0
				;outups[kk]=0
			
			endfor
			allomins[imin:imaxt]=N-1-imaxt
			imax=imaxt
			omaxs[el]=imaxt

		endif

		IF bfdiffO[imax-2] ge .25 and max(smallfracdiffO[ishock:imax]) gt 5*60. then begin
			imaxt=(where(smallfracdiffO[ishock:imax] eq	max(smallfracdiffO[ishock:imax],mxcnt)))[-1]+ishock
			if mxcnt gt 0 and imaxt-ishock gt 5*60 then begin
			for kk=imaxt+1,min([imax,N-1]) do begin
				allomins[kk]=-1

				FFO[kk]=0
				sloo[kk]=-1

				sublo[kk]=0
				sublob[kk]=0
				subloe[kk]=0
				;outdowns[kk]=0
				;outups[kk]=0
			
			endfor
			allomins[imin:imaxt]=N-1-imaxt
			imax=imaxt
			omaxs[el]=imaxt
			endif
		endif else begin
			IF nfmmd[imax-2] ge .20 and max(smallerfracdiffO[ishock:imax]) ge 5*60. then begin
			imaxt=(where(smallerfracdiffO[ishock:imax] eq	max(smallerfracdiffO[ishock:imax],mxcnt)))[-1]+ishock
			if  mxcnt gt 1 and imaxt-ishock gt 5*60 then begin
			for kk=imaxt+1,min([imax,N-1]) do begin
				allomins[kk]=-1

				FFO[kk]=0
				sloo[kk]=-1

				sublo[kk]=0
				sublob[kk]=0
				subloe[kk]=0
				;outdowns[kk]=0
				;outups[kk]=0
			
			endfor
			allomins[imin:imaxt]=N-1-imaxt
			imax=imaxt
			omaxs[el]=imaxt
			endif
		endif 

		endelse


		if ishock ge imax-t or total(finite(nBB[ishock:imax]) eq 0) ne 0 or (maxnegblock lt 0 and (-1.0*maxnegblock/(imax-ishock) gt .4 or -1* maxnegblock ge 10*60  )  ) then begin
			for k=imin,imax do begin
				;allomins[k]=-1
				outchis[k]=0
				FFO[k]=0
				;allomaxs[k]=-1
				sloo[k]=-1

				sublo[k]=0
				sublob[k]=0
				subloe[k]=0
				outdowns[k]=0
				outups[k]=0
			
			endfor
				so[ishock]=0

			;;print,x2greg(xs[N-1-ishock],/str)
			continue

		endif

		mmdmean=nmmd[(ishock+imax)/2]
		overstart=ishock
		mono=1

		i0d8=.8*(imax)+.2*(ishock)
		bound=max([i0d8,imax-t])

		fullmax=max(nBB[ishock:imax-t],fullmaxloc)


		whereBAD=WHERE(nmmdtrig[ishock:imax] ne 0)
		jj=ishock
		if whereBad[0] eq 0 then begin
				;jj=ishock
				while nmmdtrig[jj] ne 0 and jj lt imax-1 do jj++
				whereBAD=WHERE(nmmdtrig[jj:imax] ne 0)
		endif
		imax0=imax
		if whereBad[0] ne -1 and (whereBad[-1]+jj eq imax) then begin
				while whereBad[-1]+jj eq imax and imax gt max([(imax0+ishock)/2,ishock+2*60.]) and whereBad[-1] ne -1 do begin
					imax--
					if numel(whereBad) gt 1 then whereBad=temporary(whereBad[0:-2]) else $
					if numel(whereBad) eq 1 then whereBad=whereBad[0] else whereBad = -1		
				endwhile

				for kk=imax+1,min([imax0,N-1]) do begin
				allomins[kk]=-1

				FFO[kk]=0
				sloo[kk]=-1

				sublo[kk]=0
				sublob[kk]=0
				subloe[kk]=0
				;outdowns[kk]=0
				;outups[kk]=0
			
				endfor
				allomins[imin:imax]=N-1-imax
			;imax=imaxt
				omaxs[el]=imax
				;so[ishock]=0


			endif
		if whereBad[0] ne -1 then begin
			
			whereBad=temporary(whereBAD)+jj
			pbool=max(abs(nmmdtrig[whereBAD])) ge 3
			qbool=whereBad[0] lt imax-2*pp
			rbool=total(nmmdtrig[whereBAD]) *1.0 /(imax-ishock) gt .1
			;sbool=total(mmdtrig[whereBAD]) *1.0 /(imax-ishock) lt .5
			if ((~pbool or rbool) and (pbool or ( qbool and  rbool))) then begin
			for k=imin,imax do begin
				;allomins[k]=-1
				outchis[k]=0
				FFO[k]=0
				;allomaxs[k]=-1
				sloo[k]=-1

				sublo[k]=0
				sublob[k]=0
				subloe[k]=0
				;outdowns[k]=0
				;outups[k]=0
			
			endfor
				so[ishock]=0
				continue

			endif

		endif

		if max(abs(nmmd[imax-5:imax])) gt maxMMD or max(nBB[ishock:imax]) eq nBB[imax] or imax eq N then begin
			imaxt=max([(where(abs(nmmd[ishock:imax]) le maxMMD))[-1],([2*t/2+1,imax-ishock])])+ishock
			;;print,ishock,imaxt,imax
			IF imaxt LT imax and (imaxt ge ishock+t) then begin
			for kk=imaxt+1,min([imax,N-1]) do begin
				allomins[kk]=-1

				FFO[kk]=0
				sloo[kk]=-1

				sublo[kk]=0
				sublob[kk]=0
				subloe[kk]=0
				;outdowns[kk]=0
				;outups[kk]=0
			
			endfor
			allomins[imin:imaxt]=N-1-imaxt
			imax=imaxt
			omaxs[el]=imaxt
			endif
		endif

		if imax-ishock lt t then begin
			;;print,"bad point"

			for k=imin,imax do begin
				;allomins[k]=-1
				outchis[k]=0
				FFO[k]=0
				;allomaxs[k]=-1
				sloo[k]=-1

				sublo[k]=0
				sublob[k]=0
				subloe[k]=0
				outdowns[k]=0
				outups[k]=0
			
			endfor
				so[ishock]=0
			continue
		endif

		;t=min([120,imax-ishock-1])
		;t=min([max([60,6*pp]),120])
		;t=min([6*pp,120]);t=120;npt[ishock]
		quasipar=0
		for i=ishock,imax-t do begin

				
			numabove=total(ndmd[i+1:imax] gt 0)
			numbelow=total(ndmd[i+1:imax] lt 0)

			if nrhod[i] gt 0  then continue
			if over and (total(nB20d[ishock:i]  lt 0) eq 0 or max(nBB[ishock:i]) lt down or i-ishock lt wid) then continue 
			if i +2 gt imax-t then break
			if i +2 gt mean([imax,ishock]) then break
			if ndmd[i] lt ndmd[i-1] then mono=0
			if ndmd[i] ge ndmd[i-1] and mono then continue
			;B1a=1*(nB1[i] gt Fdo[i])
			;B2a=1*(nB2[i] gt Fdo[i])
			;B3a=1*(nB3[i] gt Fdo[i])
			quasipar=1
			if over and nud[i] gt 1 and i-ishock gt 4*pp then begin
				;;print,"startunder=1"
				if ~startunder then suloc=i
				startunder=1
				continue
			endif
			if startunder and nud[i] lt 0 then begin;total(nud[i:i+6] lt 0) eq 7  then begin;nud[i] lt 0 then begin
				over=0
				;;print,"foreshock"
				startunder=0
				suloc=ishock
				continue
			endif
			if startunder and nud[i] eq 0 then begin;total(nud[i:i+3] eq 0) eq 4  then begin;and nud[i] eq 0 then begin
				;dso[mean([imin,ishock]):imax,*]=[i,min([i+t,N-1])]


				overmax=max(nBB[ishock:suloc],maxloc)
				downmax=max(nBB[i:min([i+t,imax])],dmaxloc)
				thusmax=max(nBB[ishock:min([i+t,imax])],thusmaxloc)
				if (i-ishock)/(imax-ishock) le .05 and max(nBB[i:min([i+t,imax])]) eq max(nBB[ishock:imax]) then begin
				;if ((i-ishock)/(imax-ishock) lt .05 and downmax eq thusmax) or ( (i-ishock)/(imax-ishock) lt .1 and  mean(nBB[i:i+t])-down gt 4 and numbelow gt 60 )  then begin
					over=1
					startunder=0
					continue
				endif


				while max(nmm[i:min([i+t,imax])])-max(nmm[ishock:suloc]) ge 2 and max(nmm[i:min([i+t,imax])]) eq max(nmm[ishock:min([i+t,imax])]) and i-suloc gt 1 do i--
				if max(nBB[suloc:i])-1 gt max(nBB[ishock:suloc]) and imax-t-i gt 30 and i-ishock gt 20 then begin
					suloc=i
					startunder=1
					continue
				endif
				st=i

				if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
				en=min([st+t,imax])
				if en ge imax and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .75 or fracdiff(mean(nBB[st:en]),down) gt .40 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) then begin
						while st gt mean([suloc,i]) and st gt .3*(imax-ishock)+ishock and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .7 or fracdiff(mean(nBB[st:en]),down) gt .3 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) do begin
							st--
							en--
						endwhile
						print,time_string(xs[N-1-st])

					endif	
				dd=[st,en]
				for j=mean([imin,ishock]),imax do dso[j,*]=dd
				print,"CASE 1:
				print,"       ishock at",time_string(xs[N-1-ishock])
				print,"       ooend at",time_string(xs[N-1-suloc])
				print,"       ouend at",time_string(xs[N-1-i])
				print,"ishock,suloc,i,imax-t,imax=",ishock,suloc,i,imax-t,imax
			
				ooend[suloc]=1
				ooendloc[imin:imax]=suloc
				ouend[i]=1
				ouendloc[imin:imax]=i
				savedU=1
				for j=ishock,suloc do noverregion[j]=1
				for j=ishock,suloc do novershoot[j]=nBB[j]
				for j=suloc,i do nunderregion[j]=1
				for j=suloc,i do nundershoot[j]=nBB[j]
				for j=st,en do ndowntestinterval[j]=1
				;for j=i,min([i+t,imax]) do ndownBmag[j]=nBB[j]
				break
			endif
			if nOD[i] gt 0 and ~over then begin
				;;print,"over"
				over=1
				overstart=i
			endif			
		endfor
		if savedU then continue
		;;print,ishock,overstart,i,nud[i],max(nud[overstart:i]),imax-t
		if  (~quasipar or ~startunder) and ishock lt imax-t and (stddev(nBB[ishock:imax-t]) lt 1.1  or max(nBB[ishock:imax])-mean(nBB[ishock:imax]) lt 2 or (over and nud[i] lt max(nud[overstart:i])))   then begin;or (over and nud[i] lt max(nud[overstart:i]) )  then begin
			;;print,"QUASIPERP"
			startunder=0
			suloc=ishock+2*pp
			for k=suloc,imax-t do begin
				if ~startunder and (max(nBB[ishock:k]) lt down or k-ishock lt wid) then continue
				if ~ ((nrhod[k-1] gt nrhod[k]) and (nrhod[k+1] gt nrhod[k])) and ~startunder then continue
				if k +2 gt imax-t then break
				if ndmd[i] lt ndmd[i-1] then mono=0
				if ndmd[i] ge ndmd[i-1] and mono then continue
				if ((nrhod[k-1] ge nrhod[k]) and (nrhod[k+1] ge nrhod[k])) and ~startunder then begin
					startunder=1
					suloc=k
					
					if ((nrhod[k-1] gt nrhod[k]) and (nrhod[k+1] gt nrhod[k])) and ~startunder then continue
				endif
				;;print,"suLOC=",k
				

				if ~ ((nrhod[k-1] lt nrhod[k]) and (nrhod[k+1] lt nrhod[i])) and imax-k gt t+30 then begin
					
					continue

				endif


				if max(nBB[suloc:k])-1 gt max(nBB[ishock:suloc]) and imax-k gt t+30  and k-ishock gt 20 then begin
							suloc=k
						;startunder=1
							continue
				endif



				rmax=max(nmm[suloc:k],rloc)-3
				lmax=max(nmm[ishock:suloc],lloc)
				if rmax gt lmax and 1.*(rloc+suloc-ishock)/(imax-ishock) lt .3 and imax-k gt t+30  and k-ishock gt 20 then begin
							suloc=k
							startunder=1
							continue
				endif
				ooend[suloc]=1
				ooendloc[imin:imax]=suloc
				i=mean([suloc,k])
				st=i
				if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
				en=min([st+t,imax])
				if en ge imax and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .75 or fracdiff(mean(nBB[st:en]),down) gt .40 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) then begin
						while st gt mean([suloc,i]) and st gt .3*(imax-ishock)+ishock and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .7 or fracdiff(mean(nBB[st:en]),down) gt .3 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) do begin
							st--
							en--
						endwhile
						print,time_string(xs[N-1-st])

				endif
				dd=[st,en]

				for j=ishock,imax do dso[j,*]=dd
				savedU=1
				for j=ishock,suloc do noverregion[j]=1
				for j=ishock,suloc do novershoot[j]=nBB[j]
				for j=suloc,i do nunderregion[j]=1
				for j=suloc,i do nundershoot[j]=nBB[j]
				for j=st,en do ndowntestinterval[j]=1
				ouend[i]=1
				ouendloc[imin:imax]=i
				print,"CASE 2:
				print,"       ishock at",time_string(xs[N-1-ishock])
				print,"       ooend at",time_string(xs[N-1-suloc])
				print,"       ouend at",time_string(xs[N-1-i])
				;for j=i,min([i+t,imax]) do ndownBmag[j]=nBB[j]
				break
			endfor

		endif

		if startunder and ~savedU then begin
				if  stddev(nBB[suloc:imax-t]) lt 1.1  then begin
					for k=suloc,imax-t do begin
						if k +2 gt imax-t then break
						if  total(nB20d[ishock:k]  lt 0) eq 0 or  k-ishock lt 20 then continue 
						if ~ ((nrhod10[k-1] lt nrhod10[k]) and (nrhod10[k+1] lt nrhod10[k])) then continue
						if max(nBB[suloc:k])-1 gt max(nBB[ishock:suloc]) and imax-k gt t+30 then begin
							suloc=k
;							startunder=1
							continue
						endif

						
						k=mean([suloc,k])

						st=k
						if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
						en=min([st+t,imax])
						if en ge imax and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .75 or fracdiff(mean(nBB[st:en]),down) gt .40 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) then begin
						while st gt mean([suloc,k]) and st gt .3*(imax-ishock)+ishock and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .7 or fracdiff(mean(nBB[st:en]),down) gt .3 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) do begin
							st--
							en--
						endwhile
						print,time_string(xs[N-1-st])

					endif
						print,"CASE 3:
						print,"       ishock at",time_string(xs[N-1-ishock])
						print,"       ooend at",time_string(xs[N-1-suloc])
						print,"       ouend at",time_string(xs[N-1-k])
						dd=[st,en]
						ooend[suloc]=1
						ooendloc[imin:imax]=suloc
						ouend[k]=1
						ouendloc[imin:imax]=k
						for j=ishock,imax do dso[j,*]=dd;[k,min([k+t,imax])]
						savedU=1
						for j=ishock,suloc do noverregion[j]=1
						for j=ishock,suloc do novershoot[j]=nBB[j]
						for j=suloc,k do nunderregion[j]=1
						for j=suloc,k do nundershoot[j]=nBB[j]
						for j=st,en do ndowntestinterval[j]=1

						;for j=i,min([i+t,imax]) do ndownBmag[j]=nBB[j]
						break
					endfor

				endif
				if not savedU and suloc gt ishock and suloc lt imax  then begin
				;if nmmd[imax] gt 10 then startunder=0 else begin
					ooend[suloc]=1
					ooendloc[imin:imax]=suloc
					q=max(nud[suloc:i],maxloc)
					i=maxloc+suloc
					ouend[i]=1
					ouendloc[imin:imax]=i
					for j=ishock,suloc do noverregion[j]=1
					for j=ishock,suloc do novershoot[j]=nBB[j]
					for j=suloc,maxloc+suloc do nunderregion[j]=1
					for j=suloc,maxloc+suloc do nundershoot[j]=nBB[j]


					st=i
					if st lt .3*(imax-ishock)+ishock then st=.3*(imax-ishock)+ishock
					en=min([st+t,imax])
					if en ge imax and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .75 or fracdiff(mean(nBB[st:en]),down) gt .40 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) then begin
						while st gt mean([suloc,i]) and st gt .3*(imax-ishock)+ishock and mean(nBB[st:en]) lt down and (mean(nBB[st:en])/down lt .7 or fracdiff(mean(nBB[st:en]),down) gt .3 or mean(nBB[st:en]) +4 lt down or nmm[en] + 4 lt down) do begin
							st--
							en--
						endwhile
						print,time_string(xs[N-1-st])

					endif
					dd=[st,en]

					for j=ishock,imax do dso[j,*]=dd;[i,min([i+t,imax])]
					for j=st,en do ndowntestinterval[j]=1
					print,"CASE 4:
						print,"       ishock at",time_string(xs[N-1-ishock])
						print,"       ooend at",time_string(xs[N-1-suloc])
						print,"       ouend at",time_string(xs[N-1-i])
					;for j=i,min([i+t,imax]) do ndownBmag[j]=nBB[j]
					savedU=1
				endif
		endif
		
		if 0 and imax-suloc lt pp*3 and ~startunder and ~savedU and stddev(nBB[.5*ishock+.5*imax:imax])*2 lt abs(median(nBB[.5*ishock+.5*imax:imax])-down) then begin
			;;print,"bad point"

			for k=imin,imax do begin
				;allomins[k]=-1
				outchis[k]=0
				FFO[k]=0
				;allomaxs[k]=-1
				sloo[k]=-1

				sublo[k]=0
				sublob[k]=0
				subloe[k]=0
				outdowns[k]=0
				outups[k]=0
			
			endfor
				so[ishock]=0
		endif
	endfor
	dso=reverse(dso)
	for i=0, numel(dso)-1 do if dso[i] ne 0 then dso[i]=N-1-dso[i]
	outups=reverse(outups)
	outdowns=reverse(outdowns)
	novershoot=reverse(novershoot)
	noverregion=reverse(noverregion)

	nundershoot=reverse(nundershoot)
	nunderregion=reverse(nunderregion)
	ndowntestinterval=reverse(ndowntestinterval)

	store_data,'overshoot_region_outbound',data={x:xs,y:noverregion,ytitle:"overshoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'overshoot_outbound',data={x:xs,y:novershoot,ytitle:"overshoot [nT]"}


	;store_data,'undershoot_region_outbound',data={x:xs,y:underregion,ytitle:"undershoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'undershoot_outbound',data={x:xs,y:undershoot,ytitle:"undershoot [nT]"}
	store_data,'downstream_interval_outbound',data={x:xs,y:ndowntestinterval,ytitle:"downstream measure region"}
	ooend=reverse(ooend)
	ooendloc=reverse(ooendloc)

	ouend=reverse(ouend)
	ouendloc=reverse(ouendloc)

	FFO=REVERSE(FFO)
	outchis=REVERSE(outchis)
	so=REVERSE(so)
	allomaxs=reverse(allomaxs)
	allomins=reverse(allomins)
	
	sloo=REVERSE(sloo)
	sublo=REVERSE(sublo)
	sublob=REVERSE(sublob)
	subloe=REVERSE(subloe)

	datFFO.y=FFO
	datFFO.chis=outchis
	datFFO.imins=allomins
	datFFO.imaxs=allomaxs
	datSLO.y=sloo
	datso.y=so
	datsublo.y=sublo
	datsublob.y=sublob
	datsubloe.y=subloe
	datFFO.outups=outups
	datFFO.outdowns=outdowns
	store_data,"Franken_fitted_outbound",data=datFFO
	store_data,"shocks_outbound",data=datso
	store_data,"shock_locs_outbound",data=datSLO
	store_data,'sublengths_outbound',data=datsublo
	store_data,'sublengths_outbound_begin',data=datsublob
	store_data,'sublengths_outbound_end',data=datsubloe

	ooendloc[where(ooendloc ne 0)] -=N-1

	ooendloc*=-1

	ooendloc[where(ooendloc eq 0)] =-1

	store_data,"overshoot_end_inbound",data={x:datFFI.x,y:ioend}
	store_data,"overshoot_end_loc_inbound",data={x:datFFI.x,y:ioendloc}
	store_data,"overshoot_end_outbound",data={x:datFFI.x,y:ooend}
	store_data,"overshoot_end_loc_outbound",data={x:datFFI.x,y:ooendloc}

	store_data,"undershoot_end_inbound",data={x:datFFI.x,y:iuend}
	store_data,"undershoot_end_loc_inbound",data={x:datFFI.x,y:iuendloc}
	store_data,"undershoot_end_outbound",data={x:datFFI.x,y:ouend}
	store_data,"undershoot_end_loc_outbound",data={x:datFFI.x,y:ouendloc}

	store_data,"downstream_indices_inbound",data={x:datFFI.x,y:dsi,ytitle:"index_sub"}
	store_data,"downstream_indices_outbound",data={x:datFFI.x,y:dso,ytitle:"index_sub"}

	;store_data,"downstream_flags_inbound",data={x:datFFI.x,y:inflags}  ;need to fix later. Nothing should use it though
	;store_data,"downstream_flags_outbound",data={x:datFFI.x,y:outflags}
	ds=dso*0.0


	oend=fltarr(N)
	oendloc=fltarr(N)-1
	uend=fltarr(N)
	uendloc=fltarr(N)-1
	shocks=si
	sl=oendloc
	ups=fltarr(N)
	downs=fltarr(N)
	allmaxs=oendloc
	allmins=oendloc
	chis=oend
;	get_data,"overshoot_end_loc_outbound",data=datoeo
	FF=oend
	for i=0, N-1 do begin
		if so[i] ne 0 then shocks[i] = so[i]

		outSL=sloo[i]
		inSL=slii[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL)))  and i gt allimins[i] and i lt allimaxs[i] then begin
			sl[i] = inSL
			FF[i]=FFI[i]
			oendloc[i]=ioendloc[i]
			uendloc[i]=iuendloc[i]
			allmaxs[i]=allimaxs[i]
			allmins[i]=allimins[i]
			ups[i]=inups[i]
			downs[i]=indowns[i]
			chis[i]=inchis[i]
			ds[i,*]=dsi[i,*]
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL))) and i gt allomins0[i] and i lt allomaxs0[i] then begin
			FF[i]=FFO[i]
			ups[i]=outups[i]
			downs[i]=outdowns[i]
			;	zout[i]=zout[i]
			;downs[i] = outdowns[i]
			;ups[i] = outups[i]
			sl[i] = outSL
			;downups[i] = outdownups[i]
			allmaxs[i]=allomaxs[i]
			oendloc[i]=ooendloc[i]
			uendloc[i]=ouendloc[i]
			allmins[i]=allomins[i]

			chis[i]=outchis[i]
			ds[i,*]=dso[i,*]
			ENDIF 	
		ENDELSE

		;if zin[i] ne 0 then zout[i]=zin[i]
		
		;if outdowns[i] ne 0 then downs[i] = outdowns[i]
		;if outups[i] ne 0 then ups[i] = outups[i]
		;if outShockLocs[i] ne -1 then shockLocs[i] = outShockLocs[i]
		;if outdownups[i] ne 0 then downups[i] = outdownups[i]
		;if outimaxs[i] ne -1 then imaxs[i]=outimaxs[i]
		;if outimins[i] ne -1 then imins[i]=outimins[i]
	endfor
	oend=ioend+ooend
	uend=iuend+ouend

	for j=0,N-1 do begin
		overshoot[j]+=novershoot[j]
		overregion[j]+=noverregion[j]
		undershoot[j]+=nundershoot[j]
		underregion[j]+=nunderregion[j]
		downtestinterval[j]+=ndowntestinterval[j]
	endfor

	store_data,'overshoot_region',data={x:xs,y:overregion,ytitle:"overshoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'overshoot',data={x:xs,y:overshoot,ytitle:"overshoot [nT]"}


	;store_data,'undershoot_region',data={x:xs,y:underregion,ytitle:"undershoot flag"}
	;options,'overshoot_region_inbound','colors','r'

	store_data,'undershoot',data={x:xs,y:undershoot,ytitle:"undershoot [nT]"}
	;options,'undershoot_inbound','colors','c'
	store_data,'downstream_interval',data={x:xs,y:downtestinterval,ytitle:"downstream measure region"}

	store_data,"overshoot_end",data={x:datFFI.x,y:oend}
	store_data,"overshoot_end_loc",data={x:datFFI.x,y:oendloc}

	store_data,"undershoot_end",data={x:datFFI.x,y:uend}
	store_data,"undershoot_end_loc",data={x:datFFI.x,y:uendloc}

	get_data,"Franken_fitted",data=datFF
	datFF.imaxs=allmaxs
	datFF.imins=allmins
	datFF.y=FF ; {x:xs,y:zout,ytitle:datin.ytitle,downups:downups,downs:downs,ups:ups,imins:imins,imaxs:imaxs,chis:chis}
	;datFF.imaxs=allmaxs
	;datFF.imins=allmins
	datFF.chis=chis	
	datFF.ups=ups
	datFF.downs=downs
	store_data,"Franken_fitted",data=datFF
	store_data,"downstream_fit",data={x:datFFI.x,y:downs,ytitle:'MM[3]+MM[0]'}
	store_data,"upstream_fit",data={x:datFFI.x,y:ups,ytitle:'MM[3]-MM[0]'}
	store_data,"downstream_fit",data={x:datFFI.x,y:downs}
	get_data,"shocks",data=dats

	dats.y=shocks
	store_data,"shocks",data=dats
	get_data,"shock_locs",data=datsl;{x:xs,y:shockLocs,ytitle:'shock location'}
	store_data,"downstream_indices",data={x:datFFI.x,y:ds,ytitle:"index_sub"}
	datsl.y=sl
	store_data,"shock_locs",data=datsl
	;store_data,"avs2ff",data="Franken_fitted B_1stModalAverage B_2ndModalAverage B_3rdModalAverage"
	;store_data,'avgmmff',data="Franken_fitted mvn_B_1sec_MAVEN_MSO_Mag_modal_mean"

	;options,'overshoot*','colors','r'
	;options,'undershoot*','colors','c'
	options,"overshoot",'colors','r'
	;options,"B_maxs",'colors','g'
	store_data,'Bfo',data='mvn_B_1sec_MAVEN_MSO_Mag Franken_fitted overshoot'
	;store_data,'bfo',data='mvn_B_1sec_MAVEN_MSO_Mag overshoot Franken_fitted'
	;store_data,'bfou',data='mvn_B_1sec_MAVEN_MSO_Mag overshoot undershoot Franken_fitted'
	;store_data,'bf_oud',data='mvn_B_1sec_MAVEN_MSO_Mag overshoot undershoot downstream_interval Franken_fitted'
	;tplot,'bfou',/add

end


