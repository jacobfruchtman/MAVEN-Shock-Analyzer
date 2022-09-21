
pro upstreamindexer

		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout


		if total(datin.y + datout.y) eq 0 then return
		xs=datin.x
		N=numel(xs)
		get_data,'wind_cleanmean_interpolated',data=datFlag

		get_data,'B_precision',data=datBp
		get_data,"Franken_fitted_inbound",data=datFFI;{x:xs,y:zin,ytitle:datin.ytitle,inups:inups,indowns:indowns,imins:inimins,imaxs:inimaxs,chis:inChis}

		get_data,"Franken_fitted_outbound",data=datFFO;{x:xs,y:zout,ytitle:datin.ytitle,outups:outups,outdowns:outdowns,imins:outimins,imaxs:outimaxs,chis:outChis}
		get_data,"Franken_fitted",data=datFF
		get_data,"B_stddev",data=datSTD
		get_data,"Franken_fittedderiv",data=datFFD

		ffd=abs(datFFD.y)
		std=datSTD.y
		get_data,"B_meanMode_smoothed",data=datmm
		ymm=datmm.y

		get_data,'foot_start_inbound',data=datFootI
		get_data,'foot_start_outbound',data=datFootO

		get_data,'forecountdown',data=datFore

		get_data,"B_minute",data=datBmin
		get_data,"B60deriv",data=datB60d

		Bminute=datBmin.y
		nBminute=reverse(Bminute)
		B60d=datB60d.y
		nB60d=-1*REVERSE(B60d)

		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		;get_data,'BB_flag',data=datBB
		;get_data,'bBB_flag',data=datbBB
		;get_data,'CC_flag',data=datCC
		;get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'bDD_flag',data=datbDD
		get_data,'DD_effective_flag',data=dateDD
		get_data,'bDD_effective_flag',data=datebDD
		eDD = WHERE(dateDD.y ne 0, eddn);defines the boundaries in  forward direction. beginning  of last point definitely after a foreshock
		DD = WHERE(datDD.y ne 0, ddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan); beginnging of shock 

		;CC = WHERE(datCC.y ne 0, ccn) ;end of negative shock.  defines boundaries in  negative time  direction
		;BB = WHERE(datBB.y ne 0, bbn); end of  negative  shock

		ebDD = WHERE(datebDD.y ne 0, ebddn);defines the boundaries in  backward direction. end  of first point definitely before a foreshock
		bDD = WHERE(datbDD.y ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		bAA = WHERE(datbAA.y ne 0, naan); beginnging of shock 

		;bCC = WHERE(datbCC.y ne 0, nccn) ;end of negative shock.  defines boundaries in  negative time  direction
		;bBB = WHERE(datbBB.y ne 0, nbbn); end of  negative  shock
		
		if DD[0] gt AA[0] then begin
			DD=[0,DD]
			ddn++
		endif
		if bDD[-1] lt bAA[-1] then begin
			bDD=[bDD,N-1]
			nddn++
		endif
		

		;spikeFiller,'DD_effective_flag',/toloc,side="full"
		;spikeFiller,'bDD_effective_flag',/toloc,side="full"
		;get_data,'DD_effective_flag_flattened_loc',data=dateDD
		;get_data,'bDD_effective_flag_flattened_loc',data=datebDD
		;spikeFiller,'DD_flag',/toloc,side="full"
		;spikeFiller,'bDD_flag',/toloc,side="full"
		;get_data,'DD_flag_flattened_loc',data=datDD
		;get_data,'bDD_flag_flattened_loc',data=datbDD

		get_data,'mvn_B_1sec_Mag',data=datB
		;eDD=dateDD.y
		;beDD=datebDD.y

		;DD=datDD.y
		;bDD=datbDD.y

		indowns=datFFI.indowns
		inups=datFFI.inups

		outdowns=datFFO.outdowns
		outups=datFFO.outups

		footI=datFootI.y
		footO=datFootI.y
		foremax=datFore.ymax
		fore=datFore.y
		Bp=datBp.y
		B=datB.y

		flg=datFlag.y

		sins=datin.y
		souts=datout.y

		allimaxs=datFFI.imaxs
		allimins=datFFI.imins
		allomaxs=datFFO.imaxs
		allomins=datFFO.imins

		;get_data,"overshoot",data=datO

		;over=datO.y

		get_data,"regid_cleaned_plot",data=datReg
		reg=datReg.y
		rx=datReg.x
		dr=rx[1]-rx[0]
		print,dr
		print,rx[0]
		print,xs[0]
		print,"rx[0]-xs[0]=",rx[0]-xs[0]
		print,"(rx[0]-xs[0])/dr=",(rx[0]-xs[0])/dr
		ustartini=xs*0.0
		uendini=xs*0.0

		ustartino=xs*0.0
		uendino=xs*0.0
		
		
		usflagi=xs*0.0
		ueflagi=xs*0.0
		usflago=xs*0.0
		ueflago=xs*0.0

		u=fltarr(N,2)

		N=numel(xs)
		
		IG=where(sins ne 0,icount)
		OG=where(souts ne 0,ocount)

		imaxs=allimaxs[IG]
	imins=datFFI.imins[IG]
	omaxs=allomaxs[OG]
	omins=allomins[OG]

		for el=0,icount-1 do begin
			print,"~~~~~~~~~~~~"
			print,"el=",el
			i=IG[el]
			tt=60

			ed=eDD[el]
			dim=DD[el]
			ishock=i
			imax=imaxs[el]
			imin=imins[el]
;			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			;dim=imins[el]			
			if dim gt ishock then dim=0;ed
			if dim gt ishock then dim=imin
			up=inups[i]
			;i=max([i-30,ed,imin,dim])
			;if dim eq 0 then dim=N-1
;			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			;i-=30

			ii=i
			dhalf=mean([dim,i])
			down=indowns[ishock]
			wherefoot=where(footI[imin:ishock] ne 0)

			if wherefoot eq -1 then foot=imin else 	foot=footI[wherefoot[-1]]
			;i=(where(ffd[dim:i] lt .25))[-1]
			imin=max([imin,foot-10*60,i-30*60])
 
			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
;			if dhalf gt i or imin gt dim then dhalf= imin
			if dhalf gt i  then dhalf= imin
			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			;ed=max([imin,dhalf]);dhalf
			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			while footI[ii] eq 0  and ii ge imin do begin
				if ii eq 0 then begin
					ii=i
					break
				endif
				ii--

			endwhile
			iii=ii
			footin=ii
			imin=max([imin,footin-10*60,i-30*60])

			mnslope=min(B60d[imin+30:footin],mnsloc)
			if mnsloc eq 0 and mnslope lt .3/60 and mnslope gt -.1/60 and abs(Bminute[imin+30]-up) le .1 and footin-imin eq 2*60 then begin
						st=imin
						en=imin+60
						print,"bottom of slope case"
						print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
						for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
						endfor			
						usflagi[st]=1
						ueflagi[en]=1	
						continue

			endif
			;;;;;;;Want to check if forecountdown has dropped from a larger value. if so, and if that was more than 60 seconds ago, use range [ii-60,ii]. 
			;;;;;;;If the previous forecountdown was 0 then do the same
			;;;;;;;if i eq ii or last drop was less than 60 seconds ago, do things the old fashioned way

			bound=max([imin,footin-10*60]);max([dim,min([imin,footin-5*60]),dhalf,min([ed,footin-5*60])])
			;ed=bound		
			if i ne ii then begin
				if ii lt bound then ii=i
				if fore[ii] ne foremax[ii] then begin

					while fore[iii] ne foremax[iii] and iii ge ed+60 do iii--
					
				endif
				;ed=bound
				i2=ii
				if (ii-iii gt 60)  or fore[ii] eq foremax[ii] then begin
						if ii gt 30+ed and fore[ii] eq foremax[ii] then begin 
							i2=ii
							while ii ge bound+60 and (mean(B[ii-60:ii]) -up gt 3  or mm[ii]-mm[ii-60] gt 3 or stddev(B[ii-60,ii]) ge 1.5 or ((stddev([B[ii-60],B[ii],mean(B[ii-60:ii])]) lt .5  ) and (max(B[ii-60:ii])-mean(B[ii-60:ii]) gt 2)   )) do ii--;while (ii ge ed+61) and (i2-ii lt 240) do ii--;and ((B[ii]-B[ii-60] ge 1.5) or ((stddev([B[ii-60],B[ii],mean(B[ii-60,ii])]) lt .15  ) and (max(B[ii-60:ii])-mean(B[ii-60,ii]) gt 2)   )) do ii--			
							Bmax=max(B[ii-60:ii],maxuploc)
							if Bmax ge down-1 or total(abs(B[ii:60:ii]-down) lt abs(B[ii-60:ii]-inups[i])) gt 5 then begin
								AB=where(B[ii-60:ii] ge down-1)+ii-60
								if AB[0]-ii+60 lt 57 or total(B[bound:AB[0]-1] ge indowns[i]-1)/(-bound+AB[0]-1) gt .7 then begin
									while(max(B[ii-60:ii],maxuploc) ge down-1 and ii lt footin  and mean(B[ii-60:ii]) -up lt 3 )do ii++
									while(max(B[ii-tt:ii],maxuploc) ge down-1 and tt gt 20)  do tt--
								endif else begin
									while((max(B[ii-60:ii],maxuploc) ge down-1) ) and ii-60 gt bound do ii--

									
								endelse
								if abs(Bminute[imin+30]-up) lt .2 and max(B[imin:imin+60]) lt mean([down,inups[i]]) and abs(b60d[imin+30]) lt .2/60 then begin
									st=imin
									en=imin+60
									print,"spike in region -> imin"
									print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
									for kk=i-10,imax do begin;for kk=imin,imax do begin
		
									ustartini[kk]=st
									uendini[kk]=en
									endfor			
									usflagi[st]=1
									ueflagi[en]=1	
									continue
								endif
							endif
						endif
;						ustartini[ii]=max([ii-60,0])
;						uendini[ii]=ii
						print,"spike in region"
						st=max([ii-tt,0])
						en=ii
						print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
						for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
						endfor			
						usflagi[max([ii-tt,0])]=1
						ueflagi[ii]=1	
						continue
				endif
				if footin lt ed and not ((ii-iii gt 60)  or fore[ii] eq foremax[ii]) then begin

					footloc= max([footin-60,bound])
					bi=footloc+31
					st=footloc+1;max([bi-29,0])
					en=bi+30;-(bi-29+st)
					print,"if footin lt ed and not ((ii-iii gt 60)  or fore[ii] eq foremax[ii])"
					print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					for kk=i-10,imax do begin;for kk=imin,imax do begin
						;ustartini[kk]=st
						;uendini[kk]=en
						ustartini[kk]=st
						uendini[kk]=en
					endfor
				;ustartino[i]=st
				;uendino[i]=en

					usflagi[st]=1
					ueflagi[en]=1
					continue
				endif

			endif	
			;ed=bound


			if el ge 1 then fff=bound else fff=0
			;nfff=i-fff
			nfff=footin-fff
			for j=0,nfff-1 do begin
				;if (flg[i-j] ne 0) or (i-j le bound+60) then break
				if (flg[footin-j] ne 0) or (footin-j le bound+60) then break
			endfor
			print,"[dim,bound,ed,imin,i-j,i,imax]=",[dim,bound,ed,imin,i-j,i,imax]
			if j ge nfff-2 then begin
				;print,"j>nfff-2"
				xii=xs[footin];i]
				LLL=where((reg eq 1) and (rx le xii),lcount)
				;j=i-LLL[lcount-1]
				k=LLL[-1]
				if k eq -1 or rx[k] gt xs[i] then begin
					footloc=imin
					if i ne footin then footloc= max([footin-60,imin])
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc+31
					st=footloc+1;max([bi-29,0])
					en=bi+30;-(bi-29+st)
					print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					for kk=i-10,imax do begin;for kk=imin,imax do begin
						;ustartini[kk]=st
						;uendini[kk]=en
						ustartini[kk]=st
						uendini[kk]=en
					endfor
				;ustartino[i]=st
				;uendino[i]=en

					usflagi[st]=1
					ueflagi[en]=1
					continue
				endif
				xt=abs(rx[k]-xs)
				;j=i-(where( xt eq min(xt)))[0]
				j=footin-(where( xt eq min(xt)))[0]
			endif
			print,"[dim,ed,imin,j,i,imax]=",[dim,ed,imin,j,i,imax]
			;print,"i,j,i-j=",i,j,i-j
			cln=(flg[footin-j]+1)/2;cln=(flg[i-j]+1)/2
			
			xf=xs[footin-j];xf=xs[i-j]
			;ri=where((rx-2*dr le xf) and (rx+2*dr ge xf))
			ri=(where(abs(rx-xf) eq min(abs(rx-xf)))) ; want to verify we're in solar wind
			;print,"ri=",ri
			ri=ri[0]
			while reg[ri] ne 1 and rx[ri] gt xs[bound] do ri--;if we aren't we will shift backwards until we are
			
			rxf=rx[ri]; this is the time value of corresponding to index ri
			
			;bi=imin+29;(where(abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			
			bi=(where(  xs ge xs[imin+29] and  abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			if bi eq -1 then bi=imin+30
			while xs[bi] gt xs[footin] or (((Bp[bi] gt 3) and (Bp[bi-1] lt Bp[bi] )) or B60d[bi+30] gt .018)  and (bi gt bound+29) and (mean(B[ii-60:ii]) -up ge 3 or $
			 (abs(B[ii-60]-up) lt .2  and B[ii]-up gt 3  )  )  do bi--
			;while (Bp[bi] gt 3) and (bi gt 29)  and (Bp[bi-1] lt Bp[bi] ) and (bi gt bound+29) do bi--
			;if cln then begin ; if the solar wind is actually quiet, then just record the index of that location

				;ustartini[imin:imax]=bi
				;uendini[imin:imax]=bi

				;usflagi[bi]=1
				;ueflagi[bi]=1
				st=bi-29
				en=bi+30

			;endif else begin ; if it isn't quiet we'll take a 1 minute interval
				for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
				endfor	
				;ustartini[i]=bi-29

				;uendini[i]=bi+30

				usflagi[bi-29]=1
				ueflagi[bi+30]=1
			;endelse
			print,"base case"
			print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
			print,"[dim,imin,ubeg,uend,i]=",[dim,imin,ustartini[i],uendini[i],i]
		endfor



		print,"OUTBOUNDOUTBOUNDOUTBOUNDOUTBOUNDOUTBOUNDOUTBOUND"

		for el=0,ocount-1 do begin
			print,"~~~~~~~~~~~~~~~~~"
			print,"el=",el
			i=OG[el]
imin=min([omaxs[el],omins[el]])
			imax=max([omaxs[el],omins[el]]);omins[el]
			ed=ebDD[el]
			ishock=i
			dim=bDD[el]
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]
			;i+=30
			ii=i
			if dim eq 0 then dim=N-1
			if ed lt i then ed=imax
			if dim lt i then dim=ed

			dhalf=mean([dim,i])
			;if dhalf lt i or imax+5*60 lt dim then dhalf= imax
			if dhalf lt i then dhalf= imax
			bound=min([imax,dhalf,ed]);dhalf
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]
			if ed eq 0 then ed=dim
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]

			while (footI[ii] eq 0) and dim gt ii  do begin
				if ii eq imax then begin
					ii=i
					break
				endif
				ii++
			endwhile
			iii=ii
			;;;;;;;Want to check if forecountdown has dropped from a larger value. if so, and if that was more than 60 seconds ago, use range [ii-60,ii]. 
			;;;;;;;If the previous forecountdown was 0 then do the same
			;;;;;;;if i eq ii or last drop was less than 60 seconds ago, do things the old fashioned way

			footin=ii
			bound=imax;min([dim,max([imin,footin+5*60]),dhalf,max([ed,footin+5*60]),N-1])	
			;ed=bound


			;imin=max([imin,footin-10*60,i-30*60])
			up=outups[ishock]
			down=outdowns[ishock]
			mnslope=max(B60d[footin:imax],mnsloc)
			if mnsloc eq imax-footin and mnslope gt -.2/60 and mnslope lt .1/60 and abs(Bminute[imax-30]-up) le .1 and imax-footin eq 2*60 then begin
						st=imax-60
						en=imax
						print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
						for kk=imin,i+10 do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
						endfor			
						usflagi[st]=1
						ueflagi[en]=1	
						continue

			endif	
			if i ne ii then begin
				if ii gt bound then ii=i
				if fore[ii] ne foremax[ii] then begin

					while fore[iii] ne foremax[iii] and iii le bound-60 do iii++
					
				endif
				if (iii-ii gt 60)  or fore[ii] eq foremax[ii] then begin
						;print,"[imin,i,ii,imax,ed,dim]=",[imin,i,ii,imax,ed,dim]
					;	ustartini[ii]=ii
					;	uendini[ii]=min([ii+60,N-1])
						if ii  lt ed-60 and fore[ii] eq foremax[ii] then begin 
								i2=ii
								while ii le ed-60 and (stddev(B[ii,ii+60]) ge 1.5 or mm[ii]-mm[ii+60] gt 3 or ((stddev([B[ii-60],B[ii],mean(B[ii-60:ii])]) lt .5  ) and (max(B[ii-60:ii])-mean(B[ii-60:ii]) gt 2)   )) do ii++;while (ii le ed-61) do ii++;and (ii-i2 lt 240) and ((B[ii]-B[ii+60] ge 1.5) or ((stddev([B[ii],B[ii+60],mean(B[ii,ii+60])]) lt .15  ) and (max(B[ii:ii+60])-mean(B[ii,ii+60]) gt 1)   )) do ii++;while ii le ed-60 and stddev(B[ii,ii+60]) ge 1.5 do ii++
								Bmax=max(B[ii:ii+60],maxuploc)
								if Bmax ge down-1 or total(abs(B[ii:ii+60]-down) lt abs(B[ii:ii+60]-ups)) gt 5 then begin
								;if max(B[ii:ii+60],maxuploc) ge outdowns[i]-1 then begin
									AB=where(B[ii:ii+60] ge down-1)+ii
									if -AB[-1]+ii-60 lt 57 or total(B[AB[-1]+1:bound] ge down-1)/(-AB[-1]-1+bound) gt .7 then begin
										 while(max(B[ii:ii+60],maxuploc) ge down-1 and ii gt footin) do ii--
																				 

										 while(max(B[ii:ii+tt],maxuploc) ge down-1 and tt gt 20) do tt--
									endif else begin
										while(max(B[ii:ii+60],maxuploc) ge down-1 and ii+60 lt bound) do ii++
									endelse

									if abs(Bminute[imax-30]-up) lt .2 and max(B[imax-60:imax]) lt mean([down,up]) and abs(b60d[imax-30]) lt .2 then begin
									st=imax-60
									en=imax
									print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
									for kk=imin,i+10 do begin;for kk=imin,imax do begin
		
									ustartini[kk]=st
									uendini[kk]=en
									endfor			
									usflagi[st]=1
									ueflagi[en]=1	
									continue
								endif

								endif
						endif
					for kk=imin,i+10 do begin
	;					ustartini[kk]=ii
	;					uendini[kk]=min([ii+60,N-1])
						ustartino[kk]=ii
						uendino[kk]=min([ii+60,N-1])
					endfor
						usflago[ii]=1
						ueflago[min([ii+60,N-1])]=1	
						continue
				endif	
				if footin gt ed then begin
					footloc=imax
					if i ne footin then footloc= min([footin+60,bound])
					tt=30 
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc-30;imax-30
					en=min([bi+29,bound])
					st=bi-30-(bi+29-en)
					;print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					for kk=imin,i+10 do begin;for kk=imin,dhalf-2 do begin
						;ustartini[kk]=st
						;uendini[kk]=en
						ustartino[kk]=st
						uendino[kk]=en
					endfor
				;ustartino[i]=st
				;uendino[i]=en

					usflago[st]=1
					ueflago[en]=1
					continue

				endif

			endif
			;i+=30

			fff=bound;ed;if el lt ocount-1 then fff=OG[el+1] else fff=N-1
			;print,"[imin,i,fff,imax,ed,dim]=",[imin,i,fff,imax,ed,dim]
			
			for j=footin,fff-1 do begin
				if flg[j] ne 0 then break
			endfor
			;print,"[imin,i,j,fff,imax,ed,dim]=",[imin,i,j,fff,imax,ed,dim]
			if j ge fff-3 then begin
				print,"j>fff-3"
				xii=xs[i]
				LLL=where((reg eq 1) and (rx ge xii))
				k=LLL[0]
				if k eq -1 or rx[k] lt xs[i] then begin
					footloc=imax
					if i ne footin then footloc= min([footin+30,bound])
					tt=30 
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc-30;imax-30
					en=min([bi+29,N-1])
					st=bi-30-(bi+29-en)
					;print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					for kk=imin,i+10 do begin;for kk=imin,dhalf-2 do begin
						;ustartini[kk]=st
						;uendini[kk]=en
						ustartino[kk]=st
						uendino[kk]=en
					endfor
				;ustartino[i]=st
				;uendino[i]=en

					usflago[st]=1
					ueflago[en]=1
					continue
				endif
				xt=abs(rx[k]-xs)
				j=(where( xt eq min(xt)))[0]

			endif
			cln=(flg[j]+1)/2
			;print,"[imin,i,j,fff,imax,ed,dim]=",[imin,i,j,fff,imax,ed,dim]
			xf=xs[j]
			
			ri=(where(abs(rx-xf) eq min(abs(rx-xf))))[0] ; want to verify we're in solar wind
			while reg[ri] ne 1 and rx[ri] lt xs[ed] do ri++;if we aren't we will shift backwards until we are
			
			rxf=rx[ri]; this is the time value of corresponding to index ri
			
			bi=(where(xs lt xs[imax] and   abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			if bi eq -1 then bi=imax-29	
			while xs[bi] lt xs[footin] or (((Bp[bi] gt 3) and (bi lt numel(Bp)-30) and (Bp[bi+1] lt Bp[bi] )) or B60d[bi-30] lt -.02 ) and bi lt bound-30  and ~ (B[bi+30] ge outdowns[ishock]) do bi++
			;if cln  then begin ; if the solar wind is actually quiet, then just record the index of that location

				;ustartino[imin:imax]=bi
				;uendino[imin:imax]=bi

				;usflago[bi]=1
				;ueflago[bi]=1
		
			;endif else begin ; if it isn't quiet we'll take a 1 minute interval

				en=min([bi+29,bound])
				st=bi-30-(bi+29-en)
				;print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
				for kk=imin,i+10 do begin;for kk=imin,dhalf do begin
						ustartino[kk]=st
						uendino[kk]=en
				endfor
				;ustartino[i]=st
				;uendino[i]=en

				usflago[st]=1
				ueflago[en]=1

			;endelse
		
			print,"[i,ubeg,uend]=",[i,ustartino[i],uendino[i]]
		endfor


		ui=[[ustartini],[uendini]]
		uo=[[ustartino],[uendino]]

		uif=[[usflagi],[ueflagi]]
		uof=[[usflago],[ueflago]]

		u=ui+uo
		uf=uif+uof

		store_data,"upstream_indices_inbound",data={x:xs,y:ui}
		store_data,"upstream_indices_outbound",data={x:xs,y:uo}
		store_data,"upstream_indices",data={x:xs,y:u}

		store_data,"upstream_flags_inbound",data={x:xs,y:uif}
		store_data,"upstream_flags_outbound",data={x:xs,y:uof}
		store_data,"upstream_flags",data={x:xs,y:uf}
		options,"upstream_flags",'colors',['c','y']
		print,u[1,*]
		get_data,"upstream_indices",data=dat
		print,dat.y[2,*]

end
