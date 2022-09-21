;+
;CHANGELOG:
;2022-03-09:
;	fixed bug (seems to not have ever triggered) where outbound saves to usflagi,ueflagi,ustartini,uendini instead of outbound versions in a few places
;	bound wasn't defined the same in outbound. Fixed this
;	efoot=footin -> efoot=foot
;  	added features to find the interval via minima in "Bmmstd_cycloperiod"
; 	redefined tt from 
;			tt=max([60,period])
;
;			tt=period
;			while round(tt) le 60 do tt+=period
;-

pro upstreamindexer

		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout
		get_data,"proton_cyclotron_period",data=datper

		cycloperiod=datper.y

		if total(datin.y + datout.y) eq 0 then return
		xs=datin.x
		N=numel(xs)
		get_data,'wind_cleanmean_interpolated',data=datFlag

		get_data,"mvn_swe_spec_dens_interpolated",data=datne
		N_e=datne.y

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

		MMs=datFF.MMs

		widths=1/MMs[*,1]
		nwidths=reverse(widths)

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
		get_data,"Bmmstd_cycloperiod",data=datmstd
		mstd=datmstd.y

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
		footO=datFootO.y
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
		GGG=where(sins+souts ne 0)
		IG=where(sins ne 0,icount)
		OG=where(souts ne 0,ocount)

		imaxs=allimaxs[IG]
		imins=datFFI.imins[IG]
		omaxs=allomaxs[OG]
		omins=allomins[OG]

		
		for i=0,icount-1 do print,"[i,imins[i],ishocks[i],imaxs[i]]=",[i,imins[i],IG[i],imaxs[i]]
		for i=0,icount-1 do print,"[i,imint,ishockt,imaxt]=",i,", ",time_string(xs[imins[i]]),",",time_string(xs[IG[i]]),",",time_string(xs[imaxs[i]])
		for el=0,icount-1 do begin
			i=IG[el]
			imax=imaxs[el]
			wid=widths[i]
			var=dirupstreamindexer(xs,B,el,IG,cycloperiod,DD,eDD,imins,imaxs,N_e,indowns,inups,footI,B60d,mstd,Bminute,fore,ymm,flg,reg,rx,Bp,foremax,wid)

			st=var[0]
			en=var[1]
	
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
			
			print,"~~~~~~~~~~~~"
			print,"el=",el
			i=IG[el]
			period=cycloperiod[i]
			print,time_string(xs[i])
			;tt=max([60,period])

			tt=period
			while round(tt) le 60 do tt+=period

			ishock=i
			localdim=(where(DD lt i))[-1]
			print,localdim
			print,bDD
			print,i
			print,time_string(xs[i])
			if localdim ne -1 then dim=DD[localdim] else dim=0
			;dim=bDD[el]
			localed=(where(eDD lt i))[-1]
			if localed ne -1 then ed=eDD[localed] else ed=dim
	;		ed=eDD[el]
	;		dim=DD[el]
			ishock=i
			imax=imaxs[el]
			imin=imins[el]
			print,time_string(xs[imin])
			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			;dim=imins[el]			
			if dim gt i then dim=0;ed
			if dim gt i then dim=imin

			;i=max([i-tt/2,ed,imin,dim])
			;if dim eq 0 then dim=N-1
;			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			i-=30
			;i=(where(ffd[dim:i] lt .25))[-1]
			ii=i
			dhalf=mean([dim,ishock])
			down=indowns[ishock]
			up=inups[ishock]
			wherefoot=(where(footI[imin+1:ishock-1] gt 0))[-1]
			print,"wherefoot=",wherefoot
			if wherefoot eq -1 then foot=ishock-tt/2 else 	foot=wherefoot+imin+1;footI[wherefoot]+imin+1
			;imin=max([imin,foot-10*60,i-tt/2*60])
 
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
;			if dhalf gt i or imin gt dim then dhalf= imin
			if dhalf gt ishock  then dhalf= imin
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
			;ed=max([imin,dhalf]);dhalf
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
			while footI[ii] eq 0  and ii gt imin do begin
				if ii eq 0 then begin
					ii=i
					break
				endif
				ii--

			endwhile
			iii=ii
			footin=ii
			
			print,imin,foot,footin
			bound=max([imin,footin-10*60,0]);max([dim,min([imin,footin-5*60]),dhalf,min([ed,footin-5*60])])

			efoot=foot
			while max(B[max([efoot-tt/2,0]):efoot]) ge down and efoot-tt gt bound do efoot--

			mnslope=min(B60d[imin:foot],mnsloc)
			if imin ne foot and mnsloc eq 0 and mnslope lt .3/tt and mnslope gt -.1/tt and abs(Bminute[imin+tt/2]-up) le .1 and footin-imin eq 2*tt and total(finite(N_e[imin:imin+tt]) eq 1) gt 10 then begin
						st=imin
						en=imin+tt
						print,"bottom of slope case"
						print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
						for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
						endfor			
						usflagi[st]=1
						ueflagi[en]=1	
						continue

			endif
			;imin=max([imin,footin-10*60,ishock-tt/2*60])
			;;;;;;;Want to check if forecountdown has dropped from a larger value. if so, and if that was more than 60 seconds ago, use range [ii-tt,ii]. 
			;;;;;;;If the previous forecountdown was 0 then do the same
			;;;;;;;if i eq ii or last drop was less than 60 seconds ago, do things the old fashioned way

			
			bound=max([imin,foot-10*60,0])
			wherefin=where(finite(N_e[bound:efoot]) eq 1,nfin)+bound
			if nfin ge tt then begin
				bound=wherefin[0]
				 

			endif
		;bound=max([imin,footin-10*60,0]);max([dim,min([imin,footin-5*60]),dhalf,min([ed,footin-5*60])])
			strt=max([0,bound-tt/2])
			ofst=bound-strt
			stdp=(smooth(mstd[strt:ishock+tt],tt))[ofst:ishock-strt]
			stpp=stdp[0:efoot-bound-tt/2]		
			mnstd=min(stpp,mnstdlocp)
			meanstd=mean(stpp)
			mnstdloc=mnstdlocp+bound
			stdthresh=(mnstd+meanstd)/2
			if ymm[mnstdloc] le .75*up+.25*down and Bminute[mnstdloc] le .75*up+.25*down and stdp[mnstdlocp+tt/2] le meanstd then begin

					st=mnstdloc-tt/2;max([bi-tt/2-1,0])
					en=mnstdloc+tt/2;-(bi-tt/2-1+st)
					PRINT,"tt=",tt
					print,"efoot=",efoot
					print,"[imin,i,st,mnstdloc,en,efoot,imax,ed,dim]=",[imin,i,st,mnstdloc,en,efoot,imax,ed,dim]
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
			;ed=bound		
			if i ne ii then begin
				if ii lt bound then ii=i
				if fore[ii] ne foremax[ii] then begin

					while fore[iii] ne foremax[iii] and iii ge ed+tt do iii--
					
				endif
				;ed=bound
				i2=ii
				print,time_string(xs[footin])
				print,time_string(xs[foot])
				print,"numel(B),bound,ii-tt,ii,efoot,foot=",numel(B),bound,ii-tt,ii,efoot,foot
				if (ii-iii gt tt)  or fore[ii] eq foremax[ii] then begin
						if ii gt tt/2.+bound and fore[ii] eq foremax[ii] then begin 
							i2=ii

							wquietlocp=where(ymm[bound:ii] lt .75*up+.25*down and stdp[0:ii-bound] lt stdthresh)
							
							if wquietlocp[0] ne -1 then begin
								if numel( intersect(wquietlocp+bound,wherefin)) gt 10 then wquietlocp=intersect(wquietlocp+bound,wherefin)
								quietlocp=(where(stdp[wquietlocp] eq min(stdp[wquietlocp])))[0]
								quietloc=quietlocp+bound
								if total(quietlocp+tt/2 eq wquietlocp) ne 0 then begin
									st=quietloc-tt/2;max([bi-tt/2-1,0])
									en=quietloc+tt/2;-(bi-tt/2-1+st)
									print,"[imin,i,bi,st,quietloc,en,imax,ed,dim]=",[imin,i,bi,st,quietloc,en,imax,ed,dim]
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
							while ii gt bound+tt+2 and ii le foot and ii-tt gt 0 and ii ne -1 and bound lt ii-1 and (max(B[ii-tt:ii]) ge down or  ((max(B[ii-tt:ii]) gt max([down-1,.8*down+.2*up])) and (total(B[bound:ii-1] gt max([down-1,.8*down+.2*up])) lt .4*(ii-1-bound) )) or   (mean(B[ii-tt:ii]) -up gt 3  or ymm[ii]-ymm[ii-tt] gt 3 or stddev(B[ii-tt:ii]) ge 1.5   or ((stddev([B[ii-tt],B[ii],mean(B[ii-tt:ii])]) lt .5  ) and (max(B[ii-tt:ii])-mean(B[ii-tt:ii]) gt 2)   ))) do ii--;while (ii ge ed+61) and (i2-ii lt 240) do ii--;and ((B[ii]-B[ii-tt] ge 1.5) or ((stddev([B[ii-tt],B[ii],mean(B[ii-tt,ii])]) lt .15  ) and (max(B[ii-tt:ii])-mean(B[ii-tt,ii]) gt 2)   )) do ii--
							print,"numel(B),ii-tt,ii,bound=",numel(B),ii-tt,ii,bound
							print,B[ii]
							print,B[ii-tt]
							Bmax=max(B[ii-tt:ii],maxuploc)
							BII=B[ii-tt:ii]
							print,numel(BII)
							print,numel(down)
							BIId=BII-down[0]
							BIIu=BII-up[0]
							if Bmax ge max([down-1,.8*down+.2*up]) or total(0 lt -abs(BIId) + abs(BIIu)) gt 5 then begin
								AB=where(B[ii-tt:ii] ge max([down-1,.8*down+.2*up]),abcount)+ii-tt
								if abcount gt 0 and (AB[0]-ii+tt lt tt-3  or  total(B[bound:max([AB[0]-1,bound+tt])] ge indowns[i]-1)/(-bound+AB[0]-1) gt .7) then begin
									while(max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]) and  ii lt efoot  and median(B[ii-tt:ii]) -up lt 3 )do ii++;footin  and median(B[ii-tt:ii]) -up lt 3 )do ii++
				print,"while 1 numel(B),ii-tt,ii,efoot,foot,bound=",numel(B),ii-tt,ii,efoot,foot,bound
									while(max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]) and tt gt 20)  do tt--
				print,"while 2 numel(B),ii-tt,ii,efoot,foot,bound=",numel(B),ii-tt,ii,efoot,foot,bound
								endif else begin
									print,"else begin numel(B),ii,efoot,foot,bound=",numel(B),ii,efoot,foot,bound
									while (max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]))  and ii-tt gt bound do ii--

									
								endelse
								print," endelse numel(B),ii,efoot,foot,bound=",numel(B),ii,efoot,foot,bound
								if abs(Bminute[imin+tt/2]-up) lt .2 and max(B[imin:imin+tt]) lt mean([down,inups[i]]) and abs(b60d[imin+tt/2]) lt .2/tt then begin
									st=bound
									en=bound+tt
									print,"spike in region -> imin"
									print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
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
;						ustartini[ii]=max([ii-tt,0])
;						uendini[ii]=ii
						print,"spike in region"
						st=max([ii-tt,0])
						en=ii
						print,"[imin,i,st,en,efoot,foot,imax,ed,dim]=",[imin,i,st,en,efoot,foot,imax,ed,dim]
						for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
						endfor			
						usflagi[max([ii-tt,0])]=1
						ueflagi[ii]=1	
						continue
				endif
				if footin lt ed and not ((ii-iii gt tt)  or fore[ii] eq foremax[ii]) then begin

					footloc=bound; max([footin-tt/2,bound])
					bi=footloc+tt/2 +1
					st=footloc+1;max([bi-tt/2-1,0])
					en=bi+tt/2;-(bi-tt/2-1+st)
					print,"if footin lt ed and not ((ii-iii gt tt)  or fore[ii] eq foremax[ii])"
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
				;if (flg[i-j] ne 0) or (i-j le bound+tt) then break
				if (flg[footin-j] ne 0) or (footin-j le bound+tt) then break
			endfor
			print,"[dim,bound,ed,imin,footin,i-j,i,imax]=",[dim,bound,ed,footin,imin,i-j,i,imax]
			if j ge nfff-2 then begin
				;print,"j>nfff-2"
				xii=xs[footin];i]
				LLL=where((reg eq 1) and (rx le xii),lcount)
				;j=i-LLL[lcount-1]
				k=LLL[-1]
				if k eq -1 or rx[k] gt xs[i] then begin
					;footloc=imin
					;if i ne footin then footloc=imin; max([footin-tt/2,imin])
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc+tt/2+1
					st=bound;footloc+1;max([bi-tt/2-1,0])
					en=bound+tt;bi+tt/2;-(bi-tt/2-1+st)
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
			
			;bi=imin+tt/2-1;(where(abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			
			bi=(where(  xs ge xs[bound+tt/2-1] and  abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			if bi eq -1 then bi=bound+tt/2
			while (xs[bi+tt/2] ge xs[footin] or stdp[bi-bound] ge stdthresh or  ((((Bp[bi] gt 3) and (Bp[bi-1] lt Bp[bi] )) or B60d[bi+tt/2] gt .018)   and (mean(B[ii-tt:ii]) -up ge 3 or $
			 (abs(B[ii-tt]-up) lt .2  and B[ii]-up gt 3  )  ))) and bi gt bound+tt/2-1  and ~ (B[bi-tt/2] ge down)   do bi--
			;while (Bp[bi] gt 3) and (bi gt tt/2-1)  and (Bp[bi-1] lt Bp[bi] ) and (bi gt bound+tt/2-1) do bi--
			;if cln then begin ; if the solar wind is actually quiet, then just record the index of that location
			print,bi+tt/2,footin,xs[bi+tt/2],xs[footin]
			print,xs[bi+tt/2]-xs[imin],xs[footin]-xs[imin]
				;ustartini[imin:imax]=bi
				;uendini[imin:imax]=bi

				;usflagi[bi]=1
				;ueflagi[bi]=1
				st=bi-tt/2-1
				en=bi+tt/2

			;endif else begin ; if it isn't quiet we'll take a 1 minute interval
				for kk=i-10,imax do begin;for kk=imin,imax do begin
		
							ustartini[kk]=st
							uendini[kk]=en
				endfor	
				;ustartini[i]=bi-tt/2-1

				;uendini[i]=bi+tt/2

				usflagi[bi-tt/2-1]=1
				ueflagi[bi+tt/2]=1
			;endelse
			print,"base case"
			print,"[dim,ed,imin,foot,i,bi,st,en,imax,ed,dim]=",[ed,dim,imin,footin,i,bi,st,en,imax]
			print,"[dim,imin,ubeg,uend,foot,i]=",[dim,imin,ustartini[i],uendini[i],footin,i]
		endfor
	NOG=where(reverse(souts) ne 0,ocount)
	allomaxs=REVERSE(datFFO.imaxs)
	allomins=REVERSE(datFFO.imins)
	allomaxs0=datFFO.imaxs
	allomins0=datFFO.imins

	nomins=N-1-allomaxs[NOG]
	nomaxs=N-1-allomins[NOG]
	nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
	enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn);defines the boundaries in  forward direction. beginning  of negative shock
	noutdowns=reverse(outdowns)
	noutups=reverse(outups)
	nBB=reverse(B)
	nB60d=reverse(B60d)
	nBminute=reverse(Bminute)
	nfore=reverse(fore)
	ncycloperiod=reverse(cycloperiod)
	nFootO=reverse(footO)
	nmstd=reverse(mstd)
	nflg=reverse(flg)
	nymm=reverse(ymm)
	nBp=reverse(Bp)
	nreg=reverse(reg)
	nN_e=reverse(N_e)
	nforemax=reverse(foremax)
		print,"OUTBOUNDOUTBOUNDOUTBOUNDOUTBOUNDOUTBOUNDOUTBOUND"

		for el=0,ocount-1 do begin
			i=NOG[el]
			nimax=nomaxs[el]
			wid=nwidths[i]
			var=dirupstreamindexer(xs,nBB,el,NOG,ncycloperiod,nDD,enDD,nomins,nomaxs,N_e,noutdowns,noutups,nfootO,nB60d,nmstd,nBminute,nfore,nymm,nflg,nreg,rx,nBp,nforemax,wid)

			st=var[0]
			en=var[1]
			for kk=i-10,nimax do begin;for kk=imin,imax do begin
						;ustartini[kk]=st
						;uendini[kk]=en
						ustartino[kk]=N-1-en
						uendino[kk]=N-1-st
					endfor
				;ustartino[i]=st
				;uendino[i]=en

					usflago[N-1-en]=1
					ueflago[N-1-st]=1
					continue
			print,"~~~~~~~~~~~~~~~~~"
			print,"el=",el
			i=OG[el]
imin=min([omaxs[el],omins[el]])
			imax=max([omaxs[el],omins[el]]);omins[el]

			;ed=ebDD[el]
			ishock=i
			localdim=(where(bDD gt i))[0]
			print,localdim
			print,bDD
			print,i
			if localdim ne -1 then dim=bDD[localdim] else dim=N-1
			;dim=bDD[el]
			localed=(where(ebDD gt i))[0]
			if localed ne -1 then ed=ebDD[localed] else ed=dim
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]
			;i+=30

			period=cycloperiod[i]
			tt=period
			while round(tt) le 60 do tt+=period
			;tt=max([60,period])
			ii=i
			ishock=i
			if dim eq 0 then dim=N-1
			;if ed lt i then ed=imax
			;if dim lt i then dim=ed

			dhalf=mean([dim,i])
			;if dhalf lt i or imax+5*60 lt dim then dhalf= imax
			if dhalf lt i then dhalf= imax
			bound=min([imax,dhalf,ed]);dhalf
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]
			;if ed eq 0 then ed=dim
			;print,"[imin,i,imax,ed,dim]=",[imin,i,imax,ed,dim]
			wherefoot=(where(footO[ishock-1:imax] gt 0))[0]
			print,"wherefoot=",wherefoot
			if wherefoot eq -1 then foot=ishock+tt/2 else 	foot=wherefoot+ishock+1;footI[wherefoot]+imin+1
			while (footO[ii] eq 0) and dim gt ii  do begin
				if ii eq imax then begin
					ii=i
					break
				endif
				ii++
			endwhile
			iii=ii
			;;;;;;;Want to check if forecountdown has dropped from a larger value. if so, and if that was more than 60 seconds ago, use range [ii-tt,ii]. 
			;;;;;;;If the previous forecountdown was 0 then do the same
			;;;;;;;if i eq ii or last drop was less than 60 seconds ago, do things the old fashioned way

			footin=ii

			efoot=foot;in

			up=outups[ishock]
			down=outdowns[ishock]

			while max(B[efoot:min([efoot+tt/2,N-1])]) ge down and efoot+2*tt lt imax do efoot++

			bound=imax;min([dim,max([imin,footin+5*60]),dhalf,max([ed,footin+5*60]),N-1])	
			;ed=bound


			;imin=max([imin,footin-10*60,i-tt/2*60])
			bound=min([imax,foot+10*60,N-1])

			wherefin=where(finite(N_e[efoot:bound]) eq 1,nfin)+efoot
			if nfin ge tt then begin
				bound=wherefin[-1]
			endif

			fnl=min([N-1,bound+tt/2])
			ofst=bound-fnl
			stdp=(smooth(mstd[ishock-tt:fnl],tt))[tt:bound-ishock+tt]
			stpp=stdp[efoot-ishock:*]		
			mnstd=min(stpp,mnstdlocp)
			meanstd=mean(stpp)
			mnstdloc=mnstdlocp+efoot
			stdthresh=(mnstd+meanstd)/2
			if ymm[mnstdloc] le .75*up+.25*down and Bminute[mnstdloc] le .75*up+.25*down and stdp[mnstdlocp-tt/2] le meanstd then begin

					st=mnstdloc-tt/2;max([bi-tt/2-1,0])
					en=mnstdloc+tt/2;-(bi-tt/2-1+st)
					print,"[imin,i,st,mnstdloc,en,imax,ed,dim]=",[imin,i,st,mnstdloc,en,imax,ed,dim]
					for kk=imin,i+10 do begin;for kk=imin,imax do begin
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
			mnslope=max(B60d[footin:imax],mnsloc)
			if mnsloc eq imax-footin and mnslope gt -.2/tt and mnslope lt .1/tt and abs(Bminute[imax-tt/2]-up) le .1 and imax-footin eq 2*tt and total(finite(N_e[imax-tt:imax]) eq 1) gt 10 then begin
						st=imax-tt
						en=imax
						print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
						for kk=imin,i+10 do begin;for kk=imin,imax do begin
		
							ustartino[kk]=st
							uendino[kk]=en
						endfor			
						usflago[st]=1
						ueflago[en]=1	
						continue

			endif	
			
			if i ne ii then begin
				if ii gt bound then ii=i
				if fore[ii] ne foremax[ii] then begin

					while fore[iii] ne foremax[iii] and iii le bound-tt do iii++
					
				endif
				if (iii-ii gt tt)  or fore[ii] eq foremax[ii] then begin
						;print,"[imin,i,ii,imax,ed,dim]=",[imin,i,ii,imax,ed,dim]
					;	ustartini[ii]=ii
					;	uendini[ii]=min([ii+tt,N-1])
						if ii  lt ed-tt/2 and fore[ii] eq foremax[ii] then begin 
							wquietlocp=where(ymm[ii:bound] lt .75*up+.25*down and stdp[ii-ishock:*] lt stdthresh)
							
							if wquietlocp[0] ne -1 then begin
								if numel( intersect(wquietlocp+ii,wherefin)) gt 10 then wquietlocp=intersect(wquietlocp+ii,wherefin)
								quietlocp=(where(stdp[wquietlocp] eq min(stdp[wquietlocp])))[0]
								quietloc=quietlocp+ishock
								if total(quietlocp-tt/2 eq wquietlocp) ne 0 then begin
									st=quietloc-tt/2;max([bi-tt/2-1,0])
									en=quietloc+tt/2;-(bi-tt/2-1+st)
									print,"[imin,i,bi,st,quietloc,en,imax,ed,dim]=",[imin,i,bi,st,quietloc,en,imax,ed,dim]
									for kk=imin,i+10 do begin;for kk=imin,imax do begin
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
								i2=ii
								while ii lt bound-tt and (max(B[ii:ii+tt]) ge down or  ((max(B[ii:ii+tt]) gt max([down-1,.8*down+.2*up])) and (total(B[ii+1:bound] gt max([down-1,.8*down+.2*up])) lt .4*(bound-ii+1) )) or    stddev(B[ii:ii+tt]) ge 1.5 or ymm[ii]-ymm[ii+tt] gt 3 or ((stddev([B[ii+tt],B[ii],mean(B[ii:ii+tt])]) lt .5  ) and (max(B[ii:ii+tt])-mean(B[ii:ii+tt]) gt 2)   )) do ii++;while (ii le ed-61) do ii++;and (ii-i2 lt 240) and ((B[ii]-B[ii+tt] ge 1.5) or ((stddev([B[ii],B[ii+tt],mean(B[ii,ii+tt])]) lt .15  ) and (max(B[ii:ii+tt])-mean(B[ii,ii+tt]) gt 1)   )) do ii++;while ii le ed-tt and stddev(B[ii,ii+tt]) ge 1.5 do ii++
								Bmax=max(B[ii:ii+tt],maxuploc)
								if Bmax ge max([down-1,.8*down+.2*up]) or total(abs(B[ii:ii+tt]-down) lt abs(B[ii:ii+tt]-up)) gt 5 then begin
								;if max(B[ii:ii+tt],maxuploc) ge outdowns[i]-1 then begin
									AB=where(B[ii:ii+tt] ge max([down-1,.8*down+.2*up]))+ii
									if -AB[-1]+ii-tt lt tt-3 or total(B[AB[-1]:bound] ge max([down-1,.8*down+.2*up]))/(-AB[-1]-1+bound) gt .7 then begin
										 while(max(B[ii:ii+tt],maxuploc) ge max([down-1,.8*down+.2*up]) and ii gt efoot) do ii--
																				 

										 while(max(B[ii:ii+tt],maxuploc) ge max([down-1,.8*down+.2*up]) and tt gt 20) do tt--
									endif else begin
										while(max(B[ii:ii+tt],maxuploc) ge max([down-1,.8*down+.2*up]) and ii+tt lt bound) do ii++
									endelse

									if abs(Bminute[imax-tt/2]-up) lt .2 and max(B[imax-tt:imax]) lt mean([down,up]) and abs(b60d[imax-tt/2]) lt .2 then begin
									st=bound-tt
									en=bound
									print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
									for kk=imin,i+10 do begin;for kk=imin,imax do begin
		
									ustartino[kk]=st
									uendino[kk]=en
									endfor			
									usflago[st]=1
									ueflago[en]=1	
									continue
								endif

								endif
						endif
					st=ii
					en=min([ii+tt,N-1])
					for kk=imin,i+10 do begin
	;					ustartini[kk]=ii
	;					uendini[kk]=min([ii+tt,N-1])
						ustartino[kk]=st
						uendino[kk]=en
					endfor
						usflago[st]=1
						ueflago[en]=1	
					print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
						continue
				endif	
				if footin gt ed then begin
					footloc=bound;imax
					;if i ne footin then footloc= min([mnstdloc+tt/2,bound])
					;tt=30 
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc-tt/2;imax-tt/2
					en=min([bi+tt/2-1,bound])
					st=bi-tt/2-(bi+tt/2-1-en)
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
					print,"[imin,i,st,en,imax,ed,dim]=",[imin,i,st,en,imax,ed,dim]
					continue

				endif

			endif
			;i+=30

			fff=bound;ed;if el lt ocount-1 then fff=OG[el+1] else fff=N-1
			;print,"[imin,i,fff,imax,ed,dim]=",[imin,i,fff,imax,ed,dim]
			
			for j=footin,fff-1 do begin
				if (flg[j] ne 0) or (j ge bound-tt) then break
				;if flg[j] ne 0 then break
			endfor
			;print,"[imin,i,j,fff,imax,ed,dim]=",[imin,i,j,fff,imax,ed,dim]
			if j ge fff-3 then begin
				print,"j>fff-3"
				xii=xs[i]
				LLL=where((reg eq 1) and (rx ge xii))
				k=LLL[0]
				if k eq -1 or rx[k] lt xs[i] then begin
					footloc=bound;imax
					;if i ne footin then footloc= min([mnstdloc+tt/2,bound])
					;tt=30 
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					bi=footloc-tt/2;imax-tt/2
					en=min([bi+tt/2-1,N-1])
					st=bi-tt/2-(bi+tt/2-1-en)
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
			
			bi=(where(xs lt xs[bound] and   abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			if bi eq -1 then bi=bound-tt/2-1	
			while (xs[bi-tt/2] lt xs[footin] or stdp[bi-ishock] ge stdthresh or (((Bp[bi] gt 3) and (bi lt numel(Bp)-tt/2) and (Bp[bi+1] lt Bp[bi] )) or B60d[bi-tt/2] lt -.02 )) and bi lt bound-tt/2  and ~ (B[bi+tt/2] ge outdowns[ishock]) do bi++
			;if cln  then begin ; if the solar wind is actually quiet, then just record the index of that location

				;ustartino[imin:imax]=bi
				;uendino[imin:imax]=bi

				;usflago[bi]=1
				;ueflago[bi]=1
		
			;endif else begin ; if it isn't quiet we'll take a 1 minute interval

				en=min([bi+tt/2-1,bound])
				st=bi-tt/2-(bi+tt/2-1-en)
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
		ustartino=reverse(ustartino)
		uendino=reverse(uendino)
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
		return
		;help,u
		for el=0,numel(GGG)-1 do begin
			i=GGG[el]
			ouru=u[i,*]
			help,ouru
			print,"start:",ouru[0],",",time_string(xs[ouru[0]])
			print,"end:",ouru[1],",",time_string(xs[ouru[1]])
		endfor
end
