;+
; overshooter changelog:
; added 'endunder' variable, defining where the undershoot ends. From now on, while overshoot ends at minloc, we start the downstream interval a bit later, starting at 'endunder'
; ->Also Call  	get_data,"undershoot_end_loc_inbound",data=datuei
;       	get_data,"undershoot_end_loc_outbound",data=datueo
; ->if mean(B[st:en]) -down gt 4 then begin changed to recalculate minloc,endunder using findnextovershootmin

;-


function findoverend,ys,mm,ups,downs,ishock,endloc,startover,wid
	dmd=mm-downs
	umu=mm-ups
	up=ups[ishock]
	wd=wid*!pi/2
	down=downs[ishock]
	over=1*(dmd[ishock] gt 0)
	if over then startover=ishock


	fnl=min([max([endloc-120,.5*(endloc-ishock)+ishock]),.75*(endloc-ishock)+ishock]);fnl=max([endloc-4*60,.75*(endloc-i)+i]);fnl=min([max([endloc-120,ishock]),.75*(endloc-ishock)+ishock])
	HH=where(dmd[ishock:fnl] gt 0,hcount,COMPLEMENT=nHH)
	HH+=ishock
	nHH+=ishock
	startover=HH[0]
	GG=where(abs(umu[startover:fnl])/up lt abs(dmd[startover:fnl])/down)+startover
	for i=ishock, fnl-1 do begin
		if max(ys[ishock:i]) lt down  then continue
 		if ~over and dmd[i] gt 0 then begin
			startover=i
			over=1
			continue
		endif

		if over and total(dmd[i:i+3] le 0) ge 3 and max(ys[ishock:i]) gt down and i-ishock gt wd*.9 then begin
			over=0
			mn=min([fnl,i+120])
			if total(abs(umu[i:mn])/up lt abs(dmd[i:mn])/down) gt 60 and total(HH gt  mn) gt 0 and total(nHH gt mn) gt 0 then continue
			return,i
		endif
	endfor
	return,i

end


pro overshooter, insourceSub=insourceSub,outsourceSub=outsourceSub, width=width
	print,"OVERSHOOTER OVERSHOOTER OVERSHOOTER OVERSHOOTER OVERSHOOTER"
	print,"OVERSHOOTER OVERSHOOTER OVERSHOOTER OVERSHOOTER OVERSHOOTER"

get_data,"shocks_inbound",data=datsin
	get_data,"shocks_outbound",data=datsout

	ins=datsin.y
	outs=datsout.y
	if total(datsin.y + datsout.y) eq 0 then return
	routs=reverse(outs)


	SS=where(ins ne 0,sicount)
	nSS=where(routs ne 0,socount)

	if not keyword_set(width) then width = 30
	if not keyword_set(insourceSub) then insourceSub='sublengths_inbound_end'

	if not keyword_set(outsourceSub) then outsourceSub='sublengths_outbound_end'


	tplotmultiplier, 'mvn_B_1sec_MAVEN_MSO_Mag',insourceSub,'B_sheath_in'
	tplotmultiplier, 'mvn_B_1sec_MAVEN_MSO_Mag',outsourceSub,'B_sheath_out'
	get_data,"proton_cyclotron_period",data=datpcp

	pcp=datpcp.y
	npcp=reverse(pcp)

	get_data,'B_sheath_in',data=datIn
	get_data,'B_sheath_out',data=datOut
	get_data,"Franken_fitted_inbound",data=datFFI;{x:xs,y:zin,ytitle:datsin.ytitle,inups:inups,indowns:indowns,imins:inimins,imaxs:inimaxs,chis:inChis}

	get_data,"Franken_fitted_outbound",data=datFFO;{x:xs,y:zout,ytitle:datsin.ytitle,outups:outups,outdowns:outdowns,imins:outimins,imaxs:outimaxs,chis:outChis}
	
	get_data,"overshoot_end_loc_inbound",data=datoei
	;get_data,"overshoot_end_loc_inbound",data=datoeli
	get_data,"overshoot_end_loc_outbound",data=datoeo

	
	get_data,"undershoot_end_loc_inbound",data=datuei
	get_data,"undershoot_end_loc_outbound",data=datueo

	get_data,'downstream_indices_inbound',data=datdsi
	get_data,'downstream_indices_outbound',data=datdso

	;get_data,"downmmdev",data=datdmd
	get_data,"B_meanMode_smoothed",data=datmm

	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
	get_data,'BB_flag',data=datBB
	get_data,'bBB_flag',data=datbBB

	BB=where(datBB.y ne 0,bbn)
	nBB=where(reverse(datBB.y) ne 0,nbbn)


	B=datB.y
	nB=reverse(B)

	mm=datmm.y


	nmm=reverse(mm)

	dsi=datdsi.y
	dso=reverse(datdso.y)

	inups=datFFI.inups
	indowns=datFFI.indowns
	error_status=0

	dmd=mm-indowns

	outups=reverse(datFFO.outups)
	outdowns=reverse(datFFO.outdowns)

	ndmd=nmm-outdowns
	;get_data,"overshoot_end_loc_outbound",data=datoelo
	oei=datoei.y
	oeo=REVERSE(datoeo.y)
	uei=datuei.y
	ueo=REVERSE(datueo.y)
	iwidths=1./(datFFI.MMs[*,1])
	owidths=reverse(1./(datFFO.MMs[*,1]))
	;oeli=where(datoei.y ne 0,icount)
	;oelo=where( REVERSE(datoeo.y) ne 0,ocount)
	sheathin=datIn.y
	sheathout=datOut.y
	rsheathout=reverse(sheathout)
	xx=datIn.x
	ytitle=datIn.ytitle
	;;FORWARD DIRECTION. NEED TO MAKE THIS A FUNCTION?
	N=numel(xx)
	startsub=fltarr(N)
	rendsub=fltarr(N)
	imaxs=fltarr(N) ;inbound overshoot maxima. No special effects required
	imins=fltarr(N)
	omaxs=fltarr(N) ;outbound overshoot maxima. D3STR0Y ALL M3TAHUMAN5
	ominss=fltarr(N) ;outbound overshoot endings. THEY'RE COMING 
	
	ifronts=fltarr(N)
	imins=fltarr(N)
	ofronts=fltarr(N)
	omins=fltarr(N)


	shocks=ins+outs

	B_smooth=sheathin;fltarr(N)
	overshoot=fltarr(N)

	rsmooth=rsheathout;fltarr(N)
	rbsmooth=fltarr(N)
	rovershoot=fltarr(N)
	;idinterval=

	for i=1,  N-1 do begin
		if (sheathin[i] ne 0) and (sheathin[i-1] eq 0) and (finite(sheathin[i]) eq 1) then startsub[i]=1
	endfor


	for i=1,  N-1 do begin
		if (rsheathout[i] ne 0) and (rsheathout[i-1] eq 0) and (finite(rsheathout[i]) eq 1) then rendsub[i]=1
	endfor

	for i=0,N-1 do if B_smooth[i] le 0 then B_smooth[i]=!VALUES.F_NAN
	for w=4,width do begin
		B_smooth=smooth(B_smooth,w,/EDGE_MIRROR,/nan)
	endfor
	for i=0,N-1 do if rsmooth[i] le 0 then rsmooth[i]=!VALUES.F_NAN
	for w=4,width do begin
		rsmooth=smooth(rsmooth,w,/EDGE_MIRROR,/nan)
	endfor


	iwidths=1./(datFFI.MMs[*,1])
	owidths=reverse(1./(datFFO.MMs[*,1]))
	;B_smooth[where(finite(B_smooth,/nan))]=0
	;rsmooth[where(finite(rsmooth,/nan))]=0

		;temp=B_smooth
		;for j=w/4 , np-1-w/2 do begin
			;st=j-w/2
			;en=j+w/2
			;if temp[st] le 0 or temp[en]
			;B_smooth[j]=mean (temp[st:en])
				;B_smooth[j+i]=B_smooth[j]
		;endfor
			
		;endfor

	
	GG=where(startsub ne 0, gcount)

	HH=where(rendsub ne 0, hcount)
	;print,gcount,icount
	SS=where(datsin.y ne 0,scount)

	ZZ=where(reverse(datsout.y) ne 0,zcount)

	;print,GG
	;print,""
	;print,gcount,hcount,scount,zcount
	
	for el=0, scount-1 do begin
		savedend=1

		;endo=oeli[el]
		i=SS[el]
		ishock=i
		wid=iwidths[ishock]
		wd=wid*!pi/2
		wd0=wd
		
		bbloc=(where(i lt BB))[-1]
		if bbloc ne -1 then bi=BB[bbloc] else bi=N-1
		endloc=oei[i];endo;i
		endunder=uei[i];endo;i
		imax=min([(datFFI.imaxs)[i],N-1])
		if datFFI.y[imax] eq 0 then begin
			while datFFI.y[imax] eq 0 and imax gt ishock+3*60 do imax--

		endif
		;ishock=(where(ins[i:imax] eq 1))[0]+i
		;ishock=SS[(where( abs(i-SS) eq min(abs(i-SS))))[-1]]
		pt=pcp[ishock]
		down=indowns[ishock]
		;tt=min([max([100,6*pt]),120])	
		tt=pt;period
		while round(tt) le 60 do tt+=pt

		print,"sheathin[GG[el]]=sheathin[GG[",el,"]]=sheathin[",i,"]=",sheathin[i]

		print,"sheathin[oeli[el]]=sheathin[GG[",el,"]]=sheathin[",endloc,"]="
		print,sheathin[endloc]

		tmax=max(sheathin[i:endloc],tloc)
		tloc=tloc+i
		if imax lt ishock then imax=ishock+2*60
		if endloc le 0 or endunder le 0 or endloc gt imax or endloc le i  or endloc lt ishock  or 1.0*(imax-tloc)/(imax-i) gt .7 or endunder lt endloc or endunder le i  or endunder lt ishock then begin
			savedend=0
			print,"no end, endloc=",endloc,",",time_string(xx[ishock])
			endloc=imax
		endif	
			;print,"no end, endloc=",endloc,",ishock=",ishock,",imax=",imax,",",time_string(xx[ishock])
	if max(sheathin[endloc:imax]) gt max(sheathin[ishock:endloc])   then begin
			savedend=0
			print,"no end,",time_string(xx[ishock])
			endloc=i
			for j=i,min([imax,N-1]) do begin
				endloc++
				if j ge imax then break
				if sheathin[j+1] eq 0 then break
			endfor
		endif	

		if datFFI.y[imax] eq 0 then begin
			while datFFI.y[imax] eq 0 do imax--

		endif
	
		yp=sheathin[i:endloc]
		print,"mean(yp)=",mean(yp)
		np=numel(yp)
		smoothp=B_smooth[i:endloc];yp
		temp=yp
		;for w=4,width do begin
			;temp=smoothp
			;for j=0 , np-1 do begin
				;smoothp[j]=mean (temp[max([0,j-w/2]):min([np-1, j+w/2])])
				;B_smooth[j+i]=B_smooth[j]
			;endfor
			
		;endfor
		;print,"mean(smoothp)=",mean(smoothp)
		;B_smooth[i:i+np-1]=smoothp

		if endloc-i lt 2*wd then begin
			wd=0.
			wid=0.
		endif
		fnl=min([max([endloc-120,.5*(endloc-ishock)+ishock]),.75*(endloc-ishock)+ishock]);fnl=max([endloc-4*60,.75*(endloc-i)+i])
		;yp[frontlocp]

		if endloc lt imax-60 and savedend and dsi[ishock,1] gt i then begin

			if  (endloc-i)/(imax-i) lt .6 or dsi[ishock,1] gt i+wd or max((mm-indowns)[i:fnl]) lt 0 then begin
				fnl=endloc

			endif
			Bmax=max(sheathin[i:fnl],frontlocp)
			frontloc=frontlocp+i
			imaxs[frontloc]=Bmax
			if endunder gt endloc then begin

				minloc=endunder
				imins[minloc]=B[minloc]
				overshoot[i:minloc]=B[i:minloc]
				continue

			endif
		endif else begin
			if max((mm-indowns)[i:fnl]) lt 1.5 then begin

		 
				frontlocp=findnextovershootmax(smoothp,yp,down,wid)
				Bmax=yp[frontlocp]
				frontloc=frontlocp+i
			endif else begin
			frontloc=findoverend(sheathin,mm,inups,indowns,ishock,endloc,startover,wid)
			Bmax=max(sheathin[startover:frontloc])
			frontlocp=frontloc-i
			endelse
		endelse
		;frontloc=frontlocp+i

		ifronts[frontloc]=Bmax
		print,"Inaward, ifronts[",frontloc,"]=",Bmax
		;;Will smmoth the segments locally here in order to locate the valley following Bmax. May need to move this to before Bmax is located.
		
		;need to find next minimum here
		print,"frontlocp=",frontlocp

		catch,error_status
		if error_status ne 0 then begin

		errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON findnextovershootmin"]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			;PRINT,"FAILED ON "+nm
			errorsaver,xx[frontloc],errmsg
			PRINT,"setting to null result"
			minlocp=max([endloc-30-i,frontlocp+1])
			catch,/cancel
		endif

		


		minloc=endloc;minlocp+i
		if endloc gt imax-60 or ~savedend then begin 
			minlocp=findnextovershootmin(smoothP,frontlocp,yp,endunderp,down,wid)
			minloc=minlocp+i
			endunder=endunderp+i
		endif

		;catch,error_status
		if error_status ne 0 then begin

			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON minloc","i="+strtrim(i,2)+",","frontloc"+strtrim(frontloc,2), "minloc=",+strtrim(minloc,2),"N-1=",+strtrim(N-1,2),"endunder=",+strtrim(endunder,2),"N-1=",+strtrim(N-1,2)]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			;PRINT,"FAILED ON "+nm
			errorsaver,xx[frontlocp],errmsg
			minlocp=findnextovershootmin(smoothP,frontlocp,yp,endunderp,down,wid)
			minloc=minlocp+i
			endunder=endunderp+i
			;PRINT,"setting to null result"
			catch,/cancel
			;PRINT,"setting to null result"
			;continue
		endif

		imins[minloc]=sheathin[minloc]
		overshoot[i:minloc]=sheathin[i:minloc]

		maxloc=frontloc
		bmaxmax=max(overshoot[i:minloc],maxloc);max(overshoot[frontloc:minloc],maxloc)
		maxloc+=i;frontloc
		imaxs[maxloc]=bmaxmax
		print,"Inaward, imaxs[",maxloc,"]=",bmaxmax


		if ~savedend then begin

			print,"[i,ishock,minloc,minloc+120,imax]=",[i,ishock,minloc,minloc+120,imax]
			;t=min([N-1-minloc,imax-minloc,120]) 
			;en=min([minloc+tt,N-1]);,imax,N-1]);t,imax,N-1])
			;st=minloc;max([minloc,frontloc])

			t=min([bi-endunder,imax-endunder,tt]) 
			en=min([endunder+tt,bi]);,imax,N-1]);t,imax,N-1])
			st=endunder;max([minloc,frontloc])
			if en lt st then en=st
			if st eq en then st-=tt
			
			isabove=1*(dmd[st+1:min([imax,bi-1])] gt 0)
			isbelow=1*(dmd[st+1:min([imax,bi-1])] gt 0)
			numabove=total(isabove)
			numbelow=total(isbelow)
			;ispass=1*(dmd[isabove+1] le 0 or dmd[isabove-1] le 0)
			isdrop=1*(dmd[where(isabove)+1+(st+1)] le 0 and dmd[where(isabove)-1+(st+1)] gt 0)
			isrise=1*(dmd[where(isabove)-1+(st+1)] le 0 and dmd[where(isabove)+1+(st+1)] gt 0)
			ispass=isrise or isdrop			
			numpass=total(ispass)
			minloc0=endunder;minloc
			maxloc0=maxloc
			Bdmeas=mean(B[st:en])


			if  (st-ishock)/(imax-ishock) lt .4 and  mean(B[st:en])-down gt 3 and numbelow gt 60  then begin

				while (st-ishock)/(imax-ishock) lt .3 or ( (st-ishock)/(imax-ishock) lt .4 and  mean(B[st:en])-down gt 4 and numbelow gt 60) do begin
					;minloc++
					;t=min([N-1-minloc,max([2*pt,min([imax-minloc,120])])]) 
					;en=min([minloc+tt,imax,N-1]);t,imax,N-1])
					;st=max([minloc,frontloc])
					;endunder++

					;if total(ispass) gt 1 and total(isrise) gt 0 then begin 

						;nextrise= (where(isrise))[0]+st

						;if (nextrise-ishock)/(imax-ishock) lt .2 and nextrise gt st-1 then endunder=nextrise else endunder++
					;endif else endunder++
					minloc=findnextovershootmin(smoothP,st,yp,endunderp,down,wid)+i
					endur=endunderp+i					
					t=min([bi-endunder,max([2*pt,min([imax-endunder,tt])])]) 
					en=min([endunder+tt,imax,bi]);t,imax,N-1])
					st=max([minloc,endunder])
					if en lt st then en=st
					if st eq en then st-=tt
					isabove=1*(dmd[st+1:min([imax,bi-1])] gt 0)
					isbelow=1*(dmd[st+1:min([imax,bi-1])] gt 0)
					numabove=total(isabove)
					numbelow=total(isbelow)
					isdrop=1*(dmd[where(isabove)+1+(st+1)] le 0 and dmd[where(isabove)-1+(st+1)] gt 0)
					isrise=1*(dmd[where(isabove)-1+(st+1)] le 0 and dmd[where(isabove)+1+(st+1)] gt 0)
					numpass=total(isdrop+isrise)
				endwhile

				
				if st lt minloc then minloc=st
				imins[minloc0]=0
				imins[minloc]=B[minloc]


				if minloc lt minloc0 then overshoot[minloc:minloc0]=0
				overshoot[i:minloc]=B[i:minloc]
				;imins[endunder]=B[endunder]
				
				maxloc=frontloc
				bmaxmax=max(overshoot[i:minloc],maxloc);minloc],maxloc);max(overshoot[frontloc:minloc],maxloc)
				maxloc+=i;frontloc
				imaxs[maxloc0]=0
				imaxs[maxloc]=bmaxmax
			endif
			print,time_string(xx[st])
			if en gt imax and mean(B[st:en]) lt down and (Bdmeas/down lt .85 or fracdiff(Bdmeas,down) gt .25 or mean(B[st:en]) +4 lt down or B_smooth[en] + 4 lt down) then begin
				minloc0=minloc
				while st gt maxloc and minloc gt maxloc and mean(B[st:en]) lt down and (B[en]/down lt .7 or mean(B[st:en])/down lt .7 or fracdiff(mean(B[st:en]),down) gt .3 or mean(B[st:en]) +4 lt down or B_smooth[en] + 4 lt down) do begin
					st--
					en--
					minloc--

				endwhile
				if st lt minloc then minloc=st
				imins[minloc0]=0
				imins[minloc]=B[minloc]
				overshoot[i:minloc]=B[i:minloc]

			endif			

			print,time_string(xx[st])
			print,time_string(xx[en])
			dd=[st,en]

			for j=ishock-1,endloc do dsi[j,*]=dd

		endif

	endfor
	
	print, "============="
	print, "   BACK END"
	print, "============="
	nFFO=(REVERSE(datFFO.y))
	for el=0, zcount-1 do begin
		print,'----'
		print,"el=",el
		print,'----'
		savedend=1
		minloc=-1
		i=ZZ[el];HH[el]
		bbloc=(where(i lt nBB))[-1]
		if bbloc ne -1 then bi=nBB[bbloc] else bi=N-1
		endloc=N-1-oeo[ZZ[el]];i
		endunder=N-1-ueo[ZZ[el]];i
		ishock=i
		imax=max([N-1-(REVERSE(datFFO.imins))[i],N-1-(REVERSE(datFFO.imaxs))[i]])
		;print,"sheathout[GG[el]]=sheathin[GG[",el,"]]=sheathin[",i,"]=",sheathin[i]
		;print,ZZ

		if nFFO[imax] eq 0 then begin
			while nFFO[imax] eq 0 and imax gt endloc do imax--

		endif
		print,i,endloc,endunder,imax,bi
		ishock=i;ZZ[(where( abs(i-ZZ) eq min(abs(i-ZZ)) and ZZ lt bi))[-1]]
		wid=owidths[ishock]
		wd=wid*!pi/2
		wd0=wd
		
		print,i,endloc
		tmax=max(rsheathout[i:imax],tloc)
		tloc=tloc+i
		if imax-20 lt ishock then imax=ishock+2.*60
		;if endloc gt N-1 or endloc eq -1 or endloc gt imax or endloc le i or 1.0*(imax-tloc)/(imax-i) gt .7 then begin
		if endloc le 0 or endunder le 0 or endloc gt imax or endloc-20. le i  or endloc-20. lt ishock  or 1.0*(imax-tloc)/(imax-i) gt .7 or endunder lt endloc or endunder le i  or endunder lt ishock or endunder gt bi then begin
			print,"no end"

			savedend=0
			endloc=i
			print,imax
			for j=i,imax do begin
				endloc++
				if endloc+1 ge imax then break
				;if endloc le ishock+2.*60. then continue
				
				if j ge imax then break
				;if rsheathout[j+1] eq 0 then break
			endfor
			
		endif
		
		
		pt=npcp[ishock]
		down=outdowns[ishock]
		;tt=min([max([100,6*pt]),120])
		tt=pt;period
		while round(tt) le 60 do tt+=pt

		
		print,"no end,i=",i,", endloc=",endloc,"ishock=",ishock,"imax=",imax
		if max(rsheathout[endloc:imax]) gt max(rsheathout[ishock:endloc]) or endloc lt i+tt   then begin
				savedend=0
			print,"no end"
			endloc2=i
			for j=i,min([imax,N-1]) do begin
				endloc2++
				if j ge imax then break
				if rsheathout[j+1] eq 0 then break
			endfor
			if endloc2 gt endloc then endloc=endloc2
		endif	

		if nFFO[imax] eq 0 then begin
			while nFFO[imax] eq 0 do imax--

		endif
		print,i,endloc
		numErrors=0
		catch,error_status
		if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON overshooter"]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			;PRINT,"FAILED ON "+nm
			errorsaver,xx[N-1-i],errmsg
			PRINT,"setting to null result"
			;omaxs[maxloc]=
			;continue
		endif
		yp=rsheathout[i:endloc]
		;print,"mean(yp)=",mean(yp)
		np=numel(yp)
		print, np
		;if np eq 2 then return
		smoothp=rsmooth[i:endloc];yp
		;temp=yp
		;for w=4,width do begin
			;temp=smoothp
			;for j=0 , np-1 do begin
				;smoothp[j]=mean (temp[max([0,j-w/2]):min([np-1, j+w/2])])
				;B_smooth[j+i]=B_smooth[j]
			;endfor
			
		;endfor
		;print,"mean(smoothp)=",mean(smoothp)
		;rsmooth[i:i+np-1]=smoothp
		;frontlocp=findnextovershootmax(smoothp,yp,down,wid)
		;print,"frontlocp=",frontlocp
		;help,frontlocp
		;Bmax=yp[frontlocp]
		;Bmax=max(yp,frontlocp)
		;if endloc lt imax-60 and savedend then begin
		;	Bmax=max(yp,frontlocp)
		;endif else begin
		;	frontlocp=findnextovershootmax(smoothp,yp,down,wid)
		;	Bmax=yp[frontlocp]
		;endelse
		fnl=min([max([endloc-120,.5*(endloc-ishock)+ishock]),.75*(endloc-ishock)+ishock]);fnl=max([endloc-4*60,.75*(endloc-i)+i]);fnl=max([endloc-4*60,.75*(endloc-i)+i])
		if endloc lt imax-60 and savedend and dso[ishock,1] gt i then begin
			fnl=max([endloc-4*60,.75*(endloc-i)+i])
			if  (endloc-i)/(imax-i) lt .6 or dso[ishock,1] gt i+wd or max((nmm-outdowns)[i:fnl]) lt 0 then begin
				fnl=endloc

			endif
			Bmax=max(rsheathout[i:fnl],frontlocp)
			frontloc=frontlocp+i
			omaxs[frontloc]=Bmax
			if endunder gt endloc then begin

				minloc=endunder
				print,'[i,frontloc,minloc]=',[i,frontloc,minloc]
				omins[minloc]=rsheathout[minloc]
				rovershoot[i:minloc]=rsheathout[i:minloc]
				continue

			endif
		endif else begin
			print,"[i,ishock,fnl,endloc]=",[i,ishock,fnl,endloc]
			if max((nmm-outdowns)[i:fnl]) lt 1.5 then begin
				frontlocp=findnextovershootmax(smoothp[0:fnl-i],yp[0:fnl-i],down,wid)
				Bmax=yp[frontlocp]
				frontloc=frontlocp+i
			endif else begin
				frontloc=findoverend(rsheathout,nmm,outups,outdowns,ishock,endloc,startover,wid)
				Bmax=max(rsheathout[startover:frontloc])
				frontlocp=frontloc-i
			endelse
		endelse
;		frontloc=frontlocp+i

		ofronts[frontloc]=Bmax
		print,i,frontloc,endloc
		;;Will smmoth the segments locally here in order to locate the valley following Bmax. May need to move this to before Bmax is located.
		
		;need to find next minimum here
		;catch,error_status
		if error_status ne 0 then begin

			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON findnextovershootmin"]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			;PRINT,"FAILED ON "+nm
			errorsaver,xx[N-1-frontlocp],errmsg
			PRINT,"setting to null result"
			minlocp=max([endloc-i,frontlocp+1])
			;continue
			catch,/cancel
		endif
		;minlocp=findnextovershootmin(smoothP,frontlocp,yp)
		;minloc=endloc;minlocp+i
		minloc=endloc;minlocp+i
		if endloc gt imax-60 or ~savedend then begin 
			print,"bad overshoot"
			minlocp=findnextovershootmin(smoothP,frontlocp,yp,endunderp,down,wid)
			print,minlocp
			minloc=abs(minlocp)+i
			endunder=endunderp+i
		endif
		errs=0
		;catch,error_status
		if error_status ne 0 then begin
			errs++
		errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON reverse maxloc","i="+strtrim(i,2)+",","frontloc"+strtrim(frontloc,2),"frontlop="+strtrim(frontloc,2), "minloc=",+strtrim(minloc,2),"imax="+strtrim(imax,2),"N-1=",+strtrim(N-1,2),",savedend="+strtrim(savedend,2)]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			;PRINT,"FAILED ON "+nm
			frontlocp=findnextovershootmax(smoothp,yp,down,wid)

			if frontlocp eq -1 then frontlocp=0
			Bmax=yp[frontlocp]
			errorsaver,xx[N-1-frontloc],errmsg
			frontloc=frontlocp+i
			omaxs[frontloc]=max(rsheathout[ishock:frontloc])
			;minlocp=findnextovershootmin(smoothP,frontlocp,yp)
			;minloc=minlocp+i
			;PRINT,"setting to null result"
			if ~savedend then begin
			print,"[i,ishock,minloc,minloc+120,imax,bi]=",[i,ishock,minloc,minloc+120,imax,bi]
			t=min([bi-minloc,imax-minloc,120]) 
			rovershoot[i:minloc]=nB[i:minloc];rovershoot[i:minloc]=rsheathout[i:minloc]
			;st=max([minloc,frontloc])
			st=max([endunder,frontloc])
			en=min([st+tt,imax,bi]);t,imax,N-1])
			dd=[N-1-st,N-1-en]
			for j=ishock-1,min([imax,bi]) do dso[j,*]=dd
			print,dso[ishock,*]
		endif
			catch,/cancel
			continue
		endif

		print,i,frontloc,minloc,imax
		omins[minloc]=rsheathout[minloc]	;		omins[minloc]=rsheathout[minloc]
		rovershoot[i:minloc]=rsheathout[i:minloc];rovershoot[i:minloc]=rsheathout[i:minloc]
		maxloc=frontloc
		bmaxmax=max(rovershoot[i:minloc],maxloc);max(rovershoot[frontloc:minloc],maxloc)
		maxloc+=i;frontloc
		omaxs[maxloc]=bmaxmax	
		if ~savedend then begin
			print,"[i,ishock,minloc,minloc+120,imax]=",[i,ishock,minloc,minloc+120,imax]
		;	t=min([N-1-minloc,imax-minloc,120]) 
		;	st=max([minloc,frontloc])
		;	en=min([st+tt,imax,N-1]);t,imax,N-1])
			t=min([bi-endunder,imax-endunder,120]) 
			en=min([endunder+tt,bi]);,imax,N-1]);t,imax,N-1])
			st=endunder;max([minloc,frontloc])
			if en lt st then en=st
			if st eq en then st-=tt
			;numabove=total(ndmd[st+1:imax] gt 0)
			;numbelow=total(ndmd[st+1:imax] lt 0)
		;	numpass=total(ndmd[st+1:imax] eq 0)
			isabove=1*(ndmd[st+1:min([imax,bi-1])] gt 0)
			isbelow=1*(ndmd[st+1:min([imax,bi-1])] gt 0)
			numabove=total(isabove)
			numbelow=total(isbelow)
			;ispass=1*(dmd[isabove+1] le 0 or dmd[isabove-1] le 0)
			isdrop=1*(ndmd[where(isabove)+1+(st+1)] le 0 and ndmd[where(isabove)-1+(st+1)] gt 0)
			isrise=1*(ndmd[where(isabove)-1+(st+1)] le 0 and ndmd[where(isabove)+1+(st+1)] gt 0)
			ispass=isrise or isdrop			
			numpass=total(ispass)
			minloc0=endunder;minloc
			maxloc0=maxloc
			minloc00=minloc0
			if  (st-ishock)/(imax-ishock) lt .4 and  mean(nB[st:en])-down gt 4 and numbelow gt 60  then begin
				reps=0
				while  (st-ishock)/(imax-ishock) lt .3 or ((st-ishock)/(imax-ishock) lt .4 and  mean(nB[st:en])-down gt 4 and numbelow gt 60) do begin

					

					;minloc++

					minlocp=findnextovershootmin(smoothP,st,yp,endunderp,down,wid)
					minlocp00=minlocp
					minloc=minlocp+i
					endunder=endunderp+i
					t=min([bi-endunder,imax-endunder,120]) 
					en=min([endunder+tt,imax,bi]);t,imax,N-1])
					st=max([endunder,frontloc])
					if en lt st then en=st
					if st eq en then st-=tt
					isabove=1*(ndmd[st+1:min([imax,bi-1])] gt 0)
					isbelow=1*(ndmd[st+1:min([imax,bi-1])] gt 0)
					numabove=total(isabove)
					numbelow=total(isbelow)
					numpass=total(ndmd[st+1:imax] eq 0)
					if minlocp00 eq minlocp and reps gt 0 then break
					reps++

				endwhile
				if st lt minloc then minloc=st
				omins[minloc]=nB[minloc]	;		omins[minloc]=rsheathout[minloc]
				rovershoot[i:minloc]=nB[i:minloc];rovershoot[i:minloc]=rsheathout[i:minloc]
				maxloc=frontloc
				bmaxmax=max(nB[i:minloc],maxloc);max(rovershoot[frontloc:minloc],maxloc)
				maxloc+=i;frontloc
				
				omaxs[maxloc]=bmaxmax	
				omins[minloc0]=0
				


				if minloc lt minloc0 then rovershoot[minloc:minloc0]=0
				;imins[endunder]=B[endunder]
				
		
			endif
			if en gt imax and mean(nB[st:en]) lt down and (mean(nB[st:en])/down lt .85 or fracdiff(mean(nB[st:en]),down) gt .25 or mean(nB[st:en]) +4 lt down) then begin
				minloc0=minloc
				while st gt maxloc and minloc gt maxloc and mean(nB[st:en]) lt down and (mean(nB[st:en])/down lt .7 or fracdiff(mean(nB[st:en]),down) gt .3 or mean(nB[st:en]) +4 lt down or rsmooth[en] + 4 lt down) do begin
					st--
					en--
					minloc--

				endwhile
				if st lt minloc then minloc=st
				omins[minloc0]=0
				omins[minloc]=B[minloc]
				if minloc lt minloc0 then rovershoot[minloc:minloc0]=0
				rovershoot[i:minloc]=nB[i:minloc]

			endif
			dd=[N-1-st,N-1-en]
			for j=ishock-1,min([imax,bi]) do dso[j,*]=dd
			print,dso[ishock,*]
		endif
		print,"dso[ishock,*]=dso[",ishock,",*]=",dso[ishock,*]


	endfor
	dso=reverse(dso)
	bovershoot=reverse(rovershoot)
	
	bsmooth=reverse(rsmooth)

	;rsmooth=rsmooth+bsmooth
	omins=reverse(omins)
	ofronts=reverse(ofronts)
	omaxs=reverse(omaxs)
	;print,where(imaxs+omaxs ne 0)
	;print,"mean(B_smooth)=",mean(B_smooth)
	;print,"max(B_smooth)=",max(B_smooth)
	;store_data,"overfronts_inbound",data={x:xx,y:ifronts,ytitle:ytitle}
	store_data,"B_maxs_inbound",data={x:xx,y:imaxs,ytitle:ytitle}
	;store_data,"overbacks_inbound",data={x:xx,y:imins,ytitle:ytitle}
	;store_data,"Sheath_in_smooth",data={x:xx,y:B_smooth,ytitle:ytitle}
	store_data,"overshoot_inbound",data={x:xx,y:overshoot,ytitle:ytitle}

	;store_data,"overfronts_outbound",data={x:xx,y:ofronts,ytitle:ytitle}
	store_data,"B_max_outbound",data={x:xx,y:omaxs,ytitle:ytitle}
	store_data,"B_mins_outbound",data={x:xx,y:omins,ytitle:ytitle}
	;store_data,"Sheath_out_smooth",data={x:xx,y:bsmooth,ytitle:ytitle}
	store_data,"overshoot_outbound",data={x:xx,y:bovershoot,ytitle:ytitle}

	;store_data,"overfronts",data={x:xx,y:ifronts+ofronts,ytitle:ytitle}
	store_data,"B_maxs",data={x:xx,y:imaxs+omaxs,ytitle:ytitle}
	;store_data,"overbacks",data={x:xx,y:imins+omins,ytitle:ytitle}
	;store_data,"Sheath_smooth",data={x:xx,y:B_smooth+bsmooth,ytitle:ytitle}
	store_data,"overshoot",data={x:xx,y:overshoot+bovershoot,ytitle:ytitle}
	;options,"overshoot",'colors','r'
	;options,"B_maxs",'colors','g'
	;store_data,'Bfo',data='mvn_B_1sec_MAVEN_MSO_Mag Franken_fitted overshoot'
	;store_data,'Bfom',data='mvn_B_1sec_MAVEN_MSO_Mag Franken_fitted overshoot B_maxs'
	get_data,"downstream_indices",data=datds
	datdsi.y=dsi
	datdso.y=dso
	;print,datdso.y[where(shocks ne 0),*]
	;store_data,'downstream_interval_inbound',data=datdsi
	;store_data,'downstream_interval_outbound',data=datdso
	store_data,"downstream_indices_inbound",data=datdsi
	store_data,"downstream_indices_outbound",data=datdso
	ds=datds.y
	for i=0,N-1 do begin
		if sheathin[i] ne 0 then ds[i,*]=dsi[i,*]
 		if sheathout[i] ne 0 then ds[i,*]=dso[i,*]
		;if dsi[i,0] gt -1 then ds[i,*]=dsi[i,*]
		;if dso[i,0] gt -1 then ds[i,*]=dso[i,*]
		;if shocks[i] eq 1 then print,"ds=",ds[i,*]
	endfor
	;print,ds[GG,*]
	;print,"aaa"
	;print,ds[N-1-reverse(HH),*]
	;ds=datdsi.y+datdso.y
	;get_data,"downstream_indices",data=datds

	datds.y=ds
	store_data,"downstream_indices",data=datds

end
