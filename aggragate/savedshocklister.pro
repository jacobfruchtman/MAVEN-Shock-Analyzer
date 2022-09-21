pro savedshocklister,RemoveBigFracM=RemoveBigFracM,maxAllowedFM=maxAllowedFM,RemoveBigDist=RemoveBigDist,maxdist=maxdist,includeMeta=includeMeta,omitBad=omitBad,load2=load2,mindop=mindop,$
					maxUpDiff=maxUpDiff,maxB2B1fracdiff=maxB2B1fracdiff,betamaxallowed=betamaxallowed,$
					maxMagJump=maxMagJump,keepPathologies=keepPathologies,betaparttype=betaparttype,$
					minMfms=minMfms,maxover=maxover
	dire="Documents/"
	;FILE_MKDIR,dire
	doClose=1




	killBigFM=0
	;killNegOverDiff=0
	saveMeta=1
	REMOVENEGATIVES=1
	omitProbs=0
	removeDist=0
	;if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	if keyword_Set(RemoveBigFracM) then killBigFM=1
	if keyword_Set(RemoveBigDist) then removeDist=11
	maxAllowedFM=10 
	if not keyword_set(includeMeta) then saveMeta=0 else saveMeta=includeMeta
	if keyword_set(omitBad) then omitProbs=1
	if not keyword_set(maxdist) then maxdist=2000*(11+1-removeDist)
	;first get names of tplot files from txt file
	killNegOverDiff=0
	if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	if keyword_Set(RemoveBigFracM) then killBigFM=1
	if not keyword_set(maxAllowedFM) then maxAllowedFM=10 else killBigFM=1
	;if not keyword_set(maxUpDev) then maxUpDev=5
	if not keyword_set(maxUpDiff) then maxUpDiff=1.3
	if not keyword_set(maxover) then maxover=5.
	if not keyword_set(maxB2B1fracdiff) then maxB2B1fracdiff=.6
	if not keyword_set(maxMagJump) then maxMagJump=7.
	if not keyword_set(mindop) then mindop=.4
	if not keyword_set(minMfms) then minMfms=1.0
	if not keyword_set(betamaxallowed) then betamaxallowed=20.

	if not keyword_set(maxUpDiff) then maxUpDiff=1.3
	;if not keyword_set(maxB2B1fracdiff) then maxB2B1fracdiff=.6
	;if not keyword_set(maxMagJump) then maxMagJump=7.
	;if not keyword_set(mindop) then mindop=.4
	;if not keyword_set(minMfms) then minMfms=1.0
	;if not keyword_set(betamaxallowed) then betamaxallowed=9.;20.

	;first get names of tplot files from txt file

	;H=0
	;H=0
	corrText=""
	;if killBigFM or killNegOverDiff then corrText="_WithoutPathologies"

	srctxt="Documents/overmachDays.txt"

	if keyword_set(load2) then srctxt="Documents/overmachDays2.txt"
	read_strDATA,H,srctxt,numLines=numPlots
	H=H[UNIQ(H, SORT(H))]

	numPlots=numel(H)

	;in for loop, restore these files and create an array of data structures



	;maxlocs:GG,t:xs[GG],mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,downups:downups[GG],
	;ANGLE:MX3[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG]}
	locsLst=list()	
	tLst=list()
	
	locList=list()

	bins=list()
	
	pointlist=list()
	selectedlist=list()

	datepointsLst=list()
	dategoodLst=list()
	textlines=list()

	savelist=list()

	goodPoints=0
	points=0
	isGood=1


	baddists=0
	baddots=0
	badFMs=0
	badBmax=0
	badMeasure=0
	badManual=0
	badLS=0
	badbeta=0
	badB2B1=0
	badMfms=0
	badNANs=0
	badDens=0

	nline=string([13B, 10B])
	for i=0,numPlots-1 do begin

		daypoints=0.
		daygoodpoints=0.
		;;print,H
		if total(savelist eq H[i]) eq 1 then continue

		savelist.add,H[i]
		;if i gt 0 and H[i] eq H[i-1] then continue
		
		;name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		;get_data,name,data=dat

		;if size(dat,/typ) eq 2 then begin
		error_status=0
		catch,error_status
		if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			print,i
			print,H[i]
			print,'sadasd'
			return
		;		catch,/cancel
				;continue
		endif
			tplot_restore,filename="Documents/overVsMachData/"+H[i]
	

			name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
			get_data,name,data=dat

		;endif	
		;help,dat
		catch,error_status
		if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			print,i
			print,H[i]
			FOREACH EL,dat.t do print,time_string(EL)
			help,dat
			
			return
		;		catch,/cancel
				;continue
		endif
		if size(dat,/typ) eq 2 then continue
		;print,H[i]
		if numel(dat.t) gt numel(dat.shock0dist) then print,H[i],"not equal"
		if numel(dat.t) gt numel(dat.shock0dist) then print,numel(dat.t),numel(dat.shock0dist)
		if numel(dat.t) lt numel(dat.T_ion) then begin
			print,"numel(dat.t) lt numel(dat.T_ion)"
			help,dat
			help,dat.t
			help,dat.T_ion
			return

		endif
		date=name.remove(0,10)
		if numel(dat.t) gt numel(dat.T_ion) then begin
			print,name
			help,dat
			;return
			;del_data,'*'
			;continue
			print,numel(dat.t),numel(dat.T_ion)
			unq=uniq(dat.t,sort(dat.t))
			print,numel(unq),numel(dat.T_ion)
			if numel(unq) ne numel(dat.T_ion) then return
			str_element,dat,'MAXLOCS',/add,dat.MAXLOCS[unq]
			str_element,dat,'T',/add,dat.T[unq]
			str_element,dat,'MFMS',/add,dat.MFMS[unq]
			str_element,dat,'CRITS',/add,dat.CRITS[unq]
			str_element,dat,'BMAXS',/add,dat.BMAXS[unq]
			str_element,dat,'OVERDIFFS',/add,dat.OVERDIFFS[unq]
			str_element,dat,'FRACOVERDIFFS',/add,dat.FRACOVERDIFFS[unq]
			str_element,dat,'DOWNUPS',/add,dat.DOWNUPS[unq]
			str_element,dat,'ANGLE',/add,dat.ANGLE[unq]
			str_element,dat,'DOWNS',/add,dat.DOWNS[unq]
			str_element,dat,'UPS',/add,dat.UPS[unq]
			str_element,dat,'BETAS',/add,dat.BETAS[unq]
			str_element,dat,'SHOCK0DIST',/add,dat.SHOCK0DIST[unq]
			str_element,dat,'SHOCKUNIX',/add,dat.SHOCKUNIX[unq]
			str_element,dat,'SHOCK0ACC',/add,dat.SHOCK0ACC[unq]
			str_element,dat,'DOWNMEASURED',/add,dat.DOWNMEASURED[unq]
			str_element,dat,'UPMEASURED',/add,dat.UPMEASURED[unq]
			str_element,dat,'UPSTD',/add,dat.UPSTD[unq]
			str_element,dat,'DOWNSTD',/add,dat.DOWNSTD[unq]
			str_element,dat,'POS',/add,dat.POS[unq, 3]
			str_element,dat,'ALFVEN',/add,dat.ALFVEN[unq]
			str_element,dat,'UINDICES',/add,dat.UINDICES[unq, 2]
			str_element,dat,'DINDICES',/add,dat.DINDICES[unq, 2]
			str_element,dat,'CS',/add,dat.CS[unq]
			str_element,dat,'FLOWANGLE',/add,dat.FLOWANGLE[unq]
			str_element,dat,'IMINS',/add,dat.IMINS[unq]
			str_element,dat,'IMAXS',/add,dat.IMAXS[unq]
			str_element,dat,'N_P',/add,dat.N_P[unq]
			str_element,dat,'N_E',/add,dat.N_E[unq]
			str_element,dat,'UPSTARTPOS',/add,dat.UPSTARTPOS[unq, 3]
			str_element,dat,'UPMIDPOS',/add,dat.UPMIDPOS[unq, 3]
			str_element,dat,'UPENDPOS',/add,dat.UPENDPOS[unq, 3]
			str_element,dat,'DOWNSTARTPOS',/add,dat.DOWNSTARTPOS[unq, 3]
			str_element,dat,'DOWNMIDPOS',/add,dat.DOWNMIDPOS[unq, 3]
			str_element,dat,'DOWNENDPOS',/add,dat.DOWNENDPOS[unq, 3]
		endif

		catch,error_status
		if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			print,i
			print,H[i]
			
			help,dat
			
			return
		;		catch,/cancel
				;continue
		endif
		for j=0,numel(dat.t)-1 do begin
			pointtext=""

			;if j+1 gt numel(dat.shock0dist) then continue
			;if numel(dat.t) ne numel(dat.T_ion) then break
			problemtext=nline
			isGood=1
			;points++
			xt=dat.t[j];t[j]
			badNAN=0
			t0=x2Greg(xt,/strformat,/noHour)+"/0:00"
			ts=x2Greg(xt,/strformat)
			mfms=dat.mfms[j]

			crit=dat.crits[j]
			FM=mfms/crit
			Bmax=dat.Bmaxs[j]
			down=dat.downs[j]
			up=dat.ups[j]
			;print,numel(dat.shock0dist),j
			;print,dat.shock0dist
			dop=dat.shock0Acc[j]
			dist0=dat.shock0dist[j]
			pointtext=x2Greg(xt,/strformat)+";"+x2Greg(xt,/strformat,/noHour)+"/0:00"

			;mxFM=max(dat.mfms[j])



			bdmanpoint=0
			badPoint=0



			t=dat.t[j];shockUnix[j];dat.t[j]
			;if j gt 0 and numel(tLst) gt 1 and total(tLst eq t) gt 0 then continue
			xt=x2Greg(t,/strformat)
			;print,xt
			Ls=dat.Ls[j]
			;print,j
			;print,numel(dat.t)
			;print,size(dat.Ls,/n_el)
			;print,size(Ls,/typ)
			;print,Ls
			points++
			daypoints++
			;lll=dat.Ls
			;seasons.add,marsSeasonFinder(lll,dat.lat)
			;sss=marsSeasonFinder(lll,dat.lat)
			;listloc=(where(xt eq shocklist2))[0]
			;if  listloc eq -1 then begin
				;badManual++
				;badPoint=1
				;continue
			;endif



			mfms=dat.mfms[j]

			crit=dat.crits[j]
			FM=mfms/crit
			
			Bmax=dat.Bmaxs[j]
			down=dat.downs[j]
			up=dat.ups[j]
			upM=dat.downMeasured[j]
			downM=dat.upMeasured[j] ; yes. I know. Need to fix the mix up later
			dop=dat.shock0Acc[j]
			dist0=dat.shock0dist[j]
			dum=downM/upM
			downup=dat.downups[j]
			duRatio=abs(dum/downup)
			bta=dat.betas[j]
			;N_e=dat.N_e
			;N_p=dat.N_p
			dstd=dat.downSTD[j]
			ustd=dat.upSTD[j]
			;print,"downM=",downM
			;print,"upM=",upM
			;mxFM=max(dat.mfms[j])
			N_e=dat.N_e[j]
			N_p=dat.N_p[j]
			Tion=dat.T_ion[j]
			Beta_ion=dat.beta_ion[j]
			Telec=Tion*(bta/Beta_ion -1.)

			if finite(N_e) ne 1 or finite(N_p) ne 1  or finite(bta) ne 1  then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badNANs++
				badNAN=1
				badPoint=1;continue
			endif

			if downup ne down/up then begin
				print,xt
				print,"downup=",downup
				print,"down/up=",down/up
				print,"Mfms=",mfms
				print,"beta=",bta
				badPoint=1;continue
				;return
			endif


			if mfms lt minMfms then begin
				badMfms++
				print,"badMfms,",mfms,xt
				;print,"badFMs"
				badPoint=1;continue
			endif

			;if finite(N_e) ne 1 then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				;badPoint=1;continue
			;endif
			T_rat=1.0 * Telec/Tion
			if N_e le 0 or N_p le 0 or N_e /N_p gt 100   then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badDens++
				badPoint=1;continue
			endif
			
			if dist0 gt 2500 then begin

				;print,"dist0=",dist0
				baddists++
				badPoint=1
				 ;continue
			endif

			if downup ge maxMagJump or downM/upM ge maxMagJump or (downup lt 1.5 and down-up lt 1.3) then begin
				badPoint=1
				badB2B1++


			endif

			if dop lt mindop then begin
				;print,"mindop=",dop
				baddots++
				badPoint=1;continue
			endif
			;if bta gt betamaxallowed or finite(bta) ne 1 or bta le 0. then begin
				;badbeta++
				;badPoint=1
				;print,xt
				;print,bta
				;PRINT,Ls
				

			;endif
			if  bta le 0. then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				;PRINT,Ls
				

			endif
			if bta gt betamaxallowed or Beta_ion gt betamaxallowed/2.0  then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				;PRINT,Ls
				

			endif

			if (abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 2*dstd then begin;or abs(downM-down)/mean([downM,down])  gt .3 or duRatio gt 3 or duRatio lt .33 or fracdiff(dum,downup) gt maxB2B1fracdiff then begin
				badMeasure++
				;print,"bad measure"
				;print,H[i]
				badPoint=1
			endif 

			if FM le 0.0 then begin
				badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif

			if (killBigFM) and (FM ge maxAllowedFM) then begin
				badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif
			if (killNegOverDiff) and ((Bmax lt down ) or (Bmax lt up) or (Bmax lt downM) ) or (Bmax-down)/down gt maxover   then begin
				badBmax++
				;print,"badBmax"
				badPoint=1;continue
			endif
			if finite(dat.ANGLE[j]) ne 1 then badPoint=1
			Ls=dat.Ls[j]
			;lll=Ls*0
			;catch,error_status
			;if error_status ne 0 then begin
			;badPoint=1
			;print,xt
			;print,H[i]
			;help,dat
			;help,dat.Ls
			;help,dLs
				;catch,/cancel
			;return
			;endif
			;lll=Ls*0
			if size(Ls,/typ) eq 0 then begin
				;print,xt
				;print,H[i]
				badLS++
				;print,"Bad Ls"
				badPoint=1
	
			endif


			if badPoint then begin
				if ~badNAN then pointtext="A@"+pointtext else pointtext="N@"+pointtext
			endif


			;if dist0 gt maxdist then begin

				;if omitProbs then continue
			;	isGood=0
			;	problemtext+="; dis0="+strtrim(dist0,2)+">"+strtrim(maxdist,2)+nline

			;endif
			;if dop lt .1 then begin
				;if omitProbs then continue
				;isGood=0
				;problemtext+="; N_sw dot N_conic="+strtrim(dop,2)+"<"+".1"+nline

			;endif
			;if FM le 0.01 then begin
			;	if omitProbs then continue
			;	isGood=0
			;	problemtext+="; M/Mcrit="+strtrim(FM,2)+"<="+".01"+nline
			;endif
			;if (killBigFM) and (FM ge maxAllowedFM) then begin
			;	if omitProbs then continue
			;	isGood=0
			;	problemtext+="; M/Mcrit="+strtrim(FM,2)+">="+strtrim(maxAllowedFM,2)+nline
			;endif

			;if (Bmax lt down )  then begin
			;	if omitProbs then continue
			;	isGood=0
			;	problemtext+="; Bmax="+strtrim(Bmax,2)+"<"+strtrim(down,2)+"=downstream fit"+nline
			;endif
			;if (Bmax lt up) then begin
			;	if omitProbs then continue
			;	isGood=0
			;	problemtext+="; Bmax="+strtrim(Bmax,2)+"<"+strtrim(up,2)+"=upstream fit"+nline
			;endif
			
			goodPoints+=1-badPoint;isGood

			daygoodpoints+=1-badPoint
			;if saveMeta gt 0 then begin
			;	if isgood then pointtext+="   GOOD" else pointtext+="   BAD"+problemtext
			;endif
			;if saveMeta gt 1 then begin
				;pointtext+=nline+";   (upstream,Bmax,downstream)= ("+strtrim(up,2)+","+strtrim(Bmax,2)+","+strtrim(down,2)+")"
			;endif
			pointlist.add,pointtext

		endfor

		datepointsLst.add,daypoints
		dategoodLst.add,daygoodpoints
		textlines.add,date;+" :: "+strtrim(daygoodpoints,2)+" / "+strtrim(daypoints,2)

		print,"goodpoints / points:",goodpoints,"/",points
		del_data,name
		
	endfor
	catch,error_status
	if error_status ne 0 then begin
		PRINT, 'Error index: ', error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		print,'after'
		return
		;		catch,/cancel
				;continue
	endif	
	fname=dire+"savedPoints"
	;if keyword_set(load2) then fname+="2"
	if saveMeta then fname+="_withMetadata"
	if omitProbs then fname+="_withoutPathologies"
	;pointlist=pointlist.sort()
	openW,1,fname+".txt"
	foreach el,pointlist do printf,1,el

	printf,1,";===================="
printf,1,";"+strtrim(goodPoints,2)+" good points"+nline+";--------------"
	if (saveMeta gt 0) or omitProbs then printf,1,";"+strtrim(goodPoints,2)+" good points"+nline+";--------------"
	printf,1,";"+strtrim(points,2)+" points"
	if (saveMeta gt 0)or omitProbs then printf,1,";"+strtrim(1.0*goodPoints/points,2)+"% valid"
	close,1
	print,strtrim(goodPoints,2)+" good points"
	print,strtrim(points,2)+" points"

	dategoodLst=dategoodLst.toarray()
	datepointsLst=datepointsLst.toarray()
	avgpoints=mean(datepointsLst)
	avggoodpoints=mean(dategoodLst)
	fname=dire+'shock_statistics2.png'
	format2='(A10,:," :: ", I2, " / ", I2 )'
	openW,1,fname+".txt"
	printf,1,"YYYY-MM-DD :: goodpoints / allpoints "
	;foreach el,textlines do printf,1,el
	for i=0, numel(textlines)-1 do printf,1, textlines[i],dategoodLst[i],datepointsLst[i],format=format2
	printf,1,'=================='
	printf,1,'good: avg, total: ',avggoodpoints ,goodpoints,format='(A18," ", F10, " , ", I4)'
	printf,1,' all: avg, total: ',avgpoints,points ,format='(A18," ", F10, " , ", I4)'
	;printf,1,'average: '+strtrim(avggood,2)+'/'+strtrim(avgpoints,2)+'='+strtrim(avggood/avgpoints,2)+', \`='+strtrim(avgfrac,2)
	;printf,1,'total: '+strtrim(totalgood,2)+'/'+strtrim(totalpoints)+'='+strtrim(totalgood*1.0/totalpoints,2)
	close,1
	print,"[badNANs,baddists,baddots,badB2B1,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta]"
	print,[badNANs,baddists,baddots,badB2B1,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta]

end
