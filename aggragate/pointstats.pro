pro pointstats,load2=load2,RemoveBigFracM=RemoveBigFracM,maxAllowedFM=maxAllowedFM,RemoveBigDist=RemoveBigDist,maxdist=maxdist


	src2=''
	if keyword_set(load2) then src2='2'
	srctxt="Documents/overmachDays"+src2+".txt"
	read_strDATA,H,srctxt,numLines=numPlots
	H=H[UNIQ(H, SORT(H))]

	numPlots=numel(H)


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

	maxAllowedFM=10 

	if keyword_set(omitBad) then omitProbs=1
	if not keyword_set(maxdist) then maxdist=2000*(11+1-removeDist)
	;first get names of tplot files from txt file
	killNegOverDiff=0
	if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	if keyword_Set(RemoveBigFracM) then killBigFM=1
	if not keyword_set(maxAllowedFM) then maxAllowedFM=10 else killBigFM=1
	;if not keyword_set(maxUpDev) then maxUpDev=5
	if not keyword_set(maxUpDiff) then maxUpDiff=1.3
	if not keyword_set(maxB2B1fracdiff) then maxB2B1fracdiff=.6
	if not keyword_set(maxMagJump) then maxMagJump=7.
	if not keyword_set(mindop) then mindop=.4
	if not keyword_set(minMfms) then minMfms=0.0
	if not keyword_set(betamaxallowed) then betamaxallowed=9.;20.


	goodPoints=0
	points=0
	isGood=1
	savelist=list()
	datepointsLst=list()
	dategoodLst=list()
	textlines=list()

	for i=0,numPlots-1 do begin
		;;print,H
		if total(savelist eq H[i]) eq 1 then continue

		savelist.add,H[i]
		;if i gt 0 and H[i] eq H[i-1] then continue
		
		;name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		;get_data,name,data=dat

		;if size(dat,/typ) eq 2 then begin

			tplot_restore,filename="Documents/overVsMachData"+src2+"/"+H[i]
	

			name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
			get_data,name,data=dat

		;endif	
		;help,dat
		if size(dat,/typ) eq 2 then continue
		;print,H[i]
		if numel(dat.t) gt numel(dat.shock0dist) then print,H[i],"not equal"
		if numel(dat.t) gt numel(dat.shock0dist) then print,numel(dat.t),numel(dat.shock0dist)
		date=name.remove(0,10)
		t00=time_double(date)
		;help,dat
		if dat.t[0] ge time_double('2019-11-16/00:00:00') or H[i] eq 'OverVsMach_2019-11-16.tplot' or t00 ge time_double('2019-11-16/00:00:00') then break
		if numel(dat.t) le 0 then begin
			del_data,'*'
			continue

		endif
		unqt=dat.t[ uniq(dat.t)]
		todaypoints=numel(unqt);dat.t)
		todaygood=0

		datepointsLst.add,todaypoints
		textlines.add,date+" :: "+strtrim(todaypoints,2)
		del_data,'*'
		continue
		for j=0,numel(dat.t)-1 do begin
			todaypoints++
			pointtext=""
			if j+1 gt numel(dat.shock0dist) then continue
			
			isGood=1
			;points++
			xt=dat.t[j];t[j]

			t0=x2Greg(xt,/strformat,/noHour)+"/0:00"
			ts=x2Greg(xt,/strformat)
			mfms=dat.mfms[j]
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
			crit=dat.crits[j]
			FM=mfms/crit
			Bmax=dat.Bmaxs[j]
			down=dat.downs[j]
			up=dat.ups[j]

			N_e=dat.N_e[j]
			N_p=dat.N_p[j]

			Tion=dat.T_ion[j]
			Beta_ion=dat.beta_ion[j]
			Telec=Tion*(bta/Beta_ion -1.)

			;print,numel(dat.shock0dist),j
			;print,dat.shock0dist
			dop=dat.shock0Acc[j]
			dist0=dat.shock0dist[j]
			pointtext=x2Greg(xt,/strformat)+";"+x2Greg(xt,/strformat,/noHour)+"/0:00"

			;mxFM=max(dat.mfms[j])



			bdmanpoint=0
			badPoint=0
			T_rat=1.0 * Telec/Tion
			if finite(N_e) ne 1 or N_e le 0 or N_p le 0 or N_e /N_p gt 100 or T_rat lt 0  then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badPoint=1;continue
			endif


			t=dat.t[j];shockUnix[j];dat.t[j]

			xt=x2Greg(t,/strformat)
			;print,xt
			Ls=dat.Ls[j]
			;print,j
			;print,numel(dat.t)
			;print,size(dat.Ls,/n_el)
			;print,size(Ls,/typ)
			;print,Ls
			points++
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
				;badMfms++
				print,"badMfms,",mfms,xt
				;print,"badFMs"
				badPoint=1;continue
			endif

			if finite(N_e) ne 1 then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badPoint=1;continue
			endif

			
			if dist0 gt 2500 then begin

				;print,"dist0=",dist0
				;baddists++
				badPoint=1
				 ;continue
			endif
			if ((abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 2*dstd) then badPoint=1

			if downup ge maxMagJump or downM/upM ge maxMagJump or (downup lt 1.5 and down-up lt 1.3) or Bmax-down lt 0 then begin
				badPoint=1
				;badB2B1++


			endif

			if dop lt mindop then begin
				;print,"mindop=",dop
				;baddots++
				badPoint=1;continue
			endif
			if bta gt betamaxallowed or finite(bta) ne 1 or bta le 0. or Beta_ion gt betamaxallowed then begin
				;badbeta++
				badPoint=1
				;print,xt
				;print,bta
				;PRINT,Ls
				

			endif

			if (abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 2*dstd then begin;or abs(downM-down)/mean([downM,down])  gt .3 or duRatio gt 3 or duRatio lt .33 or fracdiff(dum,downup) gt maxB2B1fracdiff then begin
				;badMeasure++
				;print,"bad measure"
				;print,H[i]
				badPoint=1
			endif 

			if FM le 0.0 then begin
				;badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif

			if (killBigFM) and (FM ge maxAllowedFM) then begin
				;badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif
			if (killNegOverDiff) and ((Bmax lt down ) or (Bmax lt up) or (Bmax lt downM) ) or (Bmax-down)/down gt 4   then begin
				;badBmax++
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
				;badLS++
				;print,"Bad Ls"
				badPoint=1
	
			endif


			if badPoint then begin
				continue
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


			;if saveMeta gt 0 then begin
			;	if isgood then pointtext+="   GOOD" else pointtext+="   BAD"+problemtext
			;endif
			;if saveMeta gt 1 then begin
				;pointtext+=nline+";   (upstream,Bmax,downstream)= ("+strtrim(up,2)+","+strtrim(Bmax,2)+","+strtrim(down,2)+")"
			;endif
			todaygood++
		endfor
		;textlines.add,date+" :: "+strtrim(todaygood,2)+" / "+strtrim(todaypoints,2)+" = "+strtrim(1.0*todaygood/todaypoints,2)
		;datepointsLst.add,todaypoints
		;dategoodLst.ADD,todaygood		
		del_data,name
	endfor

	datepoints=datepointsLst.toarray()
	;dategoods=dategoodLst.toarray()

	;totalgood=total(dategoods)
	;avggood=mean(1.0*dategoods)
	
	totalpoints=total(datepoints)
	avgpoints=mean(1.0*datepoints)

	;fracs=1.0*dategoods/datepoints
	;avgfrac=mean(fracs)

	fname=dire+'shock_statistics'+src2+'.png'

	openW,1,fname+".txt"
	printf,1,"YYYY-MM-DD :: totalpoints "
	foreach el,textlines do printf,1,el
	printf,1,'=================='
	printf,1,'average,total: '+strtrim(avgpoints,2)+","+strtrim(totalpoints)
	;printf,1,'average: '+strtrim(avggood,2)+'/'+strtrim(avgpoints,2)+'='+strtrim(avggood/avgpoints,2)+', \`='+strtrim(avgfrac,2)
	;printf,1,'total: '+strtrim(totalgood,2)+'/'+strtrim(totalpoints)+'='+strtrim(totalgood*1.0/totalpoints,2)
	close,1
end
