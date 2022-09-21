pro overconcluder,islast=islast,currtime=currtime,REMOVENEGATIVES=REMOVENEGATIVES,RemoveBigFracM=RemoveBigFracM,$
					maxAllowedFM=maxAllowedFM,load2=load2,mindop=mindop,maxdist=maxdist,$
					maxUpDiff=maxUpDiff,maxB2B1fracdiff=maxB2B1fracdiff,betamaxallowed=betamaxallowed,$
					maxMagJump=maxMagJump,keepPathologies=keepPathologies,betaparttype=betaparttype,$
					minMfms=minMfms,manualPlotting=manualPlotting,nofixing=nofixing,maxover=maxover

clockOMP=TIC("overVmachPlotter")
	dire="Documents/Plots/CombinedPlots/"
	FILE_MKDIR,dire
	doClose=1

	if keyword_set(isLast) then doClose=0

	allowfix=1
	if keyword_set(nofixing) then allowfix=0
	if not keyword_set(betaparttype) then betaparttype=2

	;;;;beta partition types:
	;;;;;; 1: beta>1 or beta<=1
	;;;;;; 2: beta>2 or beta<=2
	;;;;;; 3: beta>=upperquartile,beta<=lowerquartile


	firstrun=0
	get_data,'Mfms',data=dat
	if size(dat,/typ) eq 2 then firstrun=1

	;;;;;;m<0: beta>= (sort(beta))[(1+1./m)*N-1],beta< (sort(beta))[(-1./m)*N-1] <--TO BE IMPLEMENTED

	killBigFM=0
	killNegOverDiff=0
	if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	if keyword_Set(RemoveBigFracM) then killBigFM=1
	if not keyword_set(maxAllowedFM) then maxAllowedFM=10 else killBigFM=1
	;if not keyword_set(maxUpDev) then maxUpDev=5
	if not keyword_set(maxUpDiff) then maxUpDiff=1.3
	if not keyword_set(maxB2B1fracdiff) then maxB2B1fracdiff=.6
	if not keyword_set(maxMagJump) then maxMagJump=7.
	if not keyword_set(mindop) then mindop=.4
	if not keyword_set(minMfms) then minMfms=1.0
	if not keyword_set(betamaxallowed) then betamaxallowed=20.;9.;20.
	if not keyword_set(maxover) then maxover=5.
	;first get names of tplot files from txt file

	;H=0
	corrText=""
	biglistnum=""
	if keyword_set(load2) then biglistnum="2"
	srctxt="Documents/overmachDays.txt"


	if keyword_set(load2) then srctxt="Documents/overmachDays2.txt"
	if killBigFM or killNegOverDiff then corrText="_WithoutPathologies"
	read_strDATA,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)

	;numDates=numel(dates)
	
	manualList="Documents/savedPoints.txt"
	print,manualList
	read_strDATA2,shocklist,manualList,numLines=numLines2
;	read_strDATA2,shocklist,"Documents/savedPoints_withoutPathologies.txt",numLines=numLines2
	shocklistanom=strarr(numLines2)

	for i=0,numLines2-1 do shocklistanom[i]=(strsplit(shocklist[i],";",/extract))[0] 

	shocklist2=shocklistanom
	instructlist=strarr(numLines2)
	;for i=0,numLines2-1 do if shocklist2[i].startswith('2') then shocklist2[i]='T@'+shocklist2[i]
	for i=0,numLines2-1 do begin
		if shocklist2[i].startswith('2') then instructlist[i]='G' else begin
		;if ~shocklist2[i].startswith('2') then begin
			var=(strsplit(shocklist2[i],"@",/extract)) ;shocklist2[i].remove(0,0)
			if numel(var) ne 2 then print, var
			shocklist2[i]=var[1]
			instructlist[i]=var[0]
		endelse	
	endfor
	;in for loop, restore these files and create an array of data structures

	tmins=list()
	tmaxs=list()
	datasets=list()
	seasons=list()

	;{maxlocs:GG,t:shocksUnix,mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,$
		;B2B1_Fit:B2B1_Fit[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
	;shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD,pos:POS[GG,*],FF:datff.y,Alfven:datAlfven.y[GG],$
	;uindices:uis[GG,*],dindices:dis[GG,*],Cs:Cs[GG],flowangle:ThetaVB[GG],imins:datff.imins[GG],imaxs:datff.imaxs[GG],N_p:upNp,N_e:upNe}
	locsLst=list()	
	tLst=list()
	tjLst=list()
	xtLst=list()
	tjdLst=list()
	tdays=list()
	pointlist=list()

	pltpos=[0.15,0.25,0.95,0.8]
	cbpos=[0.30,0.07,0.70,0.15]
	allbins=list()
	qallbins=list()
	pallbins=list()
	allspbins=list()
	allsubins=list()
	allaubins=list()
	allwibins=list()


	
	maxFM=0.
	maxFMplot=0.
	maxFMplotDate=""
	maxViableFM=0.

	maxViableBeta=0.
	maxBetaPlot=""

	minDiff=0.
	minDiffplot=0.
	minDiffplotDate=""

	maxFUPDiff=0.
	maxFUPfm=0.
	maxFUPDiffplot=0.
	maxFUPDiffplotDate=""
	;maxUPDiff=0.5
	maxUPDiffplot=0.
	maxUPDiffplotDate=""
	maxUPfm=0.

	maxDU=0.
	maxDUplot=0.
	maxDUplotDate=""

	goodPoints=0
	points=0

	upintervaloffsetlst=list()
	downintervaloffsetlst=list()

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
	badDens=0
	badNANs=0
badInstab=0
	BadMultis=0
	badCrusts=0
	badSlope=0
	inOvers=0
	noOvers=0
	wrongBmaxs=0
	nonlocals=0
	missedanomolies=0
	missedmanual=0
	feet=0
	clockld = TIC('load')

	badthetaVals=list()
	badthetaManVals=list()
	badthetaAutoVals=list()

	ssnbound=list()
	ssn=-1
	tv=-1
	;tjv=-1
	ssnchanges=0
	wherebaddist=list()

	R_mars=3389.5
	semimajor=227.956 *10.^6.
	ecc=0.0935
	semilat=semimajor*(1-ecc^2)


	numnoconic=0

	for i=0,numPlots-1 do begin
		print,H[i]
		error_status=0
		if i gt 0 and H[i] eq H[i-1] then continue

		;tplot_restore,filename="Documents/overVsMachData2/"+H[i]
	

		;name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		;get_data,name,data=dat
		name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		get_data,name,data=dat
		;catch,error_status
		;	if error_status ne 0 then begin
		;	print,i
		;	print,H[i],"doesn't exist ERROR"

		;		catch,/cancel
				;continue
		;endif
		if size(dat,/typ) eq 2 then begin
			
			tplot_restore,filename="Documents/overVsMachData"+biglistnum+"/"+H[i]
	
			
			name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))

			get_data,name,data=dat

			if size(dat,/typ) eq 2 then continue

		endif
		;if size(dat,/typ) eq 2 then continue	
		datasets.add,dat
		date=name.remove(0,10)
		t00=time_double(date)
		;help,dat
		if dat.t[0] ge time_double('2019-11-16/00:00:00') or H[i] eq 'OverVsMach_2019-11-16.tplot' or t00 ge time_double('2019-11-16/00:00:00') then break

		;newBins=medianator(dat)
		;allbins.add,newBins[0]
		;print,transpose(newBins[0])
		;print,(newBins[0])[0,*]
		;z=newBins[10]
		;if numel(newBins[1]) gt 0 then  allspbins.add,newBins[1]
		;;print,"newBins=",newBins
		;;print,newBins[10]
		;if numel(newBins[2]) gt 0 then  allsubins.add,newBins[2]
		;if numel(newBins[3]) gt 0 then allaubins.add,newBins[3]
		;if numel(newBins[4]) gt 0 then allwibins.add,newBins[4]

		;qnewBins=medianator(dat,/qua)
		
		;qallbins.add,qnewBins[0]
		;if numel(qnewBins[1]) gt 0 then  qallspbins.add,qnewBins[1]		
		;if numel(qnewBins[2]) gt 0 then qallsubins.add,qnewBins[2]
		;if numel(newBins[3]) gt 0 then qallaubins.add,qnewBins[3]
		;if numel(newBins[4]) gt 0 then qallwibins.add,qnewBins[4]




		;pnewBins=medianator(dat,/par)
		
		;pallbins.add,pnewBins[0]
		;if numel(pnewBins[1]) gt 0 then  pallspbins.add,pnewBins[1]		
		;if numel(pnewBins[2]) gt 0 then pallsubins.add,pnewBins[2]
		;if numel(newBins[3]) gt 0 then pallaubins.add,pnewBins[3]
		;if numel(newBins[4]) gt 0 then pallwibins.add,pnewBins[4]
		;print,"filename=",H[i]
		;foreach el, newBins do ;print,"el=", el
		dayCrosses=0
		tday=-1
		if numel(dat.t) ne numel(dat.shock0dist) then begin
			print,"BAD dataset at ",H[i]
			wherebaddist.add,H[i]+" ## numel(dat.t),numel(dat.shock0dist)="+strtrim(numel(dat.t),2)+","+strtrim(numel(dat.shock0dist),2)
			continue

		endif

		str_element,dat,'N_SN',var,success=NSNexists

		str_element,dat,'pos_conic',var,success=PSexists

		;catch,error_staus

		if 0 and error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			help,dat
			del_data,'*'
			continue	

		endif

		str_element,dat,'Cs',var,success=Cexists
		if ~Cexists then begin
			Cspeed=Sqrt(dat.betas) * dat.Alfven
			str_element,dat,'Cs',Cspeed,/add
		endif
		str_element,dat,'mfms',var,success=Mexists
		if ~Mexists then begin
			print,H[i]
			help,dat
		endif

		str_element,dat,'T_ion',var,success=Texists
		if ~Texists then begin
			print,time_string(dat.t[0])
			print,H[i]
			print,t00
			print,time_double('2019-11-16/00:00:00')
			print,t00 ge time_double('2019-11-16/00:00:00')
			help,dat
			del_data,name
			continue;break
			;return
		endif

		if 0 and dat.t[0] le time_double('2017-04-14') then begin
			print,"goodpoint/points=",goodPoints,"/",points
					print,"[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedAnomolies]:"
			print,[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedanomolies]
			del_data,name
			continue;break
		endif	
		if 0 and numel(dat.t) eq 1 and H[i] ne 'OverVsMach_2015-07-14.tplot' then begin
			print,H[i]
			;return
		endif

		if ~PSexists then begin
			print,H[i]
			PRINT,numnoconic
			help,dat
			noconiclst.add,H[i]
			numnoconic++
			del_data,name
			continue;break

		endif
	
		

		;if   t00 gt time_double('2017-01-23')+1 then break
		for j=0,numel(dat.t)-1 do begin
			badNAN=0
			bdmanpoint=0
			badPoint=0
			points++
			badkey=''

			t=dat.t[j];shockUnix[j];dat.t[j]
			if j gt 0 and numel(tLst) gt 1 and total(tLst eq t) gt 0 then continue
			xt=x2Greg(t,/strformat)
			;print,xt
			Ls=dat.Ls[j]
			;print,j
			;print,numel(dat.t)
			;print,size(dat.Ls,/n_el)
			;print,size(Ls,/typ)
			;print,Ls

			;lll=dat.Ls
			;seasons.add,marsSeasonFinder(lll,dat.lat)
			;sss=marsSeasonFinder(lll,dat.lat)
			listloc=(where(xt eq shocklist2))[0]
			if   listloc eq -1 then begin
				badManual++
				badkey=','
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr=;"
				continue
			endif

			if 0 and (dat.N_p[j] lt 0 or dat.N_p[j] gt 1000) and finite(dat.N_e[j]) eq 1 then begin
				print,xt
				;return
				continue
			endif
			anomtext=shocklistanom[listloc]
			instruct=instructlist[listloc]
			print,instruct
			instruct0=instruct

			if instruct.StartsWith('N')  then begin

				badNANs++
				;badNAN=1
				badPoint=1
				badkey='N'
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif

			if ~allowfix and instruct.CharAt(0) ne 'G' and instruct.CharAt(0) ne 'A' then begin
				print,'no fix'
				if instruct.strlen() eq 2 then begin
					print,'instruct=instruct.CharAt[1]'
					print,instruct
					instruct0=instruct
					instruct=instruct.CharAt(1)
					if instruct eq 'B' then begin
						badManual++
						badPoint=1
						badkey=','
						badPoint=1
						tlst.add,t
						pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
						continue
					endif
				endif else begin
						badManual++
						badPoint=1
						badkey=','
						badPoint=1
						tlst.add,t
						pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
						continue
				endelse

			endif

			if instruct.StartsWith('R')  then begin
				badManual++
				badMultis++
				;badNAN=1
				badPoint=1

				badkey=',R'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif

			if instruct.StartsWith('C')  then begin
				badManual++
				badCrusts++
				;badNAN=1
				badPoint=1
				badkey=',C'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif

			if instruct.StartsWith('I')  then begin
				badManual++
				badInstab++
				
				badPoint=1
				badkey=',I'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue
			endif





			if instruct.StartsWith('V')  then begin
				badManual++
				badSlope++
				;badNAN=1
				badPoint=1
				badkey=',[slope]'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif
			if instruct.StartsWith('O')  then begin
				badManual++
				inOvers++
				;badNAN=1
				badPoint=1
				badkey=',[over]'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif
			if instruct.StartsWith('Q')  then begin
				badManual++
				noOvers++
				;badNAN=1
				badPoint=1
				badkey=',[smallOver]'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif
			if instruct.StartsWith('L')  then begin
				badManual++
				nonlocals++
				;badNAN=1
				badPoint=1
				badkey=',[nonlocal]'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif
			if instruct.StartsWith('F')  then begin
				badManual++
				feet++
				;badNAN=1
				badPoint=1
				badkey=',[foot]'
				badPoint=1
				tlst.add,t
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue

			endif
			;uptime=t00+mean(dat.uindices[j,*])
			;upDT=abs(uptime-t)
			;downtime=t00+mean(dat.dindices[j,*])
			;downDT=abs(downtime-t)


			mfms=dat.mfms[j]
			theta=dat.ANGLE[j]
			

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
			
			if downM eq 0 then begin
				print,"downM=0"

			endif

			if upM eq 0 then begin
				print,"upM=0"

			endif

			dstd=dat.downSTD[j]
			ustd=dat.upSTD[j]
			;print,"downM=",downM
			;print,"upM=",upM
			;mxFM=max(dat.mfms[j])
			crit=dat.crits[j];calccritmachnumber(theta,bta);dat.crits[j]
			FM=mfms/crit
			N_e=dat.N_e[j]
			N_p=dat.N_p[j]
			Tion=dat.T_ion[j]
			Beta_ion=dat.beta_ion[j]
			Telec=Tion*(bta/Beta_ion -1.)
			if Telec eq 0 or finite(Telec) ne 1 then begin
				print,"Telec=",Telec

			endif
			if  fracdiff(downup, down/up) gt 0.001 then begin
				print,xt
				print,"downup=",downup
				print,"down/up=",down/up
				print,fracdiff(downup , down/up)
				print,"Mfms=",mfms
				print,"beta=",bta
				badkey+='[downup]'
				badPoint=1;continue
				;return
			endif


			if mfms lt minMfms then begin
				badMfms++
				print,"badMfms,",mfms,xt
				;print,"badFMs"
				badkey+='M'
				badPoint=1
				;tlst.add,t
				;pointlist.add,'-'+badkey+'-'+xt
				badPoint=1;continue
			endif
			T_rat=1.0 * Telec/Tion
			if finite(N_e) ne 1 or N_e le 0 or N_p le 0 or N_e /N_p lt .1 or N_e /N_p gt 10 or T_rat lt 0  then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badDens++
				badkey+='[Dens]'
				badPoint=1;continue
			endif

			
			if dist0 gt 2500 then begin
				badkey+='[dist0]'
				;print,"dist0=",dist0
				baddists++
				badPoint=1
				 ;continue
			endif

			if dop lt mindop then begin
				;print,"mindop=",dop
				badkey+='.'
				baddots++
				badPoint=1;continue
			endif
			if bta gt betamaxallowed or Beta_ion gt betamaxallowed/2.  then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				badkey+='[beta]'
				;PRINT,Ls
				

			endif
			if finite(bta) ne 1 or bta le 0. then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				badkey+='[beta]'
				;PRINT,Ls
				

			endif
			if size(Ls,/typ) eq 0 then begin
				;print,xt
				;print,H[i]
				badLS++
				;print,"Bad Ls"
				badkey+='[Ls]'
				badPoint=1
	
			endif
			if finite(dat.ANGLE[j]) ne 1 then badPoint=1
			if FM le 0.0 then begin
				badFMs++
				badkey+='[FM]'
				;print,"badFMs"
				badPoint=1;continue
			endif
			if (killBigFM) and (FM ge maxAllowedFM) then begin
				badFMs++
				badkey+='[FM]'
				;print,"badFMs"
				badPoint=1;continue
			endif

			if instruct.StartsWith('M')  then begin
				down=downM
				up=upM

			endif
			if instruct.StartsWith('U') then begin

				up=upM

			endif

			if instruct.StartsWith('D') then begin

				down=downM

			endif
			downup=down/up
			if ((downup ge maxMagJump or downM/upM ge maxMagJump or (downup lt 1.5 and down-up lt 1.3)) and ~instruct.StartsWith('Y')) OR downup eq 0  then begin
				badPoint=1
				badB2B1++
				badkey+='[B2B1]'

			endif

	

			;if upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd then begin;

			if xt eq '2014-11-30T01:59:43,5Z' then begin
				print,"[up,upM,down,downM,dum,downup]=",[up,upM,down,downM,dum,downup]
				print,'abs(upM-up)=',abs(upM-up)
				print,'max([3*ustd,maxUpDiff])=max(['+strtrim(5*ustd,2)+','+strtrim(maxUpDiff,2)+'])='+strtrim(max([5*ustd,maxUpDiff]),2)
				print,'fracdiff(up,upM)=',fracdiff(up,upM)

				print,"fracdiff(dum,downup)=",fracdiff(dum,downup)
				print,"fracdiff(downM,down)=",fracdiff(downM,down)
				print,duRatio
				print,"abs(down-downM)=",abs(down-downM)
				print,strtrim(3*dstd)
				;return
			endif

			if ((abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd) and ~instruct.StartsWith('Y') then begin;or abs(downM-down)/mean([downM,down])  gt .3 or duRatio gt 3 or duRatio lt .33 or fracdiff(dum,downup) gt maxB2B1fracdiff then begin
				badMeasure++
				;print,"bad measure"
				;print,H[i]
				badPoint=1
				badkey+='[MEAS]'
			endif 

			

			if ((killNegOverDiff) and ((Bmax lt down ) or (Bmax lt up) or (Bmax lt downM) ) or (Bmax-down)/down gt maxover ) and ~instruct.StartsWith('Y')  then begin
				badBmax++
				badkey+='[Bmax]'
				;print,"badBmax"
				badPoint=1;continue
			endif

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
	

			listloc=(where(xt eq shocklist2))[0]
			if  0 and listloc eq -1  and not keyword_set(keepPathologies) then begin
				badManual++
				if badPoint eq 0 then missedmanual++
				badPoint=1
				bdmanpoint=1
				;continue
			endif


			tj=x2jul(t)
			if tj le 0 then begin
				print,t
				print,tj,"<=0"
				print,H[i]
				print,xt

				return
				badPoint=1
			endif

			if instruct.StartsWith('A')   then begin
				badPoint=1
				missedanomolies++
				badkey+='A'
				
			endif


			tlst.add,t

			if badPoint   then begin
				pointlist.add,'-'+badkey+'-'+xt+" :: instr="+instruct0
				continue
			endif 
			pointlist.add,'+'+badkey+'+'+xt+" :: instr="+instruct0
			goodPoints++

		endfor
		
		print,"goodpoint/points=",goodPoints,"/",points
		
		print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
		print,badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens,format='(I7,",",I8,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
		print,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
		print,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'

		if size(LsLst,/typ) eq 0 then begin
				print,xt
				print,H[i]
				;badPoint=1
	
		endif
		del_data,'OverVsMach*'
		;seasons.add,marsSeasonFinder(dLs,dat.lat)
	endfor
	TOC,clockld
	print,"finished binning data"

	openW,1,'Documents/pointDecisions.txt'
	foreach el,pointlist do printf,1,el
	printf,1,'======================================='
	printf,1,"goodpoint/points=",goodPoints,"/",points
	;printf,1,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,missedAnomolies,BadManual,missedManual,badCrusts,badMultis]"
	;printf,1, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,missedanomolies,BadManual,missedmanual,badCrusts,badMultis]
print,"goodpoint/points=",goodPoints,"/",points
		
	printf,1,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
	printf,1, badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens,format='(I7,",",I8,",",I7,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
	printf,1,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
	printf,1,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'
	if numel(wherebaddist) ne 0 then begin
		printf,1,'======================================='
		foreach el,wherebaddist do printf,1,el
	endif
	close,1
end
