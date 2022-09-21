


;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------

;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------
;------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------------------------PROCEDURE--------<><><><><><><><>-------------



pro curveFitter5, plt , newName=newName, curveType=curveType , doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom,minM0=minM0,allownonfore=allownonfore,shocktest=shocktest
	clock=TIC(plt)
	g0=0
	nonfore=0
	if NOT KEYWORD_SET(newName) THEN newName=newName+'_smoothed'
	if NOT KEYWORD_SET(minChi) THEN minChi=1.20
	if NOT KEYWORD_SET(minM0) THEN minM0=0.7
	if keyword_set(allownonfore) then nonfore=1
	rollup=0
	rollsec=0
	rollthird=0
	rollnone=0
	;dobug=0
	minRemain=100
	if not KEYWORD_SET(Debug) THEN dobug=0 else dobug =Debug
	maxUp=1.5
	minM1=1./(10*40.);.04
	;minM0=0.0
	upDev=2.2
	error_status=0


	curveorders,rollup,rollsec,rollthird,rollnone,orderCustom=orderCustom,doRoll=doRoll,doSecond=doSecond,doThird=doThird,doDefault=doDefault
	;if KEYWORD_SET(orderCustom) then begin
		;if Total(orderCustom eq 1) gt 0 then rollup=1
		;if Total(orderCustom eq 2) gt 0 then rollsec=1
		;if Total(orderCustom eq 3) gt 0 then rollthird=1
	;endif else begin
		;orderCustom=list()

		;if KEYWORD_SET(doRoll) THEN begin
			;rollup=1
			;orderCustom.add,1
		;endif
		;if KEYWORD_SET(doSecond) THEN begin
			;rollsec=1
			;orderCustom.add,2
			
		;endif
		;orderCustom.add,0
		;if KEYWORD_SET(doThird) THEN begin
			;orderCustom.add,3
			;orderCustom.add,4
			;rollthird=1                
		;endif
		;if numel(orderCustom) gt 3 then begin
			;orderCustom.add,4
			;orderCustom.swap,-1,-2
		;endif
		;if numel(orderCustom) gt 4 then begin
			;orderCustom.add,5
			;orderCustom.swap,-1,-2
		;endif
		;orderCustom=orderCustom.toArray()
	;endelse
	doPerturb=0
	if KEYWORD_SET(secondOrder) THEN doPerturb=1                
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"
	typ=2
	get_data,plt,data=dat,limits=limits
	;get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
print,"=curveFitter5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=curveFitter5="
print,"newName=",newName,", rollup=",rollup,", rollsec=",rollsec
print,"=curveFitter5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=curveFitter5="
	ys[where(finite(ys) eq 0)]=0

	xs=dat.x
	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	mids=fltarr(N)
	shocks=fltarr(N)
	z=fltarr(N)
	insublengths=list()
	outsublengths=list()

	brokenin=list()
	brokenout=list()

	inSubsets=fltarr(N);-1
	inends=fltarr(N);-1
	inbegs=fltarr(N);-1

	outSubsets=fltarr(N);-1
	outends=fltarr(N);-1
	outbegs=fltarr(N);-1

	debugout=fltarr(N)-1


	inimaxs=fltarr(N)-1
	outimaxs=fltarr(N)-1

	inimins=fltarr(N)-1
	outimins=fltarr(N)-1

	inchis=fltarr(N);+0
	outchis=fltarr(N);+0
	chis=fltarr(N)

	innums=fltarr(N)-1
	outnums=fltarr(N)-1
	
	inshocklocs=fltarr(N)-1
	outshocklocs=fltarr(N)-1
	
	inups=fltarr(N)
	indowns=fltarr(N)

	outups=fltarr(N)
	outdowns=fltarr(N)

	inparallel=fltarr(N)
	outparallel=fltarr(N)

	MMMs=nanarr(N,4)
	nMMMs=nanarr(N,4)

	inalgshock0locs=fltarr(N)-1
	outalgshock0locs=fltarr(N)-1
	inalgshock1locs=fltarr(N)-1
	outalgshock1locs=fltarr(N)-1

	if 1 then begin
		if dobug gt 0 then print,"type  2: curve fit"
		;.run tanhfit
		;get_data,'ascend_end',data=datae
		;get_data,'descend_end',data=datde

		;get_data,'ascend_begin',data=datab
		;get_data,'descend_begin',data=datdb

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon
		get_data,'regid_cleaned_plot_interpolated',data=datReg
		get_data,"B_medianderiv_extrema",data=datEx

		get_data,'B60derex',data=datderex

		get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_mean',data=datmm

		get_data,"foot_start_inbound",data=datfsi
		get_data,"foot_start_outbound",data=datfso

		derex=datderex.y

		for i=0,N-1 do if derex[i] le 0 then derex[i]=!values.F_NAN

		nderex=REVERSE(derex)


		ymm=datmm.y
		nymm=reverse(ymm)




		get_data,"B_3sec",data=datB3
		B3=datB3.y
		nB3=REVERSE(B3)

		get_data,"B_up_average",data=datBU
		BU=datBU.y
		nBU=REVERSE(BU)

		extr=datEx.y
		nextr=(-1)*REVERSE(extr)
		mono=datMon.y
		bmono=datbMon.y
		reg=datReg.y

		get_data,'B5-3d10',data=datB5d
		get_data,'B20deriv',data=datB20deriv

		B5d=datB5d.y
		B20d=datB20deriv.y

		;if dobug gt 0 then print,"numel(B5d)=",numel(B5d)
		get_data,"B_sec_smoothed" ,data=datBSb
		get_data,"B_sec_smoothed_back" ,data=datBS
		get_data,"B_median",data=datBmed
		Bmed=datBmed.y
		;oyae=datae.y
		;oxae=datae.x
		get_data,"rho30deriv",data=datRhod ; we want this data to avoid using crustal fields for downstream data:
		; if rho60 lt 1 then  few particles                                   
		get_data,"B_sec_smoothed_backderiv",data=datBsbd
		get_data,"B_sec_smoothedderiv",data=datBsd

		get_data,"PE_halfderiv_maxima",data=datPhm

		phm=where(datPhm.y ge 25,ppn)
		nphm=where(reverse(phm) le 25,nppn)

		rhod=datRhod.y

		Bmed=datBmed.y
		yBS=datBS.y
		yBSb=datBSb.y

		Bsd=datBsd.y
		Bsbd=datBSbd.y
		;oydb=datdb.y
		;oxdb=datdb.x
		starti=0
		;oyde=datde.y
		;oxde=datde.x

		;oyab=datab.y
		;oxab=datab.x

		yBS=datBS.y
		yBSb=datBSb.y

get_data,"proton_cyclotron_period",data=datTau
		tau=datTau.y
		ntau=reverse(tau)

	;------------------
		

		;ydb=fltarr(N)
		;yde=fltarr(N)
		;yab=fltarr(N)
		;yae=fltarr(N)
		;for i=0, N-1 do yae[i]=0
		;for i=0, N-1 do yde[i]=0
		;for i=0, N-1 do yab[i]=0
		;for i=0, N-1 do ydb[i]=0
		;print,total(yae)
		;print,total(ydb)
	;	print,"yae is non-finite at:",where(finite(yae) eq 0)
	;	print,"yde is non-finite at:",where(finite(yde) eq 0)
	;	print,"yab is non-finite at:",where(finite(yab) eq 0)
	;	print,"ydb is non-finite at:",where(finite(ydb) eq 0)
	;	print,"total(yae)=",total(yae)
	;	print,"total(ydb)=",total(ydb)
	;	print,"total(yab)=",total(yab)
	;	print,"total(yde)=",total(yde)


		;nl=size(oyae,/n_el)
		;nd=size(oydb,/n_el)
		;"variables that would otherwise be defined in loops"
		;print,N,nl,nd
		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		trust=0
		
		;if dobug gt 0 then	print,"numel(B5d)=",numel(B5d)
		;get_data,'ascend_end_refined',data=datae
		;get_data,'ascend_begin_refined',data=datab
		;get_data,'descend_end_refined',data=datde
		;get_data,'descend_begin_refined',data=datdb
		;yae=datae.y
		;yab=datab.y
		;yde=datde.y
		;ydb=datdb.y

		;DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		;AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		;CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		;BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
		get_data,'mvn_swe_spec_dens_interpolated',data=datne
		n_e=datne.y
		nn_e=reverse(n_e)


		if not keyword_set(shocktest) then begin
		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		endif else begin
 		get_data,"shock_guess_inbound",data=datAA
		get_data,"shock_guess_outbound",data=datbAA
		endelse
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'DD_effective_flag',data=dateDD
		get_data,'bDD_flag',data=datbDD
		get_data,'bDD_effective_flag',data=datebDD
		
		get_data,"forecountdown",data=datFore

		Fore=datFore.y

		nFore=reverse(Fore)
	;	

		


		
		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		eDD = WHERE(dateDD.y ne 0, eddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]


		ifoots=WHERE(datfsi.y ne 0, ifn)
		ofoots=WHERE(reverse(datfso.y) ne 0, ofn)


		AA = WHERE(datAA.y ne 0, aan); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn); end of  negative  shock


		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
		enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn); end of  negative  shock

;	if dobug gt 0 then 	PRINT,"[ddn,aan,bbn]=",[ddn,aan,bbn]
	;if dobug gt 0 then 	print,"numel(B5d)=",numel(B5d)
		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD
		foremaxs=datFore.ymax[AA]
		nforemaxs=(REVERSE(datFore.ymax))[nAA]
	;------------------SHORT CIRCUITING NONCROSSING PERIODS HERE
	zf=fltarr(N)
	if DD[0] eq -1 then begin
		zf=fltarr(N)
		store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
		store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:"abs(REDUCED CHISQ-1)"}
		store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:"abs(REDUCED CHISQ-1)"}
	;	store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks',data={x:xs,y:zf,ytitle:'flag'}
		
 		dat.y=fltarr(N)
		;print,size(dat)
		;;help,dat

		store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs,			outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks,inMMs:MMMs,outMMs:MMMs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}
		store_data,newName+"_inbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inparallel:inparallel,MMs:MMMs,inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs}
		store_data,newName+"_outbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", outchis:outchis,outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks,outparallel:outparallel,MMs:MMMs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}

;		store_data,'shocks'+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}

;		store_data,'shocks'+rl+'_outbound',data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks_outbound',data={x:xs,y:shocks,ytitle:'flag'}

		store_data,'sublengths_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

		store_data,'sublengths_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
		store_data,'sublengths_inbound_end',data={x:xs,y:inends,ytitle:'flag'}

		store_data,'sublengths_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}

		store_data,'sublengths_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
		store_data,'sublengths_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

		return

	endif





	;------------------
		;"variables that would otherwise be defined in loops"

		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		;trust=0
		g0=0;
		b0=0
		;DD = WHERE(oyae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		

		;AA = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)
		
		;counterinutitively, will want to curve fit ys[0:DD[0]] after the rest

		;need to determine if we have the whole curve or not

		;iscut=1*(ddn ne aan) ; 
		;bcutoff=1*(DD[0] le AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?
		fooffsetf=1*(ifoots[0] gt AA[0])



		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,"a offsetf=",aoffsetf
;		print,"d offsetf=",doffsetf
		;print, "bcutoff: ",bcutoff
		;print, "fcutoff: ",fcutoff
;print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0

		yBSfdiff=2*ABS(yBS-ymm)/abs(yBS+ymm)

;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
print,"==================================================================="
print,"5			inbound side				 5"
print,"==================================================================="

		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
fullIter=min([ddn,aan,bbn])
		lastmax=0
;print,ifoots
		for i=1,fullIter-1  do begin
print,"FORWARDFORWARDFORWARDFORWARDFORWARDFORWARDFORWARD"
			cont=0
			shks=shocks
			sublengths=insublengths
			Subsets=inSubsets
			begs=inbegs
			ends=inends
			brokens=brokenin
			test=-1

			chi=0
			imx=-1
			imn=-1
			shockloc=-1
			Bmin=0
			Bmax=0

			doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?


			foffsetf=1*(ifoots[0] gt AA[0])


			backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
			if numel(BB) gt 1 then boffsetb=1*(BB[1] lt DD[0]) else boffsetb=0
			if numel(AA) gt 1 then aoffsetb=1*(AA[1] lt DD[0]) else aoffsetb=0
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf +aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]

			error_status=0
			catch,error_status
			if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[AA[j]]
print,'ERROR in curveFitter5 during i=',i
				errorloc=""
			
				errmsg=["error in curveFitter5 inbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel

				continue
			endif
			;MMM=nanarr(4)
			;;help,Fore
			zl=fitloop(i, xs,ys,z,AA,BB,CC,DD,rollup,rollsec,mono,B5d,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc,Bmin,Bmax,B20d,rollthird,minM1,orderCustom,$
maxUp,Bmed,rhod,Bsd,extr,eDD,Fore,BU,B3,ymm,minM0,ifoots,foremaxs,phm,inparallel,derex,n_e,allownonfore=allownonfore,MMM=MMM,algshock0=algshock0,algshock1=algshock1)
			;if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			z=zl
			shocks=shks
			inSubsets=Subsets
			insublengths=sublengths
			inbegs=begs
			inends=ends
			brokenin=brokens

			for kk=imn,imx do begin
				inshocklocs[kk]=shockloc

				inimaxs[kk]=imx
				inimins[kk]=imn
				inchis[kk]=chi
				innums[kk]=i

				inups[kk]=Bmin
				indowns[kk]=Bmax
				inalgshock0locs[kk]=algshock0
				inalgshock1locs[kk]=algshock1
				for ll=0,3 do MMMs[kk,ll]=MMM[ll]
			endfor
		endfor
	 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=BB[0]
		if DD[0] lt BB[0] then d0=DD[0] else d0=0
		edim=d0
		yp=ys[0:b0]
		xp=xs[0:b0]
		np=size(yp,/n_el)
		

		aj=AA[0]
		ishock=aj
		isFinite=total(finite(yp) ne 1) eq 0;(;total(finite(ys[d0:b0])) ne 0) and (total(finite(ys[d0:aj])) ne 0) and (total(finite(ys[aj:b0])) ne 0)
		errorloc=""
		error_status=0
		catch,error_status
		if error_status ne 0 then begin
;print, 'Error index: ', error_status
;print, 'Error message: ', !ERROR_STATE.MSG
;print,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			xaaa=xs[ishock]
			errmsg=["error in curveFitter5 inbound 0 while producing "+newName, 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,'ERROR WHEN PERFORMING '+ errorloc]
			errorsaver,xaaa,errmsg,/spec
			errorloc=""
			catch,/cancel
		endif
		;"start with standard case"

		if error_status eq 0 then begin
		while (aoffsetf eq 0) and (np gt 4) and (AA[0] ne -1) and isFinite do begin;then begin
print,"=D=D=D=D=D=D=D=D-D=D=D=D=D=D=D=D=D=D=D=D=D"
			i=0
print,"now for the range ys[0:BB[0]]=ys[0:",b0,"]"
			aj=AA[0]
			;print,gi
			;print,"gim=",gim
			dim=d0

			regp=reg[dim:b0]
			mxfore=foremaxs[0]

			n_ep=n_e[dim:b0]
			if total(finite(n_ep) eq 1) eq 0 then break

			if (min(regp) eq 1) and regp[0] ne 1 then begin
				;while (regp[0] gt 1) do begin
				if min(reg[dim:b0-1]) gt 1 then break

				ndim=dim+(where(reg[dim:b0] eq 1))[0]
				if ndim+2*60 gt aj then break else begin 
					if dim +1 eq aj then break
					;dim++
					dim=ndim
					regp=reg[dim:b0]
					;regp=regp[1:*]
					xp=xs[dim:b0]
					yp=ys[dim:b0];yp[1:*]
				endelse;while
			endif
			wherefoot=(where(ifoots lt aj and ifoots ge dim))[-1]
			;foot=ifoots[()[0]]

		;	if wherefoot eq -1 then foot=dim else foot=ifoots[wherefoot]
			if wherefoot eq -1 then break else foot=ifoots[wherefoot]
			bfoot=max([dim,foot-10*60]);120])
			d00=dim
			dim=bfoot

			if total(finite(n_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(n_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif
			if ymm[dim] le 0 or finite(ymm[dim]) ne 1 then begin
				KK=where(ymm[dim:aj] gt 0,kcount)
;print,"KK[0],dim,KK[0]+dim,foot=",KK[0],dim,KK[0]+dim,foot
				if kcount eq -1 then break
				if wherefoot ne -1 and kcount ne -1 then begin
					LL=where(xs[dim:aj] lt xs[foot])
					SS=INTERSECT(KK,LL)
;print,"SS[0]=",SS[0]
;print,"dim,SS[0]+dim,foot=",dim,SS[0]+dim,foot
					if SS[0] ne -1 and SS[0] gt 60 then dim=SS[0]+dim else begin
						break
					endelse
				endif else if kcount ne -1 and KK[0] gt 60 then dim=KK[0]+dim

			endif
			if 0 and total(ymm[b0] le ymm[dim:b0-1]) eq 0 then begin

				while total(ymm[b0] le ymm[dim:b0-1]) eq 0 and b0-aj gt 3*60 and B20d[b0] ne 0 do b0--

			endif


			pcross=max([phm[(where(phm lt min([b0,foot+1.5*60*60]) and phm gt foot))[0]]-30,foot])
			;dim=(bfoot*2+dim)/3;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			;print,"g0=",b0
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			

			;CRUSTST=where((Bsd[aj+1:b0] gt 1.0) and (rhod[aj+1:b0] lt 0),ccount)+aj+1
			;if ccount gt 0  then begin
				;b0=CRUSTST[0]
				;yp=ys[dim:b0]
				;np=numel(yp)
				;zn=z[b0:*]
			;endif


			yend=ys[aj:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			brefines=0
			frefines=0
	

			mbeg=nbeg
			mend=nend
		


			incfunctions=1
			decfunctions=1
			bi=b0
			d00=dim
			;	jbegj=max([1,nend-20])+aj
			;	jendj=min([N-1,nend+20+aj])
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 	
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
			;	;jbegj=max([1,nend-20])+aj
			;	jendj=min([N-1,nend+20+aj])
			;	if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
			;		;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where aj=",aj,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
			;		yp=yp[1:*] ;shrinks our range by one
 			;;;		ybeg=ybeg[1:*]
			;;		np--
			;;		nbeg--
			;		dim++
			;		brefines++
			;		if ( brefines ge 2 *mbeg/5) or (dim +1 eq aj) or (np le 3) or (nbeg le 3) then begin
			;			incfunctions=0
			;			dim=d00
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nbeg=mbeg
						
			;		endif
			;	endif
			;	if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
				;if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1))  then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
			;		yp=yp[0:np-2]
			;		np--
			;		yend=yend[0:nend-2]
			;		nend--
			;		bi--
			;;		frefines++
			;		if ( frefines ge 1 *mbeg/5) or (aj+1 eq bi) or (nend le 3) or (np le 3) then begin
			;			;print,"i=",i,", ddn=",ddn
			;			decfunctions=0
			;			bi=b0
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nend=mend
			;		endif
			;	endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			dim=d00
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=xs[dim:b0]
			yp=ys[dim:b0]
			if d00 ne 0 then edim=eDD[0]
			np=numel(yp)
			xpa=xp-xp[0]
			yc=yp*0.0
			;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; second order correction  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			zeron=aj-dim
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;mn=mean(ybegtrust)
			;aj=FalseShockSkipper(aj,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
			;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono)
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,B5d,dobug) ; second order correction 
			a00=aj
;print,"[dim,edim,aj,bi]=",[dim,edim,aj,bi]
			;help,Fore

			if nonfore then Fore[aj:min([bi,aj+20*60])]=1
			aj=ajRefiner(max([edim,foot,pcross]),max([edim,dim]),aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt ymm[bi] )  do bi--

			if total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) le 1 or total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( ymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) then while max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt max(ymm[bi-20:bi]) )  do bi--
			;while max(ys[aj-60:aj]) gt max(ys[aj:bi]) do aj--
			while max(ys[max([aj-60,foot]):aj]) gt max(ys[aj:bi]) and aj gt foot do aj--
			isparallel=0
			if total(yBSfdiff[aj+60:aj+60+4*60] lt .2)/120. gt .9 then begin

				maxfdiff=max(yBSfdiff[aj+60:bi-10],mxparfdloc)
				maxpar=max(yBS[aj+60:bi-10],mxparloc)
				if maxfdiff lt .4 and abs(mxparloc-mxparfdloc) lt 120 and min([mxparloc,mxparfdloc])+aj+60 gt aj+4*60 then begin
					bi=min([mxparloc,mxparfdloc])+aj+60-1
					isparallel=1
				endif
				
			endif

			mn=BU[aj];mean(ybegtrust)
			wblwu=where(ymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1
			ymmn=min(ymm[mean([aj,bi]):bi],ymmnlc)
			ymmnlc+=mean([aj,bi])
			ymmn2=min(ys[ymmnlc-40:ymmnlc+40],ymmnlc2)
			ymmnlc2+=ymmnlc-20
			if ymmnlc2 lt bi-1 and ymmn2 lt mn*.90+.1*mean(ymm[aj:bi]) then begin
				bi=ymmnlc2-1
				while ys[bi] lt mn*.9+.1*mean(ymm[aj:bi]) and bi gt aj+4*60 do bi--
			endif
			b0=bi
			;aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; third order correction 
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25
			xp=xs[dim:b0]
			yp=ys[dim:b0]
			xpa=xp-xp[0]
			;mx=mean(yendtrust)
			mx=mean(ymm[aj:bi]);25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			mn=BU[aj]
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			errorloc="following m2ish. numel(xpa)="+strtrim(np,2)+",[dim,aj,bi,aj-dim]= ["+strtrim(dim,2)+","+strtrim(aj,2)+","+strtrim(bi,2)+","+strtrim(aj-dim,2)+"]"
			m2=xpa[aj-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1.;.5;.1;1
			MM=[m0,m1,m2,m3]
;print,"to zeroth order, guess that MM=",MM
			MM0=MM
			CHISQ=-1
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-19
			;help,xpa
			;help,yp
			;help,weight
			;help,MM
			algshock0=aj
			algshock1=aj
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			MM1=MM
;print, 'Function parameters: ', MM,", status=",status
			;ishock=round(MM[2]/MM[1]) +dim
			imin=dim
			imax=bi
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
			;badsl=0
			working=1
			if ishock le foot or ishock ge bi then begin

				ishock=aj
				working=0
			endif else begin
				B20de=B20d[ishock:imax]
				pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

					     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])])) ))



				working=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30;(status eq 0) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)
			endelse
			if dobug eq 3 then begin
			print,1
;print,"**************************"
;print,"[imin,ishock,imax]=",[imin,ishock,imax]
;print,"[a00,aj,ishock]=",[a00,aj,ishock]
;print,"Fore[a00,aj,ishock]=",[Fore[a00],Fore[aj],Fore[ishock]]
;print,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
;print,"status eq 0=",status eq 0
;print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0
;print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
;print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),[<] maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
;print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
;print,"Fore[ishock]=",Fore[ishock]
;print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]), [<] upDev, success=",yfit[0],BU[ishock],upDev,abs(yfit[0]-BU[ishock]) lt upDev
;print,"~pointerinup=",~pointerinup
;print,"--------------------------"
;print,"working=,",working
;print,"**************************"
			endif

			if doPerturb then begin
				MM1=MM

				chi1=CHISQ
				yfit1=yfit
				working1=working
				ishock1=ishock
				;if ishock le foot or ishock gt bi then begin
					; ishock=aj

				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
				;endif
				smol=min(derex[ishock:bi],smoloc)

				imin=max([dim,foot-2*60]);ishock-7*60])
				if finite(smol) eq 1 and smoloc+120 gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])

				while (yBS[imin]-yBS[dim] gt 2)  and (imin gt imax+100) and imin ge foot-3*60 do begin
					imin--
					if yBS[imin]-yBS[dim] gt 10 then begin
						 ishock--
						imax--
					endif
				endwhile
;print,"(x)shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
;print,"(x)shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
;print,"[numel(yp),numel(ypp)]=",[numel(yp),numel(ypp)]
				xppa=xpp-xpp[0]	
			errorloc="defining smmp. numel(ymm)="+strtrim(np,2)+",[imin,ishock,.8*ishock+.2*imin]= ["+strtrim(imin,2)+","+strtrim(ishock,2)+","+strtrim(8*(ishock-imin)/10+imin,2)+"]"			
				smmp=ymm[imin:.8*ishock+.2*imin]
				smypp=Bmed[imin:8*(ishock-imin)/10+imin]

				mn=min(smmp)
			if 0 and MM[3]-MM[0]  lt min(smmp) then begin
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MM[3]=(mx+mn)/2
				MM[0]=(mx-mn)/2

			endif

				;if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;if 0 and MM[3]-MM[0]  lt
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10])  or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin;then begin
					;mx=MM[3]+MM[0]
					;mn=min(smmp);mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;MM[3]=(mx+mn)/2
					;MM[0]=(mx-mn)/2

				;endif
				MM=coeffCalc(xpp,ypp, ishock,x00)
				if MM[3]-MM[0]  lt min(smmp) then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				algshock1=MM[2]+imin
				;if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;if 0 and MM[3]-MM[0]  lt
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin; then begin
				;	mx=MM[3]+MM[0]
					;mn=min(smmp);mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;	MM[3]=(mx+mn)/2
				;	MM[0]=(mx-mn)/2

				;endif
				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print, 'To second order, Function parameters: ', MM
;print,"status=",status,", CHISQ=",CHISQ,", numel(yfit)=",numel(yfit)
				working=1
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			;print,"[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
				if ishock le foot or ishock ge imax then begin
					algshock1=aj
					ishock=ishock1
					working=0
				endif 
				if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
					mx=MM[3]+MM[0]
					mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					MM2=MM
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif	

					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
						;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock2
					endif	

				endif
	
				if  MM[3]-MM[0]  lt min(smmp) then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.

					MM2=MM
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif	

					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)
				
					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock2
						working=1
					endif	
				endif

				if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

					MM2=MM
					MM2[2]=xppa[maxloc2+ishock-11-imin];xpa[maxloc2+ishock-11-dim]
					MM_22=MM2[2]
					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],"	,",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
					;yfit1=yfit
						working=1
						CHISQ=CHISQ2
						ishock=ishock2
						algshock1=MM_22+imin
					endif
			endif
			if ishock le foot or ishock ge imax then begin

					ishock=ishock1
					working=0
			endif else begin 
				B20de=B20d[ishock:imax]
				pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])])) ))




				working=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30;((status eq 0) and (MM[0] gt minM0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain)  and (ishock gt imin) and (ishock+60 lt imax) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1) and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)

			endelse
if dobug eq 3 then begin
;print,"**************************"
;print,"[imin,ishock,imax]=",[imin,ishock,imax]
;print,"[a00,aj,ishock]=",[a00,aj,ishock]
;print,"Fore[a00,aj,ishock]=",[Fore[a00],Fore[aj],Fore[ishock]]
;print,"~~~~~~~~~"
;print,"status eq 0=",status eq 0
;print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0
;print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
;print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),[<] maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
;print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
;print,"Fore[ishock]=",Fore[ishock]
;print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]), [<] upDev, success=",yfit[0],BU[ishock],upDev,abs(yfit[0]-BU[ishock]) lt upDev
;print,"~pointerinup=",~pointerinup
;print,"--------------------------"
;print,"working=,",working
;print,"**************************"
			endif

					if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=b0
						working=working1
					endif 	


			endif
			fracRemain=(bi-ishock)*1.0/(bi-dim)
			;print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			;print,"abs(MM[2]-MM0[2])=",abs(MM[2]-MM0[2]),", 1*(abs(MM[2]-MM0[2]) lt 1000)=",1*(abs(MM[2]-MM0[2]) lt 1000)
;print,"[imin,ishock,imax]=",[imin,ishock,imax] 
			;if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			;if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"
;print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
;print,"[status,CHISQ lt minChi, bi-ishock gt minRemain]=",[status,CHISQ lt minChi, bi-ishock gt minRemain]
			if working then begin
					if lastmax le 0 then lastmax=imax
;print,"0th outbound is working"
					;inimaxs[imin:imax]=imax
					;inimins[imin:imax]=imin
					;inchis[imin:imax]=CHISQ
					;innums[imin:imax]=i
					;inshocklocs[imin:imax]=ishock
					;inups[imin:imax]=MM[3]-MM[0]
					;indowns[imin:imax]=MM[3]+MM[0]
				for k=imin,imax do begin
					inimaxs[k]=imax
					inimins[k]=imin
					inchis[k]=CHISQ
					innums[k]=i
					inshocklocs[k]=ishock
					inups[k]=MM[3]-MM[0]
					indowns[k]=MM[3]+MM[0]
					inalgshock0locs[k]=algshock0
					inalgshock1locs[k]=algshock1

					for ll=0,3 do MMMs[k,ll]=MM[ll]
				endfor
				shocks[ishock]=1
				if isparallel then inparallel[ishock]=bi
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin;and (abs(MM[2]-MM0[2]) lt 1000) then begin
				;print,"no NANs"
			;	print,"gim=",gim,", g0-1=",b0-1
				
					for k=imin,imax-1 do z[k]=yfit[k-imin]	
				endif else begin
;print,"yfit has ",ncount," NANs. calculating directly."
					;if (abs(MM[2]-MM0[2]) ge 1000) then MM=MM0
					if doPerturb then tanhfit,xppa,MM,F else tanhfit,xpa,MM1,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]
					zn=z[imax:*]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]


				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


						
				endelse
				insublengths.add,bi-dim
				;ishock=round(MM[2])+dim
				
				;print,"g0"
				zn=z[b0:*]

				;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			endif else brokenin.add,[i,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			inSubsets[dim:b0]=1;i
			inbegs[dim:aj-1]=1;i
;print,numel(inends),aj,b0
			inends[aj:b0]=1;i
			break
		endwhile ;else begin

		while 0 do begin

			break 
			i=0
			;"if the trrigger pair WAS cut off, we try to approximate results here"
			;"want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth"
			onstep=1*(b0 le zwidth-4)
			;"if onstep eq 1, can assume without loss of generality that we are indeed on the step"
			;ty=yp
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,b0-1 do z[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with m2=0 
				dim=0
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM
;print,"to zeroth order, guess that MM=",MM

				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)

;				;yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType)
;print, 'Function parameters: ', MM
;print,"status=",status
				ishock=round(MM[2]) +dim;round(MM[2]/MM[1]) +dim

				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)


				bi=b0
				imax=b0
				imin=dim

			B20de=B20d[ishock:imax]
			pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 

				;ishock=round(MM[2]/MM[1])+dim;-x00
			
				ishock1=ishock
				MM1=MM
				chi1=CHISQ
				working =((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (b0-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (Fore[ishock] eq 1 or nonfore) and ~pointerinup
				

				if doPerturb then begin
					yfit1=yfit
					working1=working
					MM1=MM

					imin=max([dim,foot-2*60]);ishock-7*60])
					imax=min([b0,ishock+7*60])
					while (yBS[imin]-yBS[dim] gt 2) and (imin gt imax+100) and (imin gt foot-3*60) do begin
					imin--
					if (yBS[imin]-yBS[dim] gt 10) then begin
						ishock--
						imax--
					endif
					endwhile
;print,"(0) shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
;print,"(0) shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

					ypp=ys[imin:imax]
					xpp=xs[imin:imax]
;print,"[numel(yp),numel(ypp)]=",[numel(yp),numel(ypp)]
					xppa=xpp-xpp[0]

					MM=coeffCalc(xpp,ypp, ishock,x00)
					weight=1.0/ypp
					yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,"numel(yfit)=",numel(yfit)
;print, 'To second order, Function parameters: ', MM
;print,"status=",status,", CHISQ=",CHISQ

					ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)



					B20de=B20d[ishock:imax]
					pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				  		   (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $ ;check if average is less than downstream 
					;			for a minute and B20deriv is is monotonically increasing in this region
							 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 


					working =((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (Fore[ishock] eq 1 or nonfore) and ~pointerinup;and (TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)

					if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=b0
						working=working1
					endif 	

				endif


			;	if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			;	if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"
				fracRemain=(bi-ishock)*1.0/(bi-dim)
;print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
;print,"[status,CHISQ lt minChi, bi-ishock gt minRemain]=",[status,CHISQ lt minChi, bi-ishock gt minRemain]
				if ((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock lt imax) then begin
;print,"0th inbound is working"
					;inups[imin:imax]=MM[3]-MM[0]
					;indowns[imin:imax]=MM[3]+MM[0]
					;inimaxs[imin:imax]=imax
					;inimins[imin:imax]=imin
					;inchis[imin:imax]=CHISQ
					;innums[imin:imax]=i
					;inshocklocs[imin:imax]=ishock
				for k=imin,imax do begin
					inimaxs[k]=imax
					inimins[k]=imin
					inchis[k]=CHISQ
					innums[k]=i
					inshocklocs[k]=ishock
					inups[k]=MM[3]-MM[0]
					indowns[k]=MM[3]+MM[0]
				endfor
					insublengths.add,bi-dim
					NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					if( ncount eq 0) then begin
						for k=imin,imax-1 do z[k]=yfit[k-imin]	
					endif else begin
			;		print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F
					
						for k=imin,imax-1 do z[k]=F[k-imin]
					
					endelse


					shocks[ishock]=1
					inSubsets[dim:b0-1]=1;i
					inbegs[dim:ishock-1]=1;i
					inends[ishock:b0-1]=1;i
				endif ;else ;brokenin.add,[0,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]				
			endelse

		

		endwhile;else
		endif

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;if DD[ddn-1] gt BB[bbn-1] then GN=DD[ddn-1] else GN=BB[bbn-1]
		;yp=ys[GN:*]
		;print,GN
		for k=lastmax+1,N-1 do z[k]=0
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do z[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

		;else begin
		;	;"what we do depends on whether fcutoff eq 0"
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif


		;endelse



;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
print,"==================================================================="
print,"			outbound side"
print,"==================================================================="
;nxs=Reverse(xs)
nxs=xs
nys=REVERSE(ys)

nreg=reverse(reg)
nB20d=-1* REVERSE(B20d)
dat.y=nys
dat.x=nxs
nBmed=reverse(Bmed)
;store_data,plt+"_rev",data=dat,limits=limits

;print,"size(nys)=",size(nys)
;nyae=reverse(yae)
;dat.y=nyae
;store_data,"ascend_end_rev",data=dat
;nyab=reverse(yab)
;dat.y=nyab
;store_data,"ascend_begin_rev",data=dat
;nyde=reverse(yde)
;dat.y=nyde
;store_data,"descend_end_rev",data=dat
;nydb=reverse(ydb)
;dat.y=nydb
;store_data,"descend_begin_rev",data=dat

nrhod=REVERSE(rhod)*(-1)
nBsbd=REVERSE(Bsbd)*(-1)


nyBS=REVERSE(yBSb);REVERSE(yBS)
;datBS.y=nyBS
;datBS.x=nxs
;get_data,'B_sec_smoothed',data=datBS
;nAA = WHERE(nyae ne 0, naan, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;if dobug gt 0 then print,yae[DD]

;nDD = WHERE(ydb ne 0, nddn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

;nBB = WHERE(yab ne 0, nbbn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
;nCC = WHERE(yde ne 0, nccn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock


;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]

nmono=reverse(mono);)bmono)

get_data,'monof-B60dsane_rev',data=datMon
nB5d=reverse(B5d)*(-1)

get_data,'B5-3d10_rev',data=datB5d
nz=fltarr(N)
nshocks=fltarr(N)
nyBSfdiff=2*ABS(nyBS-nymm)/abs(nyBS+nymm)
;nDD=N-1-reverse(AA)
;nAA=N-1-reverse(DD)
;nBB=N-1-reverse(CC)
;nCC=N-1-reverse(BB)
;print,"nAA=",nAA
;print,"nBB=",nBB
;print,"nCC=",nCC
;print,"nDD=",nDD
;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
;if dobug gt 0 then print,"numel(nys)=",numel(nys)
;fullIter=min([nddn,naan,nbbn])
;for i=1 ,fullIter-1 do begin
;	j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
;	k=i-1+doffsetb
;	aj=nAA[j]
;	bi=nBB[i]
;	dim=nDD[k]
;	print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
;	if dobug gt 0 then print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
;endfor
doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if numel(nBB) gt 1 then dboffsetb=1*(nBB[1] lt nDD[0]) else  dboffsetb=0
if numel(nAA) gt 1 then daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0
;if dobug gt 1 then print,'numel(nys)=',numel(nys)
;if dobug gt 1 then print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
fullIter=min([nddn,naan,nbbn])
;print,'fullIter=',fullIter
for i=1 ,fullIter-1 do begin
	j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	ii=i+dboffsetb
	aj=nAA[j]
	bi=nBB[ii]
	dim=nDD[k]
;print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	if dobug gt 1 then print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	
endfor
;stop
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		lastmax=0
		for i=1,fullIter-1  do begin
			print,"L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-"
			cont=0
			shks=nshocks
			sublengths=outsublengths
			Subsets=outSubsets
			begs=outbegs
			ends=outends
			brokens=brokenout
			test=-1
			shockloc=-1
			imn=-1
			imx=-1
			chi=0
			doffsetf=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?


			foffsetf=1*(ofoots[0] gt nAA[0])


			backcutoff=1*(nDD[0] le nCC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(nBB[nbbn-1] le nAA[naan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
			if numel(nBB) gt 1 then boffsetb=1*(nBB[1] lt nDD[0]) else boffsetb=0
			if numel(nAA) gt 1 then aoffsetb=1*(nAA[1] lt nDD[0]) else aoffsetb=0
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf +aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			Bmin=0
			Bmax=0
			error_status=0
			catch,error_status
			if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[N-1-nAA[j]]
print,'ERROR in curveFitter5 during i=',i
				errorloc=""
			
				errmsg=["error in curveFitter5 outbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel

				continue
			endif

			zl=fitloop(i, nxs,nys,nz,nAA,nBB,nCC,nDD,rollup,rollsec,nmono,nB5d,nyBS,curveType,shks,zwidth,trust, sublengths,$
					Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc,Bmin,Bmax,$					
					nB20d,rollthird,minM1,orderCustom,maxUp,nBmed,nrhod,nBsbd,nextr,enDD,nFore,nBU,nB3,nymm,minM0,ofoots,$
					nforemaxs,nphm,outparallel,nderex,nn_e,allownonfore=allownonfore,MMM=MMM,algshock0=algshock0,algshock1=algshock1)
			;if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			nz=zl
			nshocks=shks
			outsublengths=sublengths
			outSubsets=Subsets
			outbegs=begs
			outends=ends
			brokenout=brokens
			for kk=imn,imx do begin
				outnums[kk]=i
				outshocklocs[kk]=shockloc
				outimaxs[kk]=imx
				outimins[kk]=imn
				outchis[kk]=chi

				outups[kk]=Bmin
				outdowns[kk]=Bmax


				outalgshock0locs[kk]=algshock0
				outalgshock1locs[kk]=algshock1
				for ll=0,3 do nMMMs[kk,ll]=MMM[ll]
			endfor


		endfor 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=nBB[0+dboffsetb]
		if nDD[0] lt b0 and nDD[0] ne -1 then dim=nDD[0] else dim=0

		yp=nys[0:b0]
		xp=nxs[0:b0]
		np=size(yp,/n_el)
		

		;aj=nAA[0]

		;"start with standard case"
		aj=nAA[0+daoffsetb]
		ishock=aj
		;'start with standard case'
		errorloc=""
		error_status=0
		isFinite=total(finite(yp) ne 1) eq 0;total(finite(nys[0:b0])) ne 0) and (total(finite(nys[0:aj])) ne 0) and (total(finite(nys[aj:b0])) ne 0)
		catch,error_status
		if error_status ne 0 then begin
;print, 'Error index: ', error_status
;print, 'Error message: ', !ERROR_STATE.MSG
;print,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			xaaa=xs[N-1-ishock]
			errmsg=["error in curveFitter5 outbound 0 while producing "+newName, 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc]
			errorsaver,xaaa,errmsg,/spec
			catch,/cancel
		endif
		if error_status eq 0 then begin
		;if(aoffsetb eq 0) and (nAA[0] ne -1) and isFinite and (nBB[0] ne -1) and b0 gt aj then begin
		while (aoffsetb eq 0) and (nAA[0] ne -1) and isFinite and (nBB[0] ne -1) and b0 gt aj do begin
print,"L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-"
print,"now for the range nys[0:nBB[0]]=nys[0:",nBB[0],"]"


			aj=nAA[0+daoffsetb]
			mxfore=nforemaxs[0+daoffsetb]
			;b0=(where(nBB gt aj))[0]
			;if b0 eq -1 then b0=N-1

			;dim=(where(nDD lt aj))[0]
			;if dim eq -1 then dim=0
;print,"[d0,aj,b0]=",[d0,aj,b0]
			;print,gi
			;print,"gim=",gim
			;dim=d0
			if b0 lt dim then dim=0
			errorloc="n_ep=nn_e[dim:b0]=nn_e["+strtrim(dim,2)+':'+strtrim(b0,2)+']'
			n_ep=nn_e[dim:b0]
			if total(finite(n_ep) eq 1) eq 0 then break

			nregp=nreg[dim:b0]

			if (min(nregp) eq 1) and nregp[0] ne 1 then begin

				if min(nreg[dim:b0-1]) gt 1 then break

				ndim=dim+(where(nreg[dim:b0] eq 1))[0]
				if ndim+2*60 gt aj then break else begin 

					dim=ndim
				;while (nregp[0] gt 1) and dim+6*60 le aj  do begin

				;	dim++
					nregp=nreg[dim:b0];=nregp[1:*]
					yp=nys[dim:b0]
					xp=nxs[dim:b0];xp[1:*]
				;endwhile
				endelse
			endif

			wherefoot=(where(ofoots lt aj and ofoots ge dim))[-1]
			;foot=ofoots[()[0]]

;			if wherefoot eq -1 then foot=dim else foot=ofoots[wherefoot]

			if wherefoot eq -1 then break else foot=ofoots[wherefoot]
			bfoot=max([dim,foot-10*60]);120])
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			;print,"g0=",b0
			errorloc="if total(finite(nn_e[dim:foot]) ne 1) ne 0 then begin, [dim,foot]=["+strtrim(dim,2)+','+strtrim(foot,2)+']'
			if total(finite(nn_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(nn_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif

			pcross=max([nphm[(where(nphm lt b0 and nphm gt foot))[0]]-60,foot])
			if nymm[dim] le 0 or finite(nymm[dim]) ne 1 then begin
				KK=where(nymm[dim:aj] gt 0)
				if foot ne d00 then begin
					LL=where(xs[dim:aj] lt xs[foot])
					SS=INTERSECT(KK,LL)

					if SS[0] ne -1 then dim=SS[0]+dim else begin
						break
					endelse
				endif else dim=KK[0]+dim

			endif
			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			if total(nymm[b0] le nymm[dim:b0-1]) eq 0 then begin

				while total(nymm[b0] le nymm[dim:b0-1]) eq 0  and b0-aj gt 3*60 and nB20d[b0] ne 0 do b0--

			endif
			errorloc="ybeg=nys[dim:aj]=nys["+strtrim(dim,2)+":"+strtrim(aj,2)+"]"
			ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			errorloc="CRUSTST=where((nBsbd[aj+1:b0] gt 1.0) and (nrhod[aj+1:b0] lt 0),ccount)+aj+1,[dim,aj,bi]=["+strtrim(dim,2)+","+strtrim(aj,2)+","+strtrim(b0,2)+"]"
			CRUSTST=where((nBsbd[aj+1:b0] gt 1.0) and (nrhod[aj+1:b0] lt 0),ccount)+aj+1
			if ccount gt 0  then begin
				b0=CRUSTST[0]
				yp=nys[dim:b0]
				np=numel(yp)
				
			endif


			

				nzn=nz[b0:*]
			errorloc="ybeg=nys[aj:b0]=nys["+strtrim(aj,2)+":"+strtrim(b0,2)+"]"
			yend=nys[aj:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			brefines=0
			frefines=0
	

			mbeg=nbeg
			mend=nend
		

			incfunctions=1
			decfunctions=1
			bi=b0
			d00=dim
			while  ((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((nBmed[aj+nend-1] le mean(ybegtrust)+1.1) and (decfunctions eq 1)) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where aj=",aj,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) or (nbeg eq 1) or (np eq 2) or dim ge foot-2*60 or numel(yp) le 2  then begin
						incfunctions=0
						;dim=DD[i-1]
						dim=d00
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((nBmed[aj+nend-1] le mean(ybegtrust)+1.1) and (decfunctions eq 1)) or numel(yp) le 2  then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					nend--
					
					if nend gt 1 then yend=yend[0:nend-1]
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) or (nend eq 2) or (aj +60 eq bi) or (np eq 2) then begin
						;print,"i=",i,", ddn=",ddn
						decfunctions=0
						bi=b0;nBB[0]
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						yend=nys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile
			edim=dim 
			b0=bi
			errorloc='2nd n_ep=nn_e[dim:b0]=nn_e['+strtrim(dim,2)+':'+strtrim(b0,2)+']'
			n_ep=nn_e[dim:b0]
			if total(finite(n_ep) eq 1) eq 0 then break


			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			errorloc='1st xp=nxs[dim:b0]=nxs['+strtrim(dim,2)+':'+strtrim(b0,2)+']'
			xp=nxs[dim:b0]
			yp=nys[dim:b0]
			xpa=xp-min(xp)

			;plotn1=plot(xpa,yp, title="Shock "+newName)
			
		;	plotn2=plot(xpa,thSpk(xpa,-dim+aj),'r-',/overplot)			
			;plotn2=plot(fltarr(2) + aj-dim, plotn1.yrange, name="zerothOrder",color='red', /overplot)
			yc=yp*0.0
			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5
			if foot ne d00 then mn=nBU[foot] else mn=nBU[aj];mean(ybegtrust)
			
			;mn=5
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			zeron=aj-dim
;print,"zeron"
			if dim ne 0 then edim=enDD[0]

			if aj lt edim then edim=0
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;mn=mean(ybegtrust)
			;aj=FalseShockSkipper(aj,bi,nBmed,median(nBmed[2 * nbeg/5+dim: nbeg/2+dim]))
			ishock=aj
			aj=ajRefiner(max([edim,foot,pcross]),max([edim,dim]),aj,bi,zeron,dobug,nyBS,nbeg,nend, nmono,nB5d,nB20d,orderCustom,nextr,nrhod,nFore)
if nymm[bi] eq max(nymm[aj:bi]) then while nymm[bi] eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(nymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt nymm[bi] )  do bi--

			if total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) le 1 or total(abs(nymm[aj]-nymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(nymm[aj]-nymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( nymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) then while max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(nymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt max(nymm[bi-20:bi]) )  do bi--
			ishock=aj

			;while max(nys[max([aj-30,dim]):aj]) gt max(nys[aj:bi]) and aj gt foot do aj--
			errorloc="while max(nys[max([aj-60,foot]):aj]) gt max(nys[aj:bi]) , [foot,aj,bi]="+strtrim(foot,2)+","+strtrim(aj,2)+","+strtrim(bi,2)+"]"
			while max(nys[max([aj-60,foot]):aj]) gt max(nys[aj:bi]) and aj gt foot do begin
				errorloc="while max(nys[max([aj-60,foot]):aj]) gt max(nys[aj:bi]) , [foot,aj,bi]="+strtrim(foot,2)+","+strtrim(aj,2)+","+strtrim(bi,2)+"]"
				 aj--
			endwhile


			;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,nmono)
			;plotn3=plot(xpa,thSpk(xpa,-dim+aj),'y-',/overplot)		
			;plotn3=plot(fltarr(2) + aj-dim, plotn1.yrange, name="monoroll",color='yellow', /overplot)
			;print,"after monorolling"
			;print,"[imin,aj,imax]=",[dim,aj,b0]
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,nB5d,dobug) ; second order correction 
			;p4=plot(xpa,thSpk(xpa,-dim+aj),'g-',/overplot)	
			;plotn4=plot(fltarr(2) + aj-dim, plotn1.yrange, name="rollsec",color='green', /overplot)
			;print,"after secondroll"
			;print,"[imin,aj,imax]=",[dim,aj,b0]

			;nl=300*nB20d[dim:bi]+10
			;nl[0:100]=0.0
			isparallel=0
			if total(nyBSfdiff[aj+60:aj+60+4*60] lt .2)/120. gt .9 and aj+60 lt bi-10 then begin

				maxfdiff=max(nyBSfdiff[aj+60:bi-10],mxparfdloc)
				maxpar=max(nyBS[aj+60:bi-10],mxparloc)
				if maxfdiff lt .4 and abs(mxparloc-mxparfdloc) lt 120 and min([mxparloc,mxparfdloc])+aj+60 gt aj+4*60 then begin
					bi=min([mxparloc,mxparfdloc])+aj+60-1
					isparallel=1
				endif
				
			endif
			;plotn6=plot(fltarr(2) + aj-dim, plotn1.yrange, name="third_roll",color='blue', /overplot)			
			;print,"after third_roll"
			;print,"[imin,aj,imax]=",[dim,aj,b0]
			;aj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)
			;plotn5=plot(xpa,thSpk(xpa,-dim+aj),'c-',/overplot)
			;plotn5=plot(fltarr(2) + aj-dim, plotn1.yrange, name="aoscillate",color='cyan', /overplot)
			;print,"after aoscillate"
			;print,"[imin,aj,imax]=",[dim,aj,b0]
			;plotnh=plot(xpa,nl,color=56,/overplot)
			;if (rollthird eq 1) then aj=Third_Roll(nB20d,dim,aj,bi,dobug) ; third order correction 

			;plotn6=plot(xpa,thSpk(xpa,-dim+aj),'b-',/overplot);

			mn=nBU[aj];mean(ybegtrust)
			wblwu=where(nymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1
			ymmn=min(nymm[mean([aj,bi]):bi],ymmnlc)
			ymmnlc+=mean([aj,bi])
			errorloc="ymmn2=min(nys[ymmnlc-40:ymmnlc+40],ymmnlc2), [ymmnlc-40:ymmnlc+40]="+strtrim(ymmnlc-40,2)+":"+strtrim(ymmnlc+40,2)+"]"
			ymmn2=min(nys[ymmnlc-40:ymmnlc+40],ymmnlc2)
			ymmnlc2+=ymmnlc-20
			if  0 and ymmnlc2 lt bi-1 and ymmn2 lt mn*.75+.25*mean(nymm[aj:bi]) then begin
				bi=ymmnlc2-1
				while nys[bi] lt mn*.75+.25*mean(nymm[aj:bi]) and bi gt aj+4*60 do bi--
			endif
			b0=bi
			xp=nxs[dim:b0]
			yp=nys[dim:b0]
			xpa=xp-min(xp)

			yend=nys[aj:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] 
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			errorloc='2nd xp=nxs[dim:b0]=nxs['+strtrim(dim,2)+':'+strtrim(b0,2)+']'
			m2=xpa[aj-dim]
			mx=mean(ymm[aj:bi]);max(yend)
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1.;.5;.1;1
			MM=[m0,m1,m2,m3]
;print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

			status=-11
			CHISQ=-1
;print,numel(xpa),numel(yp),numel(weight)

			tanhfit,xpa,MM,F0
	
			algshock0=aj
			algshock1=aj
			;plotf0=plot(xpa,F0,color="steel_blue",'--',/overplot)

			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)
			yfit1=yfit
;print,"status, chisq"
;print,status, CHISQ
			chi1=CHISQ
;print, 'Function parameters'
;print, MM
			ishock=findshock(aj,MM[2],MM[1],yfit,dim,1-doPerturb,foot)
			ishock1=ishock
			working=1
;print,"[max,min,bi-ishock]"
;print,[MM[3]+MM[0],MM[3]-MM[0],bi-ishock]
			;ishock=round(MM[2]/MM[1])+dim
;print,"[imin,ishock,imax]"
			imin=dim
			imax=bi
;print,[imin,ishock,imax]
			if ishock le foot or ishock ge bi then begin

				ishock=aj
				working=0
			endif else begin

				B20de=nB20d[ishock:imax]
				pointerinup=( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

					     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 

				working=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (nFore[ishock] eq 1 or nonfore) and imin lt foot-30 ;((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nFore[ishock] eq 1 or nonfore) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup;and (TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)
			endelse
			working1=working

			;if working then plotn7=plot(fltarr(2)+ ishock-dim, plotn1.yrange, name="ishock",color='magenta', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
			;if working then plotf1=plot(xpa,yfit1, name="fit",color='purple','-', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
			;ishock=round(MM[2]/MM[1])+dim;-x00
			if doPerturb then begin
				;while (nyBS[imin]-nyBS[dim] gt 2) and (imin gt dim+100  )  do begin
				;imin--
				;ishock--
				;imax--
				;endwhile
				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
				imin=max([dim,foot-2*60]);ishock-7*60])
				smol=min(nderex[ishock:bi],smoloc) 

				if finite(smol) eq 1 and smoloc gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])
;print,"[imin,ishock,imax]"
;print,[imin,ishock,imax]
				errorloc="ypp=nys[imin:imax]=nys["+strtrim(imin,2)+":"+strtrim(imax,2)+"]"
				ypp=nys[imin:imax]
				xpp=xs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				smypp=nBmed[imin:8*(ishock-imin)/10+imin]
				smmp=nymm[imin:8*(ishock-imin)/10+imin]
				mn=min(smmp)
				;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
			if 0 and MM[3]-MM[0]  lt min(smypp) then begin
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MM[3]=(mx+mn)/2
				MM[0]=(mx-mn)/2

			endif


				;if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;if 0 and MM[3]-MM[0]  lt
				if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif

				MM=coeffCalc(xpp,ypp, ishock,x00)
				if 0 and MM[3]-MM[0]  lt min(smypp) then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				;if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin
				;	mx=MM[3]+MM[0]
				;	mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;	MM[3]=(mx+mn)/2
				;	MM[0]=(mx-mn)/2

				;endif
;print,"to first order, guess that MM=",MM
;print,"where ishock=",round(MM[2]/MM[1])+imin
				algshock1=MM[2]+imin
				tanhfit,xpp,MM,F01
	

				;plotf01=plot(xpp-xp[0],F01,color="gold",'--',/overplot)
				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,CHISQ
;print,"status, chisq"
;print,status, CHISQ
;print, 'Function parameters'
;print, MM				working=1
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			;print,"[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
				if ishock le foot or ishock ge imax then begin

					ishock=ishock1
					working=0
				endif 
				if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
					mx=MM[3]+MM[0]
					mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					MM2=MM
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif	

					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
						;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock2
					endif	

				endif
	
				if  MM[3]-MM[0]  lt min(smmp) then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.

					MM2=MM
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif	

					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)
				
					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock2
						working=1
					endif	
				endif

				if (nys[ishock-11] lt nys[ishock]) and (max(nys[ishock-11:ishock],maxloc2) gt nys[ishock]) then begin

					MM2=MM
					MM2[2]=xppa[maxloc2+ishock-11-imin];xpa[maxloc2+ishock-11-dim]
					MM_22=MM2[2]
					yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],"	,",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2 gt foot then begin
						MM=MM2
						status=status2
						yfit=yfit2
					;yfit1=yfit
						working=1
						CHISQ=CHISQ2
						ishock=ishock2
						algshock1=MM_22+imin
					endif
			endif
			if ishock le foot or ishock ge imax then begin
					algshock1=aj
					ishock=ishock1
					working=0
			endif else begin 
				B20de=nB20d[ishock:imax]
				pointerinup=( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 


				working=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30;((status eq 0) and (MM[0] gt minM0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain)  and (ishock gt imin) and (ishock+60 lt imax) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1) and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)

			endelse


				
			
				;working=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (nFore[ishock] eq 1 or nonfore) and imin lt foot-30;((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nFore[ishock] eq 1 or nonfore) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup;TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0
				;if working then plotn8=plot(fltarr(2)+ ishock-dim, plotn1.yrange, name="ishock2",color='peru', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'-',color='peru',/overplot)	
				;if working then plotf2=plot(xppa-xp[0]+xpp[0],yfit, name="fit2",color=37,'--', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
				if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
					MM=MM1
					CHISQ=chi1
					ishock=ishock1
					yfit=yfit1
					imin=dim
					imax=bi
					working=working1
					algshock1=aj
					xppa=xpa
				endif 	
			endif

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)


			;if dobug gt 0 then print,"[imin,ishock,imax]"
			;if dobug gt 0 then print,[imin,ishock,imax]
			;if ishock lt imin then print, "bad ishock: ishock : ",ishock," lt ", imin,": imin"
			;if ishock gt imax then print, "bad ishock: ishock : ",ishock," gt ", imax,": imax"
			;if dobug gt 0 then print,"[max,min,remain,fracRemain]:[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]"
			fracRemain=(imax-ishock)*1.0/(imax-imin)
;print,[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			;print,"[status,CHISQ , minChi, minChi-CHISQ, minChi-CHISQ ge 0, bi-ishock gt minRemain]:",[status,CHISQ , minChi,minChi-CHISQ, minChi-CHISQ ge 0,bi-ishock gt minRemain]
			if working then begin
			if lastmax eq 0 then lastmax=imax
;print,"[imin,ishock,imax]=",[imin,ishock,imax]
;print,"0th outbound is working"
					;t1=TEXT(.1,.8,'Good fit')
					;outimaxs[imin:imax]=imax
					;outimins[imin:imax]=imin
					;outchis[imin:imax]=CHISQ
					;outnums[imin:imax]=i
					;outshocklocs[imin:imax]=ishock

					;outups[imin:imax]=MM[3]-MM[0]
					;outdowns[imin:imax]=MM[3]+MM[0]
			for k=imin,imax do begin 
				outups[k]=MM[3]-MM[0]
				outdowns[k]=MM[3]+MM[0]
				outimaxs[k]=imax
				outimins[k]=imin
				outchis[k]=CHISQ
				outnums[k]=i
				outshocklocs[k]=ishock

				outalgshock0locs[k]=algshock0
				outalgshock1locs[k]=algshock1
				for ll=0,3 do nMMMs[k,ll]=MM[ll]
			endfor
				if isparallel then outparallel[ishock]=bi
				nshocks[ishock]=1					
				outsublengths.add,bi-dim
				if( ncount eq 0)  then begin
					;print,"no NANs"
				;	print,"gim=",gim,", g0-1=",b0-1
							nshocks[ishock]=1					
					for k=imin,imax-1 do nz[k]=yfit[k-imin]	
				endif else begin
			;	print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xppa,MM,F
					nzn=nz[b0:*]

				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

					for k=imin,imax-1 do nz[k]=F[k-imin]
			
				endelse
			endif ;else ;brokenout.add,[0,dim,b0, nxs[dim]-min(nxs),nxs[b0]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			;print,"g0"
			nzn=nz[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			break
		endwhile;endif ;else begin 

		while 0 do begin
			break
			;"if the trrigger pair WAS cut off, we try to approximate results here"
;print,"0th sheath was cut off"
			;"want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth"
			onstep=1*(b0 le zwidth-4)
			;"if onstep eq 1, can assume without loss of generality that we are indeed on the step"
			;ty=yp
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,b0-1 do nz[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with m2=0 
				
			b0=nBB[0]
;print,"b0=",b0
				;if nDD[0] lt b0 then d0=nDD[0] else d0=0	
				dim=0
				;bi=b0
				m0=(mx-mn)/2.
				m3=(mx+mn)/2.

				m2=0.
				m1=1.;.5;.1;1.;atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM
				if dobug gt 0 then print,"to zeroth order, guess that MM=",MM

				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)
				if dobug gt 0 then PRINT, 'Function parameters: ', MM

				if NOT finite(MM[1]) then MM=MM0
				ishock=round(MM[2]) +dim;round(MM[2]/MM[1]) +dim
				;if dobug gt 0 then print,"[max,min,bi-ishock]=[m3+m0,m3-m0,bi-ishock]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock]
				imin=dim
				imax=bi
				;if dobug gt 0 then print,"[imin,ishock,imax]=",[imin,ishock,imax]
				if doPerturb then begin
				imin=max([dim,ishock-7*60,0])
				imax=min([bi,ishock+7*60,N-1])
				while (nyBS[imin]-nyBS[dim] gt 10) and ( imin gt dim+100) do begin
					imin--
				;ishock--
				;imax--
				endwhile
				;if dobug gt 0 then print,"[imin,ishock,imax]=",[imin,ishock,imax]
					ypp=nys[imin:imax]
					xpp=nxs[imin:imax]
					xppa=xpp-xpp[0]
					MM1=MM
					MM=coeffCalc(xpp,ypp, ishock,x00)

					weight=1.0/ypp
					yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
	
					ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			endif


				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			;if dobug gt 0 then  print,"[imin,ishock,imax]=",[imin,ishock,imax]
			;if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			;if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"
			
			if ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ)  eq 1) and (CHISQ lt minChi) and (b0-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nFore[ishock] eq 1 or nonfore)then begin
					outimaxs[imin:imax]=imax
					outimins[imin:imax]=imin
					outchis[imin:imax]=CHISQ
					outnums[imin:imax]=i
					outshocklocs[imin:imax]=ishock
					outups[imin:imax]=MM[3]-MM[0]
					outdowns[imin:imax]=MM[3]+MM[0]
					if( ncount eq 0) then begin
					for k=0,b0-1 do nz[k]=yfit[k]	
					endif else begin
			;			print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F

						for k=0,i-1 do nz[k]=F[k]
				
					endelse
					;ishock=round(MM[2]/MM[1]) +imin
					outSubsets[dim:b0]=1
					outbegs[dim:aj]=1
					outends[aj:b0]=1
					nshocks[ishock]=1				
				endif
				
			endelse
		

		endwhile;else
		endif

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;GN=nDD[bbn-1]
		;yp=nys[GN:*]
		;print,GN
		for k=lastmax+1,N-1 do nz[k]=0
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do nz[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

		;else begin
		;	;"what we do depends on whether fcutoff eq 0"
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif


;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
	endif
	catch,error_status
		if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG

			PRINT,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			;xaaa=xs[N-1-ishock]
			errmsg=["error in curveFitter5 producing "+newName+" after fitting", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
			errorsaver,xs[0],errmsg
		endif
;print,"<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>"
	zb=reverse(nz)
	znb=reverse(nzn)
	shocksb=reverse(nshocks)

	outSubsets=reverse(outSubsets)
	outends=reverse(outends)
	outbegs=reverse(outbegs)


	outimaxs=reverse(outimaxs)
	outimins=reverse(outimins)
	outchis=reverse(outchis)
	outnums=reverse(outnums)
	outparallel=reverse(outparallel)
	
	outshocklocs=reverse(outshocklocs)
	outalgshock0locs=reverse(outalgshock0locs)
	outalgshock1locs=reverse(outalgshock1locs)

	bMMMs=reverse(nMMMs)
	;outshocklocs2=outshocklocs
	;for i=0, N-1 do if outshocklocs2[i] ne -1 then outshocklocs2[i]=N-1-outshocklocs2[i]

	outdowns=reverse(outdowns)
	outups=reverse(outups)

	;adjChiIn=fltarr(N);inchis*0.0
	;adjChiOut=fltarr(N)
	;for i=0,N-1 do if inchis[i] ne 0 then adjChiIn[i]= abs(inchis[i]-1)
	;for i=0,N-1 do if outchis[i] ne 0 then adjChiOut[i]= abs(outchis[i]-1)
	chis=outchis
	for i=0 , N-1 do if inchis[i] ne 0 then chis[i] = inchis[i]
	;adjChi=fltarr(N)
	;for i=0,N-1 do if chis[i] ne 0 then adjChi[i]= abs(chis[i]-1)
	;store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
	;store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:"abs(REDUCED CHISQ-1)"}
	;store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:"abs(REDUCED CHISQ-1)"}

	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	;result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=z[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	;store_data,'shocks'+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	;store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	;store_data,'sublengths'+rl+'_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

	;store_data,'sublengths'+rl+'_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
	;store_data,'sublengths'+rl+'_inbound_end',data={x:xs,y:inends,ytitle:'flag'}


	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	;;help,dat
	store_data,newName+'_inbound',data={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inups:inups,indowns:indowns,inparallel:inparallel,$
MMs:MMMs,inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs}
	;store_data,newName+'CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_CHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
	;store_data,'sublengths'+rl+'_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}

	;store_data,'sublengths'+rl+'_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
	;store_data,'sublengths'+rl+'_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

	NNN=where(FINITE(zb) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zb[where(finite(zb,/NAN))]=0
;	print,'size(z)=',size(zb)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	;result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	znb=zb[b0:*]

	;print,"mean(zn)=",mean(znb),",  max(zn)=",max(znb)

	;store_data,'shocks'+rl+'_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	;store_data,newName+'_shocks_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	;;help,dat
	store_data,newName+'_outbound',data={x:xs,y:zb,ytitle:"Magnetic Field (nT)", outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,outups:outups,outdowns:outdowns,outparallel:outparallel,$
MMs:bMMMs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}
	
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
zf=z
	for i=0, N-1 do begin
		;if outshocks[i] ne 0 then shocks[i] = outshocks[i]

		outSL=outshocklocs[i]
		inSL=inshocklocs[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL)))  then begin
			zf[i]=z[i]
			;chis[i]=inChis[i]
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL)))  then zf[i]=zb[i]
			;downs[i] = outdowns[i]
			;ups[i] = outups[i]
			;shockLocs[i] = outSL
			;downups[i] = outdownups[i]
			;imaxs[i]=outimaxs[i]
			;imins[i]=outimins[i]
			;chis[i]=outChis[i]
			;ENDIF 	
		ENDELSE
	endfor


	;for i=0, N-1 do begin

		;if z[i] ne 0 then zb[i]=z[i]

	;endfor
	;zf=zb
	for i=0, N-1 do begin

		if shocksb[i] ne 0 then shocks[i]=shocksb[i]

	endfor

	;z=z+zb
	;shocks+=shocksb
	
	NNN=where(FINITE(zf) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zf[where(finite(zf,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	;result = MOMENT(zf)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=zf[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	;store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}
	store_data,newName+'_shocks',data={x:xs,y:shocks,ytitle:'flag'}
	if(total(zf-ys) eq 0) then print, 'broken'
 	dat.y=zf
	;print,size(dat)
	;;help,dat
	store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks,$
inMMs:MMMs,outMMs:bMMMs,inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}


print,".oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo."
print,'		finished creating ',newName
print,"============================================"

	TOC, clock
	catch,error_status



	;insublengths=insublengths.toarray()
	;outsublengths=reverse(outsublengths.toarray())
;print,"insublengths=",insublengths
	;if numel(insublengths) gt 0 then PRINT, 'Mean: ', mean(insublengths), ', Variance: ', stddev(insublengths)
;print,"outsublengths=",outsublengths
	;if numel(outsublengths) gt 0 then PRINT, 'Mean: ', mean(outsublengths), ', Variance: ', stddev(outsublengths)


	;store_data,"outnums",data={x:xs,y:outnums,ytitle:"shock number"}


	;brokenin=brokenin.toarray()
	;brokenout=reverse(brokenout.toarray())
;print,"brokenin"
;print,"i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
;print,brokenin
;print,"brokenout"
;print,"i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
;print,brokenout

	;wdelete
end
