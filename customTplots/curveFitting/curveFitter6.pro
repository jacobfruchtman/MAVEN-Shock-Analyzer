
pro curveFitter6, plt , newName=newName , doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom,minM0=minM0,allownonfore=allownonfore,shocktest=shocktest
	clock=TIC(plt)
	g0=0
	nonfore=0
	if NOT KEYWORD_SET(newName) THEN newName=newName+'_smoothed'
	if NOT KEYWORD_SET(minChi) THEN minChi=1.20
	if NOT KEYWORD_SET(minM0) THEN minM0=0.5
	if keyword_set(allownonfore) then nonfore=1
	rollup=0
	rollsec=0
	rollthird=0
	rollnone=0
	;dobug=0
	minRemain=100
	if not KEYWORD_SET(Debug) THEN dobug=0 else dobug =Debug
	maxUp=1.5
	minM1=.04
	;minM0=0.0
	upDev=2.2
	error_status=0


	curveorders,rollup,rollsec,rollthird,rollnone,orderCustom=orderCustom,doRoll=doRoll,doSecond=doSecond,doThird=doThird,doDefault=doDefault
	
	doPerturb=0
	if KEYWORD_SET(secondOrder) THEN doPerturb=1                
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	curveType="tanhfit"
	typ=2
	get_data,plt,data=dat,limits=limits
	;get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
	print,"=curveFitter6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=curveFitter6="
	print,"			newName=",newName
	print,"=curveFitter6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=6=curveFitter6="
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

		get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_mean',data=datmm

		get_data,"foot_start_inbound",data=datfsi
		get_data,"foot_start_outbound",data=datfso

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

		if dobug gt 0 then print,"numel(B5d)=",numel(B5d)
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
		
		if dobug gt 0 then	print,"numel(B5d)=",numel(B5d)
		
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


		FF=WHERE(datfsi.y ne 0, ifn)
		nFF=WHERE(reverse(datfso.y) ne 0, ofn)


		AA = WHERE(datAA.y ne 0, aan); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn); end of  negative  shock


		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn);defines the boundaries in  forward direction. beginning  of negative shock
		enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn); end of  negative  shock

	if dobug gt 0 then 	PRINT,"[ddn,aan,bbn]=",[ddn,aan,bbn]
	if dobug gt 0 then 	print,"numel(B5d)=",numel(B5d)
		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD


		if DD[0] gt AA[0] and AA[0] ne -1 then begin
			DD=[0,DD]
			ddn++
		endif
		if nDD[0] gt nAA[0] and nAA[0] ne -1 then begin
			nDD=[0,nDD]
			nddn++
		endif

		if eDD[0] gt AA[0] and AA[0] ne -1 then begin
			eDD=[0,eDD]
			eddn++
		endif
		if enDD[0] gt nAA[0] and nAA[0] ne -1 then begin
			enDD=[0,enDD]
			enddn++
		endif

		if BB[-1] lt AA[-1] and AA[0] ne -1 then begin
			BB=[BB,N-1]
			bbn++
		endif
		if BB[0] lt AA[0] and AA[0] ne -1 then begin
			BB=BB[1:*]
			bbn--
		endif

		if nBB[0] lt nAA[0] and nAA[0] ne -1 then begin
			nBB=nBB[1:*]
			nbbn--
		endif
		if nBB[-1] lt nAA[-1] and nAA[0] ne -1 then begin
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
		foremaxs=datFore.ymax[AA]
		nforemaxs=(REVERSE(datFore.ymax))[nAA]

	;------------------SHORT CIRCUITING NONCROSSING PERIODS HERE
	zf=fltarr(N)
	if AA[0] eq -1 and nAA[0] eq -1 then begin
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
		;help,dat

		store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, 			outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}
		store_data,newName+"_inbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs}
		store_data,newName+"_outbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", outchis:outchis, 			outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}

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

		;doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
		;aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?
		;fooffsetf=1*(ifoots[0] gt AA[0])



		;backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		;fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,"a offsetf=",aoffsetf
;		print,"d offsetf=",doffsetf
		;print, "bcutoff: ",bcutoff
		;print, "fcutoff: ",fcutoff

		


		print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0


;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
print,"==================================================================="
print,"6			inbound side				 6"
print,"==================================================================="


	for i=0,aan-1 do begin
			print,"~~~~~~~~~"
			print,"i=",i
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

			

		
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[AA[i]]
				PRINT,'ERROR in curveFitter6 during i=',i
				errorloc=""
			
				errmsg=["error in curveFitter6 inbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel

				continue
			endif
		 	aj=AA[i]
			bi=BB[(where(aj lt BB))[0]]
			dim=DD[(where(aj gt DD))[-1]]
			edim=eDD[(where(aj gt eDD))[-1]]
			wherefoot=(where(FF lt aj and FF ge dim))[-1]
			if wherefoot eq -1 then continue else foot=FF[wherefoot]
			mxfore=foremaxs[i]
			print,"dim,edim,foot,aj,bi=",[dim,edim,foot,aj,bi]
			zl=fitloop2(i, xs,ys,z,aj,bi,dim,foot,edim,rollup,rollsec,mono,B5d,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,B20d,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,Fore,BU,B3,ymm,minM0,mxfore,allownonfore=allownonfore)
if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			z=temporary(zl)
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
			endfor


		endfor


print,"==================================================================="
print,"6			outbound side				 6"
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

nmono=reverse(bmono);)bmono)

get_data,'monof-B60dsane_rev',data=datMon
nB5d=reverse(B5d)*(-1)

get_data,'B5-3d10_rev',data=datB5d
nz=fltarr(N)
nshocks=fltarr(N)

	for i=0,naan-1 do begin
			print,"~~~~~~~~~"
			print,"i=",i
			cont=0
			shks=shocks
			sublengths=outsublengths
			Subsets=outSubsets
			begs=outbegs
			ends=outends
			brokens=brokenout
			test=-1

			chi=0
			imx=-1
			imn=-1
			shockloc=-1
			Bmin=0
			Bmax=0

			

		
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[N-1-nAA[i]]
				PRINT,'ERROR in curveFitter6 during i=',i
				errorloc=""
			
				errmsg=["error in curveFitter6 outbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel

				continue
			endif
		 	aj=nAA[i]
			bi=nBB[(where(aj lt nBB))[0]]
			dim=nDD[(where(aj gt nDD))[-1]]
			edim=enDD[(where(aj gt enDD))[-1]]
			wherefoot=(where(nFF lt aj and nFF ge dim))[-1]
			if wherefoot eq -1 then continue else foot=nFF[wherefoot]
			mxfore=nforemaxs[i]
			print,"dim,edim,foot,aj,bi=",[dim,edim,foot,aj,bi]
			zl=fitloop2(i, xs,ys,z,aj,bi,dim,foot,edim,rollup,rollsec,nmono,nB5d,nyBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,nB20d,rollthird,minM1,orderCustom,maxUp,nBmed,nrhod,nBsd,nextr,nFore,nBU,nB3,nymm,minM0,mxfore,allownonfore=allownonfore)
if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			z=temporary(zl)
			shocks=shks
			outSubsets=Subsets
			outsublengths=sublengths
			outbegs=begs
			outends=ends
			brokenout=brokens
			for kk=imn,imx do begin
				outshocklocs[kk]=shockloc

				outimaxs[kk]=imx
				outimins[kk]=imn
				outchis[kk]=chi
				outnums[kk]=i

				outups[kk]=Bmin
				outdowns[kk]=Bmax
			endfor
		endfor


;;;catch,error_status
		if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			;xaaa=xs[N-1-ishock]
			errmsg=["error in curveFitter6 producing "+newName+" after fitting", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
			errorsaver,xs[0],errmsg
		endif
	print,"<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>"
	zb=reverse(nz)
	;znb=reverse(nzn)
	shocksb=reverse(nshocks)

	outSubsets=reverse(outSubsets)
	outends=reverse(outends)
	outbegs=reverse(outbegs)


	outimaxs=reverse(outimaxs)
	outimins=reverse(outimins)
	outchis=reverse(outchis)
	outnums=reverse(outnums)

	
	outshocklocs=reverse(outshocklocs)
	;outshocklocs2=outshocklocs
	;for i=0, N-1 do if outshocklocs2[i] ne -1 then outshocklocs2[i]=N-1-outshocklocs2[i]

	outdowns=reverse(outdowns)
	outups=reverse(outups)

	adjChiIn=fltarr(N);inchis*0.0
	adjChiOut=fltarr(N)
	for i=0,N-1 do if inchis[i] ne 0 then adjChiIn[i]= abs(inchis[i]-1)
	for i=0,N-1 do if outchis[i] ne 0 then adjChiOut[i]= abs(outchis[i]-1)
	chis=outchis
	for i=0 , N-1 do if inchis[i] ne 0 then chis[i] = inchis[i]
	adjChi=fltarr(N)
	for i=0,N-1 do if chis[i] ne 0 then adjChi[i]= abs(chis[i]-1)
	store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
	store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:"abs(REDUCED CHISQ-1)"}
	store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:"abs(REDUCED CHISQ-1)"}

	rl=""
	cl='c'
	if NOT rollup then begin
		 rl="-unrolled"
		cl='m'
	endif 

	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=z[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	;store_data,'shocks'+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	;store_data,'sublengths'+rl+'_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

	;store_data,'sublengths'+rl+'_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
	;store_data,'sublengths'+rl+'_inbound_end',data={x:xs,y:inends,ytitle:'flag'}


	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	help,dat
	store_data,newName+'_inbound',data={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inups:inups,indowns:indowns}
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
	result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	;znb=zb[b0:*]

	;print,"mean(zn)=",mean(znb),",  max(zn)=",max(znb)

	;store_data,'shocks'+rl+'_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	store_data,newName+'_shocks_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	;help,dat
	store_data,newName+'_outbound',data={x:xs,y:zb,ytitle:"Magnetic Field (nT)", outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,outups:outups,outdowns:outdowns}
	
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
	result = MOMENT(zf)
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
	;help,dat
	store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}

	;;;catch,error_status
	if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			;xaaa=xs[N-1-ishock]
			errmsg=["error in curveFitter5 producing "+newName+" after fitting", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
			errorsaver,xs[0],errmsg
			catch,/cancel
			return
	endif



	insublengths=insublengths.toarray()
	outsublengths=reverse(outsublengths.toarray())
	print,"insublengths=",insublengths
	if numel(insublengths) gt 0 then PRINT, 'Mean: ', mean(insublengths), ', Variance: ', stddev(insublengths)
	print,"outsublengths=",outsublengths
	if numel(outsublengths) gt 0 then PRINT, 'Mean: ', mean(outsublengths), ', Variance: ', stddev(outsublengths)


	store_data,"outnums",data={x:xs,y:outnums,ytitle:"shock number"}


	brokenin=brokenin.toarray()
	brokenout=reverse(brokenout.toarray())
	
	print,"brokenin"
	print,"i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
	print,brokenin

	print,"brokenout"
	print,"i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
	print,brokenout

	print,".oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo."
	print,'		finished creating ',newName
	print,"============================================"

	TOC, clock
	;wdelete
end

