


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

   
;;;;;;CHANGELOG:
;Nov16 2021,17:48:
; Commented out outbound boundary shrinking
; added extra condition to Working: "and (abs(yfit[0]-BU[ishock]) lt upDev)  "

;


pro curveFitter3, plt , newName=newName, curveType=curveType , doRoll=doRoll, doSecond=doSecond,Debug=Debug,minChi=minChi,fitnum=fitnum,secondOrder=secondOrder,doThird=doThird,orderCustom=orderCustom,minM0=minM0,doDefault=doDefault

	if NOT KEYWORD_SET(minM0) THEN minM0=0.7

	;CHANGELOG:

	maxUp=1
	minM1=1./(10*30.);.04

	g0=0
	
	upDev=2.2
	error_status=0
	minRemain=100
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_smoothed'
	if NOT KEYWORD_SET(fitnum) THEN fitnum='' else fitnum=string(fitnum)
	fitnum=fitnum.trim()
	if NOT KEYWORD_SET(minChi) THEN minChi=1.2
	rollup=0
	rollsec=0
	rollthird=0
	rollnone=0
	dobug=0

	doPerturb=0
	if not KEYWORD_SET(Debug) THEN dobug=0 else dobug =Debug
	if  KEYWORD_SET(secondOrder) THEN doPerturb=1 
	if KEYWORD_SET(orderCustom) then begin
		if Total(orderCustom eq -1) gt 0 then rollnone=1
		if Total(orderCustom eq 1) gt 0 then rollup=1
		if Total(orderCustom eq 2) gt 0 then rollsec=1
		if Total(orderCustom eq 3) gt 0 then rollthird=1
	endif else begin

		if KEYWORD_SET(doDefault) THEN begin
			orderCustom=[-1]
			rollnone=1
		
		endif else begin
			orderCustom=list()
			if KEYWORD_SET(doRoll) THEN begin
				rollup=1
				orderCustom.add,1
			endif
			if KEYWORD_SET(doSecond) THEN begin
				rollsec=1
				orderCustom.add,2
			
			endif
					
		
			if KEYWORD_SET(doThird) THEN begin
				orderCustom.add,0
				orderCustom.add,3
			
				rollthird=1                
			endif
			if numel(orderCustom) gt 3 then begin
				orderCustom.add,4
				orderCustom.swap,-1,-2
			endif
			if numel(orderCustom) gt 4 then begin
				orderCustom.add,5
				orderCustom.swap,-1,-2
			endif

			orderCustom.add,0

			orderCustom=orderCustom.toArray()
		endelse
	endelse
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	if NOT KEYWORD_SET(curveType) THEN curveType='tanhfit'
	typ=2
	get_data,plt,data=dat,limits=limits
;	get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
print,'=curveFitter3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=curveFitter3='
print,'newName=',newName,', rollup=',rollup,', rollsec=',rollsec,', dobug=',dobug
print,'=curveFitter3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=curveFitter3='
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

	inSubsets=fltarr(N);+1;-1
	inends=fltarr(N);+1;-1
	inbegs=fltarr(N);+1;-1

	;chiList=list()

	outSubsets=fltarr(N);+1;-1
	outends=fltarr(N);+1;-1
	outbegs=fltarr(N);+1;-1

	;debuginlen=fltarr(N)
	;debuginbeg=fltarr(N)
;	debuginend=fltarr(N)

	;debugoutlen=fltarr(N)
	;debugoutbeg=fltarr(N)
	;debugoutened=fltarr(N)
	
	outups=fltarr(N)
	outdowns=fltarr(N)
	inups=fltarr(N)
	indowns=fltarr(N)

	inparallel=fltarr(N)
	outparallel=fltarr(N)

	get_data,'mvn_swe_spec_dens_interpolated',data=datne
	n_e=datne.y
	nn_e=reverse(n_e)
	if (typ eq 2) then begin
		if dobug gt 1 then print,'type  2: curve fit'
		;.run tanhfit
;		get_data,'ascend_end',data=datae
;		get_data,'descend_end',data=datde
;
;		get_data,'ascend_begin',data=datab
;		get_data,'descend_begin',data=datdb

		get_data,"B_up_average",data=datBU
		BU=datBU.y
		nBU=REVERSE(BU)

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon

		mono=datMon.y
		bmono=datbMon.y

		get_data,"B_medianderiv_extrema",data=datEx
		extr=datEx.y
		nextr=(-1)*REVERSE(extr)
		get_data,'regid_cleaned_plot_interpolated',data=datReg
		reg=datReg.y

		get_data,'B5-3d10',data=datB5d      ;this data is Bmag smoothed over 5 second then 3 second average, then numerically derivativated then 10 second averaged
		get_data,'B20deriv',data=datB20deriv


		B5d=datB5d.y
		B20d=datB20deriv.y
		if dobug gt 1 then print,'numel(B5d)=',numel(B5d)
		get_data,'B_sec_smoothed' ,data=datBSb
		get_data,'B_sec_smoothed_back' ,data=datBS


		get_data,"B_3sec",data=datB3
		B3=datB3.y
		nB3=REVERSE(B3)

		get_data,'B_median',data=datBmed ;we want this data  for looking to see if we treat brief foreshocks as if they are the actual shock. 

		get_data,"rho30deriv",data=datRhod ; we want this data to avoid using crustal fields for downstream data:
		; if rho60 lt 1 then  few particles                                   
		get_data,"B_sec_smoothed_backderiv",data=datBsbd
		get_data,"B_sec_smoothedderiv",data=datBsd
		rhod=datRhod.y


		get_data,'B60derex',data=datderex
		derex=datderex.y

		for i=0,N-1 do if derex[i] le 0 then derex[i]=!values.F_NAN

		nderex=REVERSE(derex)

		get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_mean',data=datmm

		ymm=datmm.y
		nymm=reverse(ymm)

		Bmed=datBmed.y
		yBS=datBS.y
		yBSb=datBSb.y
		nyBS=reverse(yBSb)
		BSd=datBsd.y
		BSbd=datBSbd.y
	;------------------
		get_data,"forecountdown",data=datFore

		Fore=datFore.y

		nFore=reverse(Fore)

		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		trust=0
		
		if dobug gt 1 then print,'numel(B5d)=',numel(B5d)


		get_data,"foot_start_inbound",data=datfsi
		get_data,"foot_start_outbound",data=datfso


		get_data,'ascend_end_refined',data=datae
		get_data,'ascend_begin_refined',data=datab
		get_data,'descend_end_refined',data=datde
		get_data,'descend_begin_refined',data=datdb

		yae=datae.y
		yab=datab.y
		yde=datde.y
		ydb=datdb.y


		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'bDD_flag',data=datbDD
	;	


		ifoots=WHERE(datfsi.y ne 0, ifn)
		ofoots=WHERE(reverse(datfso.y) ne 0, ofn)

		
		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock


		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan, COMPLEMENT=nH_C, NCOMPLEMENT=nhcount_c); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn, COMPLEMENT=nK_C, NCOMPLEMENT=nkcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn, COMPLEMENT=nL_C, NCOMPLEMENT=nlcount_c); end of  negative  shock

		if dobug gt 1 then PRINT,'[ddn,aan,bbn]=',[ddn,aan,bbn]
		if dobug gt 1 then print,'numel(B5d)=',numel(B5d)

		meandevAA=0
		for i=1, aan -1 do meandevAA+=(AA[i]-AA[i-1])/aan

		meandevBB=0
		for i=1, bbn -1 do meandevBB+=(BB[i]-BB[i-1])/bbn

		meandevCC=0
		for i=1, ccn -1 do meandevCC+=(CC[i]-CC[i-1])/ccn

		meandevDD=0
		for i=1, ddn -1 do meandevDD+=(DD[i]-DD[i-1])/ddn


		meandev=mean([meandevAA,meandevBB,meandevCC,meandevDD])

		;;;ONE OF THE DIFFERENCES WITH curveFitter5 will be the lack of use of (b)DD_effective_flag
		eDD=DD
		enDD=nDD


		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD
	;------------------

		;'variables that would otherwise be defined in loops'

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

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,'a offsetf=',aoffsetf
;		print,'d offsetf=',doffsetf
		;print, 'bcutoff: ',bcutoff
		;print, 'fcutoff: ',fcutoff

		


		if dobug gt 1 then print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0

		lastiMax=0



	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME START!

	inimaxs=fltarr(N)-1
	outimaxs=fltarr(N)-1

	inimins=fltarr(N)-1
	outimins=fltarr(N)-1

	inchis=fltarr(N)+0
	outchis=fltarr(N)+0

	innums=fltarr(N)-1
	outnums=fltarr(N)-1
	
	inshocklocs=fltarr(N)-1
	outshocklocs=fltarr(N)-1
	

	inups=fltarr(N)
	indowns=fltarr(N)

	outups=fltarr(N)
	outdowns=fltarr(N)



	MMMs=nanarr(N,4)
	nMMMs=nanarr(N,4)
	bMMMs=reverse(nMMMs)
	inalgshock0locs=fltarr(N)-1
	outalgshock0locs=fltarr(N)-1
	inalgshock1locs=fltarr(N)-1
	outalgshock1locs=fltarr(N)-1


	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME END!








fullIter=min([ddn,aan,bbn])


	if DD[0] eq -1 then goto, NODIM

for i=1 ,fullIter-1 do begin
	j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetf
	l=i-1*(CC[0] gt BB[0])
	aj=AA[j]
	bi=BB[i]
	dim=DD[k]
	cl=CC[l]
	;cln=CC[l+1]
;print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	if dobug gt 1 then print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	
endfor


;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
if dobug gt 0 then begin 
;print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
;print,newName,'		inbound side			',newName
;print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
endif
		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
				errorloc=""
		for i=1,fullIter-1  do begin
			if aan le 1 then break
			;if dobug gt 0 then print,'-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-'
print,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
			error_status=0
			catch,error_status
			if error_status ne 0 then begin
			if i ge fullIter then i--
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[AA[i]]
print,'ERROR in curveFitter3 during i=',i
				;errorloc=""
			
				errmsg=["error in curveFitter3 inbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel
				errorloc=""
				continue
			endif
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
print,'i=',i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins

			bi=BB[i]; the address of where the fit will end and the next fit begins
			bim=BB[i-1]; the address where last fit ended
			dloc=i-1+doffsetf
			if AA[j] lt DD[dloc] then dloc--  
			dim=DD[dloc]
			if dim eq -1 then continue
			edim=eDD[dloc]
	
			n_ep=n_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then continue

			if dobug gt 1 then print,'numel(B5d)=',numel(B5d),', bi=',bi
			aj=AA[j] ; the index of the start trigger 

			if dobug gt 1 then print,'[dim,aj,bi,aj-dim,bi-dim]=',[dim,aj,bi,aj-dim,bi-dim]
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then continue
			wherefoot=(where(ifoots lt aj and ifoots ge dim,wcount))[-1]
			
			if wherefoot eq -1 then continue else foot=ifoots[wherefoot]
			;if wherefoot eq -1 then foot=dim else foot=ifoots[wherefoot]
;print,"foot,wcount=",foot,wcount
			bfoot=max([dim,foot-10*60]);120])
;print,"[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60]=",[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60.]
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			isFinite=(total(finite(ys[dim:bi]) ne 1) eq 0) and (total(finite(ys[dim:aj]) ne 1) eq 0) and (total(finite(ys[aj:bi]) ne 1) eq 0)

			if ~isFinite then begin
;print,"No MAVEN Mag Field Data here. WAT. Continuing"
				continue
			endif

			if total(finite(n_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(n_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif

			if 0 and total(ymm[bi] le ymm[dim:bi-1]) eq 0 then begin

				while total(ymm[bi] le ymm[dim:bi-1]) eq 0 and bi-aj gt 3*60 and B20d[bi] ne 0 do bi--

			endif
			bi=min([aj+40*60,bi])
			zeron=aj-dim
			yh=ys[aj]; the y value at the start trigger  
			;print,'gi=DD[i]=G[',i,']=',gi
			;print,'hj=H[',j,']=',hj
			;print,'gim=DD[i-1]=G[',i-1,']=',gim
			;yp=ys[gim:gi]
			yp=ys[dim:bi]
			np=size(yp,/n_el)

			;regp=reg[dim:bi]

			;if (min(regp) lt 2) then begin
			;	while (regp[0] gt 1) do begin

				;	dim++
				;	regp=regp[1:*]
				;;	yp=yp[1:*]
			;	endwhile
			;endif


			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)

			;want to make sure we don't include the crustal field in our measurements. When in crustal field, B increases while rho decreases
		;	CRUSTST=where((Bsd[aj+1:bi] gt 1.0) and (rhod[aj+1:bi] lt 0),ccount)+aj+1
			;if ccount gt 0  then begin
			;	bi=CRUSTST[0]
			;	yp=ys[dim:bi]
			;	np=numel(yp)
			;	yend=ys[aj:bi]
			;	nend=numel(yend)
			;endif
			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			;mbeg=nbeg
			;mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range
			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			brefines=0
			frefines=0
			incfunctions=1
			decfunctions=1
			;print,'(incfunctions,decfunctions)=',incfunctions,decfunctions
			np=numel(yp)
			bii=bi
			;jbegj=max([1,nend-20])+aj
			;jendj=min([N-1,nend+20+aj])
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 
				;ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
				;yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				;jbegj=max([1,nend-20])+aj
				;jendj=min([N-1,nend+20+aj])
				;print,'(brefines,frefines)=',brefines,frefines
				;print,'(incfunctions,decfunctions)=',incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				;if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;yp=yp[1:*] ;shrinks our range by one
 					;ybeg=ybeg[1:*]
					;np--
					;nbeg--
					;gim++
					;dim++
					;brefines++
					;if ( brefines ge 2 *mbeg/5) then begin
						;incfunctions=0
						;gim=DD[i-1]
						;dim=DD[i-1]
						;yp=ys[gim:gi]
						;yp=ys[dim:bi]
						
						;np=size(yp,/n_el)
						;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						;ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step						
						;nbeg=numel(ybeg)
			;			
					;endif
				;endif

				;if ((yend[nend-1] le mean(ybegtrust)+1) and (decfunctions eq 1))  then begin
				;if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
					;print,'pulling back the end of shock number i=',i,', from gi=',gi
					
					;yp=yp[0:np-2]
					;np--
					;yend=yend[0:nend-2]
					;nend--
					;gi--
					;bi--
					;frefines++
					;if ( frefines ge 1 *mbeg/5) then begin
						;decfunctions=0
						;gi=DD[i]
						;yp=ys[gim:gi]
						;bi=bii
						;yp=ys[dim:bi]
						;np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						;yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						;nend=numel(yend)
					;endif
				;endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				;if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			;print, 'the number of times we refined the beginning is ',brefines,'.'
			;print, 'the number of times we refined the end is ',frefines,'.'
			n_ep=n_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then continue
			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			;mn=mean(ybegtrust)
			if foot ne d00 then mn=BU[foot] else mn=BU[aj];mean(ybegtrust)
			;aj=FalseShockSkipper(aj,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
			aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
			if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt ymm[bi] )  do bi--

			if total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) le 1 or total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( ymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) then while max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt max(ymm[bi-20:bi]) )  do bi--
			ab=(aj+bi+bi)/3
			while ab lt bi and max(ymm[aj:ab])+5 lt max(ymm[ab:bi],abmax) do bi=abmax+ab-1
			while max(ys[aj-60:aj]) gt max(ys[aj:bi]) and aj-60 gt foot do aj--
			mn=BU[aj];mean(ybegtrust)
			wblwu=where(ymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1
			ymmn=min(ymm[mean([aj,bi]):bi],ymmnlc)
			ymmnlc+=mean([aj,bi])
			if ymmnlc lt bi-1 and ys[ymmnlc] lt mn*.75+.25*mean(ymm[aj:bi]) then bi=ymmnlc-1
			maxUp=max(ybegtrust)+2
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			;monp=mono[dim:bi]
			;bmonp=bmono[dim:bi]
			;monend=mono[aj:bi]
			;aj0=aj
			;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono) ; rolls predicted m2 (inflection point of atan up to 
			;aj1=aj
			;if dobug gt 1 then print,'numel(B5d)=',numel(B5d),', bi=',bi
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,B5d,dobug) ; second order correction
			;aj2=aj 
			;aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;aj3=aj
			;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; third order correction
			;aj4=aj 
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)

			;aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;aj5=aj
			;if dobug gt 1 then print,'[zeron,aj0-dim,aj1-dim,aj2-dim,aj3-dim,aj4-dim,aj5-dim]=',[zeron,aj0-dim,aj1-dim,aj2-dim,aj3-dim,aj4-dim,aj5-dim]
			;ybeg=ys[dim:aj]
			;yend=ys[aj:bi]
			;nbeg=numel(ybeg)
			;nend=numel(yend)
			aj1=aj
			if dobug gt 1 then print,'aj=',aj,', aj-dim=',aj-dim,', numel(yBS)=',numel(yBS)
			if dobug gt 1 then print,'[mono[aj-1],mono[aj],mono[aj+1]=',[mono[aj-1],mono[aj],mono[aj+1]]
			
			ypBS=yBS[mean([dim,aj]):bi]
			BSbeg=yBS[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			;print,nbeg,numel(BSbeg),numel(ybeg)
			;print,2 * (nbeg-1)/5, (nbeg-1)/2
			;BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			;BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;meanTbeg=mean(BSbegtrust)

			;meanTend=mean(BSendtrust)


			;stdbeg=finite(stddev(BSbegtrust,/nan))

			;stdend=finite(stddev(BSendtrust,/nan))
			;meanT=mean([meanTbeg,meanTend])
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=yBS[hhj]


			;aaj=aj
			;
			;BSh=yBS[aaj]


			;print,'numel(mono)=',numel(mono)
			;print,'[dim,bi]=',[dim,bi]

			;print,'total(monend)-numel(monend)=',total(monend),'-',numel(monend),'=',total(monend)-numel(monend)
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),'aj-dim=',aj-dim


			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),', aj-dim=',aj-dim
			;hj=hhj
			;print,'hj=',hj,', gim=',gim, ',gi=',gi,', size(xpa,/n_el)=',size(xpa,/n_el),', aj-dim=',aj-dim
			;xp=xs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 

			;aj=aaj
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),'aj-dim=',aj-dim
			xp=xs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 



			yc=yp*0.0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,'max=',mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=BU[aj];mean(ybegtrust)
			;mn=5
			;print,'min=',mn

			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,'m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
			m2=xpa[aj-dim]

			;print,'xpa[nbeg/2]=',xpa[nbeg/2]
			;print,'yp[nbeg/2]=',yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1.;.5;.1;1
			

			MM=[m0,m1,m2,m3]
			MM0=MM
			if dobug gt 0 then print,'to zeroth order, guess that MM=',MM0

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-10
			CHISQ=-1

			
			imin=dim
			imax=bi
			;help,xp
			;help,xpa
			;help,yp
			;help,MM
			;help,weight
			algshock0=aj
			algshock1=aj
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)


			;ishock=round(MM[2]/MM[1]+dim);m2) +dim
			;derv=dir(yfit,1,'mid')

			;gxg=where(derv eq max(derv))
			;ishock=gxg[0]+imin
;print,'status=',status,', chi^2=',CHISQ
;print, 'Function parameters: ', MM
;print,'m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
			;help,MM[1]
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
;print,"[dim,foot,ishock,ishock-bfoot,(ishock-bfoot)/60]=",[dim,foot,ishock,ishock-bfoot,(ishock-bfoot)/60.]
			badsl=0
			fracRemain=(bi-ishock)*1.0/(bi-dim)
				working=1
			if ishock gt bi or ishock lt foot then begin
;print,"ishock out of bounds,[dim,ishock,imax]=",[dim,ishock,imax]
					 ishock=aj;imax=min([bi,ishock+7*60])
					;MM[2]=MM[1]*(ishock-dim)
					;tanhfit, xpa, MM, yfit
;print,"reseting ishock->aj so that,"
;print,"[dim,ishock,imax]=",[dim,ishock,imax]
					working=0
				endif
			if working then begin
				B20de=B20d[ishock:imax]
			
				pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
						 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 

				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1) and imin lt foot-30 
			endif;1*( ((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ gt 0)) and (Fore[ishock] eq 1)  and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup and ~badsl and total(yfit) gt 0;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			if doPerturb then begin
				ishock1=ishock
				MM1=MM
				chi1=CHISQ
				yfit1=yfit
				working1=working
				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
				;MM1=MM
				;print,''
				;print,'Now for second order corrections'
				imin=max([dim,foot-3*60])

				smol=min(derex[ishock:bi],smoloc)

				;imin=max([dim,foot-3*60]);ishock-7*60])
				if finite(smol) eq 1 and smoloc gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])

				;imax=min([bi,ishock+7*60])
				while yBS[imin]-yBS[dim] gt 10 and imin gt foot-4*60 do begin
					imin--
				;ishock--
				;imax--
				endwhile
;print,"[dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
				ypp=ys[imin:imax]
				xpp=xs[imin:imax]

				xppa=xpp-xpp[0]
				;MM1=MM
				smmp=ymm[foot-30];imin:8*(ishock-imin)/10+imin]
				smypp=Bmed[foot-30];imin:8*(ishock-imin)/10+imin]
				mn=min(smmp)
				if 0 and MM[3]-MM[0]  lt min(smypp) or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);mn=min(smmp);smypp)
					if mn le 0 then mn=min(smypp)
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif


				MM=coeffCalc(xpp,ypp, ishock,x00)
				if 0 and MM[3]-MM[0]  lt min(smypp) or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);mn=min(smmp);smypp)
					if mn le 0 then mn=min(smypp)
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif

				weight=1.0/ypp

				;print,'to second order, guess that MM=',MM
				algshock1=MM[2]+imin
				;print,'[numel(xppa),numel(ypp),numel(weight)]=',[numel(xppa),numel(ypp),numel(weight)]
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				;print,'status=',status,', chi^2=',CHISQ;,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
				;PRINT, 'Function parameters: ', MM
				if dobug gt 0 then print,'MM0[2]-MM2[2]=',m2-MM[2],', MM0[2]/MM0[1]-MM2[2]/MM2[1]=',m2/m1-MM[2]/MM[1],',MM1[2]/MM1[1]-MM2[2]/MM2[1]=',MM1[2]/MM1[1]-MM[2]/MM[1]
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
				;ishock=round(MM[2]/MM[1]+dim);m2) +dim
				;derv=dir(yfit,1,'mid')
;print,"[dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
				badsl=0
				if ishock gt imax or ishock lt max([imin,foot]) then begin
					 ishock=ishock1;imax=min([bi,ishock+7*60])
					badsl=1
					algshock1=aj
				endif

				B20de=B20d[ishock:imax]
			pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 

				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1) and imin lt foot-30 and ~badsl;1*( ((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yBS[imin] - yBS[dim] lt 10) and (CHISQ gt 0)) and (Fore[ishock] eq 1)  and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup and MM[3]-MM[0] gt min(smmp)/2. and ~badsl and total(yfit) ne 0;(TOTAL(Bmed[ishock:min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)

				if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=bi

						xppa=xpa
						working=working1
						algshock1=algshock0
					endif 	


			endif
			if dobug gt 0 then begin
				
						
				;print,'[max,min,remain,fracRemain,imin,imax]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim),imin,imax]=',[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain,imin,imax]
			endif
			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			printed='unsaved'
			;((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1)   )
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
			;if ishock lt imin then print, 'bad ishock: ishock = ',ishock,' lt ', imin,'= imin'
			;if ishock gt imax then print, 'bad ishock: ishock = ',ishock,' gt ', imax,'= imax'

			;if ((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) then begin
		

			if working then begin
;print,'[imin,foot,ishock,imax]=',[imin,foot,ishock,imax]
;print,"working,yfit[ishock-imin]=",yfit[ishock-imin]
				lastiMax=imax
;				inSubsets[dim:bi]=1
;				inbegs[dim:aj1]=1
;				inends[aj1:bi]=1
;				insublengths.add,bi-dim
				;inSubsets[imin:imax]=1
				inbegs[imin:ishock]=1
				inends[ishock:imax]=1


				;inimaxs[imin:imax]=imax
				;inimins[imin:imax]=imin
				;inchis[imin:imax]=CHISQ
				;;innums[imin:imax]=i
				;inshocklocs[imin:imax]=ishock
				;inups[imin:imax]=MM[3]-MM[0]
				;indowns[imin:imax]=MM[3]+MM[0]
				for k=imin,imax do begin
					inSubsets[k]=1
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
				insublengths.add,imax-imin
				printed=' saved '
				if( ncount eq 0) then begin; and (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) then begin
					;print,'no NANs'
					;for k=gim,gi-1 do z[k]=yfit[k-gim]	

					;for k=dim,bi-1 do z[k]=yfit[k-dim]	
					for k=imin,imax-1 do z[k]=yfit[k-imin]	

				endif else begin
					;print,'yfit has ',ncount,' NANs. calculating directly.'

					;if (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) ge 1000) then MM=MM0
					tanhfit,xppa,MM,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]

					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]
				
				endelse
 				
				if dobug gt 0 then print,'ishock=',ishock
				shocks[ishock]=1
				;'in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations'
				if(i eq 1) then begin
						zwidth =(imax-imin)-MM[3]
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=z
				endif

			endif; else brokenin.add,[i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			;chiList.add,[newName,' forward ',printed, string(i),string(CHISQ)]
			;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
			;debugi=i-(1.0-working)/4.0+1
			;debuginlen[imin:imax]=debugi
			;debuginlen[ishock]=-debugi
			;inSubsets[dim:bi-1]=z[dim:bi-1]
			;inbegs[dim:aj1-1]=z[dim:aj1-1]
			;inends[aj1:bi-1]=z[aj1:bi-1]
			

		endfor
	 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
		;print,'now for the range ys[0:DD[0]]'
		;'now for the range ys[0:DD[0]]'
		b0=BB[0]
		d0=DD[0]
		aj=AA[0]
		;dim=0
		if d0 lt b0 then dim=d0 else dim=0 
		yp=ys[dim:b0]
		xp=xs[dim:b0]
		np=size(yp,/n_el)
		edim=dim

		;aj=AA[0]
		isFinite=total(finite(yp) ne 1) eq 0;(total(finite(ys[dim:b0])) ne 0) and (total(finite(ys[dim:aj])) ne 0) and (total(finite(ys[aj:b0])) ne 0)
		;'start with standard case'
		error_status=0
		errorloc=""
		error_status=0
		catch,error_status
		ishock=0
		if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
print,'ERROR WHEN PERFORMING '+errorloc
			errorloc=""
			xaaa=xs[min([N-1-ishock,N-1])]
			errmsg=["ERROR in curveFitter3 inbound 0 while producing "+newName ,'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc]
			errorsaver,xaaa,errmsg,/spec
			catch,/cancel
		endif
		;IF (aoffsetf eq 0) and ~error_status and isFinite then begin
		while (aoffsetf eq 0) and ~error_status and isFinite do begin;then begin
				;print,"no MAVEN Magnetic field data here. Continuing"
				;continue
			if dobug gt 0 then print,'>-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]->'
			i=0
			if dobug gt 1 then print,'now for the range ys[dim:BB[0]]=ys[0:',b0,']' else print,'i=',0,''

			;print,gi
			;print,'gim=',gim
			
;			dim=0
				wherefoot=(where(ifoots lt aj and ifoots ge dim))[-1]
			;foot=ifoots[()[0]]

			;if wherefoot eq -1 then foot=dim else foot=ifoots[wherefoot]

			if wherefoot eq -1 then break else foot=ifoots[wherefoot]

			bfoot=max([dim,foot-10*60]);120])
			d00=dim

			if foot ne d00 then mn=BU[foot] else mn=BU[aj];mean(ybegtrust)
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			;print,'g0=',b0
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

				while  total(ymm[b0] le ymm[dim:b0-1]) eq 0  and b0-aj gt 3*60 and B20d[b0] ne 0 do b0--

			endif
			b0=min([aj+40*60,b0])
			zn=z[b0:*]
			regp=reg[dim:b0]
			;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
			;if (min(regp) lt 2) then begin
			;	while (regp[0] gt 1) do begin
			;		if dim -1 eq aj then break
			;		dim++
			;		regp=regp[1:*]
			;		yp=yp[1:*]
			;	endwhile
			;endif

			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			

			;want to make sure we don't include the crustal field in our measurements. When in crustal field, B increases while rho decreases
			CRUSTST=where((Bsd[aj+1:b0] gt 1.0) and (rhod[aj+1:b0] lt 0),ccount)+aj+1
			if ccount gt 0  then begin
				b0=CRUSTST[0]
				yp=ys[dim:b0]
				np=numel(yp)

			endif


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
			zeron=aj-dim
			bi=b0
			d00=dim
			np=numel(yp)
			jbegj=max([1,nend-20])+aj
			jendj=min([N-1,nend+20+aj])
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 
			;	ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			;	yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;	jbegj=max([1,nend-20])+aj
			;	jendj=min([N-1,nend+20+aj])
				;print,'(brefines,frefines)=',brefines,frefines
				;print,'(incfunctions,decfunctions)=',incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
			;	if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,'pushing forward the end of shock number i=',0,', from gim=',gim,', where aj=',aj,', and g0=',g0, 'and size(yp)=',size(yp,/n_el)
			;		yp=yp[1:*] ;shrinks our range by one
 			;		ybeg=ybeg[1:*]
			;		np--
			;		nbeg--
			;		dim++
			;		brefines++
			;		if ( brefines ge 2 *mbeg/5) and (dim +1 eq aj) then begin
			;			incfunctions=0
			;			dim=d00
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nbeg=numel(ybeg)
			;			
			;		endif
			;	endif

				;ybeg;if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
			;	if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
					;print,'pulling back the end of shock number i=',i,', from gi=',gi
			;		yp=yp[0:np-2]
			;		np--
			;		yend=yend[0:nend-2]
			;		nend--
			;		bi--
			;		frefines++
			;		if ( frefines ge 1 *mbeg/5) then begin
						;print,'i=',i,', ddn=',ddn
			;			decfunctions=0
			;			bi=b0
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nend=numel(yend)
			;		endif
			;	endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			edim=(where(eDD lt aj and eDD ge d00))[-1]
			
			if d00 eq 0 or edim eq -1 then edim=d00
			n_ep=n_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then break
			b0=bi
			;print, 'the number of times we refined the beginning is ',brefines,'.'
			;print, 'the number of times we refined the end is ',frefines,'.'

			yc=yp*0.0

			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),'aj-dim=',aj-dim
;			if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono)
;			if (rollsec eq 1) then aj=secondRoll(aj,bi,B5d,dobug) ; second order correction

			;aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; second order correction  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;mn=mean(ybegtrust)
			;aj=FalseShockSkipper(aj,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
;			aj=ajRefiner(foot,max([edim,foot]),aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
			aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)

if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt ymm[bi])  do bi--

			if total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) le 1 or total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( ymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) then while max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt max(ymm[bi-20:bi]) )  do bi--
			ab=(aj+bi+bi)/3
			while ab lt bi and max(ymm[aj:ab])+5 lt max(ymm[ab:bi],abmax) do bi=abmax+ab-1

			while max(ys[aj-60:aj]) gt max(ys[aj:bi]) and aj-foot gt 30 and aj gt 60 do aj--
			b0=bi
			mn=BU[aj];mean(ybegtrust)
			wblwu=where(ymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1
			bi=b0
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			xp=xs[dim:bi]
			xpa=xp-xp[0]
			ybeg=ys[dim:aj]
			yend=ys[aj:bi]
			nbeg=numel(ybeg)
			nend=numel(yend)
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),'aj-dim=',aj-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(yendtrust)
			;mx=25
			;print,'max=',mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5


			;mn=5
			;print,'min=',mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,'m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			m2=xpa[aj-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1.;.5;.1;1
			MM=[m0,m1,m2,m3]
			;print,'to zeroth order, guess that MM=',MM
			MM0=MM
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-19
			CHISQ=-1
			algshock0=aj
			algshock1=aj
			if DD[0] ne -1 then yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,'status=',status,', chi^2=',CHISQ
;print, 'Function parameters: ', MM
			if dobug gt 0 then print,'m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
			;ishock=round(MM[2]/MM[1]) +dim
			imin=dim
			imax=bi

			ishockM=round(MM[2])+imin;round(MM[2]/MM[1])+imin
			;print,"ishock,time=",ishock,",",x2greg(xs[ishock],/str)
			;derv=dir(yfit,1,'mid')
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
			;print,"ishock,time=",ishock,x2greg(xs[ishock],/str)

			if abs(ishock-ishockM) gt 3*60 then begin
				if (ishock le foot) and ishockM gt foot then ishock=ishockM else $
				if (ishock le foot) and ishockM le foot then begin
					ishock=aj
					working=0
				endif
				MM2=MM
				MM2[2]=ishock-dim;MM[1]*(ishock-dim)
				;MM_22=MM2[2]
					;tanhfit, xpa, MM, yfit
				;MM2[2]=xpa[maxloc2+ishock-11-dim]
				yfit2=CURVEFIT(xpa, yp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)
				working2 = ishock2+1.2/MM2[1] lt bi and (status2 eq 0) and (MM2[1] gt 0) and ( MM2[0] gt minM0) and (ishock2 gt imin) and (ishock2 lt imax) and (abs(MM2[3]-MM2[0]-mn) lt maxUp) and (MM2[1] gt minM1) and (CHISQ2 lt minChi) and (bi-ishock2 gt minRemain) and (Fore[ishock2] eq 1) and (MM2[3]-MM2[0]-mn lt 10) and imin lt foot-30
				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and working2 then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2
					ishock=ishock2
					working=working2
					algshock1=ishockM
				endif
			endif 
			;working=1
			if ishock le foot or ishock ge bi then begin

				ishock=aj
				working=0
			endif else begin
			;gxg=where(derv eq max(derv))
			;ishock=gxg[0]+imin
			
				B20de=B20d[ishock:imax]
				pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 


				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and (Fore[ishock] eq 1) and (MM[3]-MM[0]-mn lt 10) and imin lt foot-30;1*(((status eq 0) and (MM[0] gt minM0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and $
			endelse
			;( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and  $
			;(MM[3]-MM[0]-mn lt 10) and (yBS[imin] - yBS[dim] lt 10) and (CHISQ gt 0)) and (Fore[ishock] eq 1) $
			;and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			chi1=CHISQ
			if doPerturb then begin
				ishock1=ishock
				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
				smol=min(derex[ishock:bi],smoloc)

				imin=max([dim,foot-3*60]);ishock-7*60])
				if finite(smol) eq 1 and smoloc gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])
				;imin=max([dim,foot-3*60])
				;imax=min([bi,ishock+7*60])
				while yBS[imin]-yBS[dim] gt 2 and imin ge foot-3*60 do imin--
				

				if 1 or imin gt ishock then begin
					sunix=xs[ishock]
					funix=xs[foot]
					minunix=xs[imin]
					;print,"corrected aj at ",x2greg(xs[aj],/str)
					;print,"shock at ",x2greg(sunix,/str)
					;print,"foot at ",x2greg(sunix,/str)
					;print,"imin at ",x2greg(minunix,/str)
					if dobug eq 10 then return
				endif
				;ishock--
				;imax--

				MM1=MM

				yfit1=yfit
				working1=working
				;MM1=MM
;print,"[dim,imin,foot,aj,ishock,imax,bi]=",[dim,imin,foot,aj,ishock,imax,bi]
				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				mmpp=ymm[imin:imax]
				xppa=xpp-xpp[0]
				;MM1=MM

				;smypp=smooth(ypp,2,/edge_mirror)
				;smypp=smypp[0:8*(ishock-imin)/10]
;print,"[imin,.8*ishock+.2*imin,ishock,numel(Bmed)]=",[imin,.8*ishock+.2*imin,ishock,numel(Bmed)]
				smypp=Bmed[imin:.8*ishock+.2*imin]
				smmp=ymm[imin:.8*ishock+.2*imin]
				mn=min(smmp)
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin;if 0 and MM[3]-MM[0]  lt
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
				if 0 and MM[3]-MM[0]  lt min(smmp) then begin
					mx=MM[3]+MM[0]
					;mn=mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					mn=min(smmp);smypp)
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				if 0 and MM[3]-MM[0]  gt smmp[0]+1 then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

				MM=coeffCalc(xpp,ypp, ishock,x00)

				if MM[3]-MM[0]  lt min(smmp) then begin
					mx=MM[3]+MM[0]
					;mn=mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					mn=min(smmp);smypp)
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				if  MM[3]-MM[0]  gt smmp[0]+1 then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif


			;	smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin;if 0 and MM[3]-MM[0]  lt
				if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);mn=mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MMtt=(mx+mn)/2
					MMzz=(mx-mn)/2
					if MMzz gt 0 then begin
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

				endif
				weight=1.0/ypp
;print,'to second order, guess that MM=',MM

				if dobug gt 0 then print,'[numel(xppa),numel(ypp),numel(weight)]=',[numel(xppa),numel(ypp),numel(weight)]
;print,chi1
				yfit1=yfit
				algshock1=MM[2]+imin
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,'status=',status,', chi^2=',CHISQ
;print, 'Function parameters: ', MM
				;print,'MM0[2]-MM2[2]=',m2-MM[2],', MM0[2]/MM0[1]-MM2[2]/MM2[1]=',m2/m1-MM[2]/MM[1],',MM1[2]/MM1[1]-MM2[2]/MM2[1]=',MM1[2]/MM1[1]-MM[2]/MM[1]
				ishock1=ishock
				ishock=round(MM[2]/MM[1]) +imin
				;derv=dir(yfit,1,'mid')
;print,"ishock,imax=",ishock,imax,numel(B20d)
				;badsl=0
				working1=working
				working=1
				if ishock gt imax or ishock lt imin then begin
					 ishock=ishock1;imax=min([bi,ishock+7*60])
					working=0;badsl=1
				endif else begin
;print,"dim,imin,foot,ishock,ishock-imin,imax,bi,=",dim,imin,foot,ishock,imax,bi,ishock-imin,numel(B20d)
				B20de=B20d[ishock:max([imax,ishock+151])]
				pointerinup=( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,max([imax,ishock+121])])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,max([imax,ishock+61])])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,max([imax,ishock+61])-ishock])], B20de[SORT(B20de[0:min([60,max([imax,ishock+61])-ishock])])]) )) 



				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1) and imin lt foot-30 
				endelse;1*(((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and  (MM[3]-MM[0]-mn lt 10) and (yBS[imin] - yBS[dim] lt 10) and (CHISQ gt 0)) and (Fore[ishock] eq 1) and (abs(yfit[0]-BU[ishock]) lt upDev) and (~pointerinup) and ~badsl and total(yfit) gt 0 and yfit[ishock-imin] gt 0
			


		if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=bi
						working=working1
						algshock1=algshock0
					endif 	


				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
			endif


			fracRemain=(imax-ishock)*1.0/(imax-imin)




			if dobug gt 0 then begin
;print,'[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=',[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			endif
			;printed='unsaved'
			;print,'abs(MM[2]/MM[1]-MM0[2]/MM0[1])=',abs(MM[2]/MM[1]-MM0[2]/MM0[1]),', 1*(abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000)=',1*(abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) 
			;working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi);1*(((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and (MM[3]-MM[0]-mn lt 10) and (yBS[imin] - yBS[dim] lt 10) and (CHISQ gt 0)) and (Fore[ishock] eq 1) and (abs(yfit[0]-BU[ishock]) lt upDev) and (TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0);((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (imax-ishock gt minRemain) and ( MM[0] gt 1))
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
			if ishock lt imin then print, 'bad ishock: ishock = ',ishock,' lt ', imin,'= imin'
			if ishock gt imax then print, 'bad ishock: ishock = ',ishock,' gt ', imax,'= imax'

					printed=' unsaved '
			if working and (ishock lt bi) and (ishock gt dim)  then begin
;print,"working, yfit[ishock-imin]=",yfit[ishock-imin]
					ishockt1=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
					if abs(ishockt1-ishock) gt 2 then begin
						imin+=ishock-ishockt1
						imax+=ishock-ishockt1
					endif
				if lastiMax eq 0 then lastiMax=imax
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin;and (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) then begin
				;print,'no NANs'
			;	print,'gim=',gim,', g0-1=',b0-1
					printed=' saved '
					;print,"saving z from [imin,imax]=[",imin,",",imax,"]"
					for k=imin,imax-1 do z[k]=yfit[k-imin]
					;print,"z[imin+1],z[ishock]=z["+strtrim(ishock,2)+"],z[imax-1]=",z[imin+1],z[ishock],z[imax-1]
				endif else begin
;print,'yfit has ',ncount,' NANs. calculating directly.'
					;if (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) ge 1000) then MM=MM0
					tanhfit,xpa,MM,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]
					zn=z[imax:*]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]


				;	print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)


						
				endelse
				insublengths.add,imax-imin
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
				;ishock=round(MM[2]/MM[1])+imin
				shocks[ishock]=1
				;print,'g0'
				zn=z[b0:*]

				;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
				for k=dim,bi do inSubsets[k]=1
				for k=dim, ishock-1 do inbegs[k]=1
;print,"aj,b0-1=",aj,b0-1
				for k=ishock, bi do inends[k]=1
			endif; else brokenin.add,[i,imin,imax, xs[imin]-min(xs),xs[imax]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			;chiList.add,[newName,'forward',printed,string(i),string(CHISQ)]
			;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
			;debugi=i-(1.0-working)/4.0+1
			;for k=imin,imax do debuginlen[k]=debugi
			;debuginlen[ishock]=-debugi
			;inSubsets[dim:b0-1]=i
			;inbegs[dim:aj-1]=i
			;inends[aj:b0-1]=i
			break
		endwhile;endif ;else begin 
		while 0 do begin
			break
			if dobug gt 0 then print,'>-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]->'
			i=0
;print,'i=',0,'. Start Trigger was cut off'
			;'if the trrigger pair WAS cut off, we try to approximate results here'
			;'want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth'
			onstep=1*(b0 le zwidth-4)
			;'if onstep eq 1, can assume without loss of generality that we are indeed on the step'
			;ty=yp
			yp=ys[dim:b0]
			xp=xs[dim:b0]
			xpa=xp-xp[0]
			np=size(yp,/n_el)
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,b0-1 do z[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with m2=0 
				
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM

				bi=b0
				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				ishock=round(MM[2]+dim);round(MM[2]/MM[1]+dim);m2) +dim
				;derv=dir(yfit,1,'mid')
				imin=dim
				imax=b0
				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
				fracRemain=(bi-ishock)*1.0/(bi-dim)
				printed='unsaved'
				if dobug gt 0 then begin
;print,'to zeroth order, guess that MM=',MM
;print, 'Function parameters: ', MM
;print,'status=',status,', chi^2=',CHISQ,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
;print,'[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=',[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
				endif

				;print,'status=',status
				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi);1*(((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1) and (CHISQ gt 0)) and (abs(yfit[0]-BU[ishock]) lt 4)
				if working then begin



				
					insublengths.add,bi-dim


					NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					printed=' saved '
					if( ncount eq 0) then begin
						for k=0,b0-1 do z[k]=yfit[k]	

					endif else begin
			;		print,'yfit has ',ncount,' NANs. calculating directly.'
						tanhfit,xp,MM,F
					
						for k=0,b0-1 do z[k]=F[k]
					
					endelse
					;ishock=round(MM[2]/MM[1]) +dim <-no reason this should exist

					shocks[ishock]=1

				for kk=imin,imax do begin
					inimaxs[kk]=imax
					inimins[kk]=imin
					inchis[kk]=CHISQ
					innums[kk]=i
					inshocklocs[kk]=ishock
					inups[kk]=MM[3]-MM[0]
					indowns[kk]=MM[3]+MM[0]
					inSubsets[kk]=1
					
				endfor
					inbegs[imin:ishock]=1
					inends[ishock:imax]=1

				endif ;else brokenin.add,[0,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]	
				;chiList.add,[newName,' forward ',printed, string(i),string(CHISQ)]			
				;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
				;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
				;debugi=i-(1.0-working)/4.0+1
			;	debuginlen[imin:imax]=debugi
				;debuginlen[ishock]=-debugi


			endelse
			;inSubsets[dim:b0-1]=i
			;inbegs[dim:aj-1]=i
			;inends[aj:b0-1]=i
	

		endwhile
		

		;'finally, we need the remaining bit after DD[ddn-1]'

		;'in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well'
		;'set the remainder to zero for now'
		zn=z[b0:*]

		;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
		;if DD[ddn-1] gt BB[bbn-1] then GN=DD[ddn-1] else GN=BB[bbn-1]
		;yp=ys[GN:*]
		;print,GN
		for k=lastiMax+1,N-1 do z[k]=0
		zn=z[b0:*]

		;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do z[k]=0 ; 'if number of remaining element is much less than a period, then the step won't arrive'

		;else begin
		;	;'what we do depends on whether fcutoff eq 0'
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif


		;endelse

NODIM: print,'NO DIMS'

;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------

	inslocs=where(shocks ne 0,numins)

	if numins gt 0 then begin
;print,"the magnitudes at the shocks are"
		foreach el,inslocs do print,"B[ishock]=B["+strtrim(el,2)+"]=",z[el]

	endif else print, "no inbounds"
print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
print,newName,'		outbound side			',newName
print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
;nxs=Reverse(xs)
nxs=xs

nreg=reverse(reg)
nBmed=reverse(Bmed)
nys=REVERSE(ys)
;nyae=reverse(yae)
;nyab=reverse(yab)
;nyde=reverse(yde)
;nydb=reverse(ydb)
;nyBS=REVERSE(yBSb);REVERSE(yBS)

nrhod=REVERSE(rhod)*(-1)
nBsbd=REVERSE(Bsbd)*(-1)

nB20d=-1* REVERSE(B20d)

;nAA = WHERE(nyae ne 0, naan, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
;		print,yae[DD]

;nDD = WHERE(ydb ne 0, nddn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

;nBB = WHERE(yab ne 0, nbbn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
;nCC = WHERE(yde ne 0, nccn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
nmono=reverse(mono);bmono)
nB5d=reverse(B5d)*(-1)


;print,'size(nys)=',size(nys)



;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]

;nDD=N-1-reverse(AA)
;nAA=N-1-reverse(DD)
;nBB=N-1-reverse(CC)
;nCC=N-1-reverse(BB)

nz=fltarr(N)
nshocks=fltarr(N)
;dat.x=xs
lastiMax=-1
ni9shock=-1
;print,'nAA=',nAA
;print,'nBB=',nBB
;print,'nCC=',nCC
;print,'nDD=',nDD
;print,"[nDD[0],nAA[0],nBB[0]]=",[nDD[0],nAA[0],nBB[0]]
doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if numel(nBB) gt 1 then dboffsetb=1*(nBB[1] lt nDD[0]) else  dboffsetb=0
if numel(nAA) gt 1 then daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0
if dobug gt 1 then print,'numel(nys)=',numel(nys)
if dobug gt 1 then print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
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
;Return
if nDD[0] eq -1 then goto, NONDIM
;stop
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;print,"dobug=",dobug
		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		errorloc=""
		for i=1,fullIter-1  do begin
			if i ge fullIter then break
			catch,error_status
			if error_status ne 0 then begin
			if i ge fullIter then i--
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
print,'ERROR in curveFitter3 during i=',i,", numel(nAA)=",numel(nAA),naan,nbbn,nddn,fullIter-1
				xaaa=xs[nAA[i]]


			
				errmsg=["error in curveFitter3 outbound while producing "+newName,"during i="+strtrim(i,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"ERROR LOCATION at"+errorloc]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel
				errorloc=""
				continue
			endif


			if dobug gt 0 then print,'((((((((((((((((((((((((((((((((('
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
print,'i=',i
			;gi=nDD[i]; the address of where the fit will end and the next fit begins
			;gim=nDD[i-1]; the address where this fit begins
			while nBB[i+dboffsetb] lt nAA[j] do begin
;print,'[nBB[i] , nAA[j]]=',[nBB[i] , nAA[j]]
				i++
			endwhile
			bi=nBB[i+dboffsetb]; the address of where the fit will end and the next fit begins
			bim=nBB[i-1+dboffsetb]; the address where last fit ended

			dloc=i-1+doffsetb
			while nAA[j] lt nDD[dloc] and dloc gt 0 do dloc--  
			if nAA[j] lt nDD[dloc] then dim=0 else dim=nDD[dloc]
;			dim=nDD[dloc];+doffsetb]
			
			
			if dim eq -1 or dim gt bi then begin
;print,"dim eq -1" 
				continue
			endif
			n_ep=nn_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then continue


			;edim=dim
			edim=enDD[dloc]
			aj=nAA[j] ; the index of the start trigger 
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then continue

			if total(finite(nys[dim:bi]) ne 1) ne 0 or total(finite(nys[dim:aj]) ne 1) ne 0 or total(finite(nys[aj:bi]) ne 1) ne 0 then begin
;print,"no MAVEN Magnetic field data here. Continuing"
				continue
			endif




			if dobug ge 1 then print,'[dim,aj,bi]=',[dim,aj,bi]
			wherefoot=(where(ofoots lt aj and ofoots ge dim))[-1]
			;foot=ofoots[()[0]]

			;if wherefoot eq -1 then foot=dim else foot=ofoots[wherefoot]
			if wherefoot eq -1 then continue else foot=ofoots[wherefoot]
			bfoot=max([dim,foot-10*60]);120])
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])

			if total(finite(nn_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(nn_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif
			
			if nymm[dim] le 0 or finite(nymm[dim]) ne 1 then begin
				KK=where(nymm[dim:aj] gt 0)
				if foot ne d00 then begin
					LL=where(xs[dim:aj] lt xs[foot])
					SS=INTERSECT(KK,temporary(LL))

					if SS[0] ne -1 then dim=SS[0]+dim else begin
						continue
					endelse
				endif else dim=KK[0]+dim

			endif

			if 0 and total(nymm[bi] le nymm[dim:bi-1]) eq 0 then begin

				while total( nymm[bi] le nymm[dim:bi-1]) eq 0 and bi-aj gt 3*60 and nB20d[bi] ne 0 do bi--

			endif
			yh=nys[aj]; the y value at the start trigger  
			;print,'aj=nAA[j]=nBB[',j,']=',aj
			;print,'bi=nBB[i]=nBB[',i,']=',bi
			;print,'hj=H[',j,']=',hj
			;print,'dim=nDD[i-1]=nDD[',i-1,']=',dim
			;yp=nys[gim:gi]
			if dobug ge 1 then print,'[dim,bi,bi-dim]',[dim,bi,bi-dim]
			yp=nys[dim:bi]
			np=size(yp,/n_el)
			regp=nreg[dim:bi]
			if (min(regp) lt 2) then begin
				while (regp[0] gt 2) do begin

					dim++
					regp=regp[1:*]
					yp=yp[1:*]
				endwhile
			endif

			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			

			;want to make sure we don't include the crustal field in our measurements. When in crustal field, B increases while rho decreases
			CRUSTST=where((nBsbd[aj+1:bi] gt 1.0) and (nrhod[aj+1:bi] lt 0),ccount)+aj+1
			if ccount gt 0  then begin
				bi=CRUSTST[0]
				yp=nys[dim:bi]
				np=numel(yp)
				
			endif
			bii=bi
			bi=min([aj+40*60,bi])
			n_ep=n_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then continue
			;yend=nys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=nys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			;mbeg=nbeg
			;mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region


			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			brefines=0
			frefines=0
			incfunctions=1
			decfunctions=1
			zeron=aj-dim
			d00=dim
			np=numel(yp)
			mend=nend
;print,"numel(yend),nend=",numel(yend),nend
			;print,'[dim,bi]=',[dim,bi]
			;while  ((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((nBmed[aj+nend-1] le mean(ybegtrust)+1.1) and (decfunctions eq 1)) do begin 

			;	ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
				
			;	yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				;jbegj=max([1,nend-20])+aj
				;jendj=min([N-1,nend+20+aj])
				;print,'[dim,bi]=',[dim,bi]
				;print,'(brefines,frefines)=',brefines,frefines
				;print,'(incfunctions,decfunctions)=',incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				;if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
				;	yp=yp[1:*] ;shrinks our range by one
 				;	ybeg=ybeg[1:*]
				;	np--
				;	nbeg--
					;gim++
				;	dim++
				;	brefines++
				;	if ( brefines ge 2 *mbeg/5) or (np-3 eq 1) or (nbeg-3 eq 1)  then begin
				;		incfunctions=0
						;gim=nDD[i-1]
				;		dim=d00
						;yp=nys[gim:gi]
				;		yp=nys[dim:bi]
						
				;		np=size(yp,/n_el)
						;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step						
				;		nbeg=numel(ybeg)
			;	;		
				;	endif
				;endif

				;if ((yend[nend-1] le mean(ybegtrust)+1) and (decfunctions eq 1)) or (nend-3 eq 1) then begin
				;if ((nBmed[aj+nend-1] le mean(ybegtrust)+1.1) and (decfunctions eq 1)) then begin
					;print,'pulling back the end of shock number i=',i,', from gi=',gi
					;print,"np=",np
					;print,"size(yp,/n_el)=",size(yp,/n_el)
				;	np--
					;print,"np=",np
					;print,"size(yp,/n_el)=",size(yp,/n_el)
				;	yp=yp[0:np-1]
				;	if nend ne numel(yend) then print,"numel(yend),nend=",numel(yend),nend
				;	nend--
				;	yend=yend[0:nend-1]
					
					;gi--
				;	bi--
				;	frefines++
				;	if ( frefines ge 1 *mbeg/5) or (nend-3 eq 0) or (np-3 eq 0) then begin
				;		decfunctions=0
						;gi=nDD[i]
						;yp=nys[gim:gi]
				;		bi=bii
				;		yp=nys[dim:bi]
				;		np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		yend=nys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		nend=numel(yend)
				;	endif
				;endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				;if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			;print, 'the number of times we refined the beginning is ',brefines,'.'
			;print, 'the number of times we refined the end is ',frefines,'.'
			;print,'[dim,bi]=',[dim,bi]

			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=nyBS[hhj]



			;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,nmono)
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,nB5d,dobug) ; second order correction 
			

			;aaj=aj
			;aaj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)
			;if (rollthird eq 1) then aaj=Third_Roll(nB20d,dim,aaj,bi,dobug) ; third order correction 
			;aaj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el)
			;hj=hhj
			;print,'hj=',hj,', gim=',gim, ',gi=',gi,', size(xpa,/n_el)=',size(xpa,/n_el)
			;xp=nxs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; second order correction
			nend=numel(yend)  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			mn=mean(ybegtrust)
			;aj=FalseShockSkipper(aj,bi,nBmed,median(nBmed[2 * nbeg/5+dim: nbeg/2+dim]))
			errorloc="aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,nyBS,nbeg,nend, nmono,nB5d,nB20d,orderCustom,nextr,nrhod,nFore)"
			aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,nyBS,nbeg,nend, nmono,nB5d,nB20d,orderCustom,nextr,nrhod,nFore)
			if nymm[bi] eq max(nymm[aj:bi]) then while nymm[bi] eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(nymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt nymm[bi] )  do bi--

			if total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) le 1 or total(abs(nymm[aj]-nymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(nymm[aj]-nymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( nymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) then while max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(nymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt max(nymm[bi-20:bi]) )  do bi--
			;aj=aaj

			ab=(aj+bi+bi)/3
			errorloc="while ab lt bi and max(nymm[aj:ab])+5 lt max(nymm[ab:bi],abmax) do bi=abmax+ab-1"
			while ab lt bi and max(nymm[aj:ab])+5 lt max(nymm[ab:bi],abmax) do bi=abmax+ab-1
			errorloc="while max(nys[aj-60:aj]) gt max(nys[aj:bi]) do aj--"
			while max(nys[max([aj-60,foot]):aj]) gt max(nys[aj:bi]) and aj gt foot do aj--
			mn=nBU[aj];mean(ybegtrust)
			wblwu=where(nymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1
			yp=nys[dim:bi]
			np=size(yp,/n_el)
			ybeg=ys[dim:aj]
			yend=ys[aj:bi]
			nbeg=numel(ybeg)
			nend=numel(yend)
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el)
			xp=nxs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-min(xp); the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			


			yc=yp*0.0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,'max=',mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=nBU[aj];mean(ybegtrust)
			;mn=5
			;print,'min=',mn

			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,'m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			;m2=xpa[hj-gim]
			m2=xpa[aj-dim]
			;print,'m2=',m2
			;print,'xpa[nbeg/2]=',xpa[nbeg/2]
			;print,'yp[nbeg/2]=',yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1.;.5;.1;1
			;m1=1.;.5;.1;1

			MM=[m0,m1,m2,m3]
;print,'to zeroth order, guess that MM=',MM
			algshock0=aj
			algshock1=aj
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-11
			CHISQ=-1
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,'status=',status,', chi^2=',CHISQ
;print, 'Function parameters: ', MM
			CHI0=CHISQ
			imin=dim
			imax=bi
			;ishock=round(MM[2]/MM[1]+dim);m2) +dim
			errorloc="ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)"
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
			fracRemain=(bi-ishock)*1.0/(bi-dim)
			yfit0=yfit
			errorloc="B20de=nB20d[ishock:imax]=nB20d["+strtrim(ishock,2)+":"+strtrim(imax,2)+"]"
			if ishock lt foot or ishock gt imax then begin
				ishock=aj
				working=0
			endif	else begin
			B20de=nB20d[ishock:imax]
			pointerinup= ( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])])) )
;			derv=dir(yfit,1,'mid')

;			gxg=where(derv eq max(derv))
;			ishock=gxg[0]+imin
			working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (nFore[ishock] eq 1) and imin lt foot-30;1*(   ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and (imax-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ gt 0)) and (nFore[ishock] eq 1) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup; (TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			endelse
			if doPerturb then begin
				working1=working
				yfit1=yfit
				MM1=MM
				ishock1=ishock
				CHI1=CHISQ
				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
				iminB=foot-3*60
				errorloc="doPerturb:iminB gt ishock then iminB=ishock-4*60"
				if iminB gt ishock then iminB=ishock-4*60
				imin=max([dim,iminB]);max([dim,min([ishock-7*60,bfoot])])
				smol=min(nderex[ishock:bi],smoloc)

				;imin=max([dim,foot-3*60]);ishock-7*60])
				if finite(smol) eq 1 and smoloc gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])
				;imax=min([bi,ishock+7*60])
				while nyBS[imin]-nyBS[dim] gt 2 and imin gt max([foot-4*60,dim]) do begin
					imin--
				;ishock--
				;imax--
				endwhile
				ypp=nys[imin:imax]
				xpp=nxs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				;if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin
				smypp=nBmed[imin:0.8*ishock+0.2*imin]
				smmp=nymm[imin:8*(ishock-imin)/10+imin]
				mn=min(smmp)
				if 0 and MM[3]-MM[0]  lt min(smypp) or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp)
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				MM=coeffCalc(xpp,ypp, ishock,x00)
				;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:9*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:9*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:9*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:9*(ishock-imin)/10]) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif
				weight=1.0/ypp
;print,'to second order, guess that MM=',MM
				algshock1=MM[2]+imin
				if dobug gt 0 then print,'[numel(xppa),numel(ypp),numel(weight)]=',[numel(xppa),numel(ypp),numel(weight)]
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,'status=',status,', chi^2=',CHISQ,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
;print, 'Function parameters: ', MM
				;ishock=round(MM[2]/MM[1])+imin;m2) +dim
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
				if ishock lt foot or ishock ge imax then begin
					ishock=ishock1
					working=0

				endif else begin
				;derv=dir(yfit,1,'mid')
				B20de=nB20d[ishock:imax]
			pointerinup= ( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 
				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
				working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (nFore[ishock] eq 1) and imin lt foot-30;1*(   ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nyBS[imin] - nyBS[dim] lt 10) and (CHISQ gt 0)) and (nFore[ishock] eq 1) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup;(TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
				endelse
			if dobug eq 3 then begin
print,"**************************"
;print,"status eq 0=",status eq 0
;print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0
;print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
;print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
;print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
;print,"Fore[ishock]=",Fore[ishock]
;print,"yfit[0],BU[ishock],updev,abs(yfit[0]-BU[ishock])-updev,<0=",yfit[0],BU[ishock],updev,abs(yfit[0]-BU[ishock]) lt updev
;print,"~pointerinup=",~pointerinup
;print,"--------------------------"
;print,"working=,",working

			endif

				
				IF (CHISQ gt CHI1) or( (not working) and working1) then begin
					yfit=yfit1
					CHISQ=CHI1
					MM=MM1
					ishock=ishock1
					imin=dim
					imax=bi	
					xppa=xpa
					algshock1=algshock0			
				ENDIF
			endif
		
			if dobug gt 0 then begin
;print,'status=',status,', chi^2=',CHISQ,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
;print, 'Function parameters: ', MM
;print,'[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=',[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			endif
			printed='unsaved'
			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
			if ishock lt imin then print, 'bad ishock: ishock = ',ishock,' lt ', imin,'= imin'
			if ishock gt imax then print, 'bad ishock: ishock = ',ishock,' gt ', imax,'= imax'

			;working = ishock+1.2/MM[1] lt bi and (status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi);1*(   ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and (imax-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1) and (CHISQ gt 0))
			if dobug gt 0 then print,'[MM[1],CHISQ,imax-ishock,minRemain,MM[0],working]=',[MM[1],CHISQ,imax-ishock,minRemain,MM[0],working]
			if working eq 1 then begin
				ishockt1=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
					if abs(ishockt1-ishock) gt 2 then begin
						imin+=ishock-ishockt1
						imax+=ishock-ishockt1
					endif
				lastiMax=imax
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
				printed=' saved '
				;outsublengths.add,bi-dim
				outsublengths.add,imax-imin
				nz0=fltarr(N)
				if( ncount eq 0) then begin
;print,'no NANs, i=',i
				;for k=gim,gi-1 do z[k]=yfit[k-gim]	
					;for k=dim,bi-1 do nz[k]=yfit[k-dim]	
					;for k=dim,bi-1 do nz0[k]=yfit[k-dim]
					;nz[imin:imax]=yfit
					;nz0[imin:imax]=yfit	
					for k=imin,imax do nz[k]=yfit[k-imin]	
					for k=imin,imax do nz0[k]=yfit[k-imin]	
					;print,'[ni9shock,N-1]=',[ni9shock,N-1]

					;if dobug gt 0 then if i eq 9 then print,'nz[ishock-3:ishock+3]=',nz[ishock-3:ishock+3]
					;if dobug gt 0 then if i eq 9 then print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:min([ni9shock+3,N-1])]
					if dobug gt 1 then print,'dim=',dim
				endif else begin
;print,'yfit has ',ncount,' NANs. calculating directly.'
					;tanhfit,xpa,MM,F
					tanhfit,xppa,MM,F
					;for k=gim,gi-1 do z[k]=F[k-gim]
					nz[imin:imax]=F
					nz0[imin:imax]=F
					;for k=dim,bi-1 do nz[k]=F[k-dim]

					;for k=dim,bi-1 do nz0[k]=F[k-dim]	
					;for k=imin,imax do nz[k]=F[k-imin]

					;for k=imin,imax do nz0[k]=F[k-imin]				
				endelse
				;ishock=round(MM[2]/MM[1])+imin;-x00
				if dobug gt 1 then print,'ishock=',ishock
				nshocks[ishock]=1


				bz0=reverse(nz)

				datzI=datBS
				datzI.y=bz0
				if dobug gt 1 then begin
;print,'[dim,bi,bi-dim,imin,imax,imax-imin]=',[dim,bi,bi-dim,imin,imax,imax-imin]
					str=['outbound',string(i)]
					str[1]=str[1].trim()
					str=str.join('_')
					store_data,str,data=datzI
				endif
				;outSubsets[dim:bi]=1
				;outbegs[dim:aj]=1
				;outends[ishock:bi]=1

				for k=imin,imax do outSubsets[k]=1
				for k=imin,ishock do outbegs[k]=1
				for k=ishock,imax do outends[k]=1

			endif; else brokenout.add,[i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
 			;chiList.add,[newName,' backword ',printed,string(i),string(CHISQ)]

			;debugoutbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debugoutend[ishock+1:imax]=i-(1.0-working)/4.0
			debugi=i-(1.0-working)/4.0+1
			;for k=imin,imax do debugoutlen[k]=debugi
			;debugoutlen[ishock]=-debugi
		
			;'in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations'
			if(i eq 1) then begin
					zwidth =(bi-dim)-m3
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=nz
			endif
		endfor 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
	;print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:ni9shock+3]
		;print,'now for the range ys[0:DD[0]]'
		;'now for the range ys[0:DD[0]]'
		i=0

		b0=nBB[0+dboffsetb]			

		d0=nDD[0]

		if d0 lt b0 then dim=d0 else dim=0
		;dim=0
		yp=nys[0:b0]
		xp=nxs[0:b0]
		np=size(yp,/n_el)
		edim=dim

		aj=0
		ishock=aj
		;'start with standard case'
		errorloc=""
		error_status=0
		catch,error_status
		if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG
print,'ERROR WHEN PERFORMING '+errorloc
			errorloc=""
			xaaa=xs[N-1-ishock]
			errmsg=["ERROR in curveFitter3 outbound 0 while producing "+newName ,'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc]
			errorsaver,xaaa,errmsg,/spec
			catch,/cancel
		endif

		if error_status eq 0  then begin
	;		if(aoffsetb eq 0) and (nAA[0] ne -1) and total(finite(nys[dim:b0]) ne 1) eq 0 then begin
			while (aoffsetb eq 0) and (nAA[0] ne -1) and total(finite(nys[dim:b0]) ne 1) eq 0 do begin
				errorloc+="if(aoffsetb eq 0) then begin: "
				if dobug gt 0 then print,'(((((((((((((((((((((((((('
				if dobug gt 1 then print,'now for the range nys[0:nBB[0]]=nys[0:',nBB[0],']' else print,'i=',0
				;if dobug gt 0 then print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:ni9shock+3]
				aj=nAA[0+daoffsetb]
				b0=min([aj+40*60,b0])
				if dobug gt 1 then print,'aj=',aj
				;print,gi
				;print,'gim=',gim
				;dim=0
				;print,'g0=',b0
				wherefoot=(where(ofoots lt aj and ofoots ge dim))[-1]
				;foot=ofoots[()[0]]

;			if wherefoot eq -1 then foot=dim else foot=ofoots[wherefoot]
			if wherefoot eq -1 then break else foot=ofoots[wherefoot]
			bfoot=max([dim,foot-10*60]);120])
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			;print,dim,foot

				;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
				regp=nreg[dim:b0]
				if (min(regp) lt 2) then begin
					while (regp[0] gt 1) do begin
						if dim +2*60 eq foot then break
						dim++
						regp=regp[1:*]
						yp=yp[1:*]
					endwhile
				endif
				n_ep=nn_e[dim:b0]
				if total(finite(n_ep) eq 1) eq 0 then break
			;print,dim,foot,numel(nn_e)
			if total(finite(nn_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(nn_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif

			if 0 and total(nymm[b0] le nymm[dim:b0-1]) eq 0 then begin

				while total(nymm[b0] le nymm[dim:b0-1]) eq 0 and b0-aj gt 3*60 and nB20d[b0] ne 0 do b0--
				yp=nys[dim:b0]
				xp=nxs[dim:b0]
				np=size(yp,/n_el)
			endif
				np=numel(yp)
				ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				nbeg=size(ybeg,/n_el)
				CRUSTST=where((nBsbd[aj+1:b0] gt 1.0) and (nrhod[aj+1:b0] lt 0),ccount)+aj+1
				if ccount gt 0  then begin
					b0=CRUSTST[0]
					yp=nys[dim:b0]
					np=numel(yp)
					
				endif
				nzn=nz[b0:*]
				yend=nys[aj:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
				nend=size(yend,/n_el)

				n_ep=nn_e[dim:b0]
				if total(finite(n_ep) eq 1) eq 0 then break


				;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

				ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
				
				yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				brefines=0
				frefines=0
	

				mbeg=nbeg
				mend=nend
		

				incfunctions=1
				decfunctions=1
				b0=min([aj+40*60,b0])
				bi=b0
				d00=dim
				zeron=aj-dim
;print,'[imin,aj,imax]=',[dim,aj,b0]
					jbegj=max([1,nend-20])+aj
					jendj=min([N-1,nend+20+aj])

				errorloc="WHILE LOOP"
				while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(nys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 
					
					ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
				
					yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
					jbegj=max([1,nend-20])+aj
					jendj=min([N-1,nend+20+aj])
					;print,'(brefines,frefines)=',brefines,frefines
					;print,'(incfunctions,decfunctions)=',incfunctions,decfunctions
					;we will do these seperately within the loop, but at same time so that one negligably affects the other 
					if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) and numel(ybeg) gt 1 then begin; this is a simple approximation, but checking if on previous step
						;print,'pushing forward the end of shock number i=',0,', from gim=',gim,', where aj=',aj,', and g0=',g0, 'and size(yp)=',size(yp,/n_el)
						yp=yp[1:*] ;shrinks our range by one
						errorloc="ybeg=ybeg[1:*]"
 					ybeg=ybeg[1:*]
						np=numel(yp)
						nbeg=numel(ybeg)
						dim++
						brefines++
						if ( brefines ge 2 *mbeg/5) or (dim +1 eq aj) or (np-3 eq 1) or (nbeg lt 3) then begin
							incfunctions=0
							;dim=DD[i-1]
							dim=d00
							yp=nys[dim:bi]
							np=size(yp,/n_el)
							ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
							nbeg=mbeg
							
						endif
					endif

					if ((median(nys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
						;print,'pulling back the end of shock number i=',i,', from gi=',gi
						;print,"numel(yp),np=",[numel(yp),np]
						yp=yp[0:np-2]
						np=numel(yp)
						errorloc="yend=yend[0:nend-2]"
						yend=yend[0:nend-2]
						nend--
						bi--
						frefines++
						if ( frefines ge 1 *mbeg/5) or (aj+1 eq bi) or (nend-3 eq 0) or (np-4 eq 0) then begin
							;print,'i=',i,', ddn=',ddn
							decfunctions=0
							bi=b0
							yp=nys[dim:bi]
							np=size(yp,/n_el)
							yend=nys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
							nend=mend
						endif
					endif

					;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
					if ((incfunctions eq 0) and (decfunctions eq 0)) then break
				endwhile 
				b0=bi
				if d00 ne 0 then edim=enDD[0]
;print,'[imin,aj,imax]=',[dim,aj,b0]
				;print, 'the number of times we refined the beginning is ',brefines,'.'
				;print, 'the number of times we refined the end is ',frefines,'.'

				yp=nys[dim:b0]
				n_ep=nn_e[dim:b0]
				if total(finite(n_ep) eq 1) eq 0 then break

				;mx=25
				;print,'max=',mx
				;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
				;or  a zeroth order estimate of ~5

				mn=nBU[aj];mean(ybegtrust)
				;mn=5
				;print,'min=',mn
				

				;if (rollthird eq 1) then aj=Third_Roll(B20d,dim,aj,bi,dobug) ; second order correction 

				;print,"2*nbeg/5,nbeg/2"
;print,2 * nbeg/5,":", nbeg/2,"numel(ybeg)=",numel(ybeg)
				ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
				
				yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				mn=mean(ybegtrust)
				;aj=FalseShockSkipper(aj,bi,nBmed,median(nBmed[2 * nbeg/5+dim: nbeg/2+dim]))
				errorloc="aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,nyBS,nbeg,nend, nmono,nB5d,nB20d,orderCustom,nextr,nrhod,nFore)"
				ishock=aj
				aj=ajRefiner(max([edim,foot]),dim,aj,bi,zeron,dobug,nyBS,nbeg,nend, nmono,nB5d,nB20d,orderCustom,nextr,nrhod,nFore)
				if ymm[bi] eq max(nymm[aj:bi]) then while nymm[bi] eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(ymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt nymm[bi] )  do bi--

			if total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) le 1 or total(abs(nymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(nymm[aj-60:aj]-median(nymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(nymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( nymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) then while max(nymm[bi-20:bi]) eq max(nymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(nymm[aj:bi])+2.5*stddev(nymm[aj:bi]) lt max(nymm[bi-20:bi]) )  do bi--
				ab=(aj+bi+bi)/3
				while ab lt bi and max(nymm[aj:ab])+5 lt max(nymm[ab:bi],abmax) do bi=abmax+ab-1
				b0=bi
				while max(nys[max([aj-60,foot]):aj]) gt max(nys[aj:bi]) and aj gt foot do aj--

				mn=nBU[aj];mean(ybegtrust)
				wblwu=where(nymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
				if blwuc gt 0 then bi=wblwu[0]-1
				yp=nys[dim:bi]
				np=size(yp,/n_el)
				ishock=aj
				b0=bi
				xp=nxs[dim:bi]
				xpa=xp-min(xp)
				yc=yp*0.0
				;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
				;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
				; or a zeroth order estimate of 25
				yend=nys[aj:bi] 
				mx=max(yend)
				m0=(mx-mn)/2
				m3=(mx+mn)/2
				;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,nmono)
				;print,'after monorolling'
				;print,'[imin,aj,imax]=',[dim,aj,b0]
				;if (rollsec eq 1) then aj=secondRoll(aj,bi,nB5d,dobug) ; second order correction 
				;;print,'after secondroll'
				;print,'[imin,aj,imax]=',[dim,aj,b0]

				;aj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)
				;if (rollthird eq 1) then aj=Third_Roll(nB20d,dim,aj,bi,dobug) ; third order correction 
				;print,'after aoscillate'
				;print,'[imin,aj,imax]=',[dim,aj,b0]
				;aj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)



				;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
				;print,'m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
				m2=xpa[aj-dim]

				;with all this, can calculate guess for  m1 to zeroth order by
				;m1=  atanh((y-m3)/m0)/(x-m2)
				;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
				;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
				m1=1.;.5;.1;1
				MM=[m0,m1,m2,m3]
;print,'to zeroth order, guess that MM=',MM
				;MM0=MM
				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

				status=-11
				CHISQ=-1
				errorloc="yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status, CHISQ=CHISQ)"
				algshock0=aj
				algshock1=aj
				yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status, CHISQ=CHISQ)
;print, 'Function parameters: ', MM
;print,'status=',status,', chi^2=',CHISQ,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]

;				if dobug gt 0 then begin
				ishock=round(MM[2]+dim);round(MM[2]/MM[1]+dim);m2) +dim

				;ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
				fracRemain=(bi-ishock)*1.0/(bi-dim)
;print,'[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=',[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
;				endif

				imin=dim
				imax=bi
				ishockM=round(MM[2])+imin;round(MM[2]/MM[1])+imin
			;print,"ishock,time=",ishock,",",x2greg(xs[ishock],/str)
			;derv=dir(yfit,1,'mid')
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1-doPerturb,foot)
			
working=1
				if abs(ishock-ishockM) gt 3*60 then begin
					if (ishock le foot) and ishockM gt foot then ishock=ishockM else $
					if (ishock le foot) and ishockM le foot then begin
					ishock=aj
					working=0
				endif
				MM2=MM
				MM2[2]=ishock-dim;MM[1]*(ishock-dim)
					;tanhfit, xpa, MM, yfit
				;MM2[2]=xpa[maxloc2+ishock-11-dim]
				yfit2=CURVEFIT(xpa, yp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2
					ishock=ishock2
					algshock1=ishockM
				endif
			endif 
			;
			if ishock le foot or ishock ge bi then begin

				ishock=aj
				working=0
			endif else begin
				B20de=nB20d[ishock:imax]


			pointerinup=( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 

				working = ishock+1.2/MM[1] lt bi and ( ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax)  and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nyBS[max([dim,ishock-7*60])] - nyBS[dim] lt 10) and (CHISQ gt 0)) and (nFore[ishock] eq 1) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup and imin lt foot-30
			endelse;(TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
				;ishock=round(MM[2]/MM[1]+dim)
				if doPerturb then begin
					working1=working
					yfit1=yfit
					MM1=MM
					ishock1=ishock
					CHI1=CHISQ
;print,''
;print,'Now for second order corrections'
				if working then begin
					if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]

				endif
					smol=min(nderex[ishock:bi],smoloc)

					imin=max([dim,foot-3*60]);ishock-7*60])
					if finite(smol) eq 1 and smoloc gt 7*60 then imax=min([smoloc+ishock+60,bi])	else imax=min([bi,ishock+7*60])
					;imin=max([dim,foot-3*60]);max([dim,ishock-7*60])
					;imax=min([bi,ishock+7*60])

					while nyBS[imin]-nyBS[dim] gt 2 and imin gt foot -4*60 do begin
						imin--
					;ishock--
					;imax--
					endwhile
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
					errorloc="ypp=ys[imin:imax]"
					ypp=ys[imin:imax]
					errorloc="xpp=xs[imin:imax]"
					xpp=xs[imin:min([imax,N-1])]
					xppa=xpp-xpp[0]
					MM1=MM
					;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
					smmp=nymm[imin:8*(ishock-imin)/10+imin]
					smypp=nBmed[imin:8*(ishock-imin)/10+imin]
					mn=min(smmp)
;				if 0 and MM[3]-MM[0]  lt min(smypp) or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5   then begin
				if  MM[3]-MM[0]  lt min(smmp)  or total(MM[3]-MM[0] gt smmp) lt 5  then begin
					;if 0 and MM[3]-MM[0]  lt min(ypp[0:9*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:9*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:9*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:9*(ishock-imin)/10]) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:9*(ishock-imin)/10])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

					endif

					MM=coeffCalc(xpp,ypp, ishock,x00)
					;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				if 0 and MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin
					;if 0 and MM[3]-MM[0]  lt min(ypp[0:9*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:9*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:9*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:9*(ishock-imin)/10]) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:9*(ishock-imin)/10])
					MM[3]=(mx+mn)/2
					MM[0]=(mx-mn)/2

				endif

					weight=1.0/ypp
;print,'to second order, guess that MM=',MM
					algshock1=MM[2]+imin
					;print,'[numel(xppa),numel(ypp),numel(weight)]=',[numel(xppa),numel(ypp),numel(weight)]
					errorloc="yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)"
					yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,'status=',status,', chi^2=',CHISQ;,', m2-MM[2]=',m2-MM[2],', m2/m1-MM[2]/MM[1]=',m2/m1-MM[2]/MM[1]
;print, 'Function parameters: ', MM
					if dobug gt 0 then print,'MM0[2]-MM2[2]=',m2-MM[2],', MM0[2]/MM0[1]-MM2[2]/MM2[1]=',m2/m1-MM[2]/MM[1],',MM1[2]/MM1[1]-MM2[2]/MM2[1]=',MM1[2]/MM1[1]-MM[2]/MM[1]
					errorloc="ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)"
					ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
					;ishock=round(MM[2]/MM[1]+dim);m2) +dim
					;derv=dir(yfit,1,'mid')
					if ishock lt foot or ishock ge imax then begin
						ishock=ishock1
						working=0
						algshock1=aj

					endif else begin
					B20de=nB20d[ishock:imax]
					pointerinup=( TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(nBmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 					and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 


					working = ishock+1.2/MM[1] lt bi and ( ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax)  and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (nyBS[imin] - nyBS[dim] lt 10) and (CHISQ gt 0)) and (nFore[ishock] eq 1) and (abs(yfit[0]-nBU[ishock]) lt upDev) and ~pointerinup and imin lt foot-30;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
					;gxg=where(derv eq max(derv))
					;ishock=gxg[0]+imin
					endelse
					IF (CHISQ gt CHI1) or( (not Working) and Working1) then begin
						yfit=yfit1
						CHISQ=CHI1
						MM=MM1
						ishock=ishock1
						imin=dim
						imax=bi	
						algshock1=algshock0			
					ENDIF
				endif
				printed='unsaved'
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
				if ishock lt imin then print, 'bad ishock: ishock = ',ishock,' lt ', imin,'= imin'
				if ishock gt imax then print, 'bad ishock: ishock = ',ishock,' gt ', imax,'= imax'

				if working then begin

					ishockt1=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
					if abs(ishockt1-ishock) gt 2 then begin
						imin+=ishock-ishockt1
						imax+=ishock-ishockt1
					endif
				if lastiMax le 0 then lastiMax=imax
				;	outimaxs[imin:imax]=imax
					;outups[imin:imax]=MM[3]-MM[0]
				;	outdowns[imin:imax]=MM[3]+MM[0]			
					outbegs[dim:ishock]=1
				;	outends[ishock:bi]=1
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

				;	outimins[imin:imax]=imin
				;	outchis[imin:imax]=CHISQ
				;	outnums[imin:imax]=i
				;	outshocklocs[imin:imax]=ishock
					outsublengths.add,bi-dim
					printed=' saved '
					nshocks[ishock]=1					
					if( ncount eq 0)  then begin
						;print,'no NANs'
					;	print,'gim=',gim,', g0-1=',b0-1
						
						for k=imin,imax-1 do nz[k]=yfit[k-imin]	
					endif else begin
				;	print,'yfit has ',ncount,' NANs. calculating directly.'
						tanhfit,xpa,MM,F
						nzn=nz[b0:*]

					;	print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)

						for k=imin,imax-1 do nz[k]=F[k-imin]
				
					endelse
				endif else brokenout.add,[0,dim,b0, nxs[dim]-min(nxs),nxs[b0]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
				;print,'g0'
				nzn=nz[imin:*]
				;chiList.add,[newName,'backword',printed, string(i),string(CHISQ)]
				;debugi=i-(1.0-working)/4.0
				;debugoutbeg[imin:ishock-1]=i-(1.0-working)/4.0
				;debugoutend[ishock+1:imax]=i-(1.0-working)/4.0
				;debugoutlen[imin:imax]=debugi
				;debugoutlen[ishock]=-debugi
				;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
				;print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:ni9shock+3]
				break
		endwhile
		;endif ;else begin 
		while 0 do begin
				break
				;'if the trrigger pair WAS cut off, we try to approximate results here'
				;'want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth'
				onstep=1*(b0 le zwidth-4)
				;'if onstep eq 1, can assume without loss of generality that we are indeed on the step'
				;ty=yp
				tp=np
				mx=mean(yp)
				mn=mean(trust)
				imin=0
				imax=b0 
				if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
					for k=0,b0-1 do nz[k]=mx
				endif else begin
				 ;if more than the width of previous, try fitting with m2=0 
					errorloc+="endif else begin: "
					
					
					m0=(mx-mn)/2
					m3=(mx+mn)/2

					m2=0
					m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

					MM=[m0,m1,m2,m3]
;print,'to zeroth order, guess that MM=',MM

					weight=1.0/yp
					;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
					status=-11
					errorloc="yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status, CHISQ=CHISQ)"
					yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status)
;print, 'Function parameters: ', MM
					printed='unsaved'
					NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					ishock=round(MM[2]);round(MM[2]/MM[1]);m2) +dim
;				working = ishock+1.2/MM[1] lt bi and (((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and ( MM[0] gt 1))
					working = ishock+1.2/MM[1] lt bi and (((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and (CHISQ gt 0)) and (nFore[ishock] eq 1) (TOTAL(nBmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
;print,'[imin,ishock,imax]=',[imin,ishock,imax]
					if ishock lt imin then print, 'bad ishock: ishock = ',ishock,' lt ', imin,'= imin'
					if ishock gt imax then print, 'bad ishock: ishock = ',ishock,' gt ', imax,'= imax'

					if working then begin
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
						outbegs[dim:ishock]=1
						outends[ishock:bi]=1

						;outshocklocs[imin:imax]=ishock
						if( ncount eq 0) then begin
							for k=0,imax-1 do nz[k]=yfit[k]
							printed=' saved '	
							;chiList.add,[newName,'backword',string(i),string(CHISQ)]
						endif else begin
				;			print,'yfit has ',ncount,' NANs. calculating directly.'
							tanhfit,xp,MM,F

							for k=imin,imax-1 do nz[k]=F[k]
					
						endelse
						;ishock=round(MM[2]/MM[1]) +imin

						nshocks[ishock]=1				
					endif
					;chiList.add,[newName,'backword',printed,string(i),string(CHISQ)]
				endelse
		

		endwhile;endelse
	endif
		;print,'now the final bit'
		;print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:ni9shock+3]

		;'finally, we need the remaining bit after DD[ddn-1]'

		;'in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well'
		;'set the remainder to zero for now'
		nzn=nz[b0:*]

		;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
		;GN=nDD[bbn-1]
		;yp=nys[GN:*]
		;print,GN
		for k=lastiMax+1,N-1 do nz[k]=0
		nzn=nz[b0:*]

		;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do nz[k]=0 ; 'if number of remaining element is much less than a period, then the step won't arrive'

		;else begin
		;	;'what we do depends on whether fcutoff eq 0'
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif

	endif

;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------

	catch,error_status
		if error_status ne 0 then begin
print, 'Error index: ', error_status
print, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
			;errorloc=""
			;xaaa=xs[N-1-ishock]
			errmsg=["error in curveFitter3 producing "+newName+" after fitting", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
			errorsaver,xs[0],errmsg
		endif
	NONDIM:print,'nDD[0] eq -1' 

	outups=reverse(outups)
	outdowns=reverse(outdowns)

	;print,'nz[ni9shock-3:ni9shock+3]=',nz[ni9shock-3:ni9shock+3]
	if dobug gt 0 then print,'(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)'
		zb=fltarr(N)
		znb=fltarr(N)
		shocksb=fltarr(N)
	if nDD[0] ne -1 then begin
		zb=reverse(nz)
		znb=reverse(nzn)
		shocksb=reverse(nshocks)
	;print,'zb[N-ni9shock-3:N-ni9shock+3]=',zb[N-ni9shock-3:N-ni9shock+3]
		;debugoutlen=reverse(debugoutlen)
	;debugoutbeg=reverse(debugoutbeg)
	;debugoutened=reverse(debugoutend)

	
		outSubsets=reverse(outSubsets)
		outends=reverse(outends)
		outbegs=reverse(outbegs)

		outimaxs=reverse(outimaxs)
		outimins=reverse(outimins)
		outchis=reverse(outchis)
		outnums=reverse(outnums)
		outshocklocs=reverse(outshocklocs)
		outalgshock0locs=reverse(outalgshock0locs)
		outalgshock1locs=reverse(outalgshock1locs)

		bMMMs=reverse(nMMMs)
	endif
	;adjChiIn=fltarr(N);inchis*0.0
	;adjChiOut=fltarr(N)
	;for i=0,N-1 do if inchis[i] ne 0 then adjChiIn[i]= abs(inchis[i]-1)
	;for i=0,N-1 do if outchis[i] ne 0 then adjChiOut[i]= abs(outchis[i]-1)
	chis=outchis
	for i=0 , N-1 do if inchis[i] ne 0 then chis[i] = inchis[i]
	;adjChi=fltarr(N)
	;for i=0,N-1 do if chis[i] ne 0 then adjChi[i]= abs(chis[i]-1)
	;store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:'REDUCED CHISQ'}
	;store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiIn,ytitle:'abs(REDUCED CHISQ-1)'}
	;store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:'REDUCED CHISQ'}
	;store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:'abs(REDUCED CHISQ-1)'}
	;store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:'REDUCED CHISQ'}
	;store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:'abs(REDUCED CHISQ-1)'}



;	inslocs=where(shocks ne 0,numins)

	;if numins gt 0 then begin
	;	print,"the magnitudes at the shocks are"
	;	foreach el,inslocs do print,"B[ishock]=B["+strtrim(el,2)+"]=",z[el]

	;endif else print, "no inbounds"
	;rl=''
	;cl='c'
	;if NOT rollup then begin
	;	 rl='-unrolled'
	;	cl='m'
	;endif 

	for i=0, N-1 do begin
		if inSubsets[i] eq -1 then inSubsets[i]=0
		if inends[i] eq -1 then inends[i]=0
		if inbegs[i] eq -1 then inbegs[i]=0
	endfor

	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0


	if numins gt 0 then begin
;print,"the magnitudes at the shocks are"
		foreach el,inslocs do print,"B[ishock]=B["+strtrim(el,2)+"]=",z[el]

	endif else print, "no inbounds"
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,'end loop'
	;result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,'max(z)=',max(z),', min(z)=',min(z)
	
	zn=z[b0:*]

	;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
	;store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'inbound Shock'}
	;store_data,'shocks'+fitnum+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	;help,inSubsets
	;store_data,'sublengths'+fitnum+rl+'_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

	;store_data,'sublengths'+fitnum+rl+'_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
	;store_data,'sublengths'+fitnum+rl+'_inbound_end',data={x:xs,y:inends,ytitle:'flag'}

	;store_data,newName+'_debug_inbegs',data={x:xs,y:debuginbegs,ytitle:'shock number'}
	;store_data,newName+'_debug_inends',data={x:xs,y:debuginends,ytitle:'shock number'}
	;store_data,newName+'_debug_ins',data={x:xs,y:debuginlen,ytitle:'shock number'}
	if(total(z-ys) eq 0) then print, 'broken'
 	;dat.y=z
	;print,size(dat)
	;help,dat
	store_data,newName+'_inbound',data={x:xs,y:z,ytitle:'Magnetic Field (nT)', inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inups:inups,indowns:indowns,$
MMs:MMMs,inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs}

;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
	;store_data,'shocks'+fitnum+rl+'_outbound',data={x:xs,y:shocks,ytitle:'flag'}
	;store_data,newName+'_shocks_outbound',data={x:xs,y:shocks,ytitle:'outbound_shock'}
	;help,inSubsets
	;store_data,'sublengths'+fitnum+rl+'_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}
	;if numins gt 0 then begin
	;	print,"the magnitudes at the shocks are"
	;	foreach el,inslocs do print,"B[ishock]=B["+strtrim(el,2)+"]=",z[el]

	;endif else print, "no inbounds"
	;store_data,'sublengths'+fitnum+rl+'_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
	;store_data,'sublengths'+fitnum+rl+'_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

	NNN=where(FINITE(zb) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zb[where(finite(zb,/NAN))]=0
;	print,'size(z)=',size(zb)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,'end loop'
	;result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,'max(z)=',max(z),', min(z)=',min(z)
	
	znb=zb[b0:*]

	;print,'mean(zn)=',mean(znb),',  max(zn)=',max(znb)

	;store_data,'shocks'+fitnum+rl+'_outbound',data={x:xs,y:shocksb,ytitle:'flag'}

	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	;help,dat
	store_data,newName+'_outbound',data={x:xs,y:zb,ytitle:'Magnetic Field (nT)', outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,outups:outups,outdowns:outdowns,$
MMs:bMMMs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}

	;store_data,newName+'_debug_outbegs',data={x:xs,y:debugoutbegs,ytitle:'shock number'}
	;store_data,newName+'_debug_outends',data={x:xs,y:debugoutends,ytitle:'shock number'}
	;store_data,newName+'_debug_outs',data={x:xs,y:debugoutlen,ytitle:'shock number'}

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
	for i=0, N-1 do begin

		if shocksb[i] ne 0 then shocks[i]=shocksb[i]

	endfor

	;z=z+zb
	;shocks+=shocksb
	
	NNN=where(FINITE(zf) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zf[where(finite(zf,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,'end loop'
	;result = MOMENT(zf)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,'max(z)=',max(z),', min(z)=',min(z)
	
	zn=zf[b0:*]

	;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
	;store_data,newName+'_shocks',data={x:xs,y:shocks,ytitle:'shock'}
	;store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}

	if(total(zf-ys) eq 0) then print, 'broken'
 	dat.y=zf
	;print,size(dat)
	;help,dat
	store_data,newName,data={x:xs,y:zf,ytitle:'Magnetic Field (nT)', inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks,outups:outups,outdowns:outdowns,inups:inups,indowns:indowns,$
inMMs:MMMs,outMMs:bMMMs,inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}

	
	;options,'shocks'+fitnum+rl,'colors',cl
	;options,'shocks'+fitnum+rl+'_inbound','colors',cl 
	;options,'shocks'+fitnum+rl+'_outbound','colors',cl 

	;if numins gt 0 then begin
		;print,"the magnitudes at the shocks are"
		;foreach el,inslocs do print,"B[ishock]=B["+strtrim(el,2)+"]=",z[el]

	;endif else print, "no inbounds"


;print, 'newName, direction, i, CHISQ'
;print, chiList
;	endelse
;print,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
;print,'		finished creating ',newName
;print,'============================================'
;print,''
;print,''
;print,''
;print,''
		;wdelete
end
