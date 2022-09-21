


pro boundaryrefiner2,explot=explot

	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=dat

	dobug=0

	if not keyword_set(explot) then explot="B_medianderiv_extrema"



	get_data,explot,data=datEx
	extr=datEx.y
	nextr=(-1)*REVERSE(extr)
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	
	typ=2
;	get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
	
	ys[where(finite(ys) eq 0)]=0

	xs=dat.x
	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	z=fltarr(N)



	inSubsets=fltarr(N);+1;-1


	outSubsets=fltarr(N);-1
	REPLICATE_INPLACE, outSubsets,-1
	outends=fltarr(N);+1;-1
	outbegs=fltarr(N);+1;-1

	forecountdown=fltarr(N);fltarr(N)
	nforecountdown=fltarr(N);fltarr(N)
	foreblock=fltarr(N)
	nforeblock=fltarr(N)
		


	get_data,'regid_cleaned_plot_interpolated',data=datReg
	reg=datReg.y

	;get_data,'B5-3d10',data=datB5d      ;this data is Bmag smoothed over 5 second then 3 second average, then numerically derivativated then 10 second averaged
	;get_data,'B20deriv',data=datB20deriv


	;hill=datB5d.y
	;hill2=datB20deriv.y

	get_data,'B_sec_smoothed' ,data=datBSb
	get_data,'B_sec_smoothed_back' ,data=datBS
	tgtname=explot.remove(-13)
	print,tgtname
	get_data,tgtname,data=datB2

	get_data,'B_median',data=datBmed ;we want this data  for looking to see if we treat brief foreshocks as if they are the actual shock. 
	get_data,'B_20sec',data=datB20   ;in case the forecountdown tries to go too far forward, need to stop it.
	get_data,"rho30deriv",data=datRhod ; we want this data to avoid using crustal fields for downstream data:
	; if rho60 lt 1 then  few particles                                   
	get_data,"B_sec_smoothed_backderiv",data=datBsbd
	get_data,"B_sec_smoothedderiv",data=datBsd
	;help,datRhod
	rhod=datRhod.y
	nrod=-1*REVERSE(rhod)
	Bmed=datBmed.y
	yBS=datBS.y
	yBSb=datBSb.y

	BSd=datBsd.y
	BSbd=datBSbd.y

	get_data,'swics_v5_interpolated',data=datv5 ;;WE'll be operating close enough to shock that fine structures may not always operate correctly
	v5=datv5.y
	nv5=REVERSE(v5)

	;------------------
	B2=datB2.y
	nB2=reverse(B2)
	mbeg=0
	mend=0
	incfunctions=1
	decfunctions=1
	trust=0
		

	foremaxi=fltarr(N)
	foremaxo=fltarr(N)

	get_data,'ascend_end_interpolated',data=datae
	get_data,'ascend_begin_interpolated',data=datab
	get_data,'descend_end_interpolated',data=datde
	get_data,'descend_begin_interpolated',data=datdb

	yae=datae.y
	yab=datab.y
	yde=datde.y
	ydb=datdb.y
	
	;	
	DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

	AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

	CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
	BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
	if dobug gt 1 then PRINT,'[ddn,aan,bbn]=',[ddn,aan,bbn]
	if dobug gt 1 then print,'numel(hill)=',numel(hill)

	meandevAA=0
	for i=1, aan -1 do meandevAA+=(AA[i]-AA[i-1])/aan

	meandevBB=0
	for i=1, bbn -1 do meandevBB+=(BB[i]-BB[i-1])/bbn

	meandevCC=0
	for i=1, ccn -1 do meandevCC+=(CC[i]-CC[i-1])/ccn

	meandevDD=0
	for i=1, ddn -1 do meandevDD+=(DD[i]-DD[i-1])/ddn


	meandev=mean([meandevAA,meandevBB,meandevCC,meandevDD])
if numel(AA) gt 1 then  if DD[0] gt AA[1] then AA=AA[1:*]
if numel(BB) gt 1 then 	if DD[0] gt BB[1] then BB=BB[1:*]	


	AA0=AA

	BB0=BB
	CC0=CC
	DD0=DD

	eDD=DD
	ebDD=eDD
	bDD=DD
	bAA=AA
	bCC=CC
	bBB=BB
	;------------------

		;'variables that would otherwise be defined in loops'

	mbeg=0
	mend=0
	incfunctions=1
	decfunctions=1
		;trust=0
	g0=0;
	b0=0
		
	newAA=AA
	newBB=BB
	newCC=CC
	newDD=DD
		;counterinutitively, will want to curve fit ys[0:DD[0]] after the rest

		;need to determine if we have the whole curve or not


	;bcutoff=1*(DD[0] le AA[0]) ; does the first element of  our data set occur between our first and second trigger?

	doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
	aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

	backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
	fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?


		


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



	innums=fltarr(N)-1
	outnums=fltarr(N)-1


		
	get_data,"PE_half",data=datpeh
	get_data,"te_half",data=datteh
	get_data,"ne_half",data=datneh


	peh=datpeh.y
	neh=datneh.y
	teh=datteh.y

	;peh=datpe10.y
	;neh=datne10.y
	;teh=datte10.y

	nneh=reverse(neh)
	nteh=reverse(teh)
	npeh=reverse(peh)
	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME END!



	badelecs=0




fullIter=min([ddn,aan,bbn])


	if AA[0] eq -1 then goto, NOAJ;NODIM
	if DD[0] eq -1 then goto, NODIM;NODIM
for i=1 ,fullIter-1 do begin
	j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetf
	l=i-1*(CC[0] gt BB[0])
	aj=AA[j]
	bi=BB[i]
	dim=DD[k]
	cl=CC[l]
	;cln=CC[l+1]
	print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	if dobug gt 1 then print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	
endfor


;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------

		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,fullIter-1  do begin
			var=midboundaryrefiner(i,ys,AA,BB,CC,DD,Bmed,rhod,Bsd,extr,forecountdown,eDD,foreblock,B2,v5,neh,peh,teh)


			if var[-1] eq -1 then begin
				badelecs=1
				newBB[(where(BB eq var[1]))[0]]=-1
				newAA[(where(AA eq var[2]))[0]]=-1
				newDD[(where(DD eq var[0]))[0]]=-1
				continue
			endif
			inSubsets[var[0]:var[1]]=1
			imx=var[1]
			imn=var[0]

			inimaxs[imn:imx]=imx
			inimins[imn:imx]=imn

			innums[imn:imx]=i
			mxfore=max(forecountdown[imn:imx])
			foremaxi[where(forecountdown[imn:imx] ne 0)+imn]=mxfore
		endfor
NODIM: print,'NO DIMS'
	if DD[0] eq -1 then begin
		DD=[0]
		eDD=[0]
	endif
	var0=startboundaryrefiner(ys,AA,BB,CC,DD,Bmed,rhod,Bsd,extr,forecountdown,reg,eDD,foreblock,B2,v5)
	imx=var0[1]
	imn=var0[0]
	inSubsets[imn:imx]=1


	inimaxs[imn:imx]=imx
	inimins[imn:imx]=imn

	innums[imn:imx]=i
	;foremaxi[imn:imx]=max(forecountdown[imn:imx])
	foremaxi[where(forecountdown[imn:imx] ne 0)+imn]=max(forecountdown[imn:imx])




NOAJ: print,'NO AJ'
	nxs=xs
	nys=REVERSE(ys)

	nreg=reverse(reg)

	nBmed=reverse(Bmed)
	;store_data,plt+"_rev",data=dat,limits=limits

	;print,"size(nys)=",size(nys)


	;store_data,"descend_begin_rev",data=dat

	nrhod=REVERSE(rhod)*(-1)
	nBsbd=REVERSE(Bsbd)*(-1)

;datBS.y=nyBS
;datBS.x=nxs
;get_data,'B_sec_smoothed',data=datBS

;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]



	nz=z*0.0


	if AA0[0] ne -1 then nDD=N-1-reverse(AA0) else nDD=[0]
	if DD0[0] ne -1 then nAA=N-1-reverse(DD0) else goto, NOnAJ
 	nBB=N-1-reverse(CC0)
	nCC=N-1-reverse(BB)
	enDD=nDD
	newnAA=nAA
	newnBB=nBB
	newnCC=nCC
	newnDD=nDD

if numel(nAA) gt 1 then 	if nDD[0] gt nAA[1] then nAA=nAA[1:*]
if numel(nBB) gt 1 then 	if nDD[0] gt nBB[1] then nBB=nBB[1:*]
	nddn=aan
	nbbn=ccn
	nccn=bbn
	naan=ddn
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
if numel(nBB) gt 1 then 	dboffsetb=1*(nBB[1] lt nDD[0]) else dboffsetb=0
if numel(nAA) gt 1 then 	daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0
	;if dobug gt 1 then print,'numel(nys)=',numel(nys)
	;if dobug gt 1 then print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
	fullIter=min([nddn,naan,nbbn])
	print,'fullIter=',fullIter
	for i=1 ,fullIter-1 do begin
	j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	ii=i+dboffsetb
	aj=nAA[j]
	bi=nBB[ii]
	dim=nDD[k]
	print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	if dobug gt 1 then print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	
endfor
;stop
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;help,nrhod

		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		lastmax=0
	for i=1,fullIter-1  do begin
			var=midboundaryrefiner(i,nys,nAA,nBB,nCC,nDD,nBmed,nrhod,nBsbd,nextr,nforecountdown,enDD,nforeblock,nB2,nv5,nneh,npeh,nteh)

			if var[-1] eq -1 then begin
				badelecs=1
				newnBB[(where(nBB eq var[1]))[0]]=-1
				newnAA[(where(nAA eq var[2]))[0]]=-1
				newnDD[(where(nDD eq var[0]))[0]]=-1
				continue
			endif

			imx=var[1]
			imn=var[0]
			outSubsets[imn:imx]=1


			outimaxs[imn:imx]=N-1-imn
			outimins[imn:imx]=N-1-imx
			mxfore=max(nforecountdown[imn:imx])
			;foremaxo[imn:imx]=max(nforecountdown[imn:imx])
			foremaxo[where(nforecountdown[imn:imx] ne 0)+imn]=mxfore
			outnums[imn:imx]=i

	endfor 

	nvar0=startboundaryrefiner(nys,nAA,nBB,nCC,nDD,nBmed,nrhod,nBsbd,nextr,nforecountdown,nreg,enDD,nforeblock,nB2,nv5)
	imx=nvar0[1]
	imn=nvar0[0]
	outSubsets[imn:imx]=1
;	foremaxo[imn:imx]=max(nforecountdown[imn:imx])
	foremaxo[where(nforecountdown[imn:imx] ne 0)+imn]=max(nforecountdown[imn:imx])

	if badelecs then begin
		AA=AA[where(newAA ne -1)]
		BB=BB[where(newBB ne -1)]
		DD=DD[where(newDD ne -1)]

		nAA=nAA[where(newnAA ne -1)]
		nBB=nBB[where(newnBB ne -1)]
		nDD=nDD[where(newnDD ne -1)]

	endif


	outimaxs[imn:imx]=N-1-imn
	outimins[imn:imx]=N-1-imx

	outnums[imn:imx]=i

	outSubsets=REVERSE(outSubsets)
	outimaxs=REVERSE(outimaxs)
	outimins=REVERSE(outimins)
	foremaxo=REVERSE(foremaxo)
	;bAA=N-1-reverse(nAA)
;	bDD=N-1-reverse(nDD)
;	ebDD=N-1-reverse(enDD)
;	bCC=N-1-reverse(nCC)
;	bBB=N-1-reverse(nBB)
;NODIM: print,'NO DIMS'
NOnAJ: print,'NO nAJ'
	;newAE=fltarr(N)
	;newAB=fltarr(N)
	;newDE=fltarr(N)
	;newDB=fltarr(N)
	newAA=fltarr(N)
	newBB=fltarr(N)
	newCC=fltarr(N)
	newDD=fltarr(N)
	neweDD=fltarr(N)
	bnewAA=fltarr(N)
	bnewBB=fltarr(N)
	bnewCC=fltarr(N)
	bnewDD=fltarr(N)
	bneweDD=fltarr(N)

if DD[0] ne -1 then begin

	newBB[BB]=1
	newDD[DD]=1
	neweDD[eDD]=1
	newAA[AA]=1
	newCC[CC]=1
endif
if nDD[0] ne -1 then begin
	bAA=N-1-reverse(nAA)
	bDD=N-1-reverse(nDD)
	ebDD=N-1-reverse(enDD)
	bCC=N-1-reverse(nCC)
	bBB=N-1-reverse(nBB)

	bnewBB[bBB]=1
	bnewDD[bDD]=1
	bneweDD[ebDD]=1
	bnewAA[bAA]=1
	bnewCC[bCC]=1
endif 



	store_data,"AA_flag",data={x:xs,y:newAA,ytitle:"flag"}
	store_data,"BB_flag",data={x:xs,y:newBB,ytitle:"flag"}
	store_data,"CC_flag",data={x:xs,y:newCC,ytitle:"flag"}
	store_data,"DD_flag",data={x:xs,y:newDD,ytitle:"flag"}
	store_data,"DD_effective_flag",data={x:xs,y:neweDD,ytitle:"flag"}

	store_data,"bAA_flag",data={x:xs,y:bnewAA,ytitle:"flag"}
	store_data,"bBB_flag",data={x:xs,y:bnewBB,ytitle:"flag"}
	store_data,"bCC_flag",data={x:xs,y:bnewCC,ytitle:"flag"}
	store_data,"bDD_flag",data={x:xs,y:bnewDD,ytitle:"flag"}
	store_data,"bDD_effective_flag",data={x:xs,y:bneweDD,ytitle:"flag"}
	;datae.y=newAE
	;datab.y=newAB
	;datde.y=newDE
	;datdb.y=newDB
	;store_data,'ascend_end_refined',data=datae
	;store_data,'ascend_begin_refined',data=datab
	;store_data,'descend_end_refined',data=datde
	;store_data,'descend_begin_refined',data=datdb

	store_data,'sublengths_Zero_inbound',data={x:xs,y:inSubsets,ytitle:"Non-Crustal sublengths",imaxs:inimaxs,imins:inimins}
	store_data,'sublengths_Zero_outbound',data={x:xs,y:outSubsets,ytitle:"Non-Crustal sublengths",imaxs:outimaxs,imins:outimins}

	bforecountdown=REVERSE(nforecountdown)
	bforeblock=REVERSE(nforeblock)
	store_data,'forecountdown_inbound',data={x:xs,y:forecountdown,ytitle:"number of foreshocks before shock",ymax:foremaxi}
	store_data,'forecountdown_outbound',data={x:xs,y:bforecountdown,ytitle:"number of foreshocks before shock",ymax:foremaxo}
	store_data,'foreblock_inbound',data={x:xs,y:foreblock,ytitle:"valid shocklocations in magnetosheath direction"}
	store_data,'foreblock_outbound',data={x:xs,y:bforeblock,ytitle:"valid shocklocations in magnetosheath direction"}
 	forecountdown+=bforecountdown
 	foreblock+=bforeblock
	foremaxi+=foremaxo
	store_data,'forecountdown',data={x:xs,y:forecountdown,ytitle:"number of foreshocks before shock",ymax:foremaxi}
	store_data,'foreblock',data={x:xs,y:foreblock,ytitle:"valid shocklocations in magnetosheath direction"}
end
	
