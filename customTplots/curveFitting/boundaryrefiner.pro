


pro boundaryrefiner,explot=explot

	if not keyword_set(explot) then explot="B_medianderiv_extrema"


	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=dat
	
	dobug=0

	tgtname=explot.remove(-13)
	print,tgtname
	get_data,tgtname,data=datB2

	B2=datB2.y
	nB2=reverse(B2)
	
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



	inSubsets=fltarr(N);xs*0.0;+1;-1


	outSubsets=fltarr(N);xs*0.0;+1;-1
	outends=fltarr(N);xs*0.0;+1;-1
	outbegs=fltarr(N);xs*0.0;+1;-1



	if dobug gt 1 then print,'type  2: curve fit'
		


	get_data,'regid_cleaned_plot_interpolated',data=datReg
	reg=datReg.y

	;get_data,'B5-3d10',data=datB5d      ;this data is Bmag smoothed over 5 second then 3 second average, then numerically derivativated then 10 second averaged
	;get_data,'B20deriv',data=datB20deriv


	;hill=datB5d.y
	;hill2=datB20deriv.y

	get_data,'B_sec_smoothed' ,data=datBSb
	get_data,'B_sec_smoothed_back' ,data=datBS

	get_data,'B_median',data=datBmed ;we want this data  for looking to see if we treat brief foreshocks as if they are the actual shock. 

	get_data,"rho30deriv",data=datRhod ; we want this data to avoid using crustal fields for downstream data:
	; if rho60 lt 1 then  few particles                                   
	get_data,"B_sec_smoothed_backderiv",data=datBsbd
	get_data,"B_sec_smoothedderiv",data=datBsd
	rhod=datRhod.y
	nrod=-1*REVERSE(rhod)

	help,datRhod
	Bmed=datBmed.y
	yBS=datBS.y
	yBSb=datBSb.y

	BSd=datBsd.y
	BSbd=datBSbd.y

	get_data,'swics_v5_interpolated',data=datv5 ;;WE'll be operating close enough to shock that fine structures may not always operate correctly
	v5=datv5.y
	nv5=REVERSE(v5)

	;------------------
		

	mbeg=0
	mend=0
	incfunctions=1
	decfunctions=1
	trust=0
		
	get_data,"PE_half",data=datpeh
	get_data,"te_half",data=datteh
	get_data,"ne_half",data=datneh

	;get_data,"PE_10sec",data=datpe10
	;get_data,"te_10sec",data=datte10
	;get_data,"ne_10sec",data=datne10

	peh=datpeh.y
	neh=datneh.y
	teh=datteh.y

	;peh=datpe10.y
	;neh=datne10.y
	;teh=datte10.y

	nneh=reverse(neh)
	nteh=reverse(teh)
	npeh=reverse(peh)



	get_data,'ascend_end_interpolated',data=datae
	get_data,'ascend_begin_interpolated',data=datab
	get_data,'descend_end_interpolated',data=datde
	get_data,'descend_begin_interpolated',data=datdb

	yae=datae.y
	yab=datab.y
	yde=datde.y
	ydb=datdb.y
	store_data,'ascend_end_refined',data={x:xs,y:yae*0.0}
	store_data,'ascend_begin_refined',data={x:xs,y:yae*0.0};datab
	store_data,'descend_end_refined',data={x:xs,y:yae*0.0};datde
	store_data,'descend_begin_refined',data={x:xs,y:yae*0.0};datdb
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

	get_data,explot,data=datEx
	extr=datEx.y	
	nextr=-1*REVERSE(extr)

	AA0=AA

	BB0=BB
	CC0=CC
	DD0=DD
	eDD=DD
	;------------------

		;'variables that would otherwise be defined in loops'

	mbeg=0
	mend=0
	incfunctions=1
	decfunctions=1
		;trust=0
	g0=0;
	b0=0
		
		
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

	inimaxs=fltarr(N)-1;xs*0.0-1
	outimaxs=fltarr(N)-1;;xs*0.0-1

	inimins=fltarr(N)-1;xs*0.0-1
	outimins=fltarr(N)-1;xs*0.0-1



	innums=fltarr(N)-1;xs*0.0-1
	outnums=fltarr(N)-1;xs*0.0-1


	forecountdown=fltarr(N)-1;xs*0.0
	nforecountdown=fltarr(N)-1;xs*0.0

	foreblock=fltarr(N)
	nforeblock=fltarr(N)
		
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

			inSubsets[var[0]:var[1]]=1
			imx=var[1]
			imn=var[0]

			inimaxs[imn:imx]=imx
			inimins[imn:imx]=imn

			innums[imn:imx]=i

		endfor
		
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



nz=fltarr(N);z*0.0


nDD=N-1-reverse(AA0)
nAA=N-1-reverse(DD0)
nBB=N-1-reverse(CC0)
nCC=N-1-reverse(BB)
enDD=nDD
nddn=aan
nbbn=ccn
nccn=bbn
naan=ddn
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
;if (numel(nBB) gt 1) and (numel(nAA) gt 1) then begin	get_data,'swics_v5_interpolated',data=datv5 ;;WE'll be operating close enough to shock that fine structures may not always operate correctly
	v5=datv5.y
	nv5=REVERSE(v5)

doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if numel(nBB) gt 1 then dboffsetb=1*(nBB[1] lt nDD[0]) else  dboffset=0
if numel(nAA) gt 1 then daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0
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


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		lastmax=0
	for i=1,fullIter-1  do begin
			var=midboundaryrefiner(i,nys,nAA,nBB,nCC,nDD,nBmed,nrhod,nBsbd,nextr,nforecountdown,enDD,nforeblock,nB2,nv5,nneh,npeh,nteh)
			imx=var[1]
			imn=var[0]
			outSubsets[imn:imx]=1


			outimaxs[imn:imx]=N-1-imn
			outimins[imn:imx]=N-1-imx

			innums[imn:imx]=i

	endfor 
;endif
	outSubsets=REVERSE(outSubsets)
	outimaxs=REVERSE(outimaxs)
	outimins=REVERSE(outimins)

	DD=N-1-reverse(nAA)
	AA=N-1-reverse(nDD)
	BB=N-1-reverse(nCC)
	CC=N-1-reverse(nBB)
NODIM: print,'NO DIMS'
	newAE=fltarr(N)
	newAB=fltarr(N)
	newDE=fltarr(N)
	newDB=fltarr(N)

 	;forecountdown+=reverse(nforecountdown)
	newDE[BB]=1
	newAE[DD]=1
	newDB[AA]=1
	newAB[CC]=1
	;datae.y=newAE
	datab.y=newAB
	datde.y=newDE
	;datdb.y=newDB
	store_data,'ascend_end_refined',data=datae
	store_data,'ascend_begin_refined',data=datab
	store_data,'descend_end_refined',data=datde
	store_data,'descend_begin_refined',data=datdb

	store_data,'sublengths_Zero_inbound',data={x:xs,y:inSubsets,ytitle:"Non-Crustal sublengths",imaxs:inimaxs,imins:inimins}
	store_data,'sublengths_Zero_outbound',data={x:xs,y:outSubsets,ytitle:"Non-Crustal sublengths",imaxs:outimaxs,imins:outimins}

	;store_data,'forecountdown',data={x:xs,y:forecountdown,ytitle:"number of foreshocks before shock"}
end
	
