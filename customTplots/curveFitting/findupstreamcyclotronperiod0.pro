pro findupstreamcyclotronperiod,snrThresh=snrThresh,isnrThresh=isnrThresh

		if not keyword_set(snrThresh) then snrThresh=20
		if not keyword_set(isnrThresh) then isnrThresh=.5

		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'bDD_flag',data=datbDD
		get_data,'forecountdown',data=datFore

		fore=datFore.y


		get_data,'DD_effective_flag',data=dateDD
		get_data,'bDD_effective_flag',data=datebDD
		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		eDD = WHERE(dateDD.y ne 0, eddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(datAA.y ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock

		enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		nDD = WHERE(REVERSE(datbDD.y) ne 0, nddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		nAA = WHERE(REVERSE(datbAA.y) ne 0, naan, COMPLEMENT=nH_C, NCOMPLEMENT=nhcount_c); beginnging of shock 

		nCC = WHERE(REVERSE(datbCC.y) ne 0, nccn, COMPLEMENT=nK_C, NCOMPLEMENT=nkcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbBB.y) ne 0, nbbn, COMPLEMENT=nL_C, NCOMPLEMENT=nlcount_c); end of  negative  shock
		
		get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datB
		get_data,"B_3sec",data=datB3
		get_data,"B_half",data=datB30

		B=datB.y
		B3=datB3.y
		B30=datB30.y
		nB3=REVERSE(B3)
		nB30=REVERSE(B30)
		N=numel(B)
		xs=datB.x
		get_data,"B_SNR",data=datSNR
		get_data,"B_SNR_inverse",data=datiSNR

		SNR=datSNR.y

		iSNR=datiSNR.y

		nB=reverse(B)
		nSNR=REVERSE(SNR)

		niSNR=reverse(iSNR)

		if DD[0] gt AA[0] then begin
			DD=[0,DD]
			ddn++
		endif
		if nDD[0] gt nAA[0] then begin
			nDD=[0,nDD]
			nddn++
		endif

		if eDD[0] gt AA[0] then begin
			eDD=[0,eDD]
			eddn++
		endif
		if enDD[0] gt nAA[0] then begin
			enDD=[0,enDD]
			enddn++
		endif

		if BB[-1] lt AA[-1] then begin
			BB=[BB,N-1]
			bbn++
		endif
		if BB[0] lt AA[0] then begin
			BB=BB[1:*]
			bbn--
		endif

		if nBB[0] lt nAA[0] then begin
			nBB=nBB[1:*]
			nbbn--
		endif
		if nBB[-1] lt nAA[-1] then begin
			nBB=[nBB,N-1]
			nbbn++
		endif

	doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
	aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
	;dboffsetb=1*(nBB[1] lt nDD[0])
	;daoffsetb=1*(nAA[1] lt nDD[0])

doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

			backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if numel(BB) gt 1 then boffsetb=1*(BB[1] lt DD[0]) else  boffsetb=0
if numel(AA) gt 1 then aoffsetb=1*(AA[1] lt DD[0]) else aoffsetb=0
			;boffsetb=1*(BB[1] lt DD[0])
			;aoffsetb=1*(AA[1] lt DD[0])
		fullIter=min([ddn,aan,bbn])
		nfullIter=min([nddn,naan,nbbn])
		for i=0, fullIter-1 do print, "[DD,AA,BB][",i,"]=",[DD[i],AA[i],BB[i]]


	
		z=fltarr(N)
		nz=fltarr(N)
		inbottoms=fltarr(N)
		outbottoms=inbottoms

		Bu=fltarr(N)
		outupdated=0
		inupdated=0
		updated=0
		;for i=0, fullIter-1 do print, "[nDD,nAA,nBB][",i,"]=",[nDD[i],nAA[i],nBB[i]]
		for i=0, fullIter-1 do begin
				lineError=""
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				catch,/cancel
			      Help, /Last_Message, Output=theErrorMessage
			      FOR j=0,N_Elements(theErrorMessage)-1 DO BEGIN
				print,theErrorMessage[j]
         			lineError=theErrorMessage[j]
      				ENDFOR

				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				print,lineError
			;PRINT,'ERROR WHEN PERFORMING '+errorloc
				errorloc=""
				xaaa=xs[AA[i]]
				errmsg=["error in findupstreamcyclotronperiod inbound", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc,lineError]
				errorsaver,xaaa,errmsg,/spec
				
				continue
			endif
			localbadfore=0

			print,"i=",i
			dim=DD[i];-1+doffsetf]
			aj=AA[i];-aoffsetf +aoffsetb]
			bi=BB[i];+boffsetb]

			outaj=(where(N-1-reverse(nAA) lt aj))[-1]
			dhalf=mean([dim+10,aj])
			if outaj ne -1 then begin

				outdim=(where(N-1-reverse(nDD) gt outaj and  N-1-reverse(nDD) lt aj))[0]
				if outdim ne -1 and mean([outdim,aj]) gt dim then dhalf=dim

			endif

			edim=enDD[i]
			
			print,"[dim,dhalf,aj,bi]=",[dim,dhalf,aj,bi]
			cyclotronperiodsaver, dim,aj,bi,B,SNR,iSNR,z,bottom,dhalf, snrThresh=snrThresh,isnrThresh=isnrThresh
			inbottoms[bottom]=1
			t=z[bottom]
			mnlc=max([0,bottom-t])
			mxlc=bottom+t+(bottom-t)-mnlc
			Bu[dhalf:bi]=mean(B3[mnlc:mxlc])
			edimm=edim
			aj0=aj
			forerefinder,dim,edim,aj,bi,fore,Bu,B30,updated=localbadfore
			print,"localbadfore=",localbadfore
			print,"edim-edimm=",edim-edimm
			if localbadfore then begin
				if edim ne 0 then dateDD.y[edimm]=0
				if edim ne 0 then dateDD.y[edim]=1
				datAA.y[aj0]=0
				datAA.y[aj]=1
				
				updated=1
				inupdated=1

			endif
		endfor


		nfore=reverse(fore)
		;z=reverse(z)
		print,"back"
		Bu=REVERSE(Bu)

if numel(nBB) gt 1 then dboffsetb=1*(nBB[1] lt nDD[0]) else  dboffsetb=0
if numel(nAA) gt 1 then daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0


	for i=0 ,nfullIter-1 do begin
	j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	ii=i+dboffsetb
	aj=nAA[j]
	bi=nBB[ii]
	dim=nDD[k]
	edim=enDD[k]
	print,'[nDD[',k,'],enDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,edim,aj,bi]
	
endfor

		for i=0, nfullIter-1 do begin
				lineError=""
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				catch,/cancel
			      Help, /Last_Message, Output=theErrorMessage
			      FOR j=0,N_Elements(theErrorMessage)-1 DO BEGIN
         			lineError=theErrorMessage[j]
      				ENDFOR
				print,lineError

				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
				errorloc=""
				xaaa=xs[N-1-nAA[i]]
				errmsg=["error in findupstreamcyclotronperiod outbound", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc,lineError]
				errorsaver,xaaa,errmsg,/spec
				
				continue
			endif

			localbadfore=0
			print,"i=",i

			j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			k=i-1+doffsetb
			ii=i+dboffsetb
			dim=nDD[k];-1+doffsetb]
			aj=nAA[j];-aoffsetb+daoffsetb]
			bi=nBB[ii];+dboffsetb]
			edim=enDD[k]
			dhalf=mean([dim,aj])
			dhalf=mean([dim+10,aj])
			outaj=(where(N-1-reverse(AA) lt aj))[-1]
			if outaj ne -1 then begin

				outdim=(where(N-1-reverse(DD) gt outaj and  N-1-reverse(DD) lt aj))[0]
				if outdim ne -1 and mean([outdim,aj]) gt dim then dhalf=dim

			endif
			cyclotronperiodsaver, dim,aj,bi,nB,nSNR,niSNR,nz,nbottom,dhalf,snrThresh=snrThresh,isnrThresh=isnrThresh
			outbottoms[nbottom]=1
			t=nz[nbottom]
			print,"tau=",t
			mnlc=max([0,nbottom-t])
			mxlc=min([nbottom+t+(nbottom-t)-mnlc,N-1])
			Bu[dhalf:bi-1]=mean(nB3[mnlc:mxlc])

			aj0=aj
			edimm=edim
			print,"dim,edim,aj,bi=",dim,edim,aj,bi
			forerefinder,dim,edim,aj,bi,nfore,Bu,nB30,updated=localbadfore
			print,"localbadfore=",localbadfore
			print,"edim-edimm=",edim-edimm
			if localbadfore then begin
				if edim ne 0 then datebDD.y[N-1-edimm]=0
				if edim ne 0 then datebDD.y[N-1-edim]=1
				datbAA.y[N-1-aj0]=0
				datbAA.y[N-1-aj]=1
				
				updated=1
				outupdated=1

			endif

		endfor
		fore=reverse(nfore)
		outbottoms=reverse(outbottoms)
		zb=reverse(nz)

		zf=fltarr(N)

		bAA=N-1-REVERSE(nAA)
	for i=0, N-1 do begin
		aj =(where( abs(i-AA) eq min(abs(i-AA))))[0]
		naj =(where( abs(i-bAA) eq min(abs(i-bAA))))[0]

		
		isOut=1*(zb[i] ne 0)
		isIN=1*(z[i] ne 0)
		;imn=inimins[i]
		;imx=inimins[i]
		;omn=outimins[i]
		;omx=outimaxs[i]
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-naj) ge abs(i-aj))) then begin;and i ge inimins[i] and i le inimaxs[i] then begin
			zf[i]=z[i]
		ENDIF ELSE BEGIN 
			IF (isOut and (not isIN)) or  ( (isOut+isIn eq 2) and (abs(i-naj) lt abs(i-aj))) then begin;and i ge outimins[i] and i le outimaxs[i] then begin
				zf[i]=zb[i]
			ENDIF 	
		ENDELSE

		;if zin[i] ne 0 then zout[i]=zin[i]
		
		;if outdowns[i] ne 0 then downs[i] = outdowns[i]
		;if outups[i] ne 0 then ups[i] = outups[i]
		;if outShockLocs[i] ne -1 then shockLocs[i] = outShockLocs[i]
		;if outdownups[i] ne 0 then downups[i] = outdownups[i]
		;if outimaxs[i] ne -1 then imaxs[i]=outimaxs[i]
		;if outimins[i] ne -1 then imins[i]=outimins[i]
	endfor

		Bu=REVERSE(Bu)
		store_data,"proton_cyclotron_period_inbound",data={x:datB.x,y:z,ytitle:"Time [seconds]"}
		store_data,"proton_cyclotron_period_outbound",data={x:datB.x,y:zb,ytitle:"Time [seconds]"}
		store_data,"proton_cyclotron_period",data={x:datB.x,y:zf,ytitle:"Time [seconds]"}

		store_data,"foot_start_inbound",data={x:datB.x,y:inbottoms,ytitle:"flag"}

		store_data,"foot_start_outbound",data={x:datB.x,y:outbottoms,ytitle:"flag"}

		store_data,"foot_start",data={x:datB.x,y:inbottoms+outbottoms,ytitle:"flag"}

		store_data,"B_up_average",data={x:datB.x,y:Bu,ytitle:"Magnetic Field [nT]"}
		options,"B_up_average",'colors','y'
		store_data,"Bu",data="mvn_B_1sec_MAVEN_MSO_Mag B_up_average"

		GG3=where(B gt 1.5+ Bu,complement=nGG3)

		outofup=fltarr(N)
		inup=fltarr(N)
		outofup[GG3]=1
		inup[nGG3]=1
		store_data,"B_above-up_regions",data={x:datB.x,y:outofup,ytitle:"Magnetic Field [nT]"}
		store_data,"B_below-up_regions",data={x:datB.x,y:inup,ytitle:"Magnetic Field [nT]"}
		options,"B_above-up_regions",'colors','r'

		if NOT updated then return

		if inupdated then begin
			store_data,'AA_flag',data=datAA
			store_data,'DD_effective_flag',data=dateDD

		endif
		if outupdated then begin
			store_data,'bAA_flag',data=datbAA
			store_data,'bDD_effective_flag',data=datebDD

		endif

		datFore.y=fore

		store_data,'forecountdown',data=datFore

end
