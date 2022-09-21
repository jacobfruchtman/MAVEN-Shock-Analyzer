pro threshFlag, plt,t,kindThresh=kindThesh,mnt=mnt,newName=newName,dointerpolate=dointerpolate,interplot=interplot,monoNAME=monoName,truesave=truesave
	get_data,plt,data=datp
	get_data,'mvn_swics_density',data=dat0,alim=limD
	if NOT KEYWORD_SET(dointerpolate) THEN dointerpolate=0	
	if NOT KEYWORD_SET(interplot) THEN interplot='mvn_B_1sec_Mag'
	get_data,interplot,data=datB
	print,size(datB.x)
	if NOT KEYWORD_SET(newName) THEN newName=plt
	if NOT KEYWORD_SET(monoName) THEN monoName=""
	name=newName+"_flag"
	
	if NOT KEYWORD_SET(mnt) THEN mnt=0

	if NOT KEYWORD_SET(kindThresh) THEN kindThresh=0

	typ=threshtypecheck(kindThesh)
	print,"typ="+string(typ)
	y=datp.y
	x=datp.x
	x2=0
	x3=0

	typs=threshtypbackup(typ)
	typi=typs[0];what to do  at y[0]
	typf=typs[1];what to do at y[N-1]
	n=Size(y,/N_ELEMENTS)-1
	print,n
	
	if(dointerpolate ne 0) then begin
		y=interpol(y,x,datB.x)
		x=datB.x
	endif
	print,"size(y)=",size(y)
	dw=fltarr(n+1);y*0.0
	
	;if we are interested in a case that involves changing  x

	;list of types that use y[i-1]:
	bef=[5,7,8,9,10,11,14,16,17,18,21,22,23,26,27,28,29]
	;list of types that use  y[i+1]:
	aft=[6,7,8,9,10,11,15,16,17,18,19,20,23,26,27,28,29]
	;list of types that use mnt:
	monty=[12,13,23]
	ret=total(bef eq typ) eq 1
	adv=total(aft eq typ) eq 1
	carl=total(monty eq typ) eq 1
	
	extras=[ret,adv,carl,mnt]

	firstloc=(where( (y gt t and y lt mnt) or (y lt t and y gt mnt) ))[0]

	if (typ ne 23) and (typ ne 25) then begin
		if(ret) then  dw[0]=threshcalc(y[0],t,0,y[1],typi,truesave=truesave) 
		print,"loop 1"
		FOR i = 0+ret, n-adv DO BEGIN
			if carl then x2 = mnt
			if ret then x2=y[i-1] 
			if adv then x3=y[i+1]
			first=1*(i eq firstloc)
			if first then print,"first"
	   		dw[i]=threshcalc(y[i], t,x2,x3,typ,truesave=truesave,first=first)
			;if(dw[i] EQ 1) AND (dw[i-2] EQ 1) THEN dw[i-1]=1
			;if(dw[i] EQ 0) AND (dw[i-2] EQ 0) THEN dw[i-1]=0
			;if(dw[i] ne 0) then print,"("+string(i)+","+string(dw[i])+")"
		ENDFOR
		print,dw[2]
		if(adv) then  dw[n]=threshcalc(y[n],t,y[n-1],0,typf,truesave=truesave) 
	endif else if typ eq 23 then begin
		
		FOR i = 0+ret+mnt, n-adv-mnt DO BEGIN
			x2=y[i-1] 
			x3=y[i+1]
		
	   		dw[i-mnt]=threshcalc(y[i], t,x2,x3,17,truesave=truesave)
	   		dw[i+mnt]=threshcalc(y[i], t,x2,x3,18,truesave=truesave)
			;if(dw[i] EQ 1) AND (dw[i-2] EQ 1) THEN dw[i-1]=1
			;if(dw[i] EQ 0) AND (dw[i-2] EQ 0) THEN dw[i-1]=0
			;if(dw[i] ne 0) then print,"("+string(i)+","+string(dw[i])+")"
		ENDFOR
	endif else  begin 

		mnflag=monotize2(y,t)
		bmnflag=reverse(monotize2(reverse(y),t))

		store_data,'monof'+monoName,data={x:datp.x,y:mnflag,ytitle:'mono-flag'}
		store_data,'bmonof'+monoName,data={x:datp.x,y:bmnflag,ytitle:'bmonof'}

		monoy=mnflag*y
		bmonoy=bmnflag*y
		
		fdescend=1*(monoy eq t) ; region where threshold is crossed from below (and where for regid, Maven leaves solar wind)
		fascend=1*(bmonoy eq t) ; region where threshold is crossed from above (and where for regid, Maven enters solar wind)
		nd=newName+'descend'+monoName
		na=newName+'ascend'+monoName

		store_data,nd+'_flag',data={x:datp.x,y:[[fdescend],[fdescend]],v:[0,1]}
		store_data,na+'_flag',data={x:datp.x,y:[[fascend],[fascend]],v:[0,1]}
	options,nd+'_flag','spec',1	;switch from line plots to spectrogram mode
	options,nd+'_flag','no_interp',1

	options,na+'_flag','spec',1	;switch from line plots to spectrogram mode
	options,na+'_flag','no_interp',1

		store_data,nd,data={x:datp.x,y:fdescend,ytitle:nd+'_flag'}
		store_data,na,data={x:datp.x,y:fascend,ytitle:nd+'_flag'}


		;dw=fdescend+fascend
		

		faf=fltarr(n+1)

		fdf=fltarr(n+1);dw*0.0

		for i=0, N-2 do begin
			if (fascend[i] eq 1) and (fascend[i+1] eq 0) then faf[i]=1 
			if (fdescend[i] eq 1) and (fdescend[i+1] eq 0) then fdf[i]=1 
		endfor 



		GG=Where(faf ne 0 and finite(faf), gcount,COMPLEMENT=G_c, NCOMPLEMENT=gcount_c)
		print,'gcount=',gcount
		xG=x[GG]
		fG=fascend[GG]
		;print,xG
		HH=Where(fdf ne 0 and finite(fdf), hcount,COMPLEMENT=H_c, NCOMPLEMENT=hcount_c)
	;	print,'hcount=',hcount
		xH=x[HH]
		fH=fdescend[HH]
	;	print,xH
	
	;	print,HH[0]
		if HH[0] eq -1 and GG[0] eq -1 then return
		;perdh=intarr(hcount-1)
		;perdd[0]=GG[0]
		;For i=0,hcount-2 do perdh[i]=HH[i+1]-HH[i]

		;print,'perdh=',perdh

		ffg=fltarr(n+1);faf*0.0

		ffh=fltarr(n+1);fdf*0.0
		ffh[HH]=1
		ffg[GG]=1
		;for i=0, min([gcount,hcount])-1 do begin

			;ig=GG[i]

			;ih=HH[i]

			;ffg[ig]=1

			;ffh[ih]=1


		;endfor

		print,"size(ffg)="+string(size(ffg,/n_el))
		store_data,'ascend_end',data={x:datp.x,y:ffg,ytitle:'flag'}
		store_data,'descend_end',data={x:datp.x,y:ffh,ytitle:'flag'}

		iaf=fltarr(n+1);dw*0.0

		idf=fltarr(n+1);dw*0.0

		for i=1, N-1 do begin
			if (fascend[i] eq 1) and (fascend[i-1] eq 0) then iaf[i]=1 
			if (fdescend[i] eq 1) and (fdescend[i-1] eq 0) then idf[i]=1 
		endfor 



		iGG=Where(iaf ne 0 and finite(iaf) eq 1, gcount,COMPLEMENT=G_c, NCOMPLEMENT=gcount_c)
		print,'gcount=',gcount
		xiG=x[iGG]
		iG=fascend[iGG]
		print,xiG
		iHH=Where(idf ne 0 and finite(idf) eq 1, hcount,COMPLEMENT=H_c, NCOMPLEMENT=hcount_c)
		print,'hcount=',hcount
		if hcount lt 1 then return
		xiH=x[HH]
		iH=fdescend[iHH]
		print,xiH
	
		print,iHH[0]
		if iHH[0] eq -1 then return
		;perdh=intarr(hcount-1)
		;perdd[0]=GG[0]
		;For i=0,hcount-2 do perdh[i]=iHH[i+1]-iHH[i]

		;print,'perdh=',perdh

		ifg=fltarr(n+1);iaf*0.0

		ifh=fltarr(n+1);idf*0.0

		;for i=0, min([gcount,hcount])-1 do begin

			;iig=iGG[i]

			;iih=iHH[i]

			;ifg[iig]=1

			;ifh[iih]=1


		;endfor
		ifg[iGG]=1
		ifh[iHH]=1
		print,"size(ifg)="+string(size(ifg,/n_el))
		store_data,'ascend_begin',data={x:datp.x,y:ifg,ytitle:'flag'}
		store_data,'descend_begin',data={x:datp.x,y:ifh,ytitle:'flag'}
		print,"This will crash in a line or two. This is fine. We've accomplished what we've wanted"
		Return

	endelse


	print,"making flag 1"
	newdat={x:datp.x,y:[[dw],[dw]],v:[0,1]}
	newdatTR={x:datp.x,y:dw}
	
	print,"truesave=",keyword_set(truesave)
	store_data,name,data=newdat
	store_data,newName+"_trigger",data=newdatTR
	;tplot,title=plt+"AND_threshflag",[plt,name]
	options,name,'spec',1	;switch from line plots to spectrogram mode
	options,name,'no_interp',1
	print,"entering i=2 to n loop"
	;if typ ne 23 then begin
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=2,truesave=truesave)
		print,"entering i=3 to n loop"
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=3,truesave=truesave)
		print,"entering i=4 to n loop"
	
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=4,truesave=truesave)
		print,"entering i=5 to n loop"

		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=5,truesave=truesave)
		print,"entering i=60 to n loop"

		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=60,truesave=truesave)
		;FOR i = 2+ret, n-adv DO BEGIN	
	 	 ; 	if ret then x2=y[i-1] 
		;	if adv then x3=y[i+1]
	 	 ; 	dw[i]=threshcalc(y[i], t,x2,x3,typ)
		;	if(dw[i] EQ 1) AND (dw[i-2] EQ 1) THEN dw[i-1]=1
		;if(dw[i] EQ 0) AND (dw[i-2] EQ 0) THEN dw[i-1]=0
		;print,dw[i]
		;ENDFOR	
		;print,"entering i=3 to n loop"
	;	FOR i = 3+ret, n-adv DO BEGIN	
	;	   	if ret then x2=y[i-1] 
	;		if adv then x3=y[i+1]
	;	   	dw[i]=threshcalc(y[i], t,x2,x3,typ)
	;		if(dw[i] EQ 1) AND (dw[i-3] EQ 1) THEN BEGIN
	;		 dw[i-2]=1
	;		 dw[i-1]=1
	;		ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
	;	ENDFOR	
	;	print,"entering i=4 to n loop"
	;	FOR i = 4+ret, n-adv DO BEGIN	
	;	   	if ret then x2=y[i-1] 
	;		if adv then x3=y[i+1]
	 ; 	 	dw[i]=threshcalc(y[i], t,x2,x3,typ)
	;		if(dw[i] EQ 1) AND (dw[i-4] EQ 1) THEN BEGIN
		;	 dw[i-3]=1
		;	 dw[i-2]=1
		;	 dw[i-1]=1
		;	ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
		;ENDFOR	
		;print,"entering i=5 to n loop"
		;FOR i = 5+ret, n-adv DO BEGIN	
	   	;	if ret then x2=y[i-1] 
		;	if adv then x3=y[i+1]
	   	;	dw[i]=threshcalc(y[i], t,x2,x3,typ)
		;	if(dw[i] EQ 1) AND (dw[i-5] EQ 1) THEN BEGIN
		;		 dw[i-4]=1
		;		 dw[i-3]=1
		;		 dw[i-2]=1
		;		 dw[i-1]=1
		;	ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
		;ENDFOR	
		;print,"entering i=60 to n loop"
		;FOR i = 60+ret, n-adv DO BEGIN	
	   	;	if ret then x2=y[i-1] 
		;	if adv then x3=y[i+1]
	   	;	dw[i]=threshcalc(y[i], t,x2,x3,typ)
		;	if(dw[i] EQ 1) AND (dw[i-60] EQ 1) THEN BEGIN
		;		For j=1,59 DO BEGIN
		;			dw[i-j]=1
		;		ENDFOR 
		;	ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
		;ENDFOR	
	
	print,dw[n-2]
	print,size(dw)
	print,"making flag 1"
	print,max(dw)
	B = WHERE(dw ne 0, count, COMPLEMENT=B_C, NCOMPLEMENT=count_c);new array with locations of nonzero elements
	print,"count=",count
	;print,B
	newdat2={x:datp.x,y:[[dw],[dw]],v:[0,1]}

	name2=newName+"_flag2"
	store_data,name2,data=newdat2
	options,name2,'spec',1	;switch from line plots to spectrogram mode
	options,name2,'no_interp',1

	newdatTR2={x:datp.x,y:dw}
	

	store_data,newName+"2_trigger",data=newdatTR2

end

