

function thr,x,t,a,b,typ
	;if NOT KEYWORD_SET(t2) THEN t2=0
	;if NOT KEYWORD_SET(t3) THEN t3=0
	;if NOT KEYWORD_SET(kindThresh) THEN typ=0
	
	CASE typ of
	0: return, 1*(x GT t);
	1: return, 1*(x Ge t);
	2: return, 1*(x lt t);
	3:  return, 1*(x le t);
	4: return, (x eq t);
	5: return, 1*((x eq t) and (a ne t));hit
	6: return, 1*((x eq t) and (a ne t));leave	
	7:return, 1*(((x eq t) and (a eq t))or ((x eq t) and (b eq t))) 
	;orT
		
	8: return, 1*((x eq t) and  ((a ne t) or (b ne t)))
	;orF

	9:  return, 1*((x eq t) and  ((a eq t) and (b eq t)))
	;T_and
	10: return, 1*((x eq t) and  ((a ne t) and (b ne t)))
	;F_and
	11: return, 1*(((x eq t) and (a eq t))xor ((x eq t) and (b eq t))) ;xor
	12: return, 1*(((x le t) and(x ge a)) or ((x ge t) and(x le a))) ;closed set
	13: return, 1*(((x lt t) and(x gt a)) or ((x gt t) and(x lt a))) ;open set
	14: return, 1*((a eq t) and (x eq t)) ;unmoved
	15: return, 1*((b eq t) and (x eq t)) ;wont move
	16: return, 1*((x eq t) and (((a  lt t )and (b gt t))or ((a gt t)and( b lt t ))))
 ;pass
	17: return, 1*((x eq t) and ((a  lt t)and (b gt t))) ;ascend
	18: return, 1*((x eq t) and ((a  gt t)and (b lt t))) ;descend
	19: return, 1*((x eq t) and (b lt t)) ;drop
	20: return, 1*((x eq t) and (b gt t)) ;rise
	21: return, 1*((x eq t) and (a  gt t)) ;fell
	22: return, 1*((x eq t) and (a  lt t)) ;rose

	24: return, 1*((x lt t)and(a lt t)and(b lt a))

	26: return, 1*( ((x  gt a)and (x gt b))) ;max
	27: return, 1*( ((x  lt a)and (x lt b))) ;min
	ENDCASE

	return, 1*(x eq t)
end



function typeCheck,kindThresh
	errorBuilder=[0,1]
	typ=-1
	print,kindThresh
	;want to include multiple threshold inplementations (x>t, x==t, x[i]==t but x[i]!=t,t>x>mnt, etc
	;that's what typ is for. this procedure allows the following values for typ
	kindThresh=string(kindThresh)
	if (kindThresh eq string(0)) or  (kindThresh eq "greater") or  (kindThresh eq "grtr")or  (kindThresh eq "gt") then typ=0$
	;returns dat.y[i] gt t forall i
	else if (kindThresh eq string(1)) or  (kindThresh eq "greaterEqual") or  (kindThresh eq "grtrEq")or  (kindThresh eq "ge") then typ=1$
	;returns dat.y[i] ge t forall i
	else if (kindThresh eq string(2)) or  (kindThresh eq "less than") or (kindThresh eq "less") or  (kindThresh eq "lt") then typ=2$
	;returns dat.y[i] lt t forall i
	else if (kindThresh eq string(3)) or  (kindThresh eq "less or Equal") or (kindThresh eq "lessEq") or  (kindThresh eq "le") then typ=3$
	;returns dat.y[i] le t forall i
	else if (kindThresh eq string(4)) or  (kindThresh eq "Equal") or (kindThresh eq "eq") then typ=4$
	;returns dat.y[i] eq t forall i
	else if (kindThresh eq string(5)) or  (kindThresh eq "hit") then typ=5$
	;returns true if dat.y[i-1] ne t and dat.y[i] eq t

	else if (kindThresh eq string(6)) or  (kindThresh eq "leave") then typ=6$
	;returns true if dat.y[i+1] ne t and dat.y[i] eq t

	else if (kindThresh eq string(7)) or  (kindThresh eq "T_or") then typ=7$
	;returns true if (dat.y[i+1] eq t and dat.y[i] eq t) or  (dat.y[i-1] eq t and dat.y[i] eq t)
	else if (kindThresh eq string(8)) or  (kindThresh eq "F_or") then typ=8$
	;returns true if (dat.y[i+1] ne t and dat.y[i] eq t) or  (dat.y[i-1] ne t and dat.y[i] eq t)

	else if (kindThresh eq string(9)) or  (kindThresh eq "T_and") or (kindThresh eq "stable") then typ=9$
	;returns true if (dat.y[i+1] eq t) and  (dat.y[i-1] eq t) and (dat.y[i] eq t)


	else if (kindThresh eq string(10)) or  (kindThresh eq "F_and") or (kindThresh eq "unstable") then typ=10$
	;returns true if (dat.y[i+1] ne t) and  (dat.y[i-1] ne t) and (dat.y[i] eq t)

	else if (kindThresh eq string(11)) or  (kindThresh eq "xor") then typ=11$
	;returns true if (dat.y[i+1] ne t and dat.y[i] eq t) xor  (dat.y[i-1] ne t and dat.y[i] eq t)
	else if (kindThresh eq string(12)) or  (kindThresh eq "bound") then typ=12$
	;returns true if (dat.y[i] le t and dat.y[i] ge mnt)
	else if (kindThresh eq string(13)) or  (kindThresh eq "between") then typ=13$
	;returns true if (dat.y[i] lt t and dat.y[i] gt mnt) 
	else if (kindThresh eq string(14)) or  (kindThresh eq "unmoved") then typ=14$
	;returns true if (dat.y[i-1] eq t and dat.y[i] eq t)
	else if (kindThresh eq string(15)) or  (kindThresh eq "unmoving") then typ=15$
	;returns true if (dat.y[i+1] eq t and dat.y[i] eq t) (or creates an error)
	else if (kindThresh eq string(16)) or  (kindThresh eq "pass") then typ=16$
	; returns true if y goes passes  through t from above or below
	else if (kindThresh eq string(17)) or  (kindThresh eq "ascend") then typ=17$
	; returns true if y passes from below
	else if (kindThresh eq string(18)) or  (kindThresh eq "descend") then typ=18$
	; returns true if y passes from above
	else if (kindThresh eq string(19)) or  (kindThresh eq "drop")or  (kindThresh eq "fall") then typ=19$
	; returns true if y[i+1] lt t andd y[i] eq t
	else if (kindThresh eq string(20)) or  (kindThresh eq "rise") then typ=20$
	; returns true if y[i+1] gt t andd y[i] eq t
	else if (kindThresh eq string(21)) or  (kindThresh eq "fell") then typ=19$
	; returns true if y[i-1] gt t and y[i] eq t
	else if (kindThresh eq string(22)) or  (kindThresh eq "rose") then typ=20$
	; returns true if y[i-1] lt t and y[i] eq t
	else if (kindThresh eq string(23)) or  (kindThresh eq "delay") then typ=23$
	; special type which returns true to an array element mnt behind if passing from below and mnt ahead  if passing from above
	else if (kindThresh eq string(25)) or  (kindThresh eq "pause") then typ=25$
	else if (kindThresh eq string(26)) or  (kindThresh eq "max") then typ=26$
	; if daty[i] ge daty[i-1] and daty[i] gt daty[i+1]
	else if (kindThresh eq string(27)) or  (kindThresh eq "min") then typ=27$
	; if daty[i] le daty[i-1] and daty[i] lt daty[i+1]
	 else  return, errorBuilder[3]

	print,typ
	return, typ
end


function typBackup, typ

	typi=typ
	typf=typ
	CASE typ OF

	0: BEGIN;gt
	         PRINT, 'gt'

	       END
	1: Begin;ge
		 PRINT, 'ge'
		end
	2: Begin;lt
		 PRINT, 'lt'
		end
	3: Begin;le
		PRINT, 'le'
		end
	4: Begin;eq
		PRINT, 'eq'
		end
	5: Begin;hit
		PRINT, 'hit'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	6: Begin;leave
		PRINT, 'leave'
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	7: Begin;T_or
		PRINT, 'T_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	8: Begin;F_or
		PRINT, 'F_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	9: Begin;T_and
		PRINT, 'T_and'
		typi=15;if we are evaluating y[0], just check  y[1] rather  than nonexistent y[-1]
		typf=14;if we are evaluating y[N-1], just check  y[N-2] rather than nonexistent y[N]
		end
	10: Begin;F_and
		PRINT, 'F_and'
		typi=6;if we are evaluating y[0], just check  y[1] rather  than nonexistent y[-1]
		typf=5;if we are evaluating y[N-1], just check  y[N-2] rather than nonexistent y[N]
		end
	11: Begin;xor
		PRINT, 'F_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold since we ddon't know what is happening on other side. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	12: Begin
		;x2=mnt

		print,"closed range"
	        ; PRINT, 'Stooge'
		end
	13: Begin
		;x2=mnt
	         PRINT, 'open range'
		end
	14: Begin;hit
		PRINT, 'unmoved'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	15: Begin;hit
		PRINT, 'unmoving'
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	16: Begin;pass
		PRINT, 'pass'
		typi=6
		typf=5;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	17: Begin;ascend
		PRINT, 'pass'
		typi=20;rise
		typf=22;rose
		end
	18: Begin;descend
		PRINT, 'pass'
		typi=19;drop
		typf=21;fell
		end
	19: Begin;drop
		PRINT, 'pass'
		
		typf=4
		end
	20: Begin;rise
		PRINT, 'pass'
		
		typf=4
		end
	21: Begin;fell
		PRINT, 'pass'
		
		typi=4
		end
	22: Begin;rose
		PRINT, 'pass'
		
		typf=4
		end
	else: Begin
		print,typ
		end
	ENDCASE
	return,[typi,typf]
end




function thrloop, y,th,dw,typ,typi,typf,extras,cleans=cleans
	if NOT KEYWORD_SET(cleans) THEN cleans=0
	
	x2=0
	x3=0
	ret=extras[0]
	adv=extras[1]
	carl=extras[2]
	mnt=extras[3]
	n=size(y,/n_el)-1
	if typ ne 23 then begin
		FOR i = cleans+ret, n-adv DO BEGIN	
	   		if ret then x2=y[i-1] 
			if adv then x3=y[i+1]
			if carl then x2=mnt
	   		dw[i]=thr(y[i], th,x2,x3,typ)
			if(dw[i] EQ 1) AND (dw[i-cleans] EQ 1) THEN BEGIN
				For j=1,cleans-1 DO BEGIN
					dw[i-j]=1
				ENDFOR 
			ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
		ENDFOR	
	endif else begin
		FOR i = cleans+ret+mnt, n-adv-mnt DO BEGIN	
	   		x2=y[i-1] 
			x3=y[i+1]
			
	   		dw[i-250]=thr(y[i],th,y[i-10],mnt,24)		
	   		;dw[i-mnt]=thr(y[i], th,x2,x3,17)
	   		dw[i+mnt]=thr(y[i], th,x2,x3,18)
			if(dw[i] EQ 1) AND (dw[i-cleans] EQ 1) THEN BEGIN
				For j=1,cleans-1 DO BEGIN
					dw[i-j]=1
				ENDFOR 
			ENDIF
		;if(dw[i] EQ 0) AND (dw[i-3] EQ 0) THEN dw[i-2]=0
		;print,dw[i]
		ENDFOR	

	endelse
	return, dw
end 

function monotize2,y,t;returns an array with 1 where monotonically increasing and starts returning 0  when that stops being true until things start increasing again. ignores random spikes


;for example, 
;   the array y=[2,0,4,3,2,3,5,0,3,2,2,3] 
; will return m=[0,0,1,0,0,1,1,0,1,0,0,1], ignoring 0s


	;count=Size(B,/n_el)
	print,'entering mono2'
	N=Size(y,/n_el)
	print,N
	isinc=1*(y[0] eq t)


	;monoB=B*0

	mono=y*0

	flips=mono

	mono[0]=isinc

	lasty=max([y[0]*isinc,1])
	lasti=-1+isinc
	llasty=lasty
	llasti=-1+isinc
	lastvi=lasti;last vefified lasti
	lastvy=lasty;last  verified lasty
	prei=lasti
	prey=lasty


		
	print,'entering mono2 for  loop'
	for i=1,N-1 do begin  ;may want to modify this so that it makes a distinction between if the previous element is zero or not

		

		if y[i] gt y[i-1] then begin

			isinc=1

			lasty=y[i]
			prey=y[i-1]
			lasti=i
			prei=i-1
			if(mono[i-1] eq 0) then begin 
				llasty=lasty
				llasti=i
				flips[i]=1
			endif else begin
				lastvy=llasty
				lastvi=llasti
			endelse
		endif else	if y[i]  lt y[i-1] then begin
			isinc=0
			if(llasti eq lasti) and y[i-1] eq t then begin
				for j=lasti,i do begin
					mono[j]=0
				endfor
				llasti=lastvi
				llasty=lastvy
			endif
		endif 
		;endelse		
		if (isinc and (y[i] ge y[i-1])) then mono[i]=1

	endfor
	

	return, mono
end  



pro threshFlag, plt,t,kindThresh=kindThesh,mnt=mnt,newName=newName,dointerpolate=dointerpolate,interplot=interplot,monoNAME=monoName
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

	typ=typeCheck(kindThesh)
	print,"typ="+string(typ)
	y=datp.y
	x=datp.x
	x2=0
	x3=0

	typs=typBackup(typ)
	typi=typs[0];what to do  at y[0]
	typf=typs[1];what to do at y[N-1]
	n=Size(y,/N_ELEMENTS)-1
	print,n
	
	if(dointerpolate ne 0) then begin
		y=interpol(y,x,datB.x)
		x=datB.x
	endif
	print,"size(y)=",size(y)
	dw=y*0
	
	;if we are interested in a case that involves changing  x

	;list of types that use y[i-1]:
	bef=[5,7,8,9,10,11,14,16,17,18,21,22,23,26,27]
	;list of types that use  y[i+1]:
	aft=[6,7,8,9,10,11,15,16,17,18,19,20,23,26,27]
	;list of types that use mnt:
	monty=[12,13,23]
	ret=total(bef eq typ) eq 1
	adv=total(aft eq typ) eq 1
	carl=total(monty eq typ) eq 1
	
	extras=[ret,adv,carl,mnt]

	if (typ ne 23) and (typ ne 25) then begin
		if(ret) then  dw[0]=thr(y[0],t,0,y[1],typi) 
		FOR i = 0+ret, n-adv DO BEGIN
			
			if ret then x2=y[i-1] 
			if adv then x3=y[i+1]
		
	   		dw[i]=thr(y[i], t,x2,x3,typ)
			;if(dw[i] EQ 1) AND (dw[i-2] EQ 1) THEN dw[i-1]=1
			;if(dw[i] EQ 0) AND (dw[i-2] EQ 0) THEN dw[i-1]=0
			;if(dw[i] ne 0) then print,"("+string(i)+","+string(dw[i])+")"
		ENDFOR
		print,dw[2]
		if(adv) then  dw[n]=thr(y[n],t,y[n-1],0,typf) 
	endif else if typ eq 23 then begin
		
		FOR i = 0+ret+mnt, n-adv-mnt DO BEGIN
			x2=y[i-1] 
			x3=y[i+1]
		
	   		dw[i-mnt]=thr(y[i], t,x2,x3,17)
	   		dw[i+mnt]=thr(y[i], t,x2,x3,18)
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


		dw=fdescend+fascend
		

		faf=dw*0

		fdf=dw*0
		fdescend[where(finite(fdescend) eq 0)] =0
		fascend[where(finite(fascend) eq 0)] =0
		for i=0, N-2 do begin
			faf[i]=0
			fdf[i]=0
			if (fascend[i] eq 1) and (fascend[i+1] eq 0) then faf[i]=1 
			if (fdescend[i] eq 1) and (fdescend[i+1] eq 0) then fdf[i]=1 
		endfor 



		GG=Where(faf ne 0, gcount,COMPLEMENT=G_c, NCOMPLEMENT=gcount_c)
		print,'gcount=',gcount
		xG=x[GG]
		fG=fascend[GG]
		print,xG-x[0]
		HH=Where(fdf ne 0, hcount,COMPLEMENT=H_c, NCOMPLEMENT=hcount_c)
		print,'hcount=',hcount
		xH=x[HH]
		fH=fdescend[HH]
		print,xH-x[0]
	
		print,HH[0]
		perdh=intarr(hcount-1)
		;perdd[0]=GG[0]
		For i=0,hcount-2 do perdh[i]=HH[i+1]-HH[i]

		print,'perdh=',perdh

		ffg=faf*0

		ffh=fdf*0
		

		for i=0, min([gcount,hcount])-1 do begin

			ig=GG[i]

			ih=HH[i]

			ffg[ig]=1

			ffh[ih]=1

		endfor

		;ffg[GG]=1
		;ffh[HH]=1



		print,"size(ffg)="+string(size(ffg,/n_el))
		store_data,'ascend_end',data={x:datp.x,y:ffg,ytitle:'flag'}
		store_data,'descend_end',data={x:datp.x,y:ffh,ytitle:'flag'}

		iaf=dw*0

		idf=dw*0

		for i=1, N-1 do begin
			if (fascend[i] eq 1) and (fascend[i-1] eq 0) then iaf[i]=1 
			if (fdescend[i] eq 1) and (fdescend[i-1] eq 0) then idf[i]=1 
		endfor 



		iGG=Where(iaf ne 0, gcount,COMPLEMENT=G_c, NCOMPLEMENT=gcount_c)
		print,'gcount=',gcount
		xiG=x[iGG]
		iG=fascend[iGG]
		print,xiG-x[0]
		iHH=Where(idf ne 0, hcount,COMPLEMENT=H_c, NCOMPLEMENT=hcount_c)
		print,'hcount=',hcount
		xiH=x[HH]
		iH=fdescend[iHH]
		print,xiH-x[0]
	
		print,iHH[0]
		perdh=intarr(hcount-1)
		;perdd[0]=GG[0]
		For i=0,hcount-2 do perdh[i]=iHH[i+1]-iHH[i]

		print,'perdh=',perdh

		ifg=iaf*0

		ifh=idf*0

		for i=0, min([gcount,hcount])-1 do begin

			iig=iGG[i]

			iih=iHH[i]

			ifg[iig]=1

			ifh[iih]=1


		endfor
		;for i=0, gcount-1 do ifg[iGG[i]]=1
		;for i=0, hcount-1 do ifh[iHH[i]]=1

		print,"size(ifg)="+string(size(ifg,/n_el))
		store_data,'ascend_begin',data={x:datp.x,y:ifg,ytitle:'flag'}
		store_data,'descend_begin',data={x:datp.x,y:ifh,ytitle:'flag'}


	endelse


	print,"making flag 1"
	newdat={x:datp.x,y:[[dw],[dw]],v:[0,1]}

	
	store_data,name,data=newdat
	options,name,'spec',1	;switch from line plots to spectrogram mode
	options,name,'no_interp',1
	print,"entering i=2 to n loop"
	;if typ ne 23 then begin
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=2)
		print,"entering i=3 to n loop"
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=3)
		print,"entering i=4 to n loop"
	
		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=4)
		print,"entering i=5 to n loop"

		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=5)
		print,"entering i=60 to n loop"

		dw= thrloop(y,t,dw,typ,typi,typf,extras,cleans=60)
		;FOR i = 2+ret, n-adv DO BEGIN	
	 	 ; 	if ret then x2=y[i-1] 
		;	if adv then x3=y[i+1]
	 	 ; 	dw[i]=thr(y[i], t,x2,x3,typ)
		;	if(dw[i] EQ 1) AND (dw[i-2] EQ 1) THEN dw[i-1]=1
		;if(dw[i] EQ 0) AND (dw[i-2] EQ 0) THEN dw[i-1]=0
		;print,dw[i]
		;ENDFOR	
		;print,"entering i=3 to n loop"
	;	FOR i = 3+ret, n-adv DO BEGIN	
	;	   	if ret then x2=y[i-1] 
	;		if adv then x3=y[i+1]
	;	   	dw[i]=thr(y[i], t,x2,x3,typ)
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
	 ; 	 	dw[i]=thr(y[i], t,x2,x3,typ)
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
	   	;	dw[i]=thr(y[i], t,x2,x3,typ)
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
	   	;	dw[i]=thr(y[i], t,x2,x3,typ)
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
end

