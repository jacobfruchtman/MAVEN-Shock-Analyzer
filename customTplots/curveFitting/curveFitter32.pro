function numel,A

	return, size(A,/n_el)

end


function atanh,x

	return, ALOG((1+x)/(1-x))/2
end


function alwaysOn, A

	return, 1*(total(A) eq numel(A))
end

function monoRoll,aj,bi,dim,mono
			a2=aj
			monend=mono[aj:bi]
			print,"mono[a2]=",mono[a2]
			if NOT alwaysOn(monend) then begin
				print,"monorolling"
;				while mono[aj] eq 1 do aj++
				while mono[a2] eq 1 do a2++
				;aj=aj
			endif 
		print,"[before,after]=",[aj,a2]
	return,a2
end

function secondRoll,aj,bi,hill,dobug
	if dobug gt 1 then print,"numel(hill)=",numel(hill),", bi=",bi
	
	;hills=hill[0:bi]
	N=numel(hill)
	ball=aj


	peakHeight=hill[ball]
	
	peakloc=aj
	lasth=0
	lastloc=aj
	highestHeight=peakHeight
	highestloc=aj
	
	onPeak=0
	peakStart=aj
	peakend=aj
	for i=aj, N-2 do begin
		if i ge bi then break
		if (hill[i-1] lt hill[i]) and (hill[i+1] le hill[i]) then begin
			onPeak=1
			peakStart=i
		endif
	
		if ((hill[i-1] le hill[i]) and (hill[i+1] lt hill[i])) and onPeak then begin
			onPeak=0
			peakEnd=i
			peakloc=mean([peakStart,peakEnd])
			peakHeight=hill[i]

			if peakHeight gt highestHeight then begin
				highestHeight=peakHeight
				highestLoc=peakloc
			endif

			if peakHeight lt highestHeight then return,highestloc ; we keep rolling up and down peaks until we reach one higher than the next. That one should be the midpoint of the jump
		endif
		

	endfor
	return, aj

end

function aoscillate, aj, bi,dim,yBS,nbeg,nend,dobug
			nend=bi-aj
			nbeg=aj-dim
			if dobug gt 1 then print,nbeg,nend
			if dobug gt 1 then print,numel(yBS),"yBS[dim:bi]=yBS[",dim,":",bi,"]"
			ypBS=yBS[dim:bi]
			BSbeg=yBS[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			if dobug gt 1 then print,"numel(BSend)=",numel(BSend)
			if dobug gt 1 then print,"[8 * (nend-1)/10: 9 * (nend-1)/10]=","[",8 * (nend-1)/10,":", 9 * (nend-1)/10,"]"
			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			meanTbeg=mean(BSbegtrust)

			meanTend=mean(BSendtrust)


			stdbeg=finite(stddev(BSbegtrust,/nan))

			stdend=finite(stddev(BSendtrust,/nan))
			meanT=mean([meanTbeg,meanTend])
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=yBS[hhj]


			aaj=aj

			BSh=yBS[aaj]

			;print,"hhj-gim=",hhj-gim
			;print,"yBS[hhj]=",BSh
			;print,"meanT=",meanT
			;print,"gi=DD[i]=G[",i,"]=",gi
			;print,"hj=H[",j,"]=",hj
			;print,"gim=DD[i-1]=G[",i-1,"]=",gim
			hosc=[0,0]
			;print,"numel(mono)=",numel(mono)
			;print,"[dim,bi]=",[dim,bi]

			;print,"total(monend)-numel(monend)=",total(monend),"-",numel(monend),"=",total(monend)-numel(monend)
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim

			
			;print,"starting while loop"
			decs=0
			incs=0
			while((meanT+stdend lt BSh) or (meanT-stdbeg gt BSh)) do begin
				
				if(meanT+stdend lt BSh) then begin
					decs++
					;print,"aaj--"
					;hhj--
					aaj--
					;BSh=yBS[hhj]
					BSh=yBS[aaj]
					hosc[1]=1
				endif
				
				if(meanT-stdbeg gt BSh) then begin
					incs++
					;print,"aaj++"
				;	hhj++
				;	BSh=yBS[hhj]
					aaj++
					BSh=yBS[aaj]
					hosc[0]=1
				endif
				;print,"hhj=",hhj
				;print,"BSh=",BSh
				;print,"hosc=",hosc
			;	if((hhj ge gi) or (hhj le gim)) then begin
			;		;hhj+=hosc[1]-hosc[0]
			;		hhj=hj
				if((aaj ge bi) or (aaj le dim)) or max([decs,incs]) ge 1000 then begin
					;hhj+=hosc[1]-hosc[0]
					aaj=aj

					break 
				endif
				if(total(hosc) eq 2) then break

				
			
		
			endwhile
		if dobug gt 1 then print,"[decs,incs]=",[decs,incs]

return, aaj
end

function coeffCalc, xx,yy, ishock,x0
	;;		"dim" is xp[0]-xs[0]
	;;		then xx0=xx[0]-x0		
	
	xx0=xx[0]-x0
	ybeg=yy[0:ishock-xx0]
	yend=yy[ishock-xx0:*]

	xbeg=xx[0:ishock-xx0]
	xend=xx[ishock-xx0:*]
	
	xpa=xx-xx[0]


	N=numel(xx)
	nend=numel(yend)
	nbeg=numel(ybeg)
	i0=xx[0]-x0
	

	mx=max(yend[nend/5:*])

	mn=min(ybeg[0:4*nbeg/5])

	m0=(mx-mn)/2
	m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
	m2=xpa[ishock-xx0]

	m1=1
	
	MM=[m0,m1,m2,m3]

	RETURN,MM

end


function findShock,m2,m1,yfit,imin, diff
	diff=1
	if diff then begin

		derv=dir(yfit,1,'mid')

		gxg=where(derv eq max(derv))
		return,gxg[0]+imin


	endif else begin
		return, round(m2/m1+imin);m2) +dim
	endelse
end

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



pro curveFitter3, plt , newName=newName, curveType=curveType , doRoll=doRoll, doSecond=doSecond,Debug=Debug,minChi=minChi,fitnum=fitnum,secondOrder=secondOrder

	;CHANGELOG:

	


	g0=0

	minRemain=100
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_smoothed'
	if NOT KEYWORD_SET(fitnum) THEN fitnum="" else fitnum=string(fitnum)
	fitnum=fitnum.trim()
	if NOT KEYWORD_SET(minChi) THEN minChi=1.5
	rollup=0
	rollsec=0
	dobug=0

	doPerturb=0
	if not KEYWORD_SET(Debug) THEN dobug=0 else dobug =Debug
	if  KEYWORD_SET(secondOrder) THEN doPerturb=1 
	if KEYWORD_SET(doRoll) THEN rollup=1
	if KEYWORD_SET(doSecond) THEN rollsec=1                
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"
	typ=2
	get_data,plt,data=dat,limits=limits
	get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
	print,"=curveFitter3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=curveFitter3="
	print,"newName=",newName,", rollup=",rollup,", rollsec=",rollsec,", dobug=",dobug
	print,"=curveFitter3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=3=curveFitter3="
	ys[where(finite(ys) eq 0)]=0

	xs=dat.x
	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	mids=ys*0
	shocks=ys*0
	z=ys*0
	insublengths=list()
	outsublengths=list()

	brokenin=list()
	brokenout=list()

	inSubsets=xs*0;+1;-1
	inends=xs*0;+1;-1
	inbegs=xs*0;+1;-1

	chiList=list()

	noutSubsets=xs*0;+1;-1
	noutends=xs*0;+1;-1
	noutbegs=xs*0;+1;-1

	debuginlen=xs*0
	debuginbeg=xs*0
	debuginend=xs*0

	debugoutlen=xs*0
	debugoutbeg=xs*0
	debugoutened=xs*0
	


	if (typ eq 2) then begin
		if dobug gt 1 then print,"type  2: curve fit"
		;.run tanhfit
		get_data,'ascend_end',data=datae
		get_data,'descend_end',data=datde

		get_data,'ascend_begin',data=datab
		get_data,'descend_begin',data=datdb

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon

		mono=datMon.y
		bmono=datbMon.y


		get_data,'B5-3d10',data=datB5d

		hill=datB5d.y
		if dobug gt 1 then print,"numel(hill)=",numel(hill)
		get_data,"B_sec_smoothed" ,data=datBS

		oyae=datae.y
		oxae=datae.x

		oydb=datdb.y
		oxdb=datdb.x
		;starti=0
		oyde=datde.y
		oxde=datde.x

		oyab=datab.y
		oxab=datab.x

		yBS=datBS.y

	;------------------
		

		ydb=ys*0
		yde=ys*0
		yab=ys*0
		yae=ys*0
		for i=0, N-1 do yae[i]=0
		for i=0, N-1 do yde[i]=0
		for i=0, N-1 do yab[i]=0
		for i=0, N-1 do ydb[i]=0
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


		nl=size(oyae,/n_el)
		nd=size(oydb,/n_el)
		;"variables that would otherwise be defined in loops"
		;print,N,nl,nd
		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		trust=0
		
		if dobug gt 1 then print,"numel(hill)=",numel(hill)


		DF = WHERE(oyae ne 0, ddn, COMPLEMENT=D_C, NCOMPLEMENT=gcount_c)
		

		AF = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)


		CF=WHERE(oyab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c)

		BF=WHERE(oyde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c)




	;	print,"Size(DF)=",size(DF)
	;	print,"Size(AF)=",size(AF)

		;print,"oxae[DF]-oxae[0]=",oxae[DD]-oxdb[0]
		;print,"oxdb[AF]-oxdb[0]=",oxdb[AA]-oxdb[0]
		dx=oxae[DF]
		ax=oxdb[AF]
		cx=oxab[CF]
		bx=oxde[BF]	

		dxa=dx-oxae[0]
		axa=ax-oxdb[0]
		cxa=cx-oxab[0]
		bxa=bx-oxde[0]

		xsa=xs-xs[0]

		for i=0, N-2 do begin
				for j=0, ddn-1 do begin
					if ((dxa[j] le xsa[i]+xsa[1]) and (dxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						a=(xsa[i+1]+xsa[i])/2.0; print,"i=",i,", j=",j,", dxa[j]=",dxa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a
						
						yae[i+1]=1*(dxa[j] gt a)
						yae[i]=1*(dxa[j] le a)
					endif
				endfor
		endfor


		for i=0, N-2 do begin
				for j=0, aan-1 do begin
					if ((axa[j] le xsa[i]+xsa[1]) and (axa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						a=(xsa[i+1]+xsa[i])/2.0;print,"i=",i,", j=",j,", axa[j]=",axa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a;print,(axa[j] gt a),", ", (axa[j] le a)
						ydb[i+1]=1.0*(axa[j] gt a)
						ydb[i]=1.0*(axa[j] le a);print,"ydb[i]=",ydb[i],", ydb[i+1]=",ydb[i+1]
					endif
				endfor
		endfor

		for i=0, N-2 do begin
				for j=0, ccn-1 do begin
					if ((cxa[j] le xsa[i]+xsa[1]) and (cxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						a=(xsa[i+1]+xsa[i])/2.0;print,"i=",i,", j=",j,", cxa[j]=",cxa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a;print,(cxa[j] gt a),", ", (axa[j] le a)
						yab[i+1]=1.0*(cxa[j] gt a)
						yab[i]=1.0*(cxa[j] le a);print,"yab[i]=",yab[i],", yab[i+1]=",yab[i+1]
					endif
				endfor
		endfor

		for i=0, N-2 do begin
				for j=0, bbn-1 do begin
					if ((bxa[j] le xsa[i]+xsa[1]) and (bxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						a=(xsa[i+1]+xsa[i])/2.0;print,"i=",i,", j=",j,", axa[j]=",cxa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a;print,(axa[j] gt a),", ", (axa[j] le a)
						yde[i+1]=1.0*(bxa[j] gt a)
						yde[i]=1.0*(bxa[j] le a);print,"yde[i]=",yde[i],", yde[i+1]=",yde[i+1]
					endif
				endfor
		endfor

		store_data,'ascend_end_interpolated',data={x:xs,y:yae,ytitle:'yae'}
		store_data,'ascend_begin_interpolated',data={x:xs,y:yab,ytitle:'yab'}
		store_data,'descend_end_interpolated',data={x:xs,y:yde,ytitle:'yde'}
		store_data,'descend_begin_interpolated',data={x:xs,y:ydb,ytitle:'ydb'}
	;	print,"yae is non-finite at:",where(finite(yae) eq 0)
	;	print,"yde is non-finite at:",where(finite(yde) eq 0)
	;	print,"yab is non-finite at:",where(finite(yab) eq 0)
	;	print,"ydb is non-finite at:",where(finite(ydb) eq 0)
	;	print,"total(yae)=",total(yae)
	;	print,"total(ydb)=",total(ydb)
	;	print,"total(yab)=",total(yab)
	;	print,"total(yde)=",total(yde)

	;	for i=0,N-1 do begin
	;		if(total(oxae[DF] eq xs[i]) eq 1) then yae[i]=1
	;		if(total(oxdb[AF] eq xs[i]) eq 1) then ydb[i]=1
			if(total(oxab[CF] eq xs[i]) eq 1) then yab[i]=1
			if(total(oxde[BF] eq xs[i]) eq 1) then yde[i]=1
	;	endfor
		DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
		if dobug gt 1 then PRINT,"[ddn,aan,bbn]=",[ddn,aan,bbn]
		if dobug gt 1 then print,"numel(hill)=",numel(hill)

		meandevAA=0
		for i=1, aan -1 do meandevAA+=(AA[i]-AA[i-1])/aan

		meandevBB=0
		for i=1, bbn -1 do meandevBB+=(BB[i]-BB[i-1])/bbn

		meandevCC=0
		for i=1, ccn -1 do meandevCC+=(CC[i]-CC[i-1])/ccn

		meandevDD=0
		for i=1, ddn -1 do meandevDD+=(DD[i]-DD[i-1])/ddn


		meandev=mean([meandevAA,meandevBB,meandevCC,meandevDD])

		


		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD
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

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,"a offsetf=",aoffsetf
;		print,"d offsetf=",doffsetf
		;print, "bcutoff: ",bcutoff
		;print, "fcutoff: ",fcutoff

		


		if dobug gt 1 then print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0

		lastiMax=0



	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME START!

	inimaxs=xs*0-1
	outimaxs=xs*0-1

	inimins=xs*0-1
	outimins=xs*0-1

	inchis=xs*0+0
	outchis=xs*0+0

	innums=xs*0-1
	outnums=xs*0-1
	
	inshocklocs=xs*0-1
	outshocklocs=xs*0-1
	








	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME END!












;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
if dobug gt 0 then begin 
print,"=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3="
print,newName,"		inbound side			",newName
print,"=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3="
endif
		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,ddn-1  do begin
			;if dobug gt 0 then print,"-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-"
			 print,">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"

			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,"i=",i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins

			bi=BB[i]; the address of where the fit will end and the next fit begins
			bim=BB[i-1]; the address where last fit ended
			dloc=i-1+doffsetf
			if AA[j] lt DD[dloc] then dloc--  
			dim=DD[dloc]
			
			if dobug gt 1 then print,"numel(hill)=",numel(hill),", bi=",bi
			aj=AA[j] ; the index of the start trigger 
			if dobug gt 1 then print,"[dim,aj,bi,aj-dim,bi-dim]=",[dim,aj,bi,aj-dim,bi-dim]
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then continue
			zeron=aj-dim
			yh=ys[aj]; the y value at the start trigger  
			;print,"gi=DD[i]=G[",i,"]=",gi
			;print,"hj=H[",j,"]=",hj
			;print,"gim=DD[i-1]=G[",i-1,"]=",gim
			;yp=ys[gim:gi]
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

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
			;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions

			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					;gim++
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) then begin
						incfunctions=0
						;gim=DD[i-1]
						dim=DD[i-1]
						;yp=ys[gim:gi]
						yp=ys[dim:bi]
						
						np=size(yp,/n_el)
						;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step						
						nbeg=mbeg
			;			
					endif
				endif

				if ((yend[nend-1] le mean(ybegtrust)+1) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					;gi--
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) then begin
						decfunctions=0
						;gi=DD[i]
						;yp=ys[gim:gi]
						bi=BB[i]
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."


			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			;monp=mono[dim:bi]
			;bmonp=bmono[dim:bi]
			;monend=mono[aj:bi]
			aj0=aj
			if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono) ; rolls predicted m2 (inflection point of atan up to 
			if dobug gt 1 then print,"numel(hill)=",numel(hill),", bi=",bi
			if (rollsec eq 1) then aj=secondRoll(aj,bi,hill,dobug) ; second order correction 


			aj1=aj
			if dobug gt 1 then print,"aj=",aj,", aj-dim=",aj-dim,", numel(yBS)=",numel(yBS)
			if dobug gt 1 then print,"[mono[aj-1],mono[aj],mono[aj+1]=",[mono[aj-1],mono[aj],mono[aj+1]]
			
			ypBS=yBS[dim:bi]
			BSbeg=yBS[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 


			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
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


			aaj=aj
			aaj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;BSh=yBS[aaj]
			if dobug gt 1 then print,"[zeron,aj0-dim,aj-dim,aaj-dim]=",[zeron,aj0-dim,aj-dim,aaj-dim]

			;print,"numel(mono)=",numel(mono)
			;print,"[dim,bi]=",[dim,bi]

			;print,"total(monend)-numel(monend)=",total(monend),"-",numel(monend),"=",total(monend)-numel(monend)
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim


			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;hj=hhj
			;print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;xp=xs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 

			aj=aaj
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
			xp=xs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 



			yc=yp*0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn

			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
			m2=xpa[aj-dim]

			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1
			

			MM=[m0,m1,m2,m3]
			MM0=MM
			if dobug gt 0 then print,"to zeroth order, guess that MM=",MM0

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-10
			CHISQ=-1

			
			imin=dim
			imax=bi

			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)

			ishock=findShock(MM[2],MM[1],yfit,imin,1-doPerturb)
			;ishock=round(MM[2]/MM[1]+dim);m2) +dim
			;derv=dir(yfit,1,'mid')

			;gxg=where(derv eq max(derv))
			;ishock=gxg[0]+imin
			fracRemain=(bi-ishock)*1.0/(bi-dim)
			print,"status=",status,", chi^2=",CHISQ
			
			PRINT, 'Function parameters: ', MM
			print,"m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]

			if doPerturb then begin
				print,""
				print,"Now for second order corrections"
				imin=max([dim,ishock-7*60])
				imax=min([bi,ishock+7*60])
	
				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				MM=coeffCalc(xpp,ypp, ishock,x00)
				weight=1.0/ypp

				print,"to second order, guess that MM=",MM

				;print,"[numel(xppa),numel(ypp),numel(weight)]=",[numel(xppa),numel(ypp),numel(weight)]
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				print,"status=",status,", chi^2=",CHISQ;,", m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]
				PRINT, 'Function parameters: ', MM
				if dobug gt 0 then print,"MM0[2]-MM2[2]=",m2-MM[2],", MM0[2]/MM0[1]-MM2[2]/MM2[1]=",m2/m1-MM[2]/MM[1],",MM1[2]/MM1[1]-MM2[2]/MM2[1]=",MM1[2]/MM1[1]-MM[2]/MM[1]
				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				;ishock=round(MM[2]/MM[1]+dim);m2) +dim
				;derv=dir(yfit,1,'mid')
	
				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
			endif
			if dobug gt 0 then begin
				
						
				print,"[max,min,remain,fracRemain,imin,imax]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim),imin,imax]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain,imin,imax]
			endif
			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			printed="unsaved"
			working=1*( ((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1)   )
			if working then begin
				lastiMax=imax
;				inSubsets[dim:bi]=1
;				inbegs[dim:aj1]=1
;				inends[aj1:bi]=1
;				insublengths.add,bi-dim
				inSubsets[imin:imax]=1
				inbegs[imin:ishock]=1
				inends[ishock:imax]=1


				inimaxs[imin:imax]=imax
				inimins[imin:imax]=imin
				inchis[imin:imax]=CHISQ
				innums[imin:imax]=i
				inshocklocs[imin:imax]=ishock

				insublengths.add,imax-imin
				printed=" saved "
				if( ncount eq 0) then begin; and (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) then begin
					;print,"no NANs"
					;for k=gim,gi-1 do z[k]=yfit[k-gim]	

					;for k=dim,bi-1 do z[k]=yfit[k-dim]	
					for k=imin,imax-1 do z[k]=yfit[k-imin]	

				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."

					;if (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) ge 1000) then MM=MM0
					tanhfit,xpa,MM,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]

					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]
				
				endelse
 				
				if dobug gt 0 then print,"ishock=",ishock
				shocks[ishock]=1
				;"in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations"
				if(i eq 1) then begin
						zwidth =(imax-imin)-MM[3]
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=z
				endif

			endif else brokenin.add,[i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			chiList.add,[newName,' forward ',printed, string(i),string(CHISQ)]
			;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
			debugi=i-(1.0-working)/4.0+1
			debuginlen[imin:imax]=debugi
			debuginlen[ishock]=-debugi
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
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=BB[0]
		d0=DD[0]
		;dim=0
		if d0 lt b0 then dim=d0 else dim=0 
		yp=ys[dim:b0]
		xp=xs[dim:b0]
		np=size(yp,/n_el)
		

		;a0=AA[0]

		;"start with standard case"


		if(aoffsetf eq 0) then begin
			if dobug gt 0 then print,">-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]->"
			i=0
			if dobug gt 1 then print,"now for the range ys[dim:BB[0]]=ys[0:",b0,"]" else print,"i=",0,""
			a0=AA[0]
			;print,gi
			;print,"gim=",gim
			
;			dim=0
			;print,"g0=",b0
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[a0:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
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
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where a0=",a0,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) then begin
						incfunctions=0
						;dim=DD[i-1]
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) then begin
						;print,"i=",i,", ddn=",ddn
						decfunctions=0
						bi=BB[0]
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						yend=ys[a0:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=xs[dim:b0]
			xpa=xp-xp[0]
			yc=yp*0

			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim
			if (rollup eq 1) then a0=monoRoll(a0,bi,dim,mono)
			if (rollsec eq 1) then a0=secondRoll(a0,bi,hill,dobug) ; second order correction 
			a0=aoscillate(a0,bi,dim,yBS,nbeg,nend,dobug)
			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(yendtrust)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			m2=xpa[a0-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1
			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM
			MM0=MM
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-19
			CHISQ=-1
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			ishock=round(MM[2]/MM[1]) +dim
			print,"status=",status,", chi^2=",CHISQ
			PRINT, 'Function parameters: ', MM
			if dobug gt 0 then print,"m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]
			imin=dim
			imax=bi
			ishock=round(MM[2]/MM[1])+imin
			;derv=dir(yfit,1,'mid')

			;gxg=where(derv eq max(derv))
			;ishock=gxg[0]+imin

			if doPerturb then begin
				imin=max([dim,ishock-7*60])
				imax=min([bi,ishock+7*60])
	
				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				MM=coeffCalc(xpp,ypp, ishock,x00)
				weight=1.0/ypp

				print,"to second order, guess that MM=",MM

				if dobug gt 0 then print,"[numel(xppa),numel(ypp),numel(weight)]=",[numel(xppa),numel(ypp),numel(weight)]
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				print,"status=",status,", chi^2=",CHISQ
				PRINT, 'Function parameters: ', MM
				print,"MM0[2]-MM2[2]=",m2-MM[2],", MM0[2]/MM0[1]-MM2[2]/MM2[1]=",m2/m1-MM[2]/MM[1],",MM1[2]/MM1[1]-MM2[2]/MM2[1]=",MM1[2]/MM1[1]-MM[2]/MM[1]
				ishock=round(MM[2]/MM[1]) +imin
				;derv=dir(yfit,1,'mid')

				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
			endif


			fracRemain=(imax-ishock)*1.0/(imax-imin)




			if dobug gt 0 then begin
				
			
				print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			endif
			printed="unsaved"
			;print,"abs(MM[2]/MM[1]-MM0[2]/MM0[1])=",abs(MM[2]/MM[1]-MM0[2]/MM0[1]),", 1*(abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000)=",1*(abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) 
			working=1*(((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (imax-ishock gt minRemain) and ( MM[0] gt 1))
			if working then begin

				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin;and (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) lt 1000) then begin
				;print,"no NANs"
			;	print,"gim=",gim,", g0-1=",b0-1
					printed=" saved "
					for k=imin,imax-1 do z[k]=yfit[k-imin]

				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."
					;if (abs(MM[2]/MM[1]-MM0[2]/MM0[1]) ge 1000) then MM=MM0
					tanhfit,xpa,MM,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]
					zn=z[imax:*]
					if ncount eq 0 then for k=dim,imax-1 do z[k]=F[k-imin]


				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


						
				endelse
				insublengths.add,imax-imin

				inimaxs[imin:imax]=imax
				inimins[imin:imax]=imin
				inchis[imin:imax]=CHISQ
				innums[imin:imax]=i
				inshocklocs[imin:imax]=ishock
				;ishock=round(MM[2]/MM[1])+imin
				shocks[ishock]=1
				;print,"g0"
				zn=z[b0:*]

				;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
				inSubsets[dim:b0-1]=1
				inbegs[dim:a0-1]=1
				inends[a0:b0-1]=1
			endif else brokenin.add,[i,imin,imax, xs[imin]-min(xs),xs[imax]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			chiList.add,[newName,'forward',printed,string(i),string(CHISQ)]
			;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
			debugi=i-(1.0-working)/4.0+1
			debuginlen[imin:imax]=debugi
			debuginlen[ishock]=-debugi
			;inSubsets[dim:b0-1]=i
			;inbegs[dim:a0-1]=i
			;inends[a0:b0-1]=i

		endif else begin 
			if dobug gt 0 then print,">-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]-]->"
			i=0
			print,"i=",0,". Start Trigger was cut off"
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
				
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM


				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				ishock=round(MM[2]/MM[1]+dim);m2) +dim
				;derv=dir(yfit,1,'mid')
				imin=dim
				imax=b0
				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
				fracRemain=(bi-ishock)*1.0/(bi-dim)
				printed="unsaved"
				if dobug gt 0 then begin
					print,"to zeroth order, guess that MM=",MM
					PRINT, 'Function parameters: ', MM
					print,"status=",status,", chi^2=",CHISQ,", m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]
	
					print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
				endif

				;print,"status=",status
				working=1*(((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1))
				if working then begin



					inimaxs[imin:imax]=imax
					inimins[imin:imax]=imin
					inchis[imin:imax]=CHISQ
					innums[imin:imax]=i
					inshocklocs[imin:imax]=ishock

					insublengths.add,bi-dim
					NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					printed=" saved "
					if( ncount eq 0) then begin
						for k=0,b0-1 do z[k]=yfit[k]	

					endif else begin
			;		print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F
					
						for k=0,b0-1 do z[k]=F[k]
					
					endelse
					;ishock=round(MM[2]/MM[1]) +dim <-no reason this should exist

					shocks[ishock]=1
					inSubsets[dim:b0]=1
					inbegs[dim:a0]=1
					inends[a0:b0]=1
				endif else brokenin.add,[0,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]	
				chiList.add,[newName,' forward ',printed, string(i),string(CHISQ)]			
				;debuginbeg[imin:ishock-1]=i-(1.0-working)/4.0
				;debuginend[ishock+1:imax]=i-(1.0-working)/4.0
				debugi=i-(1.0-working)/4.0+1
				debuginlen[imin:imax]=debugi
				debuginlen[ishock]=-debugi


			endelse
			;inSubsets[dim:b0-1]=i
			;inbegs[dim:a0-1]=i
			;inends[a0:b0-1]=i
	

		endelse
		

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;if DD[ddn-1] gt BB[bbn-1] then GN=DD[ddn-1] else GN=BB[bbn-1]
		;yp=ys[GN:*]
		;print,GN
		for k=lastiMax+1,N-1 do z[k]=0
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

print,"=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3="
print,newName,"		outbound side			",newName
print,"=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3="
;nxs=Reverse(xs)
nxs=xs
nys=REVERSE(ys)
nyae=reverse(yae)
nyab=reverse(yab)
nyde=reverse(yde)
nydb=reverse(ydb)
nyBS=REVERSE(yBS)
nAA = WHERE(nyae ne 0, naan, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		print,yae[DD]

nDD = WHERE(ydb ne 0, nddn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

nBB = WHERE(yab ne 0, nbbn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
nCC = WHERE(yde ne 0, nccn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
nmono=reverse(mono);bmono)
nhill=reverse(hill)

sAA=DD
sDD=CC
sBB=AA

if dobug eq dobug then begin
	dat.y=nys
	dat.x=nxs

	store_data,plt+"_rev",data=dat,limits=limits

	dat.y=nyae
	store_data,"ascend_end_rev",data=dat

	dat.y=nyab
	store_data,"ascend_begin_rev",data=dat
	
	dat.y=nyde
	store_data,"descend_end_rev",data=dat
	
	dat.y=nydb
	store_data,"descend_begin_rev",data=dat




	datBS.y=nyBS
	datBS.x=nxs
	store_data,'B_sec_smoothed',data=datBS
	datMon.y=bmono
	datMon.x=nxs
	store_data,'monof-B60dsane_rev',data=datMon

	datB5d.y=nhill
	datB5d.x=nxs
	store_data,'B5-3d10_rev',data=datB5d

endif
;print,"size(nys)=",size(nys)



;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]



nz=z*0
nshocks=shocks*0
dat.x=xs
lastiMax=-1
ni9shock=-1
;print,"nAA=",nAA
;print,"nBB=",nBB
;print,"nCC=",nCC
;print,"nDD=",nDD
doffsetb=1*(sBB[0] gt sDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(sBB[0] lt sAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if dobug gt 1 then print,"numel(nys)=",numel(nys)
if dobug gt 1 then print,"[nddn,naan,nbbn]=",[nddn,naan,nbbn]
fullIter=min([nddn,naan,nbbn])
print,"fullIter=",fullIter
for i=1 ,fullIter-1 do begin
	j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	aj=sAA[j];nAA[j]
	bi=sBB[i]
	dim=sDD[k]
	print,"[sDD[",k,"],sAA[",j,",sBB[",i,"]]=",[dim,aj,bi]
	;print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
	;if dobug gt 1 then print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
	
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
		for i=1,fullIter-1  do begin
			if dobug gt 0 then print,"((((((((((((((((((((((((((((((((("
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,"i=",i
			;gi=nDD[i]; the address of where the fit will end and the next fit begins
			;gim=nDD[i-1]; the address where this fit begins
			;while sBB[i] lt sAA[j] do begin
			;	print,"[nBB[i] , nAA[j]]=",[sBB[i] , sAA[j]]
			;	i++
			;endwhile
			bi=sBB[i]; the address of where the fit will end and the next fit begins
			bim=sBB[i-1]; the address where last fit ended
			bii=bi
			dloc=i-1+doffsetb
			if sAA[j] lt sDD[dloc] then dloc--  
;			dim=nDD[dloc];+doffsetb]
			dim=sDD[dloc]

			aj=sAA[j] ; the index of the start trigger 
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then continue
			if dobug gt 1 then print,"[dim,aj,bi]=",[dim,aj,bi]
			yh=ys[aj]; the y value at the start trigger  
			;print,"aj=nAA[j]=nBB[",j,"]=",aj
			;print,"bi=nBB[i]=nBB[",i,"]=",bi
			;print,"hj=H[",j,"]=",hj
			;print,"dim=nDD[i-1]=nDD[",i-1,"]=",dim
			;yp=nys[gim:gi]
			if dobug gt 1 then print,"[dim,bi,bi-dim]",[dim,bi,bi-dim]
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=nys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=nys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			;mbeg=nbeg
			;mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region

			rang=[2.0/5,1.0/2,8.0/10,9.0/10]

			

			ybegtrust=ybeg[(nbeg-1)*(1-rang[3]): (nbeg-1)*(1-rang[2])] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[(nend-1)*(1-rang[1]): (nend-1)*(1-rang[0])] ; we're pretty sure the step will always be on in this region

			brefines=0
			frefines=0
			incfunctions=1
			decfunctions=1
			;print,"[dim,bi]=",[dim,bi]
			while  (((ybeg[0] lt mean(yendtrust)) and incfunctions) or ((yend[nend-1] gt mean(ybegtrust)) and decfunctions  )) do begin 
				;print,"[dim,bi]=",[dim,bi]
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] lt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					;gim++
					dim++
					brefines++
					if ( brefines ge 1 *mbeg/5) then begin
						incfunctions=0
						;gim=nDD[i-1]
						dim=sDD[i-1]
						;yp=nys[gim:gi]
						yp=ys[dim:bi]
						
						np=size(yp,/n_el)
						;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step						
						nbeg=mbeg
			;			
					endif
				endif

				if ((yend[nend-1] ge mean(ybegtrust)+1) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					;gi--
					bi--
					frefines++
					if ( frefines ge 2 *mbeg/5) then begin
						decfunctions=0
						;gi=nDD[i]
						;yp=nys[gim:gi]
						bi=sBB[i]
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			;print,"[dim,bi]=",[dim,bi]

			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=nyBS[hhj]



			if (rollup eq 1) then aj=monoRoll(aj,bi,dim,nmono)
			if (rollsec eq 1) then aj=secondRoll(aj,bi,nhill,dobug) ; second order correction 
			aaj=aj
			aaj=aoscillate(aj,bi,dim,nyBS,nbeg,nend,dobug)


			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el)
			;hj=hhj
			;print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el)
			;xp=nxs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 

			aj=aaj
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el)
			xp=nxs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-min(xp); the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			


			yc=yp*0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn

			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			;m2=xpa[hj-gim]
			m2=xpa[aj-dim]
			;print,"m2=",m2
			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1
			;m1=1

			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-11
			CHISQ=-1
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)

			fracRemain=(bi-ishock)*1.0/(bi-dim)
			print,"status=",status,", chi^2=",CHISQ
			PRINT, 'Function parameters: ', MM
			CHI0=CHISQ
			imin=dim
			imax=bi
			ishock=round(MM[2]/MM[1]+dim);m2) +dim
			yfit0=yfit

;			derv=dir(yfit,1,'mid')

;			gxg=where(derv eq max(derv))
;			ishock=gxg[0]+imin

			if doPerturb then begin
				imin=max([dim,ishock-7*60])
				imax=min([bi,ishock+7*60])
	
				ypp=nys[imin:imax]
				xpp=nxs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				MM=coeffCalc(xpp,ypp, ishock,x00)
				weight=1.0/ypp

				print,"to second order, guess that MM=",MM

				if dobug gt 0 then print,"[numel(xppa),numel(ypp),numel(weight)]=",[numel(xppa),numel(ypp),numel(weight)]
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				print,"status=",status,", chi^2=",CHISQ,", m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]
				PRINT, 'Function parameters: ', MM
				ishock=round(MM[2]/MM[1])+imin;m2) +dim
				;derv=dir(yfit,1,'mid')

				;gxg=where(derv eq max(derv))
				;ishock=gxg[0]+imin
			endif
		
			if dobug gt 0 then begin
				print,"status=",status,", chi^2=",CHISQ,", m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]
				PRINT, 'Function parameters: ', MM
				print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			endif
			printed="unsaved"
			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if dobug gt 0 then print,"[MM[1],CHISQ,imax-ishock,minRemain,MM[0],working]="
			working=1*(   ((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (imax-ishock gt minRemain) and ( MM[0] gt 1)   )
			if dobug gt 0 then print,"[MM[1],CHISQ,imax-ishock,minRemain,MM[0],working]=",[MM[1],CHISQ,imax-ishock,minRemain,MM[0],working]
			if working eq 1 then begin
				lastiMax=imax

				
				outimaxs[imin:imax]=imax
				outimins[imin:imax]=imin
				outchis[imin:imax]=CHISQ
				outnums[imin:imax]=i
				outshocklocs[imin:imax]=ishock
				printed=" saved "
				;outsublengths.add,bi-dim
				outsublengths.add,imax-imin
				nz0=nz*0
				if( ncount eq 0) then begin
				print,"no NANs, i=",i
				;for k=gim,gi-1 do z[k]=yfit[k-gim]	
					;for k=dim,bi-1 do nz[k]=yfit[k-dim]	
					;for k=dim,bi-1 do nz0[k]=yfit[k-dim]
					;nz[imin:imax]=yfit
					;nz0[imin:imax]=yfit	
					for k=imin,imax do nz[k]=yfit[k-imin]	
					for k=imin,imax do nz0[k]=yfit[k-imin]	
					if dobug gt 0 then if i eq 9 then print,"nz[ishock-3:ishock+3]=",nz[ishock-3:ishock+3]
					if dobug gt 0 then if i eq 9 then print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]
					if dobug gt 1 then print,"dim=",dim
				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."
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
				ishock=round(MM[2]/MM[1])+imin;-x00
				if dobug gt 1 then print,"ishock=",ishock
				nshocks[ishock]=1


				bz0=reverse(nz)

				datzI=datBS
				datzI.y=bz0
				if dobug gt 1 then begin
					print,"[dim,bi,bi-dim,imin,imax,imax-imin]=",[dim,bi,bi-dim,imin,imax,imax-imin]
					str=["outbound",string(i)]
					str[1]=str[1].trim()
					str=str.join('_')
					store_data,str,data=datzI
				endif
				;noutSubsets[dim:bi]=1
				;noutbegs[dim:aj]=1
				;noutends[aj:bi]=1

				noutSubsets[imin:imax]=1
				noutbegs[imin:imax]=1
				noutends[imin:imax]=1

			endif else brokenout.add,[i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
 			chiList.add,[newName,' backword ',printed,string(i),string(CHISQ)]

			;debugoutbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debugoutend[ishock+1:imax]=i-(1.0-working)/4.0
			debugi=i-(1.0-working)/4.0+1
			debugoutlen[imin:imax]=debugi
			debugoutlen[ishock]=-debugi
		
			;"in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations"
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
	;print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		i=0

		b0=nBB[0]
		d0=nDD[0]

		yp=nys[0:b0]
		xp=nxs[0:b0]
		np=size(yp,/n_el)
		

		;a0=nAA[0]

		;"start with standard case"


		if(aoffsetb eq 0) then begin

			if dobug gt 0 then print,"(((((((((((((((((((((((((("
			if dobug gt 1 then print,"now for the range nys[0:nBB[0]]=nys[0:",nBB[0],"]" else print,"i=",0
			;if dobug gt 0 then print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]
			a0=nAA[0]
			;print,gi
			;print,"gim=",gim
			dim=0
			;print,"g0=",b0
			nzn=nz[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=nys[0:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=nys[a0:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
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
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where a0=",a0,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) then begin
						incfunctions=0
						;dim=DD[i-1]
						dim=0
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						ybeg=nys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) then begin
						;print,"i=",i,", ddn=",ddn
						decfunctions=0
						bi=nBB[0]
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						yend=nys[a0:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=nxs[dim:b0]
			xpa=xp-min(xp)
			yc=yp*0
			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			if (rollup eq 1) then a0=monoRoll(a0,bi,dim,nmono)
			if (rollsec eq 1) then a0=secondRoll(a0,bi,nhill,dobug) ; second order correction 
			a0=aoscillate(a0,bi,dim,nyBS,nbeg,nend,dobug)





			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			m2=xpa[a0-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1
			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

			status=-11
			CHISQ=-1
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status, CHISQ=CHISQ)
			ishock=round(MM[2]/MM[1]+dim);m2) +dim
			fracRemain=(bi-ishock)*1.0/(bi-dim)
			PRINT, 'Function parameters: ', MM
			print,"status=",status,", chi^2=",CHISQ,", m2-MM[2]=",m2-MM[2],", m2/m1-MM[2]/MM[1]=",m2/m1-MM[2]/MM[1]

			if dobug gt 0 then begin
				

				print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			endif
			;ishock=round(MM[2]/MM[1]+dim)

			printed="unsaved"
			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			working =1*( ((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1)   )

			if working then begin
				outimaxs[imin:imax]=imax
				outimins[imin:imax]=imin
				outchis[imin:imax]=CHISQ
				outnums[imin:imax]=i
				outshocklocs[imin:imax]=ishock
				outsublengths.add,bi-dim
				printed=" saved "
				nshocks[ishock]=1					
				if( ncount eq 0)  then begin
					;print,"no NANs"
				;	print,"gim=",gim,", g0-1=",b0-1
					
					for k=dim,b0-1 do nz[k]=yfit[k-dim]	
				endif else begin
			;	print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xpa,MM,F
					nzn=nz[b0:*]

				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

					for k=dim,b0-1 do nz[k]=F[k-dim]
			
				endelse
			endif else brokenout.add,[0,dim,b0, nxs[dim]-min(nxs),nxs[b0]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			;print,"g0"
			nzn=nz[b0:*]
			chiList.add,[newName,'backword',printed, string(i),string(CHISQ)]
			debugi=i-(1.0-working)/4.0
			;debugoutbeg[imin:ishock-1]=i-(1.0-working)/4.0
			;debugoutend[ishock+1:imax]=i-(1.0-working)/4.0
			debugoutlen[imin:imax]=debugi
			debugoutlen[ishock]=-debugi
			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			;print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]

		endif else begin 
			;"if the trrigger pair WAS cut off, we try to approximate results here"
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
				
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				print,"to zeroth order, guess that MM=",MM

				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status)
				PRINT, 'Function parameters: ', MM
				printed="unsaved"
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				working =1*(((status eq 0) and (MM[1] gt 0)) and (CHISQ lt minChi) and ( MM[0] gt 1))
				if working then begin
					outimaxs[imin:imax]=imax
					outimins[imin:imax]=imin
					outchis[imin:imax]=CHISQ
					outnums[imin:imax]=i
					outshocklocs[imin:imax]=ishock
					if( ncount eq 0) then begin
						for k=0,b0-1 do nz[k]=yfit[k]
						printed=" saved "	
						chiList.add,[newName,'backword',string(i),string(CHISQ)]
					endif else begin
			;			print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F

						for k=0,b0-1 do nz[k]=F[k]
				
					endelse
					ishock=round(MM[2]/MM[1]) +dim

					nshocks[ishock]=1				
				endif
				chiList.add,[newName,'backword',printed,string(i),string(CHISQ)]
			endelse
		

		endelse
		;print,"now the final bit"
		;print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;GN=nDD[bbn-1]
		;yp=nys[GN:*]
		;print,GN
		for k=lastiMax+1,N-1 do nz[k]=0
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

	endif
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------

	;print,"nz[ni9shock-3:ni9shock+3]=",nz[ni9shock-3:ni9shock+3]
	if dobug gt 0 then print,"(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)-(O)"
	zb=reverse(nz)
	znb=reverse(nzn)
	shocksb=reverse(nshocks)
	;print,"zb[N-ni9shock-3:N-ni9shock+3]=",zb[N-ni9shock-3:N-ni9shock+3]
	debugoutlen=reverse(debugoutlen)
	;debugoutbeg=reverse(debugoutbeg)
	;debugoutened=reverse(debugoutend)

	
	outSubsets=reverse(noutsubsets)
	outends=reverse(noutends)
	outbegs=reverse(noutbegs)

	outimaxs=reverse(outimaxs)
	outimins=reverse(outimins)
	outchis=reverse(outchis)
	outnums=reverse(outnums)
	outshocklocs=reverse(outshocklocs)

	adjChiIn=inchis*0
	adjChiOut=xs*0
	for i=0,N-1 do if inchis[i] ne 0 then adjChiIn[i]= abs(inchis[i]-1)
	for i=0,N-1 do if outchis[i] ne 0 then adjChiOut[i]= abs(outchis[i]-1)
	chis=outchis
	for i=0 , N-1 do if inchis[i] ne 0 then chis[i] = inchis[i]
	adjChi=xs*0
	for i=0,N-1 do if chis[i] ne 0 then adjChi[i]= abs(chis[i]-1)
	store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
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

	for i=0, N-1 do begin
		if inSubsets[i] eq -1 then inSubsets[i]=0
		if inends[i] eq -1 then inends[i]=0
		if inbegs[i] eq -1 then inbegs[i]=0
	endfor

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
	store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'inbound Shock'}
	store_data,'shocks'+fitnum+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	help,inSubsets
	store_data,'sublengths'+fitnum+rl+'_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

	store_data,'sublengths'+fitnum+rl+'_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
	store_data,'sublengths'+fitnum+rl+'_inbound_end',data={x:xs,y:inends,ytitle:'flag'}

	;store_data,newName+'_debug_inbegs',data={x:xs,y:debuginbegs,ytitle:'shock number'}
	;store_data,newName+'_debug_inends',data={x:xs,y:debuginends,ytitle:'shock number'}
	store_data,newName+'_debug_ins',data={x:xs,y:debuginlen,ytitle:'shock number'}
	if(total(z-ys) eq 0) then print, 'broken'
 	;dat.y=z
	;print,size(dat)
	;help,dat
	store_data,newName+'_inbound',data={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs}

;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
	store_data,'shocks'+fitnum+rl+'_outbound',data={x:xs,y:shocks,ytitle:'flag'}
	store_data,newName+'_shocks_outbound',data={x:xs,y:shocks,ytitle:'outbound_shock'}
	help,inSubsets
	store_data,'sublengths'+fitnum+rl+'_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}

	store_data,'sublengths'+fitnum+rl+'_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
	store_data,'sublengths'+fitnum+rl+'_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

	NNN=where(FINITE(zb) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zb[where(finite(zb,/NAN))]=0
;	print,'size(z)=',size(zb)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	znb=zb[b0:*]

	;print,"mean(zn)=",mean(znb),",  max(zn)=",max(znb)

	store_data,'shocks'+fitnum+rl+'_outbound',data={x:xs,y:shocksb,ytitle:'flag'}

	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	;help,dat
	store_data,newName+'_outbound',data={x:xs,y:zb,ytitle:"Magnetic Field (nT)", outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs}

	;store_data,newName+'_debug_outbegs',data={x:xs,y:debugoutbegs,ytitle:'shock number'}
	;store_data,newName+'_debug_outends',data={x:xs,y:debugoutends,ytitle:'shock number'}
	store_data,newName+'_debug_outs',data={x:xs,y:debugoutlen,ytitle:'shock number'}

;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------

	for i=0, N-1 do begin

		if z[i] ne 0 then zb[i]=z[i]

	endfor
	zf=zb
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
	store_data,newName+'_shocks',data={x:xs,y:shocks,ytitle:'shock'}
	store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}

	if(total(zf-ys) eq 0) then print, 'broken'
 	dat.y=zf
	;print,size(dat)
	;help,dat
	store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}

	
	options,'shocks'+fitnum+rl,'colors',cl
	options,'shocks'+fitnum+rl+'_inbound','colors',cl 
	options,'shocks'+fitnum+rl+'_outbound','colors',cl 
	if dobug gt 0 then begin
		insublengths=insublengths.toarray()
		outsublengths=reverse(outsublengths.toarray())
		print,"insublengths=",insublengths
		PRINT, 'Mean: ', mean(insublengths), ', Variance: ', stddev(insublengths)
		print,"outsublengths=",outsublengths
		PRINT, 'Mean: ', mean(outsublengths), ', Variance: ', stddev(outsublengths)


		brokenin=brokenin.toarray()	
		brokenout=reverse(brokenout.toarray())
	
		print,"brokenin"
		print,"i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
		print,brokenin

		print,"brokenout"
		print,"i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
		print,brokenout

	endif else begin

		print, "newName, direction, i, CHISQ"
		print, chiList
	endelse
		print,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
		print,'		finished creating ',newName
		print,"============================================"
end
