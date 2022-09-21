function numel,A

	return, size(A,/n_el)

end


function atanh,x

	return, ALOG((1+x)/(1-x))/2
end


function alwaysOn, A

	return, 1*(total(A) eq numel(A))
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



pro curveFitter4, plt,newName=newName,curveType=curveType

	g0=0
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_smoothed'


	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"
	typ=2
	get_data,plt,data=dat
	get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y

	ys[where(finite(ys) eq 0)]=0

	xs=dat.x
	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	mids=ys*0
	shocks=ys*0
	z=ys*0

	if (typ eq 2) then begin
		print,"type  2: curve fit"
		;.run tanhfit
		get_data,'ascend_end',data=datae
		get_data,'descend_end',data=datde

		get_data,'ascend_begin',data=datab
		get_data,'descend_begin',data=datdb

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon

		mono=datMon.y
		bmono=datbMon.y

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
		



		DF = WHERE(oyae ne 0, ddn, COMPLEMENT=D_C, NCOMPLEMENT=gcount_c)
		

		AF = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)


		CF=WHERE(oyab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c)

		BF=WHERE(oyde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c)




		print,"Size(DF)=",size(DF)
		print,"Size(AF)=",size(AF)

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

		doffsetf=1*(BB[0] gt CC[0]) ; does the first element of  our data set occur between our first and second trigger?
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
		print,"a offsetf=",aoffsetf
		print,"d offsetf=",doffsetf
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


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,ddn-1  do begin
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,"i=",i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins

			bi=BB[i]; the address of where the fit will end and the next fit begins
			;bim=BB[i-1]; the address where this fit begins


			dim=DD[i-1+doffsetf]


			aj=AA[j] ; the index of the start trigger 
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

			ypBS=yBS[dim:bi]
			BSbeg=yBS[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 


			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			meanTbeg=mean(BSbegtrust)

			meanTend=mean(BSendtrust)


			stdbeg=finite(stddev(BSbegtrust,/nan))

			stdend=finite(stddev(BSendtrust,/nan))
			meanT=mean([meanTbeg,meanTend])
			print,meanTbeg,meanTend,stdbeg,stdend

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
			print,"numel(mono)=",numel(mono)
			print,"[dim,bi]=",[dim,bi]
			monp=mono[dim:bi]
			bmonp=bmono[dim:bi]
			monend=mono[aj:bi]
			print,"total(monend)-numel(monend)=",total(monend),"-",numel(monend),"=",total(monend)-numel(monend)
			print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
			aj=monoRoll(aj,bi,dim,mono)
			
			print,"starting while loop"
			while((meanT+stdend lt BSh) or (meanT-stdbeg gt BSh)) do begin

				if(meanT+stdend lt BSh) then begin
					;print,"hhj--"
					;hhj--
					aaj--
					;BSh=yBS[hhj]
					BSh=yBS[aaj]
					hosc[1]=1
				endif
				
				if(meanT-stdbeg gt BSh) then begin
					;print,"hhj++"
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
				if((aaj ge bi) or (aaj le dim)) then begin
					;hhj+=hosc[1]-hosc[0]
					aaj=aj

					break 
				endif
				if(total(hosc) eq 2) then break

				
			
		
			endwhile
			print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;hj=hhj
			;print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;xp=xs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 

			aj=aaj
			print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
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
			print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType)
			PRINT, 'Function parameters: ', MM

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				;print,"no NANs"
				;for k=gim,gi-1 do z[k]=yfit[k-gim]	
				for k=dim,bi-1 do z[k]=yfit[k-dim]	

			endif else begin
				;print,"yfit has ",ncount," NANs. calculating directly."
				tanhfit,xpa,MM,F

				;for k=gim,gi-1 do z[k]=F[k-gim]

				for k=dim,bi-1 do z[k]=F[k-dim]
				
			endelse
 			ishock=round(m2)+dim;-x00
			print,"ishock=",ishock
			shocks[ishock]=1
			;"in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations"
			if(i eq 1) then begin
					zwidth =(bi-dim)-m3
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=z
			endif
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

		yp=ys[0:b0]
		xp=xs[0:b0]
		np=size(yp,/n_el)
		

		;a0=AA[0]

		;"start with standard case"


		if(aoffsetf eq 0) then begin
			a0=AA[0]
			;print,gi
			;print,"gim=",gim
			dim=0
			;print,"g0=",b0
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[0:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
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
		;	while((ybeg[0] gt mean(yendtrust)) or (yend[nend-1] lt mean(ybegtrust))) do begin 

				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				;if ((ybeg[0] gt mean(yendtrust)) and incfunctions) then begin; this is a simple approximation, but checking if on previous step
				;	yp=yp[1:*] ;shrinks our range by one
 				;	ybeg=ybeg[1:*]
				;	np--
				;	nbeg--
				;	gim++
				;	brefines++
				;	if ( brefines ge 2 *mbeg/5) then begin
				;		incfunctions=0
				;		gim=DD[i+1]
				;		yp=ys[gim:g0]
				;		np=size(yp,/n_el)
				;		ybeg=ys[gim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		nbeg=mbeg
				;		
				;	endif
				;endif

				;if ((yend[nend-1] lt mean(ybegtrust)) and decfunctions) then begin
				;	yp=yp[0:np-2]
				;	np--
				;	yend=yend[0:nend-2]
				;	nend--
				;	g0--
				;	frefines++
				;	if ( frefines ge 1 *mbeg/5) then begin
				;		decfunctions=0
				;		g0=DD[0]
				;		yp=ys[gim:g0]
				;		np=size(yp,/n_el)
				;		yend=ys[a0:g0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		nend=mend
				;	endif
				;endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if (incfunctions + decfunctions eq 0) then break
			;endwhile 


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

			print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim
			a0=monoRoll(a0,bi,dim,mono)
			print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			;mx=mean(yendtrust)
			mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			mn=5
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

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType)
			PRINT, 'Function parameters: ', MM
			ishock=round(m2)+dim
			shocks[ishock]=1					

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				;print,"no NANs"
			;	print,"gim=",gim,", g0-1=",b0-1
				
				for k=dim,b0-1 do z[k]=yfit[k-dim]	
			endif else begin
			;	print,"yfit has ",ncount," NANs. calculating directly."
				tanhfit,xpa,MM,F
				zn=z[b0:*]

			;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

				for k=dim,b0-1 do z[k]=F[k-dim]
				
			endelse
			;print,"g0"
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

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
				for k=0,b0-1 do z[k]=mx
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

;				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType)
				PRINT, 'Function parameters: ', MM
				print,"status=",status
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin
					for k=0,b0-1 do z[k]=yfit[k]	
				endif else begin
			;		print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xp,MM,F

					for k=0,b0-1 do z[k]=F[k]
				
				endelse
				ishock=round(m2) +dim

				shocks[ishock]=1					
			endelse
		

		endelse
		

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		GN=DD[bbn-1]
		yp=ys[GN:*]
		;print,GN
		for k=GN-1,N-1 do z[k]=0
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
print,"outbound side"
;nxs=Reverse(xs)
nxs=xs
nys=REVERSE(ys)
print,"size(nys)=",size(nys)
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



;nAA=Reverse(DD)
;nBB=Reverse(CC)
;nCC=Reverse(BB)
;nDD=Reverse(AA)

;nAA=Reverse(DD)
;nBB=Reverse(CC)
;nCC=Reverse(BB)
;nDD=Reverse(AA)
nmono=reverse(bmono)

nz=z*0
nshocks=shocks*0
print,"nAA=",nAA
print,"nBB=",nBB
print,"nCC=",nCC
print,"nDD=",nDD
doffsetb=1*(nBB[0] gt nCC[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?

for i=1 ,nddn-1 do begin
	j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	aj=nAA[j]
	bi=nBB[i]
	dim=nDD[k]
	print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
endfor

;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,nddn-1  do begin
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,"i=",i
			;gi=nDD[i]; the address of where the fit will end and the next fit begins
			;gim=nDD[i-1]; the address where this fit begins

			bi=nBB[i]; the address of where the fit will end and the next fit begins
			;bim=nBB[i-1]; the address where this fit begins
			bii=bi

			dim=nDD[i-1+doffsetb]


			aj=nAA[j] ; the index of the start trigger 
			yh=nys[aj]; the y value at the start trigger  
			print,"aj=nAA[j]=nBB[",j,"]=",aj
			print,"bi=nBB[i]=nBB[",i,"]=",bi
			;print,"hj=H[",j,"]=",hj
			print,"dim=nDD[i-1+doffsetb]=nDD[",i-1+doffsetb,"]=",dim
			;yp=nys[gim:gi]
			yp=nys[dim:bi]
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


			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			brefines=0
			frefines=0
			incfunctions=1
			decfunctions=1
			print,"[dim,bi]=",[dim,bi]
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				;print,"[dim,bi]=",[dim,bi]
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
						;gim=nDD[i-1]
						dim=nDD[i-1]
						;yp=nys[gim:gi]
						yp=nys[dim:bi]
						
						np=size(yp,/n_el)
						;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						ybeg=nys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step						
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
						;gi=nDD[i]
						;yp=nys[gim:gi]
						bi=nBB[i]
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						yend=nys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			print,"[dim,bi]=",[dim,bi]

			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			ypBS=nyBS[dim:bi]
			BSbeg=nyBS[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=nyBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 


			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			meanTbeg=mean(BSbegtrust)

			meanTend=mean(BSendtrust)


			stdbeg=finite(stddev(BSbegtrust,/nan))

			stdend=finite(stddev(BSendtrust,/nan))
			meanT=mean([meanTbeg,meanTend])
			print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=nyBS[hhj]


			aaj=aj
			BSh=nyBS[aaj]

			;print,"hhj-gim=",hhj-gim
			;print,"yBS[hhj]=",BSh
			;print,"meanT=",meanT
			;print,"gi=DD[i]=G[",i,"]=",gi
			;print,"hj=H[",j,"]=",hj
			;print,"gim=DD[i-1]=G[",i-1,"]=",gim
			hosc=[0,0]

			

			while((meanT+stdend lt BSh) or (meanT-stdbeg gt BSh)) do begin

				if(meanT+stdend lt BSh) then begin
					;print,"hhj--"
					;hhj--
					aaj--
					;BSh=yBS[hhj]
					BSh=nyBS[aaj]
					hosc[1]=1
				endif
				
				if(meanT-stdbeg gt BSh) then begin
					;print,"hhj++"
				;	hhj++
				;	BSh=nyBS[hhj]
					aaj++
					BSh=nyBS[aaj]
					hosc[0]=1
				endif
				;print,"hhj=",hhj
				;print,"BSh=",BSh
				;print,"hosc=",hosc
			;	if((hhj ge gi) or (hhj le gim)) then begin
			;		;hhj+=hosc[1]-hosc[0]
			;		hhj=hj
				if((aaj ge bi) or (aaj le dim)) then begin
					;hhj+=hosc[1]-hosc[0]
					aaj=aj

					break 
				endif
				if(total(hosc) eq 2) then break

				
			
		
			endwhile
			print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el)
			;hj=hhj
			;print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el)
			;xp=nxs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 

			aj=aaj
			print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el)
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
			print,"m2=",m2
			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1
			

			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-11
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status)
			print,"status=",status
			PRINT, 'Function parameters: ', MM

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if status eq 0  then  begin
				if( ncount eq 0) then begin
				;print,"no NANs"
				;for k=gim,gi-1 do z[k]=yfit[k-gim]	
					for k=dim,bi-1 do nz[k]=yfit[k-dim]	

				endif else begin
					;print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xpa,MM,F

					;for k=gim,gi-1 do z[k]=F[k-gim]

					for k=dim,bi-1 do nz[k]=F[k-dim]
				
				endelse
				ishock=round(m2)+dim;-x00
				print,"ishock=",ishock
				nshocks[ishock]=1
			endif
 
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
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=nBB[0]
		d0=nDD[0]

		yp=nys[0:b0]
		xp=nxs[0:b0]
		np=size(yp,/n_el)
		

		;a0=nAA[0]

		;"start with standard case"


		if(aoffsetb eq 0) then begin
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
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status)
			print,"status=",status
			PRINT, 'Function parameters: ', MM
			ishock=round(m2)+dim
			nshocks[ishock]=1					

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if status eq 0 then begin
				if( ncount eq 0) and (status eq 0) then begin
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
			endif
			;print,"g0"
			nzn=nz[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

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
		
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if status eq 0 then begin
					if( ncount eq 0) then begin
					for k=0,b0-1 do nz[k]=yfit[k]	
					endif else begin
			;			print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F

						for k=0,b0-1 do nz[k]=F[k]
				
					endelse
					ishock=round(m2) +dim

					nshocks[ishock]=1				
				endif
				
			endelse
		

		endelse
		

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		GN=nDD[bbn-1]
		yp=nys[GN:*]
		;print,GN
		for k=GN-1,N-1 do nz[k]=0
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

	zb=reverse(nz)
	znb=reverse(nzn)
	shocksb=reverse(nshocks)


	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0
	print,'size(z)=',size(z)	
	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=z[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	store_data,'shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}

	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	help,dat
	store_data,newName+'-inbound',data=dat


;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----


	NNN=where(FINITE(zb) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zb[where(finite(zb,/NAN))]=0
	print,'size(z)=',size(zb)	
	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	znb=zb[b0:*]

	;print,"mean(zn)=",mean(znb),",  max(zn)=",max(znb)

	store_data,'shocks_outbound',data={x:xs,y:shocksb,ytitle:'flag'}

	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	help,dat
	store_data,newName+'-outbound',data=dat

;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------

	for i=0, N-1 do begin

		if zb[i] ne 0 then z[i]=zb[i]

	endfor

	for i=0, N-1 do begin

		if shocksb[i] ne 0 then shocks[i]=shocksb[i]

	endfor

	;z=z+zb
	;shocks+=shocksb
	
	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0
	print,'size(z)=',size(z)	
	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=z[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	store_data,'shocks',data={x:xs,y:shocks,ytitle:'flag'}

	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	help,dat
	store_data,newName,data=dat
	options,'shocks','colors','c' 
	options,'shocks_inbound','colors','c' 
	options,'shocks_outbound','colors','c' 

	;print,'finished'
end
