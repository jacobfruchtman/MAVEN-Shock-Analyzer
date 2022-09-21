function numel,A

	return, size(A,/n_el)

end


function atanh,x

	return, ALOG((1+x)/(1-x))/2
end


pro plotSmoother22, plt,newName=newName,width=width,smoothType=smoothType,curveType=curveType

	g0=0
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_smoothed'
	if NOT KEYWORD_SET(width) THEN width=10
	if NOT KEYWORD_SET(smoothType) THEN smoothType=1
	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"
	typ=0
	get_data,plt,data=dat
	get_data,'ascend_end',data=datAE
	print,size(dat)
	ys=dat.y
	xs=dat.x
	N=Size(ys,/n_el)
	print,N
	if (string(smoothType) eq string(1)) then typ=1;or ('average'.contains(string(smoothType)))) then typ=1
	if (string(smoothType) eq string(2)) then typ=2;or ('curvefit'.contains(compress(string(smoothType)))) then typ=2
	if (string(smoothType) eq string(3)) then typ=3;or ('full'.contains(compress(string(smoothType)))) then typ=3
	print,'type='+string(typ)
	print,"starting loop"
	z=ys
	if (typ eq 1) then begin
		print,"smooth by average"
		r=width/2
		for i=0,N-1 do begin
			beg=max([i-r,0])
			fin=min([i+r,N-1])
			z[i]=mean(ys[beg:fin])
		endfor
	endif

	if (typ eq 3) then begin
		print,"smooth full"
		z=vecsmoother(ys,2,wid=width)
		print,total(z-ys)
	endif

	if (typ eq 2) then begin
		print,"type  2: curve fit"
		;.run tanhfit
		get_data,'ascend_end',data=datae
		get_data,'descend_end',data=datde

		get_data,"B_sec_smoothed" ,data=datBS

		yl=datae.y
		xl=datae.x

		yd=datde.y
		xd=datde.x
		;starti=0


		yBS=datBS.y

	;------------------


		yll=ys
		ydd=ys
		for i=0, N-1 do yll[i]=0
		for i=0, N-1 do ydd[i]=0
		print,total(yll)
		print,total(ydd)
		nl=size(yl,/n_el)
		nd=size(yd,/n_el)
		;"variables that would otherwise be defined in loops"
		print,N,nl,nd
		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		trust=0
		



		Gl = WHERE(yl ne 0, gn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		

		Hd = WHERE(yd ne 0, hn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)
		print,"Size(Gl)=",size(Gl)
		print,"Size(Hd)=",size(Hd)

		;print,"xl[Gl]-xl[0]=",xl[GG]-xd[0]
		;print,"xd[Hd]-xd[0]=",xd[HH]-xd[0]
		gx=xl[Gl]
		hx=xd[Hd]
		gxa=gx-xl[0]
		hxa=hx-xd[0]
		xsa=xs-xs[0]

		for i=0, N-2 do begin
				for j=0, gn-1 do begin
					if ((gxa[j] le xsa[i]+xsa[1]) and (gxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yll[i]=1
						a=(xsa[i+1]+xsa[i])/2.0
						print,"i=",i,", j=",j,", gxa[j]=",gxa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a
						
						yll[i+1]=1*(gxa[j] gt a)
						yll[i]=1*(gxa[j] le a)
					endif
				endfor
		endfor


		for i=0, N-2 do begin
				for j=0, hn-1 do begin
					if ((hxa[j] le xsa[i]+xsa[1]) and (hxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yll[i]=1
						a=(xsa[i+1]+xsa[i])/2.0
						print,"i=",i,", j=",j,", hxa[j]=",hxa[j],", xsa[i]=",xsa[i],", xsa[i+1]=",xsa[i+1],", a=",a
						print,(hxa[j] gt a),", ", (hxa[j] le a)
						ydd[i+1]=1.0*(hxa[j] gt a)
						ydd[i]=1.0*(hxa[j] le a)
						print,"ydd[i]=",ydd[i],", ydd[i+1]=",ydd[i+1]
					endif
				endfor
		endfor
		
		print,"total(yll)=",total(yll)
		print,"total(ydd)=",total(ydd)

	;	for i=0,N-1 do begin
	;		if(total(xl[Gl] eq xs[i]) eq 1) then yll[i]=1
	;		if(total(xd[Hd] eq xs[i]) eq 1) then ydd[i]=1
	;	endfor
		GG = WHERE(yll ne 0, gn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		print,yll[GG]

		HH = WHERE(ydd ne 0, hn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)


	;------------------

		;"variables that would otherwise be defined in loops"

		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		;trust=0
		g0=0
		;GG = WHERE(yl ne 0, gn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		

		;HH = WHERE(yd ne 0, hn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)
		
		;counterinutitively, will want to curve fit ys[0:GG[0]] after the rest

		;need to determine if we have the whole curve or not

		;iscut=1*(gn ne hn) ; 
		bcutoff=1*(GG[0] le HH[0]) ; does the first element of  our data set occur between our first and second trigger?
		fcutoff=1*(GG[gn-1] le HH[hn-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
		print, "bcutoff: ",bcutoff
		print, "fcutoff: ",fcutoff
		print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0
		for i=1,gn-1  do begin
			j=i-bcutoff ; if cutoff at beginning, then HH[i-1] and GG[i]  will be paired together, rather than HH[i] and GG[i]
			print,"i=",i
			gi=GG[i]; the address of where the fit will end and the next fit begins
			gim=GG[i-1]; the address where this fit begins
			hj=HH[j] ; the index of the start trigger 
			yh=ys[hj]; the y value at the start trigger  
			print,"gi=GG[i]=G[",i,"]=",gi
			print,"hj=H[",j,"]=",hj
			print,"gim=GG[i-1]=G[",i-1,"]=",gim
			yp=ys[gim:gi]
			np=size(yp,/n_el)
			ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			;mbeg=nbeg
			;mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region


			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region

			brefines=0
			frefines=0
			;incfunctions=1
			;decfunctions=1
			loopnum=0
			print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
			while  ((ybeg[0] gt mean(yendtrust)) or (yend[nend-1] lt mean(ybegtrust))) do begin
				loopnum++ 
				print,"(brefines,frefines)=",brefines,frefines
				print,"(incfunctions,decfunctions)=",incfunctions,decfunctions, ", hj-gim=",hj-gim,", gi-hj=",gi-hj
				if loopnum eq 500 then break
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					gim++
					brefines++
					if ( brefines ge 2 *mbeg/5) then begin
						incfunctions=0
						gim=GG[i-1]
						yp=ys[gim:gi]
						np=size(yp,/n_el)
						ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					gi--
					frefines++
					if ( frefines ge 1 *mbeg/5) then begin
						decfunctions=0
						gi=GG[i]
						yp=ys[gim:gi]
						np=size(yp,/n_el)
						yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				;print,"(brefines,frefines)=",brefines,frefines
			;	print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
			;	if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
			;		yp=yp[1:*] ;shrinks our range by one
 			;		ybeg=ybeg[1:*]
			;		np--
			;		nbeg--
			;		gim++
			;		brefines++
			;		if ( brefines ge 2 *mbeg/5) then begin
			;			incfunctions=0
			;			gim=GG[i-1]
			;			yp=ys[gim:gi]
			;			
			;			np=size(yp,/n_el)
			;			ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nbeg=mbeg
			;			
			;		endif
			;	endif

			;	if ((yend[nend-1] le mean(ybegtrust)+1) and (decfunctions eq 1)) then begin
			;		print,"pulling back the end of shock number i=",i,", from gi=",gi
			;		yp=yp[0:np-2]
			;		np--
			;		yend=yend[0:nend-2]
			;		nend--
			;		gi--
			;		frefines++
			;		if ( frefines ge 1 *mbeg/5) then begin
			;			decfunctions=0
			;			gi=GG[i]
			;			yp=ys[gim:gi]
			;			np=size(yp,/n_el)
			;			yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nend=mend
			;		endif
			;	endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			print, "the number of times we refined the beginning is ",brefines,"."
			print, "the number of times we refined the end is ",frefines,"."


			ypBS=yBS[gim:gi]

			BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			
			
			BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region

			meanTbeg=mean(BSbegtrust)

			meanTend=mean(BSendtrust)


			stdbeg=finite(stddev(BSbegtrust,/nan))

			stdend=finite(stddev(BSendtrust,/nan))
			meanT=mean([meanTbeg,meanTend])
			print,meanTbeg,meanTend,stdbeg,stdend

			hhj=hj
			BSh=yBS[hhj]
			;print,"hhj-gim=",hhj-gim
			;print,"yBS[hhj]=",BSh
			;print,"meanT=",meanT
			print,"gi=GG[i]=G[",i,"]=",gi
			print,"hj=H[",j,"]=",hj
			print,"gim=GG[i-1]=G[",i-1,"]=",gim
			hosc=[0,0]
			while((meanT+stdend lt BSh) or (meanT-stdbeg gt BSh)) do begin

				if(meanT+stdend lt BSh) then begin
					print,"hhj--"
					hhj--
					BSh=yBS[hhj]
					hosc[1]=1
				endif
				
				if(meanT-stdbeg gt BSh) then begin
					print,"hhj++"
					hhj++
					BSh=yBS[hhj]
					hosc[0]=1
				endif
				print,"hhj=",hhj
				print,"BSh=",BSh
				print,"hosc=",hosc
				if((hhj ge gi) or (hhj le gim)) then begin
					;hhj+=hosc[1]-hosc[0]
					hhj=hj
					break 
				endif
				if(total(hosc) eq 2) then break

				
			
		
			endwhile
			print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el)
			hj=hhj
			print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el)
			xp=xs[gim:gi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			yc=yp*0
			;if curve function is f(x)=a0 tanh( a1 (x-a2))+a3 then
			;top of step is at y=mx=a0+a3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(yendtrust)
			;mx=25
			print,"max=",mx
			;bottom of  step is at y=mn=a3-a0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			print,"min=",mn

			a0=(mx-mn)/2
			a3=(mx+mn)/2
			
			;a2  is the x value in approximately at the top of the curve. to zeroth order, lets use a2=xpa[hj-gim]
			;print,"a2  is the x value in the middle of the curve. to zeroth order, lets use a2=xpa[hj-gim]
			a2=xpa[hj-gim]
			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  a1 to zeroth order by
			;a1=  atanh((y-a3)/a0)/(x-a2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;a1=atanh( ( yp[nbeg/2] -a3 )/a0)/(xpa[nbeg/2] -a2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			a1=1
			

			AA=[a0,a1,a2,a3]
			;print,"to zeroth order, guess that AA=",AA

			weigh=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly

			yfit=CURVEFIT(xpa, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
			;PRINT, 'Function parameters: ', AA

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				;print,"no NANs"
				for k=gim,gi-1 do z[k]=yfit[k-gim]	
			endif else begin
				print,"yfit has ",ncount," NANs. calculating directly."
				tanhfit,xpa,AA,F

				for k=gim,gi-1 do z[k]=F[k-gim]
				
			endelse
 			
			;"in case bcutoff eq 1, want an estimate of the step width of the GG[0:1] step to make approximations"
			if(i eq 1) then begin
					zwidth =(gi-gim)-a3
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=z
			endif
		endfor 
		;print,"now for the range ys[0:GG[0]]"
		;"now for the range ys[0:GG[0]]"
		g0=GG[0]
		yp=ys[0:g0]
		xp=xs[0:g0]
		np=size(yp,/n_el)
		

		;h0=HH[0]

		;"start with standard case"


		if(bcutoff eq 0) then begin
			h0=HH[0]
			print,gi
			print,"gim=",gim
			gim=0
			print,"g0=",g0
			zn=z[g0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[0:h0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[h0:g0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[2 * nend/5: 4 * nend/5] ; we're pretty sure the step will always be on in this region
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
				;		gim=GG[i+1]
				;		yp=ys[gim:g0]
				;		np=size(yp,/n_el)
				;		ybeg=ys[gim:h0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
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
				;		g0=GG[0]
				;		yp=ys[gim:g0]
				;		np=size(yp,/n_el)
				;		yend=ys[h0:g0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
				;		nend=mend
				;	endif
				;endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if (incfunctions + decfunctions eq 0) then break
			;endwhile 


			incfunctions=1
			decfunctions=1
			gi=g0
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
			;	print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
			;	;we will do these seperately within the loop, but at same time so that one negligably affects the other 
			;	if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
			;		print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where h0=",h0,", and g0=",g0, and "size(yp)=",
			;		yp=yp[1:*] ;shrinks our range by one
 			;		ybeg=ybeg[1:*]
			;		np--
			;		nbeg--
			;		gim++
			;		brefines++
			;		if ( brefines ge 2 *mbeg/5) then begin
			;			incfunctions=0
			;			gim=GG[i-1]
			;			yp=ys[gim:gi]
			;			np=size(yp,/n_el)
			;			ybeg=ys[gim:h0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nbeg=mbeg
			;			
			;		endif
			;	endif

			;	if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
			;		print,"pulling back the end of shock number i=",i,", from gi=",gi
			;		yp=yp[0:np-2]
			;		np--
			;		yend=yend[0:nend-2]
			;		nend--
			;		gi--
			;		frefines++
			;		if ( frefines ge 1 *mbeg/5) then begin
			;			print,"i=",i,", gn=",gn
			;			decfunctions=0
			;			gi=GG[0]
			;			yp=ys[gim:gi]
			;			np=size(yp,/n_el)
			;			yend=ys[h0:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nend=mend
			;		endif
			;	endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			g0=gi
			print, "the number of times we refined the beginning is ",brefines,"."
			print, "the number of times we refined the end is ",frefines,"."
			xp=xs[gim:g0]
			xpa=xp-xp[0]
			yc=yp*0
			;if curve function is f(x)=a0 tanh( a1 *(x-a2))+a3 then
			;top of step is at y=mx=a0+a3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			;mx=mean(yendtrust)
			mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=a3-a0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			mn=5
			;print,"min=",mn
			
			a0=(mx-mn)/2
			a3=(mx+mn)/2
			
			;a2  is the x value in approximately at the top of the curve. to zeroth order, lets use a2=xpa[hj-gim]
			;print,"a2  is the x value in the middle of the curve. to zeroth order, lets use a2=xpa[hj-gim]
			a2=xpa[h0-gim]

			;with all this, can calculate guess for  a1 to zeroth order by
			;a1=  atanh((y-a3)/a0)/(x-a2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;a1=atanh( ( yp[nbeg/2] -a3 )/a0)/(xpa[nbeg/2] -a2)
			a1=1
			AA=[a0,a1,a2,a3]
		;	print,"to zeroth order, guess that AA=",AA

			weigh=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly

			yfit=CURVEFIT(xpa, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
		;	PRINT, 'Function parameters: ', AA

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				;print,"no NANs"
			;	print,"gim=",gim,", g0-1=",g0-1
				
				for k=gim,g0-1 do z[k]=yfit[k-gim]	
			endif else begin
				print,"yfit has ",ncount," NANs. calculating directly."
				tanhfit,xpa,AA,F
				zn=z[g0:*]

			;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

				for k=gim,g0-1 do z[k]=F[k-gim]
				
			endelse
			;print,"g0"
			zn=z[g0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		endif else begin 
			;"if the trrigger pair WAS cut off, we try to approximate results here"
			;"want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth"
			onstep=1*(g0 le zwidth-4)
			;"if onstep eq 1, can assume without loss of generality that we are indeed on the step"
			;ty=yp
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,g0-1 do z[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with a2=0 
				
				
				
				a0=(mx-mn)/2
				a3=(mx+mn)/2

				a2=0
				a1=atanh( ( yp[np/2] -a3 )/a0)/(xp[np/2] -a2)

				AA=[a0,a1,a2,a3]
			;	print,"to zeroth order, guess that AA=",AA

				weigh=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly

				yfit=CURVEFIT(xp, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
			;	PRINT, 'Function parameters: ', AA

				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin
					for k=0,g0-1 do z[k]=yfit[k]	
				endif else begin
			;		print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xp,AA,F

					for k=0,g0-1 do z[k]=F[k]
				
				endelse
											
			endelse
		

		endelse
		

		;"finally, we need the remaining bit after GG[gn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		zn=z[g0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		GN=GG[gn-1]
		yp=ys[GN:*]
		;print,GN
		for k=GN-1,N-1 do z[k]=0
		zn=z[g0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=gn-1,N-1 do z[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

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
	endif

	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	print,'size(z)=',size(z)	
	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)

	zn=z[g0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)



	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	help,dat
	store_data,newName,data=dat
	;print,'finished'
end
