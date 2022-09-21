function atanh,x

	return, ALOG((1+x)/(1-x))/2
end

;			for k=2,nend-1 do if((yend[k-1] le yend[k]) and (yend[k-1] le yend[k-2])) then yend[k-1]=(yend[k]+yend[k-2])/2
function vecsmoother, yend, d, wid=wid
	nend=size(yend,/n_el)
	nk=0
	if NOT KEYWORD_SET(wid) THEN wid=d
	for i=d, wid do begin
		for k=i,nend-i do if (( total(yend[k-i:k-1] le yend[k]) ne 0 ) and (total(yend[k-i:k-1] le yend[k-i-1]) ne 0 ) ) then begin
			xb=findgen(size(yend[k-i-1:k],/n_el))
			xx=[0,k]
			yy=[yend[k-i-1],yend[k]]
			yend=interpol(yy,xx,xb)
		endif
	endfor
	
	return,yend
end

;if total(ys[0,*] ne 0) ne 0 then begin

pro shockfitter, plt=plt,newName=newName

	if NOT KEYWORD_SET(plt) THEN plt='mvn_B_1sec_Mag'	
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_fitted'
	
	get_data,plt,data=dat
	ys=dat.y
	xs=dat.x
	N=Size(ys,/n_el)
	print,N
	print,"size(ys)=",N
	print,"starting loop"
	z=ys

	print,"type  2: curve fit"
	if(1 eq 1) then begin
		;.run tanhfit
		get_data,'ascend_end',data=datae
		get_data,'descend_end',data=datde
		yl=datae.y
		xl=datae.x

		yd=datde.y
		xd=datde.x
		;starti=0
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
		;print,"xs[GG]-xs[0]=",xs[GG]-xs[0]
		;print,"xs[HH]-xs[0]=",xs[HH]-xs[0]
		print,"Size(GG)=",Size(GG)
		print,"Size(HH)=",Size(HH)
		print,"Size(ys)=",Size(ys)
		;counterinutitively, will want to curve fit ys[0:GG[0]] after the rest

		;need to determine if we have the whole curve or not

		;iscut=1*(gn ne hn) ; 
		bcutoff=1*(GG[0] le HH[0]) ; does the first element of  our data set occur between our first and second trigger?
		fcutoff=1*(GG[gn-1] le HH[hn-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
		print, "bcutoff: ",bcutoff
		print, "fcutoff: ",fcutoff
		
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0
		for i=1,gn-1  do begin
			j=i-bcutoff ; if cutoff at beginning, then HH[i-1] and GG[i]  will be paired together, rather than HH[i] and GG[i]
			print,"pulling back the end of shock number i=",i,", from gi=",gi
			gi=GG[i]
			gim=GG[i-1]
			hj=HH[j] ; the index of the start trigger 
			yh=ys[hj]; the y value at the start trigger  
			print,"hj=H[",j,"]=",hj
			print,"gim=GG[i-1]=G[",i-1,"]=",gim
			yp=ys[gim:gi]
			np=size(yp,/n_el)
			ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)


			;dumb idea: lets try and smooth out yend

			;for k=2,nend-1 do if((yend[k-1] le yend[k]) and (yend[k-1] le yend[k-2])) then yend[k-1]=(yend[k]+yend[k-2])/2
			

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[1 *(nbeg-1)/5: 2*(nbeg-1)/5] ; we're pretty sure the step will never be  in this region
			print,ybegtrust[0],ybegtrust[Size(ybegtrust,/n_el)-1]
			;mbeg=nbeg
			;mend=nend
			yendtrust=yend[ (nend-1)/2: 4*(nend-1)/5] ; we're pretty sure the step will always be on in this region
			print,yendtrust[0],yendtrust[Size(yendtrust,/n_el)-1]
			brefines=0
			frefines=0
			incfunctions=1
			decfunctions=1
			print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				;print,"(brefines,frefines)=",brefines,frefines
				print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
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
					print,"i=",i					
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
			print, "the number of times we refined the beginning is ",brefines,"."
			print, "the number of times we refined the end is ",frefines,"."
			xp=xs[gim:gi]
			xpa=xp-xp[0]
			yc=yp*0
			;if curve function is f(x)=a0 tanh( a1 (x-a2))+a3 then
			;top of step is at y=mx=a0+a3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(yendtrust)
			;mx=20
			print,"max=",mx
			;bottom of  step is at y=mn=a3-a0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			print,"min=",mn

			a0=(mx-mn)/2
			a3=(mx+mn)/2
			
			;a2  is the x value  approximately at the top of the curve. to zeroth order, lets use a2=xpa[hj-gim]
			;print,"a2  is the x value in the middle of the curve. to zeroth order, lets use a2=xpa[hj-gim]


			;for a function y= a tanh(bx -c)+d, this can also be written with alternative constants y=a tanh(p(x-q))+d= a tanh(px -pq)+d.
			;Then we can identify p=b and c=pq=bq. a2  is the x value in the middle of the tanh curve. For large values of q, the difference in x between the middle of the curve
			;and the x values just at the top and bottom of the 'step' is neglible. Without loss of generality, we can say that to zeroth order, q~xpa[hj-gim], the first trigger point

			;solving y=a*tanh(bx-c)+d for b and c, we get the formulae



			;b= (c- atanh((d-y)/a))/x
			;and
			;c= bx+ atanh((d-y)/a)) -> bq=bx+atanh((d-y)/a)) -> b=atanh((d-y)/a))/(q-x)
			
			;thus for
			hx=xpa[hj-gim]
			;have a1=atanh((a3-y)/a0)/(hx-x)
			
			;to be safe, lets use x==xpa[(nbeg-1)/2] and y==yp[(nbeg-1)/2]

			print,(a3-yp[1*(nbeg-1)/5]),a0
			print,(a3-yp[1*(nbeg-1)/5])/a0
			print,atanh((a3-yp[1*(nbeg-1)/5])/a0)
			print,(hx-xpa[1*(nbeg-1)/5])
			a1=atanh((a3-yp[1*(nbeg-1)/5])/a0)/(hx-xpa[1*(nbeg-1)/5])

			;and c = b hx -> a2= a1*hx

			a2=a1*hx
			
			;a2=xpa[hj-gim]
			print,"xpa[nbeg/2]=",xpa[(nbeg-1)/2]
			print,"yp[nbeg/2]=",yp[(nbeg-1)/2]
			;with all this, can calculate guess for  a1 to zeroth order by
			;a1=  atanh((y-a3)/a0)/(x-a2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;a1=atanh( ( yp[(nbeg-1)/5] -a3 )/a0)/(xpa[(nbeg-1)/5] -a2)
			
			

			;that gives garbage. lets try 1 as an  arbitrary guess
			;a1=1
			

			AA=[a0,a1,a2,a3]
			print,"to zeroth order, guess that AA=",AA
			print,"size(AA)=",size(AA)
			weigh=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly
			yfit=[atanh(2),1]
			;yfit=CURVEFIT(xpa, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
			PRINT, 'Function parameters: ', AA

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				print,"no NANs"
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
		print,"now for the range ys[0:GG[0]]"
		;"now for the range ys[0:GG[0]]"
		g0=GG[0]
		yp=ys[0:g0]
		xp=xs[0:g0]
		np=size(yp,/n_el)
		

		;h0=HH[0]

		;"start with standard case"


		if(bcutoff eq 0) then begin
			h0=HH[0]

			gim=0



			ybeg=ys[0:h0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[h0:g0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
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
			print, "the number of times we refined the beginning is ",brefines,"."
			print, "the number of times we refined the end is ",frefines,"."
			xp=xs[gim:g0]
			xpa=xp-xp[0]
			yc=yp*0
			;if curve function is f(x)=a0 tanh( a1 *(x-a2))+a3 then
			;top of step is at y=mx=a0+a3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			;mx=mean(yendtrust)
			mx=20
			print,"max=",mx
			;bottom of  step is at y=mn=a3-a0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			print,"min=",mn
			
			a0=(mx-mn)/2
			a3=(mx+mn)/2
			
			hx=xpa[h0-gim]
			;have a1=atanh((a3-y)/a0)/(hx-x)
			
			;to be safe, lets use x==xpa[(nbeg-1)/2] and y==yp[(nbeg-1)/2]

			print,(a3-yp[4*(nbeg-1)/5]),a0
			print,(a3-yp[4*(nbeg-1)/5])/a0
			print,atanh((a3-yp[4*(nbeg-1)/5])/a0)
			print,(hx-xpa[4*(nbeg-1)/5])
			a1=atanh((a3-yp[4*(nbeg-1)/5])/a0)/(hx-xpa[4*(nbeg-1)/5])

			;and c = b hx -> a2= a1*hx

			a2=a1*hx
			;a1=1
			AA=[a0,a1,a2,a3]
			print,"to zeroth order, guess that AA=",AA

			weigh=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly
			print,"size(AA)=",size(AA)
			yfit=CURVEFIT(xpa, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
			PRINT, 'Function parameters: ', AA

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if( ncount eq 0) then begin
				for k=gim,g0-1 do z[k]=yfit[k-gim]	
			endif else begin
				print,"yfit has ",ncount," NANs. calculating directly."
				tanhfit,xpa,AA,F

				for k=gim,g0-1 do z[k]=F[k-gim]
				
			endelse
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
				print,"to zeroth order, guess that AA=",AA

				weigh=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly

				yfit=CURVEFIT(xp, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)
				PRINT, 'Function parameters: ', AA

				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin
					for k=0,g0-1 do z[k]=yfit[k]	
				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xp,AA,F

					for k=0,g0-1 do z[k]=F[k]
				
				endelse
											
			endelse
		

		endelse
		

		;"finally, we need the remaining bit after GG[gn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"

		yp=ys[gn-1:*]
		for k=gn-1,N-1 do z[k]=0
		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bn-1,N-1 do z[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

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
	print,'size of NNN=',size(NNN) ;,'with NNN[0:10]=',NNN[0:10]
	print,"end loop"
	result = MOMENT(z)
	PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	print,size(dat)
	help,dat
	store_data,newName,data=dat
	print,'finished'

end
