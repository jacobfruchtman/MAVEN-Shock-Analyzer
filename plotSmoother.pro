pro plotSmoother, plt,newName=newName,width=width,smoothType=smoothType,curveType=curveType,rev=rev
	clock=TIC("plotsmoother")
	print,"====================================================="
	print,"!!!			PLOTSMOOTHER		 !!!"
	print,"====================================================="

	g0=0
	get_data,plt,data=dat
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_smoothed'
	if NOT KEYWORD_SET(width) THEN width=10
	if NOT KEYWORD_SET(smoothType) THEN smoothType=1
	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"

	typ=0

	get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
	xs=dat.x

	wherefinite=where(finite(ys) eq 1,numfin)
	firstfin=wherefinite[0]
	finalfin=wherefinite[-1]

	if keyword_set(rev) then ys=reverse(ys)
	N=Size(ys,/n_el)
	;print,N
	typ=smoothType
	;if (string(smoothType) eq string(1)) then typ=1;or ('average'.contains(string(smoothType)))) then typ=1
	;if (string(smoothType) eq string(2)) then typ=2;or ('curvefit'.contains(compress(string(smoothType)))) then typ=2
	;if (string(smoothType) eq string(3)) then typ=3;or ('full'.contains(compress(string(smoothType)))) then typ=3
	;if (string(smoothType) eq string(4)) then typ=4;or ('neg'.contains(compress(string(smoothType)))) then typ=4
	;if (string(smoothType) eq string(5)) then typ=5;or ('exact'.contains(compress(string(smoothType)))) then typ=5
	;if (string(smoothType) eq string(6)) then typ=6;or ('nexact'.contains(compress(string(smoothType)))) then typ=6
	;if (string(smoothType) eq string(7)) then typ=7;or ('exactUD'.contains(compress(string(smoothType)))) then typ=6
	print,'type='+string(typ)
	print,"starting loop"
	z=ys
	;areNANs=1*(total(finite(ys,/NAN) eq 1))
	nans=where(finite(ys,/NAN) eq 1,nancount)


	if (typ eq 1) then begin
		print,"smooth by average"

		;r=width/2
		;for i=0,N-1 do begin
		;	beg=max([i-r,0])
		;	fin=min([i+r,N-1])
		;	z[i]=mean(ys[beg:fin],/NAN)
		;endfor
		z=smooth(ys,width,/EDGE_MIRROR,/nan)
	endif

	if (typ eq -1) then begin
		print,"smooth by average"
		r=width/2
		for i=firstfin,finalfin do begin;for i=0,N-1 do begin
			beg=max(/NAN,[i-r,firstfin]);0])
			fin=min(/NAN,[i+r,finalfin]);N-1])
			z[i]=stddev(ys[beg:fin])
		endfor
	endif
	if (typ eq -2) then begin
		print,"smooth by average"
		r=width/2
		;for i=0,N-1 do begin
			;beg=max([i-r,0])
			;fin=min([i+r,N-1])
		for i=firstfin,finalfin do begin;for i=0,N-1 do begin
			beg=max(/NAN,[i-r,firstfin]);0])
			fin=min(/NAN,[i+r,finalfin]);N-1])
			z[i]=median(ys[beg:fin])
			if finite(ys[i]) ne 1 then z[i] = !VALUES.F_NAN  
		endfor
	endif

	if (typ eq 3) then begin
		TIC
		print,"smooth full"
		z=vecsmoother(ys,2,wid=width)
		print,total(z-ys)
		TOC
	endif

	if (typ eq 4) then begin
		TIC
		print,"smooth Exact"
		z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
		TOC
	endif

	NMM=where(FINITE(ys) eq 0, mcount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	foreach nn,NMM do z[nn]=!VALUES.F_NAN 
	;ys[where(finite(ys,/NAN))]=0
	;z[where(finite(z,/NAN))]=0
	print, "mcount=",mcount
	;stop

	if (typ eq 5) then begin
		
		print,"smooth Exact"

		for i=2,width do begin 
			 ;print, i
			;nwid=width-i
			zz=smooth(z,width,/EDGE_MIRROR,/nan)
			z=zz
			;for j= i/2, N-1-i/2 do begin
			;	q=mean(ys[j-i/2:j+i/2],/NAN)
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-i/2:j+i/2],", q[i=",i,",j=",j,"]=",q
			;	z[j]=mean(ys[j-i/2:j+i/2],/NAN)

			;endfor

		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif

	if (typ eq 6) then begin
		print,"smooth Full"
		zz=z

		for i=2,width do begin 
			 ;print, i
			;nwid=width-i
			zz=smooth(z,width,/EDGE_MIRROR,/nan)
			;for j= i, N-1-i do begin
			;	q=mean(ys[j-i:j+i],/NAN)
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-i/2:j+i/2],", q[i=",i,",j=",j,"]=",q
				;if(finite(q) eq 0) then continue
				;zz[j]=mean(z[j-i:j+i],/NAN)

			;endfor
			z=zz
		endfor
		zz=z
		for i=2,width do begin 
			 ;print, i
			;ni=width-i
			zz=smooth(z,width,/EDGE_MIRROR,/nan)
			;for j= ni, N-1-ni do begin
			;	q=mean(ys[j-ni:j+ni],/nan)
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-ni/2:j+ni/2],", q[i=",ni,",j=",j,"]=",q
				;if(finite(q) eq 0) then continue
			;	zz[j]=mean(z[j-ni:j+ni],/nan)

			;endfor
			z=zz
		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif

	if (typ eq 7) then begin
		print,"smooth neg Exact"
		zz=z
		for i=2,width do begin 
			 ;print, i
			;ni=width-i
			zz=smooth(z,width,/EDGE_MIRROR,/nan)
			;for j= ni/2, N-1-ni/2 do begin
				;q=mean(ys[j-ni/2:j+ni/2],/nan)
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-ni/2:j+ni/2],", q[i=",ni,",j=",j,"]=",q
				;if(finite(q) eq 0) then continue
				;zz[j]=mean(z[j-ni/2:j+ni/2])

			;endfor
			z=zz
		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif

;-------------------------------------------------------STDDEV
	if (typ eq 8) then begin
		print,"stddev Exact"

		for i=2,width do begin 
			 ;print, i
			;nwid=width-i

			for j=firstfin+ i/2, finalfin-i/2 do begin;for j= i/2, N-1-i/2 do begin
				q=stddev(ys[j-i/2:j+i/2])
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-i/2:j+i/2],", q[i=",i,",j=",j,"]=",q
				z[j]=stddev(ys[j-i/2:j+i/2])

			endfor

		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif

	if (typ eq 9) then begin
		print,"stddev Full"
		zz=z

		for i=2,width do begin 
			 ;print, i
			;nwid=width-i

			for j=firstfin+ i/2, finalfin-i/2 do begin;for j= i/2, N-1-i/2 do begin
				q=stddev(ys[j-i/2:j+i/2])
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-i/2:j+i/2],", q[i=",i,",j=",j,"]=",q
				zz[j]=stddev(z[j-i/2:j+i/2])

			endfor
			z=zz
		endfor
		zz=z
		for i=2,width do begin 
			 ;print, i
			ni=width-i

			for j= ni/2, N-1-ni/2 do begin
				q=stddev(ys[j-ni/2:j+ni/2])
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-ni/2:j+ni/2],", q[i=",ni,",j=",j,"]=",q
				zz[j]=stddev(z[j-ni/2:j+ni/2])

			endfor
			z=zz
		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif
	if (typ eq 10) then begin
		print,"stddev neg Exact"
		zz=z
		for i=2,width do begin 
			; print, i
			ni=width-i

			for j=firstfin+ i/2, finalfin-i/2 do begin;for j= ni/2, N-1-ni/2 do begin
				q=stddev(ys[j-ni/2:j+ni/2])
				;if(finite(q) eq 0) then print, "ys[j-i/2:j+i/2]=",ys[j-ni/2:j+ni/2],", q[i=",ni,",j=",j,"]=",q
				zz[j]=stddev(z[j-ni/2:j+ni/2])

			endfor
			z=zz
		endfor

		;z=vecsmoother(ys,width-5,wid=width)
		print,total(z-ys)
	endif



	if (typ eq 2) then begin
			curveFitter,plt,newName=newName,curveType=curveType
			;stop
	endif
	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	;z[where(finite(z,/NAN))]=0
	;print,'size(z)=',size(z)	
	;print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	;result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)

	;zn=z[g0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	;foreach nn,NMM do z[nn]=!VALUES.F_NAN 

	if(total(z-ys) eq 0) then print, 'broken'

	if keyword_set(rev) then begin
		z=reverse(z)
		newname+="_back"
	endif
 	dat.y=z
	;print,size(dat)
	;help,dat
	store_data,newName,data=dat
	;print,'finished'
	TOC,clock
end
