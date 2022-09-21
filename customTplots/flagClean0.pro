pro flagClean, plt,newName=newName
	get_data,plt,data=dat
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_cleaned'
	ys=dat.y
	xs=dat.x
	N=size(dat.x,/n_el)
	lastOn=0
	on=0
	;firstFlip=0
	lastOnFlag=[-1,-1]
	print,ys[0,*]
	ons=(dat.x)*0
	if total(ys[0,*] ne 0) ne 0 then begin
		print,"startOn"
		on =1 
		ons[0]=on
		lastOn=0
		lastOnflag=ys[0,*]
	endif else begin
		 on=0
		ons[0]=0
	endelse
	print,lastOn
	print,lastOnflag
	loops=0
	miniloops=0
	microloops=0
	for i=1,N-1 do begin
		if ys[i,0] ne 0 then begin
			;firstFlip++
			;if(firstFlip eq 1) then begin
			;	print,"current flag and lastOnFlag"
			;	print,ys[i,*] 
			;	print,lastOnflag
			;	print, "i="+string(i)a
			;endif
			
			on =1 
			
			lastOn=i
			lastOnFlag=ys[i,*]
		endif else on=0
		ons[i]=on
		
		if (ons[i] eq 1) AND (ons[i-1] eq 0) then begin
				;if total(ys[i,0] eq lastOnflag[0]) eq size(dat.v,/n_el) then begin

				; checks if the flag value is the same as last time it gave a valid value


				if ys[i,0] eq lastOnflag[0] then begin
					;if so, update all inbetween invalid values to take the two surrounding identical valid values
					for j=lastOn,i do begin
						ys[j,*]=lastOnflag
						ons[j]=1
					endfor			
					;ys[lastOn:i,*]=lastOnflag
					
				endif
		endif
		;PRINT,size(ons)
		i2=i-2
		i1=i-1
		;os=ons[i2:i1]
		if ((i ge 1) and (ys[i,0] eq ys[i-2,0]) and (ys[i-1,0] eq  0) and (ys[i,0] ne 0)) then ys[i-1,*]=ys[i,*]
		if ((i ge 2) and (ys[i,0] eq ys[i-3,0]) and (ons[i-2]+ons[i-1] eq  0) and (ys[i,0] ne 0)) then ys[i-2:i-1,*]=ys[i,0]

		if ((i ge 3) and (ys[i,0] eq ys[i-4,0]) and (ons[i-3]+ons[i-1] eq  0) and (ys[i,0] ne 0)) then ys[i-3:i-1,*]=ys[i,0]
		for k=1,lastOn do begin
			plc=i-k
			microloops+=1
			p=product(ys[i-k:i-1,0]) 
			if (p ne 0) or (ys[i,0] eq 0) then break
			
			if (ys[i,0] * ys[i-k-1,0] ne 0)then begin
				if (ys[i,0] eq ys[i-k-1,0]) then begin
						ys[i-k:i,0]=ys[i,0]
				endif else begin
						bb=xs[i-k-1:i]-xs[0]
						ystart=ys[i-k-1,0]
						yend=ys[i,0]
						ybnd=[ystart,yend]
						xend=xs[i]
						xstart=xs[i-k-1]
						;xbnd=[xs[i-k-1]-xs[0],xs[i]-xs[0]]
						mm=1.0*(yend-ystart)/(xend-xstart)
						
						;y0th=interpol(ybnd,xbnd,bb)
						;miniloops+=1
						for j=0,k do begin
							xx=i-k+j
							loops+=1
						;	if ((y0th[j] eq 0) or ((y0th[j] ne 0))
							;ys[xx,*]=round(y0th[j])
							ys[xx,*]=round(mm*xx+ystart)
						endfor
				endelse
			endif
		endfor
	endfor

	numON=total(ons)

	B = WHERE(ons EQ 1, count, COMPLEMENT=B_C, NCOMPLEMENT=count_c)
	print,'size(B)'
	print,size(B)
	
	
	
	print,'size(B_C)'
	print,size(B_C)
	print,"count,count_c,B[count-1],xs[B[count-1]]"
	print,string(count)+", "+string(count_c)+", "+string(B[count-1])+", "+string(xs[B[count-1]])+", "+string(xs[B[count-1]]-xs[0])
	;print,count_c
	;YYY=B*0
	print,xs[1]-xs[0]
	XXX=B+xs[0]

	YYY=ys[B,0]
	;FOR j=0, count-1 DO Begin
		;el=B[j]
		;XXX=xs[el]
		;YYY=ys[B]
	;ENDFOR
	
	xsNormalized=xs-xs[0]
	XXXNormalized=XXX-xs[0]
	
;	ZZZ=Interpol(YYY,XXXNormalized,xsNormalized,/NAN)
	;print,ys[0,*]
	print,'max,min,mean'
	print,max(ys[*,0])
	print,min(ys[*,0])
	print,mean(ys[*,0])
	print,""
	ZZZ=Interpol(YYY,B,xsNormalized,/NAN,/QUADRATIC)
	print,max(ZZZ,/NAN)
	print,min(ZZZ,/NAN)
	print,mean(ZZZ,/NAN)
	ZZZ=FIX(ZZZ)
	print,""
	print,max(YYY)
	print,min(YYY)
	print,mean(YYY)
	;dat.y=ys
	yy=ys[*,0]

	print,'size'
	print,size(yy)
	print,size(ZZZ)
	print,yy[0:1000]
	;print,'max,min,mean,stddev,variance,'
	result = MOMENT(yy)
	PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	print,"loops="+string(loops)

	print,"miniloops="+string(miniloops)
	print,"microloops="+string(microloops)
	print,'finishedCleaning'
	
	plotName=newName+'_plot'
	plotflag={x:dat.x,y:yy,ytitle:'flag Height'}
	store_data,plotName,data=plotFlag

	pltName=plt+'_plot'
	pltflag={x:dat.x,y:dat.y,ytitle:'flag Height'}
	store_data,pltName,data=pltFlag

	flagNew2 = {x:dat.x,y:[[ZZZ],[ZZZ]],v:[0,1],spec:1}
	flagNew = {x:dat.x,y:[[yy],[yy]],v:[0,1],spec:1}
	store_data,newName,data=flagNew,limits = {panel_size:0.1, no_interp:1}

	store_data,newName+'2',data=flagNew2,limits = {panel_size:0.1, no_interp:1}
end
