function thrloop, y,th,dw,typ,typi,typf,extras,cleans=cleans,truesave=truesave
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
	   		dw[i]=threshcalc(y[i], th,x2,x3,typ,truesave=truesave)
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
			
	   		dw[i-250]=threshcalc(y[i],th,y[i-10],mnt,24,truesave=truesave)		
	   		;dw[i-mnt]=threshcalc(y[i], th,x2,x3,17)
	   		dw[i+mnt]=threshcalc(y[i], th,x2,x3,18,truesave=truesave)
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
