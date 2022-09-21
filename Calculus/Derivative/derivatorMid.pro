function derivatorMid, y,step,x=x
	n=Size(y,/N_ELEMENTS)-1
	z=y
	z[0]=0.0
	z[n]=0.0
	if keyword_set(x) then begin

	FOR i=0,step-1 DO BEGIN

		if (FINITE(y[i-1] ) ne 1) or FINITE(y[i+1] ) ne 1 then z[i]=!VALUES.F_NAN 
		if (FINITE(y[n-i] ) ne 1) or FINITE(y[n-i-2] ) ne 1 then z[n-i]=!VALUES.F_NAN 
		z[i]=(y[i+1]-y[i-1])/(x[i+1]-x[i-1]);/1.5
		z[n-1-i]=(y[n-i]-y[n-i-2])/(x[n-i]-x[n-i-2]);/1.5
	ENDFOR
	
	FOR i=step,n-step DO BEGIN
		if (FINITE(y[i+step] ) ne 1) or FINITE(y[i-step] ) ne 1 then z[i]=!VALUES.F_NAN 
		z[i]=(y[i+step]-y[i-step])/(x[i+step]-x[i-step]);/(1.5*step)
		;y[n-i]=(y[n-i+1]-y[n-i-1])/2
	ENDFOR

	endif else begin
		FOR i=0,step-1 DO BEGIN

			if (FINITE(y[i-1] ) ne 1) or FINITE(y[i+1] ) ne 1 then z[i]=!VALUES.F_NAN 
			if (FINITE(y[n-i] ) ne 1) or FINITE(y[n-i-2] ) ne 1 then z[n-i]=!VALUES.F_NAN 
			z[i]=(y[i+1]-y[i-1])/1.5
			z[n-1-i]=(y[n-i]-y[n-i-2])/1.5
		ENDFOR
	
		FOR i=step,n-step DO BEGIN
			if (FINITE(y[i+step] ) ne 1) or FINITE(y[i-step] ) ne 1 then z[i]=!VALUES.F_NAN 
			z[i]=(y[i+step]-y[i-step])/(1.5*step)
			;y[n-i]=(y[n-i+1]-y[n-i-1])/2
		ENDFOR

	endelse

	return, z
END
