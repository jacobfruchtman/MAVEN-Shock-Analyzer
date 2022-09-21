function derivatorStart, y,step,x=x
	n=Size(y,/N_ELEMENTS)-1
	z=y
	;y[0]=0
	y[n]=0
	if keyword_set(x) then begin
		FOR i=1,step-1 DO BEGIN
			z[n-step+i]=(y[n]-y[n-step+i])/(x[n]-x[n-step+i]);(step-i)
			;z[n-1-i]=(y[n-i]-y[n-i-2])/2
		ENDFOR
	
		FOR i=step,n-step DO BEGIN
			z[i]=(y[i+step]-y[i])/(x[i+step]-x[i]);(step)
			;y[n-i]=(y[n-i+1]-y[n-i-1])/2
		ENDFOR
	endif else begin
		FOR i=1,step-1 DO BEGIN
			z[n-step+i]=(y[n]-y[n-step+i])/(step-i)
			;z[n-1-i]=(y[n-i]-y[n-i-2])/2
		ENDFOR
	
		FOR i=step,n-step DO BEGIN
			z[i]=(y[i+step]-y[i])/(step)
			;y[n-i]=(y[n-i+1]-y[n-i-1])/2
		ENDFOR

	endelse
	return, z
END

