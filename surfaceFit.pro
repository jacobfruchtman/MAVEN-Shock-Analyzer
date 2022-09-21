function surfaceFit, plt=plt,fitType=fitType

	if NOT KEYWORD_SET(plt) THEN plt='POS_interpolated_(MARS_MSO)'

	if NOT KEYWORD_SET(fitType) THEN fitType=0 ; 0<--regress for 3 equations, 0=2Ax+Dy+Fz+G
	get_data,plt,data=dat
	get_data,'shocks',data=datS
	allpos=dat.y
	sy=datS.y
	slocs=where(sy ne 0, scount)
	pos=allpos[slocs,*]
	
	posx=pos[*,0]
	posy=pos[*,1]
	posz=pos[*,2]

	; will be using 
	; A*x^2 + B*y^2 +C*z^2 +D*x*y+E*y*z+F*x*z+G*x+H*y+I*z=0
	

	;need to turn this into case statement
	if fitType eq 0 then begin
	
		J=fltarr(scount)+1
		
	
	endif

	return, coeff
end
