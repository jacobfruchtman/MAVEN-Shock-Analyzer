;+
; NAME:
;	fourfunc
;
; PURPOSE:
;	  Compute a Fourier series function 
; CALLING SEQUENCE:
;	   fourfunc,x,c,f,pder ;for standard (original) IDL website based definition
;
;	   fourfunc,x,c,f,pder,phi=phi ;;for the definition
;			 f=c[0]+sum[ c[m] ( cos(2pi*phi[m-1])*cos( 2pi* m* x) +sin(2pi*phi[m-1]) sin (2pi*m*x)),{m,1,N_elements[c]-1}]
;
; INPUTS:
;	x - independent variable (phase, between zero and one) 
;
;       c - fourier series coefficients
;        0 - a(0) -> constant term
;        1 - a(1) -> cos(x2pi)
;        2 - b(1) -> sin(x2pi)
;        3 - a(2) -> cos(2x2pi)
;        4 - b(2) -> sin(2x2pi)
;    and so on for as many elements as in c 
;
; Keyword Input Parameters:
;
;
;	 RAD = if set, then x is treated as a value in radians rather then a phase between 0 and 1 
;		in this case c[m]*cos( (m+2)/2* 2!pi*x) ->c[m]*cos( (m+2)/2*x)
;
;	PHI = set this variable to an array (or list) of phases (between 0 and 1) to instead
;		build the function based on the definition
;	f=c[0]+sum[ c[m] ( cos(2pi*phi[m-1])*cos( 2pi* m* x) +sin(2pi*phi[m-1]) sin (2pi*m*x)),{m,1,N_elements[c]-1}]
;	      if RAD is also set, we also treat PHI as an angle in radians, and do 
;		not multiply the phi terms by 2pi either
; 	Note:if n_elements(phi)+1 lt n_elements(c) then any extra phases will default to zero
;
;	ANS = if set to a constant or dimension 1 array of doubles will act as coefficients for
;		 only cos terms, ans[m] -> cos((m+1) *x*2pi)
;	      note that this will occur in addition to the c term(s).
;	      if set to a 2xM array (that is an array of M 2-element-arrays, size(ans,/n_dim) eq 2. 
;			reminder: size([1,2],/n_dim) eq 1, size([[1],[2]],/n_dim) eq 2, size([[1,2],[2,5],[7,3]],/n_dim) eq 2  ) 
;		then the 0th element in the inner array is the value of n, and the 1st element is the corresponding coefficient such that
;			for i=0,(size(ans,/dims))[1]-1 do f+= ans[1,i]*cos(2*!pi*ans[0,i]*x)	
;
;
;	BNS = if set to a constant or dimension 1 array of doubles will act as coefficients for
;		 only sin terms, bns[m] -> sin((m+1) *x*2pi)
;	      note that this will occur in addition to the c term(s)
;	      if set to a 2xM array (that is an array of M 2-element-arrays, i.e. size(ans,/n_dim) eq 2. 
;			reminder: size([1,2],/n_dim) eq 1, size([[1],[2]],/n_dim) eq 2, size([[1,2],[2,5],[7,3]],/n_dim) eq 2  ) 
;		then the 0th element in the inner array is the value of n, and the 1st element is the corresponding coefficient such that
;			for i=0,(size(ans,/dims))[1]-1 do f+= ans[1,i]*sin(2*!pi*ans[0,i]*x)	
;
;	PERIOD = if set, then 2pi -> 2pi/period everywhere. If not set, PERIOD=1
;
;
;	Note: if PHI is set,  then ANS and BNS will not be used
; OUTPUTS:
;	  f - Evaluated function value
;	  pder - (Optional) partial derivatives 
; MODIFICATION HISTORY:
;	Original Written, Marc W. Buie, Lowell Observatory , 94-10-10
;	Re built , Jacob Fruchtman, 22-06-24
;-
pro fourfunc,x,c,f,pder,rad=rad,ans=ans,bns=bns,phi=phi,period=period

	if not keyword_set(period) then period=1.
	J=(2*!pi)/period

	if not keyword_set(rad) then xx=x*J else begin
		xx=x
		J=1.
	endelse	
	
	

	NN=n_elements(c)
	f=c[0]+fltarr(N_elements(x))
	pder=fltarr(N_elements(x))


	if keyword_set(phi) then begin
		for i=1,NN-1 do begin
			aa=c[i]
			n=i;(i+2)/2
			if i-1 lt n_elements(phi) then ph=phi[i-1] else ph=0.
			
			f+=aa*(sin(n* xx)*sin(J *ph)+ cos(n* xx)*cos(J *ph))
			pder+=aa*n*J*(cos(n* xx)*sin(J *ph) -sin(n* xx)*cos(J *ph))

		endfor
		return
	endif


	for i=1,NN-1 do begin
		aa=c[i]
		n=(i+2)/2
		if i mod 2 eq 0 then begin
			f+=aa*sin(n* xx) 
			pder+=aa*n*J*cos((n*xx))
		endif else begin
			 f+=aa*cos(n*xx)
			 pder-=aa*n*J*sin((n*xx))
		endelse		
	endfor
	
	if keyword_set(ans) then begin
		if size(ans,/n_dim) ne 2 then begin
		for i=0, n_elements(ans)-1 do begin
			n=i+1
			an=ans[i]
			f+=an*cos(n*xx)
			pder-=an*n*J*sin(n*xx)
		endfor
		endif else begin
		for i=0, (size(ans,/dim))[1]-1 do begin
			n=ans[0,i]
			an=ans[1,i]
			f+=an*cos(n*xx)
			pder-=an*n*J*sin(n*xx)
		endfor
		endelse
	endif
	if keyword_set(bns) then begin

		if size(bns,/n_dim) ne 2 then begin

		for i=0, n_elements(bns)-1 do begin
			n=i+1
			bn=bns[i]
			f+=bn*sin(n*xx)
			pder+=bn*n*J*cos(n*xx)
		endfor
		endif else begin
		for i=0, (size(bns,/dim))[1]-1 do begin
			n=bns[0,i]
			bn=bns[1,i]
			f+=bn*sin(n*xx)
			pder+=bn*n*J*cos(n*xx)
		endfor

		endelse
		
	endif

end
