PRO polyfit, X, MM, F, pder
	;print,"size(MM) at beginning tanhfit=",size(MM)
	ax = XX[*,0]^2
	bx = XX[*,1]^2
	cx = XX[*,2]^2
	dx =  XX[*,0]*XX[*,1]
	ex = XX[*,1]*XX[*,2]
	fx = XX[*,0]*XX[*,2]
	gx = XX[*,0]
	hx = XX[*,1]
	ix = XX[*,2]
;	bx = TANH(MM[1] * X  -  MM[2])
;	sx = ( COSH( MM[1] *  X -  MM[2] ) )^(-2)


 	F = MM[0] * ax+MM[1] *bx+MM[2] * cx+MM[3] * dx +MM[4]*ex +MM[5]*fx+MM[6]*gx+MM[7]*hx+MM[8]*ix

 	;print,"size(MM) after F is called=",size(MM)

;If the procedure is called with four parameters, calculate the

;partial derivatives.
	
	IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
	    pder = [[ax],[bx],[cx],[dx],[ex],[fx],[gx],[hx],[ix]]
		;print,"size(MM) after F at end of if=",size(MM)
	ENDIF
	;print,"size(MM) after after if=",size(MM)
END
