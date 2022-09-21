PRO tanhfit2, X, MM, F, pder
	;print,"size(MM) at beginning tanhfit=",size(MM)
	bx = TANH(MM[1] * (X  -  MM[2]))
	sx = ( COSH( MM[1] *(  X -  MM[2] )) )^(-2)
	;bx = TANH(MM[1] *( X  -  MM[2]))
	;sx = MM[0]*( COSH( MM[1] *  (X -  MM[2]) ) )^(-2)
 	F = MM[0] * bx + MM[3]

 	;print,"size(MM) after F is called=",size(MM)

;If the procedure is called with four parameters, calculate the

;partial derivatives.
	
	IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
	    pder = [[bx], [MM[0] *(X-MM[2]) * sx ],[ - MM[0] *MM[1]* sx] ,[replicate(1.0, N_ELEMENTS(X))]]
	    ;pder = [[bx], [(X-MM[2]) * sx ],[ - MM[1] * sx] ,[replicate(1.0, N_ELEMENTS(X))]]
		;print,"size(MM) after F at end of if=",size(MM)
	ENDIF
	;print,"size(MM) after after if=",size(MM)
END
