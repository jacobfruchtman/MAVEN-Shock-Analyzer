function lm_tanhfit,X,MM
	bx = TANH(MM[1] * (X  -  MM[2]))
	sx = ( COSH( MM[1] *(  X -  MM[2] )) )^(-2)
	;bx = TANH(MM[1] *( X  -  MM[2]))
	;sx = MM[0]*( COSH( MM[1] *  (X -  MM[2]) ) )^(-2)
 	F = MM[0] * bx + MM[3]

	return,[[F],[bx], [MM[0] *(X-MM[2]) * sx ],[ - MM[0] *MM[1]* sx] ,[replicate(1.0, N_ELEMENTS(X))]]
