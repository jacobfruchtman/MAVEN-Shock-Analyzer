pro cosfit2,X,MM,F,PDER
	bx=cos((X-MM[2])/MM[1])
	F=MM[0]*bx+MM[3]
	IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
		sx=Sin(MM[1]*(X-MM[2]))
	
	    pder = [[bx], [-MM[0] *(X-MM[2]) * sx /MM[1]^2],[  MM[0] * sx/MM[1]] ,[replicate(1.0, N_ELEMENTS(X))]]
	ENDIF
end
