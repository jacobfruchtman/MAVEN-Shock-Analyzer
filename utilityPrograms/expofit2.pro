pro expofit2,X,A,F,pder
	B=EXP(A[1]*(X-A[2]))
	F=A[0]*B+A[3]

	IF N_PARAMS() GE 4 THEN BEGIN
		
	    pder = [[B],[A[0]* (X - A[2])*B], [ -A[0]*A[1]*B ],[replicate(1.0, N_ELEMENTS(X))]]
	    
	ENDIF
end
