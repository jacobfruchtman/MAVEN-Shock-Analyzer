pro expofit,X,A,F,pder
	F=A[0]*(X-A[1])^A[2]+A[3]

	IF N_PARAMS() GE 4 THEN BEGIN
		
	    pder = [[X^A[1]],[-A[0]* (X - A[1])^(-1 + A[2])* A[2]], [ A[0]*(X-A[1])^A[2] *ALOG(X-A[1]) ],[replicate(1.0, N_ELEMENTS(X))]]
	    
	ENDIF
end
