pro alogfit,X, MM,F,PDER
	help,X
bx=alog(MM[1]*(X-MM[2]))
	help,bx
F=MM[0]*bx+MM[3]
;help,F
IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
		
	    pder = [[bx], [MM[0]/MM[1]* replicate(1.0, N_ELEMENTS(X))],[  -MM[0]/(X-MM[2])] ,[replicate(1.0, N_ELEMENTS(X))]]
	ENDIF
end
