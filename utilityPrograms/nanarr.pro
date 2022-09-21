function nanarr,d1,d2,d3,d4,d5,d6,d7,d8
	;nar=0

	  ON_ERROR, 2   
	;print,n_params()
	;tic
	case n_params() of
		1: RETURN,FLTARR(d1,/NOZERO)*!VALUES.F_NAN
		2: RETURN,FLTARR(d1,d2,/NOZERO)*!VALUES.F_NAN
		3:RETURN,FLTARR(d1,d2,d3,/NOZERO)*!VALUES.F_NAN
		4:RETURN,FLTARR(d1,d2,d3,d4,/NOZERO)*!VALUES.F_NAN
		5:RETURN,FLTARR(d1,d2,d3,d4,d5,/NOZERO)*!VALUES.F_NAN
		6:RETURN,FLTARR(d1,d2,d3,d4,d5,d6,/NOZERO)*!VALUES.F_NAN
		7:RETURN,FLTARR(d1,d2,d3,d4,d5,d6,d7,/NOZERO)*!VALUES.F_NAN
		8:RETURN,FLTARR(d1,d2,d3,d4,d5,d6,d7,d8,/NOZERO)*!VALUES.F_NAN
		ELSE:message, 'NANARR:Incorrect number of arguments.';RETURN,FLTARR(d1,d2,d3,d4,d5,d6,d7,d8,10)*!VALUES.F_NAN
	ENDCASE
	
END
