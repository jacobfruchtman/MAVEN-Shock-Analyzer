function ArrMag_vecMag,ar

	mag=ar[*,0]
	dim=size(ar,/dim)
	;print,'dim='
	;print,dim
	dimdim=size(dim,/dim)
	;print,dimdim
	if(dimdim EQ 1) and (size(ar,/n_el) le 6) THEN BEGIN
		;N=2
		rnk=dim
		CASE rnk of
			1: mag=abs(ar[*])
			2: mag=sqrt(TOTAL(ar[*]^2))
			3: mag=sqrt(TOTAL(ar[*]^2))
			4: mag=sqrt(TOTAL(ar[0:2]^2))
			6: mag=TOTAL(ar[0:2])/3
			ELSE: RETURN, "this is a bad plot"/0
		ENDCASE
	ENDIF ELSE BEGIN
		N=dim[0]
		if dimdim gt 1 then rnk=dim[1] else rnk =1
	
	;N=size(ar,/dim)[0]
	;rnk=size(ar,/dim)[1]
		CASE rnk of
			1: mag=abs(ar);For i=0,N-1 do mag[i]=abs(ar[i,*])
			2: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,*]^2))
			3: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,*]^2))
			4: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,0:2]^2))
			6:For i=0,N-1 do mag[i]=TOTAL(ar[i,0:2])/3
			ELSE: RETURN, "this is a bad plot"/0
		ENDCASE
	ENDELSE
	return, mag
end
