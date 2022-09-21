function calcconeangle,Bx,By,Bz

	if N_PARAMS() eq 1 and numel(Bx) eq 3 then return, acos(Bx[0]/ SQRT( TOTAL(BX^2) ) ) 


	IF N_PARAMS() eq 1 and ((size(Bx,/dim))[-1] eq 3 or (size(Bx,/dim))[0] eq 3) then begin

		B=Bx
		print,(size(B,/n_dim))
		print,(size(B,/dim))
		if (size(B,/n_dim)) eq 1  then B=transpose(B)
		Bxx=B[*,0]
		Byy=B[*,1]
		Bzz=B[*,2]
		den=sqrt(Bxx^2+Byy^2+Bzz^2)
		cone=acos(Bxx/den)
		if (size(cone,/n_dim)) eq 2 then cone=transpose(cone)
		return,cone
	endif
	den=sqrt(Bx^2+By^2+Bz^2)
	cone=acos(Bx/den)
	if (size(cone,/n_dim)) eq 2 then cone=transpose(cone)
	return,cone
end
