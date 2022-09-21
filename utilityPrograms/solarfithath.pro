pro solarfithath,X,MM,F,pder

	;;; a (t-t0)^3 / ( exp( (t-t0)^2 /b^2 ) -c )
 	;c=.71
	dt=X-MM[0]
	c=.71
	if numel(MM) eq 2 then begin

		b0=27.12
		b1=25.15 /10.^(3./4)
		b=b0+b1/(MM[1])^(1/4.)
		ex=Exp(dt^2 /b^2)
		den=ex-c
		F= MM[1] *dt^3  /den
		
		IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
			pder1=dt^3 / den -dt^5 *ex *b1 /(2*den^2 *b^3 *MM[1]^(1./4))
			pder0=-3*MM[1]*dt^2 /den + 2*ex *MM[1] *dt^4 /(den^2. *b^2)
			pder = [[pder0], [pder1]]
			;return
		endif
		return
	ENDIF else if numel(MM) eq 3 then begin
	
		ex=Exp(dt^2 /MM[2]^2)
		den=ex-c
		F=MM[1] *dt^3  /den
		IF N_PARAMS() GE 4 THEN BEGIN
			pder1= dt^3 / den
			pder2=2*ex*MM[1]*dt^5 /(den^2 *MM[2]^3)
			pder0= - 3*MM[1]*dt^2 /den + 2* ex *MM[1] *dt^4/ ( den^2 *MM[2]^2  )
			;pder1=dt^3 / den -dt^5 *ex *b1 /(2*den^2 *b^3 *MM[1]^(1./4))
			pder0=-3*MM[1]*dt^2 /den + 2*ex *MM[1] *dt^4 /(den^2. *b^2)
			pder = [[pder0], [pder1],[pder2]]
			;return
		endif
		return		
	endif
	
	c=MM[3]
	ex=Exp(dt^2 /MM[2]^2)
	den=ex-c
	F=MM[1] *dt^3  /den
	IF N_PARAMS() GE 4 THEN BEGIN
			pder1= dt^3 / den
			pder2=2*ex*MM[1]*dt^5 /(den^2 *MM[2]^3)
			pder3=MM[1]*dt^3/den^2
			pder0= - 3*MM[1]*dt^2 /den + 2* ex *MM[1] *dt^4/ ( den^2 *MM[2]^2  )
			;pder1=dt^3 / den -dt^5 *ex *b1 /(2*den^2 *b^3 *MM[1]^(1./4))
			pder0=-3*MM[1]*dt^2 /den + 2*ex *MM[1] *dt^4 /(den^2. *b^2)
			pder = [[pder0], [pder1],[pder2],[pder3]]
			;return
	endif
end
