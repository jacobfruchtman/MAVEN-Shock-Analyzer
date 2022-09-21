pro solarfitdu,X,MM,F,pder
	
	;;; A exp( - (t-tmax)^2 /(2*B^2  *(1+alpha *(t-tmax))^2  )  )
	;;; =MM[1] *exp( -(t-MM[0])^2 /(2*MM[2]^2 *(1+MM[3] *(t-MM[0]))^2))
	
	;;; MM[0]==tmax == peak of curve
	
	;;;for 2 paramameters, B=-(22.)+MM[0]*(1.5)-(.0115)*MM[0]^2
	;;;	and	   alpha=(48.4)-(1.35)*MM[0] +(.00943)*MM[0]^2
	
	

	tm=MM[0]
	A=MM[1]	
	dt=X-MM[0]
	
	if numel(MM) eq 2 then begin

		b0=-22.
		b1=1.53
		b2=-.0115
		
		c0=48.4
		c1=-1.35
		c2=.00943
		

		b=b0+b1*MM[0]+b2*MM[0]^2
		c=c0+c1*tm+c2*tm^2
		
		sx=(1+c*dt)
		den=2*b^2 *sx^2
		
		iex=dt^2/ den
		ex=Exp(-iex)
		F= MM[1]*ex
		
		IF N_PARAMS() GE 4 THEN BEGIN
		;print,"size(MM) after F at beginning of if=",size(MM)
			pder1=ex
			pder0=ex* MM[1]*(-2*dt/den + (b1+2 *b2*tm)*dt^2 /(b^3*sx^2)+ dt^2 *(-c +(c1+2*c2*tm)*dt )/(b^2 *sx^3))
			pder = [[pder0], [pder1]]
			;return
		endif
		return
	ENDIF 	
	c=MM[3]
	b=MM[2]
	sx=(1+c*dt)
	den=2.*b^2 *sx^2
		
	iex=dt^2/ den
	ex=Exp(-iex)
	F= MM[1]*ex
		
	

	IF N_PARAMS() GE 4 THEN BEGIN
			pder1= ex
			pder2=ex *dt^2*A /(b^3 *sx^2)
			
			pder3=MM[1]*ex *dt^3 /(b^2 *sx^3  )
			pder0= A*ex *(-2*iex *c/SX +dt/(sx^2 *b^2)) 

			pder = [[pder0], [pder1],[pder2],[pder3]]
			;return
	endif
end
