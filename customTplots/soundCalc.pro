;FUNCTION LIST
	

FUNCTION VECSQUARE,vv
		;"Programs can't be compiled from single statement mode." What?
		;"Variable is undefined: VV." WHAT?

			sqr=0
	    FOR i = 0, 2 DO BEGIN	
	   	sqr=sqr+vv[*,i]*vv[*,i]
	
		ENDFOR
		
	    RETURN, sqr
	
END
	
FUNCTION VECSQUARE2, vx,vy,vz
		sqr=vx*vx+vy*vy+vz*vz
	
		;ENDFOR
	
	    RETURN, sqr
	
END
	
FUNCTION VECMAG, vv
		mag=0
		mag=SQRT(VECSQUARE(vv))
	
		RETURN, mag
END
	
FUNCTION VECMAG2, vecx, vecy , vecz
		mag=0
		mag=SQRT(VECSQUARE2(vecx,vecy,vecz))
	
		RETURN, mag
END

FUNCTION VECDIV,vv,denom
	
		q=TRANSPOSE([transpose(vv[*,0]/denom),transpose(vv[*,1]/denom),transpose(vv[*,2]/denom)])
		
		RETURN, q
END
	
FUNCTION TESTFUNCTION, x
		y= x^2
	RETURN, y 
END

FUNCTION VECSTORE, dat
	vec=dat.Y
	return, vec
END


; ▄▄▄▄▄▄▄ ▄▄▄▄▄▄   ▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄ ▄▄▄▄▄▄  ▄▄   ▄▄ ▄▄▄▄▄▄   ▄▄▄▄▄▄▄ 
;█       █   ▄  █ █       █       █       █      ██  █ █  █   ▄  █ █       █
;█    ▄  █  █ █ █ █   ▄   █       █    ▄▄▄█  ▄    █  █ █  █  █ █ █ █    ▄▄▄█
;█   █▄█ █   █▄▄█▄█  █ █  █     ▄▄█   █▄▄▄█ █ █   █  █▄█  █   █▄▄█▄█   █▄▄▄ 
;█    ▄▄▄█    ▄▄  █  █▄█  █    █  █    ▄▄▄█ █▄█   █       █    ▄▄  █    ▄▄▄█
;█   █   █   █  █ █       █    █▄▄█   █▄▄▄█       █       █   █  █ █   █▄▄▄ 
;█▄▄▄█   █▄▄▄█  █▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄██▄▄▄▄▄▄▄█▄▄▄█  █▄█▄▄▄▄▄▄▄█



PRO soundCalc

	

	;problem with using fine data is that it doesn't always measure in regions of interest. Lets try coarse

;Here we will calculate various mach numbers

;Start by loading Pressure and turning it into a scalar
	get_data,'mvn_swica_pressure',data=datPressureCA  ,alim=lim
	;Pyz= datPressureCA.Y[*,0]

	;Pyz= datPressureCA.Y[*,5]
	;Pxz= datPressureCA.Y[*,4]
	;Pxy= datPressureCA.Y[*,3]
	Pzz= datPressureCA.Y[*,2]
	Pyy= datPressureCA.Y[*,1]
	Pxx= datPressureCA.Y[*,0]
	print, [Pxx[1],Pyy[1],Pzz[1]]
	ScalarPressureCA=(Pxx+Pyy+Pzz)/3
	pp=ScalarPressureCA
	print, pp[1]
	

;This is in eV/cm^3 want it in SI

;1 eV/cm^3=1.602E-13 kg/(m s^2) lets do unit conversions at the end.

	;ScalarPressureCA=SQRT(Pxx^2+Pyy^2+Pzz^2+2*Pxy^2+2*Pxz^2 +2*Pyz^2)  
	;ScalarPressureCA=SQRT(Transpose(Pxx)#Pxx+Transpose(Pyy)#Pyy+transpose(Pzz)#Pzz+2*transpose(Pxy)#Pxy+2*transpose(Pxz)#Pxz+2*transpose(Pyz)#Pyz)
	print,"scalarPressure"
	print,ScalarPressureCA[1]

	;Here we load the density

	get_data,'mvn_swica_density',data=datDensityCA,alim=limD

	nn=datDensityCA.Y
;this is the number density in 1/cm^3. we want mass density in kg/m^3

;for  purposes of precision, we'll give  mass first in MeV/c^2 so we don't have to mess with pressure
	protonEVmass=938.0*10^6




	EeV=ProtonEVmass*nn
	rndm=EeV[777]


	Cs= SQRT(ScalarPressureCA/EeV)*!const.c
	Cspeed=Cs/1000
	print,Cspeed[1]
;->this should be on the order of tens of km. Something has gone horribly wrongq
	datDensityCA.Y=Cspeed
	datDensityCA.YTITLE="Sound Speed (km/s)"


;But for later, we'll still switch to SI
	Rho=!const.mp* 10^6 *nn
;1/cm^3=10^6 /m^3

	store_data,'v_Sound',data = datDensityCA,dlim=limD
END
