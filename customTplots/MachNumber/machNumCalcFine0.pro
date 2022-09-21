
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



PRO machNumCalcFine

	

	;problem with using fine data is that it doesn't always measure in regions of interest. Lets try Fine

;Here we will calculate various mach numbers

;Start by loading Pressure and turning it into a scalar
	get_data,'mvn_swifa_pressure',data=datPressureFA  ,alim=lim
	;Pyz= datPressureFA.Y[*,0]

	;Pyz= datPressureFA.Y[*,5]
	;Pxz= datPressureFA.Y[*,4]
	;Pxy= datPressureFA.Y[*,3]
	Pzz= datPressureFA.Y[*,2]
	Pyy= datPressureFA.Y[*,1]
	Pxx= datPressureFA.Y[*,0]
	
	ScalarPressureFA=(Pxx+Pyy+Pzz)/3

	

;This is in eV/cm^3 want it in SI

;1 eV/cm^3=1.602E-13 kg/(m s^2) lets do unit conversions at the end.

	;ScalarPressureFA=SQRT(Pxx^2+Pyy^2+Pzz^2+2*Pxy^2+2*Pxz^2 +2*Pyz^2)  
	;ScalarPressureFA=SQRT(Transpose(Pxx)#Pxx +Transpose(Pyy)#Pyy+transpose(Pzz)#Pzz+2*transpose(Pxy)#Pxy+2*transpose(Pxz)#Pxz+2*transpose(Pyz)#Pyz)
	print,"scalarPressure"
	;print,ScalarPressureFA

	;Here we load the density

	get_data,'mvn_swifa_density',data=datDensityFA,alim=limD

	nn=datDensityFA.Y
;this is the number density in 1/cm^3. we want mass density in kg/m^3

;for  purposes of precision, we'll give  mass first in MeV/c^2 so we don't have to mess with pressure
	protonEVmass=937*10.0^6




	EeV=ProtonEVmass*nn
	rndm=EeV[777]

;But for later, we'll still switch to SI
	Rho=!const.mp* 10.0^6 *nn
;1/cm^3=10.0^6 /m^3

	Cs= SQRT(ScalarPressureFA/EeV)*!const.c
;this is in m/s. Want in km/s
	Cspeed=Cs/1000
;->this should be on the order of tens of km. Something has gone horribly wrongq
	datDensityFA.Y=Cspeed
	datDensityFA.YTITLE="Sound Speed (km/s)"


	store_data,'v_Sound_fine',data = datDensityFA,dlim=limD

	get_data,'mvn_swifa_velocity',data=datVelFA,alim=limV
; this is in km/s Which is fine for our purposes
	;vCVec = [datVelFA.Y[*,0],datVelFA.Y[*,1],datVelFA.Y[*,2]]
	;print,vCVec

	;can be more compactly written as:
	vCVec=datVelFA.Y
	;print,vCVec
	vCx=datVelFA.Y[*,0]
	vCy=datVelFA.Y[*,1]
	vCz=datVelFA.Y[*,2]
	;print,"vcx"
	;print, vCx
	;print,"vCx[*]"
	;print, vCx[*]
	;print,"vCx[3]"
	;print, vcVec[3]
	;print,"cspeed"
	;print,Cspeed
	;vFAscalar=SQRT(TRANSPOSE(vCx)#vCx+TRANSPOSE(vCy)#vCy+TRANSPOSE(vCz)#vCz)
	vFAscalar=VECMAG(vCVec)

	;tstr=vCx/Cspeed
	;print,"vCx/Cspeed"
	;print,tstr

	;tstr2=transpose(vCx)/Cspeed
	;print,"transpose(vCx)/Cspeed"
	;print,tstr2
	;tstr3=VecDiv(vCVec,Cspeed)
	;print,"VecDiv(vCVec,Cspeed)"
	;print,tstr3
	
	MacCCvec=VecDiv(vCVec,Cspeed)
	;MacCCvec =transpose([transpose(vCx/transpose(Cspeed)),transpose(vCy/transpose(Cspeed)),transpose(vCz/transpose(Cspeed))])
	;above is probably unecessary
	;MacCCvec=vCVec/Cspeed

;Bunch of tests to see if the output is the correct size
	;print,"MacCCvec"
	;print,MacCCvec
	;print,"[0,*]"
	;print,MacCCvec[*,0]
	;print,"[*,1]"
	;print,MacCCvec[*,1]
	;print,"[2,*]"
	;print,MacCCvec[2,*]
	;print,"[7,2]"
	testMACH=MacCCvec[7,2]
	;print, "testMACH"
	;print, testMACH 
	datVelFA.Y=MacCCvec
	testVelFA=datVelFA.Y[7,2]
	;print, "testVelFA"
	;print, testVelFA
	relCMACH=testMACH-testVelFA

	;print, "relCMACH"
	;print, relCMACH


;Sonic Mach Number vector plot defined here

	datVelFA.Ytitle= 'Sonic Mac Speed Vector (Fine)'
	store_data,'Mach_sonic_Vec_Fine',data=datVelFA,dlim=limV

;Sonic Mach Number scalar defined here

	MacCCscalar0=vFAscalar/Cspeed ;mach number should be a scalar, but we gave an option for a vector version anyway
	MacCCscalar=VECMAG(MacCCvec)
	mcdiff=MacCCscalar-MacCCscalar0
	print,"SONICMACHDIFF"
	;print, mcdiff
	datDensityFA.Y=MacCCscalar
	datDensityFA.Ytitle='Sound Mach Number (Fine)'
	;limV.YTITLE='Sound Mach Number (Fine)'

	store_data,'mach_sound_scalar_Fine',data = datDensityFA,dlim=limD


;Load Magnetic Field Data

	get_data,'mvn_B_1sec',data=DatMag,alim=lim

	;MagVec=Transpose([DatMag.Y[*,0],DatMag.Y[*,1],DatMag.Y[*,2]])
	MagVec=DatMag.Y


;This is  in nT. SI unit is in tesla

	;MagVec=MagVec

;calculate the alfven velocity vectors

	;vACVec=MagVec/SQRT(!const.MU0 * Rho),DatMag.Y[*,1]/SQRT(!const.MU0 * Rho),DatMag.Y[*,2]/SQRT(!const.MU0 * Rho)])

	vACVe=VecDiv(MagVec, SQRT(!const.MU0 * Rho))

;this is in m/s want in km/s

	vACVec=vACVe/(1000.0*10.0^9)
;comparing to mathematica, we seem to have lost two more powers of ten somhow: our result should be at the high end about 60km/s. I can't identify where, so lets fudge it in temporarily.

;	vACVec=vACVec


;stores alfven vectors in datVelFA so that plot can be constructed from it

	datVelFA.Y=vACVec
	datVelFA.ytitle='Alfven Velocity (Fine) (km/s)'  
	;lim.ytitle= 'Alfven Velocity (Fine) (km/s)'  

	store_data,'v_Alfven_Fine',data = datVelFA ,dlim=lim       

	;vAsqr=TRANSPOSE(vACVec[*,0])#vACVec[*,0]+TRANSPOSE(vACVec[*,1])#vACVec[*,1]+TRANSPOSE(vACVec[*,2])#vACVec[*,2]
	vAsqr=VECSQUARE2(vACVec[*,0],vACVec[*,1],vACVec[*,2])
	
	vAscalar=Sqrt(vAsqr)

	datDensityFA.Y=vAscalar
	datDensityFA.Ytitle='Alfven velocity (km/s)'
	;limV.YTITLE='Alfven Velocity (Fine)'

	store_data,'alfven_velocity_Fine',data = datDensityFA,dlim=limD


	fms=SQRT(Cspeed^2+vAsqr)   ;Fast Magnetosonic Speed
	datDensityFA.ytitle= 'Fast Magnetosonic"+"STRING(10B)+"Speed (km/s)'
	;lim.ytitle='Fast Magnetosonic Speed (km/s)'
	datDensityFA.Y=fms		
	store_data,'Fast_Magnetosonic_Fine',data=datDensityFA,dlim=limV	

	M_fmsVec=VecDiv(vCVec,fms)  ;Mach number should be a scalar, but we'll create a vector version anyway


	datVelFA.ytitle= 'Fast Magnetosonic Mach numbers'
	;limV.ytitle= 'Fast Magnetosonic Mach numbers'
	datVelFA.Y=M_fmsVec
	store_data,'Mach_fms_vec_Fine',data=datVelFA,dlim=limV

	;Mfms = SQRT(TRANSPOSE(M_fmsVec)#M_fmsVec)
	
	;Mfms=SQRT(TRANSPOSE(M_fmsVec[*,0])#M_fmsVec[*,0]+TRANSPOSE(M_fmsVec[*,1])#M_fmsVec[*,1]+TRANSPOSE(M_fmsVec[*,2])#M_fmsVec[*,2])


;the actual FMS Mach number


	Mfms=vFAscalar/fms

	rndm2=Mfms[777]

	print, rndm-rndm2

	datDensityFA.Y=Mfms
	datDensityFA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_Fine',data = datDensityFA,dlim=limD

	datDensityFA.Y=Rho


;The normal components of the mach numbers: that is \vec{M}_{fms}\cdot\hat{n}_{SW}
	get_data,'Shock_Normal_MX1',data=datMX1
	get_data,'Shock_Normal_MX2',data=datMX2
	get_data,'Shock_Normal_MX3',data=datMX3

	MX1=datMX1.y
	MX2=datMX2.y
	MX3=datMX3.y

	MfmsMX1=DotProduct(M_fmsVec,MX1)
	MfmsMX2=DotProduct(M_fmsVec,MX2)
	MfmsMX3=DotProduct(M_fmsVec,MX3) 

	datDensityFA.Y=MfmsMX1
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_Fine_MX1',data = datDensityFA,dlim=limD

	datDensityFA.Y=MfmsMX2
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_Fine_MX2',data = datDensityFA,dlim=limD

	datDensityFA.Y=MfmsMX3
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_Fine_MX3',data = datDensityFA,dlim=limD


END


