
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



PRO machNumCalc2

	
;Load Magnetic Field Data

	get_data,'mvn_B_1sec',data=DatMag,alim=lim

	;MagVec=Transpose([DatMag.Y[*,0],DatMag.Y[*,1],DatMag.Y[*,2]])
	MagVec=DatMag.Y

	help,DatMag
	xs=DatMag.x
	print,"numel(xs)=",numel(xs)
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
	
	Pzz=interpol(Pzz,datPressureCA.x,xs)
	Pyy=interpol(Pyy,datPressureCA.x,xs)
	Pxx=interpol(Pxx,datPressureCA.x,xs)
	print,"numel(Pzz)=",numel(Pzz)

	ScalarPressureCA=(Pxx+Pyy+Pzz)/3

	

;This is in eV/cm^3 want it in SI

;1 eV/cm^3=1.602E-13 kg/(m s^2) lets do unit conversions at the end.

	;ScalarPressureCA=SQRT(Pxx^2+Pyy^2+Pzz^2+2*Pxy^2+2*Pxz^2 +2*Pyz^2)  
	;ScalarPressureCA=SQRT(Transpose(Pxx)#Pxx +Transpose(Pyy)#Pyy+transpose(Pzz)#Pzz+2*transpose(Pxy)#Pxy+2*transpose(Pxz)#Pxz+2*transpose(Pyz)#Pyz)
	;print,"scalarPressure"
	;print,ScalarPressureCA[1]

	;Here we load the density

	get_data,'mvn_swica_density',data=datDensityCA,alim=limD

	;nn=datDensityCA.Y

	nn=interpol(datDensityCA.Y,datDensityCA.x,xs)
	print,"numel(nn)=",numel(nn)
;this is the number density in 1/cm^3. we want mass density in kg/m^3

;for  purposes of precision, we'll give  mass first in MeV/c^2 so we don't have to mess with pressure
	protonEVmass=938.0*10.0^6




	EeV=ProtonEVmass*nn
	rndm=EeV[777]


	Cs= SQRT(ScalarPressureCA/EeV)*!const.c
	Cspeed=Cs/1000
	print,Cspeed[1]
;->this should be on the order of tens of km. Something has gone horribly wrongq
	;datDensityCA.Y=Cspeed
	;datDensityCA.YTITLE="Sound Speed (km/s)"
	;datDensityCA.x=xs

;But for later, we'll still switch to SI
	Rho=!const.mp* 10.0^6 *nn
;1/cm^3=10.0^6 /m^3

	store_data,'v_Sound_coarse',data = {x:xs,y:Cspeed,ytitle:"Sound Speed (km/s)"},dlim=limD

	get_data,'mvn_swica_velocity',data=datVelCA,alim=limV
; this is in km/s Which is fine for our purposes
	;vCVec = [datVelCA.Y[*,0],datVelCA.Y[*,1],datVelCA.Y[*,2]]
	;print,vCVec

	;can be more compactly written as:
	vCVec=datVelCA.Y
	;print,vCVec
	vCx=interpol(datVelCA.Y[*,0],datVelCA.x,xs)
	vCy=interpol(datVelCA.Y[*,1],datVelCA.x,xs)
	vCz=interpol(datVelCA.Y[*,2],datVelCA.x,xs)
	vCVec=[[vCx],[vCy],[vCz]]
	print,"numel(vCz)=",numel(vCz)
	;print,"vcx"
	;print, vCx
	;print,"vCx[*]"
	;print, vCx[*]
	;print,"vCx[3]"
	;print, vcVec[3]
	;print,"cspeed"
	;print,Cspeed
	;vCAscalar=SQRT(TRANSPOSE(vCx)#vCx+TRANSPOSE(vCy)#vCy+TRANSPOSE(vCz)#vCz)
	vCAscalar=VECMAG(vCVec)

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
	print,"numel(MacCCvec)=",numel(MacCCvec)
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
	;datVelCA.Y=MacCCvec
	testVelCA=datVelCA.Y[7,2]
	;print, "testVelCA"
	;print, testVelCA
	relCMACH=testMACH-testVelCA

	;print, "relCMACH"
	;print, relCMACH

	labels=datVelCA.labels
	labflag=datVelCA.labflag
	v=datVelCA.v
	
;Sonic Mach Number vector plot defined here
	;datVelCA.x=xs
	datVelCA.Ytitle= 'Sonic Mac Speed Vector (coarse)'
	store_data,'Mach_sonic_Vec_coarse',data={x:xs,y:MacCCvec,v:v,ytitle:'Sonic Mac Speed Vector (coarse)',labels:labels,labflag:labflag},dlim=limV

;Sonic Mach Number scalar defined here

	MacCCscalar0=vCAscalar/Cspeed ;mach number should be a scalar, but we gave an option for a vector version anyway
	MacCCscalar=VECMAG(MacCCvec)
	mcdiff=MacCCscalar-MacCCscalar0
	print,"SONICMACHDIFF"
	;print, mcdiff
;	datDensityCA.Y=MacCCscalar
	datDensityCA.Ytitle='Sound Mach Number (coarse)'
	;limV.YTITLE='Sound Mach Number (coarse)'

	store_data,'mach_sound_scalar_coarse',data ={x:xs,y:MacCCscalar,ytitle:'Sound Mach Number (coarse)'},dlim=limD





;This is  in nT. SI unit is in tesla

	;MagVec=MagVec

;calculate the alfven velocity vectors

	;vACVec=MagVec/SQRT(!const.MU0 * Rho),DatMag.Y[*,1]/SQRT(!const.MU0 * Rho),DatMag.Y[*,2]/SQRT(!const.MU0 * Rho)])

	vACVe=VecDiv(MagVec, SQRT(!const.MU0 * Rho))

;this is in m/s want in km/s. the  10^-9  is for converting

	vACVec=vACVe/(1000.0*10.0^9)
;comparing to mathematica, we seem to have lost two more powers of ten somhow: our result should be at the high end about 60km/s. I can't identify where, so lets fudge it in temporarily.

;	vACVec=vACVec


;stores alfven vectors in datVelCA so that plot can be constructed from it

	;datVelCA.Y=vACVec
	datVelCA.ytitle='Alfven Velocity (Coarse) (km/s)'  
	;lim.ytitle= 'Alfven Velocity (Coarse) (km/s)'  

	store_data,'v_Alfven_coarse',data = {x:xs,y:vACVec,v:v,ytitle:'Alfven Velocity (Coarse) (km/s)' ,labels:labels,labflag:labflag} ,dlim=lim       

	;vAsqr=TRANSPOSE(vACVec[*,0])#vACVec[*,0]+TRANSPOSE(vACVec[*,1])#vACVec[*,1]+TRANSPOSE(vACVec[*,2])#vACVec[*,2]
	vAsqr=VECSQUARE2(vACVec[*,0],vACVec[*,1],vACVec[*,2])
	
	vAscalar=Sqrt(vAsqr)
	print,"numel(vAscalar)=",numel(vAscalar)
	;datDensityCA.Y=vAscalar
	datDensityCA.Ytitle='Alfven velocity (km/s)'
	;limV.YTITLE='Alfven Velocity (coarse)'

	store_data,'alfven_velocity_coarse',data = {x:xs,y:vAscalar,ytitle:'Alfven velocity (km/s)'},dlim=limD


	fms=SQRT(Cspeed^2+vAsqr)   ;Fast Magnetosonic Speed
	datDensityCA.ytitle= 'Fast Magnetosonic Speed (km/s)'
	;lim.ytitle='Fast Magnetosonic "+STRING(10B)+"Speed (km/s)'
	;datDensityCA.Y=fms		
	store_data,'Fast_Magnetosonic_coarse',data={x:xs,y:fms,ytitle:'Fast Magnetosonic Speed (km/s)'},dlim=limV	

	M_fmsVec=VecDiv(vCVec,fms)  ;Mach number should be a scalar, but we'll create a vector version anyway


	;datVelCA.ytitle= 'Fast Magnetosonic Mach numbers'
	;limV.ytitle= 'Fast Magnetosonic Mach numbers'
	;datVelCA.Y=M_fmsVec
	store_data,'Mach_fms_vec_coarse',data={x:xs,y:M_fmsVec,v:v,ytitle:'FMS Mach number' ,labels:labels,labflag:labflag},dlim=limV

	;Mfms = SQRT(TRANSPOSE(M_fmsVec)#M_fmsVec)
	
	;Mfms=SQRT(TRANSPOSE(M_fmsVec[*,0])#M_fmsVec[*,0]+TRANSPOSE(M_fmsVec[*,1])#M_fmsVec[*,1]+TRANSPOSE(M_fmsVec[*,2])#M_fmsVec[*,2])


;the actual FMS Mach number


	Mfms=vCAscalar/fms

	rndm2=Mfms[777]

	print, rndm-rndm2

	;datDensityCA.Y=Mfms
	datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_coarse',data = {x:xs,y:Mfms,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"},dlim=limD

	;datDensityCA.Y=Rho


;The normal components of the mach numbers: that is \vec{M}_{fms}\cdot\hat{n}_{SW}
	get_data,'Shock_Normal_MX1',data=datMX1
	get_data,'Shock_Normal_MX2',data=datMX2
	get_data,'Shock_Normal_MX3',data=datMX3

	MX1=datMX1.y
	MX2=datMX2.y
	MX3=datMX3.y

	print,"[numel(M_fmsVec[*,0]),numel(MX3[*,0])]=",[numel(M_fmsVec[*,0]),numel(MX3[*,0])]
	MfmsMX1=abs(DotProduct(M_fmsVec,MX1))
	MfmsMX2=abs(DotProduct(M_fmsVec,MX2))
	MfmsMX3=abs(DotProduct(M_fmsVec,MX3)) 
	GG=where(MfmsMX3 ne 0)
	print,"GG=where(MfmsMX3 ne 0)=",GG
	print,"MfmsMX3[GG]=",MfmsMX3[GG]
	;datDensityCA.Y=MfmsMX1
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_coarse_MX1',data = {x:xs,y:MfmsMX1,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}

;	datDensityCA.Y=MfmsMX2
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_coarse_MX2',data ={x:xs,y:MfmsMX2,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}

;	datDensityCA.Y=MfmsMX3
	;datDensityCA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	print,"numel(MfmsMX3)=",numel(MfmsMX3)
	store_data,'mach_fms_coarse_MX3',data = {x:xs,y:MfmsMX3,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}
END



