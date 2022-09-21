
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
		;mag=0
		mag=SQRT(VECSQUARE(vv))
	
		RETURN, mag
END
	
FUNCTION VECMAG2, vecx, vecy , vecz
		mag=0
		mag=SQRT(VECSQUARE2(vecx,vecy,vecz))
	
		RETURN, mag
END

FUNCTION VECDIV,vv,denom
		vv2=vv
		for i=0,numel(denom)-1 do begin
			
			if denom[i] ne 0 then vv2[i,*]=[vv[i,0]/denom[i],vv[i,1]/denom[i],vv[i,2]/denom[i]] else vv2[i,*]=[0.,0.,0.]
		endfor
		;q=TRANSPOSE([transpose(vv[*,0]/denom),transpose(vv[*,1]/denom),transpose(vv[*,2]/denom)])
		
		RETURN, vv2
END
	
FUNCTION TESTFUNCTION, x
		y= x^2
	RETURN, y 
END

FUNCTION VECSTORE, dat
	vec=dat.Y
	return, vec
END

FUNCTION VECMEAN,vec,istart,iend
	;RETURN, [[vec[istart:iend,0]],[vec[istart:iend,1]],[vec[istart:iend,2]]]
	RETURN, [mean(vec[istart:iend,0],/nan),mean(vec[istart:iend,1],/nan),mean(vec[istart:iend,2],/nan)]
END

FUNCTION ADJDOT, vec1,vec2

	GG=where(VECMAG(vec2) ne 0, gcount)
	N=numel(ins)
	z=ins*0.0
	for el=0,gcount-1 do begin ;I know I can use foreach I prefer it this way

		i=GG[el]

		if ins[i] eq 1 then begin
			j=max([0,i-7*60])

			adj=VECMEAN(vec1,j,j+60)

			z[i]= adj[0]*vec2[i,0]+adj[1]*vec2[i,1]+adj[2]*vec2[i,2]
		endif
	
		if outs[i] eq 1 then begin
			j=min([N-1,i+7*60])

			adj=VECMEAN(vec1,j-60,j)

			z[i]= adj[0]*vec2[i,0]+adj[1]*vec2[i,1]+adj[2]*vec2[i,2]
		endif


	endfor

	return,z


RETURN, dp
END

FUNCTION ADJDOT2, vec1,vec2,ui

	;get_data,"shocks_inbound",data=datin
	;get_data,"shocks_outbound",data=datout

	;ins=datin.y
	;outs=datout.y

	GG=where(VECMAG(vec2) ne 0, gcount)
	N=numel(ui[*,0])
	z=fltarr(N)
	for el=0,gcount-1 do begin ;I know I can use foreach I prefer it this way

		i=GG[el]

		ubeg=ui[i,0]
		uend=ui[i,1]

		;if ins[i] eq 1 then begin
			;j=max([0,i-7*60])

			adj=VECMEAN(vec1,ubeg,uend);j,j+60)

			z[i]= adj[0]*vec2[i,0]+adj[1]*vec2[i,1]+adj[2]*vec2[i,2]
		;endif
	



	endfor

	return,z


RETURN, dp
END

; ▄▄▄▄▄▄▄ ▄▄▄▄▄▄   ▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄ ▄▄▄▄▄▄  ▄▄   ▄▄ ▄▄▄▄▄▄   ▄▄▄▄▄▄▄ 
;█       █   ▄  █ █       █       █       █      ██  █ █  █   ▄  █ █       █
;█    ▄  █  █ █ █ █   ▄   █       █    ▄▄▄█  ▄    █  █ █  █  █ █ █ █    ▄▄▄█
;█   █▄█ █   █▄▄█▄█  █ █  █     ▄▄█   █▄▄▄█ █ █   █  █▄█  █   █▄▄█▄█   █▄▄▄ 
;█    ▄▄▄█    ▄▄  █  █▄█  █    █  █    ▄▄▄█ █▄█   █       █    ▄▄  █    ▄▄▄█
;█   █   █   █  █ █       █    █▄▄█   █▄▄▄█       █       █   █  █ █   █▄▄▄ 
;█▄▄▄█   █▄▄▄█  █▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄▄█▄▄▄▄▄▄██▄▄▄▄▄▄▄█▄▄▄█  █▄█▄▄▄▄▄▄▄█



PRO machNumCalcFine
	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout

	ins=datin.y
	outs=datout.y
	;if total(datin.y + datout.y) eq 0 then return

	get_data, "upstream_indices",data=datu
	ui=datu.y
;Load Magnetic Field Data

	

	shocks=ins+outs

	get_data,'mvn_B_1sec_MAVEN_MSO',data=DatMag,alim=lim
	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=DatBMag,alim=lim

	;MagVec=Transpose([DatMag.Y[*,0],DatMag.Y[*,1],DatMag.Y[*,2]])
	MagVec=DatMag.Y

	;help,DatMag
	xs=DatMag.x
	N=numel(xs)
	;print,"numel(xs)=",numel(xs)
	;problem with using fine data is that it doesn't always measure in regions of interest. Lets try Fine
	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=DatBMag,alim=lim
	Bmag=DatBMag.y
;Here we will calculate various mach numbers

;Start by loading Pressure and turning it into a scalar
	;get_data,"mvn_swifs_pressure",data=datPressureFA  ,alim=lim
	;get_data,'mvn_swifa_pressure_proton',data=datPressureFA
	get_data,'mvn_pressure_electron',data=datPressureE
	get_data,"Plasma_Beta_upstream_flattened",data=datBeta
	get_data,'Shock_Angle_AVG',data=datThetaNB
	get_data,'Flow_Angle',data=datThetaVB

	thetaNB=datThetaNB.y
	thetaNBn=!pi/2-abs(thetaNB-!pi/2)
	thetaVB=datThetaVB.y
	thetaVBn=!pi/2-abs(thetaVB-!pi/2)
	
	betas=datBeta.y
	;Pyz= datPressureFA.Y[*,0]

	;Pyz= datPressureFA.Y[*,5]
	;Pxz= datPressureFA.Y[*,4]
	;Pxy= datPressureFA.Y[*,3]
	;Pzz= datPressureFA.Y[*,2]
	;Pyy= datPressureFA.Y[*,1]
	;Pxx= datPressureFA.Y[*,0]
	
	;Pzz=interpol(Pzz,datPressureFA.x,xs)
	;Pyy=interpol(Pyy,datPressureFA.x,xs)
	;Pxx=interpol(Pxx,datPressureFA.x,xs)
	;print,"numel(Pzz)=",numel(Pzz)

	;ScalarPressureFA=datPressureFA.Y;(Pxx+Pyy+Pzz)/3
	;this is in eV/cm^3
	PE=datPressureE.y

	get_data,"mvn_swifs_temperature",data=datTemp
	get_data,"tproton_Mag",data=datTP
	get_data,"talpha_Mag",data=datTA
	get_data,"mvn_swics_temperature",data=datTcs

	Temp=datTP.y;datTemp.y[*,3]

	TT=interpol(Temp,datTP.x,xs)	
        ;this is in eV


;This is in eV/cm^3 want it in SI

;1 eV/cm^3=1.602E-13 kg/(m s^2) lets do unit conversions at the end.

	;ScalarPressureFA=SQRT(Pxx^2+Pyy^2+Pzz^2+2*Pxy^2+2*Pxz^2 +2*Pyz^2)  
	;ScalarPressureFA=SQRT(Transpose(Pxx)#Pxx +Transpose(Pyy)#Pyy+transpose(Pzz)#Pzz+2*transpose(Pxy)#Pxy+2*transpose(Pxz)#Pxz+2*transpose(Pyz)#Pyz)
	;;print,"scalarPressure"
	;;print,ScalarPressureFA[1]

	;Here we load the density

	get_data,'mvn_swifs_density',data=datDensityFA,alim=limD
	get_data,'mvn_swe_spec_dens_interpolated',data=datNelectron
	get_data,'mvn_swe_spec_temp_interpolated',data=datTelectron
	nn=datDensityFA.Y
	;nn[where(finite(nn, /NAN))]=0
	nn=interpol(nn,datDensityFA.x,xs)
	;print,"numel(nn)=",numel(nn)

	N_el=datNelectron.y
	protonPressure=nn*TT
	T_el=datTelectron.y
;this is the number density in 1/cm^3. we want mass density in kg/m^3

;for  purposes of precision, we'll give  mass first in MeV/c^2 so we don't have to mess with pressure
	m_p=938.0*10.0^6

	m_el=.511*10.0^6

	g_e=1.
	g_i=5./3.

	rho_p=m_p*nn
	;rndm=rho_p[777]
	rho_e=m_el*N_el
	Cs= SQRT((g_i*TT +g_e*T_el)/m_p   )*!const.c;SQRT(protonPressure/rho_p)*!const.c;SQRT(ScalarPressureFA/rho_p)*!const.c
;	Cs= SQRT((5./3.)* ScalarPressureFA/rho_p)*!const.c;SQRT(protonPressure/rho_p)*!const.c;SQRT(ScalarPressureFA/rho_p)*!const.c
	Cspeed=Cs/1000
	;print,Cspeed[1]
;->this should be on the order of tens of km. Something has gone horribly wrongq
	;datDensityFA.Y=Cspeed
	;datDensityFA.YTITLE="Sound Speed (km/s)"
	;datDensityFA.x=xs

;But for later, we'll still switch to SI
	Rho=!const.mp* 10.0^6 *nn
;1/cm^3=10.0^6 /m^3

	store_data,'v_Sound_Fine',data = {x:xs,y:Cspeed,ytitle:"Sound Speed (km/s)"},dlim=limD

	get_data,'mvn_swifs_velocity',data=datVelFA,alim=limV
	get_data,'mvn_swifs_velocity_MAVEN_MSO',data=datVelFMSO,alim=limV
; this is in km/s Which is fine for our purposes
	;vCVec = [datVelFA.Y[*,0],datVelFA.Y[*,1],datVelFA.Y[*,2]]
	;;print,vCVec

	;can be more compactly written as:
	vCVec=datVelFMSO.Y
	;vCVec[where(finite(vCVec, /NAN))]=0
	;;print,vCVec
	vCx=interpol(datVelFMSO.Y[*,0],datVelFA.x,xs)
	vCy=interpol(datVelFMSO.Y[*,1],datVelFA.x,xs)
	vCz=interpol(datVelFMSO.Y[*,2],datVelFA.x,xs)
	vCVec=[[vCx],[vCy],[vCz]]
	;vCVec[where(finite(vCVec) eq 0)]=0
	;print,"numel(vCz)=",numel(vCz)
	;;print,"vcx"
	;;print, vCx
	;;print,"vCx[*]"
	;;print, vCx[*]
	;;print,"vCx[3]"
	;;print, vcVec[3]
	;;print,"cspeed"
	;;print,Cspeed
	;vFAscalar=SQRT(TRANSPOSE(vCx)#vCx+TRANSPOSE(vCy)#vCy+TRANSPOSE(vCz)#vCz)
	vFAscalar=VECMAG(vCVec)

	;tstr=vCx/Cspeed
	;;print,"vCx/Cspeed"
	;;print,tstr

	;tstr2=transpose(vCx)/Cspeed
	;;print,"transpose(vCx)/Cspeed"
	;;print,tstr2
	;tstr3=VecDiv(vCVec,Cspeed)
	;;print,"VecDiv(vCVec,Cspeed)"
	;;print,tstr3
	
	MacCCvec=VecDiv(vCVec,Cspeed)
	;print,"numel(MacCCvec)=",numel(MacCCvec)
	;MacCCvec =transpose([transpose(vCx/transpose(Cspeed)),transpose(vCy/transpose(Cspeed)),transpose(vCz/transpose(Cspeed))])
	;above is probably unecessary
	;MacCCvec=vCVec/Cspeed
	RhoSI= !const.mp* 10.0^6 *nn;Rho;+!const.me*10.0^6 * N_el
;Bunch of tests to see if the output is the correct size
	;;print,"MacCCvec"
	;;print,MacCCvec
	;;print,"[0,*]"
	;;print,MacCCvec[*,0]
	;;print,"[*,1]"
	;;print,MacCCvec[*,1]
	;;print,"[2,*]"
	;;print,MacCCvec[2,*]
	;;print,"[7,2]"
	testMACH=MacCCvec[7,2]
	;;print, "testMACH"
	;;print, testMACH 
	;datVelFA.Y=MacCCvec
	testVelFA=datVelFA.Y[7,2]
	;;print, "testVelFA"
	;;print, testVelFA
	relCMACH=testMACH-testVelFA

	;;print, "relCMACH"
	;;print, relCMACH

	labels=datVelFA.labels
	labflag=datVelFA.labflag
	v=datVelFA.v
	
;Sonic Mach Number vector plot defined here
	;datVelFA.x=xs
	datVelFA.Ytitle= 'Sonic Mac Speed Vector (Fine)'
	store_data,"Mach_sonic_Vec_Fine",data={x:xs,y:MacCCvec,v:v,ytitle:'Sonic Mac Speed Vector (Fine)',labels:labels,labflag:labflag},dlim=limV

;Sonic Mach Number scalar defined here

	MacCCscalar0=vFAscalar/Cspeed ;mach number should be a scalar, but we gave an option for a vector version anyway
	MacCCscalar=VECMAG(MacCCvec)
	mcdiff=MacCCscalar-MacCCscalar0
	;print,"SONICMACHDIFF"
	;;print, mcdiff
;	datDensityFA.Y=MacCCscalar
	datDensityFA.Ytitle='Sound Mach Number (Fine)'
	;limV.YTITLE='Sound Mach Number (Fine)'

	store_data,'mach_sound_scalar_Fine',data ={x:xs,y:MacCCscalar,ytitle:'Sound Mach Number (Fine)'},dlim=limD





;This is  in nT. SI unit is in tesla

	;MagVec=MagVec

;calculate the alfven velocity vectors

	;vACVec=MagVec/SQRT(!const.MU0 * Rho),DatMag.Y[*,1]/SQRT(!const.MU0 * Rho),DatMag.Y[*,2]/SQRT(!const.MU0 * Rho)])

	vACVe=VecDiv(MagVec, SQRT(!const.MU0 * RhoSI))

	;vACVe[where(finite(vACVe) eq 0)]=0
;this is in m/s want in km/s. the  10^-9  is for converting

	vACVec=vACVe/(1000.0*10.0^9)
;comparing to mathematica, we seem to have lost two more powers of ten somhow: our result should be at the high end about 60km/s. I can't identify where, so lets fudge it in temporarily.

;	vACVec=vACVec


;stores alfven vectors in datVelFA so that plot can be constructed from it

	;datVelFA.Y=vACVec
	datVelFA.ytitle='Alfven Velocity (Fine) (km/s)'  
	;lim.ytitle= 'Alfven Velocity (Fine) (km/s)'  

	store_data,"v_Alfven_Fine",data = {x:xs,y:vACVec,v:v,ytitle:'Alfven Velocity (Fine) (km/s)' ,labels:labels,labflag:labflag} ,dlim=lim       

	;vAsqr=TRANSPOSE(vACVec[*,0])#vACVec[*,0]+TRANSPOSE(vACVec[*,1])#vACVec[*,1]+TRANSPOSE(vACVec[*,2])#vACVec[*,2]
	vAsqr=VECSQUARE2(vACVec[*,0],vACVec[*,1],vACVec[*,2])
	
	vAscalar=Bmag/SQRT(!const.MU0 * RhoSI) /(1000.0*10.0^9);Sqrt(vAsqr)
	vAsqr=vAscalar^2
	;print,"numel(vAscalar)=",numel(vAscalar)
	;datDensityFA.Y=vAscalar
	datDensityFA.Ytitle='Alfven velocity (km/s)'
	;limV.YTITLE='Alfven Velocity (Fine)'

	store_data,"alfven_speed_Fine",data = {x:xs,y:vAscalar,ytitle:'Alfven velocity (km/s)'},dlim=limD


	;help,thetaVBn
	;help,Cspeed
	fms=SQRT(1/2. *(( Cspeed^2+vAsqr)+ SQRT( ( Cspeed^2+vAsqr)^2 -4 *Cspeed^2 * vAsqr*(cos(thetaNBn))^2 )))
	;help,fms
;SQRT( Cspeed^2+vAsqr);   ;Fast Magnetosonic Speed
	;fms[where(finite(fms) ne 1)]=0
	datDensityFA.ytitle= 'Fast Magnetosonic Speed (km/s)'
	;lim.ytitle='Fast Magnetosonic "+STRING(10B)+"Speed (km/s)'
	;datDensityFA.Y=fms		
	store_data,'Fast_Magnetosonic_Fine',data={x:xs,y:fms,ytitle:'Fast Magnetosonic Speed (km/s)'},dlim=limV	

	M_fmsVec=VecDiv(vCVec,fms)  ;Mach number should be a scalar, but we'll create a vector version anyway

	M_fmsVec[where(finite(M_fmsVec) eq 0)]=0
	;datVelFA.ytitle= 'Fast Magnetosonic Mach numbers'
	;limV.ytitle= 'Fast Magnetosonic Mach numbers'
	;datVelFA.Y=M_fmsVec
	store_data,'Mach_fms_vec_Fine',data={x:xs,y:M_fmsVec,v:v,ytitle:'FMS Mach number' ,labels:labels,labflag:labflag},dlim=limV

	;Mfms = SQRT(TRANSPOSE(M_fmsVec)#M_fmsVec)
	
	;Mfms=SQRT(TRANSPOSE(M_fmsVec[*,0])#M_fmsVec[*,0]+TRANSPOSE(M_fmsVec[*,1])#M_fmsVec[*,1]+TRANSPOSE(M_fmsVec[*,2])#M_fmsVec[*,2])


;the actual FMS Mach number


	Mfms=fltarr(N);vFAscalar/fms
	for i=0,N-1 do if fms[i] ne 0 then Mfms[i]=vFAscalar[i]/fms[i] else Mfms[i]=0
	;rndm2=Mfms[777]

	;print, rndm-rndm2

	;datDensityFA.Y=Mfms
	datDensityFA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	store_data,'mach_fms_Fine',data = {x:xs,y:Mfms,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"},dlim=limD

	;datDensityFA.Y=Rho


;The normal components of the mach numbers: that is \vec{M}_{fms}\cdot\hat{n}_{SW}
	;get_data,'Shock_Normal_MX1',data=datMX1
	;get_data,'Shock_Normal_MX2',data=datMX2
	get_data,'Shock_Normal_MX3',data=datMX3
	get_data,'Shock_Normal_AVG',data=datAVG
	;get_data,'Shock_Normal_best',data=datbest
	;MX1=datMX1.y
	;MX2=datMX2.y
	MX3=datMX3.y
	nAVG=datAVG.y
	;nbest=datbest.y


	;print,"[numel(M_fmsVec[*,0]),numel(MX3[*,0])]=",[numel(M_fmsVec[*,0]),numel(MX3[*,0])]
	;MfmsMX1=abs(ADJDOT2(M_fmsVec,MX1,ui));DotProduct(M_fmsVec,MX1))
	;MfmsMX2=abs(ADJDOT2(M_fmsVec,MX2,ui));abs(DotProduct(M_fmsVec,MX2))
	MfmsMX3=abs(ADJDOT2(M_fmsVec,MX3,ui));abs(DotProduct(M_fmsVec,MX3)) 
	MfmsAVG=abs(ADJDOT2(M_fmsVec,nAVG,ui));abs(DotProduct(M_fmsVec,MX3)) 
	;Mfmsbest=abs(ADJDOT2(M_fmsVec,nbest,ui));abs(DotProduct(M_fmsVec,MX3)) 
	;MfmsMX1=abs(ADJDOT(M_fmsVec,MX1));DotProduct(M_fmsVec,MX1))
	;MfmsMX2=abs(ADJDOT(M_fmsVec,MX2));abs(DotProduct(M_fmsVec,MX2))
	;MfmsMX3=abs(ADJDOT(M_fmsVec,MX3));abs(DotProduct(M_fmsVec,MX3)) 
	;MfmsMX1[where(finite(MfmsMX1) ne 1)]=0
	;MfmsMX2[where(finite(MfmsMX2) ne 1)]=0
	MfmsMX3[where(finite(MfmsMX3) ne 1)]=0
	;Mfmsbest[where(finite(Mfmsbest) ne 1)]=0
	;MfmsMX1[where(finite(MfmsMX1,/NAN) eq 1)]=0
	;MfmsMX2[where(finite(MfmsMX2,/NAN) eq 1)]=0
	MfmsMX3[where(finite(MfmsMX3,/NAN) eq 1)]=0
	MfmsAVG[where(finite(MfmsAVG,/NAN) eq 1)]=0
	;Mfmsbest[where(finite(Mfmsbest,/NAN) eq 1)]=0
;	MfmsMX2[where(finite(MfmsMX2) ne 1)]=0
;	MfmsMX3[where(finite(MfmsMX3) ne 1)]=0
	GG=where(shocks ne 0,gcount)
	;GG=where((MfmsAVG ne 0) and (finite(MfmsAVG) eq 1))
	;print,"GG=where(MX3 ne 0)=",GG
	;print,"MfmsAVG[GG]=",MfmsAVG[GG]
	;datDensityFA.Y=MfmsMX1
	;datDensityFA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	;store_data,'mach_fms_Fine_MX1',data = {x:xs,y:MfmsMX1,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}

;	datDensityFA.Y=MfmsMX2
	;datDensityFA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	
	;store_data,'mach_fms_Fine_MX2',data ={x:xs,y:MfmsMX2,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}

;	datDensityFA.Y=MfmsMX3
	;datDensityFA.ytitle="Fast Magnetosonic"+STRING(10B)+"Mach Number"
	;limD.Ytitle="Fast Magnetosonic Mach Number"
	;print,"numel(MfmsMX3)=",numel(MfmsMX3)
	store_data,'mach_fms_Fine_MX3',data = {x:xs,y:MfmsMX3,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}
	store_data,'mach_fms_Fine_AVG',data = {x:xs,y:MfmsAVG,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}
	;store_data,'mach_fms_Fine_best',data = {x:xs,y:Mfmsbest,ytitle:"Fast Magnetosonic"+STRING(10B)+"Mach Number"}



RETURN

	;help,nn
	;help,xs
	get_data,'Shock_Angle_AVG',data=datAAVG
	ANGLE=datAAVG.y
	
	get_data,"downstream_indices",data=datDI

	di=datDI.y
		
	foreach el, GG do begin
		;print,"==================="
		st=min(ui[el,*])
		en=max(ui[el,*])
		dst=min(di[el,*])
		den=max(di[el,*])

		sp=mean(ScalarPressureFA[st:en])
		nup=mean(nn[st:en],/nan)
		tup=mean(TT[st:en],/nan)	
		vup=VECMEAN(vCVec,st,en)


		nT=nup*tup
		
		mnup=mean(rho_p[st:en])
		rhoup=mean(RhoSI[st:en])
		Csup=mean(Cspeed[st:en])
		Csupm=SQRT(sp/mnup)*!const.c/1000
		vAsqrup=mean(vAsqr[st:en])
		Bup=VECMEAN(MagVec,st,en)
		Bdown=VECMEAN(MagVec,dst,den)
		VAup=VECMEAN(vACVec,st,en)
		VAupm=[Bup[0]/SQRT(!const.MU0 * rhoup)/(1000.0*10.0^9),Bup[1]/SQRT(!const.MU0 * rhoup)/(1000.0*10.0^9),Bup[2]/SQRT(!const.MU0 * rhoup)/(1000.0*10.0^9)]
		vAsqrupM=VAupm[0]^2+VAupm[1]^2+VAupm[2]^2

		vAsqrup=mean(vAsqr[st:en])
		fmsup=mean(fms[st:en])
		fmsM=SQRT(Csupm^2+vAsqrupm)

		MfmsVup=VECMEAN(M_fmsVec,st,en)
		MfmsVupM=[vup[0]/fmsM,vup[1]/fmsM,vup[2]/fmsM]
		Nav=nAVG[el,*]
		Mfmsup=MfmsAVG[el]
		MfmsM=abs(total(Nav*MfmsVupM))


		bta=betas[el]
		btam=nT *!const.e *10.0^6/(  (norm(Bup *10.0^(-9)))^2 /(2*!const.mu0 ) )

		aup=90-ABS(90-ANGLE[el]*180/!PI)
		th=90-abs(90-acos(TOTAL(Bup*Nav)/SQRT(TOTAL(Bup^2))) *180/!PI)

		BdBu=norm(Bdown)/norm(Bup)
		RH_parameters,Mfmsup,th,bta,a,b,c,d,yy,delta
		B2B1_RH=SQRT((b+c*delta)/(b+c))
		;print,el
		;print,"upstream Pressure [eV/m^3],upstream Temp [eV],upstream Num density [1/cm^3], ,mass density [eV/cm^3], mass density [kg/m^3],proton pressure"
		;print,sp,tup,nup,mnup,nT
		;print,"---------------"
		;print,"upstream B [nT]=", Bup
		;print,"N_AVG=",Nav
		;print,"upstream velocity [km]=",vup
		;print,"---------------"
		;print,"beta,measured beta=nT *!const.e *10.0^6/((Bup *10.0^(-9))^2 /(2*!const.mu0 ) )"
		;print,bta,btam,fracdiff(bta,btam)
		;print,"---------------"

		;print,"upstream Cs [km]=",Csup
		;print,"measured upstream Cs= SQRT(Pressure/rho) *c /1000=",Csupm
		;print,"---------------"
		;print,"upstream V_A [km]=",VAup
		;print,"measured upstream V_A=Bvec/sqrt(mu0* rho) / (1000* 10.0^9)=",VAupm
		;print,"---------------"
		;print,"upstream V_A^2 [km]=",vAsqrup
		;print,"measured upstream V_A^2="+strjoin(strtrim(VAupm^2),",")+"=",vAsqrupM
		;print,"---------------"
		;print,"fms up=",fmsup
		;print,"measured fms up=",fmsM
		;print,"---------------"
		;print,"MfmsV up=",MfmsVup
		;print,"measured MfmsV up=",MfmsVupM
		;print,"---------------"
		;print,"Mfms=",Mfmsup
		;print,"measured Mfms=",MfmsM
		;print,"fracdiff(Mfms,MfmsM)=",fracdiff(Mfmsup,MfmsM)
		;print,"Previously Calculated Angle=",aup
		;print,"just now calculated angle=",th
		;print,"fracdiff(aup,th)=",fracdiff(aup,th)

		;print,"----------------"
		;print,"measured Bd/Bu=",BdBu
		;print,"RH calculated Bd/Bu=",B2B1_RH

	endforeach
END



