pro aggraderive

	get_data,"N_e",data=datNe
	N_e=datNe.y
	get_data,"N_ion",data=datNi
	N_ion=datNi.y
	num=numel(N_ion)
	get_data,'Tproton',data=datTp
	Tproton=datTp.y
	get_data,'Telectron',data=datTe
	T_e=datTe.y
	get_data,"ThetaNBn",data=datThNB
	thNB=datThNB.y *!pi/180
	get_data,"Mcrit",data=datMcrit
	Mcrit=datMcrit.y
	get_data,"ThetaNVn2",data=datThNV
	ThNV=datThNV.y*!pi/180
	get_data,'downM',data=datdownM
	downM=datdownM.y
	get_data,'upM',data=datupM
	upM=datupM.y
	get_data,'Vumag',data=datV
	V1=datV.y
	VN=abs(V1)*cos(ThNV)
	Pe=N_e*T_e
	Pp=N_ion*Tproton
	g_e=1
	g_i=5./3
	beta_p=abs(Pp)*!const.e*10.0^(6.)/((upM *10.0^(-9.))^2./(2* !const.mu0))
	beta_e=abs(Pe)*!const.e*10.0^(6.)/((upM *10.0^(-9.))^2./(2* !const.mu0))
	bta=beta_p+beta_e
	t=datV.x
	N=numel(t)
	if 0 then begin

	get_data,'beta',data=datBeta
	bta0=datBeta.y
	store_data,'beta0',data=datBeta
	datBeta.y=bta
	store_data,'beta',data=datBeta

	get_data,'beta_proton',data=datBeta
	store_data,'beta_proton0',data=datBeta
	datBeta.y=beta_p
	store_data,'beta_proton',data=datBeta

	get_data,'beta_electron',data=datBeta
	store_data,'beta_electron0',data=datBeta
	datBeta.y=beta_e
	store_data,'beta_electron',data=datBeta

	Mcrit0=Mcrit


	m_p=938.0*10.0^6

	m_el=.511*10.0^6

	g_e=1.
	g_i=5./3.

	rho_p=m_p*N_ion
	;rndm=rho_p[777]
	rho_e=m_el*N_e
	Cs= SQRT((g_i*Tproton +g_e*T_e)/m_p   )*!const.c/1000.;SQRT(protonPressure/rho_p)*!const.c;SQRT(ScalarPressureFA/rho_p)*!const.c
	get_data,'SoundSpeed',data=datCs
	store_data,'SoundSpeed0',data=datCs
	datCs.y=Cs
	
	store_data,'SoundSpeed',data=datCs

	Rho=!const.mp* 10.0^6 *N_ion

	RhoSI= !const.mp* 10.0^6 *N_ion

	vACVe=upM/ SQRT(!const.MU0 * RhoSI)
	VA=vACVe /(1000.0*10.0^9)
	get_data,'AlfCalc',data=datVA
	store_data,'AlfCalc0',data=datVA
	datVA.y=VA
	
	store_data,'AlfCalc',data=datVA

	M_A=abs(V1)*cos(ThNV)/VA
	get_data,'MachAlfven',data=datMA
	store_data,'MachAlfven0',data=datMA
	datMA.y=M_A
	
	store_data,'MachAlfven',data=datMA

	fms=SQRT(1/2. *(( Cs^2+VA^2)+ SQRT( ( Cs^2+VA^2)^2 -4 *Cs^2 * VA^2 *(cos(thNB))^2 )))
	Mfms=VN/fms
	FM=Mfms/Mcrit
	get_data,'Mfms',data=datMfms
	Mfms0=datMfms.y
	store_data,'Mfms0',data=datMfms
	datMfms.y=Mfms
	store_data,'Mfms',data=datMfms

	get_data,'fms',data=datfms
	store_data,'fms0',data=datfms
	datfms.y=fms
	store_data,'fms',data=datfms

	
	get_data,'FM',data=datFM
	store_data,'FM0',data=datFM
	datFM.y=FM
	store_data,'FM',data=datFM
	Mcrit=fltarr(num)
	betas=bta
	thetaNBn=thNB
	for i=0,num-1 do begin 
		if (i mod 10) eq 0 then print,time_string(t[i])
		Mcrit[i]=calccritmachnumber(thNB[i],bta[i])
		print,'[Mcrit0,Mcrit]=',[Mcrit0[i],Mcrit[i]]
		print,'[Mfms0,Mfms]=',[Mfms0[i],Mfms[i]]
		print,'[beta0,beta,betas]=',[bta0[i],bta[i],betas[i]]
	endfor
	help,bta
	store_data,'Mcrit0',data=datMcrit
	datMcrit.y=Mcrit
	store_data,'Mcrit',data=datMcrit
	ms1=sqrt(Cs^2+VA^2)
	Mflow=V1/ms1
	get_data,'Mflow',data=datMflow
	store_data,'Mflow0',data=datMflow
	datMflow.y=Mflow
	store_data,'Mflow',data=datMflow

	N=num
	endif
	B2B1_RH_ion=fltarr(N);B2B1_Fit*0.0
	densityjump_RH_ion=fltarr(N)
	beta2_ion=fltarr(N)
	
	get_data,'Mfms',data=datMfms
	Mfms=datMfms.y
	
	B2B1_RH=fltarr(N);B2B1_Fit*0.0
	densityjump=fltarr(N)
	beta2=fltarr(N)
	for i=0,N-1 do begin


			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,Mfms[i],ThNB[i],bta[i],a,b,c,d,yy,delta
			densityjump[i]=1/yy
			B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
			beta2[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

			RH_parameters,Mfms[i],ThNB[i],beta_p[i],a,b,c,d,yy,delta
			densityjump_RH_ion[i]=1/yy
			B2B1_RH_ion[i]=SQRT((b+c*delta)/(b+c))
			beta2_ion[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)
	endfor
	help,bta
	betajump=beta2/bta
	get_data,'B2B1_RH',data=datB2B1_RH
	store_data,'B2B1_RH0',data=datB2B1_RH
	datB2B1_RH.y=B2B1_RH
	store_data,'B2B1_RH',data=datB2B1_RH

	get_data,'densityJump',data=datdensityjump
	store_data,'densityJump0',data=datdensityjump
	datdensityjump.y=densityjump
	store_data,'densityJump',data=datdensityjump

	get_data,'betajump',data=datbetajump
	store_data,'betajump0',data=datbetajump
	datbetajump.y=betajump
	store_data,'betajump',data=datbetajump

	get_data,'beta2',data=datbeta2
	store_data,'beta20',data=datbeta2
	datbeta2.y=beta2
	store_data,'beta2',data=datbeta2
	;endif
	N=num

	
	get_data,'B2B1_RH',data=datB2B1_RH
	B2B1_RH=datB2B1_RH.y

	betajump_ion=beta2_ion/beta_p
	get_data,'B2B1_RH_ion',data=datB2B1_RH_ion
	
	store_data,'B2B1_RH_ion0',data=datB2B1_RH_ion
	datB2B1_RH_ion.y=B2B1_RH_ion
	store_data,'B2B1_RH_ion',data=datB2B1_RH_ion

	get_data,'beta2_ion',data=datbeta2_ion
	store_data,'beta2_ion0',data=datbeta2_ion
	datbeta2_ion.y=beta2_ion
	store_data,'beta2_ion',data=datbeta2_ion

	get_data,'densityJump_ion',data=datdensityjump_ion
	store_data,'densityJump_ion0',data=datdensityjump_ion
	datdensityjump_ion.y=densityjump_RH_ion
	store_data,'densityJump_ion',data=datdensityjump_ion
	get_data,'betajump_ion',data=datbetajump_ion
	store_data,'betajump_ion0',data=datbetajump_ion
	datbetajump_ion.y=betajump_ion
	store_data,'betajump_ion',data=datbetajump_ion


	get_data,'B2B1fit',data=datB2B1
	B2B1_Fit=datB2B1.y
	get_data,'B2B1M',data=datB2B1M
	B2B1_Measure=datB2B1M.y

	B2B1fdiff=200*abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit+B2B1_RH)
	B2B1Mdiff=200*abs(B2B1_Measure-B2B1_RH)/abs(B2B1_Measure+B2B1_RH)
	B2B1FMdiff=200*abs(B2B1_Measure-B2B1_Fit)/abs(B2B1_Measure+B2B1_Fit)

	RH_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_RH)
	Fit_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit)

	B2B1fdiff_i=200*abs(B2B1_Fit-B2B1_RH_ion)/abs(B2B1_Fit+B2B1_RH_ion)
	B2B1Mdiff_i=200*abs(B2B1_Measure-B2B1_RH_ion)/abs(B2B1_Measure+B2B1_RH_ion)

	RH_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_RH)
	Fit_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit)

	RH_error_i=abs(B2B1_Fit-B2B1_RH_ion)/abs(B2B1_RH_ion)
	Fit_error_i=abs(B2B1_Fit-B2B1_RH_ion)/abs(B2B1_Fit)


	get_data,'B2B1fdiff',data=datB2B1fdiff
	store_data,'B2B1fdiff0',data=datB2B1fdiff
	datB2B1fdiff.y=B2B1fdiff
	store_data,'B2B1fdiff',data=datB2B1fdiff
	get_data,'B2B1Mdiff',data=datB2B1Mdiff
	store_data,'B2B1Mdiff0',data=datB2B1Mdiff
	datB2B1Mdiff.y=B2B1Mdiff
	store_data,'B2B1Mdiff',data=datB2B1Mdiff
	get_data,'RH_error',data=datRH_error
	store_data,'RH_error0',data=datRH_error
	datRH_error.y=RH_error
	store_data,'RH_error',data=datRH_error
	get_data,'Fit_error',data=datFit_error
	store_data,'Fit_error0',data=datFit_error
	datFit_error.y=Fit_error
	store_data,'Fit_error',data=datFit_error
	get_data,'B2B1fdiff_i',data=datB2B1fdiff_i
	store_data,'B2B1fdiff_i0',data=datB2B1fdiff_i
	datB2B1fdiff_i.y=B2B1fdiff_i
	store_data,'B2B1fdiff_i',data=datB2B1fdiff_i
	get_data,'B2B1Mdiff_i',data=datB2B1Mdiff_i
	store_data,'B2B1Mdiff_i0',data=datB2B1Mdiff_i
	datB2B1Mdiff_i.y=B2B1Mdiff_i
	store_data,'B2B1Mdiff_i',data=datB2B1Mdiff_i
	get_data,'RH_error_i',data=datRH_error_i
	store_data,'RH_error_i0',data=datRH_error_i
	datRH_error_i.y=RH_error_i
	store_data,'RH_error_i',data=datRH_error_i
	get_data,'Fit_error_i',data=datFit_error_i
	store_data,'Fit_error_i0',data=datFit_error_i
	datFit_error_i.y=Fit_error_i
	store_data,'Fit_error_i',data=datFit_error_i

end
