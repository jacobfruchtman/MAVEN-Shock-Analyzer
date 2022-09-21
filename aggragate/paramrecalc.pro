function paramrecalc,dat

	N_e=dat.N_e/.7
	str_element,dat,'N_e',N_e,/add
	N_ion=dat.N_p/.7
	str_element,dat,'N_p',N_ion,/add
	T_e=dat.T_e
	Tproton=dat.T_ion
 	thNB=!pi/2-abs(!pi/2-dat.ANGLE)
	N_SN=dat.N_SN
	Vuvec=dat.VUVEC_FINE
	ThNV=vecanglecalc(Vuvec,N_SN,dot=VN)
	str_element,dat,'FLOWANGLE',ThNV,/add
	upM=dat.downMeasured
	downM=dat.upMeasured; yes. I know. Need to fix the mix up later

	Pe=N_e*T_e
	Pp=N_ion*Tproton
	g_e=1
	g_i=5./3
	beta_p=abs(Pp)*!const.e*10.0^(6.)/((upM *10.0^(-9.))^2./(2* !const.mu0))
	beta_e=abs(Pe)*!const.e*10.0^(6.)/((upM *10.0^(-9.))^2./(2* !const.mu0))
	bta=beta_p+beta_e

	N=numel(N_e)

	BETA_ION0=dat.BETA_ION
	str_element,dat,'BETA_ION0',BETA_ION0,/add
	str_element,dat,'BETA_ION',beta_p,/add
	bta0=dat.Betas
	str_element,dat,'beta0',bta0,/add
	str_element,dat,'betas',bta,/add
	;store_data,'beta0',data=datBeta

	;store_data,'beta',data=datBeta

	;get_data,'beta_proton',data=datBeta
	;store_data,'beta_proton0',data=datBeta
	
	;store_data,'beta_proton',data=datBeta

	;get_data,'beta_electron',data=datBeta
	;store_data,'beta_electron0',data=datBeta

	;store_data,'beta_electron',data=datBeta

	str_element,dat,'crits',crits0
	str_element,dat,'crits0',crits0,/add



	m_p=938.0*10.0^6

	m_el=.511*10.0^6

	g_e=1.
	g_i=5./3.

	rho_p=m_p*N_ion
	;rndm=rho_p[777]
	rho_e=m_el*N_e
	Cs= SQRT((g_i*Tproton +g_e*T_e)/m_p   )*!const.c/1000.;SQRT(protonPressure/rho_p)*!const.c;SQRT(ScalarPressureFA/rho_p)*!const.c
	;get_data,'SoundSpeed',data=datCs
	;store_data,'SoundSpeed0',data=datCs
	str_element,dat,'CS',Cs,/add
	
	;store_data,'SoundSpeed',data=datCs

	Rho=!const.mp* 10.0^6 *N_ion

	RhoSI= !const.mp* 10.0^6 *N_ion

	vACVe=upM/ SQRT(!const.MU0 * RhoSI)
	VA=vACVe /(1000.0*10.0^9)
	str_element,dat,'ALFVEN',VA,/add



	;M_A=VN/VA
	;get_data,'MachAlfven',data=datMA
	;store_data,'MachAlfven0',data=datMA
	;datMA.y=M_A
	
	;store_data,'MachAlfven',data=datMA

	fms=SQRT(1/2. *(( Cs^2+VA^2)+ SQRT( ( Cs^2+VA^2)^2 -4 *Cs^2 * VA^2 *(cos(thNB))^2 )))
	Mfms=abs(VN)/fms

	str_element,dat,'Mfms',Mfms,/add
	
	Mcrit=fltarr(N)
	betas=bta
	thetaNBn=thNB
	for i=0,N-1 do begin 
		;if (i mod 10) eq 0 then print,time_string(t[i])
		Mcrit[i]=calccritmachnumber(thNB[i],bta[i])
		;print,'[Mcrit0,Mcrit]=',[Mcrit[i]]
		;print,'[beta0,beta,betas]=',[bta0[i],bta[i],betas[i]]
	endfor
	str_element,dat,'crits',Mcrit,/add


	return,dat

end
