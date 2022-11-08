function shocknorm, a , b, c
	sn=b*0.0
;					      (B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;					    |(B_d x B_u)x(ΔB)|

;					       (B_u x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx1 =± -----------------
;					     |(B_u x ΔV)x(ΔB)|

;					       (B_d x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx2 =± -----------------
;					     |(B_d x ΔV)x(ΔB)|

;					       (ΔB x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx3 =± -----------------
;					     |(ΔB x ΔV)x(ΔB)|


;	;;print,"bd=",bd
;	;;print,"bu=",bu
	
	nu= cp(cp(a,b),c)
	;;;print,nu
	mag=sqrt(nu[0]^2+ nu[1]^2+nu[2]^2)
	;;;print,mag
	for el=0,2 do nu[el]=nu[el]/mag
	;sn=vecdiv(nu,norm2(nu));vecmag(nu));nu/norm(nu);vecdiv(nu,norm2(nu));vecmag(nu))
	;;;print,"nu=",nu
	;;;;print,"norm(nu)=",norm(nu)
	;;;print,"Sqrt(Total(nu^2))=",Sqrt(Total(nu^2))
	return, nu;sn
end

function paramrecalc,dat

	N_e=dat.N_e/.7
	str_element,dat,'N_e',N_e,/add
	N_ion=dat.N_p/.7
	str_element,dat,'N_p',N_ion,/add
	T_e=dat.T_e
	Tproton=dat.T_ion
	N=numel(N_e)
 	thNB=!pi/2-abs(!pi/2-dat.ANGLE)
 	str_element,dat,'th0',dat.ANGLE,/add
 	t=dat.t
 	N_SN0=dat.N_SN
 	str_element,dat,'N_SN0',N_SN0,/add
 	str_element,dat,'shock0Acc0',dat.shock0Acc,/add
 	Buvec=dat.Buvec
 	Bdvec=dat.Bdvec
 	Vdvec=dat.Vdvec
 	Vuvec=dat.VUVEC_Fine
	POS=dat.pos
	N_SN=N_SN0
	S0n=dat.N_conic
	ANGLE=fltarr(N)
	shock0acc=ANGLE
	for j=0,N-1 do begin
		;print,time_string(t[j])
		Bu=transpose(Buvec[j,*])
	;	;print,"Bu=",Bu
		Bd=transpose(Bdvec[j,*])
	;	;print,"Bd=",Bd
		Vu=transpose(Vuvec[j,*])
	;	;print,"Vu=",Vu
		Vd=transpose(Vdvec[j,*])
	;	;print,"Vd=",Vd
		dV=Vd-vu
	;	;print,"dV=",dv
		db=bd-bu
	;	;print,"dB=",dB
	;	;print,''
		;SN=shocknorm(BD,BU,dB)
		SN_MX1 =shocknorm(BU,dv,dB)
		SN_MX2=shocknorm(BD,dv,dB)
		SN_MX3=shocknorm(dB,dv,dB)
		;VdNmc=dotproduct(SN,Vd)
		
	;	;print,"N_MC(BD,BU,dB)=",SN
				;n_SN[i,*]=
		

		VdN1=dotproduct(SN_MX1,Vd)
	;	;print,"N_MX1(BU,dV,dB)=",SN_MX1
		
		VdN2=dotproduct(SN_MX2,Vd)
	;	;print,"N_MX2(BD,dV,dB)=",SN_MX2

		VdN3=dotproduct(SN_MX3,Vd)
	;	;print,"N_MX3(BD,dV,dB)=",SN_MX3	
	;	;print,''
	;	;print,"POS=",Pos[j,*]
	;	;print,''
		;n_SNcyl=carttocyl_instant(SN,Pos[j,*])
	;	;print,"N_MC_CYL=",n_SNcyl
		n_SN1cyl=carttocyl_instant(SN_MX1,Pos[j,*])
	;	;print,"N1_CYL=",n_SN1cyl
		n_SN2cyl=carttocyl_instant(SN_MX2,Pos[j,*])
	;	;print,"N2_CYL=",n_SN2cyl
		n_SN3cyl=carttocyl_instant(SN_MX3,Pos[j,*])
	;	;print,"N3_CYL=",n_SN3cyl
	;	;print,''
	;	;print,'N_conic=',S0n[j,*]
		;n_SN_Acc=vecDotProduct(n_SNcyl,S0n[j,*])
	;	;print,'Nmc.Nconic=',n_SN_Acc
		n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,S0n[j,*])
	;	;print,'Nmx1.Nconic=',n_SN_MX1_Acc
		n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,S0n[j,*])
	;	;print,'Nmx2.Nconic=',n_SN_MX2_Acc
		n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,S0n[j,*])
	;	;print,'Nmx3.Nconic=',n_SN_MX3_Acc
		;if n_SN_Acc le 0 then begin
	;		;print,'MC reversed'
			;ntemp=-1*SN
	;		;print,'NtempMC=',ntemp
			;n_SNcyltemp=carttocyl_instant(Ntemp,Pos[j,*])
	;		;print,'Ncyl=',n_SNcyltemp
			;acctemp=vecDotProduct(n_SNcyltemp,S0n[j,*])
	;		;print,'Ntemp.Nconic=',acctemp
			;if acctemp gt n_SN_ACC then begin
	;			;print,'updating to new'
				;SN=ntemp
				;n_SNcyl=n_SNcyltemp
				;n_SN_ACC=acctemp
			
			;endif
		;endif
		if n_SN_MX1_Acc le 0 then begin
	;		;print,'MX1 reversed'
			ntemp=-1*SN_MX1
	;		;print,'N1temp=',ntemp
			n_SNcyltemp=carttocyl_instant(Ntemp,Pos[j,*])
	;		;print,'N1cyl=',n_SNcyltemp
			acctemp=vecDotProduct(n_SNcyltemp,S0n[j,*])
	;		;print,'N1temp.Nconic=',acctemp
			if acctemp gt n_SN_MX1_ACC then begin
	;			;print,'updating to new'
				SN_MX1=ntemp
				n_SN1cyl=n_SNcyltemp
				n_SN_MX1_ACC=acctemp
			
			endif
		endif
		if n_SN_MX2_Acc le 0 then begin
	;		;print,'MX2 reversed'
			ntemp=-1*SN_MX2
	;		;print,'N2temp=',ntemp
			n_SNcyltemp=carttocyl_instant(Ntemp,Pos[j,*])
	;		;print,'N2cyl=',n_SNcyltemp
			acctemp=vecDotProduct(n_SNcyltemp,S0n[j,*])
	;		;print,'N2temp.Nconic=',acctemp
			if acctemp gt n_SN_MX2_ACC then begin
	;			;print,'updating to new'
				SN_MX2=ntemp
				n_SN2cyl=n_SNcyltemp
				n_SN_MX2_ACC=acctemp
			
			endif
		endif
		if n_SN_MX3_Acc le 0 then begin
	;		;print,'MX3 reversed'
			ntemp=-1*SN_MX3
	;		;print,'N3temp=',ntemp
			n_SNcyltemp=carttocyl_instant(Ntemp,Pos[j,*])
	;		;print,'N3cyl=',n_SNcyltemp
			acctemp=vecDotProduct(n_SNcyltemp,S0n[j,*])
	;		;print,'N3temp.Nconic=',acctemp
			if acctemp gt n_SN_MX3_ACC then begin
	;			;print,'updating to new'
				SN_MX3=ntemp
				n_SN3cyl=n_SNcyltemp
				n_SN_MX3_ACC=acctemp
			endif
		endif
		
		;VdNmc=dotproduct(SN,Vd)
		
	;	;print,"N_MC(BD,BU,dB)=",SN
				;n_SN[i,*]=
		

		VdN1=dotproduct(SN_MX1,Vd)
	;	;print,"N_MX1(BU,dV,dB)=",SN_MX1
		
		VdN2=dotproduct(SN_MX2,Vd)
	;	;print,"N_MX2(BD,dV,dB)=",SN_MX2

		VdN3=dotproduct(SN_MX3,Vd)
		;VuNmc=dotproduct(SN,Vu)
		
	;	;print,"N_MC(BD,BU,dB)=",SN
				;n_SN[i,*]=
		

		VuN1=dotproduct(SN_MX1,Vu)
	;	;print,"N_MX1(BU,dV,dB)=",SN_MX1
		
		VuN2=dotproduct(SN_MX2,Vu)
	;	;print,"N_MX2(BD,dV,dB)=",SN_MX2

		VuN3=dotproduct(SN_MX3,Vu)
		;VdNs=[VdNmc,VdN1,VdN2,VdN3] 
		;VuNs=[VuNmc,VuN1,VuN2,VuN3] 
		VdNs=[VdN1,VdN2,VdN3] 
		VuNs=[VuN1,VuN2,VuN3] 
		;print,'[VdNmc,VdN1,VdN2,VdN3]:',[VdNmc,VdN1,VdN2,VdN3] 
		;print,'[VuNmc,VuN1,VuN2,VuN3]:',[VuNmc,VuN1,VuN2,VuN3] 
		ww2=where(finite(VdNs) eq 1,wcount2)
		;if wcount2 eq 0 then ww2=[0,1,2,3]
		;print,'ww2=',ww2
		ww1=where(abs(VdNs/VuNs) lt 1 and VdNs lt 10,wcount1,ncomp=nwc1,complement=nw1) 
		;print,'VdNs/VuNs:',VdNs/VuNs
		;print,'ww1=',ww1
		if wcount1 eq 0 then ww1=ww2
		wcount01=wcount1
		ww=intersect(ww1,ww2)
		;print,'ww=',ww
		wcount=N_elements(ww)
		;print,[SN[0],SN_MX1[0],SN_MX2[0],SN_MX3[0]]
;		N1avg=mean(([SN[0],SN_MX1[0],SN_MX2[0],SN_MX3[0]])[ww],/nan)
		N1avg=mean(([SN_MX1[0],SN_MX2[0],SN_MX3[0]]),/nan)
		;print,''
		;print,'Navg1=',N1avg
		;print,[SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]]
		;N2avg=mean(([SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]])[ww],/nan)		
		N2avg=mean(([SN_MX1[1],SN_MX2[1],SN_MX3[1]]),/nan)		
		;print,'Navg2=',N2avg
		;print,[SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]]
;		N3avg=mean(([SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]])[ww],/nan)
		N3avg=mean(([SN_MX1[2],SN_MX2[2],SN_MX3[2]]),/nan)
		;print,'Navg3=',N3avg
		Navg=[N1avg,N2avg,N3avg]
		;print,''
		;print,"unnormalized Navg=",Navg
		Nnorm=norm(Navg)
		for k=0,2 do Navg[k]/=Nnorm
		;print,"normalized Navg=",Navg
		;print,'norm(Navg)=',norm(Navg)
		nw=intersect(nw1,ww2)
		N0=NAVG
		
		n_AVGcyl=carttocyl_instant(NAVG,Pos[j,*])
		dop=dotproduct(N_AVGcyl,S0n[j,*])
		;dopmc=dotproduct(SN,NAVG)
		use0=total(ww eq 0)
		;if 0 and dop lt .6 then begin
		;print,''
			;print,'try without N_MC'
			;N1avg=mean([SN_MX1[0],SN_MX2[0],SN_MX3[0]],/nan)
		;print,'Navg1=',N1avg
		;print,[SN_MX1[1],SN_MX2[1],SN_MX3[1]]
			;N2avg=mean([SN_MX1[1],SN_MX2[1],SN_MX3[1]],/nan)		
		;print,'Navg2=',N2avg
		;print,[SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]]
			;N3avg=mean([SN_MX1[2],SN_MX2[2],SN_MX3[2]],/nan)
		;print,'Navg3=',N3avg
			;Navg2=[N1avg,N2avg,N3avg]
			;print,''
		;print,"unnormalized Navg=",Navg
			;Nnorm=norm(Navg2)
			;for k=0,2 do Navg2[k]/=Nnorm
			;print,"normalized Navg=",Navg
		;print,'norm(Navg)=',norm(Navg)
			;doprange=[dotproduct(NAVG2,SN_MX1),dotproduct(NAVG2,SN_MX2),dotproduct(NAVG2,SN_MX3)]
			
			;if dotproduct(SN,NAVG2) lt min([min(doprange)-5*stddev(doprange),.86]) then begin
			;print,'N_MC diverges'
			;VdNs=[VdN1,VdN2,VdN3] 
			;VuNs=[VuN1,VuN2,VuN3] 
			;ww2=where(finite(VdNs) eq 1,wcount2)
			;if wcount2 eq 0 then ww2=[1,2,3]
			;ww1=where(VdNs/VuNs gt 0 or VdNs lt  10,wcount1,ncomp=nwc1,complement=nw1) 
		;	if wcount1 eq 0 then ww1=ww2
		
		;	ww=intersect(ww1,ww2)

		;	wcount=N_elements(ww)
	;	;;print,[SN[0],SN_MX1[0],SN_MX2[0],SN_MX3[0]]
		;	N1avg=mean(([SN[0],SN_MX1[0],SN_MX2[0],SN_MX3[0]])[ww],/nan)
	;	;print,'Navg1=',N1avg
	;	;print,[SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]]
		;	N2avg=mean(([SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]])[ww],/nan)		
	;	;print,'Navg2=',N2avg
	;	;print,[SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]]
		;	N3avg=mean(([SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]])[ww],/nan)
	;	;print,'Navg2=',N3avg
		;	Navg=[N1avg,N2avg,N3avg]
	;	;print,"unnormalized Navg=",Navg
		;	Nnorm=norm(Navg)
		;	for k=0,2 do Navg[k]/=Nnorm
		;	nw=intersect(nw1,ww2)
		;	endif
		;	use0=0
		;endif
		
		;use0=0
		if 0 and nwc1 mod (3+use0) gt 0 and nw[0] ne -1 then begin
			

			foreach el,nw do begin
				
				;if el eq 0 and not use0 then continue
				;if total(el eq [0,1,2,3]) eq 0 then continue
				if total(el eq [0,1,2]) eq 0 then continue
				;;print,'el=', el
				case el of
;					0: Ntest=SN
;					1: Ntest=SN_MX1
;					2: Ntest=SN_MX2
;					3: Ntest=SN_MX3
					0: Ntest=SN_MX1
					1: Ntest=SN_MX2
					2: Ntest=SN_MX3
					;else: continue
				endcase
				if (dotproduct(Ntest,N0) gt .86 and -1*dotproduct(Ntest,Vd)/sign(dotproduct(Ntest,Vu)) lt 10) or dotproduct(Ntest,N0) gt .96 then begin
					ww=[el,ww]
					ww = ww[UNIQ(ww, SORT(ww))]	
						;N1avg=mean(([SN[0],SN_MX1[0],SN_MX2[0],SN_MX3[0]])[ww],/nan)
				N1avg=mean(([SN_MX1[0],SN_MX2[0],SN_MX3[0]])[ww],/nan)
	;	;print,'Navg1=',N1avg
	;	;print,[SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]]
				N2avg=mean(([SN_MX1[1],SN_MX2[1],SN_MX3[1]])[ww],/nan)	
					;N2avg=mean(([SN[1],SN_MX1[1],SN_MX2[1],SN_MX3[1]])[ww],/nan)		
	;	;print,'Navg2=',N2avg
	;	;print,[SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]]
				N3avg=mean(([SN_MX1[2],SN_MX2[2],SN_MX3[2]])[ww],/nan)
				;N3avg=mean(([SN[2],SN_MX1[2],SN_MX2[2],SN_MX3[2]])[ww],/nan)
	;	;print,'Navg2=',N3avg
				Navg=[N1avg,N2avg,N3avg]
	;	;print,"unnormalized Navg=",Navg
				Nnorm=norm(Navg)
				for k=0,2 do Navg[k]/=Nnorm
				endif
			
			
			endforeach
		
		
		endif
		
		VDN=dotproduct(NAVG,Vd)
		VUN=dotproduct(NAVG,Vu)
		if VDN/VUN le 0 or (wcount01 mod 4 gt 0) then begin
			openU,1,'Documents/whereNegVdN.txt',/APPEND
			tx=''
			printf,1,time_string(t[j])
			case 1*(VDN/VUN le 0) + 10*(wcount01 mod 4 gt 0) of
				1: tx='VDNavg/VUNavg le 0'
				10: tx='Not all methods give VDN/VUN ge 0'
				11: tx='VDNavg/VUNavg le 0 and Not all methods give VDN/VUN ge 0'
			endcase
					
			printf,1,tx
			

			printf,1,'VdN=',VdN
			printf,1,'VuN=',VuN
			printf,1,'N0=['+strtrim(N0[0],2)+','+strtrim(N0[1],2)+','+strtrim(N0[2],2)+']'
			printf,1,'NAVG=['+strtrim(NAVG[0],2)+','+strtrim(NAVG[1],2)+','+strtrim(NAVG[2],2)+']'
			printf,1,'VD=['+strtrim(VD[0],2)+','+strtrim(VD[1],2)+','+strtrim(VD[2],2)+']'
		
			;printf,1,'VDN_mc=',VDNmc
			printf,1,'VDN1=',VDN1
			printf,1,'VDN2=',VDN2
			printf,1,'VDN3=',VDN3
			;printf,1,'Nmc=['+strtrim(SN[0],2)+','+strtrim(SN[1],2)+','+strtrim(SN[2],2)+']'
			printf,1,'NMX1=['+strtrim(SN_MX1[0],2)+','+strtrim(SN_MX1[1],2)+','+strtrim(SN_MX1[2],2)+']'
			printf,1,'NMX2=['+strtrim(SN_MX2[0],2)+','+strtrim(SN_MX2[1],2)+','+strtrim(SN_MX2[2],2)+']'
			printf,1,'NMX3=['+strtrim(SN_MX3[0],2)+','+strtrim(SN_MX3[1],2)+','+strtrim(SN_MX3[2],2)+']'
			printf,1,'===='
			close,1			
		endif
		;;print,"normalized Navg=",Navg
		;;print,'norm(Navg)=',norm(Navg)
		ANGLE[j]=AngleCalc(Navg,Bu)
		;;print,"thBNavg=",ANGLE[j]*180/!pi
		for k=0,2 do N_SN[j,k]=Navg[k]
		n_AVGcyl=carttocyl_instant(NAVG,Pos[j,*])
		shock0acc[j]=dotproduct(N_AVGcyl,S0n[j,*])
		;;print,'shock0acc=',shock0acc[j]
		;print,'~~~~~~~~~~~``
	endfor 	
 	dat.angle=angle
 	dat.N_SN=N_SN
 	 thNB=!pi/2-abs(!pi/2-dat.ANGLE)
 	dat.shock0Acc=shock0acc
 	
 	
 	
 	
 	
	;N_SN=dat.N_SN
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
		;if (i mod 10) eq 0 then ;print,time_string(t[i])
		Mcrit[i]=calccritmachnumber(thNB[i],bta[i])
		;;print,'[Mcrit0,Mcrit]=',[Mcrit[i]]
		;;print,'[beta0,beta,betas]=',[bta0[i],bta[i],betas[i]]
	endfor
	str_element,dat,'crits',Mcrit,/add


	return,dat

end
