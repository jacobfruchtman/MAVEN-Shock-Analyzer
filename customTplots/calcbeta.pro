pro calcbeta,iononly=iononly

	;get_data,"mvn_swifs_velocity_MAVEN_MSO_interpolated",data=datvfs
	;get_data,"mvn_swics_velocity_MAVEN_MSO_interpolated",data=datvfc

	;s=arrMag("mvn_swics_pressure")
	;interpolator,"mvn_swics_pressure_Mag"

	;t=arrMag("mvn_B_1sec_MAVEN_MSO",yt="Magnetic Field")


	;get_data,"mvn_swics_pressure_Mag_interpolated",data=datP
	get_data,'mvn_swifs_pressure_proton',data=datP
	;help,datP
	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datB
	;help,datB
	betas=abs(datP.y)*!const.e*10.0^(6.)/((datB.y *10.0^(-9.))^2./(2* !const.mu0))
  	;;; [eV/cm^3] * !const.e J/eV * 10^6 cm^3/m^3  /( [nT]*10^-9 T/nT 
	store_data,"Plasma_Beta_proton",data={x:datP.x,y:betas,ytitle:"Plasma Beta"}
	if keyword_set(iononly) then return
	get_data,'mvn_pressure_electron',data=datPe
	betae=abs(datPe.y)*!const.e*10.0^(6.)/((datB.y *10.0^(-9.))^2./(2* !const.mu0))
  	;;; [eV/cm^3] * !const.e J/eV * 10^6 cm^3/m^3  /( [nT]*10^-9 T/nT 
	store_data,"Plasma_Beta_electron",data={x:datP.x,y:betae,ytitle:"Plasma Beta"}
	store_data,"Plasma_Beta",data={x:datP.x,y:betae+betas,ytitle:"Plasma Beta"}

end
