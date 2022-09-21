pro upstreamparametercalc

	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout

	ins=datin.y
	outs=datout.y
	if total(datin.y + datout.y) eq 0 then return
	shockNormCalc;,/manualDown
	zerothtest
	x0=datin.x[0]
	;if x0 eq time_double('2015-02-03') then shockPlotter,currtime=currtime ;,startdate=startDate,ndays=ndays,currtime=currtime
	;shockPlotter,currtime=currtime ;,startdate=startDate,ndays=ndays,currtime=currtime    
	calcBeta

	UpstreamMeasurer,"Plasma_Beta_proton"
	spikeFiller3,"Plasma_Beta_proton_upstream"
	UpstreamMeasurer,"tproton_Mag_interpolated",newname="tproton_upstream"
	spikeFiller3,"tproton_upstream"
	UpstreamMeasurer,"Plasma_Beta"
	spikeFiller3,"Plasma_Beta_upstream"

	upstreamMeasurer,'mvn_B_1sec_MAVEN_MSO',newname='B_upstream'
	DownstreamMeasurer,'mvn_B_1sec_MAVEN_MSO',newname='Bd_vector'

	machNumCalcFine


	UpstreamMeasurer,"alfven_speed_Fine"
	spikeFiller3,"alfven_speed_Fine_upstream"

	UpstreamMeasurer,"v_Sound_Fine"
	spikeFiller3,"v_Sound_Fine_upstream"
	UpstreamMeasurer,"swics_velocity_interpolated_Mag",newname="V_up"
	UpstreamMeasurer,"mvn_swics_velocity_MAVEN_MSO_interpolated",newname="Vion_up"
	DownstreamMeasurer,"mvn_swics_velocity_MAVEN_MSO_interpolated",newname="Vion_down"
	UpstreamMeasurer,"mvn_swifs_velocity_MAVEN_MSO_interpolated",newname="Vion_up_fine"
	;RESTORE, 'dateData1.sav'
	;.run critMcalc
	critMcalc,startdate=startdate,currtime=currtime

	shockcoordcalc,"POS_interpolated_(MARS_MSO)",newname='POS_NIF'
	shockcoordcalc,"mvn_swics_velocity_MAVEN_MSO_interpolated",newname='V_NIF'
end
