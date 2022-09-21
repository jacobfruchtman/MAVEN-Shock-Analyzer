pro baverages
mvn_mag_load,'l2_full'
;mvn_eph_subsol_pos,xs,data=datSubSol,/ls
mvn_mag_load,'l1_1sec'				;Load L1 B data at 1-sec cadence
tplot,1
mvn_mag_load,'l2_full'
spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for MAVEN position

options,'mvn_swics_velocity','SPICE_FRAME','MAVEN_SWIA'


		spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

		options,'mvn_swifs_velocity','SPICE_FRAME','MAVEN_SWIA'
		spice_vector_rotate_tplot,'mvn_swifs_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

		options,'mvn_swifa_velocity','SPICE_FRAME','MAVEN_SWIA'
		spice_vector_rotate_tplot,'mvn_swifa_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO


		options,'mvn_swica_velocity','SPICE_FRAME','MAVEN_SWIA'
		spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

s=arrMag("mvn_B_1sec",yt="Magnetic Field")
s=arrMag("mvn_B_1sec_MAVEN_MSO",yt="Magnetic Field")

plotSmoother,"mvn_B_1sec_MAVEN_MSO_Mag",newName="B_20sec",smoothType=1,width=20
plotSmoother,"mvn_B_1sec_MAVEN_MSO_Mag",newName="B_30sec",smoothType=1,width=30
plotSmoother,"mvn_B_1sec_MAVEN_MSO_Mag",newName="B_40sec",smoothType=1,width=40
options,"B_20sec","colors",'g'
options,"B_30sec","colors",'b'
options,"B_40sec","colors",'y'
store_data,"avs",data="mvn_B_1sec_MAVEN_MSO_Mag B_20sec B_40sec"
calc,"'dB20sec'='mvn_B_1sec_MAVEN_MSO_Mag'-'B_20sec'"
calc,"'dB30sec'='mvn_B_1sec_MAVEN_MSO_Mag'-'B_30sec'"
calc,"'dB40sec'='mvn_B_1sec_MAVEN_MSO_Mag'-'B_40sec'"
calc,"'dB23'='dB20sec'-'B_30sec'"
calc,"'dB234'='dB23'-'B_40sec'"
options,"dB20sec","colors",'g'
options,"dB30sec","colors",'b'
options,"dB40sec","colors",'y'

options,'dB20sec','ytitle','tau average [nT]'
options,'dB30sec','ytitle','tau average [nT]'
options,'dB40sec','ytitle','tau average [nT]'

absPlot,'dB20sec'

absPlot,'dB30sec'
absPlot,'dB40sec'

store_data,"dBs",data='dB20sec dB30sec dB40sec'
store_data,"dBs_abs",data='dB20sec_abs dB30sec_abs dB40sec_abs'
tplot,["dBs","B_20sec","B_30sec","B_40sec"]
tplot,'mvn_B_1sec_MAVEN_MSO_Mag'
end
