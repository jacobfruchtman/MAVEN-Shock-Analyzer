
pro quickdatafixer
	startdate='2018-12-23'

	overname='overVsMach_'
	overfname=overname+'.tplot'
	
	tplot_restore,filename="Documents/overVsMachData/"+overfname

	get_data,overname,data=dat
	timespan,startdate,1
	mvndatareload
	s=arrMag("mvn_B_1sec",yt="Magnetic Field")
	s=arrMag("mvn_B_1sec_MAVEN_MSO",yt="Magnetic Field")
	electronsetup
	ionsetup

calc,"'mvn_swifs_pressure_proton'='tproton_Mag_interpolated'*'mvn_swifs_density_interpolated'"
calc,"'mvn_swifa_pressure_proton'='tproton_Mag_interpolated'*'mvn_swifa_density_interpolated'"
tplot_element,'mvn_swifa_pressure_proton','ytitle','Pressure !C [eV/cm^3]',/add
tplot_element,'mvn_swifs_pressure_proton','ytitle','Pressure !C [eV/cm^3]',/add

interpolator,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"
plotToCyl,"POS_interpolated_(MARS_MSO)",newName="POS_MSO_CYL"
	zerothBow

	;dat={maxlocs:GG,t:shocksUnix,mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,$
		downups:downups[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
	shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD,pos:POS[GG,*],FF:datff.y,Alfven:datAlfven.y[GG],$
	uindices:uis[GG,*],dindices:dis[GG,*],Cs:Cs[GG],flowangle:ThetaVB[GG],imins:datff.imins[GG],imaxs:datff.imaxs[GG],N_p:upNp,N_e:upNe};shock0Acc}
	overshoot=dat.overshoot
	get_data,"mvn_B_1sec_MAVEN_MSO",data=datB
	xs=dat.x
	ts=dat.t
	GG=ts-xs[0]
	N=numel(xs)
	IG=list()
	OG=list()
	ui=fltarr(N,2)-1.0
	uo=fltarr(N,2)-1.0
	di=fltarr(N,2)-1.0
	do=fltarr(N,2)-1.0
	for el=0,numel(GG)-1 do begin
		ishock=GG[el]
		uindex=dat.uindices[el,*]
		dindex=dat.dindices[el,*]
		if dat.uindices[el,0] lt ishock then begin
			
			 IG.add,ishock

			 ust=min(uindex)
			 uen=max(uindex)
			 dst=min(dindex)
			 den=max(dindex)
			for k=ust,den+1 do begin
				ui[k,0]=ust
				ui[k,1]=uen
				di[k,0]=dst
				di[k,1]=den
			endfor
		endif else begin

			OG.add,ishock

			 ust=min(uindex)
			 uen=max(uindex)
			 dst=min(dindex)
			 den=max(dindex)
			for k=dst,uen+1 do begin
				uo[k,0]=ust
				uo[k,1]=uen
				do[k,0]=dst
				do[k,1]=den
			endfor

		endelse
		
	endfor
	IG=IG.toarray()
	OG=OG.toarray()
	shocks=fltarr(N)
	shocks_inbound=fltarr(N)
	shocks_outbound=fltarr(N)

	shocks[GG]=1
	shocks_inbound[IG]=1
	shocks_outbound[OG]=1
	
	calcbeta
	
end



