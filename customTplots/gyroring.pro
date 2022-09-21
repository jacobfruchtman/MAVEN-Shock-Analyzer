pro gyroring

	get_data,'shocks_inbound',data=datsin
	get_data,'shocks_outbound',data=datsout

	if total(datsin.y+datsout.y) eq 0 then return

	get_data,"Franken_fitted",data=datFF
	imins=datFF.imins
	;imaxs=datFF.imaxs
	get_data,'BB_flag',data=datBB
	get_data,'bBB_flag',data=datbBB
	imaxs=where(datBB.y ne 0)
	;shockcoordcalc,'shockDIST_MAVEN_MSO',newname='shockDIST_NIF'
	t=datsin.x
	get_data,'V_up',data=datV

	Vion=datV.y
	;get_data,"POS_interpolated_(MARS_MSO)",data=datpos
	;POS=datpos.y
	N=numel(t)
	get_data,'shockDIST_MAVEN_MSO',data=datDIST
	shockdist=sqrt(datDIST.y[*,0]^2+datDIST.y[*,1]^2+datDIST.y[*,2]^2)

	get_data,'Shock_Normal_AVG',data=datAVG

	AVG=datAVG.y
	
	get_data,"proton_cyclotron_period",data=datPer

	IG=where(datsin.y eq 1,icount)

	OG=where(datsout.y eq 1,ocount)

	tau=datPer.y

	;todd=time_double('2015-08-16/22:28:29')
	;print,todd+.5-t[0]

	ugyrflag=fltarr(N)
	ugyrflag68=fltarr(N)
	ugyrflag45=fltarr(N)
	dgyrflag=fltarr(N)
	dgyrflag68=fltarr(N)
	dgyrflag45=fltarr(N)
	larmradii=fltarr(N)
	for i=0,icount-1 do begin
		print,"******"
		ishock=IG[i]
		
		NAVG=AVG[ishock,*]
		VV=Vion[ishock]
		tshock=t[ishock]
		print,time_string(tshock)
		;Vn=VV[0]*NAVG[0]+VV[1]*NAVG[1]+VV[2]*NAVG[2]
		period=tau[ishock]
		larmor_radius=VV*period/(2*!pi)

		larmradii[where(tau eq period)]=larmor_radius
		;print,"shockdist=",shockdist[ todd+.5-t[0]]
		print,"larmor_radius=",larmor_radius
		imin=imins[ishock]
		imax=imaxs[(where(imaxs-ishock gt 0))[0]];imaxs[ishock]
		whereout=where((t gt t[imin] or tau eq period) and .45*larmor_radius le shockdist and t lt tshock,outfoots)
		if outfoots ne 0 then begin
			print,whereout[0]
			print,whereout[-1]
			foot45=whereout[-1]
			print,foot45
			ugyrflag45[foot45]=shockdist[foot45]

		
			whereout=where((t gt t[imin] or tau eq period) and .68*larmor_radius le abs(shockdist) and t lt tshock,outfoots)
			if outfoots ne 0 then begin
				foot68=whereout[-1]
				ugyrflag68[foot68]=shockdist[foot68]

				whereout=where((t gt t[imin] or tau eq period) and larmor_radius le shockdist and t lt tshock,outfoots)
				if outfoots ne 0 then begin
					foot=whereout[-1]
					ugyrflag[foot]=shockdist[foot]
				endif
			endif
		endif

		whereout=where((t lt t[imax] or tau eq period) and .45*larmor_radius le shockdist and t gt tshock,outfoots)
		if outfoots ne 0 then begin
			foot45=whereout[0]
			dgyrflag45[foot45]=shockdist[foot45]

		
			whereout=where((t lt t[imax] or tau eq period) and .68*larmor_radius le shockdist and t gt tshock,outfoots)
			if outfoots ne 0 then begin
				foot68=whereout[0]
				dgyrflag68[foot68]=shockdist[foot68]

				whereout=where((t lt t[imax] or tau eq period) and larmor_radius le shockdist and t gt tshock,outfoots)
				if outfoots ne 0 then begin
					foot=whereout[0]
					dgyrflag[foot]=shockdist[foot]
				endif
			endif
		endif

	endfor
imaxs=datFF.imaxs
	print,where(ugyrflag45 gt 0)
	for i=0,ocount-1 do begin
		ishock=OG[i]
		NAVG=AVG[ishock,*]
		VV=Vion[ishock]
		tshock=t[ishock]
		;Vn=VV[0]*NAVG[0]+VV[1]*NAVG[1]+VV[2]*NAVG[2]
		period=tau[ishock]
		larmor_radius=VV*period/(2*!pi)

		larmradii[where(tau eq period)]=larmor_radius
		imin=min([imins[ishock],imaxs[ishock]])
		imax=max([imins[ishock],imaxs[ishock]]);imaxs[ishock]
		;imin=(where(tau eq period))[0]
		whereout=where((t gt t[imin] or tau eq period) and .45*larmor_radius le shockdist and t lt tshock,outfoots)
		if outfoots ne 0 then begin
			foot45=whereout[-1]
			dgyrflag45[foot45]=shockdist[foot45]

		
			whereout=where((t gt t[imin] or tau eq period) and .68*larmor_radius le shockdist and t lt tshock,outfoots)
			if outfoots ne 0 then begin
				foot68=whereout[-1]
				dgyrflag68[foot68]=shockdist[foot68]

				whereout=where((t gt t[imin] or tau eq period) and larmor_radius le shockdist and t lt tshock,outfoots)
				if outfoots ne 0 then begin
					foot=whereout[-1]
					dgyrflag[foot]=shockdist[foot]
				endif
			endif
		endif

		whereout=where((t lt t[imax] or tau eq period)  and .45*larmor_radius le shockdist and t gt tshock,outfoots)
		if outfoots ne 0 then begin
			foot45=whereout[0]
			ugyrflag45[foot45]=shockdist[foot45]

		
			whereout=where((t lt t[imax] or tau eq period) and .68*larmor_radius le shockdist and t gt tshock,outfoots)
			if outfoots ne 0 then begin
				foot68=whereout[0]
				ugyrflag68[foot68]=shockdist[foot68]

				whereout=where((t lt t[imax] or tau eq period) and larmor_radius le shockdist and t gt tshock,outfoots)
				if outfoots ne 0 then begin
					foot=whereout[0]
					ugyrflag[foot]=shockdist[foot]
				endif
			endif
		endif

	endfor

	gyrflag=ugyrflag+dgyrflag
	gyrflag45=ugyrflag45+dgyrflag45
	gyrflag68=ugyrflag68+dgyrflag68

	store_data,'larmor_radius',data={x:t,y:larmradii,ytitle:'upstream Larmor Radius [km]'}

	store_data,'gyro_distance_upstream_flag',data={x:t,y:ugyrflag,ytitle:'where upstream dx_n = Vu*tau'}
	store_data,'gyro_distance68_upstream_flag',data={x:t,y:ugyrflag68,ytitle:'where upstream dx_n = 0.68Vu*tau'}
	store_data,'gyro_distance45_upstream_flag',data={x:t,y:ugyrflag,ytitle:'where upstream dx_n = 0.45Vu*tau'}

	store_data,'gyro_distance_downstream_flag',data={x:t,y:dgyrflag,ytitle:'where downstream dx_n = Vu*tau'}
	store_data,'gyro_distance68_downstream_flag',data={x:t,y:dgyrflag68,ytitle:'where downstream dx_n = 0.68Vu*tau'}
	store_data,'gyro_distance45_downstream_flag',data={x:t,y:dgyrflag,ytitle:'where downstream dx_n = 0.45Vu*tau'}


	store_data,'gyro_distance_flag',data={x:t,y:gyrflag,ytitle:'where dx_n = Vu*tau'}
	store_data,'gyro_distance68_flag',data={x:t,y:gyrflag68,ytitle:'where dx_n = 0.68Vu*tau'}
	store_data,'gyro_distance45_flag',data={x:t,y:gyrflag,ytitle:'where dx_n = 0.45Vu*tau'}
end
