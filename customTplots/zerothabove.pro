pro zerothabove,keep_plot=keep_plot
        ;maven_orbit_tplot doesn't output the approximate bow shock height as a data structure. It would be great to have that available

	R_equ = 3396.19D  ; +/- 0.1
	R_pol = 3376.20D  ; N pole = 3373.19 +/- 0.1 ; S pole = 3379.21 +/- 0.1
	R_vol = 3389.50D  ; +/- 0.2

	R_m = R_vol       ; use the mean radius for converting to Mars radii

	get_data,"POS_interpolated_(MARS_MSO)",data=datPos,alim=limPos

	interpolator,"alt"

	get_data,"alt_interpolated",data=datAlt

	MSO=datPos.y
	time=datPos.x

	gregTime=x2greg(time[0],/strformat)
	shockDate=(gregTime.split('T'))[0]
	print,shockDate
	YEAR=(shockDate.split('-'))[0]
		;print,shockDate
		dir='Documents/Plots/'+YEAR+'/'+shockDate+'/'
	FILE_MKDIR,dir
	x=MSO[*,0]/R_m
	y=MSO[*,1]/R_m
	z=MSO[*,2]/R_m
    	s = sqrt(y*y + z*z)
    	r = sqrt(x*x + y*y + z*z)
	hgt=datAlt.y
	; Shock conic (Trotignon)

 	 x0  = 0.600
 	 ecc = 1.026
  	L   = 2.081
  	; Shock conic (EDBURG)

 	; x0  = 0.550
 	; ecc = 1.05
  	;L   = 2.1

  	phm = 160.*!dtor

 	 phi   = atan(s,(x - x0))
 	 rho_s = sqrt((x - x0)^2. + s*s)
  	shockR_m = L/(1. + ecc*cos(phi < phm))
	shock=shockR_m*R_m

	shockR_mX=shockR_m*cos(phi < phm)+x0
	shockR_mRho=shockR_m*sin(phi < phm)
	shockX=shockR_mX*R_m
	shockRho=shockR_mRho*R_m

	store_data,"shock_height0_RM",data={x:datPos.x,y:shockR_m,ytitle:"zeroth Order Shock Height [R_m]"}
	store_data,"shock_height0",data={x:datPos.x,y:shock,ytitle:"zeroth Order Shock Height [km]"}
	shockabove=shock-(rho_s-.35)*R_m
	store_data,"shock_above0",data={x:datPos.x,y:shockabove,ytitle:"zeroth Order Shock Height [km]"}
	shockaboveRM=shockabove/R_m
	store_data,"shock_above0RM",data={x:datPos.x,y:shockaboveRM,ytitle:"zeroth Order Shock Height [km]"}
	;shplt=plot(x0+shockR_m ,rho_s,xtitle="shockR_m*cos",ytitle="shockR_m*sin")
	;shplt.close
; MPB conic (2-conic model of Trotignon)
	tplot
	return
end
