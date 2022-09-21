pro zerothBow,keep_plot=keep_plot
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
	

	;shplt=plot(x0+shockR_m ,rho_s,xtitle="shockR_m*cos",ytitle="shockR_m*sin")
	;shplt.close
; MPB conic (2-conic model of Trotignon)

  rho_p = x
  MPB   = x
	newxx=x
	newrhorho=x

	mx=x
	ms=x
; First conic (x > 0)

  indx = where(x ge 0)

  x0  = 0.640
  ecc = 0.770
  L   = 1.080

  phi = atan(s,(x - x0))

  rho_p[indx] = sqrt((x[indx] - x0)^2. + s[indx]*s[indx])
  MPB[indx] = L/(1. + ecc*cos(phi[indx]))
  newxx[indx]=MPB[indx]*cos(phi[indx])+x0
  newrhorho[indx]=MPB[indx]*sin(phi[indx])
  
; Second conic (x < 0)

  indx = where(x lt 0)

  x0  = 1.600
  ecc = 1.009
  L   = 0.528

  phm = 160.*!dtor

  phi = atan(s,(x - x0))
  rho_p[indx] = sqrt((x[indx] - x0)^2. + s[indx]*s[indx])
  MPB[indx] = L/(1. + ecc*cos(phi[indx] < phm))
  newxx[indx]=MPB[indx]*cos(phi[indx] <phm)+x0
  newrhorho[indx]=MPB[indx]*sin(phi[indx]<phm)
; Define the regions
	npts=size(x,/n_el)
  ss = dblarr(npts, 5)
  ss[*,0] = x
  ss[*,1] = y
  ss[*,2] = z
  ss[*,3] = r
  ss[*,4] = hgt

  indx = where(rho_s ge shockR_m, count)
  sheath = ss
  if (count gt 0L) then begin
    sheath[indx,0] = !values.f_nan
    sheath[indx,1] = !values.f_nan
    sheath[indx,2] = !values.f_nan
    sheath[indx,3] = !values.f_nan
    sheath[indx,4] = !values.f_nan
  endif

  indx = where(rho_p ge MPB, count)
  pileup = ss
  if (count gt 0L) then begin
    pileup[indx,0] = !values.f_nan
    pileup[indx,1] = !values.f_nan
    pileup[indx,2] = !values.f_nan
    pileup[indx,3] = !values.f_nan
    pileup[indx,4] = !values.f_nan
  endif

 ; indx = where((x gt 0D) or (s gt shadow), count)
;  wake = ss
;  if (count gt 0L) then begin
;    wake[indx,0] = !values.f_nan
 ;   wake[indx,1] = !values.f_nan
 ;   wake[indx,2] = !values.f_nan
 ;   wake[indx,3] = !values.f_nan
 ;   wake[indx,4] = !values.f_nan
 ; endif

  indx = where(finite(sheath[*,0]) eq 1, count)
  wind = ss
  if (count gt 0L) then begin
    wind[indx,0] = !values.f_nan
    wind[indx,1] = !values.f_nan
    wind[indx,2] = !values.f_nan
    wind[indx,3] = !values.f_nan
    wind[indx,4] = !values.f_nan
  endif
  
  indx = where(finite(pileup[*,0]) eq 1, count)
  if (count gt 0L) then begin
    sheath[indx,0] = !values.f_nan
    sheath[indx,1] = !values.f_nan
    sheath[indx,2] = !values.f_nan
    sheath[indx,3] = !values.f_nan
    sheath[indx,4] = !values.f_nan
  endif
  
 ; indx = where(finite(wake[*,0]) eq 1, count)
 ; if (count gt 0L) then begin
   ; sheath[indx,0] = !values.f_nan
   ; sheath[indx,1] = !values.f_nan
   ; sheath[indx,2] = !values.f_nan
   ; sheath[indx,3] = !values.f_nan
   ; sheath[indx,4] = !values.f_nan

  ;  pileup[indx,0] = !values.f_nan
  ;  pileup[indx,1] = !values.f_nan
 ;   pileup[indx,2] = !values.f_nan
 ;   pileup[indx,3] = !values.f_nan
 ;   pileup[indx,4] = !values.f_nan
 ; endif

;	newxx=MPB*Cos(phi)
		
	;plt=PLOT(x,rho_p,xtitle="x",ytitle="rho_p")
	;plt=PLOT(MPB,rho_p,xtitle="MPB",ytitle="rho_p")
	;pltB=PLOT(shockR_mX,shockR_mRho,xtitle="X [R_m]",ytitle="$\sqrt{Y^2+Z^2}$ [R_m]",name="shockBoundary, $R_{shock}(\phi)\sin\phi$ vs $R_{shock}(\phi)\cos\phi$",'b-')
	;pltS=PLOT(newxx,newrhorho,/overplot,name="Sheath, $R_{MPB}(\phi)\sin\phi$ vs $R_{MPB}(\phi)\cos\phi$",'c-')

	;pltM=PLOT(cos(findgen(200)*!pi/100),sin(findgen(200)*!pi/100),/overplot,'r-',name="Mars")
	;cc0 = LEGEND(target=[pltS,pltB,pltM])
	;fname=dir+"ZerothOrderShockConicR_m.png"
	;cc0.save,fname, BORDER=10, $

;RESOLUTION=300
	;pltB.close	
;if not keyword_set(keep_plot) then pltB.close
	R_m = 3389.50D  ; +/- 0.2
	
	 x0  = 0.600*R_m
 	ecc = 1.026
  	L   = 2.081*R_m
	xx2t=(findgen(1000000.)/500000.0 +max([-0.7,min(x)]))*( L /(1+ecc)) 
	xx2a=xx2t+x0
	rr2t=sqrt((L-ecc*xx2t)^2 -xx2t^2)

	
	;pltBR=PLOT(shockX,shockRho,xtitle="X [km]",ytitle="$\sqrt{Y^2+Z^2}$ [km]",name="shockBoundary,$R_{shock}(\phi)\sin\phi$ vs $R_{shock}(\phi)\cos\phi$",'b-')
	;pltBR2=PLOT(xx2a,rr2t,xtitle="X [km]",ytitle="$\sqrt{Y^2+Z^2}$ [km]",name="test shockBoundary,$R_{shock}(\phi)\sin\phi$ vs $R_{shock}(\phi)\cos\phi$",'g-',/overplot)
	;pltSR=PLOT(newxx*R_m,newrhorho*R_m,/overplot,name="Sheath, $R_{MPB}(\phi)\sin\phi$ vs $R_{MPB}(\phi)\cos\phi$",'c-')
	;pltMR=PLOT(cos(findgen(200)*!pi/100)*R_m,sin(findgen(200)*!pi/100)*R_m,/overplot,'r-',name="Mars")
	;cc1 = LEGEND(target=[pltSR,pltBR,pltMR])
	;fname=dir+"ZerothOrderShockConic.png"
	;cc1.save,fname, BORDER=10, $

  ; RESOLUTION=300
	;pltBR.close
	shock0Vec=[[shockRho],[shockRho*0.0],[shockX]]
	shock0VecBig=[[rr2t],[fltarr(1000000)],[xx2a]]
	;plt=PLOT(newxx,rho_p,xtitle="MPB*cos(phi)",ytitle="rho_p")
	;if not keyword_set(keep_plot) then pltBR.close
	store_data,'shock0rho_interpolated',data={x:time, y:shockRho,ytitle:"Sqrt(Ymso^2+Zmso^2) [km]"}
	store_data,'shock0x_interpolated',data={x:time, y:shockX,ytitle:"X_mso [km]"}

	store_data,'shock0_interpolated',data={x:time, y:shock0Vec,ytitle:"Shock Conic [km]",LABELS:['rho','phi','x'],LABFLAG:1}
	;options,'shock0_interpolated','colors',['r','g','b']

	store_data,'shock0_conic',data={conic:shock0VecBig}
	;options,'shock0_interpolated','colors',['r','g','b']


	store_data,'sheath0rho_interpolated',data={x:time, y:newrhorho*R_m,ytitle:"Sqrt(Ymso^2+Zmso^2) [km]"}
	store_data,'sheath0x_interpolated',data={x:time, y:newxx*R_m,ytitle:"X_mso [km]"}
	store_data,'rho_interpolated',data={x:time, y:s*R_m,ytitle:"Sqrt(Ymso^2+Zmso^2) [km]"}

	store_data,'sheath_interpolated',data={x:time, y:sheath[*,4],ytitle:"in sheath alt [km]"}
 ; options,'sheath_interpolated','color','red'
	
	store_data,'wind_interpolated',data={x:time, y:wind[*,4],ytitle:"in wind alt [km]"}
;  options,'wind_interpolated','color','blue'



	s0rho=shockRho



	anyBackwards=0
	N=size(shockX,/n_el)
	sx0=shockX-x0



	srr=SQRT(sx0^2+s0rho^2)
	srt=Sqrt(s0rho^2+(sx0+ecc*srr)^2)
	snxx=(ecc+sx0/srr)*srr/srt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT. Fixed, I think?
	snrr=s0rho/srt
	SN=[[snrr],[fltarr(N)],[snxx]]

	store_data,"shock0_normals",data={x:time,y:SN,LABELS:['rho','phi','x'],LABFLAG:1,ytitle:"Shock0 Normal"}
	;options,'shock0_normals','colors',['r','g','b']

	s2rr=SQRT(rr2t^2+xx2t^2)
	s2rt=Sqrt(rr2t^2+(xx2t+ecc*s2rr)^2)
	s2nxx=(ecc+xx2t/s2rr)*s2rr/s2rt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT. Fixed, I think?
	s2nrr=rr2t/s2rt
	SN2=[[s2nrr],[fltarr(1000000)],[s2nxx]]
	store_data,"shock0_conic_normals",data={normals:SN2}
	;wdelete
end
