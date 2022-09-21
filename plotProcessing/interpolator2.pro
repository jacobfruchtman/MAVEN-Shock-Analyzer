;+
;----- Documentation for jacobf/Documents/plotProcessing/interpolator2.pro -----
;PROCEDURE:  interpolator2, plt
;PURPOSE:
;   Interpolates tplot data to resize to match a source tplot to be saved as a new tplot
; Input:
;   plt,        string  (name of tplot variable)      
;
;KEYWORDS:
;  source:  An optional string containing the name of the tplot for the target plot to be interpolated to match the size of.
;	   if not set, 'mvn_B_1sec' will be used
;  newName: An optional string containing the name of the new tplot structure. 
;	   If not set, newName will be defined as newName=plt+'_interpolated'
;  discrete:  Set this keyword to round the interpolated y values to the nearest integer
;  ytitle:   An optional keyword to manually define the ytitle. if not included, will set to the original, if it exists
; EXTRAPOLATE:  Controls interpolation of the ends of the data. Effects:
;                0:  Default action.  Set new y data to NAN or to MISSING.
;                1:  Extend the endpoints horizontally.
;                2:  Extrapolate the ends.  If the range of 't' is
;                    significantly larger than the old time range, the ends
;                    are likely to blow up.
;  INTERP_GAP:   Determines if points should be interpolated between data gaps,
;                together with the GAP_DIST.  IF the data gap > GAP_DIST,
;                follow the action of INTERP_GAP
;                0:  Default action.  Set y data to MISSING.
;                1:  Interpolate gaps
;  GAP_DIST:     Determines the size of a data gap above which interpolation
;                is regulated by INTERP_GAP.
;                Default value is 5, in units of the average time interval:
;                delta_t = (t(end)-t(start)/number of data points)
;  MISSING:      Value to set the new y data to for data gaps.  Default is NAN.
;  LAST_VALUE:  Set this keyword to return the last value of y array:  y[index]    (no interpolation performed)
;
;Notes:
;  1. does not currently work for spectrogram data
; Examples:
;    Interpolate the y values of 'mvn_swica_density' to the size of 'mvn_B_1sec'
;    interpolator2,'mvn_swica_density'
;    Interpolate the y values of 'mvn_swica_density' to the size of 'MAVEN_POS_(MARS-MSO)'
;    interpolator2,'mvn_swica_density',source='MAVEN_POS_(MARS-MSO)'
;    Interpolate the y values of 'MAVEN_POS_(MARS-MSO)' to the size of 'mvn_B_1sec', and name the new tplot "POS_interpolated_(MARS_MSO)"
;    interpolator2,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"
;   
;Modifications:
;  2021-10-29: Added discrete option? Don't remember
;  2021-12-29 : Added documentation, added ytitle keyword 
;  2021-12-29 : Modified to try to preserve other data structure elements, allow for interpolation of multidimensional y arrays with size(y,/n_dim) eq 1 or size(y,/n_dim) eq 2.
;		(previously, only allowed   size(y,/n_dim) eq 1 or (size(y,/n_dim) eq 2 and (size(y,/dim))[1] eq 3)
;
;CREATED BY:    Jacob Fruchtman
;FILE:  interpolator.pro
;VERSION  1.10
;LAST MODIFICATION: 12/29/21
; CREATED BY: Jacob Fruchtman
; $LastChangedBy: jacobf $
; $LastChangedDate: 2019-05-16 14:56:00 CST (Wed, 29 Dec 2021) $
; $LastChangedRevision: 27250 $
;-
pro interpolator2,plt,source=source,newName=newName,discrete=discrete,yt=yt,EXTRAPOLATE=EXTRAPOLATE,INTERP_GAP=INTERP_GAP,GAP_DIST=GAP_DIST,MISSING=MISSING
	tic


	GET_DATA,plt,data=dat,alim=limits
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_interpolated'
	;if NOT KEYWORD_SET(source) THEN source="mvn_B_1sec_Mag"
	;get_data,source,data=datS
	;nx=datS.x

	ox=dat.x
	oy=dat.y
	ny=0
	dim=size(oy,/n_dim)
	
	;+2021-12-29 update
	if NOT KEYWORD_SET(source) THEN source="mvn_B_1sec"
	get_data,source,data=datS
	nx=datS.x
	newlen=size(nx,/n_el)
	oldlen=size(ox,/n_el)

	ny=data_cut(dat,nx)
	
	if keyword_set(discrete) then ny=round(ny)	



	;datN=datS


	;help,datS.y
	;help,ny
	;datS.y=ny
	;datS.x=nx

	ndat={x:nx,y:ny}
	;help,ndat
	store_data,newName,data=ndat,dlim=limits
	
	possibleEls=['YLOG','ZLOG','SPEC','NO_INTERP','YRANGE','ZRANGE','ystyle','labels','labflag','ytitle','ztitle']

	str_element,dat,'v',valv,success=sv

	if sv then begin
		ov=dat.v
		vv=ov
		
		vdim=size(dat.v,/dim)
		;elv=size(dat.v,/n_el)
		if vdim[0] eq oldlen and size(ov,/n_dim) lt 3 then begin
			dat2={x:ox,y:ov}
			vv=data_cut(dat2,nx)	

			if total(size(ov,/type) eq [1,2,3,12,13,14,15]) eq 1 then vv=round(vv)
		endif

		str_element,ndat,'v',vv,/add_replace

	endif

	foreach el, possibleEls do begin
			str_element,dat,el,val,success=s
			if s then str_element,ndat,el,val,/add
	endforeach
	;help,ndat
	if NOT KEYWORD_SET(yt) THEN begin
		str_element,dat,'ytitle',vald,success=sd
		str_element,limits,'ytitle',vall,success=sl
		if sd then str_element,ndat,'ytitle',vald,/add_replace else $
		if sl then str_element,ndat,'ytitle',vall,/add
	endif else str_element,ndat,'ytitle',yt,/add_replace;ndat.ytitle=yt
	store_data,newName,data=ndat,dlim=limits
	;help,ndat
	toc
end
