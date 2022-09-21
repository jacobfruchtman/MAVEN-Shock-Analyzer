pro getintervalpositions

	ndays=1
	srctxt="Documents/overmachDays.txt"


	read_strDATA,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)

	for day=0,numPlots-1 do begin
		;if day eq 7 then day=600
		thisfile=H[day]
		print,thisfile
		name=(strsplit(((strsplit(thisfile,"." , /extract))[0]),'/',/extract))[0]
		get_data,name,data=dat
		;catch,error_status
		;	if error_status ne 0 then begin
		;	print,i
		;	print,H[i],"doesn't exist ERROR"

		;		catch,/cancel
				;continue
		;endif
		if size(dat,/typ) eq 2 then begin
			
			tplot_restore,filename="Documents/overVsMachData/"+H[day]
	
			
			name=(strsplit(((strsplit(thisfile,"." , /extract))[0]),'/',/extract))

			get_data,name,data=dat

			if size(dat,/typ) eq 2 then begin
				del_data,'*'
				continue
			endif
		endif
		if numel(dat.t) ne numel(dat.shock0dist) then begin
			print,"BAD dataset at ",thisday
			wherebaddist.add,thisday+" ## numel(dat.t),numel(dat.shock0dist)="+strtrim(numel(dat.t),2)+","+strtrim(numel(dat.shock0dist),2)
			return;continue
		endif

		str_element,dat,"downstartpos",var,su=s
	
		if s then  begin
				del_data,'*'
				continue
			endif

		TIC
		date=name.remove(0,10)
		t00=time_double(date)
		

		timespan,date,ndays
		mvn_mag_load,'l1_1sec'				;Load L1 B data at 1-sec
mvn_mag_load,'l2_full'
		mk = mvn_spice_kernels(/load)			;Load SPICE Kernels

		
		;print,systime()
						;rotate B to MSO
	;mvn_swe_load_l2,/sumplot			;Load SWEA data and make tplot vars

		maven_orbit_tplot

		spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for MAVEN position



		spice_vector_rotate_tplot,'mvn_B_1sec','MAVEN_MSO',check = 'MAVEN_SC_BUS'
		print,"finished loading"
		print,"~~~~~~~~~~~~~~~~"
		interpolator,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"

		numpoints=numel(dat.t)
		;shockPos=dat.pos
		upstartpos=fltarr(numpoints,3)
		upendpos=fltarr(numpoints,3)
		downstartpos=fltarr(numpoints,3)
		downendpos=fltarr(numpoints,3)
		upmidpos=fltarr(numpoints,3)
		downmidpos=fltarr(numpoints,3)

		get_data,"POS_interpolated_(MARS_MSO)",data=datPos
		POS=datPos.y

		uindices=dat.uindices
		dindices=dat.dindices
		print,"numpoints=",numpoints
		for el=0,numpoints-1 do begin

			ustart=min(uindices[el,*])
			uend=min(uindices[el,*])
			dstart=min(dindices[el,*])
			dend=min(dindices[el,*])
			umid=mean(uindices[el,*])
			dmid=mean(dindices[el,*])

			thisusp=POS[ustart,*]
			thisump=POS[umid,*]
			thisuep=POS[uend,*]
			thisdsp=POS[dstart,*]
			thisdmp=POS[dmid,*]
			thisdep=POS[dend,*]

			for j=0, 2 do begin
				upstartpos[el,j]=thisusp[j]
				upmidpos[el,j]=thisump[j]
				upendpos[el,j]=thisuep[j]
				downstartpos[el,j]=thisdsp[j]
				downmidpos[el,j]=thisdmp[j]
				downendpos[el,j]=thisdep[j]
			endfor

		endfor
		str_element,dat,'upstartpos',upstartpos,/add
		str_element,dat,'upmidpos',upmidpos,/add
		str_element,dat,'upendpos',upendpos,/add

		str_element,dat,'downstartpos',downstartpos,/add
		str_element,dat,'downmidpos',downmidpos,/add
		str_element,dat,'downendpos',downendpos,/add
		;clearPlots
		del_data,'*'
		print,"STORING:"
		;help,dat
		store_data,name[0],data=dat
		
		;return
		tplot_save,name,filename="Documents/overVsMachData/"+name[0]

		print,"finished day"
		del_data,'*'
		print,"#####################"
		TOC
		print,"#####################"
	endfor
end
