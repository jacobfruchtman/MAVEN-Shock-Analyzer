pro tictoctester
	startdate='2015-07-26/00:00:00'
	t0=time_double(startdate)
	N=86400.
	xx=findgen(N)+t0
	yy=findgen(N,3)
	zz=sin(2*!pi*xx/1800.)
	MMs=findgen(N,4)

	imin=500.
	imax=imin+30*60.
	allmm=findgen(6,4,10)
	
	MM=allmm[2,*,3]
		

	tic
	for i=imin,imax do MMs[i,*]=MM 
	toc
	MMs=findgen(N,4)

	imin=500.
	imax=imin+30*60.
	allmm=findgen(6,4,10)
	
	MM=allmm[2,*,3]
	tic
	for i=imin,imax do for j=0,3 do MMs[i,j]=MM[j] 
	toc	


	return

	dat={x:xx,y:yy,z:zz,m:mm}
	store_data,'testsutff',data=dat
	
	allxx=fltarr(N,10)
	allyy=fltarr(N,3,10)
	allzz=allxx
	allmm=fltarr(N,4,10)

	alloxx=fltarr(N,10)
	alloyy=fltarr(N,3,10)
	allozz=allxx
	allomm=fltarr(N,4,10)

	datclock=tic('datload0')

	for i=0,10-1 do begin

		allxx[*,i]=dat.x
		alloxx[*,i]=reverse(dat.x)
		help,dat.y
		help,allyy[*,*,i]
		allyy[*,*,i]=dat.y
		alloyy[*,*,i]=reverse(dat.y)
		allzz[*,i]=dat.z
		allozz[*,i]=reverse(dat.z)
		allmm[*,*,i]=dat.m
		allomm[*,*,i]=reverse(dat.m)

	endfor


	toc,datclock
	allxx=fltarr(N,10)
	allyy=fltarr(N,3,10)
	allzz=allxx
	allmm=fltarr(N,4,10)

	alloxx=fltarr(N,10)
	alloyy=fltarr(N,3,10)
	allozz=allxx
	allomm=fltarr(N,4,10)

	datclock1=tic('datload1')

	for i=0,10-1 do begin
		ix=dat.x
		ox=reverse(dat.x)
		iy=dat.y
		oy=reverse(dat.y)
		iz=dat.z
		oz=reverse(dat.z)
		im=dat.m
		om=reverse(dat.m)
		allxx[*,i]=ix
		alloxx[*,i]=ox
		allyy[*,*,i]=iy;dat.y
		alloyy[*,*,i]=oy;reverse(dat.y)
		allzz[*,i]=iz;dat.z
		allozz[*,i]=oz;reverse(dat.zz)
		allmm[*,*,i]=im;dat.m
		allomm[*,*,i]=om;reverse(dat.m)

	endfor


	toc,datclock1
	allxx=fltarr(N,10)
	allyy=fltarr(N,3,10)
	allzz=allxx
	allmm=fltarr(N,4,10)

	alloxx=fltarr(N,10)
	alloyy=fltarr(N,3,10)
	allozz=allxx
	allomm=fltarr(N,4,10)
	datclock2=tic('datload2')

	for i=0,10-1 do begin
		ix=dat.x
		ox=reverse(dat.x)
		iy=dat.y
		oy=reverse(dat.y)
		iz=dat.z
		oz=reverse(dat.z)
		im=dat.m
		om=reverse(dat.m)

		for j=0,N-1 do begin
		allxx[j,i]=ix[j]
		alloxx[j,i]=ox[j]
		
		allzz[j,i]=iz[j];dat.z
		allozz[j,i]=oz[j];reverse(dat.zz)

		for k=0,2 do begin
		allyy[j,k,i]=iy[j,k];dat.y
		alloyy[j,k,i]=oy[j,k];reverse(dat.y)
		allmm[j,k,i]=im[j,k];dat.m
		allomm[j,k,i]=om[j,k];reverse(dat.m)
		endfor
		
		allmm[j,3,i]=im[j,3];dat.m
		allomm[j,3,i]=om[j,3];reverse(dat.m)

		endfor
	endfor


	toc,datclock2
	return
	startdate='2015-07-26/00:00:00'
	timespan,startdate
	totclock=tic('total')
		mvn_swia_load_l2_data,/tplot,/loadall		;Load SWIA data and make tplot vars	
mvn_mag_load,'l1_1sec'				;Load L1 B data at 1-sec
mvn_mag_load,'l2_full'
		mk = mvn_spice_kernels(/load)			;Load SPICE Kernels

		
		;print,systime()
						;rotate B to MSO
	;mvn_swe_load_l2,/sumplot			;Load SWEA data and make tplot vars
	;orbclock=tic('orbit')
		maven_orbit_tplot
		spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for
		spice_vector_rotate_tplot,'mvn_B_1sec','MAVEN_MSO',check = 'MAVEN_SC_BUS'
	;toc,orbclock
	partclock=tic('parts')
		mvn_swia_part_moments,type=['cs','fs']

	toc,partclock
		mvn_swia_protonalphamoms_minf_mag

		options,'mvn_swics_velocity','SPICE_FRAME','MAVEN_SWIA'


		spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

		options,'mvn_swifs_velocity','SPICE_FRAME','MAVEN_SWIA'
		spice_vector_rotate_tplot,'mvn_swifs_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
			

	toc,totclock
end
