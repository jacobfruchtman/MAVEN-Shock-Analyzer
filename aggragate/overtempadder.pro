pro overtempadder,firstD=firstD,lastD=lastD,dontload=dontload

	srctxt="Documents/overmachDays.txt"




	read_strdata,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)
	if not keyword_set(firstD) then firstD=0
	if not keyword_set(lastD) then lastD=numPlots


	for day=firstD,lastD-1 do begin
		tplot_restore,filename="Documents/overVsMachData/"+H[day]
	
		thisfile=H[day]	

		if thisfile eq 'OverVsMach_2019-11-17.tplot' then break
		name=(strsplit(((strsplit(thisfile,"." , /extract))[0]),'/',/extract))

		get_data,name,data=dat

		if size(dat,/typ) eq 2 then begin
			del_data,'*'
			continue
		endif

		str_element,dat,"T_e",var,su=s
	
		if  s then  begin
				del_data,'*'
				continue
			endif

		TIC
		date=name.remove(0,10)
		t00=time_double(date)
		loadclock=TIC('loading')
		if  1 then begin
		;;TIC
		timespan,date,ndays

		;mvn_swia_load_l2_data,/tplot,/loadall		;Load SWIA data and make tplot vars	
		mvn_mag_load,'l1_1sec'				;Load L1 B data at 1-sec
		mvn_mag_load,'l2_full'
		mk = mvn_spice_kernels(/load)			;Load SPICE Kernels

		
		;print,systime()
						;rotate B to MSO
		mvn_swe_load_l2,/spec			;Load SWEA data and make tplot vars
		mvn_swe_n1d
	;orbclock=;TIC('orbit')
		maven_orbit_tplot
		spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for
		spice_vector_rotate_tplot,'mvn_B_1sec','MAVEN_MSO',check = 'MAVEN_SC_BUS'
	;;TOC,orbclock
		mvn_eph_subsol_pos,xs,data=datSubSol,/ls
		;print,systime()

		mvn_scpot
		mvn_swe_addpot
		mvn_swe_n1d
		;mvn_swia_part_moments,type=['cs','fs']


		;mvn_swia_protonalphamoms_minf_mag

		;options,'mvn_swics_velocity','SPICE_FRAME','MAVEN_SWIA'


		;spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

		;options,'mvn_swifs_velocity','SPICE_FRAME','MAVEN_SWIA'
		;spice_vector_rotate_tplot,'mvn_swifs_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
		;	
		;done loading


		;s=arrMag("mvn_B_1sec_MAVEN_MSO",yt="Magnetic Field")
		;interpolator,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"

		;ionsetup,/noarc,/doquick
		;calc,"'mvn_swifs_pressure_proton'='tproton_Mag_interpolated'*'mvn_swifs_density_interpolated'"

		;plottocyl,"POS_interpolated_(MARS_MSO)",newName="POS_MSO_CYL"
		;zerothbow
		;calcbeta,/iononly
		get_data,'mvn_swe_spec_temp',data=datte

		numpoints=numel(dat.t)
		get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
		xs=datB.x

		tox=datte.x

		ot_e=datte.y
	
		for i=0,numel(tox)-1 do if ot_e[i] lt 0 then ot_e[i]= !values.f_nan


		store_data,'mvn_swe_spec_temp',data=datte
		get_data,'mvn_swe_spec_dens',data=datde


		dox=datde.x

		od_e=datte.y
	
		for i=0,numel(dox)-1 do if od_e[i] lt 0 then od_e[i]= !values.f_nan


		store_data,'mvn_swe_spec_dens',data=datde

options,'mvn_swe_spec_*','interpol',1
interpolator2,'mvn_swe_spec_temp';,/yonly
interpolator2,'mvn_swe_spec_dens';,/yonly


get_data,'mvn_swe_spec_temp_interpolated',data=datte
t_e=datte.y
for i=0,numel(t_e)-1 do if t_e[i] lt 0 then t_e[i]= !values.f_nan
get_data,'mvn_swe_spec_dens_interpolated',data=datde
d_e=datde.y
for i=0,numel(d_e)-1 do if d_e[i] lt 0 then d_e[i]= !values.f_nan

		;;Finished computing raw stuff
		;TOC
		TOC,loadclock
		endif
		print,"==================="
		print,"Finished computing raw stuff"
		;TIC
		numpoints=numel(dat.t)
		get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
		xs=datB.x
		t0=xs[0]
		

		
		uindices=dat.uindices
		dindices=dat.dindices
		
		shocktimes=dat.t
		ishocks=shocktimes-t0

		help,shocktimes
		help,ishocks
		help,numpoints


		T_els=fltarr(numpoints)
		T_els2=fltarr(numpoints)
		N_els2=fltarr(numpoints)
		;Beta_ions=fltarr(numpoints)


		for el=0,numpoints-1 do begin
			;print,el
			tshock=shocktimes[el]
			ishock=ishocks[el]
			ubeg=min(uindices[el,*])
			uend=max(uindices[el,*])
			if ubeg eq uend then begin
				print,ubeg ,"=ubeq =uend=",uend
				return
				
			endif
			dbeg=min(dindices[el,*])
			dend=max(dindices[el,*])
			if dbeg eq dend then begin
				print,dbeg ,"=ubeq =uend=",dend
				return
				
			endif
			;print,"beg:",time_string(xs[ubeg])
			;print,"end:",time_string(xs[uend])
			T_els[el]=mean(t_e[ubeg:uend],/NAN)
			T_els2[el]=mean(t_e[dbeg:dend],/NAN)
			N_els2[el]=mean(d_e[dbeg:dend],/NAN)
			;print,T_els[el]
			;Beta_ions[el]=mean(Betas[ubeg:uend])
			;Bum=[Mean(Bvec[ubeg:uend,0]),Mean(Bvec[ubeg:uend,1]),Mean(Bvec[ubeg:uend,2])]
			;vumf=[Mean(Vf[ubeg:uend,0]),Mean(Vf[ubeg:uend,1]),Mean(Vf[ubeg:uend,2])]
			;vumc=[Mean(Vc[ubeg:uend,0]),Mean(Vc[ubeg:uend,1]),Mean(Vc[ubeg:uend,2])]
			;BD=[Mean(Bvec[dbeg:dend,0]),Mean(Bvec[dbeg:dend,1]),Mean(Bvec[dbeg:dend,2])]
			;vd=[Mean(Vc[dbeg:dend,0]),Mean(Vc[dbeg:dend,1]),Mean(Vc[dbeg:dend,2])]
			;cxx=Xcyl[ishock]
			;crho=RHOcyl[ishock]
			;D=Sqrt( (cxx-s0x)^2+(crho-s0rho)^2);+ophi^2)
			;minDists=min(D,minloc)
		endfor
		;print,minDists[i]
		;print,T_els
		;print,"[s0x,s0rho][i]=",[s0x[i],s0rho[i]]
		;print,"[Pxx[el],Prho[el]]=",[Pxx[el],Prho[el]]
		;N_Conics[el,0]=snrr[minloc]
		;N_Conics[el,1]=0.0;snxx[minloc]
		;N_Conics[el,2]=snxx[minloc]
		str_element,dat,'T_e',T_els,/add
		str_element,dat,'T_e_down',T_els2,/add
		str_element,dat,'N_e_down',N_els2,/add
		;str_element,dat,'beta_ion',Beta_ions,/add
		;str_element,dat,'Buvec',Buvecs,/add
		;str_element,dat,'Bdvec',Bdvecs,/add
		;str_element,dat,'Vuvec_fine',Vuvecs_fine,/add
		;str_element,dat,'Vuvec_coarse',Vuvecs_coarse,/add
		;str_element,dat,'Vdvec',Vdvecs,/add
		;str_element,dat,'N_conic',N_Conics,/add
		;str_element,dat,'pos_conic',Posonics,/add

		;return
		del_data,'*'
		print,"STORING:"
		;help,dat
		store_data,name[0],data=dat
		
		;return
		tplot_save,name,filename="Documents/overVsMachData/"+name[0]

		print,"finished day"
		del_data,'*'
		print,"#####################"
		;TOC
		TOC
		print,"#####################"
		;if day gt 4 then return
	endfor


end


