;+
; quickmeasurer
; runs through saved data, calculates and saves the following raw measured quantities:
;	Tion,beta_ion,Bu_vec,Vu_vec,Bd_vec,Vd_vec,pos_conic,N_conic
;-
pro quickmeasurer,firstD=firstD,lastD=lastD,dontload=dontload

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

		str_element,dat,"T_ion",var,su=s
	
		if s then  begin
				del_data,'*'
				continue
			endif

		TIC
		date=name.remove(0,10)
		t00=time_double(date)
		if not keyword_set(dontload) then begin
		;;TIC
		timespan,date,ndays

		mvn_swia_load_l2_data,/tplot,/loadall		;Load SWIA data and make tplot vars	
		mvn_mag_load,'l1_1sec'				;Load L1 B data at 1-sec
		mvn_mag_load,'l2_full'
		mk = mvn_spice_kernels(/load)			;Load SPICE Kernels

		
		;print,systime()
						;rotate B to MSO
	;mvn_swe_load_l2,/sumplot			;Load SWEA data and make tplot vars
	;orbclock=;TIC('orbit')
		maven_orbit_tplot
		spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for
		spice_vector_rotate_tplot,'mvn_B_1sec','MAVEN_MSO',check = 'MAVEN_SC_BUS'
	;;TOC,orbclock

		mvn_swia_part_moments,type=['cs','fs']


		mvn_swia_protonalphamoms_minf_mag

		options,'mvn_swics_velocity','SPICE_FRAME','MAVEN_SWIA'


		spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

		options,'mvn_swifs_velocity','SPICE_FRAME','MAVEN_SWIA'
		spice_vector_rotate_tplot,'mvn_swifs_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
			
		;done loading


		s=arrMag("mvn_B_1sec_MAVEN_MSO",yt="Magnetic Field")
		interpolator,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"

		ionsetup,/noarc,/doquick
		calc,"'mvn_swifs_pressure_proton'='tproton_Mag_interpolated'*'mvn_swifs_density_interpolated'"

		plottocyl,"POS_interpolated_(MARS_MSO)",newName="POS_MSO_CYL"
		zerothbow
		calcbeta,/iononly

		;;Finished computing raw stuff
		;TOC
		endif
		print,"==================="
		print,"Finished computing raw stuff"
		;TIC
		numpoints=numel(dat.t)
		get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
		xs=datB.x
		t0=xs[0]
		Bvec=datB.y

		get_data,'mvn_swics_velocity_MAVEN_MSO_interpolated',data=datVc
		get_data,'mvn_swifs_velocity_MAVEN_MSO_interpolated',data=datVf

		Vc=datVc.y
		Vf=datVf.y

		get_data,'shock0rho_interpolated',data=datS0rho
		get_data,'shock0x_interpolated',data=datS0x
		R_m = 3389.50D
		 x0  = 0.600*R_m
 		ecc = 1.026
  		L   = 2.081*R_m

		s0rho=datS0rho.y
		s0x=datS0x.y
		N=numel(xs)
		s0phi=xs*0.0
		sx0=s0x-x0
		srr=SQRT(sx0^2+s0rho^2)
		srt=Sqrt(s0rho^2+(sx0+ecc*srr)^2)
		;xx=datS0rho.x
		snxx=(ecc+sx0/srr)*srr/srt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT
		snrr=s0rho/srt
		SN=[[snrr],[fltarr(N)],[snxx]]
		get_data,"POS_MSO_CYL",data=datPosCyl

		posCyl=datPosCyl.y
		Xcyl=posCyl[*,2]
		RHOcyl=posCyl[*,0]

		get_data,'tproton_Mag_interpolated',data=datTion
		Tion=datTion.y
		get_data,'mvn_swifs_pressure_proton',data=datP
		Pion=datP.y

		get_data,"Plasma_Beta_proton",data=datBeta

		Betas=datBeta.y

		uindices=dat.uindices
		dindices=dat.dindices
		
		shocktimes=dat.t
		ishocks=shocktimes-t0

		help,shocktimes
		help,ishocks
		help,numpoints

		Buvecs=fltarr(numpoints,3)
		Bdvecs=fltarr(numpoints,3)

		Vuvecs_coarse=fltarr(numpoints,3)
		Vuvecs_fine=fltarr(numpoints,3)
		Vdvecs=fltarr(numpoints,3)

		PosConics=fltarr(numpoints,3)
		N_Conics=fltarr(numpoints,3)

		T_ions=fltarr(numpoints)
		Beta_ions=fltarr(numpoints)


		for el=0,numpoints-1 do begin
			print,el
			tshock=shocktimes[el]
			ishock=ishocks[el]
			ubeg=min(uindices[el,*])
			uend=min(uindices[el,*])
			dbeg=min(dindices[el,*])
			dend=min(dindices[el,*])

			T_ions[el]=mean(Tion[ubeg:uend])
			Beta_ions[el]=mean(Betas[ubeg:uend])
			Bum=[Mean(Bvec[ubeg:uend,0]),Mean(Bvec[ubeg:uend,1]),Mean(Bvec[ubeg:uend,2])]
			vumf=[Mean(Vf[ubeg:uend,0]),Mean(Vf[ubeg:uend,1]),Mean(Vf[ubeg:uend,2])]
			vumc=[Mean(Vc[ubeg:uend,0]),Mean(Vc[ubeg:uend,1]),Mean(Vc[ubeg:uend,2])]
			BD=[Mean(Bvec[dbeg:dend,0]),Mean(Bvec[dbeg:dend,1]),Mean(Bvec[dbeg:dend,2])]
			vd=[Mean(Vc[dbeg:dend,0]),Mean(Vc[dbeg:dend,1]),Mean(Vc[dbeg:dend,2])]
			cxx=Xcyl[ishock]
			crho=RHOcyl[ishock]
			D=Sqrt( (cxx-s0x)^2+(crho-s0rho)^2);+ophi^2)
			minDists=min(D,minloc)
			
		;print,minDists[i]
		;print,"[s0x,s0rho][i]=",[s0x[i],s0rho[i]]
		;print,"[Pxx[el],Prho[el]]=",[Pxx[el],Prho[el]]
		N_Conics[el,0]=snrr[minloc]
		N_Conics[el,1]=0.0;snxx[minloc]
		N_Conics[el,2]=snxx[minloc]
			closePos=[s0rho[minloc],0.0,s0x[minloc]]
			for j=0,2 do begin
				Buvecs[el,j]=Bum[j]
				Bdvecs[el,j]=BD[j]
				Vuvecs_coarse[el,j]=vumc[j]
				Vuvecs_fine[el,j]=vumf[j]
				Vdvecs[el,j]=vd[j]
				PosConics[el,j]=closePos[j]
			endfor
		endfor
		str_element,dat,'T_ion',T_ions,/add
		str_element,dat,'beta_ion',Beta_ions,/add
		str_element,dat,'Buvec',Buvecs,/add
		str_element,dat,'Bdvec',Bdvecs,/add
		str_element,dat,'Vuvec_fine',Vuvecs_fine,/add
		str_element,dat,'Vuvec_coarse',Vuvecs_coarse,/add
		str_element,dat,'Vdvec',Vdvecs,/add
		str_element,dat,'N_conic',N_Conics,/add
		str_element,dat,'pos_conic',Posonics,/add

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
	endfor


end
