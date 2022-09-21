pro aggramissingadd
	del_data,'OverVsMach_*'
	get_data,'up',data=datUp
	t=datUp.x
	N=numel(t)
	ndays=1
	srctxt="Documents/overmachDays.txt"


	read_strDATA,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)

;
	Bdvec=fltarr(N,3)
	Vdvec=fltarr(N,3)
	POSCONIC=fltarr(N,3)
	Bustd=fltarr(N)
	Bdstd=fltarr(N)
	NCONIC=fltarr(N,3)
	lat=fltarr(N)
	downdur=fltarr(N)
	updur=fltarr(N)
	profiledur=fltarr(N)
	overdur=fltarr(N)
	direction=fltarr(N)
	MMs=fltarr(N,4)
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
				;del_data,'*'
				continue
			endif
		endif
		
		if dat.t[0] ge max(t) then break
		date=name.remove(0,10)
		t00=time_double(date)
		for j=0, numel(dat.t)-1 do begin
			tt=dat.t[j]
			;if total(tt eq ts) eq 0 then continue
			w=(where(tt eq t,wcount))[0]
			if wcount eq 0 then continue
			dir=dat.direction[j]
			direction[w]=dir
			for kk=0,3 do MMs[w,kk]=dat.MMs[j,kk]
				;imax=dat.imaxs[j]
			;imin=dat.imins[j]
			;;ishock=tt-t00
			;profiledur[w]=abs(imax-imin)
			;if dir eq 1 then begin
				;updur[w]=ishock-imin
				;downdur[w]=imax-ishock
			;endif else begin
				;if imax gt imin then begin
				;	updur[w]=imax-ishock
				;	downdur[w]=ishock-imin
				;endif else begin
				;	updur[w]=ishock-imax
				;	downdur[w]=imin-ishock
				;endelse
			
			;endelse
			;overdur[w] =total(/nan, (dat.overshoot[min([imin,imax]):max([imin,imax])]) gt 0) 
			;lat[w]=dat.lat[j]
			;Bdstd[w]=dat.DOWNSTD[j]
			;Bustd[w]=dat.UPSTD[j]
			;Vdvec[w,*]=dat.vdvec[j,*]
			;POSCONIC[w,*]=dat.POS_CONIC[j,*]
			;Bdvec[w,*]=dat.Bdvec[j,*]
			;NCONIC[w,*]=dat.N_CONIC[j,*]
		endfor
		del_data,name
		;return
	endfor
	del_data,'OverVsMach_*'
;	store_data,'lat',data={x:t,y:lat,ytitle:'Latitude MSO [rad]',YN:'Latitude',fn:'lat',degree:[0],radian:[1],vec:[0],binsize:[10*!pi/180]}
;	store_data,'Vdvec',data={x:t,y:Vdvec,ytitle:"Vdvec",YN:'Vdvec',fn:'vdvec',degree:[0],radian:[0],vec:[1],binsize:[2]}			
;	store_data,'Bdvec',data={x:t,y:Bdvec,ytitle:"Bdvec",YN:'Bdvec',fn:'bdvec',degree:[0],radian:[0],vec:[1],binsize:[2]}
;	store_data,'Bustd',data={x:t,y:Bustd,ytitle:'$\sigma_{BU}$ [nT]',YN:'$\sigma_{BU}$',fn:'bustd',degree:[0],radian:[0],vec:[0],binsize:[2]}
;	store_data,'Bdstd',data={x:t,y:Bdstd,ytitle:'$\sigma_{BD}$ [nT]',YN:'$\sigma_{BD}$',fn:'bdstd',degree:[0],radian:[0],vec:[0],binsize:[2]}
;	store_data,'POSCONIC',data={x:t,y:POSCONIC,ytitle:'POSCONIC',YN:'POSCONIC',fn:'posconic',degree:[0],radian:[0],vec:[1],binsize:[200]}
;	store_data,'n_conic_cyl',data={x:t,y:NCONIC,ytitle:'NCONIC',YN:'NCONIC',fn:'n-conic-cyl',degree:[0],radian:[0],vec:[1],binsize:[.1]}

;	store_data,'profile_duration',data={x:t,y:profiledur,ytitle:'shock profile duration [sec]',YN:'Profile duration',fn:'profiledur',degree:[0],radian:[1],vec:[0],binsize:[120]}
;	store_data,'overshoot_duration',data={x:t,y:profiledur,ytitle:'overshoot calculated duration [sec]',YN:'overshoot duration',fn:'overshootdur',degree:[0],radian:[1],vec:[0],binsize:[60]}
;	store_data,'upstream_duration',data={x:t,y:updur,ytitle:'upstream profile duration [sec]',YN:'Upstream Profile duration',fn:'updur',degree:[0],radian:[1],vec:[0],binsize:[60]}
;	store_data,'downstream_duration',data={x:t,y:downdur,ytitle:'downstream profile duration [sec]',YN:'downstream Profile duration',fn:'downdur',degree:[0],radian:[1],vec:[0],binsize:[60]}
;	store_data,'shock_direction',data={x:t,y:updur,ytitle:'inbound:1, outbound:-1',YN:'Crossing Direction',fn:'direction',degree:[0],radian:[1],vec:[0],binsize:[1]}
	store_data,'MMs',data={x:t,y:MMs}
	tplot_element,'invMM1s','y',1./(MMs[*,1]),/add	
	;fitWidth=
	tplot_element,'down','y',down
tplot_element,'up','y',up
tplot_element,'invMM1s','y',invMM1
tplot_element,'invMM1s','x',t

m3=(down+up)/2.
m0=(down-up)/2.
RampScale=invMM1*m3/m0
store_data,'RampScale',data={x:t,y:RampScale,ytitle:'Ramp Scale Length $L=m_3/(m_0 m_1)$ [sec]',YN:'Ramp Scale',fn:'RampScale',binsize:10,radians:[0],degree:[0]} 
end
