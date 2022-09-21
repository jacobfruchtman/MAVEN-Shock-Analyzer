pro vel2shockrest,pltv=pltv,newname=newname
	if not keyword_set(newname) then newname='Vrest'
	if not keyword_Set(pltv) then pltv='mvn_swics_velocity_MAVEN_MSO_interpolated'
	get_data,pltv,data=datv
	V=datv.y
	;get_data,'mvn_swics_density_interplolated',data=datN
	;Nion=datN.y
	t=datv.x
	date=(time_string(t[0])).remove(-9)
	get_data,'shocks',data=dats
	if total(dats.y) eq 0 then return
	get_data,'shock_locs',data=datsl
	shock_locs=datsl.y
	;get_data,'OverVsMach_'+date,data=datdat
	N=numel(t)
	tplot_element,'Varb_SH','y',Varb_SH
	tplot_element,'Shock_Normal_AVG','y',N_AVG
	Vrest=fltarr(N,3)
	for i=0,N-1 do begin
		if shock_locs[i] eq -1 then continue
		ishock=shock_locs[i]
		N_SN=N_AVG[ishock,*]
		Vmso=V[i,*]
		Varb=Varb_SH[ishock]
		;Vrest[i,*]=
		for k=0,2 do Vrest[i,k]=Vmso[k]-Varb*N_SN[k]
	endfor
	store_data,newname,data={x:t,y:Vrest,ytitle:'Vrest [km/s]'}
	options,newname,'LABELS',['x','y','z']
	options,newname,'LABFLAG',1

	options,newname,'colors',['r','g','b']
end
