pro colorflagger
	get_data,'regid',data=datReg,alim=lim
	get_data,'AA_flag',data=datA
	get_data,'BB_flag',data=datB
	get_data,'CC_flag',data=datC
	get_data,'DD_flag',data=datD
	get_data,'foot_start_inbound',data=datF
	x=datF.x
	CC=datC.y*1
	DD=datD.y*3
	FF=datF.y*4
	AA=datA.y*5
	BB=datB.y*6
	N=n_elements(CC)
	ABCDF=AA+BB+CC+DD+FF
	w=where(abcdf ne 0,nw)
	FLG=fltarr(N)
	FLG[0:w[0]]=ABCDF[w[0]]-1
	for i=0,nw-2 do FLG[w[i]:w[i+1]]=ABCDF[w[i]] mod 6
	FLG[w[-1]:*]=ABCDF[w[-1]] mod 6
	print,FLG[w[0]]
	print,total(FLG ne 0)
	;FLG[w]=0
	foreach el,w do if total(FLG[el-1:el+1] eq 0) eq 0 then FLG[el-2:el+2]=0
	y=[[FLG],[FLG]]
	store_data,'ABCDF_flag',data={x:x,y:y,v:[0,1],spec:[1],ytitle:' '},dlim=lim
	help,datReg
	print,datReg.v
	
	
	get_data,'bAA_flag',data=datD
	get_data,'bBB_flag',data=datC
	get_data,'bCC_flag',data=datB
	get_data,'bDD_flag',data=datA
	get_data,'foot_start_outbound',data=datF
	x=datF.x
	CC=datC.y*1
	DD=datD.y*2
	FF=datF.y*3
	AA=datA.y*5
	BB=datB.y*6
	N=n_elements(CC)
	ABCDF=AA+BB+CC+DD+FF
	w=where(abcdf ne 0,nw)
	FLG=fltarr(N)
	FLG[0:w[0]]=ABCDF[w[0]]-1
	for i=0,nw-2 do FLG[w[i]:w[i+1]]=ABCDF[w[i]] mod 6
	FLG[w[-1]:*]=ABCDF[w[-1]] mod 6
	;FLG=reverse(FLG)
	;FLG[w]=0
	foreach el,w do  if total(FLG[el-1:el+1] eq 0) eq 0 then  FLG[el-2:el+2]=0
	print,FLG[w[0]]
	print,total(FLG ne 0)
	y=[[FLG],[FLG]]
	store_data,'bABCDF_flag',data={x:x,y:y,v:[0,1],spec:[1],ytitle:' '},dlim=lim

	
	get_data,'ascend_begin',data=datC
	get_data,'ascend_end',data=datD
	get_data,'descend_begin',data=datA
	get_data,'descend_end',data=datB
	;get_data,'foot_start_outbound',data=datF
	x=datC.x
	CC=datC.y*1
	DD=datD.y*2

	AA=datA.y*4
	BB=datB.y*6
	N=n_elements(CC)
	ABCDF=AA+BB+CC+DD;+FF
	w=where(abcdf ne 0,nw)
	FLG=fltarr(N)
	FLG[0:w[0]]=ABCDF[w[0]]-1
	for i=0,nw-2 do FLG[w[i]:w[i+1]]=ABCDF[w[i]] mod 6
	FLG[w[-1]:*]=ABCDF[w[-1]] mod 6
	;FLG=reverse(FLG)
	print,FLG[w[0]]
	print,total(FLG ne 0)
		FLG[w]=0
	y=[[FLG],[FLG]]
	store_data,'ABCDF0_flag',data={x:x,y:y,v:[0,1],spec:[1],ytitle:' '},dlim=lim
	
	x=datF.x
	N=numel(x)
	tplot_element,'upstream_flags','y',uis
	tplot_element,'mvn_B_1sec_MAVEN_MSO_Mag','y',B
	Bup=fltarr(N)
	sts=where(uis[*,0] ne 0,ns)
	ens=where(uis[*,1] ne 0)
	for i=0,ns-1 do for k=sts[i],ens[i] do Bup[k]=B[k]
	store_data,'Bup-colored',data={x:x,y:Bup}
	options,'Bup-colored','colors','g'
	
	tplot_element,'downstream_interval','y',dreg
	dreg*=B/dreg
	store_data,'Bdown-colored',data={x:x,y:dreg}
	options,'Bdown-colored','colors','c'
	options,'bABCDF_flag','panel_size',.05
	options,'ABCDF_flag','panel_size',.05
	options,'ABCDF0_flag','panel_size',.05
	store_data,'Boudf',data='mvn_B_1sec_MAVEN_MSO_Mag overshoot Bup-colored Bdown-colored Franken_fitted'
end
