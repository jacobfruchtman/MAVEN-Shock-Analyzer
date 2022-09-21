pro locateclosestconic

	R_m = 3389.50D  ; +/- 0.2
 	x0  = 0.600*R_m
 	ecc = 1.026
  	L   = 2.081*R_m
	get_data,"POS_MSO_CYL",data=datPosCyl

	get_data,"shocks",data=datS

	shocks=datS.y

	
	posCyl=datPosCyl.y

	Prho=posCyl[*,0]
	Pxx=posCyl[*,2]
	Pphi=posCyl[*,1]
	post=datPosCyl.x
	get_data,'shock0_interpolated',data=datS0
	get_data,'shock0_normals',data=datS0n

	S0=datS0.y
	S0n=datS0n.y
	N=size(shocks,/n_el)
	dists=fltarr(N)
	s0x=S0[*,2]
	s0rho=S0[*,0]
	sx0=s0x-x0
	GG=where(shocks ne 0, gcount)
	minDists=GG*0.0
	s0locs=GG*0.0
	dotprods=GG*0.0
	backwardsLocs=GG*0.0

	closestConic=fltarr(N,3)
	closestNorm=fltarr(N,3)
	for i=0,gcount-1 do begin
		el=GG[i]
		ox=Pxx[el]
		ophi=Pphi[el]
		orho=Prho[el]
		;oy=oposy[el]
		;oz=oposz[el]

		D=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)
		minDists[i]=min(D,minloc)
		s0locs[i]=minloc

		closestConic[el,*]=S0[minloc,*]
		closestNorm[el,*]=S0n[minloc,*]
		print,minDists[i]
		print,"[s0x,s0rho][i]=",[s0x[i],s0rho[i]]
		print,"[Pxx[el],Prho[el]]=",[Pxx[el],Prho[el]]

		dists[el]=minDists[i]
	endfor

	tplot_element,"dateDataCurrent",'shock0Dist',minDists,/add
	store_data,"shock0dist",data={x:post,y:dists,ytitle:"distance from conic [km]"}

	store_data,"closestConic",data={x:post,y:closestConic,ytitle:"Conic Position"}
	store_data,"closestConicNormal",data={x:post,y:closestNorm,ytitle:"Normal vector of closest conic"}
end
