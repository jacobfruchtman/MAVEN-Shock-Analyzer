function cp22, v1,v2
	;;print,"v1,v2"
	;;print,v1,v2
	
	px=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
	py=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
	pz=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
	;;print,"px,py,pz=",px,py,pz
	product=[px,py,pz]
	;;print,product
	return, product
END

pro manualshocknorm

	get_data,'mvn_B_1sec_MAVEN_MSO',data=datB
	get_data,"mvn_swics_velocity_MAVEN_MSO_interpolated",data=datVc	
	get_data,"mvn_swifs_velocity_MAVEN_MSO_interpolated",data=datVf
	get_data,"POS_MSO_CYL",data=datPosCyl
	x=datB.x
	B=datB.y

	Vf=datVf.y
	Vc=datVc.y
	time=[-1]

	while N_elements(time) ne 2 do begin
	print,"grab upstream interval"
	ctime,time
	endwhile
	tup=mean(time)
	for i=0,1 do time[i]-=x[0]	

	Bup=instantcomponentaverage(B,0,0,tbound=time)
	Vup=instantcomponentaverage(Vf,0,0,tbound=time)

	time=[-1]

	while N_elements(time) ne 2 do begin
	print,"grab downstream interval"
	ctime,time
	tdown=mean(time)
	tshock=mean([tup,tdown])
	dx=min(abs(datPosCyl.x-tshock),shockloc)
	ourpos=datPosCyl.y[shockloc,*]
	endwhile
	Bdown=instantcomponentaverage(B,0,0,tbound=time)
	Vdown=instantcomponentaverage(Vc,0,0,tbound=time)

	dB=Bdown-Bup
	dV=Vdown-Vup

;					      (B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;					    |(B_d x B_u)x(ΔB)|

;					       (B_u x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx1 =± -----------------
;					     |(B_u x ΔV)x(ΔB)|

;					       (B_d x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx2 =± -----------------
;					     |(B_d x ΔV)x(ΔB)|

;					       (ΔB x ΔV)x(ΔB)						
; The shocknormal vector is given by n_mx3 =± -----------------
;					     |(ΔB x ΔV)x(ΔB)|



	N_MC=normalize(cp22(cp22(Bdown,Bup),dB))
	if N_MC[0] lt 0 and ourpos[2] gt 0 then N_MC*=-1
	print,N_MC
	N_MX1=normalize(cp22(cp22(Bup,dV),dB))
	if N_MX1[0] lt 0 and ourpos[2] gt 0 then N_MX1*=-1
	print,N_MX1
	N_MX2=normalize(cp22(cp22(Bdown,dV),dB))
	if N_MX2[0] lt 0 and ourpos[2] gt 0 then N_MX2*=-1
	print,N_MX2
	N_MX3=normalize(cp22(cp22(dB,dV),dB))
	if N_MX3[0] lt 0 and ourpos[2] gt 0 then N_MX3*=-1
	print,N_MX3

	N_AVG=fltarr(3)
	for i=0,2 do N_AVG[i]=mean([N_MC[i],N_MX1[i],N_MX3[i],N_MX3[i]],/nan)
	print,N_AVG

	return,arcos(dotproduct(N_AVG,normalize(Bup)))
end

