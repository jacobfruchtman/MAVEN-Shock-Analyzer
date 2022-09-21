
function isArray,x
	
	nell=size(v1,/N_ELEMENTS)
	;print,nell
	if(nell EQ 3) THEN return,0 ELSE return,1	

END


function vecCrossProduct, v1,v2
	;;print,"v1,v2"
	;;print,v1,v2
	
	px=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
	py=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
	pz=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
	;;print,"px,py,pz=",px,py,pz
	product=[[px],[py],[pz]]
	;;print,product
	return, product
END

function arrayCrossProduct, v1,v2
	help,v1
	help,v2
	px=v1[*,2-1]*v2[*,3-1]-v1[*,3-1]*v2[*,2-1]
	py=v1[*,3-1]*v2[*,1-1]-v1[*,1-1]*v2[*,3-1]
	pz=v1[*,1-1]*v2[*,2-1]-v1[*,2-1]*v2[*,1-1]
	
	product=transpose([transpose(px),transpose(py),transpose(pz)])
	
	return, product
END


function cp, v1,v2
	;product=0
	nel1=size(v1,/N_ELEMENTS)
	;print,v1,v2
	;print,nel1
	if nel1 eq 3 THEN product= vecCrossProduct(v1,v2) ELSE product=arrayCrossProduct(v1,v2)

	return, product
End




FUNCTION VECSQUARE,vv
		;"Programs can't be compiled from single statement mode." What?
		;"Variable is undefined: VV." WHAT?
		sqr=0
		if(isArray(vv))THEN BEGIN
		
	    FOR i = 0, 2 DO BEGIN	
	   	sqr=sqr+vv[*,i]*vv[*,i]
	
		ENDFOR
		ENDIF ELSE BEGIN
	    	FOR i = 0, 2 DO BEGIN	
	   			sqr=sqr+vv[i]*vv[i]
	
			ENDFOR
		ENDELSE
	    RETURN, sqr
	
END

FUNCTION VECMAG, vv
		mag=0
		mag=SQRT(VECSQUARE(vv))
	
		RETURN, mag
END



FUNCTION VECDIV,vv,denom
		q=0
		if(isArray(vv))THEN BEGIN
			q=TRANSPOSE([transpose(vv[*,0]/denom),transpose(vv[*,1]/denom),transpose(vv[*,2]/denom)])
		ENDIF ELSE BEGIN
			q=TRANSPOSE([vv[0]/denom,vv[1]/denom,v[2]/denom])
		ENDELSE
		
		RETURN, q
END


function icalc,x,dt

	N=size(x,/n_el)

	di=1

	for i=1,N-1 do begin
		if x[i]-x[0] lt dt then continue
		di=i
		break
	endfor
	return, di

end


function normalizedcomponentaverage,Ns

	Navg=[.0,.0,.0]
	;;print,Ns
	Ns0=Ns
	if TYPENAME(Ns) eq "LIST" then Ns=Ns.toarray()

	for i=0, 2 do Navg[i]= mean(Ns[*,0,i],/nan)

	Nnorm=norm(Navg)

		

	if Nnorm ne 0.0 then begin
		for j=0, 2 do Navg[j]=Navg[j]/Nnorm		
		return, Navg
	endif else begin
		if total(Ns0[-1]) ne 0 then return,Ns0[-1]
		if total(Ns0[-2]) ne 0 then return,Ns0[-2]
		if total(Ns0[-3]) ne 0 then return,Ns0[-3]
		if total(Ns0[-4]) ne 0 then return,Ns0[-4]
		return,[0.0,0.0,0.0] 

	endelse
end

function shocknorm, a , b, c
	sn=b*0.0
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


;	;print,"bd=",bd
;	;print,"bu=",bu
	
	nu= cp(cp(a,b),c)
	;;print,nu
	mag=sqrt(nu[0]^2+ nu[1]^2+nu[2]^2)
	;;print,mag
	for el=0,2 do nu[el]=nu[el]/mag
	;sn=vecdiv(nu,norm2(nu));vecmag(nu));nu/norm(nu);vecdiv(nu,norm2(nu));vecmag(nu))
	;;print,"nu=",nu
	;;;print,"norm(nu)=",norm(nu)
	;;print,"Sqrt(Total(nu^2))=",Sqrt(Total(nu^2))
	return, nu;sn
end


function shocknormMC, bu , bd
	sn=bu*0.0
;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|
;	;print,"bd=",bd
;	;print,"bu=",bu
	db=bd-bu
	nu= cp(cp(bd,bu),db)
	mag=sqrt(nu[0]^2+ nu[1]^2+nu[2]^2)
	for el=0,2 do nu[el]=nu[el]/mag
	;sn=vecdiv(nu,vecmag(nu));nu/norm(nu);vecdiv(nu,vecmag(nu))
;	;print,"sn=",sn

	return, nu
end

function shocknormMX1, bu , db,dv
	sn=bu*0.0
;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|
;	;print,"bd=",bd
;	;print,"bu=",bu
	db=bd-bu

	nu= cp(cp(bd,bu),db)
	mag=sqrt(nu[0]^2+ nu[1]^2+nu[2]^2)
	for el=0,2 do nu[el]=nu[el]/mag
	;sn=vecdiv(nu,vecmag(nu));nu/norm(nu);vecdiv(nu,vecmag(nu))
;	;print,"sn=",sn

	return, nu
end


function vecDotProduct, a, b
	;;print,"a=",a
	;;print,"b=",b
	return, a[0]*b[0]+a[1]*b[1]+a[2]*b[2]
end

function arrayDotProduct, a , b
	return, a[*,0]*b[*,0]+a[*,1]*b[*,1]+a[*,2]*b[*,2]
end


;functon vDP, a, b
;	return, vecDotProduct(a,b)
;end

function AngleCalc, n, BU

	magB= SQRT(BU[0]^2+BU[1]^2+BU[2]^2);norm(BU);vecMag(BU)
	num=vecDotProduct(n,BU)
	;;print,"BU=",BU
	;;print,"n=",n
	
	;;print,"num=",num
	;;print,"magB",magB
	
	return, acos(num/magB)
end



pro shockNormCalc;, manualDown=manualDown;,plt=plt

	useDownstream=1

	;if keyword_set(manualDown) then useDownStream=0

	;if keyword_set(plt) then
;
;		get_data,plt+"_shocks_inbound",data=datin
;		get_data,plt+"_shocks_outbound",data=datout


;	endif else begin

		;get_data,"B_fitted10_shocks_inbound",data=datin
		;get_data,"B_fitted10_shocks_outbound",data=datout

;	endelse
		
	;print,"SHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALC"
	;print,"SHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALC"
	;print,"SHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALC"
	;print,"SHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALCSHOCKNORMCALC"
get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout
	if total(datin.y + datout.y) eq 0 then return
	;get_data,"BF_shockA",data=datBFA,limits=limBFA
	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout




	;get_data,"BF_shockD",data=datBFD,limits=limBFD

	get_data,"mvn_B_1sec_MAVEN_MSO",data=datB
	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=datBmag

	;get_data,'Franken_fittedderiv',data=datDer

	get_data,'Franken_fitted_inbound',data=datffin
	get_data,'Franken_fitted_outbound',data=datffout
	;get_data,'Franken_fittedderiv_updown',data=datUD

	get_data,"mvn_swics_velocity_MAVEN_MSO_interpolated",data=datV
	get_data,"swics_velocity_interpolated_Mag",data=datVmag

	get_data,"upstream_indices_inbound",data=datui
	ui=datui.y

	get_data,"downstream_indices_inbound",data=datdi

	get_data,"POS_MSO_CYL",data=datPosCyl

	get_data,'closestConic',data=datS0
	get_data,'closestConicNormal',data=datS0n
	PosCyl=datPosCyl.y
	conic=datS0.y
	S0n=datS0n.y
	di=datdi.y



	;UD=datUD.y

	;BFA=datBFA.y
	;BFD=datBFD.y
	BB=datB.y
	;Bder=datB.y
	get_data,"POS_interpolated_(MARS_MSO)",data=datPos,alim=limPos
	pos=datPos.y
	;xpos=datPos.x
	xx=datB.x
	N=size(xx,/n_el)
	shock0Acc=fltarr(N)
	;yposNewX=interpol(pos[*,0],xpos,xx)
	;yposNewY=interpol(pos[*,1],xpos,xx)
	;yposNewZ=interpol(pos[*,2],xpos,xx)
	;posNew=[[yposNewX],[yposNewY],[yposNewZ]]

	;datPosN=datB
	;datPosN.y=posNew
	;datPosN.x=xx

	;store_data,"POS_interpolated_(MARS_MSO)",data=datPosN,dlim=limPos
	get_data,"POS_interpolated_(MARS_MSO)",data=datPos,alim=limPos
	Pos=datPos.y
	;help,datV
	;ov=datV.y
	;vt=datV.x

	;ovx=ov[*,0]
	;ovy=ov[*,1]
	;ovz=ov[*,2]

	;vx=interpol(ovx,vt,xx)
	;vy=interpol(ovy,vt,xx)
	;vz=interpol(ovz,vt,xx)

	vp=datV.y;[[vx],[vy],[vz]]

	;npos=Size(xpos,/n_el)
;;;;;;;;note: BB[t,*] is the B vector at time t
;;;;;;;;	  BB[*,k] is the k component of B, B_k, for all times

	B_D=fltarr(N,3)
	B_U=fltarr(N,3)

	v_u=vp*0.0
	v_d=vp*0.0	
	


	ins=datin.y
	outs=datout.y

	;print,"for sanity check, ,[mean(BFD),stddev(BFD),max(BFD),min(BFD)]=",[mean(BFD),stddev(BFD),max(BFD),min(BFD)]

;	xsFA=datBFA.x
;	xsFD=datBFD.x
	xs=datB.x

	;print,"N=",N
	flagstart=0
	flagon=0
	flagend=0

	Bsum=BB[0,*]*0.0

	n_SN=fltarr(N,3)
	n_SN_MX1=fltarr(N,3)
	n_SN_MX2=fltarr(N,3)
	n_SN_MX3=fltarr(N,3)
	n_SN_AVG=fltarr(N,3)
	n_SN_best=fltarr(N,3)

	n_SN_Accs=fltarr(N)
	n_SN_MX1_Accs=fltarr(N)
	n_SN_MX2_Accs=fltarr(N)
	n_SN_MX3_Accs=fltarr(N)

	n_SN_Accs_Adj=fltarr(N)
	n_SN_MX1_Accs_Adj=fltarr(N)
	n_SN_MX2_Accs_Adj=fltarr(N)
	n_SN_MX3_Accs_Adj=fltarr(N)
	n_SN_AVG_Accs=fltarr(N)
	n_SN_best_Accs=fltarr(N)
	n_SN_AVG_Accs_Adj=fltarr(N)


	n_SN_CYL=fltarr(N,3)
	n_SN_MX1_CYL=fltarr(N,3)
	n_SN_MX2_CYL=fltarr(N,3)
	n_SN_MX3_CYL=fltarr(N,3)
	n_SN_AVG_CYL=fltarr(N,3)
	n_SN_best_CYL=fltarr(N,3)

	SHOCKANGLE_MC=fltarr(N)
	SHOCKANGLE_MX1=fltarr(N)
	SHOCKANGLE_MX2=fltarr(N)
	SHOCKANGLE_MX3=fltarr(N)
	SHOCKANGLE_AVG=fltarr(N)
	SHOCKANGLE_best=fltarr(N)

	flowANGLE=fltarr(N)
	nflowANGLE=fltarr(N)

	imin=icalc(xs,60)

	inLocs=where(ins ne 0,numin)
	outlocs=where(outs eq 1,numout)

	;print,"inLocs=",inLocs
	;print,"outLocs=",outLocs




	
;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|

; where ΔB=B_d-B_u
	atFlag=0


	upLocF=fltarr(N,3)
	upLocB=fltarr(N,3)
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	;print, "			FORWARD LOOP				    "
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	;for i=10*imin,N-1-10*imin do begin
	for el=0,numin -1 do begin
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[inLocs[el]]
				PRINT,'ERROR in shockNormCalc during el=',el
				;errorloc=""
			
				errmsg=["error in shockNormCalc inbound during el="+strtrim(el,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel
				;errorloc=""
				continue
			endif

		i=inLocs[el]

		ubeg=ui[i,0]
		uend=ui[i,1]
		BD=0
		BU=0
		vd=0
		vu=0		

		atFlag=0
		Bsum=0
		;if(BFD[i] eq BFA[i]) or (BFA[i] ne 0) then continue
		;;print,"i=",i
		;if((Bder[i] gt Bder[i-1] ) and (Bder[i] gt Bder[i-1])) then begin
		;if (((UD[i] eq 1) and (UD[i-1] eq 0) ) and (Bder[i] gt 0)) then begin
		;if((Bder[i] gt Bder[i-1] ) and (Bder[i] gt Bder[i-1])) and (BFD[i] ne 0 ) and (BFD[i-imin/12] eq 0) then begin
		;if(BFD[i] ne 0 ) and (BFD[i-imin/12] eq 0) then begin
			atFlag=1
			flagstart=i
			;establish B_U
			iret=ubeg;i-4*imin;ubeg;i-4*imin
			;print,"[iret,ubeg,uend]=",[i-4*imin,ubeg,uend]
		;	for j=iret,iret+imin do Bsum+=BB[j,*]

		;	B_avg=vecdiv(Bsum,imin)
		;	Bsum=BB[0,*]*0.0
			Bum=[Mean(BB[ubeg:uend,0]),Mean(BB[ubeg:uend,1]),Mean(BB[ubeg:uend,2])]
			vum=[Mean(vp[ubeg:uend,0]),Mean(vp[ubeg:uend,1]),Mean(vp[ubeg:uend,2])]
			;print,time_string(xs[ubeg])
			;print,time_string(xs[uend])
			;print,"Bum=",Bum
			;print,"vum=",vum
			;Bum=[Mean(BB[i-4*imin:i-3*imin,0]),Mean(BB[i-4*imin:i-3*imin,1]),Mean(BB[i-4*imin:i-3*imin,2])]
			;vum=[Mean(vp[i-4*imin:i-3*imin,0]),Mean(vp[i-4*imin:i-3*imin,1]),Mean(vp[i-4*imin:i-3*imin,2])]
			for j=iret,i+10 do begin

				for k=0,2 do begin
					B_U[j,k]=Bum[k]
					v_U[j,k]=vum[k]
				endfor
				;B_U[j,*]=Bum
				;v_U[j,*]=vum
				;print,Bum
				;print,vum
				;help,Bum
				;help,vum
				flowANGLE[j]=acos(dotproduct(Bum,vum)/(norm(Bum)*norm(vum)))

			endfor

			;establish B_d


			;this will be an utterly bullshit implementation usign a loop ending with i itself being updated



			if useDownstream then begin
				dbeg=di[i,0]
				dend=di[i,1]
				;print,numel(BB[*,0]),dbeg,dend
				BD=[Mean(BB[dbeg:dend,0]),Mean(BB[dbeg:dend,1]),Mean(BB[dbeg:dend,2])]
				vd=[Mean(vp[dbeg:dend,0]),Mean(vp[dbeg:dend,1]),Mean(vp[dbeg:dend,2])]
				BU=Bum;B_U[i,*]

				vu=vum;v_u[i,*]

				dB=BD-BU
				
				dv=vd-vu
				print,time_string(xx[i])
				print,"BD=",BD
				print,"BU=",BU
				print,"VD=",VD
				print,"VU=",VU
				;print,"BU=",BU,B_U[i,*]
				;print,"B_D=",BD
				;print,"dB=",dB
				;print,"dv=",dv
				;n_SN[i,*]=shocknormMC(BU,BD)

				SN=shocknorm(BD,BU,dB)
				;n_SN[i,*]=
				SN_MX1 =shocknorm(BU,dv,dB)
				SN_MX2=shocknorm(BD,dv,dB)
				SN_MX3=shocknorm(dB,dv,dB)
				;print,"norm(SN)=",norm(SN)
				;print,"norm(SN_MX1)=",norm(SN_MX1)
				;print,"norm(SN_MX2)=",norm(SN_MX2)
				;print,"norm(SN_MX3)=",norm(SN_MX3)
				;n_SN_MX1[i,*]=shocknorm(BU,dv,dB)
				;n_SN_MX2[i,*]=shocknorm(BD,dv,dB)
				;n_SN_MX3[i,*]=shocknorm(dB,dv,dB)

				
				;;print,'SN[',i,",*]=",SN
				
				n_SNcyl=carttocyl_instant(SN,Pos[i,*])
				n_SN1cyl=carttocyl_instant(SN_MX1,Pos[i,*])
				n_SN2cyl=carttocyl_instant(SN_MX2,Pos[i,*])
				n_SN3cyl=carttocyl_instant(SN_MX3,Pos[i,*])


				;;print,"norm(n_SNcyl)=",norm(n_SNcyl)
				;;print,"norm(n_SN1cyl)=",norm(n_SN1cyl)
				;;print,"norm(n_SN2cyl)=",norm(n_SN2cyl)
				;;print,"norm(n_SN3cyl)=",norm(n_SN3cyl)

				n_SN_Acc=vecDotProduct(n_SNcyl,S0n[i,*])
				n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,S0n[i,*])
				n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,S0n[i,*])
				n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,S0n[i,*])

				n_SN_Acc_Adj=(n_SN_Acc-(n_SNcyl[1]*S0n[i,1]))/(1-n_SNcyl[1]^2)
				n_SN_MX1_Acc_Adj=(n_SN_MX1_Acc-(n_SN1cyl[1]*S0n[i,1]))/(1-n_SN1cyl[1]^2)
				n_SN_MX2_Acc_Adj=(n_SN_MX2_Acc-(n_SN2cyl[1]*S0n[i,1]))/(1-n_SN2cyl[1]^2)
				n_SN_MX3_Acc_Adj=(n_SN_MX3_Acc-(n_SN3cyl[1]*S0n[i,1]))/(1-n_SN3cyl[1]^2)

				if ((n_SN_Acc lt 0)  or (n_SN_Acc_Adj lt 0) or (n_SNcyl[2] lt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) lt .15 and Pos[i,0] gt 0) or (n_SNcyl[0] lt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) gt .85 and Pos[i,0] gt 0)) and not ((n_SNcyl[2] gt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) gt .15 and Pos[i,0] gt 0 and n_SNcyl[0] gt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) lt .85))then begin;or (  sign(n_SNcyl[2]*S0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN[0] gt 0)  then begin;and (SN[0] lt 0.0) then begin
					;;print,"n_SN_Acc=",n_SN_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
					;;print,"n_SNcyl=",n_SNcyl
					;print,SN
 					SN=(-1.0)*SN
					;print,SN
					;;print,"n_SNcyl=",n_SNcyl
					n_SNcyl=carttocyl_instant(SN,Pos[i,*])
					n_SN_Acc=vecDotProduct(n_SNcyl,S0n[i,*])
					;print,"new n_SN_Acc=",n_SN_Acc
				endif
				if ((n_SN_MX1_Acc lt 0)  or (n_SN_MX1_Acc_Adj lt 0) or (n_SN1cyl[2] lt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) lt .15 and Pos[i,0] gt 0) or (n_SN1cyl[0] lt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) gt .85 and Pos[i,0] gt 0)) and not ((n_SN1cyl[2] gt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) gt .15 and Pos[i,0] gt 0 and n_SN1cyl[0] gt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) lt .85)) then begin; or (  sign(n_SN1cyl[2]*S0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX1[0] gt 0) then begin;and (SN_MX1[0] lt 0.0) then begin
					;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
					;;print,"n_SN1cyl=",n_SN1cyl
					;print,SN_MX1
					SN_MX1=(-1.0)*SN_MX1
					;print,SN_MX1
					n_SN1cyl=carttocyl_instant(SN_MX1,Pos[i,*])
					;;print,"n_SN1cyl=",n_SN1cyl
					n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,S0n[i,*])
					;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
				endif

				if ((n_SN_MX2_Acc lt 0)  or (n_SN_MX2_Acc_Adj lt 0) or (n_SN2cyl[2] lt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) lt .15 and Pos[i,0] gt 0) or (n_SN2cyl[0] lt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) gt .85 and Pos[i,0] gt 0)) and not ((n_SN2cyl[2] gt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) gt .15 and Pos[i,0] gt 0 and n_SN2cyl[0] gt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) lt .85)) then begin;or (  sign(n_SN2cyl[2]*S0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX2[0] gt 0) then begin;and (SN_MX2[0] lt 0.0) then begin
					;;print,"n_SN_MX2_Acc=",n_SN_MX2_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
					;;print,"n_SN2cyl[i,*]=",Transpose(n_SN2cyl[i,*])
					SN_MX2=(-1.0)*SN_MX2
					n_SN2cyl=carttocyl_instant(SN_MX2,Pos[i,*])
					n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,S0n[i,*])
					;;print,"n_SN2cyl=",n_SN2cyl
					;print,"new n_SN_MX2_Acc=",n_SN_MX2_Acc
				endif
				if ((n_SN_MX3_Acc lt 0)  or (n_SN_MX3_Acc_Adj lt 0) or (n_SN3cyl[2] lt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) lt .15 and Pos[i,0] gt 0) or (n_SN3cyl[0] lt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) gt .85 and Pos[i,0] gt 0)) and not ((n_SN3cyl[2] gt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) gt .15 and Pos[i,0] gt 0 and n_SN3cyl[0] gt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) lt .85)) then begin; or (  sign(n_SN3cyl[2]*S0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX3[0] gt 0) then begin;and (SN_MX3[0] lt 0.0) then begin
					;;print,"n_SN_MX3_Acc=",n_SN_MX3_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
					;;print,"n_SN3cyl=",n_SN3cyl
					SN_MX3=(-1)*SN_MX3
					n_SN3cyl=carttocyl_instant(SN_MX3,Pos[i,*])
					n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,S0n[i,*])
					;;print,"n_SN3cyl=",n_SN3cyl
					;print,"new n_SN_MX3_Acc=",n_SN_MX3_Acc
				endif
				print,SN_MX3
				;Dst=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)
				;;print,"SN_MX1=",SN_MX1
				;;print,"SN_MX2=",SN_MX2
				;Ns1=(list(SN,SN_MX1,SN_MX2,SN_MX3)).toarray()

				;sangles1=[SHOCKANGLE_MC[i],SHOCKANGLE_MX1[i],SHOCKANGLE_MX2[i],SHOCKANGLE_MX3[i]]
				;Ns1=Ns1[where(finite(sangles1)),*,*]
				;;print,Ns1
				;SN_AVG1=normalizedcomponentaverage(Ns1);[mean([n_SN_MX1[i,0],n_SN_MX2[i,0],n_SN_MX3[i,0],n_SN[i,0]]),$
					   ;    mean([n_SN_MX1[i,1],n_SN_MX2[i,1],n_SN_MX3[i,1],n_SN[i,1]]),$
					    ;   mean([n_SN_MX1[i,2],n_SN_MX2[i,2],n_SN_MX3[i,2],n_SN[i,2]])]
				;Ns=(list(SN_MX1,SN_MX2,SN_MX3,SN)).toarray()
				;if 90-abs(90-SHOCKANGLE_MC[i]) lt 80 or 90-abs(90-SHOCKANGLE_MC[i]) gt 10 then Ns=(list(SN_MX1,SN_MX2,SN_MX3,SN)).toarray() else Ns=(list(SN_MX1,SN_MX2,SN_MX3)).toarray();Ns=(list(SN_MX1,SN_MX2,SN_MX3)).toarray()
				Ns=(list(SN_MX2,SN_MX3)).toarray()
				print,Ns
						sangles=[SHOCKANGLE_MC[i],SHOCKANGLE_MX1[i],SHOCKANGLE_MX2[i],SHOCKANGLE_MX3[i]]
				print,sangles
				Ns=Ns[where(finite(sangles)),*,*]
				;;print,Ns1
				SN_AVG=normalizedcomponentaverage(Ns)
				print,SN_AVG
				;Ns2=list(SN_MX1,SN_MX2,SN_MX3)
				;SN_AVG2=normalizedcomponentaverage(Ns2)
				;Ns3=list(SN_MX2,SN_MX3)
				;SN_AVG3=normalizedcomponentaverage(Ns3)
				n_SNAcyl=carttocyl_instant(SN_AVG,Pos[i,*])
				;SNAcyl1=carttocyl_instant(SN_AVG1,Pos[i,*])
				;SNAcyl2=carttocyl_instant(SN_AVG2,Pos[i,*])
				;SNAcyl3=carttocyl_instant(SN_AVG3,Pos[i,*])
				;n_SN_AVG_Acc1=vecDotProduct(SNAcyl1,S0n[i,*])
				;n_SN_AVG_Acc2=vecDotProduct(SNAcyl2,S0n[i,*])
				;n_SN_AVG_Acc3=vecDotProduct(SNAcyl3,S0n[i,*])
				;avgAccs=[n_SN_AVG_Acc1,n_SN_AVG_Acc2,n_SN_AVG_Acc3]
				;n_SN_AVG_Acc=max(avgAccs,amaxloc,/NAN)

				;case amaxloc of 
				;	0: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc1
				;		n_SNAcyl=SNAcyl1
				;		SN_AVG=TRANSPOSE(SN_AVG1)
				;	end
				;	1: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc2
				;		n_SNAcyl=SNAcyl2
				;		SN_AVG=TRANSPOSE(SN_AVG2)
				;	end
				;	2: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc3
				;		n_SNAcyl=SNAcyl3
				;		SN_AVG=TRANSPOSE(SN_AVG3)
				;	end

				;endcase



				;catch,error_status

				;if error_status ne 0 then begin
					;catch,/cancel
					;continue

				;endif

				n_SN_MX1[i,*]=SN_MX1
				n_SN_MX2[i,*]=SN_MX2
				n_SN_MX3[i,*]=SN_MX3
				n_SN[i,*]=SN
				sangles=[SHOCKANGLE_MC[i],SHOCKANGLE_MX1[i],SHOCKANGLE_MX2[i],SHOCKANGLE_MX3[i]]
				Ns=Ns[where(finite(sangles),ncounttt),*,*]
				if ncounttt eq 0 then continue

				;;print,Ns1
				SN_AVG=normalizedcomponentaverage(Ns)
				nurm=Sqrt(SN_AVG[0]^2+SN_AVG[1]^2+SN_AVG[2]^2)
				if nurm ne 1. then for kk=0,2 do SN_AVG[kk]/=nurm
				n_SN_AVG[i,*]=SN_AVG
				n_SNAcyl=carttocyl_instant(SN_AVG,Pos[i,*])
				;n_SNAcyl=carttocyl_instant(n_SN_AVG[i,*],Pos[i,*])
				;n_SN_AVG_Acc=vecDotProduct(n_SNAcyl,S0n[i,*])
				;shock angle th=acos(n • B_u/|B_u|)
				SHOCKANGLE_MC[i]=AngleCalc(n_SN[i,*],BU)
				SHOCKANGLE_MX1[i]=AngleCalc(n_SN_MX1[i,*],BU)
				SHOCKANGLE_MX2[i]=AngleCalc(n_SN_MX2[i,*],BU)
				SHOCKANGLE_MX3[i]=AngleCalc(n_SN_MX3[i,*],BU)
	
				SHOCKANGLE_AVG[i]=AngleCalc(n_SN_AVG[i,*],BU)

				SNAcyl=carttocyl_instant(SN_AVG,Pos[i,*])
				n_SN_AVG_Acc=vecDotProduct(SNAcyl,S0n[i,*])
				;print,"SNAcyl=",SNAcyl
				;print,"S0n[i,*]",S0n[i,*]

				n_SN_Accs[i]=n_SN_Acc
				n_SN_MX1_Accs[i]=n_SN_MX1_Acc
				n_SN_MX2_Accs[i]=n_SN_MX2_Acc
				n_SN_MX3_Accs[i]=n_SN_MX3_Acc
				n_SN_AVG_Accs[i]=n_SN_AVG_Acc



				n_SN_AVG_Acc_Adj=(n_SN_AVG_Acc-n_SNAcyl[1]*S0n[i,1])/(1-(n_SNAcyl[1])^2)
				;n_SN_Accs_Adj[i]=n_SN_Acc_Adj
				;n_SN_MX1_Accs_Adj[i]=n_SN_MX1_Acc_Adj
				;n_SN_MX2_Accs_Adj[i]=n_SN_MX2_Acc_Adj
				;n_SN_MX3_Accs_Adj[i]=n_SN_MX3_Acc_Adj
				;n_SN_AVG_Accs_Adj[i]=n_SN_AVG_Acc_Adj

				n_SN_CYL[i,*]=n_SNcyl
				n_SN_MX1_CYL[i,*]=n_SN1cyl
				n_SN_MX2_CYL[i,*]=n_SN2cyl
				n_SN_MX3_CYL[i,*]=n_SN3cyl
				n_SN_AVG_CYL[i,*]=n_SNAcyl
				sangles0=[SHOCKANGLE_MC[i],SHOCKANGLE_MX1[i],SHOCKANGLE_MX2[i],SHOCKANGLE_MX3[i],SHOCKANGLE_AVG[i]]
				;print,"sangles0=",sangles0
				sangles=sangles0[where(finite(sangles0))]
				;print,where(finite(sangles0))
				;;print,"sangles0=",sangles0
				dops=([n_SN_Acc,n_SN_MX1_Acc,n_SN_MX2_Acc,n_SN_MX3_Acc,n_SN_AVG_Acc])[where(finite(sangles0))]
				;dops_Adj=([n_SN_Acc_Adj,n_SN_MX1_Acc_Adj,n_SN_MX2_Acc_Adj,n_SN_MX3_Acc_Adj,n_SN_AVG_Acc_Adj])[where(finite(sangles0))]
				cyls=([[n_SNcyl],[n_SN1cyl],[n_SN2cyl],[n_SN3cyl],[n_SNAcyl]])
				;help,cyls
				cyls=cyls[*,where(finite(sangles0))]
				;help,cyls
				;;print,"SN=",SN
				;;print,"SN_AVG=",SN_AVG
				;;print,"[SN,SN_MX1,SN_MX2,SN_MX3,SN_AVG]=",[SN,SN_MX1,SN_MX2,SN_MX3,TRANSPOSE(SN_AVG)]
				sns=transpose([SN,SN_MX1,SN_MX2,SN_MX3,transpose(SN_AVG)]);[where(finite(sangles0))]
				;help,sns
				;;print,sns
				sns=sns[*,where(finite(sangles0))]
				;help,sns
				;print,sns
				;bestDop=max(dops_Adj,bstSN,/NAN)
				bestDop=max(dops,bstSN,/NAN)
				;print,dops
				;print,bestDop,bstSN
				n_SN_best_Accs[i]=dops[bstSN]
				;;print,bstSN
				n_SN_best[i,*]=sns[*,bstSN];transpose(sns[*,bstSN])
				n_SN_best_CYL[i,*]=cyls[*,bstSN]
				SHOCKANGLE_best[i]=sangles[bstSN]
				;print,"cyls=",cyls
				;print,"n_SN_best_CYL[i,*]=",n_SN_best_CYL[i,*]
				;print,"n_SN_best_Accs[i]=",n_SN_best_Accs[i],bestDop
				;print,"SHOCKANGLE_best[i]=",SHOCKANGLE_best[i]
				continue
			endif
			foundEND=0
			print,fltarr(100)+999
			print,"OTHER WAY"
;		foundEND=0
;		for j=i,N-1 do begin
;			if foundEND eq 1 then break
;			if (BFD[j+10] ne 0) then continue
;			


;			;print,"j=",j
;			ll=mean([mean([j,i]),i])
;			;print,"ll=",ll
;			jmin=imin
;			;print, "BFD[ll]=BFD[",ll,"]=",BFD[ll]

;			While 1*(BFD[ll] ne 0) ne 1 do begin
;				ll=mean([ll,i])
;				;print,"ll=",ll
;				;print, "BFD[ll]=BFD[",ll,"]=",BFD[ll]
;				if ll lt j +10 then break
;				
;			endwhile
;			;print, "BFD[ll]=BFD[",ll,"]=",BFD[ll]
;			startON=1*(BFD[ll] ne 0)
;			;print,"startON[i,ll]=startON[",i,",",ll,"]=",startON

;			while (BFD[ll+jmin] eq 0 ) and (startON eq 1) do begin
;				 ;;print,jmin
;				;print, "BFD[ll+jmin]=BFD[",ll,"+",jmin,"]=",BFD[ll+jmin]					
;				 jmin=jmin/2
;				;print,"startON[i,ll]=startON[",i,",",ll,"]=",startON
;				 if (jmin le 1) then break
;			endwhile
;			;print,'ll+jmin=',ll+jmin
;			BD=[Mean(BB[ll:ll+jmin,0]),Mean(BB[ll:ll+jmin,1]),Mean(BB[ll:ll+jmin,2])]
;			vd=[Mean(vp[ll:ll+jmin,0]),Mean(vp[ll:ll+jmin,1]),Mean(vp[ll:ll+jmin,2])]

;			;flagend=j

;			;midflag=(i+j)/2
;			;qflag=(i+midflag)/2
;			;;print,'qflag=',qflag

;			;for k=qflag,qflag+imin do Bsum+= BB[k,*]
;			
;			;BD=vecdiv(Bsum,imin)
;			BU=B_U[i,*]

;			vu=v_u[i,*]
;			;;print,Bsum
;			;for k=i-10*imin, qflag+imin do begin
;			;		B_D[k,*]=BD
;			;		n_SN[k,*]=shocknorm(BU,BD)
;			;endfor				

;			;;print,"pos[",i,"]=",pos[i/npos,*]
;			;;print,"posN[",i,"]=",posNew[i,*]
;			;print,"BD=",BD
;			;print,"BU=",BU
;			dB=BD-BU
;			
;			dv=vd-vu

;			;n_SN[i,*]=shocknormMC(BU,BD)
;			SN=shocknorm(BD,BU,dB)
;			;n_SN[i,*]=
;			SN_MX1 =shocknorm(BU,dv,dB)
;			SN_MX2=shocknorm(BD,dv,dB)
;			SN_MX3=shocknorm(dB,dv,dB)
;			;n_SN_MX1[i,*]=shocknorm(BU,dv,dB)
;			;n_SN_MX2[i,*]=shocknorm(BD,dv,dB)
;			;n_SN_MX3[i,*]=shocknorm(dB,dv,dB)

;			
;			;print,'SN[',i,",*]=",SN
;			
;			n_SNcyl=carttocyl_instant(SN,Pos[i,*])
;			n_SN1cyl=carttocyl_instant(SN_MX1,Pos[i,*])
;			n_SN2cyl=carttocyl_instant(SN_MX2,Pos[i,*])
;			n_SN3cyl=carttocyl_instant(SN_MX3,Pos[i,*])
;			n_SN_Acc=vecDotProduct(n_SNcyl,S0n[i,*])
;			n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,S0n[i,*])
;			n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,S0n[i,*])
;			n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,S0n[i,*])



;			if ((n_SN_Acc lt 0) or (n_SN_Acc_Adj lt 0)  or ( (n_SN_Acc lt 0.1)  and sign(n_SNcyl[2]*nS0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN[0] gt 0)  then begin;then begin;and (SN[0] lt 0.0) then begin
;				;;print,"n_SN_Acc=",n_SN_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
;				;;print,"n_SNcyl=",n_SNcyl
;				;print,SN
 					;SN=(-1)*SN
;				;print,SN
;				;;print,"n_SNcyl=",n_SNcyl
;				n_SNcyl=carttocyl_instant(SN,Pos[i,*])
;				n_SN_Acc=vecDotProduct(n_SNcyl,S0n[i,*])
;				;print,"new n_SN_Acc=",n_SN_Acc
;			endif
;			if ((n_SN_MX1_Acc lt 0) or (n_SN_MX1_Acc_Adj lt 0) or ( (n_SN_MX1_Acc lt 0.1)  and sign(n_SN1cyl[2]*nS0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX1[0] gt 0) then begin;then begin;and (SN_MX1[0] lt 0.0) then begin
;				;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
;				;;print,"n_SN1cyl=",n_SN1cyl
;				;print,SN_MX1
;				SN_MX1=(-1)*SN_MX1
;				;print,SN_MX1
;				n_SN1cyl=carttocyl_instant(SN_MX1,Pos[i,*])
;				;;print,"n_SN1cyl=",n_SN1cyl
;				n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,S0n[i,*])
;				;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
;			endif

;			if ((n_SN_MX2_Acc lt 0) or (n_SN_MX2_Acc_Adj lt 0) or ( (n_SN_MX2_Acc lt 0.1)  and sign(n_SN2cyl[2]*nS0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX2[0] gt 0) then begin;then begin; and (SN_MX2[0] lt 0.0) then begin
;				;;print,"n_SN_MX2_Acc=",n_SN_MX2_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
;				;;print,"n_SN2cyl[i,*]=",Transpose(n_SN2cyl[i,*])
;				SN_MX2=(-1)*SN_MX2
;				n_SN2cyl=carttocyl_instant(SN_MX2,Pos[i,*])
;				n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,S0n[i,*])
;				;;print,"n_SN2cyl=",n_SN2cyl
;				;print,"new n_SN_MX2_Acc=",n_SN_MX2_Acc
;			endif
;			if ((n_SN_MX3_Acc lt 0) or (n_SN_MX3_Acc_Adj lt 0)  or ( (n_SN_MX3_Acc lt 0.1)  and sign(n_SN3cyl[2]*nS0n[i,2]) eq -1)) and ~ (Pos[i,0] gt 0 and SN_MX3[0] gt 0) then begin;then begin;;and (SN_MX3[0] lt 0.0) then begin
;				;;print,"n_SN_MX3_Acc=",n_SN_MX3_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(S0n[i,*])
;				;;print,"n_SN3cyl=",n_SN3cyl
;				SN_MX3=(-1)*SN_MX3
;				n_SN3cyl=carttocyl_instant(SN_MX3,Pos[i,*])
;				n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,S0n[i,*])
;				;;print,"n_SN3cyl=",n_SN3cyl
;				;print,"new n_SN_MX3_Acc=",n_SN_MX3_Acc
;			endif
;			;Dst=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)

;			Ns=(list(SN,SN_MX1,SN_MX2,n_SN_MX3))
;			SN_AVG=normalizedcomponentaverage(Ns);[mean([n_SN_MX1[i,0],n_SN_MX2[i,0],n_SN_MX3[i,0],n_SN[i,0]]),$
;				   ;    mean([n_SN_MX1[i,1],n_SN_MX2[i,1],n_SN_MX3[i,1],n_SN[i,1]]),$
;				    ;   mean([n_SN_MX1[i,2],n_SN_MX2[i,2],n_SN_MX3[i,2],n_SN[i,2]])]

;			n_SN_MX1[i,*]=SN_MX1
;			n_SN_MX2[i,*]=SN_MX2
;			n_SN_MX3[i,*]=SN_MX3
;			n_SN[i,*]=SN

;			n_SN_AVG[i,*]=SN_AVG
;			;n_SN_AVG[i,*]=[mean([n_SN_MX1[i,0],n_SN_MX2[i,0],n_SN_MX3[i,0],n_SN[i,0]]),$
;				    ;   mean([n_SN_MX1[i,1],n_SN_MX2[i,1],n_SN_MX3[i,1],n_SN[i,1]]),$
;				     ;  mean([n_SN_MX1[i,2],n_SN_MX2[i,2],n_SN_MX3[i,2],n_SN[i,2]])]
;			n_SNAcyl=carttocyl_instant(n_SN_AVG[i,*],Pos[i,*])
;			n_SN_AVG_Acc=vecDotProduct(n_SNAcyl,S0n[i,*])
;			;shock angle th=acos(n • B_u/|B_u|)
;			SHOCKANGLE_MC[i]=AngleCalc(n_SN[i,*],BU)
;			SHOCKANGLE_MX1[i]=AngleCalc(n_SN_MX1[i,*],BU)
;			SHOCKANGLE_MX2[i]=AngleCalc(n_SN_MX2[i,*],BU)
;			SHOCKANGLE_MX3[i]=AngleCalc(n_SN_MX3[i,*],BU)
;			SHOCKANGLE_AVG[i]=AngleCalc(n_SN_AVG[i,*],BU)
;			;for k=iret, flagend do SHOCKANGLE[k]=AngleCalc(n_SN[i,*],BU)
;			n_SN_Accs[i]=n_SN_Acc
;			n_SN_MX1_Accs[i]=n_SN_MX1_Acc
;			n_SN_MX2_Accs[i]=n_SN_MX2_Acc
;			n_SN_MX3_Accs[i]=n_SN_MX3_Acc
;			n_SN_AVG_Accs[i]=n_SN_AVG_Acc				

;			n_SN_CYL[i,*]=n_SNcyl
;			n_SN_MX1_CYL[i,*]=n_SN1cyl
;			n_SN_MX2_CYL[i,*]=n_SN2cyl
;			n_SN_MX3_CYL[i,*]=n_SN3cyl
;			n_SN_AVG_CYL[i,*]=n_SNAcyl
;			;;print,"i=",i
;			;i+=10*imin
;			;;print,"i=",i
;			foundEND=1
;			;print,"end of first innermost loop. About to break. j=",j
;			if i eq i then break

;		endfor
;		;print,"just below innermost loop Inside If statement. i=",i
;		
			;print,"just below innermost loop Inside If statement. i=",i
			
		;endif 
		;;print,"just below If statement, at end of innermost loop. i=",i

	endfor




	;print,"endfor 1"

	get_data,"upstream_indices_outbound",data=datuo

	uo=datuo.y
	nuo=N-1-reverse(uo)
	get_data,"downstream_indices_outbound",data=datdo

	dio=datdo.y
	ndio=N-1-reverse(dio)


	;print, "last shock at i=",flagstart
	;print, "which is N-i=",N-flagstart," indices from the end"
	nB=REVERSE(BB)
	nv=REVERSE(vp)
	;nBFA=Reverse(BFA)
	;nBFD=Reverse(BFD)
	;nBder=Reverse(Bder)
	;nUD=Reverse(UD)
	nPos=REVERSE(Pos)
	nx=reverse(xs)
	nPosCyl=REVERSE(PosCyl)
	nConic=REVERSE(conic)
	nS0n=REVERSE(S0n)
	nouts=reverse(outs)
	noutlocs=where(nouts eq 1,numout)
	;;print,"inLocs=",inLocs
	;print,"noutlocs=",noutlocs
	if noutlocs[numout-1] eq N-1 then begin
		numout--
		noutlocs=noutlocs[0:numout-1]
	endif

	nB_D=fltarr(N,3)
	;print,"numel(BB)=",numel(BB)
	nB_U=fltarr(N,3)
	nv_D=fltarr(N,3)
	nv_U=fltarr(N,3)
	nBD=0
	nBU=0
	nn_SN=fltarr(N,3)
	nn_SN_MX1=fltarr(N,3)
	nn_SN_MX2=fltarr(N,3)
	nn_SN_MX3=fltarr(N,3)
	nn_SN_AVG=fltarr(N,3)
	nn_SN_best=fltarr(N,3)

	nSHOCKANGLE_MC=fltarr(N)
	nSHOCKANGLE_MX1=fltarr(N)
	nSHOCKANGLE_MX2=fltarr(N)
	nSHOCKANGLE_MX3=fltarr(N)
	nSHOCKANGLE_AVG=fltarr(N)
	nSHOCKANGLE_best=fltarr(N)

	nn_SN_Accs=fltarr(N)
	nn_SN_MX1_Accs=fltarr(N)
	nn_SN_MX2_Accs=fltarr(N)
	nn_SN_MX3_Accs=fltarr(N)
	nn_SN_AVG_Accs=fltarr(N)
	nn_SN_best_Accs=fltarr(N)

	nn_SN_Accs_Adj=fltarr(N)
	nn_SN_MX1_Accs_Adj=fltarr(N)
	nn_SN_MX2_Accs_Adj=fltarr(N)
	nn_SN_MX3_Accs_Adj=fltarr(N)
	nn_SN_AVG_Accs_Adj=fltarr(N)
	nn_SN_best_Accs_Adj=fltarr(N)


	nn_SN_CYL=fltarr(N,3)
	nn_SN_MX1_CYL=fltarr(N,3)
	nn_SN_MX2_CYL=fltarr(N,3)
	nn_SN_MX3_CYL=fltarr(N,3)
	nn_SN_AVG_CYL=fltarr(N,3)
	nn_SN_best_CYL=fltarr(N,3)

	nouts[N-1]=0
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	;print, "SHOCKNORMCALC		    BACKWARD LOOP	       SHOCKNORMCALC"
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

	for el=0,numout-1 do begin
		;print,"noutlocs=",noutlocs
		i=noutlocs[el]
		print,"el=",el
			error_status=0
			;catch,error_status
			if error_status ne 0 then begin
				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				xaaa=xs[N-1-i]
				PRINT,'ERROR in shockNormCalc during el=',el
				;errorloc=""
			
				errmsg=["error in shockNormCalc outbound during el="+strtrim(el,2), 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
				error_status=0
				catch,/cancel
				;errorloc=""
				continue
			endif

		ubeg=nuo[i,1]
		uend=nuo[i,0]
	;for i=10*imin,N-1-10*imin do begin
		nBD=0
		nBU=0
		nvD=0
		nvU=0
		atFlag=0
		ni=N-i
		;if(nBFA[i] eq nBFD[i]) or (nBFD[i] ne 0) then continue
		;;print,"i,ni=",i,ni
		;if (nUD[i] eq 1) and (nUD[i-1] eq 0)  then begin
		;if((nBder[i] lt nBder[i-1] ) and (Bder[i] lt nBder[i-1])) then begin
;		if(nBFA[i] ne 0 ) and (nBFA[i-imin/30] eq 0) and ((nBder[i] lt nBder[i-1] ) and (Bder[i] lt nBder[i-1])) then begin
			atFlag=1
			flagstart=i
			;establish B_U
			iret=ubeg;i-10*imin
			;Bsum=0
			;for j=iret,iret+imin do Bsum+=nB[j,*]

			;B_avg=vecdiv(Bsum,imin)
			;Bsum=BB[0,*]*0.0
			;print,"ubeg,uend,N-1=",ubeg,uend,N-1
			Bum=[Mean(nB[ubeg:uend,0]),Mean(nB[ubeg:uend,1]),Mean(nB[ubeg:uend,2])]
			vum=[Mean(nv[ubeg:uend,0]),Mean(nv[ubeg:uend,1]),Mean(nv[ubeg:uend,2])]
			;print,time_string(xs[N-ubeg])
			;print,time_string(xs[N-uend])
			;Bum=[Mean(nB[i-4*imin:i-3*imin,0]),Mean(nB[i-4*imin:i-3*imin,1]),Mean(nB[i-4*imin:i-3*imin,2])]
			;vum=[Mean(nv[i-4*imin:i-3*imin,0]),Mean(nv[i-4*imin:i-3*imin,1]),Mean(nv[i-4*imin:i-3*imin,2])]
			;print,"Bum=",Bum
			;print,"[i,imin,i-10*imin,numel(nB_U[*,0])]=",[i,imin,i-10*imin,numel(nB_U[*,0])]
			for j=iret,min([i+1,N-1]) do begin
				;nB_U[j,*]=Bum
				;nv_U[j,*]=vum
				for k=0,2 do begin
					nB_U[j,k]=Bum[k]
					nv_U[j,k]=vum[k]
				endfor
				nflowANGLE[j]=acos(dotproduct(Bum,vum)/(norm(Bum)*norm(vum)))
			endfor

			;establish B_d
			if useDownstream then begin
				dbeg=min(ndio[i,*])
				dend=max(ndio[i,*])

				BD=[Mean(nB[dbeg:dend,0]),Mean(nB[dbeg:dend,1]),Mean(nB[dbeg:dend,2])]
				vd=[Mean(nv[dbeg:dend,0]),Mean(nv[dbeg:dend,1]),Mean(nv[dbeg:dend,2])]
				BU=Bum;nB_U[i,*]

				vu=vum;nv_u[i,*]

				dB=BD-BU
				
				dv=vd-vu

				;n_SN[i,*]=shocknormMC(BU,BD)
				SN=shocknorm(BD,BU,dB)
				SN_MX1=shocknorm(BU,dv,dB)
				SN_MX2=shocknorm(BD,dv,dB)
				SN_MX3=shocknorm(dB,dv,dB)
				n_SNcyl=carttocyl_instant(SN,nPos[i,*])
				n_SN1cyl=carttocyl_instant(SN_MX1,nPos[i,*])
				n_SN2cyl=carttocyl_instant(SN_MX2,nPos[i,*])
				n_SN3cyl=carttocyl_instant(SN_MX3,nPos[i,*])
				;print,"norm(SN)=",norm(SN),sqrt(total(SN^2)),total(SN)
				;print,"norm(SN_MX1)=",norm(SN_MX1),sqrt(total(SN_MX1^2))
				;print,"norm(SN_MX2)=",norm(SN_MX2),sqrt(total(SN_MX2^2))
				;print,"norm(SN_MX3)=",norm(SN_MX3),sqrt(total(SN_MX3^2))
				;print,"norm(n_SNcyl)=",SQRT(n_SNcyl[0]^2+n_SNcyl[1]^2+n_SNcyl[2]^2)
				;print,"norm(n_SN1cyl)=",SQRT(n_SN1cyl[0]^2+n_SN1cyl[1]^2+n_SN1cyl[2]^2)
				;print,"norm(n_SN2cyl)=",SQRT(n_SN2cyl[0]^2+n_SN2cyl[1]^2+n_SN2cyl[2]^2)
				;print,"norm(n_SN3cyl)=",SQRT(n_SN3cyl[0]^2+n_SN3cyl[1]^2+n_SN3cyl[2]^2)

				n_SN_Acc=vecDotProduct(n_SNcyl,nS0n[i,*])
				n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,nS0n[i,*])
				n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,nS0n[i,*])
				n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,nS0n[i,*])
				n_SN_Acc_Adj=(n_SN_Acc-(n_SNcyl[1]*nS0n[i,1]))/(1-n_SNcyl[1]^2)
				n_SN_MX1_Acc_Adj=(n_SN_MX1_Acc-(n_SN1cyl[1]*nS0n[i,1]))/(1-n_SN1cyl[1]^2)
				n_SN_MX2_Acc_Adj=(n_SN_MX2_Acc-(n_SN2cyl[1]*nS0n[i,1]))/(1-n_SN2cyl[1]^2)
				n_SN_MX3_Acc_Adj=(n_SN_MX3_Acc-(n_SN3cyl[1]*nS0n[i,1]))/(1-n_SN3cyl[1]^2)
				if ((n_SN_Acc lt 0)  or (n_SN_Acc_Adj lt 0) or (n_SNcyl[2] lt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) lt .15 and nPos[i,0] gt 0) or (n_SNcyl[0] lt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) gt .85 and nPos[i,0] gt 0)) and not ((n_SNcyl[2] gt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) gt .15 and nPos[i,0] gt 0 and n_SNcyl[0] gt 0 and abs(n_SNcyl[0]/n_SNcyl[2]) lt .85)) then begin;and (SN[0] lt 0.0) then begin
					;;print,"n_SN_Acc=",n_SN_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
					;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
					;;print,"n_SNcyl=",n_SNcyl
					;print,SN
 					SN=(-1)*SN
					;print,SN
					n_SNcyl=carttocyl_instant(SN,nPos[i,*])
					n_SN_Acc=vecDotProduct(n_SNcyl,nS0n[i,*])
					;;print,"n_SNcyl=",n_SNcyl
					;print,"new n_SN_Acc=",n_SN_Acc
				endif
				if ((n_SN_MX1_Acc lt 0)  or (n_SN_MX1_Acc_Adj lt 0) or (n_SN1cyl[2] lt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) lt .15 and nPos[i,0] gt 0) or (n_SN1cyl[0] lt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) gt .85 and nPos[i,0] gt 0)) and not ((n_SN1cyl[2] gt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) gt .15 and nPos[i,0] gt 0 and n_SN1cyl[0] gt 0 and abs(n_SN1cyl[0]/n_SN1cyl[2]) lt .85)) then begin;and (SN_MX1[0] lt 0.0) then begin
					;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
					;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
					;;print,"n_SN1cyl=",n_SN1cyl
					;print,SN_MX1
					SN_MX1=(-1)*SN_MX1
					;print,SN_MX1
					n_SN1cyl=carttocyl_instant(SN_MX1,nPos[i,*])
					n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,nS0n[i,*])
					;;print,"n_SN1cyl=",n_SN1cyl
					;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
				endif

				if ((n_SN_MX2_Acc lt 0)  or (n_SN_MX2_Acc_Adj lt 0) or (n_SN2cyl[2] lt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) lt .15 and nPos[i,0] gt 0) or (n_SN2cyl[0] lt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) gt .85 and nPos[i,0] gt 0)) and not ((n_SN2cyl[2] gt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) gt .15 and nPos[i,0] gt 0 and n_SN2cyl[0] gt 0 and abs(n_SN2cyl[0]/n_SN2cyl[2]) lt .85)) then begin; and (SN_MX2[0] lt 0.0) then begin
					;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
					;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
					;;print,"n_SN2cyl=",n_SN2cyl
					;print,SN_MX2
					SN_MX2=(-1)*SN_MX2
					;print,SN_MX2
					n_SN2cyl=carttocyl_instant(SN_MX2,nPos[i,*])
					n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,nS0n[i,*])
					;;print,"n_SN2cyl=",n_SN2cyl
					;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
				endif
				if ((n_SN_MX3_Acc lt 0)  or (n_SN_MX3_Acc_Adj lt 0) or (n_SN3cyl[2] lt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) lt .15 and nPos[i,0] gt 0) or (n_SN3cyl[0] lt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) gt .85 and nPos[i,0] gt 0)) and not ((n_SN3cyl[2] gt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) gt .15 and nPos[i,0] gt 0 and n_SN3cyl[0] gt 0 and abs(n_SN3cyl[0]/n_SN3cyl[2]) lt .85)) then begin;and (SN_MX3[0] lt 0.0) then begin
					;;print,"n_SN_MX3_Acc=",n_SN_MX3_Acc
					;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
					;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
					;;print,"n_SN3cyl=",n_SN3cyl
					SN_MX3=(-1)*SN_MX3
					n_SN3cyl=carttocyl_instant(SN_MX3,nPos[i,*])
					n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,nS0n[i,*])
					;;print,"n_SN3cyl=",n_SN3cyl
					;print,"new n_SN_MX3_Acc=",n_SN_MX3_Acc
				endif
				;if n_SN_MX2_Acc lt 0 then nn_SN_MX2[i,*]=(-1)*nn_SN_MX2[i,*]
				;if n_SN_MX3_Acc lt 0 then nn_SN_MX3[i,*]=(-1)*nn_SN_MX3[i,*]
				;Ns=(list(SN,SN_MX1,SN_MX2,SN_MX3)).toarray()
				;if 90-abs(90-nSHOCKANGLE_MC[i]) lt 80 or 90-abs(90-nSHOCKANGLE_MC[i]) gt 10 then Ns=(list(SN_MX1,SN_MX2,SN_MX3,SN)).toarray() else Ns=(list(SN_MX1,SN_MX2,SN_MX3)).toarray() ;Ns=(list(SN_MX1,SN_MX2,SN_MX3)).toarray()
				Ns=(list(SN_MX2,SN_MX3)).toarray()
				sangles=[nSHOCKANGLE_MC[i],nSHOCKANGLE_MX1[i],nSHOCKANGLE_MX2[i],nSHOCKANGLE_MX3[i]]
				Ns=Ns[where(finite(sangles),ncounttt),*,*]
				;catch,error_status
				if ncounttt eq 0 then continue
				;if error_status ne 0 then begin
				;	;print,"NAN shock angles"
				;catch,/cancel
				;	continue

				;endif
				SN_AVG=normalizedcomponentaverage(Ns)
				nurm=Sqrt(SN_AVG[0]^2+SN_AVG[1]^2+SN_AVG[2]^2)
				if nurm ne 1. then for kk=0,2 do SN_AVG[kk]/=nurm
				Ns1=list(SN,SN_MX1,SN_MX2,SN_MX3)
				SN_AVG1=normalizedcomponentaverage(Ns1)
				n_SNAcyl1=carttocyl_instant(SN_AVG1,nPos[i,*])
				n_SN_AVG_Acc1=vecDotProduct(n_SNAcyl1,nS0n[i,*])
				Ns2=list(SN_MX1,SN_MX2,SN_MX3)
				SN_AVG2=normalizedcomponentaverage(Ns2)
				n_SNAcyl2=carttocyl_instant(SN_AVG2,nPos[i,*])
				n_SN_AVG_Acc2=vecDotProduct(n_SNAcyl2,nS0n[i,*])

				Ns3=list(SN_MX2,SN_MX3)
				SN_AVG3=normalizedcomponentaverage(Ns3)
				n_SNAcyl3=carttocyl_instant(SN_AVG3,nPos[i,*])
				n_SN_AVG_Acc3=vecDotProduct(n_SNAcyl3,nS0n[i,*])

				;avgAccs=[n_SN_AVG_Acc1,n_SN_AVG_Acc2,n_SN_AVG_Acc3]
				;n_SN_AVG_Acc=max(avgAccs,amaxloc,/NAN)


				;case amaxloc of 
					;0: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc1
						;n_SNAcyl=n_SNAcyl1
						;SN_AVG=SN_AVG1
					;end
					;1: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc2
					;	n_SNAcyl=n_SNAcyl2
						;SN_AVG=SN_AVG2
					;end
					;2: begin
						;n_SN_AVG_Acc=n_SN_AVG_Acc3
					;	n_SNAcyl=n_SNAcyl3
						;SN_AVG=SN_AVG3
					;end

				;endcase

				;Ns=(list(SN,SN_MX1,SN_MX2,SN_MX3)).toarray()
				;sangles=[nSHOCKANGLE_MC[i],nSHOCKANGLE_MX1[i],nSHOCKANGLE_MX2[i],nSHOCKANGLE_MX3[i]]
				;Ns=Ns[where(finite(sangles)),*,*]


				nn_SN[i,*]=SN
				nn_SN_MX1[i,*]=SN_MX1
				nn_SN_MX2[i,*]=SN_MX2
				nn_SN_MX3[i,*]=SN_MX3
				nn_SN_AVG[i,*]=SN_AVG
				;SN_AVG=[mean([nn_SN_MX1[i,0],nn_SN_MX2[i,0],nn_SN_MX3[i,0],nn_SN[i,0]]),$
					     ;   mean([nn_SN_MX1[i,1],nn_SN_MX2[i,1],nn_SN_MX3[i,1],nn_SN[i,1]]),$
					       ; mean([nn_SN_MX1[i,2],nn_SN_MX2[i,2],nn_SN_MX3[i,2],nn_SN[i,2]])]
				n_SNAcyl=carttocyl_instant(SN_AVG,nPos[i,*])
				n_SN_AVG_Acc=vecDotProduct(n_SNAcyl,nS0n[i,*])
				;n_SN_AVG_Acc_Adj=(n_SN_AVG_Acc-n_SNAcyl[1]*nS0n[i,1])/(1-(n_SNAcyl[1])^2)
				nn_SN_AVG[i,*]=SN_AVG

				;print,'n_SN2[',N-1-i,",*]=",nn_SN[i,*]
				
				;shock angle th=acos(n • B_u/|B_u|)
				;nSHOCKANGLE[i]=AngleCalc(nn_SN[i,*],BU)

				nSHOCKANGLE_MC[i]=AngleCalc(nn_SN[i,*],BU)
				nSHOCKANGLE_MX1[i]=AngleCalc(nn_SN_MX1[i,*],BU)
				nSHOCKANGLE_MX2[i]=AngleCalc(nn_SN_MX2[i,*],BU)
				nSHOCKANGLE_MX3[i]=AngleCalc(nn_SN_MX3[i,*],BU)
				nSHOCKANGLE_AVG[i]=AngleCalc(SN_AVG,BU)
				nn_SN_Accs[i]=n_SN_Acc
				nn_SN_MX1_Accs[i]=n_SN_MX1_Acc
				nn_SN_MX2_Accs[i]=n_SN_MX2_Acc
				nn_SN_MX3_Accs[i]=n_SN_MX3_Acc
				nn_SN_AVG_Accs[i]=n_SN_AVG_Acc

				;nn_SN_MX1_Accs_Adj[i]=n_SN_MX1_Acc_Adj
				;nn_SN_MX2_Accs_Adj[i]=n_SN_MX2_Acc_Adj
				;nn_SN_MX3_Accs_Adj[i]=n_SN_MX3_Acc_Adj
				;nn_SN_AVG_Accs_Adj[i]=n_SN_AVG_Acc_Adj

				nn_SN_CYL[i,*]=n_SNcyl
				nn_SN_MX1_CYL[i,*]=n_SN1cyl
				nn_SN_MX2_CYL[i,*]=n_SN2cyl
				nn_SN_MX3_CYL[i,*]=n_SN3cyl
				nn_SN_AVG_CYL[i,*]=n_SNAcyl

				;dops=[n_SN_Acc,n_SN_MX1_Acc,n_SN_MX2_Acc,n_SN_MX3_Acc,n_SN_AVG_Acc]
				;cyls=[[n_SNcyl],[n_SN1cyl],[n_SN2cyl],[n_SN3cyl],[n_SNAcyl]]
				;sns=[SN,SN_MX1,SN_MX2,SN_MX3,transpose(SN_AVG)]
				;sangles=[nSHOCKANGLE_MC[i],nSHOCKANGLE_MX1[i],nSHOCKANGLE_MX2[i],nSHOCKANGLE_MX3[i],nSHOCKANGLE_AVG[i]]

				;;print,"dops=",dops
				;;print,"cyls=",cyls
				;;print,"sns=",sns
				;;print,"sangles",sangles
				;best_Accs=max(dops,bstSN,/NAN)
				;bestn=sns[*,bstSN]
				;bestCYL=cyls[*,bstSN]
				;bestSA=sangles[bstSN]
				;;print,"best_Accs=",best_Accs
				;;print,"bestn=",bestn
				sangles0=[nSHOCKANGLE_MC[i],nSHOCKANGLE_MX1[i],nSHOCKANGLE_MX2[i],nSHOCKANGLE_MX3[i],nSHOCKANGLE_AVG[i]]
				;print,"sangles0=",sangles0
				;print,where(finite(sangles0))
				sangles=sangles0[where(finite(sangles0))]
				;;print,"sangles0=",sangles0
				dops=([n_SN_Acc,n_SN_MX1_Acc,n_SN_MX2_Acc,n_SN_MX3_Acc,n_SN_AVG_Acc])[where(finite(sangles0))]
			;	dops_Adj=([n_SN_Acc_Adj,n_SN_MX1_Acc_Adj,n_SN_MX2_Acc_Adj,n_SN_MX3_Acc_Adj,n_SN_AVG_Acc_Adj])[where(finite(sangles0))]
				cyls=([[n_SNcyl],[n_SN1cyl],[n_SN2cyl],[n_SN3cyl],[n_SNAcyl]])
				;help,cyls
				cyls=cyls[*,where(finite(sangles0))]
				;help,cyls
				;;print,"SN=",SN
				;;print,"SN_AVG=",SN_AVG
				;;print,"[SN,SN_MX1,SN_MX2,SN_MX3,SN_AVG]=",[SN,SN_MX1,SN_MX2,SN_MX3,TRANSPOSE(SN_AVG)]
				sns=transpose([SN,SN_MX1,SN_MX2,SN_MX3,transpose(SN_AVG)]);[where(finite(sangles0))]
				;help,sns
				;;print,sns
				sns=sns[*,where(finite(sangles0))]
				;help,sns
				;print,sns
				
				bestDops=max(dops,bstSN);bestDops=max(dops_Adj,bstSN)
				;print,(["SN","MX1","MX2","MX3","AVG"])[where(finite(sangles0))]
				;print,dops
				;print,"bestDops=",bestDops,bstSN
				nn_SN_best_Accs[i]=dops[bstSN];max(dops,bstSN)
				nn_SN_best[i,*]=sns[*,bstSN]
				nn_SN_best_CYL[i,*]=cyls[*,bstSN]
				nSHOCKANGLE_best[i]=sangles[bstSN]
				;print,"cyls=",cyls
				;print,"nn_SN_best_CYL[i,*]=",nn_SN_best_CYL[i,*]
				;print,"nn_SN_best_Accs[i]=",nn_SN_best_Accs[i],bestDops
				;print,"nSHOCKANGLE_best[i]=",nSHOCKANGLE_best[i]
				;return
				continue
			endif

			;this will be an utterly bullshit implementation usign a loop ending with i itself being updated
			print,fltarr(100)+999
			print,"2nd WAY"
			foundEND=0
;		for j=i,N-1 do begin
;			if foundEND eq 1 then break
;			if nBFA[j+2] ne 0 then continue
;
;			;print,"i,j=",i,",",j
;			ll=mean([mean([j,i]),i])
;			;print,"ll=",ll
;			jmin=imin
;			;print, "nBFA[ll]=nBFA[",ll,"]=",nBFA[ll]
;				While 1*(nBFA[ll] ne 0) ne 1 do begin 
;				;print,ll
;				ll=mean([ll,i])
;				if ll eq j then break
;			endwhile
;			;print, "BFA[ll]=BFA[",ll,"]=",nBFA[ll]
;			startON=1*(nBFA[ll] ne 0)
;			while (nBFA[ll+jmin] eq 0 ) and (startON eq 1) do begin
;				;print, "nBFA[ll+jmin]=nBFA[",ll,"+",jmin,"]=",nBFA[ll+jmin]					
;				 ;print,jmin					
;				 jmin=jmin/2
;				 if (jmin le 1) then break
;			endwhile
;			;print,'ll+jmin=',ll+jmin
;			BD=[Mean(nB[ll:ll+jmin,0]),Mean(nB[ll:ll+jmin,1]),Mean(nB[ll:ll+jmin,2])]
;			vd=[Mean(nv[ll:ll+jmin,0]),Mean(nv[ll:ll+jmin,1]),Mean(nv[ll:ll+jmin,2])]
;				;Bsum=BB[0,*]*0.0
;			;flagend=j
;				;midflag=(i+j)/2
;			;qflag=(i+midflag)/2
;			;;print,'qflag=',qflag
;				;for k=qflag,qflag+imin do Bsum+= nB[k,*]
;			
;			;nBD=vecdiv(Bsum,imin)
;			BU=nB_U[i,*]
;			vu=nv_U[i,*]
;			
;			;;print,Bsum
;			;for k=i-10*imin, qflag+imin do nB_D[k,*]=B_avg
;			;for k=i-4*imin, qflag+imin do begin
;			;		nB_D[k,*]=nBD
;			;		nn_SN[k,*]=shocknorm(nBU,nBD)
;			;endfor	
;			
;			;nBD=nB_D[i,*]
;			;;print,"npos[",i,"]=",pos[(N-1-i)/npos,*]
;			;print,"BD=",BD
;			;print,"BU=",BU
;			dB=BD-BU
;			dv=vd-vu
;			
;			;nn_SN[i,*]=shocknormMC(BU,BD)
;				SN=shocknorm(BD,BU,dB)
;			SN_MX1=shocknorm(BU,dv,dB)
;			SN_MX2=shocknorm(BD,dv,dB)
;			SN_MX3=shocknorm(dB,dv,dB)
;			n_SNcyl=carttocyl_instant(SN,nPos[i,*])
;			n_SN1cyl=carttocyl_instant(SN_MX1,nPos[i,*])
;			n_SN2cyl=carttocyl_instant(SN_MX2,nPos[i,*])
;			n_SN3cyl=carttocyl_instant(SN_MX3,nPos[i,*])
;			n_SN_Acc=vecDotProduct(n_SNcyl,nS0n[i,*])
;			n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,nS0n[i,*])
;			n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,nS0n[i,*])
;			n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,nS0n[i,*])
;				if (n_SN_Acc lt 0) then begin;and (SN[0] lt 0.0) then begin
;				;;print,"n_SN_Acc=",n_SN_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
;				;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
;				;;print,"n_SNcyl=",n_SNcyl
;				;print,SN
;					SN=(-1)*SN
;				;print,SN
;				n_SNcyl=carttocyl_instant(SN,nPos[i,*])
;				n_SN_Acc=vecDotProduct(n_SNcyl,nS0n[i,*])
;				;;print,"n_SNcyl=",n_SNcyl
;				;print,"new n_SN_Acc=",n_SN_Acc
;			endif
;			if (n_SN_MX1_Acc lt 0) then begin;and (SN_MX1[0] lt 0.0) then begin
;				;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
;				;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
;				;;print,"n_SN1cyl=",n_SN1cyl
;				;print,SN_MX1
;				SN_MX1=(-1)*SN_MX1
;				;print,SN_MX1
;				n_SN1cyl=carttocyl_instant(SN_MX1,nPos[i,*])
;				n_SN_MX1_Acc=vecDotProduct(n_SN1cyl,nS0n[i,*])
;				;;print,"n_SN1cyl=",n_SN1cyl
;				;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
;			endif
;				if (n_SN_MX2_Acc lt 0) then begin;and (SN_MX2[0] lt 0.0) then begin
;				;;print,"n_SN_MX1_Acc=",n_SN_MX1_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
;				;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
;				;;print,"n_SN2cyl=",n_SN2cyl
;				;print,SN_MX2
;				SN_MX2=(-1)*SN_MX2
;				;print,SN_MX2
;				n_SN2cyl=carttocyl_instant(SN_MX2,nPos[i,*])
;				n_SN_MX2_Acc=vecDotProduct(n_SN2cyl,nS0n[i,*])
;				;;print,"n_SN2cyl=",n_SN2cyl
;				;print,"new n_SN_MX1_Acc=",n_SN_MX1_Acc
;			endif
;			if (n_SN_MX3_Acc lt 0) then begin;and (SN_MX3[0] lt 0.0) then begin
;				;;print,"n_SN_MX3_Acc=",n_SN_MX3_Acc
;				;;print,"S0n[i,*]=",TRANSPOSE(nS0n[i,*])
;				;;print,"nPos[i,*]=",TRANSPOSE(nPos[i,*])
;				;;print,"n_SN3cyl=",n_SN3cyl
;				SN_MX3=(-1)*SN_MX3
;				n_SN3cyl=carttocyl_instant(SN_MX3,nPos[i,*])
;				n_SN_MX3_Acc=vecDotProduct(n_SN3cyl,nS0n[i,*])
;				;;print,"n_SN3cyl=",n_SN3cyl
;				;print,"new n_SN_MX3_Acc=",n_SN_MX3_Acc
;			endif
;			;if n_SN_MX2_Acc lt 0 then nn_SN_MX2[i,*]=(-1)*nn_SN_MX2[i,*]
;			;if n_SN_MX3_Acc lt 0 then nn_SN_MX3[i,*]=(-1)*nn_SN_MX3[i,*]
;			Ns=list(SN_MX1,SN_MX2,SN_MX3,SN)
;			SN_AVG=normalizedcomponentaverage(Ns)
;
;			nn_SN[i,*]=SN
;			nn_SN_MX1[i,*]=SN_MX1
;			nn_SN_MX2[i,*]=SN_MX2
;			nn_SN_MX3[i,*]=SN_MX3
;			nn_SN_AVG[i,*]=SN_AVG
;			;nn_SN_AVG[i,*]=[mean([nn_SN_MX1[i,0],nn_SN_MX2[i,0],nn_SN_MX3[i,0],nn_SN[i,0]]),$
;				       ; mean([nn_SN_MX1[i,1],nn_SN_MX2[i,1],nn_SN_MX3[i,1],nn_SN[i,1]]),$
;				       ; mean([nn_SN_MX1[i,2],nn_SN_MX2[i,2],nn_SN_MX3[i,2],nn_SN[i,2]])]
;			n_SNAcyl=carttocyl_instant(nn_SN_AVG[i,*],nPos[i,*])
;			n_SN_AVG_Acc=vecDotProduct(n_SNAcyl,nS0n[i,*])
;				;print,'n_SN2[',N-1-i,",*]=",nn_SN[i,*]
;			
;			;shock angle th=acos(n • B_u/|B_u|)
;			;nSHOCKANGLE[i]=AngleCalc(nn_SN[i,*],BU)
;				nSHOCKANGLE_MC[i]=AngleCalc(nn_SN[i,*],BU)
;			nSHOCKANGLE_MX1[i]=AngleCalc(nn_SN_MX1[i,*],BU)
;			nSHOCKANGLE_MX2[i]=AngleCalc(nn_SN_MX2[i,*],BU)
;			nSHOCKANGLE_MX3[i]=AngleCalc(nn_SN_MX3[i,*],BU)
;			nSHOCKANGLE_AVG[i]=AngleCalc(nn_SN_AVG[i,*],BU)
;				nn_SN_Accs[i]=n_SN_Acc
;			nn_SN_MX1_Accs[i]=n_SN_MX1_Acc
;			nn_SN_MX2_Accs[i]=n_SN_MX2_Acc
;			nn_SN_MX3_Accs[i]=n_SN_MX3_Acc
;			nn_SN_AVG_Accs[i]=n_SN_AVG_Acc
;				nn_SN_CYL[i,*]=n_SNcyl
;			nn_SN_MX1_CYL[i,*]=n_SN1cyl
;			nn_SN_MX2_CYL[i,*]=n_SN2cyl
;			nn_SN_MX3_CYL[i,*]=n_SN3cyl
;			nn_SN_AVG_CYL[i,*]=n_SNAcyl
;
;				for k=iret, flagend do nSHOCKANGLE[k]=AngleCalc(nn_SN[i,*],nBU)
;		;	;print,'SHOCKANGLE2[',N-1-i,"]=",nSHOCKANGLE[i]
;		;	;print,"i=",i
;			;i+=10*imin
;		;	;print,"i=",i
;			foundEnd=1
;			if i eq i then break
;			endfor
			
		;endif 


	endfor
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	;print, "SHOCKNORMCALC			 END OF LOOP           SHOCKNORMCALC"
	;print, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	;NANs show up for some reason in places they shouldn't. Lets get set them to zero manually for now.
	n_SN[where(finite(n_SN,/NAN))]=0
	nn_SN[where(finite(nn_SN,/NAN))]=0

	n_SN2=Reverse(nn_SN)
	SHOCKANGLE2_MC=reverse(nSHOCKANGLE_MC)

	n_SN_MX1[where(finite(n_SN_MX1,/NAN))]=0
	nn_SN_MX1[where(finite(nn_SN_MX1,/NAN))]=0


	n_SN2_MX1=Reverse(nn_SN_MX1)
	SHOCKANGLE2_MX1=reverse(nSHOCKANGLE_MX1)
	
	n_SN_MX2[where(finite(n_SN_MX2,/NAN))]=0
	nn_SN_MX2[where(finite(nn_SN_MX2,/NAN))]=0

	n_SN2_MX2=Reverse(nn_SN_MX2)
	SHOCKANGLE2_MX2=reverse(nSHOCKANGLE_MX2)

	n_SN_MX3[where(finite(n_SN_MX3,/NAN))]=0
	nn_SN_MX3[where(finite(nn_SN_MX3,/NAN))]=0

	n_SN2_MX3=Reverse(nn_SN_MX3)
	SHOCKANGLE2_MX3=reverse(nSHOCKANGLE_MX3)

	n_SN_AVG[where(finite(n_SN_AVG,/NAN))]=0
	nn_SN_AVG[where(finite(nn_SN_AVG,/NAN))]=0

	n_SN2_AVG=Reverse(nn_SN_AVG)
	SHOCKANGLE2_AVG=reverse(nSHOCKANGLE_AVG)

	n_SN2_AVG=Reverse(nn_SN_AVG)
	SHOCKANGLE2_AVG=reverse(nSHOCKANGLE_AVG)

	n_SN2_best=Reverse(nn_SN_best)
	SHOCKANGLE2_best=reverse(nSHOCKANGLE_best)


	flowANGLE=flowANGLE+reverse(nflowANGLE)


	nn_SN_Accs=REVERSE(nn_SN_Accs)
	nn_SN_MX1_Accs=REVERSE(nn_SN_MX1_Accs)
	nn_SN_MX2_Accs=REVERSE(nn_SN_MX2_Accs)
	nn_SN_MX3_Accs=REVERSE(nn_SN_MX3_Accs)
	nn_SN_AVG_Accs=REVERSE(nn_SN_AVG_Accs)
	;nn_SN_Accs_Adj=REVERSE(nn_SN_Accs_Adj)
	;nn_SN_MX1_Accs_Adj=REVERSE(nn_SN_MX1_Accs_Adj)
	;nn_SN_MX2_Accs_Adj=REVERSE(nn_SN_MX2_Accs_Adj)
	;nn_SN_MX3_Accs_Adj=REVERSE(nn_SN_MX3_Accs_Adj)
	;nn_SN_AVG_Accs_Adj=REVERSE(nn_SN_AVG_Accs_Adj)

	nn_SN_best_Accs=REVERSE(nn_SN_best_Accs)

	nn_SN_CYL=REVERSE(nn_SN_CYL)
	nn_SN_MX1_CYL=REVERSE(nn_SN_MX1_CYL)
	nn_SN_MX2_CYL=REVERSE(nn_SN_MX2_CYL)
	nn_SN_MX3_CYL=REVERSE(nn_SN_MX3_CYL)
	nn_SN_AVG_CYL=REVERSE(nn_SN_AVG_CYL)
	nn_SN_best_CYL=REVERSE(nn_SN_best_CYL)

	locs=Where(n_SN[*,0] ne 0 ,n_SNc )

	locs2=Where(n_SN2[*,0] ne 0 ,n_SN2c )
	nlocs=Where(nn_SN[*,0] ne 0 ,nn_SNc )
	;print,locs2[0],N-1-nlocs[0]
	if locs2[0] gt 0 then begin 
		;;print,"n_SN2[locs2[0],*] =" ,n_SN2[locs2[0],*] 
		;print,"nn_SN[N-1-locs2[0],*] =", nn_SN[N-1-locs2[0],*]
	endif
	;foreach el , locs do begin
	;	;print,"BFD[",el,",*]=",BFD[el,*]
	;	;print,"t/N=",el*1.0/N
	;	;;print,"n_SN[",el,",*]=",transpose(n_SN[el,*])
	;endforeach

	;datB.y=n_SN

	;datT=datin
	;datT.y=SHOCKANGLE_MC
	yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	sub=""

	limPos.ysubtitle=sub

	;store_data,"Shock_NormalD",data=datB,dlim=limPos

	;options,"Shock_NormalD","ytitle","Normal Vector"

	;store_data,"Shock_AngleD",data=datT

	;options,"Shock_AngleD","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2

	datT=datin
	;datT.y=SHOCKANGLE2_MC

	;store_data,"Shock_NormalA",data=datB,dlim=limPos

	;options,"Shock_NormalA","ytitle","Normal Vector"

	;store_data,"Shock_AngleA",data=datT

	;options,"Shock_AngleA","ytitle","Shock Angle (radians)"

	n_SNf=n_SN+n_SN2
	SHOCKANGLEf_MC=SHOCKANGLE_MC+SHOCKANGLE2_MC

	n_SN_CYLf=n_SN_CYL+nn_SN_CYL

	store_data,"Shock_Normal_CYL",data={x:datT.x,y:n_SN_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
	options,'Shock_Normal_CYL','colors',['r','g','b']
	datT=datin
	datT.y=SHOCKANGLEf_MC
	datB.y=n_SNf
	store_data,"Shock_Normal",data=datB,dlim=limPos

	options,"Shock_Normal","ytitle","Normal Vector"

	store_data,"Shock_Angle",data=datT

	options,"Shock_Angle","ytitle","Shock Angle (radians)"
	;ACCf=nn_SN_Accs+n_SN_Accs

	;ACCf_Adj=nn_SN_Accs_Adj+n_SN_Accs_Adj
	;store_data,"Shock_Accuracy",data={x:datT.x,y:ACCf,ytitle:"dot product of shock normal !C with closest conic normal"}
	;store_data,"Shock_Accuracy_Adj",data={x:datT.x,y:ACCf_Adj,ytitle:"dot product of shock normal !C with closest conic normal"}
;------------MX1----------

	;datB.y=n_SN_MX1

	;datT=datin
	;datT.y=SHOCKANGLE_MX1
	;yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	;sub=""

	;limPos.ysubtitle=sub

	;store_data,"Shock_Normal_MX1_D",data=datB,dlim=limPos

	;options,"Shock_Normal_MX1_D","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX1_D",data=datT

	;options,"Shock_Angle_MX1_D","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2_MX1

	;datT=datin
	;datT.y=SHOCKANGLE2_MX1

	;store_data,"Shock_Normal_MX1_A",data=datB,dlim=limPos

	;options,"Shock_Normal_MX1_A","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX1_A",data=datT

	;options,"Shock_Angle_MX1_A","ytitle","Shock Angle (radians)"

	n_SNf_MX1=n_SN_MX1+n_SN2_MX1
	SHOCKANGLEf_MX1=SHOCKANGLE_MX1+SHOCKANGLE2_MX1


	;ACC1f=nn_SN_MX1_Accs+n_SN_MX1_Accs
	;ACC1f_Adj=nn_SN_MX1_Accs_Adj+n_SN_MX1_Accs_Adj
	;datT=datin
	datT.y=SHOCKANGLEf_MX1
	datB.y=n_SNf_MX1
	store_data,"Shock_Normal_MX1",data=datB,dlim=limPos

	options,"Shock_Normal_MX1","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX1",data=datT

	options,"Shock_Angle_MX1","ytitle","Shock Angle (radians)"

	;store_data,"Shock_Accuracy_MX1",data={x:datT.x,y:ACC1f,ytitle:"dot product of shock normal !C with closest conic normal"}
	;store_data,"Shock_Accuracy_MX1_Adj",data={x:datT.x,y:ACC1f_Adj,ytitle:"dot product of shock normal !C with closest conic normal"}
	n_SN_MX1_CYLf=n_SN_MX1_CYL+nn_SN_MX1_CYL

	store_data,"Shock_Normal_MX1_CYL",data={x:datT.x,y:n_SN_MX1_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
	;options,'Shock_Normal_MX1_CYL','colors',['r','g','b']

;-----------MX2------------
	;datB.y=n_SN_MX2

	;datT=datin
	;datT.y=SHOCKANGLE_MX2
	;yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	;sub=""

	limPos.ysubtitle=sub

	;store_data,"Shock_Normal_MX2_D",data=datB,dlim=limPos

	;options,"Shock_Normal_MX2_D","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX2_D",data=datT

	;options,"Shock_Angle_MX2_D","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2_MX2

	;datT=datin
	;datT.y=SHOCKANGLE2_MX2

	;store_data,"Shock_Normal_MX2_A",data=datB,dlim=limPos

	;options,"Shock_Normal_MX2_A","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX2_A",data=datT

	;options,"Shock_Angle_MX2_A","ytitle","Shock Angle (radians)"

	n_SNf_MX2=n_SN_MX2+n_SN2_MX2
	SHOCKANGLEf_MX2=SHOCKANGLE_MX2+SHOCKANGLE2_MX2
	ACC2f=nn_SN_MX2_Accs+n_SN_MX2_Accs
	;ACC2f_Adj=nn_SN_MX2_Accs_Adj+n_SN_MX2_Accs_Adj
	;datT=datin
	datT.y=SHOCKANGLEf_MX2
	datB.y=n_SNf_MX2
	store_data,"Shock_Normal_MX2",data=datB,dlim=limPos

	options,"Shock_Normal_MX2","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX2",data=datT

	options,"Shock_Angle_MX2","ytitle","Shock Angle (radians)"

	store_data,"Shock_Accuracy_MX2",data={x:datT.x,y:ACC2f,ytitle:"dot product of shock normal !C with closest conic normal"}
	;store_data,"Shock_Accuracy_MX2_Adj",data={x:datT.x,y:ACC2f_Adj,ytitle:"dot product of shock normal !C with closest conic normal"}
	n_SN_MX2_CYLf=n_SN_MX2_CYL+nn_SN_MX2_CYL

	store_data,"Shock_Normal_MX2_CYL",data={x:datT.x,y:n_SN_MX2_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
	options,'Shock_Normal_MX2_CYL','colors',['r','g','b']

;----------MX3-------------
	;datB.y=n_SN_MX3

	;datT=datin
	;datT.y=SHOCKANGLE_MX3
	;yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	;sub=""

	;limPos.ysubtitle=sub

	;store_data,"Shock_Normal_MX3_D",data=datB,dlim=limPos

	;options,"Shock_Normal_MX3_D","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX3_D",data=datT

	;options,"Shock_Angle_MX3_D","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2_MX3

	;datT=datin
	;datT.y=SHOCKANGLE2_MX3

	;store_data,"Shock_Normal_MX3_A",data=datB,dlim=limPos

	;options,"Shock_Normal_MX3_A","ytitle","Normal Vector"

	;store_data,"Shock_Angle_MX3_A",data=datT

	;options,"Shock_Angle_MX3_A","ytitle","Shock Angle (radians)"

	n_SNf_MX3=n_SN_MX3+n_SN2_MX3
	SHOCKANGLEf_MX3=SHOCKANGLE_MX3+SHOCKANGLE2_MX3
	ACC3f=nn_SN_MX3_Accs+n_SN_MX3_Accs
	ACC3f_Adj=nn_SN_MX3_Accs_Adj+n_SN_MX3_Accs_Adj
	;datT=datin
	datT.y=SHOCKANGLEf_MX3
	datB.y=n_SNf_MX3
	store_data,"Shock_Normal_MX3",data=datB,dlim=limPos

	options,"Shock_Normal_MX3","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX3",data=datT

	options,"Shock_Angle_MX3","ytitle","Shock Angle (radians)"

	store_data,"Shock_Accuracy_MX3",data={x:datT.x,y:ACC3f,ytitle:"dot product of shock normal !C with closest conic normal"}
	;store_data,"Shock_Accuracy_MX3_Adj",data={x:datT.x,y:ACC3f_Adj,ytitle:"dot product of shock normal !C with closest conic normal"}
;	MX3sqr=VECSQUARE(n_SNf_MX3)
;
	n_SN_MX3_CYLf=n_SN_MX3_CYL+nn_SN_MX3_CYL

	store_data,"Shock_Normal_MX3_CYL",data={x:datT.x,y:n_SN_MX3_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
;	options,'Shock_Normal_MX3_CYL','colors',['r','g','b']

;	store_data,"N_MX3_SQR",data={x:datT.x,y:MX3sqr,ytitle:"N_MX3.N_MX3"}
;	tplot,"N_MX3_SQR",/add



	;;;;;;;;AVERAGE NORMAL

	;sub=""

	;limPos.ysubtitle=sub

	;store_data,"Shock_Normal_AVG_D",data=datB,dlim=limPos

	;options,"Shock_Normal_AVG_D","ytitle","Normal Vector"

	;store_data,"Shock_Angle_AVG_D",data=datT

	;options,"Shock_Angle_AVG_D","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2_AVG

	;datT=datin
	;datT.y=SHOCKANGLE2_AVG

	;store_data,"Shock_Normal_AVG_A",data=datB,dlim=limPos
;
	;options,"Shock_Normal_AVG_A","ytitle","Normal Vector"

	;store_data,"Shock_Angle_AVG_A",data=datT

	;options,"Shock_Angle_AVG_A","ytitle","Shock Angle (radians)"

	n_SNf_AVG=n_SN_AVG+n_SN2_AVG
	SHOCKANGLEf_AVG=SHOCKANGLE_AVG+SHOCKANGLE2_AVG
	ACCAf=nn_SN_AVG_Accs+n_SN_AVG_Accs
	ACCAf_Adj=nn_SN_AVG_Accs_Adj+n_SN_AVG_Accs_Adj
	;datT=datin
	datT.y=SHOCKANGLEf_AVG
	datB.y=n_SNf_AVG
	store_data,"Shock_Normal_AVG",data=datB,dlim=limPos

	options,"Shock_Normal_AVG","ytitle","Normal Vector"

	store_data,"Shock_Angle_AVG",data=datT

	options,"Shock_Angle_AVG","ytitle","Shock Angle (radians)"

	store_data,"Shock_Accuracy_AVG",data={x:datT.x,y:ACCAf,ytitle:"dot product of shock normal !C with closest conic normal"}
	;store_data,"Shock_Accuracy_AVG_Adj",data={x:datT.x,y:ACCAf_Adj,ytitle:"dot product of shock normal !C with closest conic normal"}
	;AVGsqr=VECSQUARE(n_SNf_AVG)

	n_SN_AVG_CYLf=n_SN_AVG_CYL+nn_SN_AVG_CYL

	store_data,"Shock_Normal_AVG_CYL",data={x:datT.x,y:n_SN_AVG_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
;	options,'Shock_Normal_AVG_CYL','colors',['r','g','b']

	;store_data,"N_AVG_SQR",data={x:datT.x,y:AVGsqr,ytitle:"N_AVG.N_AVG"}
;	tplot,"N_AVG_SQR",/add



	;;;;;;;;best dot products

	;sub=""

	;limPos.ysubtitle=sub

	;store_data,"Shock_Normal_AVG_D",data=datB,dlim=limPos

	;options,"Shock_Normal_AVG_D","ytitle","Normal Vector"

	;store_data,"Shock_Angle_AVG_D",data=datT

	;options,"Shock_Angle_AVG_D","ytitle","Shock Angle (radians)"

	;datB.y=n_SN2_best

	;datT=datin
	;datT.y=SHOCKANGLE2_best

	;store_data,"Shock_Normal_AVG_A",data=datB,dlim=limPos
;
	;options,"Shock_Normal_AVG_A","ytitle","Normal Vector"

	;store_data,"Shock_Angle_AVG_A",data=datT

	;options,"Shock_Angle_AVG_A","ytitle","Shock Angle (radians)"

	n_SNf_best=n_SN_best+n_SN2_best
	SHOCKANGLEf_best=SHOCKANGLE_best+SHOCKANGLE2_best
	ACCBf=nn_SN_best_Accs+n_SN_best_Accs
	;datT=datin
	datT.y=SHOCKANGLEf_best
	datB.y=n_SNf_best
	store_data,"Shock_Normal_best",data=datB,dlim=limPos

	options,"Shock_Normal_best","ytitle","Normal Vector"

	store_data,"Shock_Angle_best",data=datT

	options,"Shock_Angle_best","ytitle","Shock Angle (radians)"

	store_data,"Shock_Accuracy_best",data={x:datT.x,y:ACC3f,ytitle:"dot product of shock normal !C with closest conic normal"}
	;AVGsqr=VECSQUARE(n_SNf_AVG)

	n_SN_AVG_CYLf=n_SN_AVG_CYL+nn_SN_AVG_CYL

	store_data,"Shock_Normal_best_CYL",data={x:datT.x,y:n_SN_AVG_CYLf,ytitle:"Normal Vector [cyl coords]",LABELS:['rho','phi','x'],LABFLAG:1}
	;options,'Shock_Normal_best_CYL','colors',['r','g','b']

	;store_data,"N_best_SQR",data={x:datT.x,y:AVGsqr,ytitle:"N_best.N_best"}
;	tplot,"N_AVG_SQR",/add

	store_data,'Flow_Angle_upstream',data={x:datT.x,y:flowANGLE,ytitle:"angle between v_u and B_u"}


	VdotB=fltarr(N)
	for i=0,N-1 do VdotB[i]=BB[i,0]*vp[i,0]+BB[i,1]*vp[i,1]+BB[i,2]*vp[i,2];cp(BB[i,*],vp[i,*])
	den=(datVmag.y *datBmag.y)
	for i=0,N-1 do VdotB[i]/=den[i]
	flowAngle2=acos(VdotB)

	store_data,'Flow_Angle',data={x:datT.x,y:flowAngle2}



end
