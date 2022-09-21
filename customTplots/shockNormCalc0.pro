
function isArray,x
	
	nell=size(v1,/N_ELEMENTS)
	if(nell EQ 3) THEN return,0 ELSE return,1	

END


function vecCrossProduct, v1,v2
	;print,"v1,v2"
	;print,v1,v2
	
	px=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
	py=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
	pz=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
	;print,"px,py,pz=",px,py,pz
	product=[[px],[py],[pz]]
	;print,product
	return, product
END

function arrayCrossProduct, v1,v2

	px=v1[*,2-1]*v2[*,3-1]-v1[*,3-1]*v2[*,2-1]
	py=v1[*,3-1]*v2[*,1-1]-v1[*,1-1]*v2[*,3-1]
	pz=v1[*,1-1]*v2[*,2-1]-v1[*,2-1]*v2[*,1-1]
	
	product=transpose([transpose(px),transpose(py),transpose(pz)])
	
	return, product
END


function cp, v1,v2
	;product=0
	;nel1=size(v1,/N_ELEMENTS)
	
	if(isArray(v1)) THEN product= vecCrossProduct(v1,v2) ELSE product=arrayCrossProduct(v1,v2)

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

function shocknorm, a , b, c
	sn=b*0
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


;	print,"bd=",bd
;	print,"bu=",bu
	
	nu= cp(cp(a,b),c)

	sn=vecdiv(nu,vecmag(nu))
;	print,"sn=",sn

	return, sn
end


function shocknormMC, bu , bd
	sn=bu*0
;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|
;	print,"bd=",bd
;	print,"bu=",bu
	db=bd-bu
	nu= cp(cp(bd,bu),db)

	sn=vecdiv(nu,vecmag(nu))
;	print,"sn=",sn

	return, sn
end

function shocknormMX1, bu , db,dv
	sn=bu*0
;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|
;	print,"bd=",bd
;	print,"bu=",bu
	db=bd-bu
	nu= cp(cp(bd,bu),db)

	sn=vecdiv(nu,vecmag(nu))
;	print,"sn=",sn

	return, sn
end


function vecDotProduct, a, b
	;print,"a=",a
	;print,"b=",b
	return, a[0]*b[0]+a[1]*b[1]+a[2]*b[2]
end

function arrayDotProduct, a , b
	return, a[*,0]*b[*,0]+a[*,1]*b[*,1]+a[*,2]*b[*,2]
end


;functon vDP, a, b
;	return, vecDotProduct(a,b)
;end

function AngleCalc, n, BU

	magB= vecMag(BU)
	num=vecDotProduct(n,BU)
	;print,"BU=",BU
	;print,"n=",n
	
	;print,"num=",num
	;print,"magB",magB
	
	return, acos(num/magB)
end



pro shockNormCalc

	get_data,"BF_shockA",data=datBFA,limits=limBFA
	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout
	get_data,"BF_shockD",data=datBFD,limits=limBFD

	get_data,"mvn_B_1sec_MAVEN_MSO",data=datB
	get_data,"MAVEN_POS_(MARS-MSO)",data=datPos,alim=limPos
	get_data,'B_fittedderiv',data=datDer

	get_data,'B_fittedderiv_updown',data=datUD

	get_data,"mvn_swics_velocity_MAVEN_MSO",data=datV

	UD=datUD.y

	BFA=datBFA.y
	BFD=datBFD.y
	BB=datB.y
	Bder=datB.y
	pos=datPos.y
	xpos=datPos.x
	xx=datB.x
	help,datV
	ov=datV.y
	vt=datV.x

	ovx=ov[*,0]
	ovy=ov[*,1]
	ovz=ov[*,2]

	vx=interpol(ovx,vt,xx)
	vy=interpol(ovy,vt,xx)
	vz=interpol(ovz,vt,xx)

	vp=[[vx],[vy],[vz]]

	npos=Size(xpos,/n_el)
;;;;;;;;note: BB[t,*] is the B vector at time t
;;;;;;;;	  BB[*,k] is the k component of B, B_k, for all times

	B_D=BB*0
	B_U=BB*0

	v_u=vp*0
	v_d=vp*0	

	yposNewX=interpol(pos[*,0],xpos,xx)
	yposNewY=interpol(pos[*,1],xpos,xx)
	yposNewZ=interpol(pos[*,2],xpos,xx)
	posNew=[[yposNewX],[yposNewY],[yposNewZ]]

	datPosN=datB
	datPosN.y=posNew
	datPosN.x=xx

	store_data,"POS_interpolated_(MARS_MSO)",data=datPosN,dlim=limPos

;	xBFA=datBFA.x
;	xBFD=datBFD.x
	xB=datB.x
	N=size(xB,/n_el)
	print,"N=",N
	flagstart=0
	flagon=0
	flagend=0

	Bsum=BB[0,*]*0

	n_SN=BB*0
	n_SN_MX1=BB*0
	n_SN_MX2=BB*0
	n_SN_MX3=BB*0

	SHOCKANGLE_MC=BFD*0
	SHOCKANGLE_MX1=BFD*0
	SHOCKANGLE_MX2=BFD*0
	SHOCKANGLE_MX3=BFD*0

	imin=icalc(xB,60)

;											(B_d x B_u)x(ΔB)						
; The shocknormal vector is given by n_mc =± -----------------
;											|(B_d x B_u)x(ΔB)|

; where ΔB=B_d-B_u
	atFlag=0

	for i=10*imin,N-1-10*imin do begin
		BD=0
		BU=0
		vd=0
		vu=0		

		atFlag=0
		Bsum=0
		if(BFD[i] eq BFA[i]) or (BFA[i] ne 0) then continue
		;print,"i=",i
		;if((Bder[i] gt Bder[i-1] ) and (Bder[i] gt Bder[i-1])) then begin
		if (((UD[i] eq 1) and (UD[i-1] eq 0) ) and (Bder[i] gt 0)) then begin
		;if((Bder[i] gt Bder[i-1] ) and (Bder[i] gt Bder[i-1])) and (BFD[i] ne 0 ) and (BFD[i-imin/12] eq 0) then begin
		;if(BFD[i] ne 0 ) and (BFD[i-imin/12] eq 0) then begin
			atFlag=1
			flagstart=i
			;establish B_U
			iret=i-4*imin

		;	for j=iret,iret+imin do Bsum+=BB[j,*]

		;	B_avg=vecdiv(Bsum,imin)
		;	Bsum=BB[0,*]*0
			Bum=[Mean(BB[i-4*imin:i-3*imin,0]),Mean(BB[i-4*imin:i-3*imin,1]),Mean(BB[i-4*imin:i-3*imin,2])]
			vum=[Mean(vp[i-4*imin:i-3*imin,0]),Mean(vp[i-4*imin:i-3*imin,1]),Mean(vp[i-4*imin:i-3*imin,2])]
			for j=iret,i do begin

			
				B_U[j,*]=Bum
				v_U[j,*]=vum

			endfor

			;establish B_d


			;this will be an utterly bullshit implementation usign a loop ending with i itself being updated

			foundEND=0	
			for j=i,N-1 do begin
				if foundEND eq 1 then break
				if (BFD[j+10] ne 0) then continue
				


				print,"j=",j
				ll=mean([mean([j,i]),i])
				print,"ll=",ll
				jmin=imin
				print, "BFD[ll]=BFD[",ll,"]=",BFD[ll]

				While 1*(BFD[ll] ne 0) ne 1 do ll=mean([ll,j])

				print, "BFD[ll]=BFD[",ll,"]=",BFD[ll]
				startON=1*(BFD[ll] ne 0)
				while (BFD[ll+jmin] eq 0 ) and (startON eq 1) do jmin=jmin/2
				print,'ll+jmin=',ll+jmin
				BD=[Mean(BB[ll:ll+jmin,0]),Mean(BB[ll:ll+jmin,1]),Mean(BB[ll:ll+jmin,2])]
				vd=[Mean(vp[ll:ll+jmin,0]),Mean(vp[ll:ll+jmin,1]),Mean(vp[ll:ll+jmin,2])]

				;flagend=j

				;midflag=(i+j)/2
				;qflag=(i+midflag)/2
				;print,'qflag=',qflag

				;for k=qflag,qflag+imin do Bsum+= BB[k,*]
				
				;BD=vecdiv(Bsum,imin)
				BU=B_U[i,*]

				vu=v_u[i,*]
				;print,Bsum
				;for k=i-10*imin, qflag+imin do begin
				;		B_D[k,*]=BD
				;		n_SN[k,*]=shocknorm(BU,BD)
				;endfor				

				print,"pos[",i,"]=",pos[i/npos,*]
				print,"posN[",i,"]=",posNew[i,*]
				print,"BD=",BD
				print,"BU=",BU
				dB=BD-BU
				
				dv=vd-vu

				;n_SN[i,*]=shocknormMC(BU,BD)
				n_SN[i,*]=shocknorm(BD,BU,dB)
				n_SN_MX1[i,*]=shocknorm(BU,dv,dB)
				n_SN_MX2[i,*]=shocknorm(BD,dv,dB)
				n_SN_MX3[i,*]=shocknorm(dB,dv,dB)
				print,'n_SN[',i,",*]=",n_SN[i,*]
				
				;shock angle th=acos(n • B_u/|B_u|)
				SHOCKANGLE_MC[i]=AngleCalc(n_SN[i,*],BU)
				SHOCKANGLE_MX1[i]=AngleCalc(n_SN_MX1[i,*],BU)
				SHOCKANGLE_MX2[i]=AngleCalc(n_SN_MX2[i,*],BU)
				SHOCKANGLE_MX3[i]=AngleCalc(n_SN_MX3[i,*],BU)
				;for k=iret, flagend do SHOCKANGLE[k]=AngleCalc(n_SN[i,*],BU)
				
				;print,"i=",i
				i+=10*imin
				;print,"i=",i
				foundEND=1
				print,"end of first innermost loop. About to break. j=",j
				if i eq i then break

			endfor
			print,"just below innermost loop Inside If statement. i=",i
			
		endif 
		;print,"just below If statement, at end of innermost loop. i=",i

	endfor

	print,"endfor 1"

	print, "last shock at i=",flagstart
	print, "which is N-i=",N-flagstart," indices from the end"
	nB=REVERSE(BB)
	nv=REVERSE(vp)
	nBFA=Reverse(BFA)
	nBFD=Reverse(BFD)
	nBder=Reverse(Bder)
	nUD=Reverse(UD)
	nB_D=BB*0
	nB_U=BB*0
	nv_D=BB*0
	nv_U=BB*0
	nBD=0
	nBU=0
	nn_SN=n_SN*0
	nn_SN_MX1=n_SN*0
	nn_SN_MX2=n_SN*0
	nn_SN_MX3=n_SN*0
	nSHOCKANGLE_MC=SHOCKANGLE_MC*0
	nSHOCKANGLE_MX1=SHOCKANGLE_MC*0
	nSHOCKANGLE_MX2=SHOCKANGLE_MC*0
	nSHOCKANGLE_MX3=SHOCKANGLE_MC*0
	for i=10*imin,N-1-10*imin do begin
		nBD=0
		nBU=0
		nvD=0
		nvU=0
		atFlag=0
		ni=N-i
		;if(nBFA[i] eq nBFD[i]) or (nBFD[i] ne 0) then continue
		;print,"i,ni=",i,ni
		if (nUD[i] eq 1) and (nUD[i-1] eq 0)  then begin
		;if((nBder[i] lt nBder[i-1] ) and (Bder[i] lt nBder[i-1])) then begin
;		if(nBFA[i] ne 0 ) and (nBFA[i-imin/30] eq 0) and ((nBder[i] lt nBder[i-1] ) and (Bder[i] lt nBder[i-1])) then begin
			atFlag=1
			flagstart=i
			;establish B_U
			iret=i-10*imin
			;Bsum=0
			;for j=iret,iret+imin do Bsum+=nB[j,*]

			;B_avg=vecdiv(Bsum,imin)
			;Bsum=BB[0,*]*0
			Bum=[Mean(nB[i-4*imin:i-3*imin,0]),Mean(nB[i-4*imin:i-3*imin,1]),Mean(nB[i-4*imin:i-3*imin,2])]
			vum=[Mean(nv[i-4*imin:i-3*imin,0]),Mean(nv[i-4*imin:i-3*imin,1]),Mean(nv[i-4*imin:i-3*imin,2])]
			print,"Bum=",Bum
			for j=i-10*imin,i+1 do begin
				nB_U[j,*]=Bum
				nv_U[j,*]=vum

			endfor

			;establish B_d


			;this will be an utterly bullshit implementation usign a loop ending with i itself being updated

			foundEND=0
			for j=i,N-1 do begin
				if foundEND eq 1 then break
				if nBFA[j+2] ne 0 then continue


				print,"i,j=",i,",",j
				ll=mean([mean([j,i]),i])
				print,"ll=",ll
				jmin=imin
				print, "nBFA[ll]=nBFA[",ll,"]=",nBFA[ll]

				While 1*(nBFA[ll] ne 0) ne 1 do begin 
					print,ll
					ll=mean([ll,j])
					if ll eq j then break
				endwhile
				print, "BFA[ll]=BFA[",ll,"]=",nBFA[ll]
				startON=1*(nBFA[ll] ne 0)
				while (nBFA[ll+jmin] eq 0 ) and (startON eq 1) do begin
					 print,jmin					
					 jmin=jmin/2
					 if (jmin le 1) then break
				endwhile
				print,'ll+jmin=',ll+jmin
				BD=[Mean(nB[ll:ll+jmin,0]),Mean(nB[ll:ll+jmin,1]),Mean(nB[ll:ll+jmin,2])]
				vd=[Mean(nv[ll:ll+jmin,0]),Mean(nv[ll:ll+jmin,1]),Mean(nv[ll:ll+jmin,2])]

				;Bsum=BB[0,*]*0
				;flagend=j

				;midflag=(i+j)/2
				;qflag=(i+midflag)/2
				;print,'qflag=',qflag

				;for k=qflag,qflag+imin do Bsum+= nB[k,*]
				
				;nBD=vecdiv(Bsum,imin)
				BU=nB_U[i,*]
				vu=nv_U[i,*]
				
				;print,Bsum
				;for k=i-10*imin, qflag+imin do nB_D[k,*]=B_avg
				;for k=i-4*imin, qflag+imin do begin
				;		nB_D[k,*]=nBD
				;		nn_SN[k,*]=shocknorm(nBU,nBD)
				;endfor	
				
				;nBD=nB_D[i,*]
				print,"npos[",i,"]=",pos[(N-1-i)/npos,*]
				print,"BD=",BD
				print,"BU=",BU
				dB=BD-BU
				dv=vd-vu
				
				;nn_SN[i,*]=shocknormMC(BU,BD)

				nn_SN[i,*]=shocknorm(BD,BU,dB)
				nn_SN_MX1[i,*]=shocknorm(BU,dv,dB)
				nn_SN_MX2[i,*]=shocknorm(BD,dv,dB)
				nn_SN_MX3[i,*]=shocknorm(dB,dv,dB)

				print,'n_SN2[',N-1-i,",*]=",nn_SN[i,*]
				
				;shock angle th=acos(n • B_u/|B_u|)
				;nSHOCKANGLE[i]=AngleCalc(nn_SN[i,*],BU)

				nSHOCKANGLE_MC[i]=AngleCalc(nn_SN[i,*],BU)
				nSHOCKANGLE_MX1[i]=AngleCalc(nn_SN_MX1[i,*],BU)
				nSHOCKANGLE_MX2[i]=AngleCalc(nn_SN_MX2[i,*],BU)
				nSHOCKANGLE_MX3[i]=AngleCalc(nn_SN_MX3[i,*],BU)

;				for k=iret, flagend do nSHOCKANGLE[k]=AngleCalc(nn_SN[i,*],nBU)
			;	print,'SHOCKANGLE2[',N-1-i,"]=",nSHOCKANGLE[i]
			;	print,"i=",i
				i+=10*imin
			;	print,"i=",i
				foundEnd=1
				if i eq i then break

			endfor
			
			
		endif 


	endfor
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

	locs=Where(n_SN[*,0] ne 0 ,nSNc )

	locs2=Where(n_SN2[*,0] ne 0 ,nSN2c )
	nlocs=Where(nn_SN[*,0] ne 0 ,nnSNc )
	print,locs2[0],N-1-nlocs[0]
	print,"n_SN2[locs2[0],*] =" ,n_SN2[locs2[0],*] 
	print,"nn_SN[N-1-locs2[0],*] =", nn_SN[N-1-locs2[0],*]

	;foreach el , locs do begin
	;	print,"BFD[",el,",*]=",BFD[el,*]
	;	print,"t/N=",el*1.0/N
	;	print,"n_SN[",el,",*]=",transpose(n_SN[el,*])
	;endforeach

	datB.y=n_SN

	datT=datBFA
	datT.y=SHOCKANGLE_MC
	yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	sub=""

	limPos.ysubtitle=sub

	store_data,"Shock_NormalD",data=datB,dlim=limPos

	options,"Shock_NormalD","ytitle","Normal Vector"

	store_data,"Shock_AngleD",data=datT

	options,"Shock_AngleD","ytitle","Shock Angle (radians)"

	datB.y=n_SN2

	datT=datBFA
	datT.y=SHOCKANGLE2_MC

	store_data,"Shock_NormalA",data=datB,dlim=limPos

	options,"Shock_NormalA","ytitle","Normal Vector"

	store_data,"Shock_AngleA",data=datT

	options,"Shock_AngleA","ytitle","Shock Angle (radians)"

	n_SNf=n_SN+n_SN2
	SHOCKANGLEf_MC=SHOCKANGLE_MC+SHOCKANGLE2_MC
	datT=datBFA
	datT.y=SHOCKANGLEf_MC
	datB.y=n_SNf
	store_data,"Shock_Normal",data=datB,dlim=limPos

	options,"Shock_Normal","ytitle","Normal Vector"

	store_data,"Shock_Angle",data=datT

	options,"Shock_Angle","ytitle","Shock Angle (radians)"


;------------MX1----------

	datB.y=n_SN_MX1

	datT=datBFA
	datT.y=SHOCKANGLE_MX1
	yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	sub=""

	limPos.ysubtitle=sub

	store_data,"Shock_Normal_MX1_D",data=datB,dlim=limPos

	options,"Shock_Normal_MX1_D","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX1_D",data=datT

	options,"Shock_Angle_MX1_D","ytitle","Shock Angle (radians)"

	datB.y=n_SN2_MX1

	datT=datBFA
	datT.y=SHOCKANGLE2_MX1

	store_data,"Shock_Normal_MX1_A",data=datB,dlim=limPos

	options,"Shock_Normal_MX1_A","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX1_A",data=datT

	options,"Shock_Angle_MX1_A","ytitle","Shock Angle (radians)"

	n_SNf_MX1=n_SN_MX1+n_SN2_MX1
	SHOCKANGLEf_MX1=SHOCKANGLE_MX1+SHOCKANGLE2_MX1
	datT=datBFA
	datT.y=SHOCKANGLEf_MX1
	datB.y=n_SNf_MX1
	store_data,"Shock_Normal_MX1",data=datB,dlim=limPos

	options,"Shock_Normal_MX1","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX1",data=datT

	options,"Shock_Angle_MX1","ytitle","Shock Angle (radians)"




;-----------MX2------------
	datB.y=n_SN_MX2

	datT=datBFA
	datT.y=SHOCKANGLE_MX2
	yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	sub=""

	limPos.ysubtitle=sub

	store_data,"Shock_Normal_MX2_D",data=datB,dlim=limPos

	options,"Shock_Normal_MX2_D","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX2_D",data=datT

	options,"Shock_Angle_MX2_D","ytitle","Shock Angle (radians)"

	datB.y=n_SN2_MX2

	datT=datBFA
	datT.y=SHOCKANGLE2_MX2

	store_data,"Shock_Normal_MX2_A",data=datB,dlim=limPos

	options,"Shock_Normal_MX2_A","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX2_A",data=datT

	options,"Shock_Angle_MX2_A","ytitle","Shock Angle (radians)"

	n_SNf_MX2=n_SN_MX2+n_SN2_MX2
	SHOCKANGLEf_MX2=SHOCKANGLE_MX2+SHOCKANGLE2_MX2
	datT=datBFA
	datT.y=SHOCKANGLEf_MX2
	datB.y=n_SNf_MX2
	store_data,"Shock_Normal_MX2",data=datB,dlim=limPos

	options,"Shock_Normal_MX2","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX2",data=datT

	options,"Shock_Angle_MX2","ytitle","Shock Angle (radians)"




;----------MX3-------------
	datB.y=n_SN_MX3

	datT=datBFA
	datT.y=SHOCKANGLE_MX3
	yt=''


	;if s then dat.ytitle="abs("+ dat.YTITLE+")" $
	;else if t then limits.ytitle="abs("+ limits.YTITLE+")" else begin
	;	if NOT KEYWORD_SET(newT) THEN newT=plt
	;	options,plt,'ytitle',newT
	;	limits.ytitle="abs("+ limits.YTITLE+")"
	;endelse

	sub=""

	limPos.ysubtitle=sub

	store_data,"Shock_Normal_MX3_D",data=datB,dlim=limPos

	options,"Shock_Normal_MX3_D","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX3_D",data=datT

	options,"Shock_Angle_MX3_D","ytitle","Shock Angle (radians)"

	datB.y=n_SN2_MX3

	datT=datBFA
	datT.y=SHOCKANGLE2_MX3

	store_data,"Shock_Normal_MX3_A",data=datB,dlim=limPos

	options,"Shock_Normal_MX3_A","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX3_A",data=datT

	options,"Shock_Angle_MX3_A","ytitle","Shock Angle (radians)"

	n_SNf_MX3=n_SN_MX3+n_SN2_MX3
	SHOCKANGLEf_MX3=SHOCKANGLE_MX3+SHOCKANGLE2_MX3
	datT=datBFA
	datT.y=SHOCKANGLEf_MX3
	datB.y=n_SNf_MX3
	store_data,"Shock_Normal_MX3",data=datB,dlim=limPos

	options,"Shock_Normal_MX3","ytitle","Normal Vector"

	store_data,"Shock_Angle_MX3",data=datT

	options,"Shock_Angle_MX3","ytitle","Shock Angle (radians)"



end
