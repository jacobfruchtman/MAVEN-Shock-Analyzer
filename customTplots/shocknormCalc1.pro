
function isArray,x
	
	nell=size(v1,/N_ELEMENTS)
	if(nell EQ 3) THEN return,0 ELSE return,1	

END

function vecCrossProduct, v1,v2
	print,"v1,v2"
	print,v1,v2
	
	px=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
	py=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
	pz=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
	print,"px,py,pz"
	print,px,py,pz
	product=transpose([px,py,pz])
	print,product
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


function greg2sec,st
	
	b=0
	b = st.Split('/')
	datestring=b[0]
	timestring1=b[1]

	datearr=datestring.Split('-')

	timearr=timestring1.Split('$')

	t=datearr[0]*365.4+datearr[1]*12+datearr[2]
	t=t*24.0+timearr[0]
	t=t*60.0+timearr[1]
	t=t*60.0
	return, t
END

FUNCTION greg2epoch, st
	
	et=greg2sec('1970-01-01/00$00')
	print, et
	ct=greg2sec(st)
	t=ct-et
	print,t
	return, t
END

pro shocknormCalc1, stDown,stUp
	get_data, "mvn_B_1sec",data=datB

	;READ, mnth, PROMPT='Enter #month:'
;2015-01-28/04:08
	;b1 = stDown.Split('/')
	;b2 = stUp.Split('/')
	;datestring1=b1[0]
	;datestring2=b2[0]
	;timestring1=b1[1]
	;timestring2=b2[1]

	;dates1=datestring1.Split('-')
	;dates2=datestring2.Split('-')

	;timearr1=timestring1.Split(':')
	;timearr1=timestring1.Split(':')
	b0x=datB.X[0]
	b0dx=datB.X[1]-datB.X[0]
	;t1=greg2epoch(stDown)
	;t2=greg2epoch(stUp)
	t1=time_double(stDown)
	t2=time_double(stUp)
	print,t1,t2,t2-t1,  t1 MOD 1000, t2 MOD 1000
	print,round(t1-b0x)
	XX=datB.X
	x1=round(t1-b0x)
	x2=round(t2-b0x)
	print, x1,x2
	Bd=TRANSPOSE(datB.Y[x1,*])
	Bu=TRANSPOSE(datB.Y[x2,*])
	dB=Bd-Bu
	Print,Bd
	print,Bd[0]
	
	PRINT, TRANSPOSE(datB.Y[x1,*])
	PRINT, TRANSPOSE(datB.Y[x2,*])
	dc=cp(cp(Bd,Bu),dB)
	print,"dc"
	print,dc
	magdc=VECMAG(dc)
	nmc=vecdiv(dc,magdc)
	print,"n_mc="
	print,nmc
	;get_data,'mvn_swica_velocity',data=datVel,alim=limV

	

END
