
function vecDot, x,y,pwr 
	i=0
	z=x[0,*]*0.0
	FOR i = 0, 2 DO BEGIN	
	   	z[i]=total(x[i,*]*y[i,*]^pwr)
	
		ENDFOR
	RETURN,z
END

function vecDiv,x,y

	i=0
	z=x[0,*]*0.0
	FOR i = 0, 2 DO BEGIN	
	   	z[i]=x[i]/Total(y[i,*])
	
		ENDFOR
	RETURN,z
END

pro SpectroMoment, plt ;,shortName

	get_data,plt,data=datp
	get_data,'mvn_swica_density',data=dat0
	y=datp.y
	v=datp.v
	;total(daten.v[1,*])

	m1=vecDiv(vecDot(y,v,1),y)
	dat0.y=m1
	dat0.YTITLE="0th moment "+ datp.YTITLE+")"

	if(N_PARAMS() EQ 1) THEN nm=plt ;ELSE nm=shortName$
	
	name1=nm+"_0thMoment"
	name2=nm+"_2ndMoment"
	store_data,name1,data=dat0
	m1=vecDiv(vecDot(y,v,2),y)
	dat0.y=m1
	dat0.YTITLE="2nd moment "+ datp.YTITLE+")"
	;name=plt+"_2ndMoment"

	store_data,name2,data=dat0
END
