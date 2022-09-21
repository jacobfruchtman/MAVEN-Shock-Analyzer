pro plotToCyl,plt,pltpos=pltpos,newName=newName

	if not keyword_set(newName) then newName=plt+"_Cyl"

	if not keyword_set(pltpos) then pltpos='POS_interpolated_(MARS_MSO)'
	get_data,pltpos,data=dpos
	get_data,plt,data=datR
	
	post=dpos.x
	oposx=dpos.y[*,0]
	oposy=dpos.y[*,1]
	oposz=dpos.y[*,2]

	pos=dpos.y
	R=datR.y


	

	;Pphi=atan(oposz,oposy)
	;Prho=SQRT(oposz^2+oposy^2)
	;Pxx=oposx

	;R_mars=3389.5
	Rcyl=carttocyl(R,pos)
	Rrho=Rcyl[*,0]
	Rphi=Rcyl[*,1]
	Rxx=Rcyl[*,2]
	datR.y=Rcyl
	store_data,newName,data=datR
	tplot_element,newName,"LABFLAG",1,/add
	tplot_element,newName,"LABELS",['rho','phi','x'],/add
	options,newName,'colors',['r','g','b']
end

	
