pro boolAND,plt0,plt1

	get_data,plt0,data=datp0
	get_data,plt1,data=datp1
	
	get_data,'mvn_swica_density',data=dat,alim=limD
	z=datp0.y*datp1.y
	dat.y=z
	dat.ytitle="AND Flag"
	store_data,'comboBool',data=dat

	newdat={x:datp0.x,y:[[z],[z]],v:[0,1]}
	print,SIZE(newdat,/TYPE,/L64)
	M = [0,1]
	store_data,'comboBool',data = newdat


	options,comboBool,'spec',1	;switch from line plots to spectrogram mode
	options,name,'no_interp',1
	
END
