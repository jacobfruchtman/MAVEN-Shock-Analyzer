pro plotcrossproduct plt1,plt2,newname,ytitle=ytitle

	get_data,plt1,data=dat1

	get_data,plt2,data=dat2


	ny=devcrossproduct(dat1.y,dat2.y)
	dat={x:dat.x,y:ny}
	
	if keyword_set(ytitle) then str_element,dat,'ytitle',ytitle,/add
	store_data,newname,data=dat

end
