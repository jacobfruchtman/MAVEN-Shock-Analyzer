pro findmonoregions,plt

	get_data,plt,data=dat

	yy=dat.y

	zi=monotize2(yy,0)

	zb=reverse(monotize2(reverse(yy),0))

	store_data,plt+"monoRegion",data={x:dat.x,y:zi,ytitle:"monotonic !C increase?"}	
	store_data,plt+"bmonoRegion",data={x:dat.x,y:zb,ytitle:"monotonic !C decrease?"}

end
