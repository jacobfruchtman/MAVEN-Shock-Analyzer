pro plotsnr,plt,width,newname=newname
	
	get_data,plt,data=dat
	ys=dat.y
	my=smooth(ys,width)
	N=N_elements(dat.x)
	sd=fltarr(N)
	for i=0,width/2 do begin
			 sd[i]=stddev(ys[0:i+width/2],/nan)
			 my[i]=mean(ys[0:i+width/2],/nan)
			j=N-1-i
			 sd[j]=stddev(ys[j-width/2:N-1],/nan)
			 my[j]=mean(ys[j-width/2:N-1],/nan)
	endfor
	for i=width/2,N-1-width/2 do sd[i]=stddev(ys[i-width/2:i+width/2],/nan)
	snr=my/sd

	dat.y=snr
	if not keyword_set(newname) then newname=plt+"_SNR"
	store_data,newname,data=dat
end
