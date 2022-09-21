function quickshrinker,array,st,en

	N=numel(array)

	y2=array
	st=0
	en=N-1

	while y2[0] gt mean([y2[0],y2[-1]])*.85 or y2[-1] lt mean([y2[0],y2[-1]])*1.15 and en-st gt N/2  do begin
		y0=y2[0]
		yf=y2[-1]
		men=mean([y0,yf])
		if y0 gt men*.85 then begin
			st++
			y2=y2[1:*]
			y0=y2[0]
		endif
		if yf lt men*1.15 then begin
			en--
			y2=y2[0:-2]
			yf=y2[-1]
		endif


	endwhile
	
	;while  mean(y2[0:10]) gt max(y2[11:20])
	help,y2
	return,y2
end

