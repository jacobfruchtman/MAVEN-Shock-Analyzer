pro postexter

	get_data,91,data=datPOS
	POS=datPOS.y
	t=datPOS.x
	N=numel(t)
	openW,1,'shockpositions.txt'
	printf,1,';;unix timestamp , X_MSO , YMSO,ZMSO'
	for i=0,N-1 do begin
		oX=POS[i,0]
		oY=POS[i,1]
		oZ=POS[i,2]
		ot=time_string(t[i])
		printf,1,ot,',',oX,',',oY,',',oZ
	endfor
	close,1
end
