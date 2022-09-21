
function AC, y
	n=Size(y,/n_el)
	a=0
	z=y
	y2=y/max(y)
	for i=0,n-1 do begin
		For j=0,n-1 do a+=y2[j]*y2[(i+j) Mod n]
		z[i]=a/n
	endfor
	return, z*max(y)

end


pro autoCorr, plt,newName=newName


	if NOT KEYWORD_SET(newName) THEN newName=plt+'autocorrellated'

	get_data,plt,data=datp

	yy=datp.y

	zz=AC(yy)
	datp.y=zz

	store_data,newName,data=datp


end
