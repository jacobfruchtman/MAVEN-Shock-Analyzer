pro onofflengthplot,plt,newName=newName,onsonly=onsonly

	if not keyword_set(newName) then newName=plt+"blocksize"


	get_data,plt,data=dat
	flg=dat.y

	N=numel(flg)
	N0=numel(flg[*,0])
	dim2=N/N0
	z=fltarr(N0,dim2)
	if dim2 eq 1 then z=onofflength(flg,onsonly=onsonly)
	for i=0,dim2-1 do begin
		print,"i=",i
		z[*,i]=onofflength(flg,onsonly=onsonly)
	endfor
	
	dat.y=z

	store_data,newName,data=dat

end
