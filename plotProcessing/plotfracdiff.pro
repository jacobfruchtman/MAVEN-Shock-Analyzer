pro plotfracdiff,plt1,plt2,newName=newName

	get_data,plt1,data=dat1
	get_data,plt2,data=dat2

	if not keyword_set(newName) then newName=plt1+"-"+plt2+"_fracdiff"

	y1=dat1.y
	y2=dat2.y
	x=dat1.x
	dim1=numel(x)
	N=numel(y1)
	dim2=N/dim1

	z=fltarr(dim1,dim2)

	if dim2 eq 1 then begin
		for i=0,dim1-1 do z[i]=fracdiff(y1[i],y2[i])

	endif else begin
		for j=0,dim2-1 do for i=0,dim1-1 do z[i,j]=fracdiff(y1[i,j],y2[i,j])
	endelse

	dat1.y=z

	store_data,newName,data=dat1
end
