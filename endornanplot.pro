pro endornanplot,plt

	get_data,plt,data=dat
	get_data,'sublengths_end',data=datEnd

	subend=datEnd.y
	y=dat.y
	N=numel(y)
	N0=(size(y,/dim))[0]
	dim2=(N/N0)
	z=nanarr(N0,dim2)
	;help,y
	;help,z
	;help,dim2
	;help,N0

	if dim2 gt 1 then begin
		for i=0,N0-1 do if subend[i] eq 1 then z[i,*]=y[i,*]
	endif else for i=0,N0-1 do if subend[i] eq 1 then z[i]=y[i]
	store_data,plt+"_End",data={x:dat.x,y:z}
	;print,z[0]
end
