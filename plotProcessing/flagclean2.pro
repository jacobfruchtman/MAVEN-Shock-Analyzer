pro flagclean2,plt,loops=loops,newName=newName,downbias=downbias,upbias=upbias
	if not keyword_set(newName) then newName=plt+"_cleaned"
	if not keyword_set(loops) then loops=1

	downb=0
	upb=0

	if keyword_set(upbias) then upb=1
	if keyword_set(downbias) then downb=1

	get_data,plt,data=dat

	y=dat.y

	z=y
	N=size(y,/n_el)
	for i=0,loops do begin

		for j=2,N-2-i do begin
			if (y[j-1] eq y[j+i]) and (y[j-1] ne y[j]) then begin

				if upb and y[j-1] lt y[j] then continue
				if downb and y[j-1] gt y[j] then continue
				left=y[j-1]
				right=y[j+i]
				endr=right
				endl=left
				for k=j+i,N-1 do begin
					kk=k
					if y[k-1] eq y[k] then continue
					if upb and y[k-1] lt y[k] then continue
					if downb and y[k-1] gt y[k] then continue
					endr=y[k]
					break
				endfor
				for m=0,j-1 do begin
					l=j-1-m
					if y[l+1] eq y[l] then continue
					if upb and y[l] lt y[l+1] then continue
					if downb and y[l] gt y[l+1] then continue
					endl=y[l]
					break
				endfor
				if endl eq endr then z[l:kk]=endl
				
			endif

		endfor
		y=z

	endfor

	for j=2,N-3 do begin
			if (y[j-1] eq y[j+1]) and (y[j-1] ne y[j]) then begin

				if upb and y[j-1] lt y[j] then continue
				if downb and y[j-1] gt y[j] then continue
				left=y[j-1]
				right=y[j+1]
				endr=right
				endl=left
				for k=j+1,N-1 do begin
					kk=k
					if y[k-1] eq y[k] then continue
					if upb and y[k-1] lt y[k] then continue
					if downb and y[k-1] gt y[k] then continue
					endr=y[k]
					break
				endfor
				for m=0,j-1 do begin
					l=j-1-m
					if y[l+1] eq y[l] then continue
					if upb and y[l] lt y[l+1] then continue
					if downb and y[l] gt y[l+1] then continue
					endl=y[l]
					break
				endfor
				if endl eq endr then z[l:kk]=endl
				
			endif

		endfor
		y=z

	dat.y=z

	store_data,newName,data=dat

end
