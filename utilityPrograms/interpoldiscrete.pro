function interpoldiscrete,oy,ox,nx

	odim1=n_elements(ox)
	ndim1=n_elements(nx)
	oN=n_elements(oy)
	dim2=N/odim1
	ny=fltarr(ndim1,dim2)

	if dim2 eq 1 then begin
		for i=0,ndim1-1 do begin
			mn=min(abs(ox-nx[i]),tn)
			ny[i]=oy[tn]
		endfor

	endif else
		for i=0,ndim1-1 do begin
			mn=min(abs(ox-nx[i]),tn)
			ny[i,*]=oy[tn,*]
		endfor
	endelse

	return,ny
end
