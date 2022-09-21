function normalize0,A
	
	if (size(A,/n_dim))[0] eq 1 then return, A/norm(A)
	if (size(A,/dim))[0] eq 1 and (size(A,/dim))[1] eq 3 then return, A/norm(tanspose(A))
	B=A
	if (size(A,/dim))[0] eq 3 and (size(A,/dim))[0] ne 3	then B=transpose(B)
	N=(size(B,/dim))[0]
	z=fltarr(N,3)
	for i=0,N-1 do begin
		mag=sqrt(total(B[i,*]^2))
		for j=0,2 do z[i,j]=B[i,j]/mag
	endfor
	
	return,z
end
