
function carttocyl,A, P
	;help,A
	Ax=A[*,0]
	Ay=A[*,1]
	Az=A[*,2]

	xp=P[*,0]
	yp=P[*,1]
	zp=P[*,2]

	dem=Sqrt(yp^2+zp^2)
	invdem=1.0/dem
	;print,'invdem=',invdem
	nel=size(Ax,/n_el)
	;print,"nel=",nel
	;print,"size(xp)=",size(xp,/n_el)
	Nrho=fltarr(nel,3)
	Nphi=fltarr(nel,3)
	Ncx=fltarr(nel,3)
	
	for i=0, nel-1 do begin

		Nrho[i,*]=[0,yp[i],zp[i]]*invdem[i]
		Nphi[i,*]=[0,-zp[i],yp[i]]*invdem[i]
	endfor
	;print,"min(Nrho)=",min(Nrho)
	;help,A
	;help,Nrho
	Arho=DotProduct(A,Nrho)
	Aphi=DotProduct(A,Nphi)
	;PRINT(max(Aphi))
	CylA=[[Arho],[Aphi],[Ax]]
		
	return, CylA

end
