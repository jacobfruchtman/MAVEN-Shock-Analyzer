
function carttocyl_instant,A, P
	;help,A
	;Ax=A[0]
	;Ay=A[1]
	;Az=A[2]
	;print,A
	;print,A[0]
	xp=P[0]
	yp=P[1]
	zp=P[2]
	Bsqr=A[1]^2+A[2]^2
	dem=Sqrt(yp^2.+zp^2.)
	invdem=1.0/dem
	;print,'invdem=',invdem
	;nel=size(Ax,/n_el)
	;print,"nel=",nel
	;print,"size(xp)=",size(xp,/n_el)
	Nrho=fltarr(3)
	Nphi=fltarr(3)
	Ncx=fltarr(3)
	
	;for i=0, nel-1 do begin

	Nrho=[0.,yp*invdem,zp*invdem]
	Nphi=[0.,-zp*invdem,yp*invdem]
	;print,"Nrho=",Nrho
	;print,"A=",A
	;endfor
	;print,"min(Nrho)=",min(Nrho)
	;help,A
	;help,Nrho
	Arho=A[1]*Nrho[1]+A[2]*Nrho[2]
	Aphi=A[1]*Nphi[1]+A[2]*Nphi[2]
	;print,"Arho=",Arho
	;Arho=DotProduct(A,Nrho)
	;Aphi=DotProduct(A,Nphi)
	;PRINT(max(Aphi))

		
	if Abs(Arho^2+Aphi^2-Bsqr) gt .05 then begin ;;; FOR SOME REASON, IDL
		eps=Arho^2+Aphi^2 -Bsqr
		Arho*=Sqrt((Bsqr)/(Bsqr+eps))
		Aphi*=Sqrt((Bsqr)/(Bsqr+eps))
	endif
	CylA=[Arho,Aphi,A[0]]
	;print,A[0]
	;print,CylA[2]
	return, CylA

end
