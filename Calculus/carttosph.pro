;A should be the vector in cartesian MSO you want to switch to the spehrical coordinate system
;P should be the (interpolated) postion vector in cartesian MSO.  

function carttosph,A, P
	help,A
	Ax=A[*,0]
	Ay=A[*,1]
	Az=A[*,2]

	xp=P[*,0]
	yp=P[*,1]
	zp=P[*,2]

	demT=Sqrt(yp^2+zp^2)
	demR=Sqrt(xp^2+yp^2+zp^2)
	invdemP=1.0/demP
	
	invdemR=1.0/demR
	invdemT=1.0/(demR*demP)

	;print,'invdem=',invdem
	nel=size(Ax,/n_el)
	;print,"nel=",nel
	;print,"size(xp)=",size(xp,/n_el)
	Nr=fltarr(nel,3)
	Nth=fltarr(nel,3)
	Nph=fltarr(nel,3)
	
	for i=0, nel-1 do begin
		Nr[i,*]=[xp[i],yp[i],zp[i]]*invdemR[i]
		Nph[i,*]=[0,-zp[i],yp[i]]*invdemP[i]
		Nth[i,*]=[yp[i]^2+zp[i]^2,yp[i]*xp[i],zp[i]*xp[i]]*invdemT[i]
	endfor
	Ar=DotProduct(A,Nr)
	Aphi=DotProduct(A,Nph)
	Ath=DotProduct(A,Nth)
	;PRINT(max(Aphi))
	SphA=[[Ar],[Ath],[Aphi]]
		
	return, SphA

end
