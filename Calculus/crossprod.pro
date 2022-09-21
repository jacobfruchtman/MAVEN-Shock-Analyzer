function crossprod, vA,vB
	;;print,"v1,v2"
	;;print,v1,v2
	v1=vA
	v2=vB
	if N_elements(vA) eq 3 and N_elements(vB) eq 3 then begin
		pp=0.0*v1
		pp[0]=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
		pp[1]=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
		pp[2]=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
		return,pp
	endif  
	if N_elements(vA) eq 3 then begin
			if (size(v2,/dim))[-1] ne 3 then v2=transpose(v2)
			;v1=0.0*v2
			N=(size(v2,/dim))[0]
			pp=fltarr(N,3)
			for i=0,N-1 do begin
				pp[i,0]=v1[2-1]*v2[i,3-1]-v1[3-1]*v2[i,2-1]
				pp[i,1]=v1[3-1]*v2[i,1-1]-v1[1-1]*v2[i,3-1]
				pp[i,2]=v1[1-1]*v2[i,2-1]-v1[2-1]*v2[i,1-1]
			endfor
			if (size(vB,/dim))[-1] ne (size(v2,/dim))[-1] then pp=transpose(pp)
			return,pp
	endif 
	 if N_elements(vB) eq 3 then begin
				if (size(v1,/dim))[-1] ne 3 then v1=transpose(v1)
			;v2=0.0*v1
				N=(size(v1,/dim))[0]
				pp=fltarr(N,3)
				for i=0,N-1 do begin
					pp[i,0]=v1[i,2-1]*v2[3-1]-v1[i,3-1]*v2[2-1]
					pp[i,1]=v1[i,3-1]*v2[1-1]-v1[i,1-1]*v2[3-1]
					pp[i,2]=v1[i,1-1]*v2[2-1]-v1[i,2-1]*v2[1-1]
				endfor
				if (size(vA,/dim))[-1] ne (size(v1,/dim))[-1] then pp=transpose(pp)
				return,pp
	endif
		   
	
	if (size(v1,/dim))[-1] ne 3 then v1=transpose(v1)
	if (size(v2,/dim))[-1] ne 3 then v2=transpose(v2)
	pp=0.0*v1
	pp[*,0]=v1[*,2-1]*v2[*,3-1]-v1[*,3-1]*v2[*,2-1]
	pp[*,1]=v1[*,3-1]*v2[*,1-1]-v1[*,1-1]*v2[*,3-1]
	pp[*,2]=v1[*,1-1]*v2[*,2-1]-v1[*,2-1]*v2[*,1-1]
	;;print,"px,py,pz=",px,py,pz
	;product=[*,[*,px],[*,py],[*,pz]]
	;;print,product
	return, pp;roduct
END
