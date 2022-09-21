
pro tplotmultiplier,p1,p2, name,yt=yt,isFlag=isFlag

	get_data,p1,data=dat1

	get_data,p2,data=dat2

	x1=dat1.x
	x2=dat2.x
	y1=dat1.y
	y2=dat2.y
	z1=y1
	z2=y2
	z=0
	dim1=size(y1,/n_dim)
	dim2=size(y2,/n_dim)

	y1[where(finite(y1,/NAN))]=0.0
	y2[where(finite(y1,/NAN))]=0.0


	s1=size(x1,/n_el)
	s2=size(x2,/n_el)
	print,"s1,s2=",s1,s2
	if NOT KEYWORD_SET(yt) THEN yt=dat1.ytitle
	if NOT KEYWORD_SET(isFlag) THEN isFlag=0
	if(dim1*dim2 eq 1) then begin
		if(s1 gt s2) then begin
			y2=interpol(y2,x2,x1)
			if isFlag eq 1 then y2[where(y2 ne 0)]=1
			x2=x1
		endif else if(s1 lt s2) then begin
			y1=interpol(y1,x1,x2)
			if isFlag eq 1 then y1[where(y1 ne 0)]=1
			x1=x2
		endif	
			print,"(s1,s2)=",size(y1,/n_el),size(y2,/n_el)
		z=y1*y2

		dat1.y=z
		store_data,name,data=dat1

		options,name,'ytitle',yt
	endif else if (dim2 eq 2) and (dim1 eq 1) then begin

		z2=y2[*,0]

		if(s1 gt s2) then begin
			z2=interpol(z2,x2,x1)
			x2=x1
		endif else if(s1 lt s2) then begin
			y1=interpol(y1,x1,x2)
			x1=x2
		endif	
			print,"(s1,s2)=",size(y1,/n_el),size(z2,/n_el)
		z=y1*z2
		dat1.y=z
		store_data,name,data=dat1

		options,name,'ytitle',yt
	endif
end
