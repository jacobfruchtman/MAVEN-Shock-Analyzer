function arrayDotProduct, a , b
	print,"a[0,*]=",a[0,*]
	return, a[*,0]*b[*,0]+a[*,1]*b[*,1]+a[*,2]*b[*,2]
end

