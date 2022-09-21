
function isArray,x
	
	nell=size(x,/N_ELEMENTS)
	if(nell EQ 3) THEN return,0 ELSE return,1	

END


function vecDotProduct, a, b
	;print,"a=",a
	;print,"b=",b
	return, a[0]*b[0]+a[1]*b[1]+a[2]*b[2]
end

function arrayDotProduct, a , b
	;print,"a[0,*]=",a[0,*]
	return, a[*,0]*b[*,0]+a[*,1]*b[*,1]+a[*,2]*b[*,2]
end

function dotproduct, v1,v2
	;product=0
	nel1=size(v1,/N_ELEMENTS)
	;print,nel1
	if nel1 gt 3 THEN product= arrayDotProduct(v1,v2) ELSE product=vecDotProduct(v1,v2)

	return, product
End

