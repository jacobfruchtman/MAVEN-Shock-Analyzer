function devcrossproduct, v1,v2
	;;print,"v1,v2"
	;;print,v1,v2
	
	pp=0.0*v1
	pp[*,0]=v1[*,2-1]*v2[*,3-1]-v1[*,3-1]*v2[*,2-1]
	pp[*,1]=v1[*,3-1]*v2[*,1-1]-v1[*,1-1]*v2[*,3-1]
	pp[*,2]=v1[*,1-1]*v2[*,2-1]-v1[*,2-1]*v2[*,1-1]
	;;print,"px,py,pz=",px,py,pz
	;product=[*,[*,px],[*,py],[*,pz]]
	;;print,product
	return, pp;roduct
END
