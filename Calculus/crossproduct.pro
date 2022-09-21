function crossproduct, v1,v2
	;;print,"v1,v2"
	;;print,v1,v2
	
	px=v1[2-1]*v2[3-1]-v1[3-1]*v2[2-1]
	py=v1[3-1]*v2[1-1]-v1[1-1]*v2[3-1]
	pz=v1[1-1]*v2[2-1]-v1[2-1]*v2[1-1]
	;;print,"px,py,pz=",px,py,pz
	product=[[px],[py],[pz]]
	;;print,product
	return, product
END
