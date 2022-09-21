function wheretoflag, warr,N

	arr=fltarr(N)
	foreach el,warr do arr[el]=1
	return,arr
end
