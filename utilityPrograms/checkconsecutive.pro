function checkconsecutive,array
	arl=shift(array,-1)
	arr=shift(array,1)
	return, 1*((array and arl) or (array and arr))
end
