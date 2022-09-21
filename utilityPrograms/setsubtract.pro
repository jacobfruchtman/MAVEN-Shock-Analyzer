function setsubtract,arr1,arr2

	union=[arr1,arr2]
	union=union[UNIQ(union, SORT(union))]
	intr=intersect(arr1,arr2)
	N=N_elements(union)
	whrnot=list()

	for i=0,N-1 do begin
		if total(intr eq union[i]) eq 0 then whrnot.add,union[i]
	endfor

	if N_elements(whrnot) eq 0 then return,-1 else return,whrnot.toarray()
end
