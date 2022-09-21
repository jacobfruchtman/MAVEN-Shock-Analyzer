function instantcomponentaverage, arr,istart,iend,tbound=tbound
	if keyword_set(tbound) then begin
		istart=tbound[0]
		iend=tbound[1]

	endif

	V=fltarr(3)

	V[0]=mean(arr[istart:iend,0])
	V[1]=mean(arr[istart:iend,1])
	V[2]=mean(arr[istart:iend,2])

	return,V

end
