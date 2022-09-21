function third_roll,hill,dim,aj,bi,dobug,foot
	dt=1000
	aaj=aj
	N=numel(hill)
	maxloc=-1
	newDim=Max([aaj-dt,foot]);dim])
	newBi=Min([aaj+dt,bi])
	if dobug gt 0  then print,"dim,foot,newDim,aaj,newBi,bi,N=",dim,foot,newDim,aaj,newBi,bi,N
	print,"dim,foot,newDim,aaj,newBi,bi,N=",[dim,foot,newDim,aaj,newBi,bi,N]
	mx=max(hill[newDim:newBi],maxloc)
	lastaj=-1
	while (mx ne hill[aaj]) do begin
		;print,aaj
		if (abs(hill[maxloc+newDim]-hill[newBi]) lt .0004) and (hill[newBi] gt hill[newDim]) and (maxloc+newDim gt aaj) and maxloc+newDim gt foot then maxloc=newBi-newDim
		if (abs(hill[newDim]-hill[maxloc+newDim]) lt .0004) and (hill[newBi] lt hill[newDim]) and (maxloc+newDim lt aaj) and newDim gt foot then maxloc=0
		
		;if dobug gt 0  then print,hill[Max([aaj-dt,dim]):Min([aaj+dt,bi])]
		if dobug gt 0  then print,"aaj,maxloc+newDim,hill[aaj],hill[maxloc+newDim]=",aaj,maxloc+newDim,hill[aaj],hill[maxloc+newDim]
		aaj=maxloc+newDim
		newDim=Max([aaj-dt,foot]);dim])
		newBi=Min([aaj+dt,bi])
		mx=max(hill[newDim:newBi],maxloc)
		if aaj eq lastaj then return,aaj
		if dobug gt 0 then print,"dim,newDim,newBi,bi,N=",dim,newDim,newBi,bi,N
		lastaj=aaj
	endwhile

	return,aaj

end
