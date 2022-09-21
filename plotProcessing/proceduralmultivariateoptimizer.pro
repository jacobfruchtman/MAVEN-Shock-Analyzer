pro proceduralmultivariateoptimizer,plotlist,weightlist=weightlist
	numDats=numel(plotlist)
	if not keyword_set(weigthlist) then weightlist=fltarr(numel(plotlist))+1.0
	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=dat0
	N=numel(dat0.x)
	allys=fltarr(N,numDats)
	
	
	zz=allys ; this will hold the score each point has been assigned

	peakWeightMult=2.0



	for i=0,numDats-1 do begin
		el=plotlist[i]
		
		get_data,el,data=dat
		;help,datin
		;help,datout
		allys[*,i]=dat.y
	endfor
	z=fltarr(N)

	for i=0,N-1 do begin
		z[i] = Total(allys[i,*]*weightlist)
	endfor
	store_data,"weightings",data={x:dat0.x,y:z,yt:"weigthed values"}

	return

	maxwidth=80
	for i=1, maxwidth do begin
		print,"i=",i
		for j=0, N-1 do begin
			startj=max([0,j-i])
			endj=min([N-1,j+1])
			startdur=j-startj
			enddur=endj-j
			for k=0,numDats-1 do begin
				;if (min(allys[startj:j,k],minloc) lt allys[j,k]) and (max(allys[startj:j,k],maxloc) gt allys[j,k]) then zz[j,k]=.1*((startdur-maxloc)+(minloc))* weightlist[k]
				if (min(allys[startj:j,k]) eq allys[j,k]) and (max(allys[startj:j,k]) eq allys[j,k]) then zz[j,k]=.4*((startdur)+(enddur)) * weightlist[k]
				;if (max(allys[startj:j,k],minloc) lt allys[j,k]) and (min(allys[startj:j,k],maxloc) gt allys[j,k]) then zz[j,k]=.1*((startdur-maxloc)+(minloc))* weightlist[k] 
				if (max(allys[startj:j,k]) eq allys[j,k]) and (min(allys[startj:j,k]) eq allys[j,k]) then zz[j,k]=.4*((startdur)+(enddur)) * weightlist[k]

				if (max(allys[startj:j,k]) eq allys[j,k]) and (max(allys[startj:j,k]) eq allys[j,k]) then zz[j,k]=1.5*((startdur)+(enddur))* weightlist[k]
				if (min(allys[startj:j,k]) eq allys[j,k]) and (min(allys[startj:j,k]) eq allys[j,k]) then zz[j,k]=1.5*((startdur)+(enddur))* weightlist[k]
			endfor

		endfor
	endfor

	Zsummed=fltarr(N)


	for i=0, N-1 do Zsummed[i]=Total(zz[i,*])

	store_data,"weightings",data={x:dat0.x,y:Zsummed,yt:"weigthed values"}
end
