pro topcorrelators,plt=plt
	if not keyword_set(plt) then plt=45

	get_data,plt,data=datA
	print,plt
	A=datA.y
	xs=datA.x
	plotlist=list()
	corrlist=list()
	indcorrelate=list()
	
	Abads=[0,1,2,3,4,5,7,9,11,34,45,46,47,48,51,52,53,54,55,56,57,$
58,70,73,77,78,82,86,88,90,91,98,104,105,106,107,108,109,$
110,111,112,113,114,115,116,117,118,119,120,121,122,123,$
124,125,126,127,128,129,130,131,132,133,134,135,136,137,$
138,141,161,162,163,164,165,166,167,168]
	vec3s=[77,78,90,91,82]
	for i=6,168 do begin
		if plt eq 'overshootAmplitude' then if total( i eq Abads) then continue
		if i ge 104 and i le 138 then continue
		if i ge 161 and i le 169 then continue
		if  total( i eq vec3s) then continue 
		ii=i
		r=plotcorrelate(plt,ii)
		corrlist.add,r[0]
		plotlist.add,ii
		indcorrelate.add,i
	endfor
	corrlist=corrlist.toarray()
	plotlist=plotlist.toarray()
	b=sort(-1*abs(corrlist))
	corrlist=corrlist[b]
	plotlist=plotlist[b]
	indcorrelate=indcorrelate[b]
	for i=0,numel(plotlist)-1 do print,indcorrelate[i],plotlist[i],", r=",corrlist[i]
end
