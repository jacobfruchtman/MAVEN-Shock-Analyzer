pro cyclotronsuperaverager,plt=plt,newName=newName

	if not keyword_set(plt) then plt="mvn_B_1sec_MAVEN_MSO_Mag"
	get_data,"proton_cyclotron_period",data=datTau
	get_data,plt,data=dat
	if not keyword_set(newName) then newName=plt+"_cyclosmoothed"

	xs=dat.x
	N=numel(xs)

	ys=dat.y

	pt=datTau.y

	maxTau=round(max(pt))
	
	mm=2*maxTau
	z=ys
	for i=0,N-1 do if pt[i] eq 0 then z[i]=!values.F_NAN
	zz=z
	FOR i=1,mm do begin

		step=maxTau-abs(i-maxTau)

		if step eq 0 then break
		print,"step=",step," out of ",maxTau
		upward=1*(i lt maxTau) 
		for j=0,N-1 do begin
			t=pt[j]
			if t le 0 then continue 
			if step gt t or (~upward and step ge t) then continue
			
			st=max([j-step/2,0])
			en=min([j+step/2,N-1])
			
			zz[j]=mean(z[st:en],/NAN)
		endfor	
		z=zz
	endfor
	;z[where(finite(z,/NAN))]=0
	dat.y=z
	store_data,newName,data=dat
	
end	
