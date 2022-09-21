pro curvefitcomparer, newName=newName, curveType=curveType , doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom,minM0=minM0,fitnum=fitnum

	if not keyword_set(newName) then begin
		if keyword_set(fitnum) then newName='B_fitted'+strtrim(fitnum,2) else newName="B_fittedtest"
	endif

	curveFitter5,"mvn_B_1sec_Mag", newName=newName+"F5",doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom,minM0=minM0
	curveFitter3,"mvn_B_1sec_Mag", newName=newName+"F3",doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom,minM0=minM0
	options,newName+'-F5*','colors','r'
	options,newName+'-F3*','colors','g'


	get_data,newName+"F5",data=datF5
	get_data,newName+"F3",data=datF3

	;newDDat=datF5
	;newFDDat=datF5
	xs=datF5.x
	ytitleD="difference in !C"+ datF5.ytitle
	ytitleFD="fracdiff in "
	fldnms=['y',' inchis','inshocks','outchis','outshocks','shocks']
	ytnms=['Magnetic Field [nT]',' in chis^2','in shocks','out chi^2','out shocks','shocks']
	suffxs=['','_CHISQ_inbound','_shocks_inbound','_CHISQ_outbound','_shocks_outbound','_shocks']
	N=numel(datF5.y)
	;zdiff=fltarr(N,6)
	;zfdiff=fltarr(N,6)
	for i=0,numel(fldnms)-1 do begin
		el=fldnms[i]
		ytitleD="difference in !C"+el
		ytitleFD="fracdiff in !C"+el
		str_element,datF5,el,v5
		str_element,datF3,el,v3
		zdiff=v3-v5
		zfdiff=fracdiff(v3,v5)
		store_data,newName+suffxs[i]+"_3v5diff",data={x:xs,y:zdiff,ytitle:ytitleD}
		store_data,newName+suffxs[i]+"_3v5fdiff",data={x:xs,y:zfdiff,ytitle:ytitleFD}

	endfor
	namelistd=newName+suffxs+"_3v5diff"
	tplot,namelistd
end
