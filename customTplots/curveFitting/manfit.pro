pro manfit,plt=plt,name=name
	
	if not keyword_set(plt) then plt='mvn_B_1sec_MAVEN_MSO_Mag'
	
	get_data,plt,data=dat
	if not keyword_set(name) then name=plt+'_manfit'
	get_data,'st_flags',data=datst
	get_data,'en_flags',data=daten
	get_data,'BbutterDeriv_enterflags',data=datA
	get_data,'BbutterDeriv_leaveflags',data=datLV
	xs=dat.x
	ys=dat.y
	sts=where(datst.y ne 0)
	ens=where(daten.y ne 0)
	as=where(datA.y ne 0)
	av=(datA.y)[as]
	LVs=where(datLV.y ne 0)
	N=n_elements(ys)
	zin=fltarr(N)
	zout=zin
	inshocks=fltarr(N)
	outshocks=fltarr(N)
	inchis=zin
	outchis=zout

	numcross=n_elements(sts)
	print,sts

	print,''
	print,ens
	print,''

	for i=0,numcross-1 do begin

		st=sts[i]
		A=as[i]
		en=ens[i]
		LV=LVs[i]

		if av[i] lt 0 then continue
		print,'i=',i
		print,time_String(xs[A])
		;if A lt LV then en=min([en,LV])
		yp=ys[st:en]
		xp=xs[st:en]
		xpa=xp-xp[0]
		mn=mean(ys[st-20:st+20])
		mx=mean(ys[en-20:en+20])
		m2=A-st
		m1=.1
		m0=(mx-mn)/2
		m3=(mx+mn)/2
		MM=[m0,m1,m2,m3]
		weight=1.0/yp
		CHISQ=-1
		
		yfit=curvefit(itmax=40,xpa, yp, weight, MM, SIGMA, FUNCTION_NAME='tanhfit',status=status,CHISQ=CHISQ)
		ishock=MM[2]+st
		
		en0=en

		if A lt LV and LV-A ge 2.5*60 then en=min([en,LV])
		yp2=ys[st:en]
		xp2=xs[st:en]
		xpa2=xp2-xp2[0]
		mn=mean(ys[st-20:st+20])
		mx=mean(ys[en-20:en+20])
		m2=A-st
		m1=.1
		m0=(mx-mn)/2
		m3=(mx+mn)/2
		MM2=[m0,m1,m2,m3]
		weight2=1.0/yp2
		CHISQ2=-1
		yfit2=curvefit(itmax=40,xpa2, yp2, weight2, MM2, SIGMA, FUNCTION_NAME='tanhfit',status=status2,CHISQ=CHISQ2)
		ishock2=MM2[2]+st

		if status2 eq 0 and CHISQ2 lt CHISQ and CHISQ2 lt 1.3 and ishock2 gt st and ishock2 lt en-2*60 and en-ishock gt !pi/MM2[1] then begin
			status=status2
			yfit=yfit2
			CHISQ=CHISQ2
			ishock=ishock2
			MM=MM2

		endif
	
		if status eq 0 and CHISQ lt 1.3 and ishock gt st and ishock lt en then begin
				inshocks[ishock]=1
				for j=st,en do begin
					zin[j]=yfit[j-st]

				endfor
				inchis[ishock]=CHISQ
		endif
	endfor

	for i=0,numcross-1 do begin

		st=sts[i]
		A=as[i]
		en=ens[i]
		LV=LVs[i]

		if av[i] gt 0 then continue
		print,'i=',i
		print,xs[A]
		;if A gt LV then st=max([st,LV])
		yp=ys[st:en]
		nyp=reverse(yp)
		xp=xs[st:en]
		xpa=xp-xp[0]
		mx=mean(ys[st-20:st+20])
		mn=mean(ys[en-20:en+20])
		m2=en-A
		m1=.1
		m0=(mx-mn)/2
		m3=(mx+mn)/2
		MM=[m0,m1,m2,m3]
		weight=1.0/nyp
		CHISQ=-1
		
		nyfit=curvefit(itmax=40,xpa, nyp, weight, MM, SIGMA, FUNCTION_NAME='tanhfit',status=status,CHISQ=CHISQ)
		yfit=reverse(nyfit)
		ishock=en-MM[2]
		if status eq 0 and CHISQ lt 1.3 and ishock gt st and ishock lt en then begin
				outshocks[ishock]=1
				for j=st,en do begin
					zout[j]=yfit[j-st]

				endfor
				outchis[ishock]=CHISQ
		endif
	endfor

	store_data,name+'_inbound',data={x:dat.x,y:zin,ytitle:name+'!C inbound fit'}
	store_data,name+'_CHISQ_inbound',data={x:dat.x,y:inchis,ytitle:name+'!C inbound CHISQ'}
	store_data,name+'_outbound',data={x:dat.x,y:zout,ytitle:name+'!C outbound fit'}
	store_data,name+'_CHISQ_outbound',data={x:dat.x,y:outchis,ytitle:name+'!C outbound CHISQ'}
	z=zin+zout
	store_data,name,data={x:dat.x,y:z,ytitle:name+'!C inbound fit'}
	store_data,name+'_CHISQ',data={x:dat.x,y:inchis+outchis,ytitle:name+'!C CHISQ'}
	store_data,name+'_shocks',data={x:dat.x,y:inshocks+outshocks,ytitle:name+'!C shocks'}
	options,name,'colors','b'
	store_data,'bmanf',data=plt+' '+name
end
