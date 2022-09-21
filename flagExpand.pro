pro flagExpand0

	get_data,'mvn_B_1sec_Mag',data=dat

	xs=dat.x
	ys=dat.y

	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	mids=fltarr(N)
	shocks=fltarr(N)
	z=fltarr(N)

		get_data,'ascend_end',data=datae
		get_data,'descend_end',data=datde

		get_data,'ascend_begin',data=datab
		get_data,'descend_begin',data=datdb

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon
		;get_data,'regid_cleaned_plot_interpolated',data=datReg
		;reg=datReg.y

		oyae=datae.y
		oxae=datae.x

		oydb=datdb.y
		oxdb=datdb.x
		;starti=0
		oyde=datde.y
		oxde=datde.x

		oyab=datab.y
		oxab=datab.x


		ydb=fltarr(N)
		yde=fltarr(N)
		yab=fltarr(N)
		yae=fltarr(N)
		;for i=0, N-1 do yae[i]=0
		;for i=0, N-1 do yde[i]=0
		;for i=0, N-1 do yab[i]=0
		;for i=0, N-1 do ydb[i]=0


nl=size(oyae,/n_el)
		nd=size(oydb,/n_el)
		;'variables that would otherwise be defined in loops'
		;print,N,nl,nd

		trust=0
		



		DF = WHERE(oyae ne 0, ddn, COMPLEMENT=D_C, NCOMPLEMENT=gcount_c)
		

		AF = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)


		CF=WHERE(oyab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c)

		BF=WHERE(oyde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c)




	;	print,'Size(DF)=',size(DF)
	;	print,'Size(AF)=',size(AF)

		;print,'oxae[DF]-oxae[0]=',oxae[DD]-oxdb[0]
		;print,'oxdb[AF]-oxdb[0]=',oxdb[AA]-oxdb[0]
		dx=oxae[DF]
		ax=oxdb[AF]
		cx=oxab[CF]
		bx=oxde[BF]	

		;dxa=dx-oxae[0]
		;axa=ax-oxdb[0]
		;cxa=cx-oxab[0]
		;bxa=bx-oxde[0]

		;xsa=xs-xs[0]

		;for i=0, N-2 do begin
		;		for j=0, ddn-1 do begin
		;			if ((dxa[j] le xsa[i]+xsa[1]) and (dxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
		;				a=(xsa[i+1]+xsa[i])/2.0; print,'i=',i,', j=',j,', dxa[j]=',dxa[j],', xsa[i]=',xsa[i],', xsa[i+1]=',xsa[i+1],', a=',a
						
		;				yae[i+1]=1*(dxa[j] gt a)
		;				yae[i]=1*(dxa[j] le a)
		;			endif
		;		endfor
		;endfor
		for j=0,ddn-1 do begin
			mn=min(abs(xs-dx[j]),mnloc)
			yae[mnloc]=1

		endfor

		for j=0,aan-1 do begin
			mn=min(abs(xs-ax[j]),mnloc)
			ydb[mnloc]=1

		endfor

		for j=0,ccn-1 do begin
			mn=min(abs(xs-cx[j]),mnloc)
			yab[mnloc]=1

		endfor

		for j=0,bbn-1 do begin
			mn=min(abs(xs-bx[j]),mnloc)
			yde[mnloc]=1

		endfor

		;for i=0, N-2 do begin
				;for j=0, aan-1 do begin
					;if ((axa[j] le xsa[i]+xsa[1]) and (axa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						;a=(xsa[i+1]+xsa[i])/2.0;print,'i=',i,', j=',j,', axa[j]=',axa[j],', xsa[i]=',xsa[i],', xsa[i+1]=',xsa[i+1],', a=',a;print,(axa[j] gt a),', ', (axa[j] le a)
						;ydb[i+1]=1.0*(axa[j] gt a)
						;ydb[i]=1.0*(axa[j] le a);print,'ydb[i]=',ydb[i],', ydb[i+1]=',ydb[i+1]
					;endif
				;endfor
		;endfor

		;for i=0, N-2 do begin
				;for j=0, ccn-1 do begin
					;if ((cxa[j] le xsa[i]+xsa[1]) and (cxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						;a=(xsa[i+1]+xsa[i])/2.0;print,'i=',i,', j=',j,', cxa[j]=',cxa[j],', xsa[i]=',xsa[i],', xsa[i+1]=',xsa[i+1],', a=',a;print,(cxa[j] gt a),', ', (axa[j] le a)
						;yab[i+1]=1.0*(cxa[j] gt a)
						;yab[i]=1.0*(cxa[j] le a);print,'yab[i]=',yab[i],', yab[i+1]=',yab[i+1]
					;endif
				;endfor
		;endfor

		;for i=0, N-2 do begin
				;for j=0, bbn-1 do begin
					;if ((bxa[j] le xsa[i]+xsa[1]) and (bxa[j] ge xsa[i+1]-xsa[1])) then begin
						;yae[i]=1
						;a=(xsa[i+1]+xsa[i])/2.0;print,'i=',i,', j=',j,', axa[j]=',cxa[j],', xsa[i]=',xsa[i],', xsa[i+1]=',xsa[i+1],', a=',a;print,(axa[j] gt a),', ', (axa[j] le a)
						;yde[i+1]=1.0*(bxa[j] gt a)
						;yde[i]=1.0*(bxa[j] le a);print,'yde[i]=',yde[i],', yde[i+1]=',yde[i+1]
					;endif
				;endfor
		;endfor

		store_data,'ascend_end_interpolated',data={x:xs,y:yae,ytitle:'yae'}
		store_data,'ascend_begin_interpolated',data={x:xs,y:yab,ytitle:'yab'}
		store_data,'descend_end_interpolated',data={x:xs,y:yde,ytitle:'yde'}
		store_data,'descend_begin_interpolated',data={x:xs,y:ydb,ytitle:'ydb'}
	
end
