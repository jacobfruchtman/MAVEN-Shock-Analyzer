pro shockIntervelator,final=final,refined=refined
	dobug=0
	if keyword_set(refined) then begin

		get_data,'ascend_end_refined',data=datae
		get_data,'ascend_begin_refined',data=datab
		get_data,'descend_end_refined',data=datde
		get_data,'descend_begin_refined',data=datdb

		yae=datae.y
		yab=datab.y
		yde=datde.y
		ydb=datdb.y
		N=numel(ydb)
		DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		print,yae[DD]

		AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
		eDD=DD
		
		nDD=N-1-reverse(AA)

		enDD=nDD
		nAA=N-1-reverse(DD)
		nBB=N-1-reverse(CC)
		nCC=N-1-reverse(BB)

		nddn=aan
		naan=ddn
		nbbn=ccn
		nccn=bbn
		newName="shockInterval"
	endif

	if keyword_set(final) then begin
		get_data,'AA_flag',data=datAA
		get_data,'bAA_flag',data=datbAA
		get_data,'BB_flag',data=datBB
		get_data,'bBB_flag',data=datbBB
		get_data,'CC_flag',data=datCC
		get_data,'bCC_flag',data=datbCC
		get_data,'DD_flag',data=datDD
		get_data,'DD_effective_flag',data=dateDD
		get_data,'bDD_flag',data=datbDD
		get_data,'bDD_effective_flag',data=datebDD
		
	;	
		DD = WHERE(datDD.y ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]
		eDD = WHERE(dateDD.y ne 0, eddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		AA = WHERE(datAA.y ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(datCC.y ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(datBB.y ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock


		nDD = WHERE(REVERSE(datbAA.y) ne 0, nddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]
		enDD = WHERE(REVERSE(datebDD.y) ne 0, enddn, COMPLEMENT=nG_C, NCOMPLEMENT=ngcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		nAA = WHERE(REVERSE(datbDD.y) ne 0, naan, COMPLEMENT=nH_C, NCOMPLEMENT=nhcount_c); beginnging of shock 

		nCC = WHERE(REVERSE(datbBB.y) ne 0, nccn, COMPLEMENT=nK_C, NCOMPLEMENT=nkcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		nBB = WHERE(REVERSE(datbCC.y) ne 0, nbbn, COMPLEMENT=nL_C, NCOMPLEMENT=nlcount_c); end of  negative  shock
		newName="shockInterval"

	endif

		doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?

	fullIter=min([ddn,aan,bbn])


	;if DD[0] eq -1 then goto, NODIM


	forwardlist=list()
	backwardlist=list()
	if (not doffsetf) and (not aoffsetf) then forwardlist.add,[0,0,AA[0],BB[0]]
	IF doffsetf and (not aoffsetf) then forwardlist.add,[DD[0],eDD[0],AA[0],BB[0]]
	for i=1 ,fullIter-1 do begin
		j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
		k=i-1+doffsetf
		;l=i-1*(CC[0] gt BB[0])
		aj=AA[j]
		bi=BB[i]
		dim=DD[k]
		edim=eDD[k]
		;cl=CC[l]
		;cln=CC[l+1]
		print,'DD[',k,'],AA[',j,',BB[',i,']]=',[dim,edim,aj,bi]
		if dobug gt 1 then print,'[DD[',k,'],AA[',j,',BB[',i,']]=',[dim,edim,aj,bi]
		forwardlist.add,[dim,edim,aj,bi]
	endfor

	;forwardlist=forwardlist.toarray()
	if numel(forwardlist) gt 0 then forwardlist=forwardlist.toarray() else forwardlist=-1
	print,forwardlist



	doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
	aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
if numel(nBB) gt 1 then dboffsetb=1*(nBB[1] lt nDD[0]) else  dboffset=0
if numel(nAA) gt 1 then daoffsetb=1*(nAA[1] lt nDD[0]) else daoffsetb=0
	if dobug gt 1 then print,'numel(nys)=',numel(nys)
	if dobug gt 1 then print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
	fullIter=min([nddn,naan,nbbn])
	print,'fullIter=',fullIter


	if (not doffsetb) and (not aoffsetb) then backwardlist.add,[0,0,nAA[0],nBB[0]]
	IF doffsetb and (not aoffsetb) then backwardlist.add,[nDD[0],enDD[0],nAA[0],nBB[0]]
	for i=1 ,fullIter-1 do begin
		j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
		k=i-1+doffsetb
		ii=i+dboffsetb
		aj=nAA[j]
		bi=nBB[ii]
		dim=nDD[k]
		edim=enDD[k]
		print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,edim,aj,bi]
		if dobug gt 1 then print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,edim,aj,bi]
		backwardlist.add,[dim,edim,aj,bi]	
endfor
	if numel(backwardlist) gt 0 then backwardlist=backwardlist.toarray() else backwardlist=-1
	print,backwardlist

	store_data,newName,data={forwardlist:forwardlist,backwardlist:backwardlist}
end
