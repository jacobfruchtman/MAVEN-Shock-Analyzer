pro curveFitter7

	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB	
	dobug=0
	ys=datB.y
	xs=datB.x
	zout=xs*0.0
	zin=zout

	wid=30
	inwid=10
	nzout=xs*0.0
	nzin=zout
	N=numel(xs)
	get_data,'ascend_end_interpolated',data=datae
	get_data,'ascend_begin_interpolated',data=datab
	get_data,'descend_end_interpolated',data=datde
	get_data,'descend_begin_interpolated',data=datdb

	get_data,'B_stddev',data=datSTD
	std=datSTD.y
	get_data,'B_median',data=datMed 
	med=datMed.y    
		yae=datae.y
		yab=datab.y
		yde=datde.y
		ydb=datdb.y

		DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
	
		


		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD
	;------------------

		;'variables that would otherwise be defined in loops'

		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		;trust=0
		g0=0;
		b0=0
		;DD = WHERE(oyae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		

		;AA = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)
		
		;counterinutitively, will want to curve fit ys[0:DD[0]] after the rest

		;need to determine if we have the whole curve or not

		;iscut=1*(ddn ne aan) ; 
		;bcutoff=1*(DD[0] le AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,'a offsetf=',aoffsetf
;		print,'d offsetf=',doffsetf
		;print, 'bcutoff: ',bcutoff
		;print, 'fcutoff: ',fcutoff

		


		;if dobug gt 1 then print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0

		lastiMax=0



	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME START!

	inimaxs=xs*0.0-1
	outimaxs=xs*0.0-1

	inimins=xs*0.0-1
	outimins=xs*0.0-1

	inchis=xs*0.0+0
	outchis=xs*0.0+0

	innums=xs*0.0-1
	outnums=xs*0.0-1
	
	inshocklocs=xs*0.0-1
	outshocklocs=xs*0.0-1
	

	inups=xs*0.0
	indowns=xs*0.0

	outups=xs*0.0
	outdowns=xs*0.0






	;;;;;;;;;;;KLUDGE IMPLEMENTATION TIME END!








fullIter=min([ddn,aan,bbn])


	;if DD[0] eq -1 then goto, NODIM

for i=1 ,fullIter-1 do begin
	j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetf
	l=i-1*(CC[0] gt BB[0])
	aj=AA[j]
	bi=BB[i]
	dim=DD[k]
	cl=CC[l]
	;cln=CC[l+1]
	print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	;if dobug gt 1 then print,'[CC[',l,'],DD[',k,'],AA[',j,',BB[',i,']]=',[cl,dim,aj,bi]
	
endfor


;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
if dobug gt 0 then begin 
print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
print,newName,'		inbound side			',newName
print,'=3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3==3='
endif
		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,fullIter-1  do begin
			;if dobug gt 0 then print,'-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-J-'
			 print,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'

			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,'i=',i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins

			bi=BB[i]; the address of where the fit will end and the next fit begins
			bim=BB[i-1]; the address where last fit ended
			dloc=i-1+doffsetf
			if AA[j] lt DD[dloc] then dloc--  
			dim=DD[dloc]
			if dim eq -1 then continue
			

			if dobug gt 1 then print,'numel(hill)=',numel(hill),', bi=',bi
			aj=AA[j] ; the index of the start trigger 

			mnbtrust=mean(ys[dim+10:dim+(aj-dim)/5])

			for k=dim,bi do begin
				st=max([dim,k-wid])
				ed=min([bi,k+wid])

				midst=round(mean([st,max([0,k-inwid])]))
				mided=round(mean([ed,min([N-1,k+inwid])]))
				;print,"[st,midst]=",[st,midst]
				;print,"[ed,mided]=",[ed,mided]
				;print,"numel(med)=",numel(med)
				if midst ne st then frnt=med[min([st,midst]):max([st,midst])] else frnt=[med[st]]
				if mided ne ed then bck=med[min([ed,mided]):max([ed,mided])] else bck=[med[ed]]
				

				smx=(std[k] eq max(std[st:ed])) and (std[k] gt 2) and (med[k] gt mnbtrust+2) and (( total(frnt lt mnbtrust +3)/(1.0*numel(frnt)) gt .5 ) xor ( total(bck lt mnbtrust +3)/(1.0*numel(bck)) gt .5 ))

				if smx then begin
					zin[k]=1*(mean(frnt) +2 lt mean(bck))
					zout[k]=1*(mean(frnt) gt 2+ mean(bck))
					print,k
				endif
			endfor		
			;zout[dim]=1
		endfor


nxs=xs

;nreg=reverse(reg)
nmed=reverse(med)
nstd=reverse(std)
nys=REVERSE(ys)
nyae=reverse(yae)
nyab=reverse(yab)
nyde=reverse(yde)
nydb=reverse(ydb)
;nyBS=REVERSE(yBSb);REVERSE(yBS)

;nhill2=-1* REVERSE(hill2)


;nmono=reverse(mono);bmono)
;nhill=reverse(hill)


;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]

nDD=N-1-reverse(AA)
nAA=N-1-reverse(DD)
nBB=N-1-reverse(CC)
nCC=N-1-reverse(BB)
naan=ddn
nbbn=ccn
nccn=bbn
nddn=aan
;nz=z*0.0
;nshocks=shocks*0.0
;dat.x=xs
;lastiMax=-1
;ni9shock=-1
;print,'nAA=',nAA
;print,'nBB=',nBB
;print,'nCC=',nCC
;print,'nDD=',nDD
print,"[nDD[0],nAA[0],nBB[0]]=",[nDD[0],nAA[0],nBB[0]]
doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
dboffsetb=1*(nBB[1] lt nDD[0])
daoffsetb=1*(nAA[1] lt nDD[0])
;if dobug gt 1 then print,'numel(nys)=',numel(nys)
print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
fullIter=min([nddn,naan,nbbn,nccn])
print,'fullIter=',fullIter
for i=1 ,fullIter-1 do begin
	j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	ii=i+dboffsetb
	aj=nAA[j]
	bi=nBB[ii]
	dim=nDD[k]
	print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	;if dobug gt 1 then print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	
endfor
;Return
;if nDD[0] eq -1 then goto, NONDIM
;stop
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		for i=1,fullIter-1  do begin
			if dobug gt 0 then print,'((((((((((((((((((((((((((((((((('
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			print,'i=',i
			;gi=nDD[i]; the address of where the fit will end and the next fit begins
			;gim=nDD[i-1]; the address where this fit begins
			while nBB[i] lt nAA[j] do begin
				print,'[nBB[i] , nAA[j]]=',[nBB[i] , nAA[j]]
				i++
			endwhile
			bi=nBB[i+dboffsetb]; the address of where the fit will end and the next fit begins
			bim=nBB[i-1+dboffsetb]; the address where last fit ended
			bii=bi
			dloc=i-1+doffsetb
			if nAA[j] lt nDD[dloc] then dloc--  
;			dim=nDD[dloc];+doffsetb]
			dim=nDD[dloc]
			if dim eq -1 then continue
			aj=nAA[j] ; the index of the start trigger 
			mnbtrust=mean(nys[dim+10:dim+(aj-dim)/5])

			for k=dim,bi do begin
				st=max([dim,k-wid])
				ed=min([bi,k+wid])

				midst=mean([st,max([dim,k-inwid])])
				mided=mean([ed,min([bi,k+inwid])])
				
				frnt=nmed[min([st,midst]):max([st,midst])]
				bck=nmed[min([ed,mided]):max([ed,mided])]
				

				smx=(nstd[k] eq max(nstd[st:ed])) and (nstd[k] gt 2) and (nmed[k] gt mnbtrust+2) and (( total(frnt lt mnbtrust +3)/(1.0*numel(frnt)) gt .5 ) xor ( total(bck lt mnbtrust +3)/(1.0*numel(bck)) gt .5 ))

				if smx then begin
					nzin[k]=1*(mean(frnt) +2 lt mean(bck))
					nzout[k]=1*(mean(frnt) gt 2+ mean(bck))
					print,k
				endif
			endfor		
			;zout[dim]=1
		endfor

		zin=zin+reverse(nzout)
		zout=zout+reverse(nzin)
		store_data,"zerothIn",data={x:xs,y:zin,ytitle:"flag"}
		store_data,"zerothOut",data={x:xs,y:zout,ytitle:"flag"}
end
