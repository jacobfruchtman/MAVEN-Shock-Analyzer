;+
;CHANGELOG: 2022-01-13
; z[dhalf:bi]=period -> z[dim:bi,0]=period   ;;; this is for inbound,outbound stuff
;			z[dhalf:bi,1]=period ;;;This is standard case
;  	    2022-01-13
;		OR NOT
;-
pro cyclotronperiodsaver, dim,aj,bi,B,SNR,iSNR,z,bottom,midsw,N_e,Ndiff,Nfdiff,B2mode,B3mode,B4mode,Bmodalmean,Bmodalstd,Bmodalstdn,snrThresh=snrThresh,isnrThresh=isnrThresh


	;help,z

	dthird=mean([min([dim+10,mean([aj,dim])]),aj,aj])
	dhalf=midsw;mean([dim+10,aj])

	if dhalf lt 120 then dhalf=dim
	if dthird lt 120 then dthird=dim
	bottom=footbottomer(dthird,aj,bi,B,SNR,iSNR,N_e,Ndiff,Nfdiff,snrThresh=snrThresh,isnrThresh=isnrThresh)

	period=calculatelocalcyclotronperiod(bottom,SNR,B,dhalf)
	prd=round(period)
	strt=max([dhalf-4*prd,0])
	ofst=dhalf-strt
	print,"[dhalf-4*prd,0]=",[dhalf-4*prd,0]
	print,"dhalf-strt=",dhalf-strt
	print,"[dim,dhalf,dthird,bottom,aj,bi]=",[dim,dhalf,dthird,bottom,aj,bi]
	B2m=smooth(B[strt:bi],2*period)
	B3m=smooth(B[strt:bi],3*period)
	B4m=smooth(B[strt:bi],4*period)

	while 1 do begin

	if bottom -dhalf lt ofst or bottom-strt le prd/2. then bottom =-1
;	help,B
;	help,SNR
;	help,bottom
;	help,dhalf
;	help,dthird
	if bottom eq -1 then return
	

	
	Bstd=fltarr(numel(B2m))
	Bmm=fltarr(numel(B2m))
	;print,numel(Bmm),numel(B2m)
	Bfluc=Bmm
	for j=0,numel(B2m)-1 do begin

		Bmm[j]=mean([B2m[j],B3m[j],B4m[j]])
		Bstd[j]=stddev([B2m[j],B3m[j],B4m[j]])
		Bfluc[j]=Bstd[j]/Bmm[j]
	endfor
	Bfluc=smooth(Bfluc,period)
	;;bottom-strt=bottom-dhalf+ofst
	;bd=mean(B[.5 *bi+.5*aj:bi])
	cyclothresh=.006
	print,"[dim,strt,dhalf,bottom,aj,bi,numel(B2m),numel(Bfluc)]=",[dim,strt,dhalf,bottom,aj,bi,numel(B2m),numel(Bfluc)]
	print,"ofst,bottom,strt,bottom-(strt),numel(Bfluc)=",ofst,bottom,strt,bottom-(strt),numel(Bfluc)
	while min(Bfluc[ofst:bottom-(strt)]) ge cyclothresh do cyclothresh+=.002


	if Bfluc[bottom-(strt)] lt cyclothresh or bottom le dim+2*period  or finite(N_e[bottom-60]) ne 1 then begin
		;print,ofst,ofst+dhalf,bi-1-strt,numel(B2m)
		for j=ofst,numel(B2m)-1 do begin
			B2mode[j+strt]=B2m[j]
			B3mode[j+strt]=B2m[j]
			B4mode[j+strt]=B2m[j]
			Bmodalmean[j+strt]=Bmm[j]
			Bmodalstd[j+strt]=Bstd[j]
			Bmodalstdn[j+strt]=Bfluc[j]
		endfor
		break
	endif

		GG=where(Bfluc[ofst:bottom-strt] lt cyclothresh)
		bottom=GG[-1]+dhalf
	period=calculatelocalcyclotronperiod(bottom,SNR,B,dhalf)
	prd=round(period)
	strt=max([dhalf-4*prd,0])
	ofst=dhalf-strt
	print,"[dim,dhalf,dthird,bottom,aj,bi]=",[dim,dhalf,dthird,bottom,aj,bi]
	B2m=smooth(B[strt:bi],2*period)
	B3m=smooth(B[strt:bi],3*period)
	B4m=smooth(B[strt:bi],4*period)

	endwhile
	;print,dhalf,dthird,bi
	;z[dim:bi]=period;dhalf:bi]=period
	z[dhalf:bi]=period
end
