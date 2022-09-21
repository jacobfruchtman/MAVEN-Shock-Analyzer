;+
;CHANGELOG: 2022-01-13
; z[dhalf:bi]=period -> z[dim:bi,0]=period   ;;; this is for inbound,outbound stuff
;			z[dhalf:bi,1]=period ;;;This is standard case
;  	    2022-01-13
;		OR NOT
;-
pro cyclotronperiodsaver, dim,aj,bi,B,SNR,iSNR,z,bottom,midsw,N_e,Ndiff,Nfdiff,snrThresh=snrThresh,isnrThresh=isnrThresh


	help,z

	dthird=mean([min([dim+10,mean([aj,dim])]),aj,aj])
	dhalf=midsw;mean([dim+10,aj])
	bottom=footbottomer(dthird,aj,bi,B,SNR,iSNR,N_e,Ndiff,Nfdiff,snrThresh=snrThresh,isnrThresh=isnrThresh)
;	help,B
;	help,SNR
;	help,bottom
;	help,dhalf
;	help,dthird
	if bottom eq -1 then return
	
	period=calculatelocalcyclotronperiod(bottom,SNR,B,dhalf)
	print,"[dim,dhalf,dthird,bottom,aj,bi]=",[dim,dhalf,dthird,bottom,aj,bi]
	;print,dhalf,dthird,bi
	;z[dim:bi]=period;dhalf:bi]=period
	z[dhalf:bi]=period
end
