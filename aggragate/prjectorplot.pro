pro prjectorplot
	R_mars=3389.5
	get_data,'X_MSO',data=datx
	get_data,'RHO_MSO',data=daty
	
	X=datx.y
	Y=daty.y
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	binsize=datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	get_data,'Mfms',data=datMfms
	get_data,'B2B1_FIT',data=datB2B1

	get_data,'overshootAmplitude',data=datA
	get_data,10,data=datTH

	get_data,'FM',data=datFM
	get_data,'MachAlfven',data=datMA

	get_data,'SolDistNorm',data=datDist
	get_data,'Velocity_N',data=datVN
	
	;;; [J   ,  A  ]
	;;; [Mfms,  M_A ]
	;;; [FM  ,  Dist]
	;;; [VN,   TH_BN]


	;;; Shock Jump
	zttl=datB2B1.ytitle[0]
	ZT=datB2B1.YN[0]
	B2B1=datB2B1.y
	p10=scatterplot(x,y,magnitude=bytscl(B2B1,min=1,max=ceil(B2B1)),$
				SYMBOL='dot', /SYM_FILLED,sym_size=.05 ,$
				/curr,layout=[2,4,1],RGB_TABLE=72)
	cb1=colorbar(target=p10,ORIENTATION=0,RGB_TABLE=72,range=[1,ceil(B2B1)],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
end
