pro quickflagger,back=back
	prfx=''
	if keyword_set(back) then prfx='b'

	get_data,prfx+'AA_flag',data=datAA
	get_data,prfx+'BB_flag',data=datBB
	get_data,prfx+'DD_flag',data=datDD
	get_data,prfx+'DD_effective_flag',data=dateDD
	get_data,'foot_start',data=datFOOT
	if keyword_set(back) then get_data,'foot_start_outbound',data=datFOOTSIDE else get_data,'foot_start_inbound',data=datFOOTSIDE

	AA=datAA.y
	BB=datBB.y

	DD=datDD.y
	FOOT=datFOOT.y
	FOOTS=datFOOTSIDE.y
	xs=datAA.x
	x0=xs[0]
	print,"assign DD"
	ctime,t
	xd=t[0]-x0
	datDD.y[xd]=1
	print,fltarr(50)
	;ctime,t
	xd=t[1]-x0
	dateDD.y[xd]=1
	print,fltarr(50)
	print,"assign AA"
	;ctime,t
	xa=t[2]-x0
	datAA.y[xa]=1
print,fltarr(50)
	print,"assign BB"
	;ctime,t
	xb=t[3]-x0
	datBB.y[xb]=1
print,fltarr(50)
	print,"assign foot"
	;ctime,t
	xf=t[4]-x0
	datFOOT.y[xf]=1
	datFOOTSIDE.y[xf]=1

	store_data,prfx+'AA_flag',data=datAA
	store_data,prfx+'BB_flag',data=datBB
	store_data,prfx+'DD_flag',data=datDD
	store_data,prfx+'DD_effective_flag',data=dateDD
	store_data,'foot_start',data=datFOOT
	if keyword_set(back) then store_data,'foot_start_outbound',data=datFOOTSIDE else store_data,'foot_start_inbound',data=datFOOTSTART

	tplot
end

