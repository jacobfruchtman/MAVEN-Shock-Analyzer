pro singleaggplot,xplt,yplt,subset=subset,keepopen=keepopen,clr=clr,lclr=lclr,format=format,fname=fname,doleg=doleg,legpos=legpos,xlog=xlog,ylog=ylog,binsize=binsize
	dire="Documents/Plots/CombinedPlots/"
	get_data,xplt,data=datx
	get_data,yplt,data=daty
	if not keyword_set(format) then format='.png'
	if not keyword_set(lclr) then lclr='black'
	if not keyword_set(clr) then lclr='dark_slate_grey'
	X=datx.y
	Y=daty.y
	XN=datx.YN[0]
	YN=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	if not keyword_set(binsize) then binsize=datx.binsize[0]
	;binsize=datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	subflag=fltarr(N)+1
	sub=findgen(N)

	terrs=''
	if keyword_set(err) then terrs='_with_error'

	plrfn=''
	if keyword_set(subset) then begin
		get_data,subset,data=datsub
		subflag=datsub.y
		sub=where(subflag eq 1)		
		subT=datsub.YN
		subfn=datsub.fn
		

		;foreach el, sub do begin
			;print,time_string(t[el])+", ["+xfnm+' , '+yfnm+']=',[X[el],Y[el]]

		;endforeach
	endif
	if not keyword_set(fname) then fname=subfn+yfnm+"_vs_"+xfnm+terrs+format



	Ysub=Y[sub]
	Xsub=X[sub]
p1=scatterplot(Xsub,Ysub,sym_color=clr,$
		ytitle=yttl,xtitle=xttl,$
				SYMBOL='dot', /SYM_FILLED,sym_size=.5 ,$
				name=YN,/over,xlog=xlog,ylog=ylog)
if keyword_set(xlog) then begin
							if xlog eq 1 then Xsub=alog10(Xsub) else Xsub=alog(xsub)

						endif

						ysub=Y[sub]
						if keyword_set(ylog) then begin
							if ylog eq 1 then ysub=alog10(ysub) else ysub=alog(ysub)

						endif						


	medliner,Xsub,Ysub,binsize,mlow,mhigh,ymed,errorbars
	xmed=(mlow+mhigh)/2.0
	if keyword_set(xlog) then begin
							if xlog eq 1 then xmed=10.^xmed else xmed=exp(xmed)

						endif
						if keyword_set(ylog) then begin
							;if ylog eq 1 then menovershoot2=10.^menovershoot2 else menovershoot2=exp(menovershoot2)
							if ylog eq 1 then ymed=10.^ymed else medovershoot2=exp(ymed)
						endif
	medplot=plot(xmed, ymed,color=lclr,name=YN+"Median line",/overplot)
	if keyword_set(doleg) or keyword_set(legpos) then begin 
	if keyword_set(legpos) then 	ccf = LEGEND( font_size=4,transparency=50,/relative,VERTICAL_SPACING=.005,VERTICAL_ALIGNMENT='bottom',pos=legpos) else $
		ccf = LEGEND(font_size=4,transparency=50,VERTICAL_SPACING=.005,/relative,VERTICAL_ALIGNMENT='bottom',pos=[1,1])
		ccf.save,dire+fname,RESOLUTION=600
	endif else medplot.save,dire+fname,RESOLUTION=600
	if ~keyword_set(keepopen) then p1.close
end
