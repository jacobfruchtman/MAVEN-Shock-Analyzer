pro inmedplot,pltx,plty,ybinsize=ybinsize,xbinsize=xbinsize
	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.15]
	get_data,pltx,data=datx
	get_data,plty,data=daty
dire="Documents/Plots/CombinedPlots/"
	xvals=datx.y
	yvals=daty.y
	t=datx.x
	binsize=datx.binsize[0]
	if not keyword_set(ybinsize) then ybinsize=daty.binsize[0]
	if not keyword_set(xbinsize) then xbinsize=binsize
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]
	Title=YT+" vs "+XT
	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	plotFName="inmed_"+yfnm+"_vs_"+xfnm+".png"
	movemedFName="inmed_"+yfnm+"_vs_"+xfnm+".eps"
	N=n_elements(t)
	b=sort(xvals)
	xvals=xvals[b]
	yvals=yvals[b]
	t=t[b]
	;medliner,xvals,yvals,binsize,mlow,mhigh,med,errorbars,/std
	maxval=max(xvals)
	mlow0 = findgen(maxval/binsize+1)*binsize
	mhigh0 = mlow0 + binsize
	med0 = fltarr(maxval/binsize+1)
	ymaxval=max(yvals)
	ymlow0 = findgen(ymaxval/ybinsize+1)*ybinsize
	ymhigh0 = ymlow0 + ybinsize
	yhists = fltarr(N);fltarr(ymaxval/ybinsize+1)
errorbars0= fltarr(maxval/binsize+1)
	mlow0n = fltarr(N);findgen(maxval/binsize+1)*binsize
	mhigh0n = fltarr(N);mlow0 + binsize
	med0n = fltarr(N);fltarr(maxval/binsize+1)
	errorbars0n=fltarr(N)
	histn=fltarr(N)
	men=1

	;movingmedian,xvals,yvals,binsize,xmed,ymed,xmed2,ybin=ybinsize,std=std,ster=ster,yhist=yhist,quartiles=quartiles
	yhist=movinghist(xvals,yvals,xbinsize,ybinsize)
	p1=scatterplot(xvals,yvals,SYMBOL='.',sym_size=.5,magnitude=bytscl(yhist,min=0.),RGB_TABLE=25,pos=pos1,xtitle=xttl,ytitle=yttl)
	cb=colorbar(pos=cpos1,rgb_table=25,range=[0,max(yhist)],title="num crossings within ellipse $\sqrt{ (x-x[i])^2/xbin^2 (y-y[i])^2/ybin^2  }$,xbin="+strtrim(xbinsize,2)+", ybin="+strtrim(ybinsize,2))
	cb.save,dire+movemedFName
	cb.close
	return

	for zzz = 0,numel(mlow0)-1 do begin;15 do begin
					
 					 w = where(xvals ge mlow0[zzz] and xvals lt mhigh0[zzz],nw)

					if nw gt 0 then begin
							yw=yvals[w]
						ymaxvalL=max(yvals[w])
						ymlow0L = findgen(ymaxvalL/ybinsize+1)*ybinsize
						ymhigh0L = ymlow0L + ybinsize
						;ymed0L = fltarr(ymaxvalL/ybinsize+1)
						print,"total(yw ge ymhigh0L[-1])=",total(yw ge ymhigh0L[-1])
						if total(yw ge ymhigh0L[-1]) gt 0 then return
						for jjj = 0,numel(ymlow0L)-1 do begin
							w2 = where(yw ge ymlow0L[jjj] and yw lt ymhigh0L[jjj],nw2)
							if nw2 gt 0 then begin
								help,yhists
								help,yhists[w]
								help,w2
								help,nw2
								(yhists[w[w2]])=nw2
							endif
						endfor
					endif

  					if nw ge 2 then begin
							yw=yvals[w]
						if not keyword_set(men) then avg = median(/even,yw) else avg = mean(yw) 
						med0[zzz]=avg
						errorbars0[zzz]=stddev(yw)
						med0n[w]=avg
						errorbars0n[w]=stddev(yw)
						pdf = HISTOGRAM(yvals[w], LOCATIONS=xbin,binsize=ybinsize)

						print,"numel(pdf)=",numel(pdf)
						print,numel(xbin)
						print,numel(w)
					endif
	endfor
	nonzero=where(med0 ne 0.0,zzz,complement=wzero)
	mlow=mlow0[nonzero]
	mhigh=mhigh0[nonzero]
	med=med0[nonzero]
	errorbars=errorbars0[nonzero]
	nonzero=where(med0n ne 0.0,zzz)
	mlown=mlow0n[nonzero]
	mhighn=mhigh0n[nonzero]
	medn=med0n[nonzero]
	xmed=(mlow+mhigh)/2.
	ymed=interpol(med,xmed,xvals)
	yadj=(yvals-ymed)/ymed
	errorbarsn=errorbars0n
	print,total(finite(errorbarsn))
	print,total( finite(errorbarsn,/nan))
	print,total(errorbarsn eq 0)
	;return
	;errorbarsn[wzero]=
	histn=fltarr(N)
	for zzz = 0,numel(mlow0)-1 do begin;15 do begin
	
 					 w = where(xvals ge mlow0[zzz] and xvals lt mhigh0[zzz],nw)

					if nw gt 0 then begin
							yw=yadj[w]
						ymaxvalL=max(yadj[w])
						yminvalL=min(yadj[w])
						print,"median(yw,/even)=",median(yw,/even)

						;rm=floor(med/bin)*bin
						;ymlow0L = findgen(ymaxvalL/ybinsize+1)*ybinsize
						ymlow0L = (findgen((ymaxvalL-yminvalL)/ybinsize+1))*ybinsize+yminvalL
						nlow=numel(ymlow0L)
						ymhigh0L = ymlow0L + ybinsize
						;ymed0L = fltarr(nlow)
						print,"total(yw ge ymhigh0L[-1])=",total(yw ge ymhigh0L[-1])
						print,"total(yw lt ymlow0L[0])=",total(yw lt ymlow0L[0])
						if total(yw ge ymhigh0L[-1]) gt 0 then return
						if total(yw lt ymlow0L[0]) gt 0 then return
						for jjj = 0,numel(ymlow0L)-1 do begin
							w2 = where(yw ge ymlow0L[jjj] and yw lt ymhigh0L[jjj],nw2)
							if nw2 gt 0 then begin

								(yhists[w[w2]])=nw2
							endif
						endfor
					endif

	endfor
	n2=numel(med)
	
	ysig=errorbarsn;fltarr(N);interpol(errorbars,xmed,xvals)
	yadj=(yvals-ymed)
	
	;ysig=sqrt(ymed)
	
	ylow=ymed-ysig
	yhigh=ymed+ysig

	yadj=(yvals-ymed)/ymed
	adjlow=-ysig/ymed
	adjhigh=ysig/ymed
	;p5=plot(xvals,ylow,color='red',thick=1)
	;p6=scatterplot(xvals,yvals,sym_size=.5,sym_color='black',/over)
	;return
	;p3=plot(xvals,[[ylow],[yhigh]],xtitle=xttl,ytitle=yttl,title=Title,fill_color='sky_blue',/FILL_BACKGROUND)
	p2=plot(xvals,ylow,color='red',thick=1,pos=pos1)
	p2=plot(xvals,yhigh,color='green',thick=1,/over)
	p1=scatterplot(xvals,yvals,/over,sym_size=.5,magnitude=bytscl(yhists,min=0.),RGB_TABLE=25);sym_color='black')
	p2=plot(xvals,ymed,color='blue',thick=1,/over)
	cb=colorbar(pos=cpos1,rgb_table=25,range=[0,max(yhists)])
	;return
	;p4=plot(xvals,[[adjlow],[adjhigh]],xtitle=xttl,ytitle='(y-median)/median',fill_color='sky_blue',layout=[1,2,2],/current,/FILL_BACKGROUND)

	p1=scatterplot(xvals,yadj,sym_size=.5,sym_color='black')
	p2=plot(xvals,adjlow,color='red',thick=1,/over)
	p2=plot(xvals,adjhigh,color='green',thick=1,/over)
	p2=plot(xvals,fltarr(N),color='blue',thick=1,/over)

end
