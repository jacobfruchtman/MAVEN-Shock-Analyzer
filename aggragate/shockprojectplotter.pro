pro shockprojectplotter,xplt,yplt,domedian=domedian

	R_mars=3389.5

	xadj=0.0
 

	if not keyword_set(domedian) then domedian=0
	corrText="_WithoutPathologies"
	dire="Documents/Plots/CombinedPlots/"
	get_data,xplt,data=datx
	get_data,yplt,data=daty
	
	seasonColors=list("green","red","orange","blue")
	seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	

	;pos1=[0.10,0.22,0.9,0.9]
	;cpos1=[.10,.09,.9,.17]
	dopolar= keyword_set(polar)
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


	if xfnm eq 'Xmso' then adj=.600*R_mars

	R=SQRT((X+xadj)^2+Y^2)
	TH=atan(Y,(X+xadj))
	binsize=2.*!pi/180

		
		get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)
					seasonsubIndices=list(springIndices,summerIndices,autumnIndices,winterIndices)
					plotFName="projection_"+yfnm+"_vs_"+xfnm+corrText
					plotlist=list()
					Title="Seasonal Shock Crossing Projection";subT+YT+" vs "+XT
		

					

					for KK=0,3 do begin
						xsub=X[seasonsubIndices[KK]]
						ysub=Y[seasonsubIndices[KK]]
						rsub=R[seasonsubIndices[KK]]
						thsub=TH[seasonsubIndices[KK]]
						plotlist.add,scatterplot(xsub,ysub,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color=seasonColors[KK],Name=seasonNames[KK],/overplot)
						


						
						if domedian ne 0 then begin

							maxval=max(thsub)
							minval=min([min(thsub),0])
							print,maxval,minval
							mlow = (findgen((maxval-minval)/binsize+1.)+minval/binsize)*binsize
							print,"min(mlow),max(mlow)=",min(mlow),max(mlow)
							mhigh = mlow + binsize
							nmed=numel(mlow)
							print,nmed
							rmedovershoot = fltarr(nmed)

							for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
			 					 w = where(thsub ge mlow[zzz] and thsub lt mhigh[zzz],nw)
  								if nw ge 2 then rmedovershoot[zzz] = median(/even,( rsub )[w])

							endfor
							nonzero=where(rmedovershoot ne 0.0,zzz)
							print,"zzz=",zzz
							mlow2=mlow[nonzero]
							mhigh2=mhigh[nonzero]
							rmedovershoot2=rmedovershoot[nonzero]
							xmed=rmedovershoot2 *cos((mlow2+mhigh2)/2.0);-adj
							ymed=rmedovershoot2 *sin((mlow2+mhigh2)/2.0)
							if zzz gt 0 then plotlist.add,plot(title=Title,xmed, ymed,color=seasonColors[KK],name="Median"+seasonNames0[KK],/overplot)
							
						endif


					endfor
					ccf = LEGEND(target=plotlist, font_size=7,transparency=50);[p10,p11,p12,p13])

					plotlist[-1].title=Title
					marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
					tmars=text(/data,0,100,"Mars",align=.5,color=[240,231,231])
					fname=dire+plotFname
					if domedian ne 0 then fname+='_median'
					tmars.save,fname+".eps",RESOLUTION=600
					plotlist[0].close
end
