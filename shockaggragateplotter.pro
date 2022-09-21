pro shockaggragateplotter,xplt,yplt,subset=subset,magnitude=magnitude,polar=polar,$
domedian=domedian,ct=ct,ar=ar,dims=dims,magrange=magrange,err=err,format=format,ylog=ylog,xlog=xlog,$
zlog=zlog,dustdist=dustdist,solsdist=solsdist,binsize=binsize,peridist=peridist,qsplit=qsplit,qconic=qconic,xrange=xrange,yrange=yrange,vdneg=vdneg,vdcneg=vdcneg,wind=wind,men=men,tbin=tbin
	if not keyword_set(domedian) then domedian=0
	if not keyword_set(format) then format='.png'
	corrText="_WithoutPathologies"
	dire="Documents/Plots/CombinedPlots/"
	get_data,xplt,data=datx
	get_data,yplt,data=daty
	seasonColors=list("green","red","orange","blue")
	seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
	seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")
	if domedian le 0 then seasonColors2=seasonColors
	seasonNames0=list("Spring","Summer","Autumn","Winter")
	
	nparts=4
	if keyword_set(wind) then w=window(dim=wind)
	
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	if not keyword_set(ct) then ct=25;75
 	LOADCT, ct;, FILE='Documents/mycolors1.tbl'
    TVLCT, red, green, blue, /GET

    rwb = [[red],[green],[blue]]
	;endif else rwb = ct
	R_mars=3389.5
	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.15]
	dopolar= keyword_set(polar)
	X=datx.y
	Y=daty.y
	t=datx.x
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl=daty.ytitle[0]
	xttl=datx.ytitle[0]
	b=sort(X)
	;X=X[b]
	;Y=Y[b]
	;t=t[b]
	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	if not keyword_set(binsize) then binsize=datx.binsize[0]
	subfn=''
	subT=''
	logfn=''
	if keyword_set(zlog) then logfn+='_zlog'
	if keyword_set(ylog) then logfn+='_ylog'
	if keyword_set(xlog) then logfn+='_xlog'
	N=numel(datx.x)
	subflag=fltarr(N)+1
	sub=findgen(N)

	terrs=''
	if keyword_set(err) then terrs='_with_error'

	plrfn=''
	if keyword_set(subset) then begin
		get_data,subset,data=datsub
		subflag=datsub.y
		subflag=subflag;[b]
		sub=where(subflag eq 1)		
		subT=datsub.YN
		subfn=datsub.fn
		

		;foreach el, sub do begin
			;print,time_string(t[el])+", ["+xfnm+' , '+yfnm+']=',[X[el],Y[el]]

		;endforeach
	endif
	
	if keyword_set(vdneg) then begin
		get_data,'Vd_N',data=datVDN
		vneg=where(datVDN.y lt 0)
		sub=intersect(sub,vneg)
	endif
	if keyword_set(vdcneg) then begin
		get_data,'Vdconic',data=datVDC
		vneg=where(datVDC.y lt 0)
		sub=intersect(sub,vneg)
	endif	
	
	
		R=X
		Th=Y	
	if dopolar then begin
		maxy=max(y[sub])
		R=X
		Th=Y						
	
		if daty.degree[0] eq 1 and yplt ne 'lat'  then Th=Th*!pi/180 else $
		if yplt eq 'lat' then TH=((360+Th) MOD 360)*!pi/180. else $
	    if daty.radian[0] eq 0 then Th=Th*2*!pi/maxy else Th=Th
		

		X=R*cos(Th)
		Y=R*sin(Th)
		xttl="radial: "+xttl
		yttl='angular: '+yttl
		plrfn='polar_'
	endif

	
	print,"max("+xfnm+")="+strtrim(max(X[sub],xmxlc),2)+" at ",time_string(t[sub[where(X[sub] eq max(X[sub]))]]),', ',yfnm,'=',Y[sub[where(X[sub] eq max(X[sub]))]]
	print,"min("+xfnm+")="+strtrim(min(X[sub],xmnlc),2)+" at ",time_string(t[sub[xmnlc]]),', '+yfnm+'=',Y[sub[xmnlc]]
	print,"max("+yfnm+")="+strtrim(max(Y[sub],ymxlc),2)+" at ",time_string(t[sub[where(Y[sub] eq max(Y[sub]))]]),', '+xfnm+'=',X[sub[where(Y[sub] eq max(Y[sub]))]]	
	print,"min("+yfnm+")="+strtrim(min(Y[sub],ymnlc),2)+" at ",time_string(t[sub[where(Y[sub] eq min(Y[sub]))]]),', '+xfnm+'=',X[sub[where(Y[sub] eq min(Y[sub]))]]


				if ~ keyword_set(magnitude) then begin

					if keyword_set(solsdist) then begin
						get_data,"solsbins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("soldist $<45^\circ$","$45^\circ\leq$ soldist <$90^\circ$","$90^\circ\leq$ soldist $<135^\circ$","$135^\circ\leq$ soldist $<180^\circ$")
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='solsquart_'
					endif else if keyword_set(dustdist) then begin
						get_data,"dustbins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("dustdist $<45^\circ$","$45^\circ\leq$ dustdist $<90^\circ$","$90^\circ\leq$ dustdist $<135^\circ$","$135^\circ\leq$ dustdist $<180^\circ$")
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='dustquart_'
					endif else if keyword_set(tbin) then begin
						;if tbin gt 1 then begin
						if tbin le 1 then begin
						get_data,"time_quartiles",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")

						seasonNames0=list('t<2016-02-14','2016-02-14<=t<2017-05-16','2017-05-16 <=t< 2018-08-15','t>=2018-08-15')
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 1)
						summerIndices=where(season eq 2)
						autumnIndices=where(season eq 3)				
						winterIndices=where(season eq 4)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						
						endif else begin
							tbin=round(tbin)
							tinterval=time_double('2019-11-16')-time_double('2014-11-15')
							tquant=floor( (t-time_double('2014-11-15'))*tbin/tinterval)
							nparts=tbin
							seasonsubIndices=list()
							seasonColors=list()
							seasonColors2=list()
							seasonNames0=list()
							
							seasonColors3=list()
							tdt0=time_double('2014-11-15')
							for lll=0.,tbin-1 do begin
								print,"lll=",lll
								w=where(tquant eq lll)
								tdt=tinterval/tbin+tdt0
								;print,'tdt0=',tdt0
								;print,'time_string(tdt0)=',time_string(tdt0)
								;print,'tdt=',tdt
								;print,'time_string(tdt)=',time_string(tdt)
								tstr=time_string(tdt0)+'<=t<'+time_string(tdt)
								tstr=tstr.replace('/','T')
								tdt=tdt0
								seasonsubIndices.add,intersect(sub, w)
								clrRAW=transpose(rwb[255.99*(lll*1.0/(tbin)+.5/tbin  ),*])
								print,"clrRAW=",clrRAW
								;mxclr=max(clrRAW)
								COLOR_CONVERT, clrRAW[0], clrRAW[1], clrRAW[2], hh, ss, vv, /RGB_HSV
								COLOR_CONVERT, hh, 1, vv, rsat, gsat, bsat, /HSV_RGB
								COLOR_CONVERT, hh, ss, (vv+1.)/2, rlght, glght, blght, /HSV_RGB
								COLOR_CONVERT, hh, 1, (vv)/2, rdrk, gdrk, bdrk, /HSV_RGB
								clrSAT=[rsat,gsat,bsat]
								print,"clrSAT=",clrRAW
								clrLGHT=[rlght,glght,blght];clrRAW*255.99/mxclr
								print,"clrLIGHT=",clrLGHT
								clrDRK=[rdrk,gdrk,bdrk]
								print,"clrDARK=",clrDRK
								seasonColors.add,clrSAT
								seasonColors2.add,clrLGHT
								seasonColors3.add,clrDRK
								seasonNames0.add,tstr
							endfor
							
							print,seasonColors2[0]
							help,seasonColors2
							help,seasonColors
							help,seasonNames0
							seasonNames=seasonNames0
							;ppp=scatterplot(X,Y,sym_color=seasonColors2[0])
							;return
						endelse
						if domedian eq 0 then seasonColors2=seasonColors
						subT+='time binned '
						subfn+='timequartiles_'
					endif else if keyword_set(peridist) then begin
						get_data,"peribins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("perihelion dist $<45^\circ$","$45^\circ\leq$ peridist $<90^\circ$","$90^\circ\leq$ peridist $<135^\circ$","$135^\circ\leq$ peridist $<180^\circ$")
						season=(datseason.y);[b]
						seasonNames=seasonNames0
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='periquart_'
					endif else if keyword_set(qsplit) then begin
						get_data,'Quasiperp',data=datQperp
						get_data,'Quasipar',data=datQpar
						QparIndices=where(datQpar.y eq 1)
						QprpIndices=where(datQperp.y eq 1)
						seasonsubIndices=list(intersect(QprpIndices,sub),intersect(QparIndices,sub))
						seasonColors=list('lime green' ,"purple")
						seasonColors2=list('lime',"magenta")
						seasonColors3=list('yellow green',"dark magenta")
						
						
						seasonNames0=list("Quasiperp","Quasiparallel")
						seasonNames=seasonNames0
						nparts=2
						subfn+='Qsplit_'
						subT+='Q split '
					endif else if keyword_set(qconic) then begin
						get_data,'thetaBconic',data=datTHC

						QprpIndices=where(datTHC.y ge 45,nqperp,complement=QparIndices)
						seasonsubIndices=list(intersect(QprpIndices,sub),intersect(QparIndices,sub))
						seasonColors=list('lime green' ,"purple")
						seasonColors2=list('lime',"magenta")
						seasonColors3=list('yellow green',"dark magenta")
						
						
						seasonNames0=list("Quasiperp","Quasiparallel")
						seasonNames=seasonNames0
						nparts=2
						subfn+='QConicsplit_'
						subT+='Conic Q split '
					endif else begin
						get_data,'season',data=datseason
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						
					endelse
					plotFName=plrfn+subfn+yfnm+"_vs_"+xfnm+corrText+terrs+logfn+format
					;if keyword_set(err) then fname+='_with_error'
					plotlist=list()
					Title=subT+YT+" vs "+XT
		



					for KK=0,nparts-1 do begin
						print,KK,'/',nparts-1
						xsub=X[seasonsubIndices[KK]]
						ysub=Y[seasonsubIndices[KK]]
						
						plotlist.add,scatterplot(xsub,ysub,$;,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
						sym_color=seasonColors2[KK],Name=seasonNames[KK],/overplot,ylog=ylog,xlog=xlog,xrange=xrange,yrange=yrange)
						
						
						
						if domedian gt 0 then begin

						if keyword_set(xlog) then begin
							if xlog eq 1 then Xsub=alog10(Xsub) else Xsub=alog(xsub)

						endif

							if ~ dopolar then begin
							;maxval=max(xsub)
							;minval=min([min(xsub),0])
							;print,maxval,minval
							;mlow = (findgen((maxval-minval)/binsize+1.)+minval/binsize)*binsize
							;print,"min(mlow),max(mlow)=",min(mlow),max(mlow)
							;mhigh = mlow + binsize
							;nmed=numel(mlow)
							;print,nmed
							;medovershoot = fltarr(nmed)
							;menovershoot = fltarr(nmed)
							;errorbars=fltarr(nmed)
							;for zzz = 0,numel(mlow)-1 do begin;15 do begin
	;
	;		 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  	;							if nw ge 2 then begin
	;									 medovershoot[zzz] = median(/even,( ysub )[w])
  	;									 menovershoot[zzz] = mean(( ysub )[w])
	;									 errorbars[zzz] = standarderror(( Ysub )[w])
	;							endif
	;						endfor
	;						nonzero=where(medovershoot ne 0.0,zzz)
	;						print,"zzz=",zzz
	;						mlow2=mlow[nonzero]
	;						mhigh2=mhigh[nonzero]
	;						medovershoot2=medovershoot[nonzero]
	;						menovershoot2=menovershoot[nonzero]
							medliner,xsub,ysub,binsize,mlow2,mhigh2,medovershoot2,errorbars2,men=men,std=std;,serr=serr
							;errorbars2=errorbars[nonzero]
							help,medovershoot2
							help,errorbars2

							xmed=(mlow2+mhigh2)/2.0
							if keyword_set(xlog) then begin
							if xlog eq 1 then xmed=10.^xmed else xmed=exp(xmed)

							endif

							if 1 then begin;zzz gt 0 then begin
								if not keyword_set(err) then plotlist.add,plot(xmed, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames0[KK],/overplot,thick=1.5) else plotlist.add,errorplot(xmed,medovershoot2,errorbars2,color=seasonColors[KK],name="Mean"+seasonNames0[KK],/overplot,ERRORBAR_COLOR=seasonColors3[KK],ERRORBAR_THICK=.75)
							;	wBig=where(xmed ge 9)
								;help,errorbars2
							; plotlist.add,errorplot(xmed[wBig], medovershoot2[wBig],errorbars2[*,wbig],name="Median",/overplot,ERRORBAR_THICK=1,ylog=ylog,xlog=xlog,errorbar_color=seasonColors3[KK],thick=1.5,color=seasonColors[KK])
							endif
							endif else begin

								rsub=R[seasonsubIndices[KK]]
								thsub=Th[seasonsubIndices[KK]]
								maxval=max(thsub)
								mlow = findgen(maxval/binsize+1)*binsize
								mhigh = mlow + binsize
								medovershoot = fltarr(maxval/binsize+1)

							for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
			 					 w = where(thsub ge mlow[zzz] and thsub lt mhigh[zzz],nw)
  								if nw ge 2 then medovershoot[zzz] = median(/even,( rsub )[w])

							endfor
							nonzero=where(medovershoot ne 0.0,zzz)
							mlow2=mlow[nonzero]
							mhigh2=mhigh[nonzero]
							medovershoot2=medovershoot[nonzero]

							if zzz gt 0 then plotlist.add,polarplot(title=Title,(mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames0[KK],/overplot)
							

							endelse
						endif

				;		maxval=max(xvals[seasonsubIndices[KK]])
					endfor
					plotlist[0].title=title
					plotlist[0].xtitle=xttl
					
					plotlist[0].ytitle=yttl
					if  xfnm eq "Ls"  then plotlist[0].xrange=[0,360]
					if  datx.degree[0] eq 1 then begin
						mxxr=Ceil(max(X[sub])/45.)*45
						mnxr=Floor(min(X[sub])/45.)*45
						plotlist[0].xrange=[mnxr,mxxr]
					endif
					if  datx.radian[0] eq 1 then begin
						degs=X[sub]*180/!pi
						mxxr=!pi*Ceil(max(degs)/45.)*45/180.
						mnxr=!pi*Floor(min(degs)/45.)*45/180.
						plotlist[0].xrange=[mnxr,mxxr]
					endif
					if  xfnm eq "time"  then plotlist[0].xtickunits="time"
					if domedian eq 5 or domedian lt 0 then begin
						if ~dopolar then begin
						Xsub=X[sub]

						if keyword_set(xlog) then begin
							if xlog eq 1 then Xsub=alog10(Xsub) else Xsub=alog(xsub)

						endif

						ysub=Y[sub]
						if keyword_set(ylog) then begin
							if ylog eq 1 then ysub=alog10(ysub) else ysub=alog(ysub)

						endif						
						if 0 then begin
						maxval=max(Xsub)
						minval=min(Xsub)
						;mlow = findgen(maxval/binsize+1)*binsize
						mlow=findgen((maxval-minval)/binsize+1)*binsize+minval
						mhigh = mlow + binsize
						nlow=numel(mlow)
						menovershoot = fltarr(nlow)
						medovershoot = fltarr(nlow);maxval/binsize+1)
							

						errorbars = fltarr(nlow)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w = where(Xsub ge mlow[zzz] and Xsub lt mhigh[zzz],nw)
  							if nw ge 2 then begin
								medovershoot[zzz] = median(/even,( Ysub )[w])
								menovershoot[zzz] = mean(( Ysub )[w])
								errorbars[zzz] = standarderror(( Ysub )[w])
							endif
						endfor
						nonzero=where(medovershoot ne 0.0,zzz)
						mlow2=mlow[nonzero]
						mhigh2=mhigh[nonzero]
						medovershoot2=medovershoot[nonzero]
						menovershoot2=menovershoot[nonzero]
						errorbars2=errorbars[nonzero]
						endif
						medliner,xsub,ysub,binsize,mlow2,mhigh2,medovershoot2,errorbars2;,serr=serr,men=men,std=std
						medx=(mlow2+mhigh2)/2.0
						if keyword_set(xlog) then begin
							if xlog eq 1 then medx=10.^medx else medx=exp(medx)

						endif
						if keyword_set(ylog) then begin
							;if ylog eq 1 then menovershoot2=10.^menovershoot2 else menovershoot2=exp(menovershoot2)
							if ylog eq 1 then medovershoot2=10.^medovershoot2 else medovershoot2=exp(medovershoot2)
						endif

						if 1 then begin
							if not keyword_set(err) then	 plotlist.add,plot(medx, medovershoot2,color=!color.slate_grey,name="Median",/overplot,thick=1.5) else  plotlist.add,errorplot(medx, medovershoot2,errorbars2,color=!color.slate_grey,name="Mean",/overplot,title=Title,ERRORBAR_THICK=.5,ylog=ylog,xlog=xlog)
							;wBig=where(medx ge 9)
							; plotlist.add,errorplot(medx[wBig], medovershoot2[wBig],errorbars2[*,wbig],color=!color.slate_grey,name="Median",/overplot,ERRORBAR_THICK=.75,ylog=ylog,xlog=xlog)
						endif		
						endif else begin

						maxval=max(Th[sub])
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						medovershoot = fltarr(maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w = where(Th[sub] ge mlow[zzz] and Th[sub] lt mhigh[zzz],nw)
  							if nw ge 2 then medovershoot[zzz] = median(/even,( R[sub] )[w])
						endfor
						nonzero=where(medovershoot ne 0.0,zzz)
						mlow2=mlow[nonzero]
						mhigh2=mhigh[nonzero]
						medovershoot2=medovershoot[nonzero]
						if zzz gt 0 then plotlist.add,polarplot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot,title=Title)


						endelse

					endif

					if keyword_set(dims) then plotlist[0].dimensions=dims

					if keyword_set(ar) then  plotlist[0].aspect_ratio=ar



					if keyword_set(dustdist) or keyword_set(solsdist)  or keyword_set(peridist) then ccf = LEGEND(target=plotlist, font_size=7,transparency=50,/relative,VERTICAL_ALIGNMENT='bottom',pos=[.2,.7]);[p10,p11,p12,p13])
					plotlist[-1].title=Title
					fname=dire+plotFname

					;if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 then begin
					if xfnm eq 'Xmso' and yfnm eq 'RHOmso' then begin
						marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
						tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
						
						plotlist[0].aspect_ratio=1
						tmars.save,fname,RESOLUTION=600
					endif else plotlist[-1].save,fname,RESOLUTION=600

					;fname=dire+plotFname

					plotlist[0].close
					

				endif else begin
					get_data,magnitude,data=zdata

					
					
				;	zdata=alldatas[datanum]
					Z=zdata.y;[0]
					zttl=zdata.ytitle;[1];xttls[II]
					if keyword_set(zlog) then begin
						Z=alog10(Z)
						zttl='log10('+zttl+')'
					
					
					endif
		;print,max(ThetaNBn),min(ThetaNBn)
					zfnm=zdata.fn;zdata[3];fvsTitles[II]
					ZT=zdata.YN;[2];vsTitles[II];'$\theta_{bn}$'
					Ysub=Y[sub]
					Xsub=X[sub]
					Zsub=Z[sub]
					if not keyword_set(magrange) then begin
						Zbyt=bytscl(Zsub)
						Zmax=max(Zsub)
						Zmin=min(Zsub)
					endif else begin
						Zmax=max(magrange)
						Zmin=min(magrange)
						Zbyt=bytscl(Zsub,max=Zmax,min=Zmin)
					endelse
					plotFName=subfn+zfnm+"_vs_"+yfnm+"_vs_"+xfnm+corrText+terrs+logfn+format
				
					
					pos11=[0.10,0.22,0.9,0.9]

					TITLE=ZT+" vs "+YT+" vs "+XT
					sctrFIT=scatterplot(Xsub,Ysub,symbol='.',/sym_filled,RGB_TABLE=rwb,xtitle=xttl,ytitle=yttl,title=TITLE,$
					POSITION=pos1,MAGNITUDE=Zbyt,ylog=ylog,xlog=xlog,xrange=xrange,yrange=yrange);,ASPECT_RATIO=1)
					if keyword_set(ar) then  sctrFIT[0].aspect_ratio=ar
					IF domedian ne 0 then begin

						if ~dopolar then begin
						maxval=max(Xsub)
						minval=min(Xsub)
						;mlow = findgen(maxval/binsize+1)*binsize
						mlow=findgen((maxval-minval)/binsize+1)*binsize+minval
						mhigh = mlow + binsize
						nlow=numel(mlow)
						menovershooty = fltarr(nlow)
						medovershooty = fltarr(nlow);maxval/binsize+1)
						menovershootz = fltarr(nlow);maxval/binsize+1)						
						medovershootz = fltarr(nlow);maxval/binsize+1)

						errorbars=fltarr(nlow);maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w= where(Xsub ge mlow[zzz] and Xsub lt mhigh[zzz],nw)

  							if nw ge 2 then medovershooty[zzz] = median(/even,( Ysub )[w])
  							if nw ge 2 then menovershooty[zzz] = mean(( Ysub )[w])
  							if nw ge 2 then menovershootz[zzz] = mean(( Zsub )[w])

  							if nw ge 2 then medovershootz[zzz] = median(/even,( Zsub )[w])
							if nw ge 2 then errorbars[zzz] = standarderror(( Ysub )[w])
						endfor
						
						mlow2=[mlow[0],mlow[0],mlow]
						mhigh2=[mhigh[0],mhigh[0],mhigh]
						medovershoot2y=[medovershooty[0],medovershooty[0],medovershooty]
						menovershoot2y=[menovershooty[0],menovershooty[0],menovershooty]
						medovershoot2z=[Zmin,Zmax,medovershootz]
						menovershoot2z=[Zmin,Zmax,menovershootz]

						errorbars2=[errorbars[0],errorbars[0],errorbars]

						if zzz gt 0 then begin
							if not keyword_set(err) then medplot=plot((mlow2+mhigh2)/2.0, medovershoot2y,vert_color=bytscl(medovershoot2z),name="Median",/overplot) else begin
								 medplot=errorplot((mlow2+mhigh2)/2.0, menovershoot2y,errorbars2,vert_color=bytscl(menovershoot2z),name="Mean",/overplot,ERRORBAR_THICK=.5)
									;print,errorbars2
							endelse
							ccf = LEGEND(target=medplot, font_size=7,transparency=50,/relative,VERTICAL_ALIGNMENT='bottom')
						endif						
		
						endif else begin
						maxval=max(Th[sub])
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						medovershooty = fltarr(maxval/binsize+1)
						
						medovershootz = fltarr(maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w = where(Th[sub] ge mlow[zzz] and Th[sub] lt mhigh[zzz],nw)

  							if nw ge 2 then medovershooty[zzz] = median(/even,( R[sub] )[w])

  							if nw ge 2 then medovershootz[zzz] = median(/even,( Zsub )[w])
						endfor
						
						mlow2=[mlow[0],mlow[0],mlow]
						mhigh2=[mhigh[0],mhigh[0],mhigh]
						medovershoot2y=[medovershooty[0],medovershooty[0],medovershooty]
						medovershoot2z=[Zmin,Zmax,medovershootz]



						if zzz gt 0 then begin
							medplot=polarplot((mlow2+mhigh2)/2.0, medovershoot2y,vert_color=bytscl(medovershoot2z),name="Median",/overplot)
							ccf = LEGEND(target=medplot, font_size=7,transparency=50,/relative,VERTICAL_ALIGNMENT='bottom')
						endif		

						endelse
					endif
	;				if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso','RHO-0geo','Zgeo']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso','Zgeo','RHO-0geo']) eq 1 then begin
					if (xfnm eq 'Xmso' and yfnm eq 'RHOmso') or (xfnm eq 'RHO-0geo' and yfnm eq 'Zgeo') or (xfnm eq 'RHO-0mso' and yfnm eq 'Zmso') then begin
						marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
						tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
						sctrFIT.aspect_ratio=1
						;tmars.save,fname,RESOLUTION=600
					endif
					cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, $
					title=zttl,RGB_TABLE=rwb,RANGE=[Zmin,Zmax])
					if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso','RHO-0geo','Zgeo']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso','RHO-0mso','RHO-0geo','Zgeo']) eq 1 then sctrFIT.aspect_ratio=1
		;dir='Documents/Plots/'+shockDate+'/'
					fname=dire+plotFname
					cbb2b2.save,fname,RESOLUTION=600
					sctrFIT.close
					print,"0======0======0======0"
					print,"max("+zfnm+")=",max(Zsub,wzmx)," at ",time_string(t[sub[wzmx]]);where(Zsub eq max(Zsub))])
					print,xfnm+'=',Xsub[wzmx]," ,"+yfnm+"=",Ysub[wzmx]
					print,'---'
					print,"min("+zfnm+")=",min(Zsub,wzmn)," at ",time_string(t[sub[wzmn]]);where(Zsub eq max(Zsub))])
					print,xfnm+'=',Xsub[wzmn]," ,"+yfnm+"=",Ysub[wzmn]
					print,"======"
					print,"max("+xfnm+")=",max(Xsub,wxmx)," at ",time_string(t[sub[wxmx]]);where(Zsub eq max(Zsub))])
					print,yfnm+'=',Ysub[wzmx]," ,"+zfnm+"=",Zsub[wzmx]
					print,'---'
					print,"min("+xfnm+")=",min(Xsub,wxmn)," at ",time_string(t[sub[wxmn]]);where(Zsub eq max(Zsub))])
					print,yfnm+'=',Ysub[wxmn]," ,"+zfnm+"=",Zsub[wxmn]
					print,'======'
					print,"max("+yfnm+")=",max(Ysub,wymx)," at ",time_string(t[sub[wymx]]);where(Zsub eq max(Zsub))])
					print,xfnm+'=',Xsub[wymx]," ,"+zfnm+"=",Zsub[wymx]
					print,'---'
					print,"min("+yfnm+")=",min(Ysub,wymn)," at ",time_string(t[sub[wymn]]);where(Zsub eq max(Zsub))])
					print,xfnm+'=',Xsub[wymn]," ,"+zfnm+"=",Zsub[wymn]
					print,"0======0======0======0"
				endelse
				X=R
				Y=Th
	print,"max("+xfnm+")="+strtrim(max(X[sub]),2)+" at ",time_string(t[sub[where(X[sub] eq max(X[sub]))]])
	print,"max("+yfnm+")="+strtrim(max(Y[sub]),2)+" at ",time_string(t[sub[where(Y[sub] eq max(Y[sub]))]])

				Nsub=numel(sub)
				print,Nsub
				;if 0 and Nsub gt 37 then return				


		if 0 and xfnm eq 'Crit_Ratio' then begin
			print,'1========================================1'
			sub=where(X lt 1)
			foreach el, sub do begin
					print,time_string(t[el])+", ["+xfnm+' , '+yfnm+']=',[X[el],Y[el]]

			endforeach
			print,'2========================================2'
			sub=where(X lt 1.5 and Y gt 1.)

			for i=0,N-1 do begin
				if not (X[i] lt 1.5 and Y[i] gt 1.) then continue
				if Y[i] le 1.1 then continue
				print,time_string(t[i])+", ["+xfnm+' , '+yfnm+']=',[X[i],Y[i]]

			endfor


			;print,
		endif

end
