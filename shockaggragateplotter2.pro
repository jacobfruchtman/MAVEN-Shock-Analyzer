pro shockaggragateplotter2,xplt,yplt,subset=subset,magnitude=magnitude,polar=polar,$
domedian=domedian,ct=ct,ar=ar,dims=dims,magrange=magrange,err=err
	if not keyword_set(domedian) then domedian=0
	corrText="_WithoutPathologies"
	dire="Documents/Plots/CombinedPlots/"
	get_data,xplt,data=datx
	get_data,yplt,data=daty
	seasonColors=list("green","red","orange","blue")
	seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	if not keyword_set(ct) then begin 
	ct=75
 LOADCT, ct, FILE='Documents/mycolors1.tbl'
    TVLCT, red, green, blue, /GET

    rwb = [[red],[green],[blue]]
	endif else rwb = ct
	R_mars=3389.5
	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.15]
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
	subflag=fltarr(N)+1
	sub=findgen(N)

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
		R=X
		Th=Y	
	if dopolar then begin
		maxy=max(y[sub])
		R=X
		Th=Y						
	
		if daty.degree[0] eq 1 then begin
			Th=Th*!pi/180
								
		endif else if daty.radian[0] eq 0 then begin
			Th=Th*2*!pi/maxy
		endif

		X=R*cos(Th)
		Y=R*sin(Th)
		xttl="radial: "+xttl
		yttl='angular: '+yttl
		plrfn='polar_'
	endif

	
	print,"max("+xfnm+")="+strtrim(max(X[sub]),2)+" at ",time_string(t[sub[where(X[sub] eq max(X[sub]))]])
	print,"max("+yfnm+")="+strtrim(max(Y[sub]),2)+" at ",time_string(t[sub[where(Y[sub] eq max(Y[sub]))]])


				if ~ keyword_set(magnitude) then begin

					
					get_data,'season',data=datseason
					springIndices=where(datseason.y eq 0)
					summerIndices=where(datseason.y eq 1)
					autumnIndices=where(datseason.y eq 2)				
					winterIndices=where(datseason.y eq 3)
					seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
					intersect(autumnIndices,sub),intersect(winterIndices,sub))
					plotFName=plrfn+subfn+yfnm+"_vs_"+xfnm+corrText+".eps"
					plotlist=list()
					Title=subT+YT+" vs "+XT
		



					for KK=0,3 do begin
						xsub=X[seasonsubIndices[KK]]
						ysub=Y[seasonsubIndices[KK]]
						
						plotlist.add,scatterplot(xsub,ysub,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
						sym_color=seasonColors[KK],Name=seasonNames[KK],/overplot)
						


						
						if domedian ne 0 then begin
							if ~ dopolar then begin
							maxval=max(xsub)
							minval=min([min(xsub),0])
							print,maxval,minval
							mlow = (findgen((maxval-minval)/binsize+1.)+minval/binsize)*binsize
							print,"min(mlow),max(mlow)=",min(mlow),max(mlow)
							mhigh = mlow + binsize
							nmed=numel(mlow)
							print,nmed
							medovershoot = fltarr(nmed)

							for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
			 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  								if nw ge 2 then medovershoot[zzz] = median(/even,( ysub )[w])

							endfor
							nonzero=where(medovershoot ne 0.0,zzz)
							print,"zzz=",zzz
							mlow2=mlow[nonzero]
							mhigh2=mhigh[nonzero]
							medovershoot2=medovershoot[nonzero]
							if zzz gt 0 then plotlist.add,plot(title=Title,(mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames0[KK],/overplot)
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

					if  xfnm eq "time"  then plotlist[0].xtickunits="time"
					if domedian eq 5 then begin
						if ~dopolar then begin

						maxval=max(X[sub])
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						medovershoot = fltarr(maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w = where(X[sub] ge mlow[zzz] and X[sub] lt mhigh[zzz],nw)
  							if nw ge 2 then medovershoot[zzz] = median(/even,( Y[sub] )[w])
						endfor
						nonzero=where(medovershoot ne 0.0,zzz)
						mlow2=mlow[nonzero]
						mhigh2=mhigh[nonzero]
						medovershoot2=medovershoot[nonzero]
						if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot,title=Title)

		
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



					;ccf = LEGEND(target=plotlist, font_size=7,transparency=50,/relative,VERTICAL_ALIGNMENT='bottom');[p10,p11,p12,p13])
					plotlist[-1].title=Title
					fname=dire+plotFname
					if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 then begin

						marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
						tmars=text(/data,0,100,"Mars",align=.5,color=[240,231,231])
						
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
					plotFName=subfn+zfnm+"_vs_"+yfnm+"_vs_"+xfnm+corrText+".eps"
				
					
					pos11=[0.10,0.22,0.9,0.9]

					TITLE=ZT+" vs "+YT+" vs "+XT
					sctrFIT=scatterplot(Xsub,Ysub,symbol='.',/sym_filled,RGB_TABLE=rwb,xtitle=xttl,ytitle=yttl,title=TITLE,$
					POSITION=pos1,MAGNITUDE=Zbyt);,ASPECT_RATIO=1)

					IF domedian ne 0 then begin

						if ~dopolar then begin
						maxval=max(Xsub)
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						menovershooty = fltarr(maxval/binsize+1)
						medovershooty = fltarr(maxval/binsize+1)
						menovershootz = fltarr(maxval/binsize+1)						
						medovershootz = fltarr(maxval/binsize+1)

						errorbars=fltarr(maxval/binsize+1)
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
								 medplot=errorplot((mlow2+mhigh2)/2.0, menovershoot2y,errorbars2,vert_color=bytscl(menovershoot2z),name="Mean",/overplot)
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
					if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 then begin

						marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
						tmars=text(/data,0,100,"Mars",align=.5,color=[240,231,231])
						;sctrFIT.aspect_ratio=1

						;tmars.save,fname,RESOLUTION=600
					endif
					cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, $
					title=zttl,RGB_TABLE=rwb,RANGE=[Zmin,Zmax])
					if total(xfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 and total(yfnm eq ['Xmso','Ymso','Zmso','RHOmso','RHO-0mso']) eq 1 then sctrFIT.aspect_ratio=1
		;dir='Documents/Plots/'+shockDate+'/'
					fname=dire+plotFname
					cbb2b2.save,fname,RESOLUTION=600
					sctrFIT.close
					print,"max("+zfnm+") at ",time_string(t[where(Zsub eq max(Zsub))])
				endelse
				X=R
				Y=Th
	print,"max("+xfnm+")="+strtrim(max(X[sub]),2)+" at ",time_string(t[sub[where(X[sub] eq max(X[sub]))]])
	print,"max("+yfnm+")="+strtrim(max(Y[sub]),2)+" at ",time_string(t[sub[where(Y[sub] eq max(Y[sub]))]])

				Nsub=numel(sub)
				print,Nsub
				;if 0 and Nsub gt 37 then return				


		if xfnm eq 'Crit_Ratio' then begin
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
