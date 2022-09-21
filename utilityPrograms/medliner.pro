pro medliner,xvals,yvals,binsize,mlow,mhigh,medovershoot,errorbars,serr=serr,men=men,std=std

				maxval=max(xvals)
				minval=min(xvals)
				
				mlow0 = findgen((maxval-minval)/binsize+1)*binsize+minval
				Nlow=numel(mlow0)
				mhigh0 = mlow0 + binsize
				medovershoot0 = fltarr(Nlow)
				if  keyword_set(serr) or keyword_set(std)  then errorbars0= fltarr(Nlow) else  errorbars0= fltarr(2,Nlow)
				;help,errorbars0
				for zzz = 0,numel(mlow0)-1 do begin;15 do begin
	
 					 w = where(xvals ge mlow0[zzz] and xvals lt mhigh0[zzz],nw)
  					if nw ge 2 then begin
							yw=yvals[w]
							;help,yw
							;help,errorbars0
						if not keyword_set(men) then avg = median(/even,yw) else avg = mean(yw) 
						medovershoot0[zzz]=avg
						if not keyword_set(serr) and not keyword_set(std) then begin
							yws=yw[sort(yw)]
							NN=n_elements(yws)
							errorbars0[0,zzz]=avg-yws[.25*(NN-1)]
							errorbars0[1,zzz]=yws[.75*(NN-1)]-avg
						endif else if keyword_set(serr) then errorbars0[zzz]= standarderror(yw) else errorbars0[zzz]=stddev(yw)
					endif
				endfor
				nonzero=where(medovershoot0 ne 0.0,zzz)
				mlow=mlow0[nonzero]
				mhigh=mhigh0[nonzero]
				medovershoot=medovershoot0[nonzero]
				if  size(errorbars0,/n_dim) eq 1 then errorbars=errorbars0[nonzero] else begin
					errorbars=fltarr(2,zzz)
					for j=0,zzz-1 do errorbars[*,j]=errorbars0[*,nonzero[j]]
				endelse
				;help,errorbars
end


