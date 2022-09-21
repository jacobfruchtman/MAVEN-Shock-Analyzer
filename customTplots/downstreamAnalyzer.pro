
function downFinder, bin, inbacks,sublengths,avdown

	N=numel(bin)

	GG=where(inbacks ne 0,gcount)
	;print,GG
	idsb=0.0*bin
	idse=0.0*bin

	for el=0, gcount-1 do begin
		;print,el
		i=GG[el]
		dd=avdown[i]
		for j=i,N-1 do if sublengths[j+1] eq 0 then break
		for k=0,i do if sublengths[i-k] eq 0 then break
		st=i-k
		ed=j

		for l=i, i+3*60 do if (abs(Bin[l]- dd) le 1) or ((Bin[l] le dd) and ( Bin[l+1] ge dd) ) or ((Bin[l] ge dd) and ( Bin[l+1] le dd) ) then break

		for m=l+59,l+3*60 do if (Bin[m] eq dd) or ((Bin[l] le dd) and ( Bin[l+1] ge dd) ) or ((Bin[l] ge dd) and ( Bin[l+1] le dd) ) then break

		for cnt=st, ed do begin
			idsb[cnt]=l
			idse[cnt]=m
	
		END
	endfor

	ids=[[idsb],[idse]]

	Return, ids

end


pro downstreamAnalyzer
	get_data,'sublengths_inbound_end',data=datSubin
	get_data,'sublengths_outbound_end',data=datSubout

	get_data,'Franken_fitted_inbound',data=datffi
	get_data,'Franken_fitted_outbound',data=datffo
	

;	get_data,'shocks_inbound',data=datSin
;	get_data,'shocks_outbound',data=datSout
	get_data,"shock_locs_inbound",data=datSLI
	get_data,"shock_locs_outbound",data=datSLO
	get_data,'B_sheath_in',data=datBin
	get_data,'B_sheath_out',data=datBout

	get_data,"overbacks_inbound",data=datiback
	get_data,"B_mins_outbound",data=datoback

	bin=datBin.y
	bout=datBout.y

	inbacks=datiback.y
	outbacks=datoback.y

	Subin=datSubin.y
	Subout=datSubout.y


	odd=datffo.outdowns
	idd=datffi.indowns

	nodd=reverse(odd)	

	nbout=reverse(bout)
	noutbacks=reverse(outbacks)
	nsubout=reverse(Subout)
	imaxs=datffi.imaxs
	omaxs=REVERSE(datffo.imaxs)
	
	N=numel(bin)
	inflags=fltarr(N)
	outflags=inflags

	inShockLocs=datSLI.y
	outShockLocs=datSLI.y

	ids=downFinder(bin,inbacks,Subin,idd)

	GGin=where(ids[*,0] ne 0)
	GG2=UNIQ(ids[GGin,0], SORT(ids[GGin,0]))
	print,numel(GG2)
	Gdin = ids[GG2,*]

	inflags[Gdin]=1

	nods=downFinder(nbout,noutbacks,nsubout,nodd)

	GGout=where(nods[*,0] ne 0)
	GG2=UNIQ(nods[GGout,0], SORT(nods[GGout,0]))
	print,numel(GG2)
	Gdout = nods[GG2,*]

	outflags[Gdout]=1

	
	ods=reverse(nods)	
	outflags=reverse(outflags)
	for i=0, numel(ods)-1 do if ods[i] ne 0 then ods[i]=N-1-ods[i]

	store_data,"downstream_indices_inbound",data={x:datBin.x,y:ids,ytitle:"index_sub"}
	store_data,"downstream_indices_outbound",data={x:datBin.x,y:ods,ytitle:"index_sub"}

	store_data,"downstream_flags_inbound",data={x:datBin.x,y:inflags}
	store_data,"downstream_flags_outbound",data={x:datBin.x,y:outflags}
	ds=ods*0.0
	for i=0, N-1 do begin
		
		outSL=outshockLocs[i]
		inSL=inshockLocs[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL)))  then begin
			ds[i,*]=ids[i,*]
			
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL)))  then begin
				ds[i,*]=ods[i,*]
			ENDIF 	
		ENDELSE

	endfor
	store_data,"downstream_indices",data={x:datBin.x,y:ds,ytitle:"index_sub"}

	store_data,"downstream_flags",data={x:datBin.x,y:inflags+outflags}	
end
