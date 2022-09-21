pro UpDownstreamIndexer

		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout

		get_data,'wind_cleanmean_interpolated',data=datFlag

		flg=datFlag.y

		sins=datin.y
		souts=datout.y
		xs=datin.y


		get_data,"overshoot",data=datO

		over=datO.y

		get_data,"regid_cleaned_plot",data=datReg
		reg=datReg.y
		rx=datReg.x
		dr=rx[1]-rx[0]

		ustartini=xs*0.0
		uendini=xs*0.0
		dstartini=xs*0.0
		dendini=xs*0.0
		ustartino=xs*0.0
		uendino=xs*0.0
		dstartino=xs*0.0
		dendino=xs*0.0		
		
		N=numel(xs)
		
		IG=where(sins ne 0,icount)
		OG=where(souts ne 0,ocount)

		foreach i,IG do begin

			for j=0,i do begin
				if flg[i-j] ne 0 then break
			endfor
			cln=(flg[i-j]+1)/2
			
			xf=xs[j]
			
			ri=(where(abs(rx-xf) eq min(abs(rx-xf))))[0] ; want to verify we're in solar wind
			while reg[ri] ne 1 do ri--;if we aren't we will shift backwards until we are
			
			rxf=rx[ri]; this is the time value of corresponding to index ri
			
			bi=(where(abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)

			if cln then begin ; if the solar wind is actually quiet, then just record the index of that location

				ustartini[i]=bi
				uendini[i]=bi
		
			endif else begin ; if it isn't quiet we'll take a 1 minute interval
				ustartini[i]=bi-29
				uendini[i]=bi+30
			endelse

			
			for j=i+1,N-61 do begin
				if (over[j] eq 0) and (over[j-1] ne 0)  then break
			endfor

			dstartini[i]=j
			dendini[i]=j+60

	
		endforeach


		foreach i,OG do begin

			for j=i,N-1 do begin
				if flg[j] ne 0 then break
			endfor
			cln=(flg[j]+1)/2
			
			xf=xs[j]
			
			ri=(where(abs(rx-xf) eq min(abs(rx-xf))))[0] ; want to verify we're in solar wind
			while reg[ri] ne 1 do ri++;if we aren't we will shift backwards until we are
			
			rxf=rx[ri]; this is the time value of corresponding to index ri
			
			bi=(where(abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)

			if cln then begin ; if the solar wind is actually quiet, then just record the index of that location

				ustartino[i]=bi
				uendino[i]=bi
		
			endif else begin ; if it isn't quiet we'll take a 1 minute interval
				ustartino[i]=bi-30
				uendino[i]=min([bi+29,N-1])
			endelse

		endforeach


end
