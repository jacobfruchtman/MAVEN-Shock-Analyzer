pro solarWindAnalyzer,mnthresh=mnthresh

		if not keyword_set(mnthresh) then mnthresh=1.25

		get_data,'ascend_end',data=datae
		get_data,'descend_begin',data=datdb

		yae=datae.y;Where spacecraft enters solar wind 
		ydb=datdb.y;where spacecraft leaves solar wind

		get_data,'regid_cleaned_plot',data=datReg
	
		ox=datae.x ; this is regid sized. We'll do things at this scale then store analyzed data of both regid scale and B scale

		reg=datReg.y
		N=size(reg,/n_el)
		AA=where(yae ne 0,acount)
		DD=where(ydb ne 0,dcount)
		meanwind=fltarr(N)
		stdwind=fltarr(N);yae*0.0

		cleanmean=fltarr(N)

		if AA[0] gt DD[0] then begin

			meen=mean(reg[0:DD[0]])
			std=stddev(reg[0:DD[0]])
			print,"meen,std=", meen,std
			for k=0,DD[0] do begin
				meanwind[k]=meen
				stdwind[k]=std
				if meen lt mnthresh then cleanmean[k]=1
			endfor		

		endif
				

		foreach i, AA do begin
			
			for j=i,N-1 do begin
				if (ydb[j] ne 0) then break
			endfor 
			j--
			meen=mean(reg[i:j])
			std=stddev(reg[i:j])
			;print,"i,j,meen,std,meanwind[i]=",i,j,meen,std,meanwind[i]
			for k=i,j do begin
				meanwind[k]=meen
				
				stdwind[k]=std
				if meen lt mnthresh then cleanmean[k]=1 else cleanmean[k]=-1
			endfor			
			;print,i,meanwind[i]
			
		endforeach
		;foreach el, AA do print, meanwind[el]
		;print,meanwind[AA[1]:AA[1]+2]
		store_data,"wind_mean",data={x:ox,y:meanwind,ytitle:"mean wind regid"}
		store_data,"wind_stddev",data={x:ox,y:stdwind,ytitle:"stddev wind regid"}
		store_data,"wind_cleanmean",data={x:ox,y:cleanmean,ytitle:"Exp[i*pi*(meanwind<"+strtrim(string(mnthresh),2)+")]"}


		get_data,'ascend_end_interpolated',data=dataei
		get_data,'descend_begin_interpolated',data=datdbi

		yaei=dataei.y;Where spacecraft enters solar wind 
		ydbi=datdbi.y;where spacecraft leaves solar wind
		NN=size(yaei,/n_el)
				nx=dataei.x
		icleanmean=fltarr(NN)
		imeanwind=fltarr(NN)
		istdwind=fltarr(NN)
		NaN=!VALUES.D_NAN
		meanwind[where(meanwind eq 0)] =NaN
		stdwind[where(stdwind eq 0)] =NaN
		cleanmean[where(cleanmean eq 0)] =NaN
		
		imeanwind=interpol(meanwind,ox,nx)
		istdwind=interpol(stdwind,ox,nx)
		icleanmean=interpol(cleanmean,ox,nx)
		
		imeanwind[where(finite(imeanwind,/NAN))]=0
		istdwind[where(finite(istdwind,/NAN))]=0
		icleanmean[where(finite(icleanmean,/NAN))]=0

		IA=where(yaei ne 0,acount)
		ID=where(ydbi ne 0,dcount)
		if IA[0] lt ID[0] then begin

			for k=0,IA[0]-1 do begin
				imeanwind[k]=0
				istdwind[k]=0
				icleanmean[k]=0
			endfor	
		endif


		foreach i, ID do begin
			
			for j=i,NN-1 do begin
				if (yaei[j] ne 0) then break
			endfor 
			j=j-2
			;print,"i,j,N-1=",i,j,N-1
			
			for k=i,j do begin
				imeanwind[k]=0
				istdwind[k]=0
				icleanmean[k]=0
			endfor	
	
		endforeach
		;print,imeanwind[IA[1]:IA[1]+2]
		store_data,"wind_mean_interpolated",data={x:nx,y:imeanwind,ytitle:"mean wind regid"}
		store_data,"wind_stddev_interpolated",data={x:nx,y:istdwind,ytitle:"stddev wind regid"}
		store_data,"wind_cleanmean_interpolated",data={x:nx,y:icleanmean,ytitle:"Exp[i*pi*(meanwind<"+strtrim(string(mnthresh),2)+")]"}


end
