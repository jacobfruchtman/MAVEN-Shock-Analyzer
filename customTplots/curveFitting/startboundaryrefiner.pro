function startboundaryrefiner,ys,AA,BB,CC,DD,Bmed,rhod,Bsd,extr,forecountdown,reg,eDD,foreblock,B2,B20,v5,neh,peh,teh
		print,"=====STARTBOUNDARY========="
		bi=BB[0]
		d0=DD[0]
		;dim=0
		if bi eq -1 then bi=numel(reg)-1
		if d0 lt bi then dim=d0 else dim=0 

		;xp=xs[dim:b0]


		;'start with standard case'
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?
		
		if(aoffsetf eq 0) then begin

			i=0

			aj=AA[0]

			regp=reg[dim:bi]
			d00=dim
			;print,'mean(zn)=',mean(zn),',  max(zn)=',max(zn)
			;if (min(regp) lt 2) then begin
				;while (regp[0] gt 1) do begin
					;if dim -1 eq aj then break
					;dim++
					;regp=regp[1:*]
					;yp=yp[1:*]
				;endwhile
			;endif
			dhalf=mean([dim,aj])  
			;print,"dim,aj,bi=",[dim,aj,bi]
			;regionalcrustalremover,dhalf,aj,bi,neh,peh,teh
			boundaryshrinker, dim,aj,bi, ys,Bsd,rhod,Bmed,neh
			eb=backendblock(dim,aj,bi,foreblock,B2,extr,rhod)
			;aj=foreshockpasser(dim,aj,eb,extr,forecountdown,rhod,B2,v5,newDim=newDim)	;bi,extr,forecountdown,rhod,newDim=newDim)	
			aj0=aj
			aj1=foreshockpasser(dim,aj,eb,extr,forecountdown,rhod,B2,v5,newDim=newDim);bi,extr,forecountdown,rhod,newDim=newDim)
			aj=max([aj1,aj0])
			BB[0]=bi
			AA[0]=aj
			if d00 eq DD[0] then begin 
				DD[0]=dim
				eDD[0]=newDim
				
			endif
			print,"=====END STARTBOUNDARY========="
			return,[dim,bi]
		endif else begin
			aj=0
			CRUSTST=where((Bsd[aj+1:bi] gt 1.0) and (rhod[aj+1:bi] lt 0),ccount)+aj+1
			if ccount gt 0  then begin
				bi=(CRUSTST[where(CRUSTST gt 7*60)])[0]
			endif

			eb=backendblock(0,0,bi,foreblock,B2,extr,rhod)
			aj=foreshockpasser(0,0,eb,extr,forecountdown,rhod,B2,v5,newDim=newDim)

			if aj ne 0 then AA=[aj,AA]

			;forecountdown[dim:bi]=1
			print,"=====END STARTBOUNDARY========="
			return,[dim,bi]
		endelse
end
