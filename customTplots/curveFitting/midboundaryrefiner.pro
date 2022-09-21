function midboundaryrefiner,i,ys,AA,BB,CC,DD,Bmed,rhod,Bsd,extr,forecountdown,eDD,foreblock,B2,v5,neh,peh,teh
			print,"=====MIDBOUNDARY ", i,"========="
			brefines=0
			frefines=0
			;help,rhod

			bbn=numel(BB)
			aan=numel(AA)
			ddn=numel(DD)
			N=numel(Bsd)

			doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

			backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
			boffsetb=1*(BB[1] lt DD[0])
			aoffsetb=1*(AA[1] lt DD[0])
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf +aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
			;print,"i=",i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins
			while BB[i+boffsetb] lt AA[j] do i++
			bi=BB[i+boffsetb]; the address of where the fit will end and the next fit begins
			bim=BB[i-1+boffsetb]; the address where the last fit ended
			;bim=BB[i-1]; the address where this fit begins
			dloc=i-1+doffsetf
			if AA[j] lt DD[dloc] then dloc--  
			dim=DD[dloc]
			d00=dim

			aj=AA[j] ; the index of the start trigger 

			if 0 and total(finite(neh[dim:bi]) eq 1) eq 0 then begin
				;AA[j]=aj
				BB[i+boffsetb]=bi
				DD[dloc]=dim
				return,[dim,bi,aj,-1]
			endif
			;zeron=aj-dim
			;yh=ys[aj]; the y value at the start trigger
			dhalf=mean([dim,aj])  
			;print,"[dim,dhalf,aj,bi]=",[dim,dhalf,aj,bi]
			;regionalcrustalremover,dhalf,aj,bi,neh,peh,teh
			;print,"[dim,dhalf,aj,bi]=",[dim,dhalf,aj,bi]
			if aj+2*60 lt bi then boundaryshrinker, dim,aj,bi, ys,Bsd,rhod,Bmed,neh
			;print,"[dim,dhalf,aj,bi]=",[dim,dhalf,aj,bi]
			;;print,"[dim,aj,bi]=",[dim,aj,bi]
			BB[i+boffsetb]=bi
			DD[dloc]=dim
			if 0 and total(finite(neh[dim:bi]) eq 1) eq 0 then begin
				;AA[j]=aj
				
				return,[dim,bi,aj,-1]
			endif	
			;;print,"frefines,brefines=",frefines,brefines
			;help,extr

			eb=backendblock(dim,aj,bi,foreblock,B2,extr,rhod)
			aj0=aj
			aj1=foreshockpasser(dim,aj,eb,extr,forecountdown,rhod,B2,v5,newDim=newDim);bi,extr,forecountdown,rhod,newDim=newDim)
			aj=max([aj1,aj0])

			;;print,"[dim,aj,bi]=",[dim,aj,bi]
			AA[j]=aj
			eDD[dloc]=newDim
			;;print,"=====end MIDBOUNDARY ", i,"========="
			return,[dim,bi]
			

end
