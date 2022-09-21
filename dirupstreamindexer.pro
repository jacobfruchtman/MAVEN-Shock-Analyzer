function dirupstreamindexer,xs,B,el,IG,cycloperiod,DD,eDD,imins,imaxs,N_e,downs,ups,foots,B60d,mstd,Bminute,fore,ymm,flg,reg,rx,Bp,foremax,wid

print,"~~~~~~~~~~~~"
			print,"el=",el
			i=IG[el]
			period=cycloperiod[i]

			;tt=max([60,period])
			
			tt=period*ceil(60/period)
			;while round(tt) le 60 do tt+=period

			ishock=i
			localdim=(where(DD lt i))[-1]
			print,localdim

			print,i
			print,time_string(xs[i])
			if localdim ne -1 then dim=DD[localdim] else dim=0
			;dim=bDD[el]
			localed=(where(eDD lt i))[-1]
			if localed ne -1 then ed=eDD[localed] else ed=dim
	;		ed=eDD[el]
	;		dim=DD[el]
			ishock=i
			imax=max([imaxs[el],imins[el]])
			imin=min([imaxs[el],imins[el]])
			print,time_string(xs[imin])
			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			;dim=imins[el]			
			if dim gt i then dim=0;ed
			if dim gt i then dim=imin

			;i=max([i-tt/2,ed,imin,dim])
			;if dim eq 0 then dim=N-1
;			print,"[dim,ed,imin,i,imax]=",[dim,ed,imin,i,imax]
			i-=2
			;i=(where(ffd[dim:i] lt .25))[-1]
			ii=i
			dhalf=mean([dim,ishock])
			down=downs[ishock]
			up=ups[ishock]
			wherefoot=(where(foots[imin+1:ishock-1] gt 0))[-1]
			print,"wherefoot=",wherefoot
			if wherefoot eq -1 then foot=ishock-tt/2 else 	foot=wherefoot+imin+1;foots[wherefoot]+imin+1
			;imin=max([imin,foot-10*60,i-tt/2*60])
			wd=wid*!pi;*1.1/2
			print,"[imin,imin+tt,ishock-wd,foot,ishock]=",[imin,imin+tt,ishock-wd,foot,ishock]
 			if ishock-wd gt imin+tt and ishock-wd lt foot and total(finite(N_e[imin:ishock-wid]) eq 1) ne 0 then foot=ishock-wd
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
;			if dhalf gt i or imin gt dim then dhalf= imin
			if dhalf gt ishock  then dhalf= imin
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
			;ed=max([imin,dhalf]);dhalf
			print,"[dim,ed,imin,foot,i,ishock,imax]=",[dim,ed,imin,foot,i,ishock,imax]
			while foots[ii] eq 0  and ii gt imin do begin
				if ii eq 0 then begin
					ii=i
					break
				endif
				ii--

			endwhile
			footsn=ii
			if ishock-wd gt imin+tt and ishock-wd lt footsn and imin lt ishock-wd then begin
				if  total(finite(N_e[imin:ishock-wd]) eq 1) ne 0 then begin
				footsn=ishock-wd
				if i ne ii then ii=ishock-wd
				endif
			endif
			iii=ii

			print,imin,foot,footsn
			bound=max([imin,footsn-10.*60,0]);max([dim,min([imin,footsn-5*60]),dhalf,min([ed,footsn-5*60])])


			efoot=foot
			while max(B[max([efoot-tt/2,0]):efoot]) ge down and efoot-tt gt bound do efoot--
			wherefin=where(finite(N_e[bound:efoot]) eq 1,nfin)+bound
			if nfin ge tt then begin
				bound=wherefin[0]
				 

			endif
			fupmmdev=(ymm-up)/up
			ool=onofflength(abs(fupmmdev[bound:foot]) lt .12)
			if total( ool gt 2.5*tt ) ge 1 and total(fupmmdev[bound:bound+60] gt .25)/(tt/2) gt .9 then begin
				print,"old bound =",bound
				bound=(where(ool eq max(ool)))[0]+bound
				print,"new bound =",bound
				;wait,5
			endif
			mnslope=min(B60d[bound:foot],mnsloc)
			if bound ne foot and mnsloc eq 0 and mnslope lt .3/tt and mnslope gt -.1/tt and abs(Bminute[bound+tt/2]-up) le .1 and footsn-bound eq 2*tt and total(finite(N_e[bound:bound+tt]) eq 1) gt 10 then begin
						st=bound
						en=bound+tt
						
						return,[st,en]

			endif
			;imin=max([imin,footsn-10*60,ishock-tt/2*60])
			;;;;;;;Want to check if forecountdown has dropped from a larger value. if so, and if that was more than 60 seconds ago, use range [ii-tt,ii]. 
			;;;;;;;If the previous forecountdown was 0 then do the same
			;;;;;;;if i eq ii or last drop was less than 60 seconds ago, do things the old fashioned way

			
			;bound=max([imin,foot-10*60,0])
			wherefin=where(finite(N_e[bound:efoot]) eq 1,nfin)+bound
			if nfin ge tt then begin
				bound=wherefin[0]
				print,"nfin=",nfin
				print,"new bound=",bound

			endif
		;bound=max([imin,footsn-10*60,0]);max([dim,min([imin,footsn-5*60]),dhalf,min([ed,footsn-5*60])])
			strt=max([0,bound-tt/2])
			ofst=bound-strt
			w25=where(fupmmdev[bound:efoot] lt .25)
			stdp=(smooth(mstd[strt:ishock+tt],tt))[ofst:ishock-strt]
			stpp=stdp[0:efoot-bound-tt/2]		
			mnstd=min(stpp[w25],mnstdlocp)
			meanstd=mean(stpp)
			mnstdloc=mnstdlocp+bound+w25[0];+tt/2+w25[0]
			stdthresh=(mnstd+meanstd)/2
			print,'tt=',tt
			if ymm[mnstdloc] le .75*up+.25*down and Bminute[mnstdloc] le .75*up+.25*down and stdp[mnstdlocp+tt/2] le stdthresh and mean(fupmmdev[mnstdloc-tt/2:mnstdloc+tt/2]) lt .25 then begin

					st=mnstdloc-tt/2;max([bi-tt/2-1,0])
					en=mnstdloc+tt/2;-(bi-tt/2-1+st)
					print,"[dim,ed,imin,bound,efoot,foot,i,imax]=",[dim,ed,imin,bound,efoot,foot,i,imax]
					print,"[st,mnstdloc,en]=",st,mnstdloc,en
					;print,"[imin,bound,i,st,mnstdloc,en,efoot,foot,imax,ed,dim]=",[imin,bound,i,st,mnstdloc,en,efoot,foot,imax,ed,dim]
					return,[st,en]

			endif
			;ed=bound		
			if i ne ii then begin
				if ii lt bound then ii=i
				if fore[ii] ne foremax[ii] then begin

					while fore[iii] ne foremax[iii] and iii ge ed+tt do iii--
					
				endif
				;ed=bound
				i2=ii
				print,time_string(xs[footsn])
				print,time_string(xs[foot])
				print,"numel(B),bound,ii-tt,ii,efoot,foot=",numel(B),bound,ii-tt,ii,efoot,foot
				if (ii-iii gt tt)  or fore[ii] eq foremax[ii] then begin
						if ii gt tt/2.+bound and fore[ii] eq foremax[ii] then begin 
							i2=ii

							wquietlocp=where(ymm[bound:ii-tt/2] lt .75*up+.25*down and stdp[0:ii-bound-tt/2] lt stdthresh)
							
							if wquietlocp[0] ne -1 then begin
								if numel( intersect(wquietlocp+bound,wherefin)) gt 10 then wquietlocp=intersect(wquietlocp+bound,wherefin)
								quietlocp=(where(stdp[wquietlocp] eq min(stdp[wquietlocp])))[0]
								quietloc=quietlocp+bound
								if total(quietlocp+tt/2 eq wquietlocp) ne 0 then begin
									st=quietloc-tt/2;max([bi-tt/2-1,0])
									en=quietloc+tt/2;-(bi-tt/2-1+st)
									print,"[imin,i,bi,st,quietloc,en,imax,ed,dim]=",[imin,i,bi,st,quietloc,en,imax,ed,dim]
									return,[st,en]
								endif 	

							endif
							while ii gt bound+tt+2 and ii le foot and ii-tt gt 0 and ii ne -1 and bound lt ii-1 and (max(B[ii-tt:ii]) ge down or  ((max(B[ii-tt:ii]) gt max([down-1,.8*down+.2*up])) and (total(B[bound:ii-1] gt max([down-1,.8*down+.2*up])) lt .4*(ii-1-bound) )) or   (mean(B[ii-tt:ii]) -up gt 3  or ymm[ii]-ymm[ii-tt] gt 3 or stddev(B[ii-tt:ii]) ge 1.5   or ((stddev([B[ii-tt],B[ii],mean(B[ii-tt:ii])]) lt .5  ) and (max(B[ii-tt:ii])-mean(B[ii-tt:ii]) gt 2)   ))) do ii--;while (ii ge ed+61) and (i2-ii lt 240) do ii--;and ((B[ii]-B[ii-tt] ge 1.5) or ((stddev([B[ii-tt],B[ii],mean(B[ii-tt,ii])]) lt .15  ) and (max(B[ii-tt:ii])-mean(B[ii-tt,ii]) gt 2)   )) do ii--
							print,"numel(B),ii-tt,ii,bound=",numel(B),ii-tt,ii,bound
							print,B[ii]
							print,B[ii-tt]
							Bmax=max(B[ii-tt:ii],maxuploc)
							BII=B[ii-tt:ii]
							print,numel(BII)
							print,numel(down)
							BIId=BII-down[0]
							BIIu=BII-up[0]
							if Bmax ge max([down-1,.8*down+.2*up]) or total(0 lt -abs(BIId) + abs(BIIu)) gt 5 then begin
								AB=where(B[ii-tt:ii] ge max([down-1,.8*down+.2*up]),abcount)+ii-tt
								if abcount gt 0 and (AB[0]-ii+tt lt tt-3  or  total(B[bound:max([AB[0]-1,bound+tt])] ge downs[i]-1)/(-bound+AB[0]-1) gt .7) then begin
									while(max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]) and  ii lt efoot  and median(B[ii-tt:ii]) -up lt 3 )do ii++;footsn  and median(B[ii-tt:ii]) -up lt 3 )do ii++
				print,"while 1 numel(B),ii-tt,ii,efoot,foot,bound=",numel(B),ii-tt,ii,efoot,foot,bound
									while(max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]) and tt gt 20)  do tt--
				print,"while 2 numel(B),ii-tt,ii,efoot,foot,bound=",numel(B),ii-tt,ii,efoot,foot,bound
								endif else begin
									print,"else begin numel(B),ii,efoot,foot,bound=",numel(B),ii,efoot,foot,bound
									while (max(B[ii-tt:ii],maxuploc) ge max([down-1,.8*down+.2*up]))  and ii-tt gt bound do ii--

									
								endelse
								print," endelse numel(B),ii,efoot,foot,bound=",numel(B),ii,efoot,foot,bound
								if abs(Bminute[imin+tt/2]-up) lt .2 and max(B[imin:imin+tt]) lt mean([down,ups[i]]) and abs(b60d[imin+tt/2]) lt .2/tt then begin
									st=bound
									en=bound+tt
									return,[st,en]
								endif
							endif
						endif
;						ustartini[ii]=max([ii-tt,0])
;						uendini[ii]=ii
						print,"spike in region"
						st=max([ii-tt,0])
						en=ii
						return,[st,en]
				endif
				if foot lt ed and not ((ii-iii gt tt)  or fore[ii] eq foremax[ii]) then begin
					if bound-foot le tt then return,[bound,foot]
					footloc=max([foot-tt/2-1,bound])
					bi=footloc+tt/2 +1
					st=footloc+1;max([bi-tt/2-1,0])
					en=bi+tt/2;-(bi-tt/2-1+st)
					print,"if footsn lt ed and not ((ii-iii gt tt)  or fore[ii] eq foremax[ii])"
					print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					return,[st,en]
				endif

			endif	
			;ed=bound


			if el ge 1 then fff=bound else fff=0
			;nfff=i-fff
			nfff=footsn-fff
			for j=0,nfff-1 do begin
				;if (flg[i-j] ne 0) or (i-j le bound+tt) then break
				if (flg[footsn-j] ne 0) or (footsn-j le bound+tt) then break
			endfor
			print,"[dim,bound,ed,imin,footsn,i-j,i,imax]=",[dim,bound,ed,footsn,imin,i-j,i,imax]
			if j ge nfff-2 then begin
				;print,"j>nfff-2"
				xii=xs[footsn];i]
				LLL=where((reg eq 1) and (rx le xii),lcount)
				;j=i-LLL[lcount-1]
				k=LLL[-1]
				if k eq -1 or rx[k] gt xs[i] then begin
					;footloc=imin
					;if i ne footsn then footloc=imin; max([footsn-tt/2,imin])
					;print,"rx[k]=rx[",k,"]=",rx[k],",xs[i]=",xs[i]
					;bi=footloc+tt/2+1
					st=bound;footloc+1;max([bi-tt/2-1,0])
					en=bound+tt;bi+tt/2;-(bi-tt/2-1+st)
					print,"[imin,i,bi,st,en,imax,ed,dim]=",[imin,i,bi,st,en,imax,ed,dim]
					return,[st,en]
				endif
				xt=abs(rx[k]-xs)
				;j=i-(where( xt eq min(xt)))[0]
				j=footsn-(where( xt eq min(xt)))[0]
			endif
			print,"[dim,ed,imin,j,i,imax]=",[dim,ed,imin,j,i,imax]
			;print,"i,j,i-j=",i,j,i-j
			cln=(flg[footsn-j]+1)/2;cln=(flg[i-j]+1)/2
			
			xf=xs[footsn-j];xf=xs[i-j]
			;ri=where((rx-2*dr le xf) and (rx+2*dr ge xf))
			ri=(where(abs(rx-xf) eq min(abs(rx-xf)))) ; want to verify we're in solar wind
			;print,"ri=",ri
			ri=ri[0]
			while reg[ri] ne 1 and rx[ri] gt xs[bound] do ri--;if we aren't we will shift backwards until we are
			
			rxf=rx[ri]; this is the time value of corresponding to index ri
			
			;bi=imin+tt/2-1;(where(abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			
			bi=(where(  xs ge xs[bound+tt/2-1] and  abs(xs-rxf) eq min(abs(xs-rxf))))[0] ; this is the index of the closest time value in the B frame (where dt=1 sec)
			;print,bi
			if bi eq -1 then bi=foot-tt/2
			while (xs[bi+tt/2] ge xs[footsn] or stdp[bi-bound] ge stdthresh or  ((((Bp[bi] gt 3) and (Bp[bi-1] lt Bp[bi] )) or B60d[bi+tt/2] gt .018)   and (mean(B[ii-tt:ii]) -up ge 3 or $
			 (abs(B[ii-tt]-up) lt .2  and B[ii]-up gt 3  )  ))) and bi gt bound+tt/2-1  and ~ (B[bi-tt/2] ge down) and finite(mean(N_e[bi-tt/2-2:bi+tt/2-1],/nan)) eq 1  do bi--
			;while (Bp[bi] gt 3) and (bi gt tt/2-1)  and (Bp[bi-1] lt Bp[bi] ) and (bi gt bound+tt/2-1) do bi--
			;if cln then begin ; if the solar wind is actually quiet, then just record the index of that location
			print,bi+tt/2,footsn,xs[bi+tt/2],xs[footsn]
			print,xs[bi+tt/2]-xs[imin],xs[footsn]-xs[imin]
				;ustartini[imin:imax]=bi
				;uendini[imin:imax]=bi

				;usflagi[bi]=1
				;ueflagi[bi]=1
				st=bi-tt/2-1
				en=bi+tt/2
				
			;endelse
			print,"base case"
			print,"[dim,ed,imin,foot,i,bi,st,en,imax,ed,dim]=",[ed,dim,imin,footsn,i,bi,st,en,imax]
			print,"[dim,imin,ubeg,uend,foot,i]=",[dim,imin,ustartini[i],uendini[i],footsn,i]
			return,[st,en]
end
