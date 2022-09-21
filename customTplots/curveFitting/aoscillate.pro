function aoscillate, aj, bi,dim,yBS,nbeg,nend,dobug
			nend=bi-aj+1
			nbeg=aj-dim+1
			;dobug=2
			;if dobug gt 1 then print,'aj=',aj
			;if dobug gt 1 then print,nbeg,nend
			;if dobug gt 1 then print,numel(yBS),'yBS[dim:bi]=yBS[',dim,':',bi,']'
			if dim eq -1 then return, aj
			ypBS=yBS[dim:bi]
			BSbeg=yBS[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			;if dobug gt 1 then print,'numel(BSend)=',numel(BSend)
			;if dobug gt 1 then print,'[8 * (nend-1)/10: 9 * (nend-1)/10]=','[',8 * (nend-1)/10,':', 9 * (nend-1)/10,']'
			BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			meanTbeg=mean(BSbegtrust)

			meanTend=mean(BSendtrust)


			stdbeg=finite(stddev(BSbegtrust,/nan))

			stdend=finite(stddev(BSendtrust,/nan))
			meanT=mean([meanTbeg,meanTend])
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=yBS[hhj]


			aaj=aj

			BSh=yBS[aaj]

			;print,'hhj-gim=',hhj-gim
			;print,'yBS[hhj]=',BSh
			;print,'meanT=',meanT
			;print,'gi=DD[i]=G[',i,']=',gi
			;print,'hj=H[',j,']=',hj
			;print,'gim=DD[i-1]=G[',i-1,']=',gim
			hosc=[0,0]
			;print,'numel(mono)=',numel(mono)
			;print,'[dim,bi]=',[dim,bi]

			;print,'total(monend)-numel(monend)=',total(monend),'-',numel(monend),'=',total(monend)-numel(monend)
			;print,'aj=',aj,', dim=',dim, ',bi=',bi,', size(xpa,/n_el)=',size(xpa,/n_el),'aj-dim=',aj-dim

			
			;print,'starting while loop'
			decs=0
			incs=0
			while((meanT+stdend lt BSh) or (meanT-stdbeg gt BSh)) do begin
				
				if(meanT+stdend lt BSh) then begin
					decs++
					;print,'aaj--'
					;hhj--
					aaj--
					;BSh=yBS[hhj]
					BSh=yBS[aaj]
					hosc[1]=1
				endif
				
				if(meanT-stdbeg gt BSh) then begin
					incs++
					;print,'aaj++'
				;	hhj++
				;	BSh=yBS[hhj]
					aaj++
					BSh=yBS[aaj]
					hosc[0]=1
				endif
				;print,'hhj=',hhj
				;print,'BSh=',BSh
				;print,'hosc=',hosc
			;	if((hhj ge gi) or (hhj le gim)) then begin
			;		;hhj+=hosc[1]-hosc[0]
			;		hhj=hj
				if((aaj ge bi) or (aaj le dim)) or max([decs,incs]) ge 1000 then begin
					;hhj+=hosc[1]-hosc[0]
					aaj=aj

					break 
				endif
				if(total(hosc) eq 2) then break

				
			
		
			endwhile
		;if dobug gt 1 then print,'[decs,incs]=',[decs,incs]

return, aaj
end

