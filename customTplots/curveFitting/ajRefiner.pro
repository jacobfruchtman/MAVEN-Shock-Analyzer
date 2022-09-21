function ajRefiner,foot,dim,aj,bi,zeron,dobug,yBS,nbeg,nend, mono,hill,hill2,orderCustom,extr,rhod,Fore
			;help,Fore
		print,orderCustom
		OCN=numel(orderCustom)

		if orderCustom[0] eq -1 then return,aj
		;help,orderCustom
		ajsdim=fltarr(OCN+2)
		ajsdim[0]=zeron

		ajsdim[1]=aj-dim		

		ajstr=strarr(OCN+2)
		ajstr[0]="zeron"
		ajstr[1]="aj0-dim"
		;xarr=findgen(numel(yBS[dim:bi]));+dim
		;plotlist=list()
		;if dim eq 56792 then begin
		;	p1=plot(xarr,yBS[dim:bi])
		;	p2=plot(fltarr(2) + aj, p1.yrange, name="zeroth",color='red', /overplot)
		;	plotlist.add,p1
		;	plotlist.add,p2
			
		;endif
		;colors=["orange red","orange","gold","green yellow","green","turqoise","blue","indigo","purple"]
		aj00=aj
		for ii=0,numel(orderCustom)-1 do begin;OCN-1 do begin
			aj0=aj
			error_status=0
			catch,error_status
			if(error_status ne 0) then begin
				PRINT, 'Error index: ', error_status
				PRINT, 'Error message: ', !ERROR_STATE.MSG
				PRINT,"CRASHED WHILE CALCULATING "+nm
				ii++
				aj=aj0
				catch,/cancel
				continue
			endif


			el=orderCustom[ii]
			nm=""

			Case el of 
				-1: BEGIN
					nm="aoscillating"
					aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)

				END
				1:BEGIN
					nm="monorolling"
					;help,aj
					;help,bi
					;help,dim
					;help,mono
					;help,Fore
					 aj=monoRoll(aj,bi,dim,mono,Fore) 
					;if dobug eq 0 then print,"after monorolling,"	
				END
				2:BEGIN
					nm="secondrolling"
					 aj=secondRoll(aj,bi,hill,dobug)
				;	if dobug eq 0 then print,"after secondrolling,"	
				END
				3:BEGIN
					nm="thirdrolling"
				 aj=Third_Roll(hill2,dim,aj,bi,dobug,foot)
				;if dobug eq 0 then print,"after thirdrolling,"	
				END
				4:BEGIN
					nm="downrolling"
				 while (yBS[max([aj-10*60,dim])] -yBS[dim] gt 10) and (aj gt dim) do aj--
				;if dobug eq 0 then print,"after downrolling,"	
				END
				5:BEGIN
					nm="fifthrolling"
				 aj=fifth_roll(dim,aj,bi,yBS)
				;if dobug eq 0 then print,"after fifthrolling,"	
				END
				6:BEGIN
					print,"numel(extr),numel(yBS),bi=",numel(extr),numel(yBS),bi
					nm="EXTREMA ROLL"
				 aj=extremaroll(dim,aj,bi,extr,rhod,Fore)
				;if dobug eq 0 then print,"after fifthrolling,"	
				END
				0:BEGIN
					 aj=aj
				END
			ENDCASE
			if aj ge bi-60 then aj=aj0
			;if dobug gt 0 then print,"after "+nm+","
			if dobug gt 0 then print,"after "+nm+","	
			if dobug gt 0 then print,"[imin,aj,imax]=",[dim,aj,bi]
			ajsdim[ii+2]=aj-dim
			ajstr[ii+2]="aj"+strtrim(ii+1,2)+"-dim"
			;if dim eq 56792 then begin
			;	plotlist.add,plot(fltarr(2) + aj, p1.yrange, name="nm, refine#"+strtrim(ii,2),color=colors[ii], /overplot)
			;endif
		endfor
		;if dim eq 56792 then begin
		;	ccf = LEGEND(target=plotlist, font_size=7,transparency=50);[p10,p11,p12,p13])
		;endif
	;		aj0=aj
	;		if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono) ; rolls predicted m2 (inflection point of atan up to 
	;		aj1=aj
	;		if dobug gt 1 then print,'numel(hill)=',numel(hill),', bi=',bi
	;		if (rollsec eq 1) then aj=secondRoll(aj,bi,hill,dobug) ; second order correction
	;		aj2=aj 
	;		aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
	;		aj3=aj
	;		if (rollthird eq 1) then aj=Third_Roll(hill2,dim,aj,bi,dobug) ; third order correction
	;		aj4=aj 
	
			;aj=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
;			aj5=aj
		if dobug gt 1 then print,"["+strjoin(ajstr,',')+"]=",ajsdim;[zeron,aj0-dim,aj1-dim,aj2-dim,aj3-dim,aj4-dim,aj5-dim]
		if dobug gt 1 then print,"["+strjoin(ajstr[1:*]+"/(bi-dim+1)",',')+"]=",ajsdim[1,*]*1.0/(bi-dim+1);[zeron,aj0-dim,aj1-dim,aj2-dim,aj3-dim,aj4-dim,aj5-dim]


	return,aj

end
