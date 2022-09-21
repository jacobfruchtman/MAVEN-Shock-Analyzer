pro saneAdjust,plt,thresh,newName=newName,absol=absol,removeBoundaryOutliers=removeBoundaryOutliers
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_sane'
	;if NOT KEYWORD_SET(absol) THEN absol=0

	title=newName
	;threshFlag,plt,thresh,kindThresh=0,newName=title 
	
	;tplotmultiplier,plt,title+"_flag2",newName,isFlag=1
	get_data,plt,data=dat,limits=lim
	ys=dat.y
	N=size(ys,/n_el)

	if KEYWORD_SET(absol) then begin
		for i =0, N-1 do if (abs(ys[i]) le thresh) then ys[i]=0.0
		if KEYWORD_SET(removeBoundaryOutliers) THEN begin
			while (max(ys) ge 100*max(ys[where(ys ne max(ys))])) and (max(ys) gt 0) do ys[where(ys eq max(ys))]=max(ys[where(ys ne max(ys))])

			while (min(ys) ge 100*min(ys[where(ys ne min(ys))])) and (min(ys) lt 0) do ys[where(ys eq min(ys))]=min(ys[where(ys ne min(ys))])
		endif
	endif else begin
		isPos=1
		if(thresh lt 0) then isPos=-1	
		for i =0, N-1 do if (ys[i]*isPos le thresh*isPos) then ys[i]=0.0
		if KEYWORD_SET(removeBoundaryOutliers) THEN begin
			while (max(ys*isPos) ge 100*max(ys[where(ys*isPos ne max(ys*isPos))])) and (max(ys*isPos) gt 0) do ys[where(ys*isPos eq max(ys*isPos))]=max(ys[where(ys*isPos ne max(ys*isPos))])
		endif
	endelse
	;ys[0]=0
	;i=-1
	
	dat.y=ys


	STORE_DATA,newName,data=dat,limits=lim
end
