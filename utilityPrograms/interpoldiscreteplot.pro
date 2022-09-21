;+
;PROCEDURE:  interpoldiscreteplot,plt_source,plt_target,newName=newName
;PURPOSE: takes source tplot 'plt_source' and resizes it's x and y (and v, if it exists) to the sizes of the target tplot 'plt_target' discretely  
;	  that is, it assigns to each element of the resized y ('ny[i]') at a given time t (nx[i])  the value of y in the source tplot corresponding to the closest associated time
;         in the original frame
;INPUT: plt_source: the name of the tplot to be resized. if newName is set, can be a tplot index. Y (and V) can have dimensions 1 or 2
;	plt_target: the name of the tplot possessing the target size. Can be a tplot index
;
;KEYWORDS: newName: the name of the new tplot variable. If not set, will be set as   newName=plt_source+"_resized"   
;
;
;METHOD: 
;
;		for i=0,ndim1-1 do begin
;			mn=min(abs(ox-nx[i]),tn)
;			ny[i]=oy[tn]
;		endfor
;	
;
;
;NOTE: This does not copy any fields in the tplot structure other than the following:
;possibleEls=['x','y','v','YLOG','ZLOG','SPEC','NO_INTERP','YRANGE','ZRANGE','ystyle','labels','labflag','ytitle','ztitle']
;
;-

pro interpoldiscreteplot,plt_source,plt_target,newName=newName

	if not keyword_set(newName) then newName=plt_source+"_resized"

	get_data,plt_source,data=datsource,alim=lim

	get_data,plt_target,data=dattarget

	dat=interpoldiscretedat(datsource,dattarget)

	store_data,newName,data=dat,dlim=lim
end
