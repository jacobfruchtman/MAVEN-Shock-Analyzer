;+
;FUNCTION:  interpoldiscretedat,datsource,dattarget
;PURPOSE: takes source data structure 'datsource' and resizes it's x and y to the sizes of the target datastructure 'dattarget' discretely  
;	  that is, it assigns to each element of the resized y the value of y in the source structure corresponding to the closest associated time
;INPUT: datsource: the data structure to be resized
;	dattarget: the data structure with the size to be used
;NOTE: This does not copy any fields in the structure other than the following:
;possibleEls=['x','y','v','YLOG','ZLOG','SPEC','NO_INTERP','YRANGE','ZRANGE','ystyle','labels','labflag','ytitle','ztitle']
;
;-
function interpoldiscretedat,datsource,dattarget
		ox=datsource.x
		oy=datsource.y
		nx=dattarget.x
		ny=interpoldiscrete(oy,ox,nx)
		dat={x:nx,y:ny}
		str_element,datsource,'v',ov,success=sv

		if sv then begin
			nv=interpoldiscrete(ov,ox,nx)
			str_element,dat,'v',nv,/add_replace
		endif
		possibleEls=['YLOG','ZLOG','SPEC','NO_INTERP','YRANGE','ZRANGE','ystyle','labels','labflag','ytitle','ztitle']
		foreach el, possibleEls do begin
			str_element,datsource,el,val,success=s
			if s then str_element,dat,el,val,/add
		endforeach
		return,dat
end
