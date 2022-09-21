pro arrstd,plt,newName=newName,ytitle=ytitle

			get_data,plt,data=dat,alim=limit
			if NOT KEYWORD_SET(newName) THEN newName=ar+'_vecstddev'
			xs=dat.x
			ys=dat.y
			N=size(ys,/n_el)
			NN=(size(ys,/dim))[0]
			dim2=N/NN
			print,"N=",N
			print,"size(ys,/dim)=",size(ys,/dim)
			z=fltarr(NN)
			print,dim2
			case dim2 of
				
				2: for i=0,NN-1 do z[i]=stddev(ys[i,*])
				3: for i=0,NN-1 do z[i]=stddev(ys[i,*])
				4: for i=0,NN-1 do z[i]=stddev(ys[i,0:2])
				6: for i=0,NN-1 do z[i]=stddev(ys[i,0:2])
			ELSE: begin
					if dim2 eq 1 then print,"this is a scalar array. You idiot." else print, "what kind of tplot is this? You moron."
					return
				end
			endcase

			ndat={x:xs,y:z}

			possibleEls=['YLOG','ZLOG','SPEC','NO_INTERP','YRANGE','ZRANGE','ystyle','ytitle']

			;if not keyword_set(ytitle) then begin
			
			foreach el, possibleEls do begin
				str_element,dat,el,val,success=s
				if s then str_element,ndat,el,val,/add
			endforeach


			if NOT KEYWORD_SET(ytitle) THEN begin
				str_element,dat,'ytitle',vald,success=sd
				str_element,limits,'ytitle',vall,success=sl
				if sd then str_element,ndat,'ytitle',vald,/add_replace else $
				if sl then str_element,ndat,'ytitle',vall,/add
			endif else str_element,ndat,'ytitle',ytitle,/add_replace;ndat.ytitle=ytitle
			store_data,newName,data=ndat,dlim=limits

end
