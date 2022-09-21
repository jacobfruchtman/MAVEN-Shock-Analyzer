pro tscatter3plot,xplt,yplt,zplt,$ ;the x, y,and z plots. Or arrays, if N_elements >1 and type ne 7
			plt=plt,cbar=cbar,$ ;named variable to save the plot object, colorbar object to
			close=close,save=save,$
			singlepoint=singlepoint,$
			zmax=zmax,zmin=zmin,$
			xrange=xrange,yrange=yrange,zrange=zrange,xlog=xlog,ylog=ylog,$
			_EXTRA=ex
			;AXIS_STYLE=AXIS_STYLE,BUFFER=BUFFER,CURRENT=CURRENT,DEVICE=DEVICE,LAYOUT=LAYOUT,LOCATION=LOCATION,$;keywords
			;MARGIN=MARGIN,NODATA=NODATA,OVERPLOT=OVERPLOT,$
			
			;ANTIALIAS=ANTIALIAS, ASPECT_RATIO= ASPECT_RATIO, AXES= AXES, BACKGROUND_COLOR= BACKGROUND_COLOR,$
			;BACKGROUND_TRANSPARENCY= BACKGROUND_TRANSPARENCY,CLIP= CLIP, CROSSHAIR= CROSSHAIR, FONT_COLOR= FONT_COLOR,$
			;FONT_NAME= FONT_NAME, FONT_SIZE= FONT_SIZE, FONT_STYLE= FONT_STYLE, HIDE= HIDE,MAGNITUDE=MAGNITUDE,$
			;MAX_VALUE=MAX_VALUE,MIN_VALUE=MIN_VALUE, NAME= NAME, POSITION= POSITION, RGB_TABLE= RGB_TABLE,$
			;SYM_COLOR= SYM_COLOR,SYM_FILLED= SYM_FILLED, SYM_FILL_COLOR= SYM_FILL_COLOR, SYM_OBJECT= SYM_OBJECT,$
			;SYM_SIZE= SYM_SIZE, SYM_THICK= SYM_THICK, SYM_TRANSPARENCY= SYM_TRANSPARENCY, SYMBOL= SYMBOL, TITLE= TITLE,$
			;UVALUE= UVALUE, WINDOW= WINDOW,WINDOW_TITLE=WINDOW_TITLE, XRANGE= XRANGE,YRANGE=YRANGE, ZVALUE= ZVALUE,$


			;xrange=xrange,yrange=yrange,zrange=zrange,xmax=xmax,ymax=ymax,zmax=zmax,xlog=xlog,ylog=ylog
			
			if (n_elements(xplt) eq 1 and ~keyword_set(singlepoint)) or size(xplt,/typ) eq 7 then begin
				get_data,xplt,data=datx
				x=datx.y
			
			endif else x=xplt
			if (n_elements(yplt) eq 1 and ~keyword_set(singlepoint)) or size(yplt,/typ) eq 7 then begin
				get_data,yplt,data=daty
				y=daty.y
			
			endif else y=yplt
			if (n_elements(zplt) eq 1 and ~keyword_set(singlepoint)) or size(zplt,/typ) eq 7 then begin
				get_data,zplt,data=datz
				z=datz.y
			
			endif else z=zplt
			
			zbyt=bytscl(z,min=zmin,max=zmax)
			if keyword_set(zrange) then zr0=zrange
			plt=scatterplot(x,y,xrange=xrange,yrange=yrange,magnitude=zbyt,_STRICT_EXTRA=ex)
			;cbar=colorbar(target=plt)
			;if keyword_set(zr0) then cbar.range=zr0
			if keyword_set(save) then plt.save,save
			if keyword_set(close) then plt.close
			print,ex
end
