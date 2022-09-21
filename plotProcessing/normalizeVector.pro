FUNCTION VECSQUARE, vv
		;"Programs can't be compiled from single statement mode." What?
		;"Variable is undefined: VV." WHAT?

			sqr=0
	    FOR i = 0, 2 DO BEGIN	
	   	sqr=sqr+vv[*,i]*vv[*,i]
	
		ENDFOR
		
	    RETURN, sqr
	
END
	
FUNCTION VECSQUARE2, vx,vy,vz
		sqr=vx*vx+vy*vy+vz*vz
	
		;ENDFOR
	
	    RETURN, sqr
	
END
	
FUNCTION VECMAG, vv
		mag=0
		mag=SQRT(VECSQUARE(vv))
	
		RETURN, mag
END
	
FUNCTION VECMAG2, vecx, vecy , vecz
		mag=0
		mag=SQRT(VECSQUARE2(vecx,vecy,vecz))
	
		RETURN, mag
END

FUNCTION VECDIV,vv,denom
	
		q=TRANSPOSE([transpose(vv[*,0]/denom),transpose(vv[*,1]/denom),transpose(vv[*,2]/denom)])
		
		RETURN, q
END


pro normalizeVector,  x
	;Was going to make this accept as data type any of plot#,plot name,data_structure, and 2dvector. Unfortunately, all except the second one run into the problem of lacking info to  automatically name the new tplot
	typarr= SIZE(x)
	xs=0
	;haveName=0
	oldName=""
	oldY=""
	CASE typarr[0] of
		0: BEGIN
			get_data,x,data=dat
			if (typarr[1] EQ 7) THEN oldName=x
			ys=dat.y
		END
		
		1: BEGIN
			IF (typarr[2] EQ 8) THEN BEGIN
				dat=x
				ys=dat.y
				oldY=dat.YTITLE
			ENDIF  ELSE BEGIN
				print, "error: This is not a valid input"
				RETURN
			ENDELSE
		END
		2: BEGIN
				ys=x
				get_data,'mvn_swica_velocity',data=dat,alim=limV
				READ, oldY, PROMPT='Enter the un-normalized yaxis name:'
				;haveName=2
		END
	ENDCASE

	print, "filetype found"
	help,dat
	;get_data,x,data=dat
	;ys=dat.Y
	ymag=vecmag(ys)
	unitYs=vecdiv(ys,ymag)

	dat.y=unitYs	

	;splitName=dat.ytitle.split('[\(\[]')
	;dat.ytitle=splitName[0]+STRING(10B)+"unit vector"

	IF (1-oldName.compare("")) THEN READ, oldName, PROMPT='Enter the un-normalized tplot name:'
	titl=oldName+"_unit_vector"
	store_data,titl,data=dat
	
END
