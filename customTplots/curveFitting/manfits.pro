pro manfits,tms,MM0=MM0

	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB

	ys=datB.y
	xs=datB.x

	imin=tms[0]
	imax=tms[1]
	yp=ys[tms[0]:tms[1]]
	xp=xs[tms[0]:tms[1]]

	print,time_string(xp[0])
	print,time_string(xp[-1])
	xpa=xp-xp[0]
	weight=1.0/yp
	curvetype="tanhfit"
	MM1=MM0
	status1=-10
	CHISQ1=-1
	help,xpa
	help,yp
	help,weight
	help,MM1
	help,MM1[2]
	help,time_string(xs[MM1[2]+imin])
	weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-10
			CHISQ=-1
	yfit1=curvefit(itmax=40,xpa, yp, weight, MM1, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
	ishock1=MM1[2]+imin

	MM3=MM0
	yfit3=lmfit(xpa,yp,MM3,weight=weight,$
	/double,FUNCTION_NAME='lm_'+curvetype,convergence=status3,$
	CHISQ=CHISQ3,sigma=sigma3)
	ishock3=MM3[2]+imin

	p0=plot(xpa,yp)
	p1=plot(/over,xpa,yfit1,name='curvefit,chisq='+strtrim(CHISQ1,2),color='blue')

p3=plot(/over,xpa,yfit3,name='lmfit,chisq='+strtrim(CHISQ3,2),color='red')

	p1s=plot(fltarr(2) + MM1[2], p0.yrange,color='cyan', /overplot,name="curvefit shock")
	
	p3s=plot(fltarr(2) + MM3[2], p0.yrange,color='tomato', /overplot,name="lmfit shock")
end
