pro clearPlots
	;print,startdate
	;strdate=startdate.remove(-5)
	
	;x00=Cal2Unix(1,1,1,stringDate=strdate)
	;get_data,1,data=dat
	;xx=dat.x
	;x0=xx[0]

	;tdur=ndays*24*60*60
	;rstarr=fltarr(tdur)
	
	;rstarr3=[[rstarr],[rstarr],[rstarr]]

	;rstx=rstarr+x00
	;suffixsio=['','_inbound','_oubound']
	;suffixstyp=['','_CHISQ']
	;Bsuffixstyp=['','_shocks','_CHISQ']
	;rstdat={x:rstx,y:rstarr}
	;rstdat3={x:rstx,y:rstarr3}
	del_data,"B_fitte*"
	del_data,"a*"
	del_data,"shocks"
	del_data,"shocks_*"
	del_data,"shock_loc*"
	del_data,"shock0*"
	del_data,"Shock_An*"
	del_data,"Shock_No*"
	del_data,"Franken_fitte*"
	del_data,"wind_*"
	del_data,"down*"
	del_data,"upstream_*"
	;foreach io,suffixsio do begin
		;FOR i=1, 12 do begin
			;if i eq 1 then num='' else num=strtrim(i,2)
			;foreach typ, Bsuffixstyp do store_data,"B_fitted"+num+typ+io,data=rstdat

		;endfor
		;foreach typ, suffixstyp do store_data,"Franken_fitted"+typ+io,data=rstdat
		;store_data,"shocks"+io,data=rstdat
		;store_data,"shock_locs"+io,data=rstdat
	;endforeach
	del_data,"overshoot"
	del_data,"overshoot*"
	;del_data,"over*"
	del_data,"overfront*"
	del_data,"overback*"
	del_data,"B_mins_outbound"
	del_data,"B_max*"
	del_data,"B*"
	del_data,"b*"
	del_data,"mvn_B*"
	del_data,"*_MAVEN_MSO"
	del_data,"*_inboun*"
	del_data,"*_outboun*"
	del_data,"*_interpolated"
	del_data,"*deriv"
	;del_data,"*_stddev"
	;del_data,"*_extrema"
	del_data,"*_flattened"
	del_data,"mvn_*"
	;del_data,"BF_*"
	del_data,"*_flag*"

	;del_data,"DD*"
;	del_data,"E*"
;	del_data,"e*"
;	del_data,"F*"

;	del_data,"H*"
;	del_data,"I*"
;	del_data,"i*"
	;DEL_DATA,"A*"
	;DEL_DATA,"B*"
	DEL_DATA,"C*"
	;DEL_DATA,"E*"
	DEL_DATA,"F*"
	;DEL_DATA,"G*"
	;DEL_DATA,"H*"
	;DEL_DATA,"I*"
	;DEL_DATA,"J*"
	;DEL_DATA,"K*"
	DEL_DATA,"L*"
	DEL_DATA,"M*"
	DEL_DATA,"O*"
	;DEL_DATA,"R*"
	DEL_DATA,"S*"
	;DEL_DATA,"T*"
	;DEL_DATA,"U*"
	;DEL_DATA,"V*"
	;DEL_DATA,"W*"
	;DEL_DATA,"X*"
	;DEL_DATA,"Y*"
	;DEL_DATA,"Z*"


	;del_data,"a*"
	del_data,"b*"
	del_data,"c*"
	;del_data,"e*"
	del_data,"f*"
	;del_data,"g*"
	;del_data,"h*"
	del_data,"i*"
	;del_data,"j*"
	;del_data,"k*"
	del_data,"l*"
	del_data,"m*"
	del_data,"o*"
	del_data,"r*"
	del_data,"s*"
	del_data,"t*"
	del_data,"u*"
	del_data,"v*"
	del_data,"w*"
	;del_data,"x*"
	;del_data,"y*"
	;del_data,"z*"
	;del_data,"*"
	;del_data,"*_mean"
	;store_data,"shock_locs"+io,data=rstdat


end
