pro ionsetup,noarc=noarc,doquick=doquick
full=1
surveyonly=1
if keyword_set(noarc) then surveyonly=1
if keyword_set(doquick) then full=0
;if ~surveyonly then get_data,'mvn_swifa_density',data=datswifan

if 1 then begin;datswifan.x[-1] lt time_double('2020-01-01/00:00') then begin
	if ~surveyonly then typelst=list('swifs','swics','swifa','swica') else typelst=list('swifs','swics')
	for i=0,numel(typelst)-1 do begin
		el=typelst[i]
		if full then begin
		pname='mvn_'+el+'_pressure'
		print,pname
		;tplot,pname[0],/add
		get_data,pname[0],data=dat,alim=lim
		;dat.y/=.7
		store_data,pname[0],data=dat,dlim=lim
		endif
		nname='mvn_'+el+'_density'
		get_data,nname,data=dat,alim=lim
		;dat.y/=.7
		store_data,nname,data=dat,dlim=lim
	endfor;each

endif

s=arrMag('tproton',yt='Temperature [eV]') 
s=arrMag('talpha',yt='Temperature [eV]') 

if ~surveyonly then begin
interpolator2,'mvn_swica_velocity_MAVEN_MSO'
;s=arrMag('mvn_swica_velocity',yt="SWIA !C Velocity !C [km]",newName="swica_velocity_Mag")
;s=arrMag('mvn_swica_velocity_MAVEN_MSO_interpolated',yt="SWIA !C Velocity !C [km]",newName="swica_velocity_interpolated_Mag")
tvectot,'mvn_swica_velocity',tot="swica_velocity_Mag"
tvectot,'mvn_swica_velocity_MAVEN_MSO_interpolated',tot="swica_velocity_interpolated_Mag"

interpolator2,'mvn_swica_density'    
interpolator2,'mvn_swifa_density'
endif

interpolator2,'tproton_Mag'  


interpolator2,'mvn_swics_velocity_MAVEN_MSO'
interpolator2,'mvn_swifs_velocity_MAVEN_MSO'
tvectot,'mvn_swics_velocity',tot="swics_velocity_Mag"
tvectot,'mvn_swics_velocity_MAVEN_MSO_interpolated',tot="swics_velocity_interpolated_Mag"
;s=arrMag('mvn_swics_velocity',yt="SWIA !C Velocity !C [km]",newName="swics_velocity_Mag")
;s=arrMag('mvn_swics_velocity_MAVEN_MSO_interpolated',yt="SWIA !C Velocity !C [km]",newName="swics_velocity_interpolated_Mag")
                                                               
interpolator2,'mvn_swics_density' 
interpolator2,'mvn_swifs_density' 

 
                                   
;plotSmoother,"mvn_swics_density_interpolated",smoothType=-2,width=30,newName="rho_median"
;plotSmoother,"mvn_swics_density_interpolated",smoothType=6,width=60,newName="rho60"

if full then begin   
s=arrMag('mvn_swics_pressure',yt="SWIA !C Pressure !C [eV/cm^3]",newName="swics_Pressure_Mag")
calc,'"dendiff"="mvn_swics_density_interpolated"-"mvn_swifs_density_interpolated"'
calc,'"denferror"=abs("mvn_swics_density_interpolated"-"mvn_swifs_density_interpolated")/("mvn_swics_density_interpolated")'
calc,'"denfdiff"=abs("mvn_swics_density_interpolated"-"mvn_swifs_density_interpolated")/("mvn_swics_density_interpolated"+"mvn_swics_density_interpolated")'
threshflag,'denfdiff',.18,kind=2,new='denfdiff_blw2'
onofflengthplot,'denfdiff_blw22_trigger',new='denfdiff_blw22Lengths'
threshFlag,'denfdiff_blw22Lengths',5*60,kind=28,new='denjump'   
threshFlag,'denfdiff_blw22Lengths',5*60,kind=29,new='denjumpdown'   
plotSmoother,"swics_velocity_Mag",smoothType=6,width=5,newName="swics_v5" ;;THIS IS FOR SKIPPING OVER WEIRD FLUCTUATIONS IN SOLAR WIND: AVERAGING OVER WILL WASH OUT DROPS, LEAVING |v|_smooth >500
									  ;;IN SOLAR WIND	
interpolator2,'swics_v5'     
                                  
plotSmoother,"mvn_swics_density_interpolated",smoothType=6,width=30,newName="rho30"
plotSmoother,"swics_velocity_interpolated_Mag",smoothType=6,width=30,newName="v30"
plotSmoother,"mvn_swics_density_interpolated",smoothType=6,width=10,newName="rho10"
numDerivative,"rho30",1,'mid'
numDerivative,"v30",1,'mid'
numDerivative,"rho10",1,'mid'
endif
end
