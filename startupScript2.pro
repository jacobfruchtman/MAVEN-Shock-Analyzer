pro startupScript2, startdate=startdate,ndays=ndays,jname=jname

if keyword_set(jname) then journal,jname

if not keyword_set(startdate) then begin
	;startdate = '2014-12-24/0:00'  
	startdate = '2015-1-26/0:00' 
	;startdate = '2015-2-26/0:00'  
	;startdate = '2015-3-14/0:00'  
endif

if not keyword_set(ndays) then ndays=2.00

timespan, startdate, ndays


@mavenLoad.idlbatch

spice_vector_rotate_tplot,'mvn_B_1sec','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

options,'mvn_swics_velocity','SPICE_FRAME','MAVEN_SWIA'

spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',check = 'MAVEN_SC_BUS'
						;rotate B to MSO

spice_position_to_tplot,'MAVEN','MARS',frame = 'MSO'	;tplot variable for MAVEN position

.run machNumCalcFine
machNumCalcFine
.run machNumCalc2
machNumCalc2
mvn_mag_load,'l2_full'


.run mvn_swia_regid
mvn_swia_regid
.run arrMag
.run absPlot
.run arrayDims

.run numDerivative
.run SpectroMoment
.run flagClean
.run threshFlag
.run regidClean
.run tanhfit
.run atanh
.run plotSmoother
.run shockfitter
.run shockfitter2
.run vecsmoother
.run curveFitter
.run curveFitter3
.run curveFitter4
.run Fitloop
.run curveFitter5
.run tplotmultiplier
.run updownFlag               
.run shockNormCalc
.run shockTester
;.run Fitloop
.run cartToCyl
.run overshooter

arrMag("mvn_B_1sec",yt="Magnetic Field")

flagClean,"regid"

;regidClean
threshFlag,'regid_cleaned_plot',2,kindThresh=25
tplotmultiplier,'mvn_B_1sec_Mag','regid_cleaned_plotascend','B_outbound',isFlag=1
tplotmultiplier,'mvn_B_1sec_Mag','regid_cleaned_plotdescend','B_inbound',isFlag=1

get_data,'B_inbound',data=datin                                                  
yin=datin.y                                                                      
;NNN=where(finite(yin,/NAN) eq 0,count)
yin[where(finite(yin) eq 0,count)]=0
inf=yin*0    
inb=yin*0                                                                    
for i=1,size(inf,/n_el)-1 do if(yin[i] ne 0) and (yin[i-1] eq 0) then inf[i]=1  
for i=0,size(inf,/n_el)-2 do if(yin[i] ne 0) and (yin[i+1] eq 0) then inb[i]=1    
store_data,'inbound_enter',data={x:datin.x,y:inf}
store_data,'inbound_exit',data={x:datin.x,y:inb}

.run saneAdjust
                                     
plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=60,newName="B60"
numDerivative,"B60",1,"mid" 
plotSmoother,"B60deriv",smoothType=6,width=60,newName="B60d"
;threshFlag,'B60deriv',2,kindThresh=25,monoName='-B60'
saneAdjust,"B60d",8.0/(10.0^4),newName="B60dsane",/abs        
;saneAdjust,"B60d",8.0/(10.0^4),newName="B60dsane",/abs,/rem        

;saneAdjust,"B60d",-8.0/(10.0^4),newName="bB60dsane",/rem        
threshFlag,'B60dsane',2,kindThresh=25,monoName='-B60dsane';  if aaj is located where monof-B60dsane is on, then if you increase aj until it turns off, should be at 0th order shock location 
							  ;  if "                  " bmonof-B60dsane is on, then if you decrease aj until off, should reach zeroth order shock location


plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=5,newName="B5" 
plotSmoother,"B5",smoothType=6,width=3,newName="B5-3"                        
NumDerivative,'B5-3',1,'mid'                                                 
plotSmoother,"B5-3deriv",smoothType=6,width=10,newName="B5-3d10"   

plotSmoother,"mvn_B_1sec_Mag",smoothType=3,width=200,newName="B_sec_smoothed" 
;plotSmoother,"mvn_B_1sec_Mag",smoothType=2, newName="B_fitted" 
@Bfitting.idlbatch

;curveFitter,"mvn_B_1sec_Mag", newName="B_fitted" 
numDerivative,"B_fitted",1,"mid"
threshFlag,"B_fittedderiv",-1,kindThresh=2,newName="shock_ascend"
threshFlag,"B_fittedderiv",1,kindThresh=0,newName="shock_descend"

                        
calc,"'updown'='regid_cleaned_plotdescend'+'regid_cleaned_plotascend'"


updownFlag,'B_fittedderiv'
tplotmultiplier,"B_fitted","updown","BF_shock" 
tplotmultiplier,"BF_shock","B_fittedderiv_updown","BF_shocker"          
tplotmultiplier,"BF_shocker","regid_cleaned_plotascend_flag","BF_shockA"
tplotmultiplier,"BF_shocker","regid_cleaned_plotdescend_flag","BF_shockD"

shockNormCalc

calc,"'B_perturb3'='mvn_B_1sec_Mag'-'B_fitted3'"
calc,"'B_perturb5'='mvn_B_1sec_Mag'-'B_fitted5'"


;tplot,[35,77,101,129,140,141]
;@shockDebug.idlbatch
;@normalPlots.idlbatch

.run DotProduct 
.run cartToCyl
.run shockPlotter

startdate = '2015-1-26/0:00' 
shockPlotter,startdate    

plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=60,newName="B60" 
plotSmoother,"mvn_B_1sec_Mag",smoothType=5,width=60,newName="Bmin" 
plotSmoother,"mvn_swica_density",smoothType=5,width=60,newName="rho60" 
numDerivative,"rho60",1,"mid" 

tplot,[137,77,109,138]
tplot,"shocks",/add
;tplot,"rho60",/add
tplot,"bf34",/add
plotSmoother,"rho60deriv",smoothType=7,width=60,newName="rho60d" 
threshFlag,'rho60deriv',1,kindThresh=1
plotSmoother,"rho60",smoothType=5,width=20,newName="rho60-20" 
plotSmoother,"rho60-20",smoothType=5,width=20,newName="rho60-20-20" 
plotSmoother,"rho60deriv",smoothType=5,width=60,newName="rhomind" 
numDerivative,'rho60-20-20',1,"mid"  
calc,"'rho_perturb1'='mvn_swica_density'-'rho60-20-20'"

options,'Bmin','colors','y'
options,'rho60-20','colors','b'
options,'rho60-20-20','colors','r'
store_data,'BfitCombo3',data = "mvn_B_1sec_Mag B_fitted3"
store_data,'rhocombo',data = "mvn_swica_density rho60-20 rho60-20-20"

plotSmoother,"mvn_swica_density",smoothType=8,width=5,newName="stdrho5" 

calc,"'bfs'='B_fitted3'-'B_fitted4'"

tplot,['bf3456','shocks']

overshooter

tplot,'overshoot',/add

end
