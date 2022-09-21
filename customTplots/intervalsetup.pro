pro intervalsetup
tic
print,"=====INTERVAL SETUP====="
get_data,"mvn_B_1sec_Mag",data=datB
xs=datB.x
x0=xs[0]
N=numel(xs)
store_data,'ascend_end',data={x:xs,y:fltarr(N),ytitle:'ascend_end'}
store_data,'descend_end',data={x:xs,y:fltarr(N),ytitle:'descend_end'}
store_data,'ascend_begin',data={x:xs,y:fltarr(N),ytitle:'ascend_begin'}
store_data,'descend_begin',data={x:xs,y:fltarr(N),ytitle:'descend_begin'}



;tplot,"mvn_B_1sec_MAVEN_MSO_Mag",/add
;tplot,"regid",/add
flagClean,"regid"
interpolator,'regid_cleaned_plot'

threshFlag,'regid_cleaned_plot_interpolated',2,kindThresh=3,newname='insheathorwind'
calc,'"denjump_trigger"="denjump_trigger"*"insheathorwind_trigger"'
calc,'"denjumpdown_trigger"="denjumpdown_trigger"*"insheathorwind_trigger"'
                                                                        
regidReclean
;regidClean

get_data,'regid_cleaned_plot',data=datReg

reg=datReg.y

JJJ=where(reg eq 1, jcount)
if jcount eq 0 then exit

threshFlag,'regid_cleaned_plot',2,kindThresh=25

flagexpand


tplotmultiplier,'mvn_B_1sec_MAVEN_MSO_Mag','regid_cleaned_plotascend','B_outbound',isFlag=1
tplotmultiplier,'mvn_B_1sec_MAVEN_MSO_Mag','regid_cleaned_plotdescend','B_inbound',isFlag=1

get_data,'B_inbound',data=datin                                                  
yin=datin.y                                                                      
;NNN=where(finite(yin,/NAN) eq 0,count)
yin[where(finite(yin) eq 0,count)]=0
inf=fltarr(N)    
inb=fltarr(N)                                                                   
for i=1,size(inf,/n_el)-1 do if(yin[i] ne 0) and (yin[i-1] eq 0) then inf[i]=1  
for i=0,size(inf,/n_el)-2 do if(yin[i] ne 0) and (yin[i+1] eq 0) then inb[i]=1    
store_data,'inbound_enter',data={x:datin.x,y:inf}
store_data,'inbound_exit',data={x:datin.x,y:inb}






plotSmoother,"B60deriv",smoothType=6,width=60,newName="B60d"
;threshFlag,'B60deriv',2,kindThresh=25,monoName='-B60'
saneAdjust,"B60d",8.0/(10.0^4),newName="B60dsane",/abs        
;saneAdjust,"B60d",8.0/(10.0^4),newName="B60dsane",/abs,/rem        

;saneAdjust,"B60d",-8.0/(10.0^4),newName="bB60dsane",/rem        
threshFlag,'B60dsane',2,kindThresh=25,monoName='-B60dsane';  if aaj is located where monof-B60dsane is on, then if you increase aj until it turns off, should be at 0th order shock location 
							  ;  if "                  " bmonof-B60dsane is on, then if you decrease aj until off, should reach zeroth order shock location

smallshockintervals
print,"=====END INTERVAL SETUP====="
toc
end
