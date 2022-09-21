pro magneticsetup1

plotSmoother,"mvn_B_1sec_Mag",smoothType=1,width=20,newName="B_3sec" 
plotSmoother,"mvn_B_1sec_Mag",smoothType=1,width=10,newName="B_10sec"                                     
plotSmoother,"mvn_B_1sec_Mag",smoothType=1,width=20,newName="B_20sec"                                     
plotSmoother,"mvn_B_1sec_Mag",smoothType=1,width=30,newName="B_half" 
plotSmoother,"mvn_B_1sec_Mag",smoothType=1,width=60,newName="B_minute"                                                                         
plotSmoother,"mvn_B_1sec_Mag",smoothType=-1,width=30,newName="B_stddev"
plotSmoother,"mvn_B_1sec_Mag",smoothType=-2,width=30,newName="B_median"  
plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=60,newName="B60"
numDerivative,"B60",1,"mid" 

;plotSmoother,"B_half",smoothType=6,width=5,newName="B_half5" 

plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=5,newName="B5" 
plotSmoother,"B5",smoothType=6,width=3,newName="B5-3"                        
NumDerivative,'B5-3',1,'mid'                                                 
plotSmoother,"B5-3deriv",smoothType=6,width=10,newName="B5-3d10"   
;TIC
plotSmoother,"mvn_B_1sec_Mag",smoothType=3,width=200,newName="B_sec_smoothed" 
;TOC
;TIC
plotSmoother,"mvn_B_1sec_Mag",smoothType=3,width=200,newName="B_sec_smoothed",/rev 
;TOC
;plotSmoother,"mvn_B_1sec_Mag",smoothType=2, newName="B_fitted" 


numDerivative,"B_sec_smoothed" ,1,'mid'

numDerivative,"B_sec_smoothed_back" ,1,'mid'

plotSmoother,"mvn_B_1sec_Mag",smoothType=6,width=20,newName="B20"
numDerivative,"B20",1,"mid" 

end
