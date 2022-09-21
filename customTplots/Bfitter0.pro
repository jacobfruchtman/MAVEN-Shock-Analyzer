pro Bfitter,Debug=Debug

;curveFitter,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted" 




curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted8",/secondOrder,Debug=Debug

;stop


curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted7" ,/doRoll,/secondOrder,Debug=Debug
curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted9" ,/doRoll,/doSecond,/secondOrder,Debug=Debug
curveFitter5,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted10" ,/doRoll,/doSecond,/secondOrder,Debug=Debug
curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted11" ,/doRoll,/doSecond,/secondOrder,/doThird,Debug=Debug
curveFitter5,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted12",/secondOrder ,/doRoll,/doSecond,/doThird,Debug=Debug

curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted3" ,/doRoll;,Debug

curveFitter5,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted6" ,/doRoll,/doSecond,Debug=Debug
curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted5" ,/doRoll,/doSecond,Debug=Debug
curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted4",orderCustom=[0,1,2,3,0],/secondOrder,Debug=Debug
curveFitter5,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted3",orderCustom=[0,1,2,3,0],/secondOrder,Debug=Debug
curveFitter5,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted2",orderCustom=[0,1,2,3,4],/secondOrder,Debug=Debug
curveFitter3,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted1",orderCustom=[0,1,2,3,4],/secondOrder,Debug=Debug
;curveFitter4,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted4"


;curveFitter4,"mvn_B_1sec_MAVEN_MSO_Mag", newName="B_fitted4"


fitCombiner

;fitCombiner2
options,'B_fitted2','colors','r' 
;options,'B_fitted','colors','r' 
options,'B_fitted3','colors','b'                   
options,'B_fitted3*','colors','b'                   
options,'B_fitted4','colors','g'
options,'B_fitted4*','colors','g'
options,'B_fitted5','colors','c'
options,'B_fitted5*','colors','c'
;options,'B_fitted3_inbound','colors','b'
;options,'B_fitted4_inbound','colors','g'
;options,'B_fitted5_inbound','colors','c'
;options,'B_fitted5_inbound','colors','r'

options,'B_fitted6','colors','r'
options,'B_fitted6*','colors','r'

;options,'B_fitted','colors','r' 
options,'B_fitted7','colors','y'
options,'B_fitted7*','colors','y'                   
options,'B_fitted8','colors','m'
options,'B_fitted8*','colors','m'
options,'B_fitted9','colors','g'
options,'B_fitted9*','colors','g'
options,'B_fitted10','colors','c'
options,'B_fitted10*','colors','c'
options,'B_fitted11','colors','m'
options,'B_fitted11*','colors','m'
options,'B_fitted12','colors','b'
options,'B_fitted12*','colors','b'

options,'Franken*','colors','b'

options,'Bmin','colors','y'                   
options,'B60','colors','m'                   
options,'shocks','colors','c' 
;curveFitter5,"B_fitted6", newName="fittest" ,/doRoll,/doSecond,/Debug
store_data,'bf',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted'
;store_data,'bf3',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3'
;store_data,'bf4',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted4'
;store_data,'bf5',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted5'
;store_data,'bf6',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted6'
;store_data,'bf34',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3 B_fitted4'
;store_data,'bf345',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3 B_fitted4 B_fitted5'
;store_data,'bf3456',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3 B_fitted4 B_fitted5 B_fitted6'

;store_data,'bf7',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted7'
;store_data,'bf8',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted8'
;store_data,'bf9',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted9'
;store_data,'bf10',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted10'
;store_data,'bf11',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted11'

;store_data,'bf789',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted7 B_fitted8 B_fitted9'
store_data,'bf789101112',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted7 B_fitted8 B_fitted9 B_fitted10 B_fitted11 B_fitted12'

;store_data,'bf37',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3 B_fitted7'
;store_data,'bf48',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted4 B_fitted8'
;store_data,'bf59',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted5 B_fitted9'
;store_data,'bf610',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted6 B_fitted10'

;store_data,'bf37o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_outbound B_fitted7_outbound'
;store_data,'bf48o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted4_outbound B_fitted8_outbound'
;store_data,'bf59o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted5_outbound B_fitted9_outbound'
;store_data,'bf610o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted6_outbound B_fitted10_outbound'

;store_data,'bf37po',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_outbound B_fitted7_outbound B_fitted3_prime_outbound'
;store_data,'bf48po',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted4_outbound B_fitted8_outbound B_fitted4_prime_outbound'
;store_data,'bf59po',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted5_outbound B_fitted9_outbound B_fitted5_prime_outbound'
;store_data,'bf610po',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted6_outbound B_fitted10_outbound B_fitted6_prime_outbound'

;store_data,'bf345i',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_inbound B_fitted4_inbound B_fitted5_inbound'
;store_data,'bf3456o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_outbound B_fitted4_outbound B_fitted5_outbound B_fitted6_outbound'
;store_data,'bpf3456o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_prime_outbound B_fitted4_prime_outbound B_fitted5_prime_outbound B_fitted6_prime_outbound'
;store_data,'bf78910o',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted7_outbound B_fitted8_outbound B_fitted9_outbound B_fitted10_outbound'
;store_data,'b60f34',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3 B_fitted4 B60'

;secondOrderFitter,'B_fitted3',min=10
;secondOrderFitter,'B_fitted4',min=10
;secondOrderFitter,'B_fitted5',min=10
;secondOrderFitter,'B_fitted6',min=10
;store_data,'bpf345',data='mvn_B_1sec_MAVEN_MSO_Mag B_fitted3_prime B_fitted4_prime B_fitted5_prime'

;store_data,'chi345',data='B_fitted3CHISQ B_fitted4CHISQ B_fitted5CHISQ'
;store_data,'adjchi3456',data='B_fitted3ADJCHISQ B_fitted4ADJCHISQ B_fitted5ADJCHISQ B_fitted6ADJCHISQ'
;store_data,'chi345',data='B_fitted3CHISQ B_fitted4CHISQ B_fitted5CHISQ'
;store_data,'adjchi345',data='B_fitted3ADJCHISQ B_fitted4ADJCHISQ B_fitted5ADJCHISQ'

;store_data,'chi78910',data='B_fitted7CHISQ B_fitted8CHISQ B_fitted9CHISQ B_fitted10CHISQ'

;store_data,'adjchi78910',data='B_fitted7ADJCHISQ B_fitted8ADJCHISQ B_fitted9ADJCHISQ B_fitted10ADJCHISQ'
;store_data,'chi678910',data='B_fitted6CHISQ B_fitted7CHISQ B_fitted8CHISQ B_fitted9CHISQ B_fitted10CHISQ'
;store_data,'adjchi678910',data='B_fitted6ADJCHISQ B_fitted7ADJCHISQ B_fitted8ADJCHISQ B_fitted9ADJCHISQ B_fitted10ADJCHISQ'
;tplot,'bf3456'
;tplot,"*_shocks"
BFITCOMBINER,[3,4,5],suffix='CHISQ'
BFITCOMBINER,[3,4,5],suffix='ADJCHISQ'
BFITCOMBINER,[3,4,5,6],suffix='CHISQ'
BFITCOMBINER,[3,4,5,6],suffix='ADJCHISQ'

BFITCOMBINER,[7,8,9,10,11],suffix='CHISQ'
BFITCOMBINER,[7,8,9,10,11],suffix='ADJCHISQ'
BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='CHISQ'
BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='ADJCHISQ'
;BFITCOMBINER,[1,2,3,5,11]
BFITCOMBINER,[4,6,7,8,9,10,11,12]
BFITCOMBINER,[6,10,12]
BFITCOMBINER,[5,9,11]
BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='shocks'
BFITCOMBINER,[3,4,5],suffix='shocks'
BFITCOMBINER,[9]
tplot,['shocks6789101112','bf6789101112','CHISQ6789101112','shocks','bff']

;tplot,['bf37','bf48','bf5911','bf61012','bpf345','outnums'],/add
end

