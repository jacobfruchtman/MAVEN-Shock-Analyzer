pro Bfitter,Debug=Debug,allow=allow
	;tplot_element,"dateDataCurrent","finished",0,/add
	del_data,"B_fitte*"
	clockB=TIC("Bfitter")
	get_data,"mvn_B_1sec_Mag",data=dat
	xs=dat.x
	z=xs*0.0
	x0=xs[0]
	N=numel(xs)

	insublengths=list()
	outsublengths=list()

	brokenin=list()
	brokenout=list()

	inSubsets=xs*0.0;-1
	inends=xs*0.0;-1
	inbegs=xs*0.0;-1

	outSubsets=xs*0.0;-1
	outends=xs*0.0;-1
	outbegs=xs*0.0;-1

	debugout=xs*0.0-1


	inimaxs=xs*0.0-1
	outimaxs=xs*0.0-1

	inimins=xs*0.0-1
	outimins=xs*0.0-1

	inchis=xs*0.0+999;+0
	outchis=xs*0.0+999;+0
	chis=xs*0.0+999

	innums=xs*0.0-1
	outnums=xs*0.0-1
	
	inshocklocs=xs*0.0-1
	outshocklocs=xs*0.0-1
	
	inups=xs*0.0
	indowns=xs*0.0

	outups=xs*0.0
	outdowns=xs*0.0


	shocks=xs*0.0
datumin={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inups:inups,indowns:indowns}
datumout={x:xs,y:z,ytitle:"Magnetic Field (nT)", outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,outups:outups,outdowns:outdowns}
datchiiin={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
datchiout={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
datchi={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
datShock={x:xs,y:shocks,ytitle:'flag'}
datum={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}

store_data,"shocks",data=datShock
store_data,"shocks_inbound",data=datShock
store_data,"shocks_outbound",data=datShock
;for i=0, 16 do store_data,"B_fitted"+strtrim(i,2)+"_inbound",data=datumin
;for i=0, 16 do store_data,"B_fitted"+strtrim(i,2)+"_outbound",data=datumout
;for i=0, 16 do store_data,"B_fitted"+strtrim(i,2),data=datum
;curveFitter,"mvn_B_1sec_Mag", newName="B_fitted" 

numDerivative,"B_sec_smoothed_back",1,"mid" 
numDerivative,"B_sec_smoothed",1,"mid" 
dt=x2greg(xs[0],/str)


nm="B_fitted0"
error_status=0
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			;if 'Illegal variable attribute: Y.' eq !ERROR_STATE.MSG or error_status eq -981 THEN EXIT
			error_status=0
			catch,/cancel
endif

;stop
curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted0" ,/secondOrder,/debug,allow=allow;,Debug;curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted0" ,orderCustom=[1,2],/secondOrder;,Debug
nm="B_fitted1"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
;curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted1",orderCustom=[-1,1,2,5,3],/secondOrder,Debug=Debug
curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted1",/secondOrder,Debug=Debug,orderCustom=[1,2,4,5,3]
nm="B_fitted2"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
;curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted2",orderCustom=[-1,1,2,5,3],/secondOrder,Debug=Debug
curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted2",orderCustom=[-1,1,2,3],Debug=Debug

nm="B_fitted3"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
;curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted3",orderCustom=[-1,1,2,3],/secondOrder,Debug=Debug
nm="B_fitted4"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted4",orderCustom=[-1,1,2,3],/secondOrder,Debug=Debug
nm="B_fitted5"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
;curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted5" ,/doRoll,/doSecond,Debug=Debug

curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted5" ,/doRoll,/doSecond,Debug=Debug
nm="B_fitted6"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif


curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted6" ,/doRoll,/doSecond,Debug=Debug,allow=allow
nm="B_fitted7"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted7" ,/doRoll,/secondOrder,Debug=Debug
nm="B_fitted8"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted8",/secondOrder,Debug=Debug
nm="B_fitted9"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted9" ,/doRoll,/doSecond,/secondOrder,Debug=Debug
nm="B_fitted10"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted10" ,/doRoll,/doSecond,/secondOrder,Debug=Debug,allow=allow
nm="B_fitted11"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted11" ,/secondOrder,Debug=Debug,orderCustom=[1,2,4,5,3];,/doRoll,/doSecond,/secondOrder,/doThird,Debug=Debug
nm="B_fitted12"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted12",/secondOrder,Debug=Debug,orderCustom=[1,2,4,5,3],allow=allow; ,/doRoll,/doSecond,/doThird
nm="B_fitted13"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter3,"mvn_B_1sec_Mag", newName="B_fitted13" ,orderCustom=[1,3],/secondOrder;,Debug
nm="B_fitted14"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted14" ,orderCustom=[1,3],/secondOrder;,Debug
nm="B_fitted15"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

;curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted15" ,orderCustom=[1,6],/secondOrder;,Debug
nm="B_fitted16"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif

curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted16" ,orderCustom=[1,6,2],/secondOrder,allow=allow;,Debug

nm="B_fitted17"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"
			store_data,nm+"_inbound",data=datumin
			store_data,nm+"_outbound",data=datumout
			store_data,nm,data=datum
			error_status=0
			catch,/cancel
endif
curveFitter5,"mvn_B_1sec_Mag", newName="B_fitted17" ,orderCustom=[1,6,2],/secondOrder
;curveFitter6,"mvn_B_1sec_Mag", newName="B_fitted17" ,orderCustom=[1,6,2],/secondOrder;,Debug
;curveFitter6,"mvn_B_1sec_Mag", newName=nm,/secondOrder,/shocktest,/allownonfore,orderCustom=[-1];,Debug

;get_data,"B_fitted16",data=datB16
;get_data,"B_fitted17",data=datB17

;B16=datB16.y
;B17=datB17.y

;K=array_equal(B16,B17)
;print,K






;curveFitter4,"mvn_B_1sec_Mag", newName="B_fitted4"


;curveFitter4,"mvn_B_1sec_Mag", newName="B_fitted4"


;fitCombiner
nm="fitCombiner2"
catch,error_status
if error_status ne 0 then begin
			errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON "+nm]
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			PRINT,"FAILED ON "+nm
			errorsaver,x0,errmsg
			PRINT,"setting to null result"

			error_status=0
			catch,/cancel
			return
endif
fitCombiner2
;if 0 then begin
options,'B_fitted2','colors','r' 
options,'B_fitted','colors','r' 
options,'B_fitted3','colors','b'                   
options,'B_fitted3*','colors','b'                   
options,'B_fitted4','colors','g'
options,'B_fitted4*','colors','g'
options,'B_fitted5','colors','c'
options,'B_fitted5*','colors','c'
options,'B_fitted3_inbound','colors','b'
options,'B_fitted4_inbound','colors','g'
options,'B_fitted5_inbound','colors','c'
options,'B_fitted5_inbound','colors','r'

options,'B_fitted6','colors','r'
options,'B_fitted6*','colors','r'
options,'B_fitted16','colors','y'
options,'B_fitted16*','colors','y'
options,'B_fitted17*','colors','b'
options,'B_fitted','colors','r' 
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

;options,'Bmin','colors','y'                   
;options,'B60','colors','m'                   
;options,'shocks','colors','c' 
;endif
TOC,clockB
;curveFitter5,"B_fitted6", newName="fittest" ,/doRoll,/doSecond,/Debug
;store_data,'bf',data='mvn_B_1sec_Mag B_fitted'
;store_data,'bf3',data='mvn_B_1sec_Mag B_fitted3'
;store_data,'bf4',data='mvn_B_1sec_Mag B_fitted4'
;store_data,'bf5',data='mvn_B_1sec_Mag B_fitted5'
;store_data,'bf6',data='mvn_B_1sec_Mag B_fitted6'
;store_data,'bf34',data='mvn_B_1sec_Mag B_fitted3 B_fitted4'
;store_data,'bf345',data='mvn_B_1sec_Mag B_fitted3 B_fitted4 B_fitted5'
;store_data,'bf3456',data='mvn_B_1sec_Mag B_fitted3 B_fitted4 B_fitted5 B_fitted6'

;store_data,'bf7',data='mvn_B_1sec_Mag B_fitted7'
;store_data,'bf8',data='mvn_B_1sec_Mag B_fitted8'
;store_data,'bf9',data='mvn_B_1sec_Mag B_fitted9'
;store_data,'bf10',data='mvn_B_1sec_Mag B_fitted10'
;store_data,'bf11',data='mvn_B_1sec_Mag B_fitted11'

;store_data,'bf789',data='mvn_B_1sec_Mag B_fitted7 B_fitted8 B_fitted9'
store_data,'bf789101112',data='mvn_B_1sec_Mag B_fitted7 B_fitted8 B_fitted9 B_fitted10 B_fitted11 B_fitted12'

;store_data,'bf37',data='mvn_B_1sec_Mag B_fitted3 B_fitted7'
;store_data,'bf48',data='mvn_B_1sec_Mag B_fitted4 B_fitted8'
;store_data,'bf59',data='mvn_B_1sec_Mag B_fitted5 B_fitted9'
;store_data,'bf610',data='mvn_B_1sec_Mag B_fitted6 B_fitted10'

;store_data,'bf37o',data='mvn_B_1sec_Mag B_fitted3_outbound B_fitted7_outbound'
;store_data,'bf48o',data='mvn_B_1sec_Mag B_fitted4_outbound B_fitted8_outbound'
;store_data,'bf59o',data='mvn_B_1sec_Mag B_fitted5_outbound B_fitted9_outbound'
;store_data,'bf610o',data='mvn_B_1sec_Mag B_fitted6_outbound B_fitted10_outbound'

;store_data,'bf37po',data='mvn_B_1sec_Mag B_fitted3_outbound B_fitted7_outbound B_fitted3_prime_outbound'
;store_data,'bf48po',data='mvn_B_1sec_Mag B_fitted4_outbound B_fitted8_outbound B_fitted4_prime_outbound'
;store_data,'bf59po',data='mvn_B_1sec_Mag B_fitted5_outbound B_fitted9_outbound B_fitted5_prime_outbound'
;store_data,'bf610po',data='mvn_B_1sec_Mag B_fitted6_outbound B_fitted10_outbound B_fitted6_prime_outbound'

;store_data,'bf345i',data='mvn_B_1sec_Mag B_fitted3_inbound B_fitted4_inbound B_fitted5_inbound'
;store_data,'bf3456o',data='mvn_B_1sec_Mag B_fitted3_outbound B_fitted4_outbound B_fitted5_outbound B_fitted6_outbound'
;store_data,'bpf3456o',data='mvn_B_1sec_Mag B_fitted3_prime_outbound B_fitted4_prime_outbound B_fitted5_prime_outbound B_fitted6_prime_outbound'
;store_data,'bf78910o',data='mvn_B_1sec_Mag B_fitted7_outbound B_fitted8_outbound B_fitted9_outbound B_fitted10_outbound'
;store_data,'b60f34',data='mvn_B_1sec_Mag B_fitted3 B_fitted4 B60'

;secondOrderFitter,'B_fitted3',min=10
;secondOrderFitter,'B_fitted4',min=10
;secondOrderFitter,'B_fitted5',min=10
;secondOrderFitter,'B_fitted6',min=10
;store_data,'bpf345',data='mvn_B_1sec_Mag B_fitted3_prime B_fitted4_prime B_fitted5_prime'

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
;BFITCOMBINER,[3,4,5],suffix='CHISQ'
;BFITCOMBINER,[3,4,5],suffix='ADJCHISQ'
;BFITCOMBINER,[3,4,5,6],suffix='CHISQ'
;BFITCOMBINER,[3,4,5,6],suffix='ADJCHISQ'

;BFITCOMBINER,[7,8,9,10,11],suffix='CHISQ'
;BFITCOMBINER,[7,8,9,10,11],suffix='ADJCHISQ'
;BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='CHISQ'
;BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='ADJCHISQ'
;BFITCOMBINER,[1,2,3,12,13,14]
;BFITCOMBINER,[4,6,7,8,9,10,11,12]
;BFITCOMBINER,[4,6,7,8,9,10,11,12],io='i'
;BFITCOMBINER,[6,10,12]
;BFITCOMBINER,[5,9,11]
;BFITCOMBINER,[13,14,16]
BFITCOMBINER,[4,6,7,8,9,10,11,12],suffix='shocks'
;BFITCOMBINER,[3,4,5],suffix='shocks'
;BFITCOMBINER,[9]
;tplot,['shocks6789101112','bf46789101112','CHISQ46789101112','shocks','bff']
		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout


		if total(datin.y + datout.y) eq 0 then return
downsmoothdev
downsmoothdev,plt="B_3sec",newName="B3"
downsmoothdev,plt="B_10sec",newName="B10"
flagclean2,'B10diffRegion' 
;flagclean2,'B3diffRegion' 
;calc,"'mmdevdiffRegion_End'= 'mmdevdiffRegion'*'sublengths_end'"
endornanplot,'mmdevdiffRegion'
endornanplot,'B3diffRegion'
flagclean2,'mmdevdiffRegion_End' 
flagclean2,'B3diffRegion_End' 
flagclean2,'mmdevdiffRegion' 
onofflengthplot,'mmdevdiffRegion_End_cleaned',newName="mmdblocksize" 
onofflengthplot,'B10diffRegion',newName="B10blocksize" 
onofflengthplot,'B3diffRegion',newName="B3blocksize" 
threshFlag, "B3blocksize" ,0,kindThresh=13,mnt=-1200,newName="B3blockNeg",/true
threshFlag, "B10blocksize" ,0,kindThresh=13,mnt=-600,newName="B10blockNeg",/true
threshFlag, "mmdblocksize" ,0,kindThresh=13,mnt=-1200,newName="mmdblockNeg",/true
extremablock,'B10diff',"B10blockNeg_trigger"
;tplot_element,"dateDataCurrent","finished",1,/add
;tplot,['bf37','bf48','bf5911','bf61012','bpf345','outnums'],/add
end

