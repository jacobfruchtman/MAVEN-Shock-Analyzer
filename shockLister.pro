

function shockLister,fileName=fileName,startDate=startDate
	if NOT KEYWORD_SET(startDate) THEN startDate=""
	if NOT KEYWORD_SET(fileName) THEN fileName="crossings-"+startDate+".csv"
;In our initial implentation, all the columns will come from these tplots here. Will need to add to it as I go on

plts=['mvn_B_1sec_Mag','B_fitted''mvn_swica_density','alt','lon','lat','mach_fms_coarse']

get_data, 'mvn_B_1sec_Mag',data=dataB
get_data, 'mvn_B_1sec_MAVEN_MSO',data=dataBv
get_data, 'B_fitted3',data=dataBF
get_data, 'mvn_swica_density',data=dataDen
get_data, 'alt',data=dataAlt
get_data, 'lon',data=dataLon
get_data, 'lat',data=dataLat
get_data, 'mach_fms_coarse',data=dataMfms
get_data,'Shock_Normal',data=dataN
get_data,'Shock_Angle',data=dataA

;These arrrays are not all the same size. We will need to  interpolate the smaller ones to be the sizes of the larger ones so that the x values line up with the ones at the shock crossing trigger points

datsX=[Ptr_New(dataB.x, /No_Copy), $
		Ptr_New(dataBv.x, /No_Copy), $
		Ptr_New(dataBF.x, /No_Copy), $
		Ptr_New(dataMfms.x, /No_Copy),$
		;Ptr_New(dataN.x, /No_Copy),$
		;Ptr_New(dataA.x, /No_Copy),$
		Ptr_New(dataDen.x, /No_Copy), $
		Ptr_New(dataAlt.x, /No_Copy), $
		Ptr_New(dataLon.x, /No_Copy), $
		Ptr_New(dataLat.x, /No_Copy)]

olddatsY=[Ptr_New(dataB.y, /No_Copy), $
		Ptr_New(dataBv.y, /No_Copy), $
		Ptr_New(dataBF.y, /No_Copy), $
		Ptr_New(dataMfms.y, /No_Copy),$
		;Ptr_New(dataN.y, /No_Copy),$
		;Ptr_New(dataA.y, /No_Copy),$
		Ptr_New(dataDen.y, /No_Copy), $
		Ptr_New(dataAlt.y, /No_Copy), $
		Ptr_New(dataLon.y, /No_Copy), $
		Ptr_New(dataLat.y, /No_Copy)]

;datsX=[dataB.x,dataBF.x,dataDen.x,dataAlt.x,dataLon.x,dataLat.x,dataMfms.x] ;this doesn't work
;datsY=[dataB.y,dataBF.y,dataDen.y,dataAlt.y,dataLon.y,dataLat.y,dataMfms.y]
print,size(datsX)

;there's probably a more efficient means of implementing this. Probably involves FOREACH. Need to come back to it later. 

szB=size(dataB.x,/n_el)
szBF=size(dataBF.x,/n_el)
szDen=size(dataDen.x,/n_el)
szAlt=size(dataAlt.x,/n_el)
szLon=size(dataLon.x,/n_el)
szLat=size(dataLat.x,/n_el)
szMfms=size(dataMfms.x,/n_el)

szs=[szB,szBF,szDen,szAlt,szLon,szLat,szMfms]
bigX=0
B=where(szs lt max(szs),count,complement=nB,nComplement=ncount)
print,"count=",count
print,"ncount=",ncount
print,"B="
print,B
print,"szs(B)="
print,szs(B)
print,"nB="
print,nB
print,"szs(nB)="
print,szs(nB)
;listLength=count+ncount
nel=size(datsX,/n_el)
len=0
for i=0,nel-1 do len+= arrayDims(*olddatsY[i])
listLength=len
bigX=*datsX[nB[0]] 
msz=size(bigX,/n_el)
foreach el,B do begin
	print,"el=",el	
	print,size(*olddatsY[el])
	*olddatsY[el]=interpol(*olddatsY[el],*datsX[el],bigX)
endforeach

;fltarr works as fltarr('length of arrays','number of arrays)
datsY=fltarr(msz,listLength); we add +1 because first array will be the 'x' array. Later we may edit this so that [1] will be humn readable time.

;datsY[0]=bigX

for i=0,listLength-1 do begin

	 	datsY[*,i]=*olddatsY[i]
endfor
print,size(datsY)


threshFlag, 'B_fittedderiv',10000,kindThresh=26
get_data,'B_fittedderiv_flag',data=datFlag
xflag=datFlag.x
dw=datFlag.y[*,0]
dw0=dw
if(size(dw,/n_el) lt msz) then dw=interpol(dw0,xflag,bigX)

BB = WHERE(dw ne 0, count, COMPLEMENT=B_C, NCOMPLEMENT=count_c);new array with locations of nonzero elements

cX=bigX[BB]
cB=datsY[BB,0]
cBF=datsY[BB,1]
cDen=datsY[BB,2]
cAlt=datsY[BB,3]
cLon=datsY[BB,4]
cLat=datsY[BB,5]
cMfms=datsY[BB,6]
headers=['crossX','crossB','crossBFitted','crossDen','crossAlt','crossLon','crossLat','crossMfms']
crossings={crossX:cX,crossB:cB,crossBFitted:cBF,$
crossDen:cDen,crossAlt:cAlt,crossLon:cLon,crossLat:cLat,crossMfms:cMfms}

WRITE_CSV,fileName,crossings,header=headers

return,crossings
end
