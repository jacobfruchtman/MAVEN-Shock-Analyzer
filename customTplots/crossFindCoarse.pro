function arrProd, arr,beg,en
p=1
For i=beg,en Do p=p*arr[i]

return,p
end

pro crossFindCoarse,step, md

get_data,'mvn_swica_density',data=dat0,alim=limD

z=0*dat0.y

numDerivative,'mvn_swica_density',step,md

numDerivative,'mvn_swica_densityderiv',step,md

absPlot, 'mvn_swica_densityderiv'
absPlot, 'mvn_swica_densityderivderiv'
get_data,'mvn_swica_densityderivabs',data=dat1,alim=limD
dz=z
get_data,'mvn_swica_densityderivderivabs',data=dat2,alim=limD
z[where(dat1.Y GT 2, /Null)]=1

dz[where(dat2.Y GT 0.4, /Null)]=1
get_data,'mvn_swim_quality_flag',data=datf

isCross=z[0]
isCross=0
dat0.y=z
print,size(z)
dat0.ytitle="Shock"+STRING(10B)+"flag"
store_data,'ShockFlag0Coarse',data=dat0

dat2.y=dz
print,size(z)
dat2.ytitle="Shock"+STRING(10B)+"flag"
store_data,'ShockFlag2Coarse',data=dat2

print,'hello'
n=Size(z,/N_ELEMENTS)-1
print,n
print,n-5
w=z
ww=z

dw=dz
print,max(z)
print,isCross
chng=z
for i=5,n-5 Do BEGIN
	chng[i]=0
	;print,"entered for loop"
;	ww[i]=z[i-4]*z[i+4]*z[i+7]
	;ww[i]=arrProd
	;print,i
	k=min([i+20,N-5])
	;print,"k=",k
	if (i EQ 5)  then print, i,z[i] 
	if (dw[i-1]*dw[i+1] EQ 1) AND (dw[i] EQ 0) THEN dw[i]=1
	if(isCross EQ 0) AND (z[i] EQ 1) THEN BEGIN 
		;print,"crossed?"
		if (z[i+1] EQ 1) AND (mean(z[i:k]) GT 0.5)THEN BEGIN
			;print,"crossed"
			isCross=1
			chng[i]=1
		endif
		if (isCross  EQ 0) THEN w[i]=arrProd(z,i-4,i+4)
	endif
	if(isCross EQ 1) AND (z[i] EQ 0) THEN BEGIN 
		if (z[i+10] EQ 0) AND (mean(z[i:k]) LT 0.5) THEN BEGIN
			isCross=0
			chng[i]=1
		endif
		if (isCross) AND(z[i-1]*z[i+1] EQ 1) THEN BEGIN
			z[i]=1;w[i]=1-arrProd(1-z,i,min([i+40,n-5]))
			;w[i]=1
			
		endif
	endif
	;print,isCross
	w[i]=isCross+(1-isCross)*.25
	;if(z[i] EQ 0) THEN w[i]=z[i-4]*z[i+4]

ENDFOR
dat0.y=w
print,size(w)
dat0.ytitle="Shock"+STRING(10B)+"flag1"
store_data,'ShockFlag1Coarse',data=dat0

print,max(chng)
dat0.y=chng
print,size(w)
dat0.ytitle="Shock"+STRING(10B)+"cross0"
store_data,'Shockcross0Coarse',data=dat0

newdat={x:dat0.x,y:[[dw],[dw]],v:[0,1]}
print,SIZE(newdat,/TYPE,/L64)
M = [0,1]
store_data,'shocktestflag',data = newdat;NEWNAME='shocktestflag'
dat2.y=dw
print,size(w)
print,mean(dw)
dat2.ytitle="Shock"+STRING(10B)+"flag3"
store_data,'ShockFlag3Coarse',data=dat2
options,'shocktestflag','spec',1	;switch from line plots to spectrogram mode
options,'shocktestflag','no_interp',1
END
