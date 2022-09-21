pro downsmoothdev,plt=plt,newName=newName,ytname=ytname
get_data,"downstream_fit",data=datdown
get_data,"upstream_fit",data=datup
;get_data,"B_meanMode_smoothed",data=datmm
if not keyword_set(plt) then plt='B_meanMode_smoothed';mvn_B_1sec_MAVEN_MSO_Mag_modal_mean'

get_data,plt,data=datmmm

if not keyword_set(newName) then newName='mmdev'


;get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
;bmm=datmm.y
down=datdown.y
up=datup.y
mmm=datmmm.y
;B=datB.y
xs=datmmm.x
N=numel(xs)
NAN=!VALUES.F_NAN
z=nanarr(N)
z2=z
w=z
w2=z

u=z
u2=z
for i=0 ,N-1 do if down[i] eq 0 then down[i]=NAN
for i=0 ,N-1 do if up[i] eq 0 then up[i]=NAN
;ss=where(finite(bmm) ne 1,nnan)
;print,nnan

;for i=0,N-1 do if bmm[i] ne 0 and down[i] ne 0 and finite(bmm[i]) and finite(down[i]) then z2[i]=bmm[i]-down[i] else z2[i]=NAN
for i=0,N-1 do if mmm[i] ne 0 and down[i] ne 0 and finite(mmm[i]) and finite(down[i]) then z[i]=mmm[i]-down[i] else z[i]=NAN

;for i=0,N-1 do if bmm[i] ne 0 and up[i] ne 0 and finite(bmm[i]) and finite(up[i]) then w2[i]=bmm[i]-up[i] else w2[i]=NAN
for i=0,N-1 do if mmm[i] ne 0 and up[i] ne 0 and finite(mmm[i]) and finite(up[i]) then w[i]=mmm[i]-up[i] else w[i]=NAN


for i=0,N-1 do if  finite(z[i]) and finite(w[i]) then u[i]=abs(w[i])-abs(z[i]) else u[i]=NAN
;for i=0,N-1 do if  finite(z2[i]) and finite(w2[i]) then u2[i]=abs(w2[i])-abs(z2[i]) else u2[i]=NAN

reg=nanarr(N)
;sreg=reg

for i=0,N-1 do if finite(u[i]) eq 1 then reg[i]=1*(u[i] gt 0.0) 
;for i=0,N-1 do if finite(u2[i]) then sreg[i]=1*(u2[i] gt 0)
;calc,"'downmmdev'='B_meanMode_smoothed'-'downstream_fit'"

ylist=list(z,w,u,reg)
prelist=list("down","up","diff","diffRegion")
nmlist=list()
ytlist=list()
for k=0,3 do begin

	yy=ylist[k]
	pre=prelist[k]
	if k lt 2 then fullname=pre+newName else fullname=newName+pre
	if not keyword_set(ytname) then yt=datmmm.ytitle else yt=fullname;nmlist[k]
	store_data,fullname,data={x:xs,y:yy,ytitle:yt}
	if k lt 3 then plotSmoother,fullname,smoothType=6,width=5,newName=fullname+"smoothed"
	nmlist.add,fullname
	ytlist.add,yt	
endfor


options,nmlist[3],'colors','r'

get_data,nmlist[2]+"smoothed",data=datus ;this is the smoothed version of u, above  (absolute difference between y and up minus abs diff between y and down)
us=datus.y

sreg=nanarr(N)

for i=0,N-1 do if finite(us[i]) eq 1 then sreg[i]=1*(us[i] gt 0.0) 
	name=nmlist[2]+"SRegion"
	if not keyword_set(ytname) then yt=datus.ytitle else yt=name
	store_data,name,data={x:xs,y:sreg,ytitle:yt}
options,name,'colors','y'
	;store_data,"down"+newName,data={x:xs,y:z,ytitle:"down"+newName}
;endif
;plotSmoother,"down"+newName,smoothType=6,width=5,newName="down"+newName+"smoothed"
;store_data,"downModedev",data={x:xs,y:z2,ytitle:"downModedev"}

;plotSmoother,'downModedev',smoothType=6,width=5,newName="downModedevsmoothed"


;store_data,"up"+newName,data={x:xs,y:w,ytitle:"up"+newName}

;plotSmoother,'upmmdev',smoothType=6,width=5,newName="updevsmoothed"
;store_data,"upModedev",data={x:xs,y:w2,ytitle:"upModedev"}

;plotSmoother,'upModedev',smoothType=6,width=5,newName="upModedevsmoothed"


;store_data,"mmdevdiff",data={x:xs,y:u,ytitle:"mmdevdiff"}

;plotSmoother,'mmdevdiff',smoothType=6,width=5,newName="devdiffsmoothed"
;store_data,"Modedevdiff",data={x:xs,y:u2,ytitle:"Modedevdiff"}

;plotSmoother,'Modedevdiff',smoothType=6,width=5,newName="Modedevdiffsmoothed"

;store_data,"mmdevdiffRegion",data={x:xs,y:reg,ytitle:"mm closer to !C down than up?"}
;store_data,"ModedevdiffRegion",data={x:xs,y:sreg,ytitle:"Mode closer !C to down !C than up?"}
end
