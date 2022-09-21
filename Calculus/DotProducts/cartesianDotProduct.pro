
PRO cartesianDotProduct, var1, var2
get_data,'mvn_swica_velocity',data=dat,alim=limV
get_data,var1,data=dat1,alim=lim1
get_data,var2,data=dat2,alim=lim2

v1=dat1.y
v2=dat2.y

v3x=transpose(v1[*,0]*v2[*,0])
v3y=transpose(v1[*,1]*v2[*,1])
v3z=transpose(v1[*,2]*v2[*,2])

v3=transpose([v3x,v3y,v3z])

yName=""

READ, yNAME, PROMPT='Enter the yaxis label:'

dat.Y=v3
dat.ytitle=yName

plotName=var1+"_dot_"+v2

store_data, plotName,data=dat

END
