pro electronsetup,quick=quick
get_data,'mvn_swe_spec_dens',data=datde
get_data,'mvn_swe_spec_temp',data=datte

dox=datde.x
tox=datte.x
;print,array_equal(dox,tox)
d_e=datde.y
t_e=datte.y
for i=0,numel(dox)-1 do if d_e[i] lt 0 then d_e[i]= !values.f_nan
for i=0,numel(dox)-1 do if t_e[i] lt 0 then t_e[i]= !values.f_nan
datde.y=d_e
datte.y=t_e
store_data,'mvn_swe_spec_dens',data=datde
store_data,'mvn_swe_spec_temp',data=datte
p_e=d_e*t_e
store_data,"mvn_swe_spec_pressure_electron",data={x:dox,y:p_e,ytitle:'Pressure !C [eV/cm^3]'}
tplot_element,'mvn_swe_spec_pressure_electron','ytitle','Pressure !C [eV/cm^3]',/add
options,'mvn_swe_spec_*','interpol',1
interpolator2,'mvn_swe_spec_temp';,/yonly
interpolator2,'mvn_swe_spec_dens';,/yonly

get_data,'mvn_swe_spec_dens_interpolated',data=datde
d_e=datde.y
for i=0,numel(d_e)-1 do if d_e[i] lt 0 then d_e[i]= !values.f_nan
datde.y=d_e
store_data,'mvn_swe_spec_dens_interpolated',data=datde

get_data,'mvn_swe_spec_temp_interpolated',data=datte
t_e=datte.y
for i=0,numel(t_e)-1 do if t_e[i] lt 0 then t_e[i]= !values.f_nan
datte.y=t_e
store_data,'mvn_swe_spec_temp_interpolated',data=datte





;calc,'"mvn_pressure_electron"="mvn_swe_spec_dens_interpolated"*"mvn_swe_spec_temp_interpolated"'
interpolator,'mvn_swe_spec_pressure_electron',newName='mvn_pressure_electron'

if keyword_set(quick) then return

totalfiniteplot,'mvn_swe_spec_dens_interpolated',30,newName='swe_density_real'
plotSmoother,"mvn_pressure_electron",smoothType=1,width=30,newName="PE_half" 
plotSmoother,"mvn_swe_spec_dens_interpolated",smoothType=1,width=30,newName="ne_half" 
plotSmoother,"mvn_swe_spec_temp_interpolated",smoothType=1,width=30,newName="te_half" 


end
