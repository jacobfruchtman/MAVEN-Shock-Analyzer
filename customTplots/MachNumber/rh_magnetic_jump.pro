;RH_parameters,Mfms[i],ThetaNBn[i],betas[i],a,b,c,d,yy,delta
;densityjump_RH[i]=1/yy
;B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
;beta2[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

function rh_magnetic_jump,Mfms=Mfms,bta=bta,theta=theta,aa=aa,bb=bb,cc=cc,dd=dd,yy=yy,delta=delta,beta_e=beta_e,beta_i=beta_i,g_e=g_e,MA=MA,Msound=Msound,U1n=Uin,Sound=Sound,Alfven=Alfven

	if keyword_set(Mfms) then RH_parameters,Mfms,theta,bta,aa,bb,cc,dd,yy,delta,beta_e=beta_e,beta_i=beta_i,g_e=g_e,MA=MA,Msound=Msound,U1n=Uin,Sound=Sound,Alfven=Alfven

	return,SQRT((bb+cc*delta)/(bb+cc))
end
