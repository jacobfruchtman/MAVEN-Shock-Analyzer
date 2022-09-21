;RH_parameters,Mfms[i],ThetaNBn[i],betas[i],a,b,c,d,yy,delta
;densityjump_RH[i]=1/yy
;B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
;beta2[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

function rh_density_jump,M=M,bta=bta,theta=theta,a=a,b=b,c=c,d=d,yy=yy,delta=delta

	if keyword_set(M) then RH_parameters,M,theta,bta,a,b,c,d,yy,delta

	return,1/yy
end
