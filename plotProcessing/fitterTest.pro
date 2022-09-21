
pro fitterTest

	get_data,'B_inbound',data=datin                                                  
	yin=datin.y                                                                      
	;NNN=where(finite(yin,/NAN) eq 0,count)
	yin[where(finite(yin) eq 0,count)]=0
	inf=yin*0    
	inb=yin*0                                                                    
	for i=1,size(inf,/n_el)-1 do if(yin[i] ne 0) and (yin[i-1] eq 0) then inf[i]=1  
	for i=0,size(inf,/n_el)-2 do if(yin[i] ne 0) and (yin[i+1] eq 0) then inb[i]=1    
	store_data,'inbound_enter',data={x:datin.x,y:inf}
	store_data,'inbound_exit',data={x:datin.x,y:inb}

          

	xin=datin.x
	HH=where(inf ne 0,count)
    in0=yin*0 
	GG=where(inb ne 0,count)
	for i=0, count-1 do begin
		yp=yin[HH[i]:GG[i]]
		xp=xin[HH[i]:GG[i]]
		xpa=xp-xin[0]


		

		s=size(yp,/n_el)

 
		mn=mean(yp[0:s/10])        
		mx=mean(yp[9*s/10:s-1])
		a0=(mx-mn)/2
		a3=(mx+mn)/2
	              
  
		a2=xpa[mean([HH[i],GG[i]])-HH[i]]

		a1=1
 		AA=[a0,a1,a2,a3]
   		print,"to zeroth order, guess that AA=",AA

  		weight=1.0/yp
		weight[where(finite(weight) eq 0)]=0
   		;yfit seems to sometimes give garbage out, so will just use it to calculate AA and then calculate directly

   		yfit=CURVEFIT(xpa, yp, weight, AA, SIGMA, FUNCTION_NAME=curveType)

   		PRINT, 'Function parameters: ', AA
		yin2=yin
		in0[HH[i]:GG[i]]=yfit
	endfor
	store_data,'inbound_fit0',data={x:datin.x,y:in0}      
end
