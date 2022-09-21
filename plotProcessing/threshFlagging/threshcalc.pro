function threshcalc,x,t,a,b,typ,truesave=truesave,first=first


	;if NOT KEYWORD_SET(t2) THEN t2=0
	;if NOT KEYWORD_SET(t3) THEN t3=0
	;if NOT KEYWORD_SET(kindThresh) THEN typ=0	
	if keyword_set(first) then begin
		if not keyword_set(truesave)  then print,"not set" else print,"set"

	endif
	if not keyword_set(truesave) then AM=1.0 else AM=abs(x)
	if keyword_set(first) then print,"AM,AM*truestuff=", AM,AM*(((x lt t) and(x gt a)) or ((x gt t) and(x lt a)))
	CASE typ of
	0: return, AM*(x GT t);
	1: return, AM*(x Ge t);
	2: return, AM*(x lt t);
	3:  return, AM*(x le t);
	4: return, AM*(x eq t);
	5: return, 1*((x eq t) and (a ne t));hit
	6: return, 1*((x eq t) and (a ne t));leave	
	7:return, 1*(((x eq t) and (a eq t))or ((x eq t) and (b eq t))) 
	;orT
		
	8: return, 1*((x eq t) and  ((a ne t) or (b ne t)))
	;orF

	9:  return, 1*((x eq t) and  ((a eq t) and (b eq t)))
	;T_and
	10: return, 1*((x eq t) and  ((a ne t) and (b ne t)))
	;F_and
	11: return, 1*(((x eq t) and (a eq t))xor ((x eq t) and (b eq t))) ;xor
	12: return, AM*(((x le t) and(x ge a)) or ((x ge t) and(x le a))) ;closed set
	13: return, AM*(((x lt t) and(x gt a)) or ((x gt t) and(x lt a))) ;open set
	14: return, 1*((a eq t) and (x eq t)) ;unmoved
	15: return, 1*((b eq t) and (x eq t)) ;wont move
	16: return, 1*((x eq t) and (((a  lt t )and (b gt t))or ((a gt t)and( b lt t ))))
 ;pass
	17: return, 1*((x eq t) and ((a  lt t)and (b gt t))) ;ascend
	18: return, 1*((x eq t) and ((a  gt t)and (b lt t))) ;descend
	19: return, 1*((x eq t) and (b lt t)) ;drop
	20: return, 1*((x eq t) and (b gt t)) ;rise
	21: return, 1*((x eq t) and (a  gt t)) ;fell
	22: return, 1*((x eq t) and (a  lt t)) ;rose

	24: return, 1*((x lt t)and(a lt t)and(b lt a))

	26: return, AM*( ((x  gt a)and (x gt b))) ;max
	27: return, AM*( ((x  lt a)and (x lt b))) ;min
	28: return, AM*( (  (x ge abs(t)) and   (b le -abs(t)) ) xor ((x le -abs(t)) and   (a ge abs(t))  ))
	29: return, AM*( (  (x le -abs(t)) and   (b ge abs(t)) ) xor ((x ge abs(t)) and   (a le -abs(t))  ))
	ENDCASE

	return, AM*(x eq t)
end
