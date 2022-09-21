function derivator, y, step, type,x=x

if(type EQ "0") OR (type EQ "start") THEN BEGIN
 return, derivatorStart(y,step,x=x)
ENDIF ELSE BEGIN
	IF(type EQ "1") OR (type EQ "mid") THEN return, derivatorMid(y,step,x=x) ELSE RETURN,0
ENDELSE
END

