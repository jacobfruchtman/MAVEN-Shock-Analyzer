function stepfunction2, x, a
; This function returns 0 for x < a and 1 for x >= a
print," Θ(x-a)=Θ(",x,"-",a,")=Θ(",x-a,")"
result = 1
if x LT a then result = 0
return, result
end
