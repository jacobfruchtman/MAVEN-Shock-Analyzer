function kroneckerdelta, a, b
; This function returns 0 for x < a and 1 for x >= a
result = 0
if a eq b then result = 1
return, result
end
