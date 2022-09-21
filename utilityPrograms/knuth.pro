function knuth,A,B,n=n

	if not keyword_set(n) or n eq 1 then return,A^B
	if B eq 0 and n gt 1 then return,1 

	return,Knuth(A,n=(n-1),Knuth(A,n=n,B-1))
end 
