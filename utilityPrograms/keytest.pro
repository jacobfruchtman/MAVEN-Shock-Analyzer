pro keytest, key=key
	if NOT KEYWORD_SET(key) THEN key=-1
	print,key+10
end
