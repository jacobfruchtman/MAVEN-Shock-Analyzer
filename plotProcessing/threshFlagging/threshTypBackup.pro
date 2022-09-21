function threshtypbackup, typ

	typi=typ
	typf=typ
	CASE typ OF

	0: BEGIN;gt
	         PRINT, 'gt'

	       END
	1: Begin;ge
		 PRINT, 'ge'
		end
	2: Begin;lt
		 PRINT, 'lt'
		end
	3: Begin;le
		PRINT, 'le'
		end
	4: Begin;eq
		PRINT, 'eq'
		end
	5: Begin;hit
		PRINT, 'hit'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	6: Begin;leave
		PRINT, 'leave'
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	7: Begin;T_or
		PRINT, 'T_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	8: Begin;F_or
		PRINT, 'F_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	9: Begin;T_and
		PRINT, 'T_and'
		typi=15;if we are evaluating y[0], just check  y[1] rather  than nonexistent y[-1]
		typf=14;if we are evaluating y[N-1], just check  y[N-2] rather than nonexistent y[N]
		end
	10: Begin;F_and
		PRINT, 'F_and'
		typi=6;if we are evaluating y[0], just check  y[1] rather  than nonexistent y[-1]
		typf=5;if we are evaluating y[N-1], just check  y[N-2] rather than nonexistent y[N]
		end
	11: Begin;xor
		PRINT, 'F_or'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold since we ddon't know what is happening on other side. may update this to be an optional paramenter instead
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	12: Begin
		;x2=mnt

		print,"closed range"
	        ; PRINT, 'Stooge'
		end
	13: Begin
		;x2=mnt
	         PRINT, 'open range'
		end
	14: Begin;hit
		PRINT, 'unmoved'
		typi=4;if we are evaluating y[0], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	15: Begin;hit
		PRINT, 'unmoving'
		typf=4;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	16: Begin;pass
		PRINT, 'pass'
		typi=6
		typf=5;if we are evaluating y[N-1], just check  if its currently at the threshold. may update this to be an optional paramenter instead
		end
	17: Begin;ascend
		PRINT, 'pass'
		typi=20;rise
		typf=22;rose
		end
	18: Begin;descend
		PRINT, 'pass'
		typi=19;drop
		typf=21;fell
		end
	19: Begin;drop
		PRINT, 'pass'
		
		typf=4
		end
	20: Begin;rise
		PRINT, 'pass'
		
		typf=4
		end
	21: Begin;fell
		PRINT, 'pass'
		
		typi=4
		end
	22: Begin;rose
		PRINT, 'pass'
		
		typf=4
		end
	else: Begin
		print,typ
		end
	ENDCASE
	return,[typi,typf]
end

