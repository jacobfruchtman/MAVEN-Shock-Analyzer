
; GET_COLOR
; GET_COLOR reads the standard IDL color table to get r,g,b arguments
; for e.g. WRITE_GIF without actually loading a color table, and squeezes
; or expands the color vectors into n_color entries.
; Cribbed from LOADCT. D.M. fecit, 12 January, 1994. Happy 5th birthday, Zev!
;
pro get_color, table_number, n_color, r, g, b

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

nc = n_color & get_lun, lun

is_4 = fix(strmid(!version.release, 0, 1)) ge 4

if strlowcase(!version.os) eq 'vms' then begin
   if is_4 then table_file = 'idl_dir:[resource.colors]colors1.tbl' else $
      table_file = 'idl_dir:[000000]colors1.tbl'
endif else begin
   if is_4 then table_file = '$IDL_DIR/resource/colors/colors1.tbl' else $
         table_file = '$IDL_DIR/colors1.tbl'
end

openr, lun, table_file, /block

ntables = 0b
readu, lun, ntables

if (table_number ge ntables) or (table_number lt 0) then begin
  message, 'Table number must be from 0 to ' + strtrim(ntables-1, 2)
  end

message,'Reading table ' + strtrim(table_number, 2), /INFO
aa=assoc(lun, bytarr(256),1)	;Read 256 long ints <-- This must be bytes now.
r = aa(table_number*3)
g = aa(table_number*3+1)
b = aa(table_number*3+2)

if nc ne 256 then begin	;Interpolate
   p = (lindgen(nc) * 255) / (nc-1)
   r = r(p)
   g = g(p)
   b = b(p)
endif

; r_curr = r
; g_curr = g
; b_curr = b

return & end

