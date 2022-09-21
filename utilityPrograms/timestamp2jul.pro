function timestamp2jul,t

	timestamp_string = t.Replace(',', '.')

	TIMESTAMPTOVALUES, timestamp_string, $
   YEAR=year, MONTH=month, DAY=day, $
   HOUR=hour, MINUTE=minute, $
   SECOND=second, OFFSET=offset

	jt= JULDAY(month, day, year, hour, minute, second)

	return, jt
end
