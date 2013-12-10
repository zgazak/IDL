function range_find,array,extend,remexp=remexp
  yrange = mm(array)+[-1,1]*(extend*(max(array)-min(array)))
  if keyword_set(remexp) then begin
     remexp = 10^(floor(alog10(yrange[1]))*1d0)
     yrange/=remexp
  endif
  return,yrange
end
