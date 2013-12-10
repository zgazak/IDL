function zg_colors,plotsetup=plotsetup
; call as colors=zg_colors()
; then in plots can put color=colors.blue, etc
  colors= {white:  100 ,$
           black:     0 ,$
           blue:    3 ,$  
           red:   1 ,$
           green:   2 ,$
           violet:  4 ,$
           orange:  7 ,$
           gray:    6 ,$
           dkblue:  8 ,$
           dkgreen: 9 ,$
           dkorange:5 ,$
           red2:    11,$
           red3:    12,$
           green1:  13,$
           green2:  14,$
           green3:  15, $
           litegray: 16, $
           litegray2: 17 $
          }
  tvlct,0,0,0,colors.black       ; black
  tvlct,255,255,255,colors.white ; white
  tvlct,255,0,0,colors.red       ; red
  tvlct,155,0,0,colors.red2      ; red
  tvlct,055,0,0,colors.red3      ; red
  tvlct,0,180,100,colors.green   ; green
  tvlct,0,220,100,colors.dkgreen ; green
  tvlct,0,110,220,colors.blue    ; blue
  tvlct,0,110,420,colors.dkblue  ; blue
  tvlct,250,112,0,colors.orange  ; orange
  tvlct,255,220,0,colors.dkorange; orange
  tvlct,120,120,120,colors.gray
  tvlct,150,150,150,colors.litegray2
  tvlct,200,200,200,colors.litegray
  tvlct,131,61,172,colors.violet ; indigo_violet_1
  tvlct,0,220,80,colors.green1   ; green
      tvlct,0,250,60,colors.green2   ; green
  tvlct,0,160,40,colors.green3   ; green

  
  if keyword_set(plotsetup) then $
     case plotsetup of
     'classic': begin
        !p.background = colors.white
        !p.color = colors.black
        !p.thick = 1
        !x.thick = 3
        !y.thick = 3
        window,0,xsize=800,ysize=400
        window,30,xsize=800,ysize=400,/pixmap
        wset,0
        !p.font = 2
     end 
     'classic2': begin
        !p.background = colors.white
        !p.color = colors.black
        !p.thick = 2
        !x.thick = 3
        !y.thick = 3
        window,0,xsize=800,ysize=800
        window,30,xsize=800,ysize=800,/pixmap
        wset,0
        !p.font = 2
        !p.multi=[0,1,2]
     end
  endcase
  
  

return,colors
end	
