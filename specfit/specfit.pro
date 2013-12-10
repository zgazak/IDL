


pro specfit::initialfits,bestcenter=bestcenter
;  stop
  
  center = [n_elements((*self.models).grid.params.temps)/2d0,$
            n_elements((*self.models).grid.params.gravs)/2d0,$
            n_elements((*self.models).grid.params.abuns)/2d0,$
            n_elements((*self.models).grid.params.turbs)/2d0] ;; teff 3900, grav 0.0, z = 0, turb = 4
  
  center = round(center-0.5)
  
  ;; if keyword_set(bestcenter) then $
  ;;    center = [(*self.active_object).bestloc[0],$
  ;;              (*self.active_object).bestloc[1],$
  ;;              (*self.active_object).bestloc[2],$
  ;;              (*self.active_object).bestloc[3]]
  
  count=0
  
  
  for i=-1,1 do $
     for j=-1,1 do $
        for k=-1,1 do $ 
           for l=-1,1 do begin
     self.initialfits[*,count] = center+[i,j,k,l]
     count++
  endfor
  
  if (where(self.initialfits[0,*] gt n_elements((*self.models).grid.params.temps)-1))[0] ne -1 then $
     self.initialfits[*,where(self.initialfits[0,*] gt n_elements((*self.models).grid.params.temps)-1)] = -1     
  if (where(self.initialfits[1,*] gt n_elements((*self.models).grid.params.gravs)-1))[0] ne -1 then $
     self.initialfits[*,where(self.initialfits[1,*] gt n_elements((*self.models).grid.params.gravs)-1)] = -1     
  if (where(self.initialfits[2,*] gt n_elements((*self.models).grid.params.abuns)-1))[0] ne -1 then $
     self.initialfits[*,where(self.initialfits[2,*] gt n_elements((*self.models).grid.params.abuns)-1)] = -1     
  if (where(self.initialfits[3,*] gt n_elements((*self.models).grid.params.turbs)-1))[0] ne -1 then $
     self.initialfits[*,where(self.initialfits[3,*] gt n_elements((*self.models).grid.params.turbs)-1)] = -1     

                                ;stop
 
end 

pro specfit::color_manage,type
  case type of
     'bw': begin
        loadct,0,/silent
        !p.color=0
        !p.background=255
     end
     'color': begin
        ptr_free,self.colors
        self.colors = ptr_new(specfit_colors())
        !p.color=(*self.colors).black
        !p.background=(*self.colors).white
     end
     'else': print,"unknown color management type"
  endcase
end  

pro specfit::MenuMap,event,manual=manual
  if keyword_set(manual) then info = event else Widget_Control, event.id, Get_UValue=info
  
  case info.type of
     'main': begin
        for i=0,n_elements(*self.bases)-1,1 do if (*self.bases)[i] ne 0 then widget_control,(*self.bases)[i],MAP=0
        case info.value of
           'Setup': begin
              ;; self->setup,'setup'
              widget_control,   (*self.bases)[self->base('Setup')],/map
           end
           'Fitting Options': begin
              self->setup,'fittingoptions'
              widget_control,   (*self.bases)[self->base('Fitting Options')],/map
           end
           'Analysis': begin
              self->setup,'analysis'
                                ; widget_control,   (*self.bases)[3],/map
           end
           'Quit' : begin
              self->setup,'quit'
              widget_control,  (*self.bases)[self->base('Quit')],/map
           end
           'Plotting' : begin
              self->setup,'plotting'
                                ; widget_control,  (*self.bases)[4],/map
           end
           else:  print,'Unknown MenuMap Event "'+info.value+'"'
        endcase
     end
     'wspace': begin 
        for i=0,n_elements(*self.mainbases)-2,1 do if (*self.mainbases)[i] ne 0 then widget_control,(*self.mainbases)[i],MAP=0
        case info.value of
           'single': begin
              widget_control,   (*self.mainbases)[0],/map
           end
           else:  print,'Unknown MenuMap Event "'+info.value+'"'
        endcase
     end
  endcase
end

pro specfit::startplot,id
  wset,(*self.plot_windows)[id].pix_window
end

pro specfit::endplot,id
  wset,(*self.plot_windows)[id].w_id
  device,copy=[0,0,(*self.plot_windows)[id].x,(*self.plot_windows)[id].y,0,0,(*self.plot_windows)[id].pix_window]    
end


pro specfit::quit,event
  self->message,message='Destroying Widget',which='A'
  self->message,message='Destroying Widget',which='B'

  for i=0,n_elements(*self.buttons)-1,1 do if (*self.buttons)[i] then widget_control,(*self.buttons)[i],/destroy
  for i=0,n_elements(*self.sliders)-1,1 do  if (*self.sliders)[i].id then widget_control,(*self.sliders)[i].id,/destroy
  for i=n_elements(*self.bases)-1,0,-1 do if (*self.bases)[i] then widget_control,(*self.bases)[i],/destroy
  for i=n_elements(*self.mainbases)-1,0,-1 do if (*self.mainbases)[i] then widget_control,(*self.mainbases)[i],/destroy
  for i=n_elements(*self.setbases)-1,0,-1 do if (*self.setbases)[i] then widget_control,(*self.setbases)[i],/destroy
  for i=n_elements(*self.layout_bases)-1,0,-1 do if (*self.layout_bases)[i] then widget_control,(*self.layout_bases)[i],/destroy
  
  

  if self.master_base then widget_control,self.master_base,/destroy
  
  ptr_free,self.colors,self.plot_windows,self.sliders,$
           self.buttons,self.menus,self.bases,self.extra_windows,$
           self.setbases_id,self.paths,self.fields,$
           self.mainbases,$
           self.setbases,self.objects,self.models,self.active_object,$
           self.location,self.fit_res,self.master_dflux,self.master_derr,$
           self.master_swave,self.bases_ID,self.layout_bases,self.linelist,$
           self.infields_id,self.infields
  
                                ; stop
  obj_destroy,self
  help,/heap
end

pro specfit_cleanup,event
                                ; help,/heap
  
end

pro specfit::switchmaster
  if self.do_master then begin
     (*self.active_object).spec = (*self.active_object).store_spec
     place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+self.init_shift)
     
     if n_elements(place) gt n_elements(*self.master_dflux) then place = place[0:n_elements(*self.master_dflux)-1]
     if n_elements(place) lt n_elements(*self.master_dflux) then place = [place,max(place)+1]
     
     ;; stop
     
     ;; ((*self.active_object).spec[*,1]) *= 0
     ;; ((*self.active_object).spec[*,2]) *= 0
     ;; ((*self.active_object).spec[*,1]) += sqrt(-1)
     ;; ((*self.active_object).spec[*,2]) += sqrt(-1)
     case self.runtype of
        'run_diag': begin
        end
        'run_full': begin
           ((*self.active_object).spec[place,1]) = *self.master_swave
           ((*self.active_object).spec[place,1]) = *self.master_dflux
           ((*self.active_object).spec[place,2]) = *self.master_derr
           (*self.active_object).storebestfix = (*self.active_object).bestfix
           (*self.active_object).bestfix *= 0d0
        end
     endcase
  endif
  
  print,"re measure best res with this new wave solution"
  ;;    stop
  
  *self.location = (*self.active_object).bestloc
;;  *self.location= self->find_bestloc(self.locsize)

  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  if fail then begin
     print,"Best model is bad!!"
     stop
  endif

  bestres =  self->fitres(modwave,modflux,modres,/doplot)

  print,bestres,(*self.active_object).overallres
  
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/bestres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=bestres
  self->linemask,sflux=sflux,swave=swave,finalres=bestres,dflux=dflux
  if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=bestres
  if (where(*(*self.active_object).linemask eq 2))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 2)]=sqrt(-1)

                                ;csq = self->fullchi(swave,sflux,dflux,derr,chispec=chispec)
  csq = self->gstatistic_trusterr(swave,sflux,dflux,derr,chispec=chispec)
  if csq le self.best_chi[0] then begin
     (*self.active_object).overallres = bestres
     ;;   stop
     if 0 then $
        if (where(chispec gt 10*median(chispec)))[0] ne -1 and self.first then begin
        (*(*self.active_object).linemask)[where(chispec gt 10*median(chispec))] = 2
        self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=bestres
        ;;  if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
        if (where(*(*self.active_object).linemask eq 2))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 2)]=sqrt(-1)
        ;;        csq = self->fullchi(swave,sflux,dflux,derr,chispec=chispec) 
        csq = self->gstatistic_trusterr(swave,sflux,dflux,derr,chispec=chispec)
        
        self.first=0
     endif
  endif
  
  print,csq,self.best_chi[0]
  
  ;; stop
  
  
  
  ;; print,"overallres ... bestres after wave fix"
  ;; print,(*self.active_object).overallres,bestres
  
  if 1 then begin
     self->initialfits,/bestcenter
  endif
end



pro specfit::lininterp,params,iwave,iflux,makestop=makestop
  pars =  (*self.models).grid.params
                                ;print,params


  spy=0
  debug=0
  
  
  if keyword_set(makestop) then begin
     spy=1
     debug=1
  endif

  if spy then window
  if spy then colors=zg_colors()

  teff = 0
  logg = 1
  abun = 2
  turb = 3
  
  if n_elements(pars.temps) ge 2 then tbounds = pars.temps[(sort(abs(pars.temps-params[0])))[0:1]] else tbounds = [pars.temps,pars.temps]
  gbounds = pars.gravs[(sort(abs(pars.gravs-params[1])))[0:1]]
  abounds = pars.abuns[(sort(abs(pars.abuns-params[2])))[0:1]]
  if n_elements(pars.turbs) ge 2 then mbounds = pars.turbs[(sort(abs(pars.turbs-params[3])))[0:1]] else mbounds = [pars.turbs,pars.turbs]

 ;;; MTG1:
;;
  usem = mbounds[0]   ;; mturb
  usea = abounds[0]   ;; abund
  uset = tbounds[0]
  useg = gbounds[0]
  l3 = [where(pars.temps eq uset),$
        where(pars.gravs eq useg),$
        where(pars.abuns eq usea),$
        where(pars.turbs eq usem)]
  l3p = [uset,useg,usea,usem]
  uset = tbounds[1]
  l4 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l4p = [uset,useg,usea,usem]
  useg = gbounds[1] 
  l5 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l5p = [uset,useg,usea,usem]
  uset = tbounds[0]
  l6 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l6p = [uset,useg,usea,usem]
  
  m3 = (*self.models).flux[*,l3[0],l3[1],l3[2],l3[3]]
  m4 = (*self.models).flux[*,l4[0],l4[1],l4[2],l4[3]]
  m5 = (*self.models).flux[*,l5[0],l5[1],l5[2],l5[3]]
  m6 = (*self.models).flux[*,l6[0],l6[1],l6[2],l6[3]]
  
  if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
  
  if spy then print,"m0 - m1 : m0p5"
  m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
  m3p5p = [params[teff],l3p[1:3]]
  if spy then begin
     plot,m3[1000:1200],psym=10
     oplot,m4[1000:1200],psym=10,color=colors.red
     oplot,m3p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

  if spy then print,"m2 - m3 : m2p5"
  m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
  m5p5p = [params[teff],l5p[1:3]]
  if spy then begin
     plot,m5[1000:1200],psym=10,color=colors.black
     oplot,m6[1000:1200],psym=10,color=colors.red
     oplot,m5p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  mtg1 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
  ;;mtg2 = m3p5+((m3p5-m5p5)/(m3p5p[logg]-m5p5p[logg]))*(params[logg]-m5p5[logg])
  mtg1p = [params[teff],params[logg],usea,usem]
  if spy then begin
     plot,m3p5[1000:1200],psym=10
     oplot,m5p5[1000:1200],psym=10,color=colors.red
     oplot,mtg1[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

;;   interpolate teff, logg
;;   usem = mbounds[0]
;;   usea = abounds[0]
;;   if spy then begin
;;      print,'interpolating Teff, logg for model 1'
;;   endif
;;   uset = tbounds[0]
;;   useg = gbounds[0]
;;   l0 = [where(pars.temps eq uset),$
;;         where(pars.gravs eq useg),$
;;         where(pars.abuns eq usea),$
;;         where(pars.turbs eq usem)]
;;   l0p = [uset,useg,usea,usem]
;;   uset = tbounds[1]
;;   l1 =  [where(pars.temps eq uset),$
;;          where(pars.gravs eq useg),$
;;          where(pars.abuns eq usea),$
;;          where(pars.turbs eq usem)]
;;   l1p = [uset,useg,usea,usem]
;;   useg = gbounds[1] 
;;   l2 =  [where(pars.temps eq uset),$
;;          where(pars.gravs eq useg),$
;;          where(pars.abuns eq usea),$
;;          where(pars.turbs eq usem)]
;;   l2p = [uset,useg,usea,usem]
;;   uset = tbounds[0]
;;   l3 =  [where(pars.temps eq uset),$
;;          where(pars.gravs eq useg),$
;;          where(pars.abuns eq usea),$
;;          where(pars.turbs eq usem)]
;;   l3p = [uset,useg,usea,usem]
  
;;   m0 = (*nmodelspec.grid_nflux)[*,l0[0],l0[1],l0[2],l0[3]]
;;   m1 = (*nmodelspec.grid_nflux)[*,l1[0],l1[1],l1[2],l1[3]]
;;   m2 = (*nmodelspec.grid_nflux)[*,l2[0],l2[1],l2[2],l2[3]]
;;   m3 = (*nmodelspec.grid_nflux)[*,l3[0],l3[1],l3[2],l3[3]]
  
;;   MTG1
;;   this is Teff step 1
;;   so X here is teff
;;   if spy then print,"m0 - m1 : m0p5"
;;   m0p5 = m0+((m0-m1)/(l0p[teff]-l1p[teff]))*(params[teff]-l0p[teff])
;;   m0p5p = [params[teff],l0p[1:3]]
;;   if spy then print,"m2 - m3 : m2p5"
;;   m2p5 = m2+((m2-m3)/(l2p[teff]-l3p[teff]))*(params[teff]-l2p[teff])
;;   m2p5p = [params[teff],l2p[1:3]]
  
;;   this is grav step 1
;;   so X here is grav
;;   mtg1 = m2p5+((m0p5-m2p5)/(m0p5p[logg]-m2p5p[logg]))*(params[logg]-m0p5[logg])
;;   mtg1p = [params[teff],params[logg],l2p[2:3]]
;;   if spy then begin
;;      plot,m0p5[1000:1500],psym=10
;;      oplot,m2p5[1000:1500],psym=10,color=colors.red
;;      oplot,mtg1[1000:1500],psym=10,color=colors.blue
;;   endif
  
  ;;; MTG2:
  ;;
  usem = mbounds[0] ;; mturb
  usea = abounds[1] ;; abund
  uset = tbounds[0]
  useg = gbounds[0]
  l3 = [where(pars.temps eq uset),$
        where(pars.gravs eq useg),$
        where(pars.abuns eq usea),$
        where(pars.turbs eq usem)]
  l3p = [uset,useg,usea,usem]
  uset = tbounds[1]
  l4 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l4p = [uset,useg,usea,usem]
  useg = gbounds[1] 
  l5 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l5p = [uset,useg,usea,usem]
  uset = tbounds[0]
  l6 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l6p = [uset,useg,usea,usem]
  
  m3 = (*self.models).flux[*,l3[0],l3[1],l3[2],l3[3]]
  m4 = (*self.models).flux[*,l4[0],l4[1],l4[2],l4[3]]
  m5 = (*self.models).flux[*,l5[0],l5[1],l5[2],l5[3]]
  m6 = (*self.models).flux[*,l6[0],l6[1],l6[2],l6[3]]
  if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
  
  if spy then print,"m0 - m1 : m0p5"
  m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
  m3p5p = [params[teff],l3p[1:3]]
  if spy then begin
     plot,m3[1000:1200],psym=10
     oplot,m4[1000:1200],psym=10,color=colors.red
     oplot,m3p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

  if spy then print,"m2 - m3 : m2p5"
  m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
  m5p5p = [params[teff],l5p[1:3]]
  if spy then begin
     plot,m5[1000:1200],psym=10
     oplot,m6[1000:1200],psym=10,color=colors.red
     oplot,m5p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  ;; this is grav step 1
  ;; so X here is grav
  mtg2 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
  ;;mtg2 = m3p5+((m3p5-m5p5)/(m3p5p[logg]-m5p5p[logg]))*(params[logg]-m5p5[logg])
  mtg2p = [params[teff],params[logg],usea,usem]
  if spy then begin
     plot,m3p5[1000:1200],psym=10
     oplot,m5p5[1000:1200],psym=10,color=colors.red
     oplot,mtg2[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  ;; mtga1 is proper teff, grav, and abund, first microturb
  mtga1 = specfit_lint(mtg1,mtg1p[abun],mtg2,mtg2p[abun],params[abun])
  if spy then begin
     plot,mtg1[1000:1200],psym=10
     oplot,mtg2[1000:1200],psym=10,color=colors.red
     oplot,mtga1[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  mtga1p = [params[0:2],usem]
  
  
  ;; now need mtga2 at proper teff, grav, abund, second microturb
  ;; so first mtg3 = proper t,g, abund1, mt2
  
  ;;; MTG3:
  ;;
  usem = mbounds[1] ;; mturb
  usea = abounds[0] ;; abund
  uset = tbounds[0]
  useg = gbounds[0]
  l3 = [where(pars.temps eq uset),$
        where(pars.gravs eq useg),$
        where(pars.abuns eq usea),$
        where(pars.turbs eq usem)]
  l3p = [uset,useg,usea,usem]
  uset = tbounds[1]
  l4 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l4p = [uset,useg,usea,usem]
  useg = gbounds[1] 
  l5 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l5p = [uset,useg,usea,usem]
  uset = tbounds[0]
  l6 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l6p = [uset,useg,usea,usem]
  
  m3 = (*self.models).flux[*,l3[0],l3[1],l3[2],l3[3]]
  m4 = (*self.models).flux[*,l4[0],l4[1],l4[2],l4[3]]
  m5 = (*self.models).flux[*,l5[0],l5[1],l5[2],l5[3]]
  m6 = (*self.models).flux[*,l6[0],l6[1],l6[2],l6[3]]
  if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
  
  if spy then print,"m0 - m1 : m0p5"
  m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
  m3p5p = [params[teff],l3p[1:3]]
  if spy then begin
     plot,m3[1000:1200],psym=10
     oplot,m4[1000:1200],psym=10,color=colors.red
     oplot,m3p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

  if spy then print,"m2 - m3 : m2p5"
  m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
  m5p5p = [params[teff],l5p[1:3]]
  if spy then begin
     plot,m5[1000:1200],psym=10,color=colors.black
     oplot,m6[1000:1200],psym=10,color=colors.red
     oplot,m5p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  ;; this is grav step 1
  ;; so X here is grav
  mtg3 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
  mtg3p = [params[teff],params[logg],usea,usem]
  if spy then begin
     plot,m3p5[1000:1200],psym=10
     oplot,m5p5[1000:1200],psym=10,color=colors.red
     oplot,mtg3[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  ;; second mtg4 = proper t,g, abund2, mt2
  
  ;;; MTG4:
   ;;; MTG3:
  ;;
  usem = mbounds[1] ;; mturb
  usea = abounds[1] ;; abund
  uset = tbounds[0]
  useg = gbounds[0]
  l3 = [where(pars.temps eq uset),$
        where(pars.gravs eq useg),$
        where(pars.abuns eq usea),$
        where(pars.turbs eq usem)]
  l3p = [uset,useg,usea,usem]
  uset = tbounds[1]
  l4 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l4p = [uset,useg,usea,usem]
  useg = gbounds[1] 
  l5 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l5p = [uset,useg,usea,usem]
  uset = tbounds[0]
  l6 =  [where(pars.temps eq uset),$
         where(pars.gravs eq useg),$
         where(pars.abuns eq usea),$
         where(pars.turbs eq usem)]
  l6p = [uset,useg,usea,usem]
  
  m3 = (*self.models).flux[*,l3[0],l3[1],l3[2],l3[3]]
  m4 = (*self.models).flux[*,l4[0],l4[1],l4[2],l4[3]]
  m5 = (*self.models).flux[*,l5[0],l5[1],l5[2],l5[3]]
  m6 = (*self.models).flux[*,l6[0],l6[1],l6[2],l6[3]]
  if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
  
  
  if spy then print,"m0 - m1 : m0p5"
  m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
  m3p5p = [params[teff],l3p[1:3]]
  if spy then begin
     plot,m3[1000:1200],psym=10
     oplot,m4[1000:1200],psym=10,color=colors.red
     oplot,m3p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

  if spy then print,"m2 - m3 : m2p5"
  m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
  m5p5p = [params[teff],l5p[1:3]]
  if spy then begin
     plot,m5[1000:1200],psym=10
     oplot,m6[1000:1200],psym=10,color=colors.red
     oplot,m5p5[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  ;; this is grav step 1
  ;; so X here is grav
  mtg4 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
  mtg4p = [params[teff],params[logg],usea,usem]
  if spy then begin
     plot,m3p5[1000:1200],psym=10
     oplot,m5p5[1000:1200],psym=10,color=colors.red
     oplot,mtg3[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  

  
  ;; mtga2 is proper teff, grav, and abund, second microturb
  mtga2 = specfit_lint(mtg3,mtg3p[abun],mtg4,mtg4p[abun],params[abun])
  if spy then begin
     plot,mtg1[1000:1200],psym=10
     oplot,mtg2[1000:1200],psym=10,color=colors.red
     oplot,mtga1[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  mtga2p = [params[0:2],usem]
  
  
  ;; now mtgat is linearily interpolated full ass model
  mtgat = specfit_lint(mtga1,mtga1p[turb],mtga2,mtga2p[turb],params[turb])
  mtgatp = params
  if spy then begin
     plot,mtga1[1000:1200],psym=10
     oplot,mtga2[1000:1200],psym=10,color=colors.red
     oplot,mtgat[1000:1200],psym=10,color=colors.blue
     if debug then stop
  endif
  
  iflux = mtgat
  iwave = (*self.models).wave
  if keyword_set(makestop) then stop
end



pro specfit::arrange_objects
  spawn,'ls '+self.directory+'/*.fits',objs
 
  
  objs = reverse(objs)
  
  ptr_free,self.objects
  self.objects = ptr_new(objs)
  
                                ;for i=0,n_elements(objs)-1 do $
                                ;   if i eq 0 then self->writetotex,'Objects & '+objs[i]+' \\' else $
                                ;      self->writetotex,'  & '+objs[i]+' \\'
  objs = 0L
end

pro specfit_Event,event
  heap = 0
  if heap then help,/heap
  Widget_Control, event.id, Get_UValue=info
  Call_Method, info.method, info.object, event
  if heap then help,/heap
end



pro specfit::linemask,swave=swave,sflux=sflux,finalres=finalres,dflux=dflux
  ;; if it is necessary:
  if strcmp(self.runtype,'run_diag') or strcmp(self.runtype,'run_pixel') then begin
     if self.lock_mask eq 0 then begin
        ptr_free,(*self.active_object).linemask
        linemask = dblarr(n_elements(swave))
        for i=0,n_elements((*self.linelist).wave)-1 do begin
           
           ;; definte the FWHM of the lines for the given fit spectral
           ;; resolution
           dl = (*self.linelist)[i].wave/finalres
           if (*self.linelist)[i].num eq 2 then range = [(*self.linelist)[i].wave-self.doubline*dl,(*self.linelist)[i].wave+self.doubline*dl] else $
              range = [(*self.linelist)[i].wave-self.singline*dl,(*self.linelist)[i].wave+self.singline*dl]
           line = where(swave ge range[0] and swave le range[1])
           
           if line[0] ne -1 and n_elements(line) gt 1 then begin
              while sflux[line[0]] gt sflux[line[1]] and line[0]-1 ge 0 do line = [line[0]-1,line]
              while sflux[line[n_elements(line)-1]] gt sflux[line[n_elements(line)-2]] and line[n_elements(line)-1]+1 le n_elements(sflux)-1 do line = [line,line[n_elements(line)-1]+1]
                                ;     line = line[1:n_elements(line)-2]
              linemask[line] = 1d0
           endif
           
           if 0 then begin
              plot,swave,sflux,psym=10,xrange=[(*self.linelist)[i].wave-10*dl,(*self.linelist)[i].wave+10*dl],/xs
              oplot,swave[where(linemask)],sflux[where(linemask)],color=(*self.colors).blue,thick=2,psym=8
              vline,(*self.linelist)[i].wave,color=(*self.colors).red
           endif
           range = 0L
           dl = 0L
           line = 0L
        endfor
        
        (*self.active_object).linemask = ptr_new(linemask)
        linemask = 0L
     endif                      ; else stop
  endif else begin
     ptr_free,(*self.active_object).linemask
     linemask = dblarr(n_elements(swave))+1d0


     wide = 0.0006
     lam1 = 1.2083+[-1,1]*wide
     lam2 = 1.18275+[-1,1]*wide
     
     cond = where(swave gt lam1[0] and swave le lam1[1])
     if cond[0] ne -1 then linemask[cond] = 0
     cond = where(swave gt lam2[0] and swave le lam2[1])
     if cond[0] ne -1 then linemask[cond] = 0
     
     if (*self.linemask).active then $
        for i=0,n_elements((*self.linemask).l1)-1 do begin
        cond = where(swave gt (*self.linemask).l1[i] and swave le (*self.linemask).l2[i])
      ;  print,cond
        if cond[0] ne -1 then linemask[cond] = 0 
     endfor
     
     
                                ;stop if cond[0] ne -1 then linemask[cond] = 1d0
     
     (*self.active_object).linemask = ptr_new(linemask)
  endelse
end


pro specfit::heaphelp,string
  print,'heaphelp: '+string
  help,/heap
end

pro specfit::makeobject,file
  heap = 0
  if heap then help,/heap
  
  x = mrdfits(file,0,hdr,/silent)
  
  x = x[where(finite(x[*,1])),*]
  
                                ; stop
  
  self.baseres = sxpar(hdr,'expres')
  
  if self.baseres eq 0 then self.baseres = self.expected_res else self.expected_res = self.baseres
  
  self->setupgrid
  self->initialfits
  
  
  if file_test(strjoin(last_element(strsplit(file,'.',/extract),/all_but),'.')+'.mask') then $
     mask = mrdfits(strjoin(last_element(strsplit(file,'.',/extract),/all_but),'.')+'.mask',0,hdr,/silent) else $
        mask = x[*,0]*0d0 + 1d0

  if (where(mask eq 0))[0] ne -1 then begin
     x[where(mask eq 0),1] = sqrt(-1)
     self.message = 'Masked bad points...'
     self->message,which='B'
  endif
  

  self->meas_init_shift,self.baseres,x

  place = where(x[*,0] ge self.fitrange[0]+self.init_shift and x[*,0] le self.fitrange[1]+self.init_shift)
  
                                ; stop
  
  if self.made_active then ptr_free,(*self.active_object).linemask,$
                                    (*self.active_object).storecont,$
                                    (*self.active_object).bestcont,$
                                    (*self.active_object).header
  
  ptr_free,self.active_object
  
  narrow = where(x[*,0] ge self.fitrange[0] and x[*,0] le self.fitrange[1])
  self.active_object = ptr_new({objfile: file,$
                                obj: last_element(strsplit(last_element(strsplit(file,'/',/extract)),'.',/extract),/all_but),$
                                bk_objfile: self.outdir+last_element(strsplit(file,'/',/extract)),$
                                version: self.version,$
                                grid: self.modgrid,$
                                refit: 0L,$
                                modsnr: 0d0,$
                                origsnr: 0d0,$
                                spec: x,$
                                store_spec: x,$
                                modcount: 0d0,$
                                goodmodcount: 0d0,$
                                linemask: ptr_new(0),$
                                mcgrid: dblarr(n_elements(x[place,0]),$
                                               n_elements((*self.models).grid.params.temps),$
                                               n_elements((*self.models).grid.params.gravs),$
                                               n_elements((*self.models).grid.params.abuns),$
                                               n_elements((*self.models).grid.params.turbs)),$
                                csq_ind:  dblarr(n_elements((*self.models).grid.params.temps),$
                                                 n_elements((*self.models).grid.params.gravs),$
                                                 n_elements((*self.models).grid.params.abuns),$
                                                 n_elements((*self.models).grid.params.turbs)),$
                                chisq: dblarr(n_elements((*self.models).grid.params.temps),$
                                              n_elements((*self.models).grid.params.gravs),$
                                              n_elements((*self.models).grid.params.abuns),$
                                              n_elements((*self.models).grid.params.turbs)),$
                                chisq_smooth: dblarr(n_elements((*self.models).grid.params.temps),$
                                                     n_elements((*self.models).grid.params.gravs),$
                                                     n_elements((*self.models).grid.params.abuns),$
                                                     n_elements((*self.models).grid.params.turbs)),$
                                fullchisq: dblarr(n_elements(x[place,0]),$
                                                  n_elements((*self.models).grid.params.temps),$
                                                  n_elements((*self.models).grid.params.gravs),$
                                                  n_elements((*self.models).grid.params.abuns),$
                                                  n_elements((*self.models).grid.params.turbs)),$
                                fullchisqw: dblarr(n_elements(x[place,0]),$
                                                   n_elements((*self.models).grid.params.temps),$
                                                   n_elements((*self.models).grid.params.gravs),$
                                                   n_elements((*self.models).grid.params.abuns),$
                                                   n_elements((*self.models).grid.params.turbs)),$
                                fullmod: dblarr(n_elements(x[narrow,0]),$
                                                n_elements((*self.models).grid.params.temps),$
                                                n_elements((*self.models).grid.params.gravs),$
                                                n_elements((*self.models).grid.params.abuns),$
                                                n_elements((*self.models).grid.params.turbs)),$
                                fullwave: dblarr(n_elements(x[narrow,0]),$
                                                 n_elements((*self.models).grid.params.temps),$
                                                 n_elements((*self.models).grid.params.gravs),$
                                                 n_elements((*self.models).grid.params.abuns),$
                                                 n_elements((*self.models).grid.params.turbs)),$
                                fulldat: dblarr(n_elements(x[narrow,0]),$
                                                n_elements((*self.models).grid.params.temps),$
                                                n_elements((*self.models).grid.params.gravs),$
                                                n_elements((*self.models).grid.params.abuns),$
                                                n_elements((*self.models).grid.params.turbs)),$
                                bestres: dblarr(n_elements((*self.models).grid.grid[0,*,0,0,0]),$
                                                n_elements((*self.models).grid.grid[0,0,*,0,0]),$
                                                n_elements((*self.models).grid.grid[0,0,0,*,0]),$
                                                n_elements((*self.models).grid.grid[0,0,0,0,*])),$
                                fixwave: dblarr(n_elements((*self.models).grid.grid[0,*,0,0,0]),$
                                                n_elements((*self.models).grid.grid[0,0,*,0,0]),$
                                                n_elements((*self.models).grid.grid[0,0,0,*,0]),$
                                                n_elements((*self.models).grid.grid[0,0,0,0,*])),$
                                bestmod: dblarr(n_elements(x[narrow,0])),$
                                bestwave:x[narrow,0],$                               
                                bestdat: x[narrow,1],$
                                bestfix: dblarr(n_elements(x[narrow,0])),$
                                storebestfix: dblarr(n_elements(x[narrow,0])),$
                                interpmod: dblarr(n_elements(x[narrow,0])),$
                                interpwave: dblarr(n_elements(x[narrow,0])),$                               
                                interpdat: dblarr(n_elements(x[narrow,0])),$
                                overallres: 0d0,$
                                lockres: 0,$
                                lockcont: 0,$
                                guessres: self.baseres,$
                                params: (*self.models).grid.params,$
                                bestpar: '',$
                                bestcont: ptr_new(),$
                                storecont: ptr_new(),$
                                bestloc: dblarr(4),$
                                header: ptr_new(),$
                                mc_params: dblarr(4,2),$
                                mc_params_data: dblarr(4,2)})  
  (*self.active_object).bestres += abs(sqrt(-1))
  for i=0d0,n_elements((*self.active_object).csq_ind)-1 do (*self.active_object).csq_ind[i] = i
  ptr_free,(*self.active_object).header
  (*self.active_object).header = ptr_new(hdr)
  hdr=0L
  
                                ; if heap then help,/heap

  self->plot
  
  if heap then help,/heap
  if heap then stop
  self.made_active = 1
end

pro specfit::plot_close,nopdf=nopdf,nopng=nopng
  device, /CLOSE_FILE
  set_plot, self.base_plot
                                ;cp = strjoin(last_element(strsplit(self.curr_plot,'.',/extract),/all_but),'.')
  cp = (strsplit(self.curr_plot,'.eps',/regex,/extract))[0]
;  if 1-keyword_set(nopdf) then spawn,'epstopdf '+self.curr_plot+'; rm
;  '+self.curr_plot
  if self.plot_test then stop
  if 1-keyword_set(nopng) then begin
     openw,lun,'temp.txt',/get_lun
     printf,lun,'convert -flatten -density 300 '+cp+'.eps '+cp+'.png'
     printf,lun,'rm '+cp+'.eps'
     ;;printf,lun,'rm temp.txt'
     free_lun,lun
     spawn,'source temp.txt &'
     spawn,'rm temp.txt'
  endif
  cp = 0L
  self.curr_plot = ''
end


pro specfit::writetoascii,string,fn
  printf,self.asciifile[fn],string
end

pro specfit::plot_open, filename, $
                        LANDSCAPE=landscape, $
                        XSIZE=xsize, $
                        YSIZE=ysize, $
                        INCHES=inches, $
                        COLOR=color, $
                        ENCAPSULATED=encapsulated, $
                        BITS_PER_PIXEL=bits_per_pixel, $
                        _REF_EXTRA=_extra
  
  self.base_plot = !d.name
  !p.font = 0
;;  device, /helvetica
  set_plot, 'PS', COPY=keyword_set(COLOR), INTERPOLATE=keyword_set(COLOR)
  
  device, FILENAME  = filename, $
          LANDSCAPE = keyword_set(LANDSCAPE), $
          XSIZE     = xsize, $
          YSIZE     = ysize, $
          XOFFSET   = xoffset, $
          YOFFSET   = yoffset, $
          INCHES    = keyword_set(INCHES), $
          COLOR     = keyword_set(COLOR), $
          BITS_PER_PIXEL = 8, $
          ENCAPSULATED   = keyword_set(ENCAPSULATED), $
          _EXTRA    = _extra

  self.curr_plot = filename
end


pro specfit::plot,swave=swave,sflux=sflux
  self->startplot,0
  
  ptr_free,self.colors
  self.colors = ptr_new(specfit_colors())
  !p.multi=[0,1,1]
  xrange=[self.fitrange[0],self.fitrange[1]]
  yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
  yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
                                ; plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10
  plot,[0],[0],color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns'
  
  
  if self.nomg then begin
   
     lam1 = 1.2083
     lam2 = 1.1828
     wide = 0.0006

     y = [!y.crange[0],!y.crange[1],!y.crange[1],!y.crange[0]]
     x = [lam1+wide,lam1+wide,lam1-wide,lam1-wide]
     polyfill,x,y,color=(*self.colors).litegray
     x = [lam2+wide,lam2+wide,lam2-wide,lam2-wide]
     polyfill,x,y,color=(*self.colors).litegray
     
                                ;  vline,[lam1,lam2]+wide,color=(*self.colors).violet
                                ;  vline,[lam1,lam2]-wide,color=(*self.colors).violet
  endif
  
  plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns',/noerase
  oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10
  if keyword_set(swave) then oplot,swave,sflux,psym=10,color=(*self.colors).red
  ;;vline,self.fitrange,color=(*self.colors).red
  
  
  abov = !y.crange[0]+0.05*(!y.crange[1]-!y.crange[0])
  for i=0,n_elements((*self.linelist).wave)-1 do xyouts,(*self.linelist)[i].wave,abov,(*self.linelist)[i].type,align=0.5,color=(*self.colors).black
  abov=0L
  

  sharpcorners,color=(*self.colors).black,thick=!x.thick 
  


  self->endplot,0
  
  if 1 then begin
                                ;if (*self.active_object).lockres then begin
     self->startplot,1
     xrange=[1.1875,1.19]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,xticks=3,xtickformat='(d5.3)',xtitle='Microns',/ys
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10
                                ;  if keyword_set(swave) then oplot,swave,sflux,psym=10,color=(*self.colors).red
     xyouts,1.1883,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Fe',align=0.5,color=(*self.colors).blue
     xyouts,1.1892,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Ti',align=0.5,color=(*self.colors).violet
     sharpcorners,color=(*self.colors).black,thick=!x.thick
     self->endplot,1
  endif 
  if (*self.active_object).lockres then begin
     self->startplot,2
     xrange=[1.1965,1.20]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,xticks=3,xtickformat='(d5.3)',xtitle='Microns',/ys
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10 
                                ;   if keyword_set(swave) then oplot,swave,sflux,psym=10,color=(*self.colors).red
     xyouts,1.1974,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Fe',align=0.5,color=(*self.colors).blue,charsize=2,charthick=2
     xyouts,1.1984,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     xyouts,1.1992,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     sharpcorners,color=(*self.colors).black,thick=!x.thick 
     self->endplot,2
  endif else begin
     !p.multi=[0,2,1]
     self->startplot,3
     xrange=[1.1965,1.20]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,xticks=3,xtickformat='(d5.3)',xtitle='Microns',/ys
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10 
                                ;   if keyword_set(swave) then oplot,swave,sflux,psym=10,color=(*self.colors).red
     xyouts,1.1974,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Fe',align=0.5,color=(*self.colors).blue,charsize=2,charthick=2
     xyouts,1.1984,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     xyouts,1.1992,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     sharpcorners,color=(*self.colors).black,thick=!x.thick 
     
     xrange=[1.166,1.170]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,xticks=3,xtickformat='(d5.3)',xtitle='Microns',/ys
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10 
                                ;   if keyword_set(swave) then oplot,swave,sflux,psym=10,color=(*self.colors).red
     xyouts,1.1974,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Fe',align=0.5,color=(*self.colors).blue,charsize=2,charthick=2
     xyouts,1.1984,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     xyouts,1.1992,yrange[1]-(0.08*(yrange[1]-yrange[0])),'Si',align=0.5,color=(*self.colors).violet
     sharpcorners,color=(*self.colors).black,thick=!x.thick 

     self->endplot,3
     !p.multi=[0,1,1]
  endelse
end


pro specfit::setupgrid
  !p.multi=[0,1,1]
  self->color_manage,'color'
  
  restore,self.modgrid
  img = mrdfits(grid.dir+grid.files[0,0,0,0,0],0,head,/silent)
  img[*,0]/=1d4
  shift = 0.01
  self->startplot,0
  
  ;; mr = where(img[*,0] ge self.fitrange[0]-shift and img[*,0] le self.fitrange[1]+shift)
  
  modres = 5.9d5  ;; (XX km/s or R= 590,000)
  newmodres = self.baseres*2d0
  dlam = (1.2/newmodres)/4d0 ;; 4 pixels per res element...
  newwave = fillarr(dlam,self.fitrange[0]-0.003,self.fitrange[1]+0.003)
  
  ptr_free,self.models
  saved_grid = self.saved_grids+'grid_'+$
               last_element(strsplit(last_element(strsplit(self.modgrid,'/',/extract)),'.',/extract),/all_but)+$
               '_R'+string(newmodres,format='(i6.6)')+$
               '_lam'+string(self.fitrange[0],format='(d5.3)')+'to'+string(self.fitrange[1],format='(d5.3)')+'.sav' 
                                ; print,saved_grid
  root_saved = self.saved_grids+'grid_'+$
               last_element(strsplit(last_element(strsplit(self.modgrid,'/',/extract)),'.',/extract),/all_but)+$
               '_R*'            ;+string(newmodres,format='(i6.6)')+'*'
;  stop
  spawn,'ls '+root_saved,temp
                                ;print,saved_grid
  if strcmp(temp[0],'') eq 0 then $
     if file_test(saved_grid) eq 0 then $
        for i=0,n_elements(temp)-1,1 do begin
     low = strmid(last_element((strsplit(temp[i],"_",/extract))),3,5)*1d0
     high = strmid(last_element((strsplit(temp[i],"_",/extract))),10,5)*1d0
     res = strmid(last_element((strsplit(temp[i],"R",/extract))),0,6)*1d0
     if round(low*1000)/1000d0 le round(1000*self.fitrange[0])/1000d0 and $
        round(1000*high)/1000d0 ge round(1000*self.fitrange[1])/1000d0 and $
        res ge newmodres then saved_grid = temp[i]
                                ; print,temp,low,high,res,newmodres
                                ; print,saved_grid
                                ; stop
  endfor
  
;  print,saved_grid

  temp = 0L
  root_saved = 0L
  ;; print,'zach, smaller range is okay too...'
  ;;stop
                                ;stop
  
  if file_test(saved_grid) then begin
     self->message,message='Load '+last_element(strsplit(saved_grid,'/',/extract)),which='A'
     restore,saved_grid
     self.models = ptr_new(fullgrid)
     fullgrid=0L
     newmodres = strmid(last_element((strsplit(saved_grid,"R",/extract))),0,6)*1d0
     dlam = (1.2/newmodres)/4d0 ;; 4 pixels per res element...
     newwave = fillarr(dlam,self.fitrange[0]-0.003,self.fitrange[1]+0.003)
  endif else begin
     self->message,message='No '+last_element(strsplit(saved_grid,'/',/extract)),which='A'
     self->message,message='Creating grid...',which='A'
     self.models = ptr_new({gridfile: self.modgrid,$
                            grid: grid,$
                            res:  newmodres,$
                            wave: newwave,$
                            flux: dblarr(n_elements(newwave),$
                                         n_elements(grid.grid[0,*,0,0,0]),$
                                         n_elements(grid.grid[0,0,*,0,0]),$
                                         n_elements(grid.grid[0,0,0,*,0]),$
                                         n_elements(grid.grid[0,0,0,0,*]))})
     
     self->message,message=' Filling grid'
     widget_control,self.progbarB,set_value=0

     fwhm = sqrt((1.1/newmodres)^2-(1.1/modres)^2)
     count=0
     a = n_elements(grid.grid[0,*,0,0,0])    ;;
     b = n_elements(grid.grid[0,0,*,0,0])    ;;
     c = n_elements(grid.grid[0,0,0,*,0])    ;;
     d = n_elements(grid.grid[0,0,0,0,*])    ;;
     counter,count,n_elements(grid.files),/timeleft,starttime=starttime

     for i=0,a-1,1 do $
        for j=0,b-1,1 do $
           for k=0,c-1,1 do $ 
              for l=0,d-1,1 do $
                 if (*self.models).grid.grid[0,i,j,k,l] then begin  
        widget_control,self.progbarB,set_value=(count*1d0)/n_elements(grid.files)
        
        img = mrdfits(grid.dir+grid.files[i,j,k,l],0,head,/silent)
        modfail = 0
        if max(img[where(img[*,0]/1d4 ge self.fitrange[0] and img[*,0]/1d4 le self.fitrange[1]),2]) gt 1.1 or $
           min(img[where(img[*,0]/1d4 ge self.fitrange[0] and img[*,0]/1d4 le self.fitrange[1]),2]) lt 0.0 then begin
                                ;  if (where(img[*,2] gt 1.1))[0] ne -1 then begin
                                ; img[where(img[*,2] gt 1.1),1] = sqrt(-1)
                                ; img = img[where(finite(img[*,1])),*]
           self->startplot,0
           plot,img[*,0]/1d4,img[*,2],/xs,/ys,color=(*self.colors).black,title=string('T=',((*self.models).grid.params.temps)[i],'         logg=',((*self.models).grid.params.gravs)[j],'           [Z]=', ((*self.models).grid.params.abuns)[k],'       xit=', ((*self.models).grid.params.turbs)[l],format='(a,i5,a,d5.2,a,d5.2,a,d3.1)')+' Model '+string(count,format='(i5)')+' of '+string(n_elements(grid.files),format='(i5)'),psym=10,xrange=mm(newwave)
           vline,self.fitrange,color=(*self.colors).blue
          
           self->endplot,0
          
           ;; try something new here
           self->message,message=string('',((*self.models).grid.params.temps)[i],' ',((*self.models).grid.params.gravs)[j],' ', ((*self.models).grid.params.abuns)[k],' ', ((*self.models).grid.params.turbs)[l],format='(a,i4,a,d+4.1,a,d+5.2,a,d3.1)')+' REMOVED',which='B'
             
           print,mm(img[where(img[*,0]/1d4 ge self.fitrange[0] and img[*,0]/1d4 le self.fitrange[1]),2])
           modfail = 1
         ;  stop
        endif
        ;;     img = img[where(img[*,0]/1d4 gt self.fitrange[0]-0.015 and img[*,0]/1d4 lt self.fitrange[1]+0.015),*]
        
        if modfail eq 0 then begin
           if (count mod 100) eq 0 then spec_conv5aa,img[*,0]/1d4,img[*,2],wave,flux,fwhm=fwhm,/quiet,lims=mm(newwave) else $
              spec_conv5aa,img[*,0]/1d4,img[*,2],wave,flux,fwhm=fwhm,/quiet,/noinfo,lims=mm(newwave)
           (*self.models).flux[*,i,j,k,l] = interpol(flux,wave,newwave)
           count++
                                ;  if count eq 11 then stop
           counter,count,n_elements(grid.files),/timeleft,starttime=starttime
           if (count mod 10) eq 0 or count eq 1 then begin
              self->startplot,0
              plot,(*self.models).wave,(*self.models).flux[*,i,j,k,l],/xs,/ys,color=(*self.colors).black,title=string('T=',((*self.models).grid.params.temps)[i],'         logg=',((*self.models).grid.params.gravs)[j],'           [Z]=', ((*self.models).grid.params.abuns)[k],'       xit=', ((*self.models).grid.params.turbs)[l],format='(a,i5,a,d5.2,a,d5.2,a,d3.1)')+' Model '+string(count,format='(i5)')+' of '+string(n_elements(grid.files),format='(i5)'),psym=10,xrange=mm(newwave),yrange=[0,1.01]
              vline,self.fitrange,color=(*self.colors).blue
              self->endplot,0
              if (where((*self.models).flux[where((*self.models).wave lt self.fitrange[1] and (*self.models).wave gt self.fitrange[0]),i,j,k,l] gt 1.1))[0] ne -1 then stop
           endif
           if stddev((*self.models).flux[*,i,j,k,l]) eq 0 then stop
        endif else (*self.models).flux[*,i,j,k,l]+=sqrt(-1)
        img = 0L
        
     endif else stop
     
     self->message,message=' ....done.  saving grid for posterity.'
     fullgrid = *self.models
     save,fullgrid,filename=saved_grid
     fullgrid = 0L
     self->message,message=' ....saved, moving on.'
     widget_control,self.progbarB,set_value=0



  endelse
  self->writetotex,'Model Grid [Actual] & '+last_element(strsplit(saved_grid,'/',/extract))+' \\'
  
  count = 0L
  grid = 0L
  dlam = 0L
  newwave = 0L
  saved_grid = 0L
  ;; help,/heap
  ;; stop
  if self.interpolate then self->interpolate_grid
  
end

pro specfit::quadlin,waverange=waverange
  self->message,message='Quad. Linear chi2 interpolation',which='B'

  widget_control,self.progbarB,set_value=0


  pars =  (*self.models).grid.params
  
  spy=0
  makestop=0

  csq = self->makechiarr(waverange)
  
  newT = fillarr(10,mm((*self.models).grid.params.temps))
  newg = fillarr(.1,mm((*self.models).grid.params.gravs))
  newz = fillarr(.05,mm((*self.models).grid.params.abuns))
  newx = fillarr(.1,mm((*self.models).grid.params.turbs))
  
  newcsq = dblarr(5,$
                  n_elements(newt),$
                  n_elements(newg),$
                  n_elements(newz),$
                  n_elements(newx))
  
  count=0d0
  for i=0,n_elements(newt)-1 do $
     for j=0,n_elements(newg)-1 do $
        for k=0,n_elements(newz)-1 do $
           for l=0,n_elements(newx)-1 do begin
     newcsq[1:4,i,j,k,l] = [newt[i],newg[j],newz[k],newx[l]]
     
     params = newcsq[1:4,i,j,k,l]
     ;; for i,j,k,l...
     count++
     counter,count,n_elements(newcsq[0,*,*,*,*])
     widget_control,self.progbarB,set_value=count/n_elements(newcsq[0,*,*,*,*])

     teff = 0
     logg = 1
     abun = 2
     turb = 3
     
     tbounds = pars.temps[(sort(abs(pars.temps-params[0])))[0:1]]
     gbounds = pars.gravs[(sort(abs(pars.gravs-params[1])))[0:1]]
     abounds = pars.abuns[(sort(abs(pars.abuns-params[2])))[0:1]]
     if n_elements(pars.turbs) ge 2 then mbounds = pars.turbs[(sort(abs(pars.turbs-params[3])))[0:1]] else mbounds = [pars.turbs,pars.turbs]

 ;;; MTG1:
;;
     usem = mbounds[0]   ;; mturb
     usea = abounds[0]   ;; abund
     uset = tbounds[0]
     useg = gbounds[0]
     l3 = [where(pars.temps eq uset),$
           where(pars.gravs eq useg),$
           where(pars.abuns eq usea),$
           where(pars.turbs eq usem)]
     l3p = [uset,useg,usea,usem]
     uset = tbounds[1]
     l4 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l4p = [uset,useg,usea,usem]
     useg = gbounds[1] 
     l5 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l5p = [uset,useg,usea,usem]
     uset = tbounds[0]
     l6 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l6p = [uset,useg,usea,usem]
     
     m3 = csq[l3[0],l3[1],l3[2],l3[3]]
     m4 = csq[l4[0],l4[1],l4[2],l4[3]]
     m5 = csq[l5[0],l5[1],l5[2],l5[3]]
     m6 = csq[l6[0],l6[1],l6[2],l6[3]]
     
     if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
     
     if spy then print,"m0 - m1 : m0p5"
     m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
     m3p5p = [params[teff],l3p[1:3]]
     if spy then begin
        plot,m3[1000:1200],psym=10
        oplot,m4[1000:1200],psym=10,color=colors.red
        oplot,m3p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     

     if spy then print,"m2 - m3 : m2p5"
     m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
     m5p5p = [params[teff],l5p[1:3]]
     if spy then begin
        plot,m5[1000:1200],psym=10,color=colors.black
        oplot,m6[1000:1200],psym=10,color=colors.red
        oplot,m5p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     mtg1 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
     ;;mtg2 = m3p5+((m3p5-m5p5)/(m3p5p[logg]-m5p5p[logg]))*(params[logg]-m5p5[logg])
     mtg1p = [params[teff],params[logg],usea,usem]
     if spy then begin
        plot,m3p5[1000:1200],psym=10
        oplot,m5p5[1000:1200],psym=10,color=colors.red
        oplot,mtg1[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif


  ;;; MTG2:
     ;;
     usem = mbounds[0] ;; mturb
     usea = abounds[1] ;; abund
     uset = tbounds[0]
     useg = gbounds[0]
     l3 = [where(pars.temps eq uset),$
           where(pars.gravs eq useg),$
           where(pars.abuns eq usea),$
           where(pars.turbs eq usem)]
     l3p = [uset,useg,usea,usem]
     uset = tbounds[1]
     l4 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l4p = [uset,useg,usea,usem]
     useg = gbounds[1] 
     l5 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l5p = [uset,useg,usea,usem]
     uset = tbounds[0]
     l6 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l6p = [uset,useg,usea,usem]
     
     m3 = csq[l3[0],l3[1],l3[2],l3[3]]
     m4 = csq[l4[0],l4[1],l4[2],l4[3]]
     m5 = csq[l5[0],l5[1],l5[2],l5[3]]
     m6 = csq[l6[0],l6[1],l6[2],l6[3]]
     if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
     
     if spy then print,"m0 - m1 : m0p5"
     m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
     m3p5p = [params[teff],l3p[1:3]]
     if spy then begin
        plot,m3[1000:1200],psym=10
        oplot,m4[1000:1200],psym=10,color=colors.red
        oplot,m3p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     

     if spy then print,"m2 - m3 : m2p5"
     m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
     m5p5p = [params[teff],l5p[1:3]]
     if spy then begin
        plot,m5[1000:1200],psym=10
        oplot,m6[1000:1200],psym=10,color=colors.red
        oplot,m5p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     ;; this is grav step 1
     ;; so X here is grav
     mtg2 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
     ;;mtg2 = m3p5+((m3p5-m5p5)/(m3p5p[logg]-m5p5p[logg]))*(params[logg]-m5p5[logg])
     mtg2p = [params[teff],params[logg],usea,usem]
     if spy then begin
        plot,m3p5[1000:1200],psym=10
        oplot,m5p5[1000:1200],psym=10,color=colors.red
        oplot,mtg2[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     ;; mtga1 is proper teff, grav, and abund, first microturb
     mtga1 = specfit_lint(mtg1,mtg1p[abun],mtg2,mtg2p[abun],params[abun])
     if spy then begin
        plot,mtg1[1000:1200],psym=10
        oplot,mtg2[1000:1200],psym=10,color=colors.red
        oplot,mtga1[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     mtga1p = [params[0:2],usem]
     
     
     ;; now need mtga2 at proper teff, grav, abund, second microturb
     ;; so first mtg3 = proper t,g, abund1, mt2
     
  ;;; MTG3:
     ;;
     usem = mbounds[1] ;; mturb
     usea = abounds[0] ;; abund
     uset = tbounds[0]
     useg = gbounds[0]
     l3 = [where(pars.temps eq uset),$
           where(pars.gravs eq useg),$
           where(pars.abuns eq usea),$
           where(pars.turbs eq usem)]
     l3p = [uset,useg,usea,usem]
     uset = tbounds[1]
     l4 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l4p = [uset,useg,usea,usem]
     useg = gbounds[1] 
     l5 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l5p = [uset,useg,usea,usem]
     uset = tbounds[0]
     l6 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l6p = [uset,useg,usea,usem]
     
     m3 = csq[l3[0],l3[1],l3[2],l3[3]]
     m4 = csq[l4[0],l4[1],l4[2],l4[3]]
     m5 = csq[l5[0],l5[1],l5[2],l5[3]]
     m6 = csq[l6[0],l6[1],l6[2],l6[3]]
     if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
     
     if spy then print,"m0 - m1 : m0p5"
     m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
     m3p5p = [params[teff],l3p[1:3]]
     if spy then begin
        plot,m3[1000:1200],psym=10
        oplot,m4[1000:1200],psym=10,color=colors.red
        oplot,m3p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     

     if spy then print,"m2 - m3 : m2p5"
     m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
     m5p5p = [params[teff],l5p[1:3]]
     if spy then begin
        plot,m5[1000:1200],psym=10,color=colors.black
        oplot,m6[1000:1200],psym=10,color=colors.red
        oplot,m5p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     ;; this is grav step 1
     ;; so X here is grav
     mtg3 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
     mtg3p = [params[teff],params[logg],usea,usem]
     if spy then begin
        plot,m3p5[1000:1200],psym=10
        oplot,m5p5[1000:1200],psym=10,color=colors.red
        oplot,mtg3[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     ;; second mtg4 = proper t,g, abund2, mt2
     
  ;;; MTG4:
   ;;; MTG3:
     ;;
     usem = mbounds[1] ;; mturb
     usea = abounds[1] ;; abund
     uset = tbounds[0]
     useg = gbounds[0]
     l3 = [where(pars.temps eq uset),$
           where(pars.gravs eq useg),$
           where(pars.abuns eq usea),$
           where(pars.turbs eq usem)]
     l3p = [uset,useg,usea,usem]
     uset = tbounds[1]
     l4 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l4p = [uset,useg,usea,usem]
     useg = gbounds[1] 
     l5 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l5p = [uset,useg,usea,usem]
     uset = tbounds[0]
     l6 =  [where(pars.temps eq uset),$
            where(pars.gravs eq useg),$
            where(pars.abuns eq usea),$
            where(pars.turbs eq usem)]
     l6p = [uset,useg,usea,usem]
     
     m3 = csq[l3[0],l3[1],l3[2],l3[3]]
     m4 = csq[l4[0],l4[1],l4[2],l4[3]]
     m5 = csq[l5[0],l5[1],l5[2],l5[3]]
     m6 = csq[l6[0],l6[1],l6[2],l6[3]]
     if max(m3) eq 0 or max(m4) eq 0 or max(m5) eq 0 or max(m6) eq 0 then stop
     
     
     if spy then print,"m0 - m1 : m0p5"
     m3p5 = specfit_lint(m3,l3p[teff],m4,l4p[teff],params[teff])
     m3p5p = [params[teff],l3p[1:3]]
     if spy then begin
        plot,m3[1000:1200],psym=10
        oplot,m4[1000:1200],psym=10,color=colors.red
        oplot,m3p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     

     if spy then print,"m2 - m3 : m2p5"
     m5p5 = specfit_lint(m5,l5p[teff],m6,l6p[teff],params[teff])
     m5p5p = [params[teff],l5p[1:3]]
     if spy then begin
        plot,m5[1000:1200],psym=10
        oplot,m6[1000:1200],psym=10,color=colors.red
        oplot,m5p5[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     ;; this is grav step 1
     ;; so X here is grav
     mtg4 = specfit_lint(m5p5,m5p5p[logg],m3p5,m3p5p[logg],params[logg])
     mtg4p = [params[teff],params[logg],usea,usem]
     if spy then begin
        plot,m3p5[1000:1200],psym=10
        oplot,m5p5[1000:1200],psym=10,color=colors.red
        oplot,mtg3[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     

     
     ;; mtga2 is proper teff, grav, and abund, second microturb
     mtga2 = specfit_lint(mtg3,mtg3p[abun],mtg4,mtg4p[abun],params[abun])
     if spy then begin
        plot,mtg1[1000:1200],psym=10
        oplot,mtg2[1000:1200],psym=10,color=colors.red
        oplot,mtga1[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     mtga2p = [params[0:2],usem]
     
     
     ;; now mtgat is linearily interpolated full ass model
     mtgat = specfit_lint(mtga1,mtga1p[turb],mtga2,mtga2p[turb],params[turb])
     mtgatp = params
     if spy then begin
        plot,mtga1[1000:1200],psym=10
        oplot,mtga2[1000:1200],psym=10,color=colors.red
        oplot,mtgat[1000:1200],psym=10,color=colors.blue
        if debug then stop
     endif
     
     newcsq[0,i,j,k,l] = mtgat
     
     ;; iwave = (*self.models).wave
     if keyword_set(makestop) then stop
     
     
  end
  self->message,message=' Finished',which='B'

  
  stop

  
  
end

pro specfit::interpolate_grid
  self->message,message='Interpolating to Lock Grid',which='B'
  widget_control,self.progbarB,set_value=0
  
  ;; help,*self.models
  ;; help,*self.models,/struct
  ;; stop
  
  ;;; define new params struct:
  t = (*self.models).grid.params.temps
  g = (*self.models).grid.params.gravs
  z = (*self.models).grid.params.abuns
  x = (*self.models).grid.params.turbs
  
  ;; if LOCKED:
  if self.lock_param[0] then t = self.lock_values[0]
  if self.lock_param[1] then g = self.lock_values[1]
  if self.lock_param[2] then z = self.lock_values[2]
  if self.lock_param[3] then x = self.lock_values[3]

  ;;if AVOID:
  if self.turb_avoid ne 0 then x = x[where(x ne self.turb_avoid)]
  
;  stop

  newparams = {temps: t,$
               gravs: g,$
               abuns: z,$
               turbs: x}

  newgrid = dblarr(5,n_elements(t),n_elements(g),n_elements(z),n_elements(x))
  
  newmodels = {gridfile: 'interpolated from '+(*self.models).gridfile,$
               grid: {dir: '',$
                      params: newparams,$
                      grid: newgrid,$
                      complete: 1},$
               res:  (*self.models).res,$
               wave: (*self.models).wave,$
               flux: dblarr(n_elements((*self.models).wave),n_elements(t),n_elements(g),n_elements(z),n_elements(x))}

  a = n_elements(t)
  b = n_elements(g)
  c = n_elements(z)
  d = n_elements(x)
  co=0d0
  for i=0,a-1 do $
     for j=0,b-1 do $
        for k=0,c-1 do $
           for l=0,d-1 do begin
     ;; is there a perfect match?  
     co++
     match = intarr(4)
     wt = where((*self.models).grid.params.temps-newparams.temps[i] eq 0,count)
     match[0] = min([1,count])
     wg = where((*self.models).grid.params.gravs-newparams.gravs[j] eq 0,count)
     match[1] = min([1,count])
     wz = where((*self.models).grid.params.abuns-newparams.abuns[k] eq 0,count)
     match[2] = min([1,count])
     wx = where((*self.models).grid.params.turbs-newparams.turbs[l] eq 0,count)
     match[3] = min([1,count])
     
     ;; yes, there is a match:  use exact flux
     if min(match) then newmodels.flux[*,i,j,k,l] = (*self.models).flux[*,wt,wg,wz,wx] else begin 
        ;; no match, linear interpolate flux:
        self->lininterp,[t[i],g[j],z[k],x[l]],iwave,iflux
                                ;   print,t[i],g[j],z[k],x[l]
        if max((*self.models).wave - iwave) ne 0 then stop
        newmodels.flux[*,i,j,k,l] = iflux
                                ;    stop
     endelse
     
     widget_control,self.progbarB,set_value=co/n_elements(newmodels.flux[0,*,*,*,*])
  endfor
  match = 0L
  
  ptr_free,self.models
  self.models = ptr_new(newmodels)
  newmodels = 0L
  
  self->message,message='..... DONE',which='B'
  widget_control,self.progbarB,set_value=0
  
  
                                ; o = 'HD14469'
                                ; specfit,cont_order=2,outdir="SF_obj_"+o+"_TESTING_v4.02",obj_dir="Cobj_"+o,model_grid='/Users/zgazak/Projects/MassiveStars/RSGs/SSC/RSG_MARCS/MARCS_20120822_J.sav',lock_array=[1,1,0,0],lock_values=[3750,0.0,0,0],/auto_start

  
end

pro specfit::writetotex,string,start=start,close=close,s_table=s_table,c_table=c_table,output_report=output_report,$
                        string2=string2,clos_output=clos_output,begin_bigtable=begin_bigtable,end_bigtable=end_bigtable,$
                        write_tobig=write_tobig
  
  
end

pro specfit::adjust_lim,event
  widget_control, event.id, GET_UVALUE= uvalue
  widget_control, /Hourglass

  case uvalue.value of
     'lam_min': self.fitrange[0]=*event.value
     'lam_max': self.fitrange[1]=*event.value
     'lock_param': self.lock_values[uvalue.loc] = *event.value
     else: stop
  endcase
  if self.diag then print,self.fitrange
  if self.diag then print,self.lock_values,self.lock_param
end

function specfit::switcher,value
  if value eq 0 then return,1
  return,0
end

pro specfit::ButtonEvent,event,manual=manual
  if keyword_set(manual) then uvalue = event else Widget_Control, event.id, Get_UValue=uvalue
  widget_control, /Hourglass
  
  case uvalue.value of 
     'radio': begin
        case uvalue.type of
           'run_full': self.run_full = self->switcher(self.run_full)
           'run_diag': self.run_diag = self->switcher(self.run_diag)
           'run_diag_focus': self.run_diag_focus = self->switcher(self.run_diag_focus)
           'run_pixel': self.run_pixel = self->switcher(self.run_pixel)
           'lock_param': self.lock_param[uvalue.loc] = self->switcher(self.lock_param[uvalue.loc])
        end
        if self.diag then print,self.run_full,self.run_diag
     end
     'Setup File Button':begin
        path = dialog_pickfile(dialog_parent=(*self.bases)[self->base('Setup')],title='Select File')
        if path ne '' then begin
           widget_control,(*self.fields)[uvalue.field,1],set_value=path
           widget_control,(*self.buttons)[uvalue.button],sensitive=1
        endif 
     end
     'Clear Path': begin
        widget_control,(*self.fields)[uvalue.field,1],set_value=''
        if uvalue.button ne 99 then widget_control,(*self.buttons)[uvalue.button],sensitive=0
     end  
     'Output Dir Sel':begin
        path = dialog_pickfile(dialog_parent=(*self.bases)[self->base('Setup')],title='Select File',/directory)
        widget_control,(*self.fields)[uvalue.field,1],set_value=path
        ;; widget_control,(*self.buttons)[uvalue.button],sensitive=1
        self.outdir = path
        widget_control,self.master_base,base_set_title='specFit :: '+self.version+' :: '+last_element(strsplit(self.outdir,'/',/extract))
     end
     'Output Dir':begin
        widget_control,(*self.fields)[uvalue.field,1],get_value=path
        self.outdir = path
        widget_control,self.master_base,base_set_title='specFit :: '+self.version+' :: '+last_element(strsplit(self.outdir,'/',/extract))
        self->message,message='Output directory set',which='A'
     end
     'Load Grid': begin
        widget_control,(*self.fields)[uvalue.field,1],get_value=path
        if file_test(path) then begin
           self.modgrid = path
           self->message,message='Model grid set',which='A'
        endif else begin
           self->message,message='Model grid does not exist... resetting.',which='A'
           widget_control,(*self.fields)[uvalue.field,1],set_value=self.modgrid
        endelse
     end
     'Pather':begin
        path = dialog_pickfile(dialog_parent=(*self.bases)[self->base('Setup')],title='Select File')
        if path ne '' then begin
           widget_control,(*self.fields)[uvalue.field,1],set_value=path
           widget_control,(*self.buttons)[uvalue.button],sensitive=1
           self.modgrid = path
        endif 
     end
     'SpecfitDir Button':begin
        path = dialog_pickfile(dialog_parent=(*self.bases)[self->base('Setup')],title='Select Directory',/directory)
        if path ne '' then begin
           widget_control,(*self.fields)[uvalue.field,1],set_value=path
           widget_control,(*self.buttons)[uvalue.button],sensitive=1
           self.directory = path
        endif 
     end
     'Set Spec Dir': begin
        spawn,'ls '+self.directory+'/*.fits',files
        if n_elements(files) ne 0 then begin
           
           self->message,message='Found '+string(n_elements(files),format='(i3)')+' Fits Spectra',which='A'
           self->arrange_objects
           spawn,'pwd',cdir
           cd,self.directory
           spawn,'pwd',dir
           self.directory = dir
           cd,cdir
           cdir = 0L
           dir = 0L
           ;;     self->makeobject,(*self.objects)[0]
        endif else begin
           self->message,message='No fits format spectra found!',which='A'
        endelse
        files = 0L
     end
     'Run Fit': begin
        self->run_button_code
     end
     '': print,'blank button event!'
     else: print,"unknown button event: '"+uvalue.value+"'"
  endcase
end

pro specfit::run_button_code
  spawn,'pwd',curdir
  spawn,'mkdir '+self.outdir     
  cd,self.outdir
  spawn,'pwd',tdir
  self.outdir = tdir
  tdir=0L
  spawn,'mkdir original_fits'
  spawn,'mkdir output_fits'
  spawn,'cp -r '+self.directory+'/*.fits original_fits/.'
  spawn,'mv '+curdir+'/temp_specfit_log_A.txt specfit_log_A.txt'   
  spawn,'mv '+curdir+'/temp_specfit_log_B.txt specfit_log_B.txt'
  close,/all
                                ; stop
  openw,lun,'./specfit_log_A.txt',/get_lun,width=2500,bufsize=0,/append 
  openw,lun2,'./specfit_log_B.txt',/get_lun,width=2500,bufsize=0,/append 
  self.logfileA = lun
  self.logfileB = lun2
  
  
  if self.skipsaves then   openw,lun,'specfit_report.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'specfit_report.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[0] = lun
  
  
  if self.skipsaves then   openw,lun,'specfit_report_rev_range_nomg.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'specfit_report_rev_range_nomg.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[1] = lun
  self->writetoascii,'#                               obj Lam_start   Lam_end      teff  sigt      grav   sigg         z   sigz      turb  sig_turb    Rexpect       Rfit   runtype',1
  
  if self.skipsaves then   openw,lun,'specfit_report_rev_range_nomg_best.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'specfit_report_rev_range_nomg_best.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[2] = lun
  self->writetoascii,'#            obj            Lam_start  Lam_end   teff   grav    z     turb  min_chi   Rexpect   Rfit     runtype',2
  
  if self.skipsaves then   openw,lun,'specfit_report_nomg_grav_all.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'specfit_report_nomg_grav_all.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[3] = lun
  self->writetoascii,'#            obj            Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype',3
  
  if self.skipsaves then   openw,lun,'specfit_report_nomg_grav_avg.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'specfit_report_nomg_grav_avg.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[4] = lun
  self->writetoascii,'#            obj            Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype',4
  
  if self.skipsaves then   openw,lun,'sf_nomg_mcerr.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'sf_nomg_mcerr.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[5] = lun
  self->writetoascii,'#            obj            Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype',5
  
  if self.skipsaves then   openw,lun,'sf_nomg_mcerr_dat.txt',/get_lun,width=2500,bufsize=0,/append else $
     openw,lun,'sf_nomg_mcerr_dat.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[9] = lun
  self->writetoascii,'#            obj            Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype',9
  
  openw,lun,'sf_byparam.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[6] = lun
  self->writetoascii,'#            obj                 Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype                   minchi',6
  
  
  openw,lun,'sf_best10.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[7] = lun
  self->writetoascii,'#            obj                 Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype                   minchi',7
  
  openw,lun,'sf_bestwithin10perc.txt',/get_lun,width=2500,bufsize=0
  self.asciifile[8] = lun
  self->writetoascii,'#            obj                 Lam_start  Lam_end     teff   sigt     grav   sigg        z    sigz      turb  sig_turb     Rexpect     Rfit    runtype                   minchi',8
  
  
  
  ;; menu remapping...
  for i=0,n_elements(*self.bases)-1,1 do if (*self.bases)[i] ne 0 then widget_control,(*self.bases)[i],MAP=0  
  self->setup,'plotting' 
  widget_control,(*self.bases)[self->base('Plotting')],/map
  
  ;; self->setupgrid
  ;; self->initialfits  MOVED TO ::makeobject
  
  self->arrange_objects
  
  
  
;  self->message,message='Parameter locks:',which='A'
  self->message,message='(lock)   T   logg    Z    xi',which='A'
  self->message,message='(lock)'+string(self.lock_param,format='(i4,i6,i6,i5)'),which='A'
  self.lock_values[where(self.lock_param eq 0)] = sqrt(-1)
  self->message,message='(lock)'+string(self.lock_values,format='(i5,d6.2,d6.2,d5.1)'),which='A'
  if max(self.lock_param) then self.interpolate = 1


  self->control_run
  
  for i=0,n_elements(*self.bases)-1,1 do if (*self.bases)[i] ne 0 then widget_control,(*self.bases)[i],MAP=0  
  self->setup,'fittingoptions' 
  widget_control,(*self.bases)[self->base('Fitting Options')],/map
  
  cd,curdir
  curdir = 0L
end

pro specfit::setup,set_this
  case set_this of
     'plotwins': begin
        (*self.plot_windows)[0].x = 1120d0
        (*self.plot_windows)[0].y = 350d0
        (*self.plot_windows)[0].window = widget_draw((*self.layout_bases)[0] $
                                                     , xsize=(*self.plot_windows)[0].x $
                                                     , ysize=(*self.plot_windows)[0].y $
                                                     , uvalue='Plot Window 1')
        
        (*self.plot_windows)[1].x = 270d0
        (*self.plot_windows)[1].y = 250d0
        (*self.plot_windows)[1].window = widget_draw((*self.layout_bases)[5] $
                                                     , xsize=(*self.plot_windows)[1].x $
                                                     , ysize=(*self.plot_windows)[1].y $
                                                     , uvalue='Plot Window 2')

        (*self.plot_windows)[2].x = 270d0
        (*self.plot_windows)[2].y = 250d0
        (*self.plot_windows)[2].window = widget_draw((*self.layout_bases)[5] $
                                                     , xsize=(*self.plot_windows)[2].x $
                                                     , ysize=(*self.plot_windows)[2].y $
                                                     , uvalue='Plot Window 3')
     end
     'plotting': begin
        if (*self.bases)[self->base('Plotting')] eq 0 then begin
           (*self.bases)[self->base('Plotting')] = widget_base((*self.setbases)[self->setbase('menus')],/column,MAP=0,frame=1)
           
           (*self.plot_windows)[3].x = 530d0
           (*self.plot_windows)[3].y = 250d0
           (*self.plot_windows)[3].window = widget_draw((*self.bases)[self->base('Plotting')]  $
                                                        , xsize=(*self.plot_windows)[3].x $
                                                        , ysize=(*self.plot_windows)[3].y $
                                                        , uvalue='Plot Window 4')
           
        endif 
     end
     'setup': begin
        ys  = 80
        xsp = 43
        xsl = 52
        (*self.bases)[self->base('Setup')] = widget_base((*self.setbases)[self->setbase('menus')],/column,MAP=0,frame=0)
        
        label = widget_label((*self.bases)[self->base('Setup')],value='Restore Existing Setup:',$
                             font=self.buttonfont,/align_left)
        bigcol = widget_base((*self.bases)[self->base('Setup')],/column,/Base_align_center,frame=1)
        bigrow =  widget_base(bigcol,/column,/Base_align_center,frame=0)
        row = widget_base( bigrow,/ROW,/Base_align_center,frame=0)
        
        button = widget_button(row,font=self.buttonfont,xsize=120,value='Existing Savefile',$
                               uvalue={object:self, $
                                       method:'ButtonEvent', $
                                       value:'Setup File Button',$
                                       field:1, $
                                       button: 1})
        
        path = coyote_field2(row,labelfont=self.buttonfont,FIELDFONT=self.textfont,$
                             TITLE=':',VALUE = (*self.paths)[1],UVALUE='Save File Field',$
                             XSIZE=xsp,TEXTID=textid)
        (*self.fields)[1,*] = [path,textid]
        
        clear = widget_button(row,FONT=self.buttonfont,VALUE='Clear',xsize=50,$
                              uvalue={object:self, $
                                      method:'ButtonEvent', $
                                      value:'Clear Path', $
                                      field: 1, $
                                      button: 1})
        
        (*self.buttons)[1] =  widget_button(row, font=self.buttonfont, xsize=50, value='Load',$
                                            uvalue={object:self, $
                                                    method:'ButtonEvent', $
                                                    value:'Load Savefile'},sensitive=1)
        
        

        label = widget_label((*self.bases)[self->base('Setup')],value='Prepare New Setup:',$
                             font=self.buttonfont,/align_left)
        
        bigcol = widget_base( (*self.bases)[self->base('Setup')],/column,/Base_align_center,frame=1)
        bigrow =  widget_base(bigcol,/column,/Base_align_left,frame=0)
        row = widget_base( bigrow,/ROW,/Base_align_center,frame=0)
        
        button = widget_button(row,font=self.buttonfont,xsize=120,value='Input Spec Dir',$
                               uvalue={object:self, $
                                       method:'ButtonEvent', $
                                       value:'SpecfitDir Button',$
                                       field:0, $
                                       button: 0})
        
        (*self.paths)[0] = self.directory
        path = coyote_field2(row,labelfont=self.buttonfont,FIELDFONT=self.textfont,$
                             TITLE=':',VALUE = (*self.paths)[0],UVALUE='Spec Dir Field',$
                             XSIZE=xsp,TEXTID=textid)
        (*self.fields)[0,*] = [path,textid]
        
        clear = widget_button(row,FONT=self.buttonfont,VALUE='Clear',xsize=50,$
                              uvalue={object:self, $
                                      method:'ButtonEvent', $
                                      value:'Clear Path', $
                                      field: 0, $
                                      button: 0})
        
        (*self.buttons)[0] =  widget_button(row, font=self.buttonfont, xsize=50, value='Load',$
                                            uvalue={object:self, $
                                                    method:'ButtonEvent', $
                                                    value:'Set Spec Dir'},sensitive=1)
        
        
                                ;  bigrow =  widget_base( (*self.bases)[self->base('Setup')],/column,/Base_align_center,frame=1)
        row = widget_base( bigrow,/ROW,/Base_align_center,frame=0)
        
        button = widget_button(row,font=self.buttonfont,xsize=120,value='Model Grid',$
                               uvalue={object:self, $
                                       method:'ButtonEvent', $
                                       value:'Pather',$
                                       field:2, $
                                       button: 2})
        (*self.paths)[2] = self.modgrid

        path = coyote_field2(row,labelfont=self.buttonfont,FIELDFONT=self.textfont,$
                             TITLE=':',VALUE = (*self.paths)[2],UVALUE='Model Grid Field',$
                             XSIZE=xsp,TEXTID=textid)
        (*self.fields)[2,*] = [path,textid]
        
        clear = widget_button(row,FONT=self.buttonfont,VALUE='Clear',xsize=50,$
                              uvalue={object:self, $
                                      method:'ButtonEvent', $
                                      value:'Clear Path', $
                                      field: 2, $
                                      button: 99})
        
        if 1 then (*self.buttons)[2] =  widget_button(row, font=self.buttonfont, xsize=50, value='Set',$
                                                      uvalue={object:self, $
                                                              method:'ButtonEvent', $
                                                              value:'Load Grid',$
                                                              field: 2},sensitive=1)
        row = widget_base( bigrow,/ROW,/Base_align_center,frame=0)
        
        button = widget_button(row,font=self.buttonfont,xsize=120,value='Output Dir',$
                               uvalue={object:self, $
                                       method:'ButtonEvent', $
                                       value:'Output Dir Sel',$
                                       field:3, $
                                       button: 3})
        
        (*self.paths)[3] = self.outdir
        path = coyote_field2(row,labelfont=self.buttonfont,FIELDFONT=self.textfont,$
                             TITLE=':',VALUE = (*self.paths)[3],UVALUE='Output Dir Field',$
                             XSIZE=xsp,TEXTID=textid)
        (*self.fields)[3,*] = [path,textid]
        
        clear = widget_button(row,FONT=self.buttonfont,VALUE='Clear',xsize=50,$
                              uvalue={object:self, $
                                      method:'ButtonEvent', $
                                      value:'Clear Path', $
                                      field: 3, $
                                      button: 99})
        if 1 then (*self.buttons)[2] =  widget_button(row, font=self.buttonfont, xsize=50, value='Set',$
                                                      uvalue={object:self, $
                                                              method:'ButtonEvent', $
                                                              value:'Output Dir',$
                                                              field: 3},sensitive=1)
        
     End
     'fittingoptions': begin
        if (*self.bases)[self->base('Fitting Options')] eq 0 then begin
           ;; build base, it does not yet exist...
                                ;print,(*self.bases)[self->base('Fitting Options')]
                                ;stop
           
           (*self.bases)[self->base('Fitting Options')] = widget_base((*self.setbases)[self->setbase('menus')],/column,MAP=0)
           bigcol = widget_base( (*self.bases)[self->base('Fitting Options')],/column,/Base_align_center,frame=1)

           row=widget_base(bigcol,/row,frame=1)
           label = widget_label(row,value='Wave range [um]:',font=self.buttonfont,/align_left)
           ;;  radio = widget_base(row,column=1,/nonexclusive)
           (*self.infields)[self->match('infield','lam_min')] = coyote_field2(row,$
                                                                              LABELFONT=self.buttonfont,$
                                                                              FIELDFONT=self.textfont,$
                                                                              TITLE='',$
                                                                              UVALUE={object:self, method:'adjust_lim',$
                                                                                      value:'lam_min',$
                                                                                      type:'waverange'},$
                                                                              VALUE=self.fitrange[0],$
                                                                              /positive,$
                                                                              XSIZE=6,$
                                                                              scr_ysize=30,$
                                                                              event_pro='specfit_event',$
                                                                              textid=textid)
           label = widget_label(row,value=' to ',font=self.buttonfont,/align_left)
           
           (*self.infields)[self->match('infield','lam_max')] = coyote_field2(row,$
                                                                              LABELFONT=self.buttonfont,$
                                                                              FIELDFONT=self.textfont,$
                                                                              TITLE='',$
                                                                              UVALUE={object:self, method:'adjust_lim',$
                                                                                      value:'lam_max',$
                                                                                      type:'waverange'},$
                                                                              VALUE=self.fitrange[1],$
                                                                              /positive,$
                                                                              XSIZE=6,$
                                                                              scr_ysize=30,$
                                                                              event_pro='specfit_event',$
                                                                              textid=textid)
           bigrow = widget_base(bigcol,/row,frame=0)
           
           smallcol = widget_base(bigrow,/column,frame=1)     
           

           label = widget_label(smallcol,value='Chi-sq Options:',font=self.buttonfont,/align_left)

           row = widget_base(smallcol,/row)
           radio = widget_base(row,/column,/nonexclusive)
           button = widget_button(radio,value='Full Spectrum',$ 
                                  uvalue={object:self, method:'buttonevent',$
                                          value: 'radio',type:'run_full'})
           widget_control,button,set_button=self.run_full
           
           button = widget_button(radio,value='Diagnostic Lines',$ 
                                  uvalue={object:self, method:'buttonevent',$
                                          value: 'radio',type:'run_diag'})
           widget_control,button,set_button=self.run_diag
           row = widget_base(smallcol,/row)
           
           button = widget_button(radio,value='Line Focus',$ 
                                  uvalue={object:self, method:'buttonevent',$
                                          value: 'radio',type:'run_diag_focus'})
           widget_control,button,set_button=self.run_diag_focus
           row = widget_base(smallcol,/row)
           
           button = widget_button(radio,value='Pixel Scan',$ 
                                  uvalue={object:self, method:'buttonevent',$
                                          value: 'radio',type:'run_pixel'})
           widget_control,button,set_button=self.run_pixel
           


           smallcol = widget_base(bigrow,/column,frame=1)     
           label = widget_label(smallcol,value='Parameter Locks:',font=self.buttonfont,/align_left)

           labels=['Teff','log g','[Z]','xi']
           ys = 35
           for t=0,3 do begin
              row = widget_base(smallcol,/row,ysize=ys,frame=0)
              radio = widget_base(row,/column,/nonexclusive)
              button = widget_button(radio,value=labels[t],$ 
                                     uvalue={object:self, method:'buttonevent',$
                                             value: 'radio',type:'lock_param',loc:t})
              widget_control,button,set_button=self.lock_param[t]
              (*self.infields)[self->match('infield','lock_teff')] = coyote_field2(row,$
                                                                                   LABELFONT=self.buttonfont,$
                                                                                   FIELDFONT=self.textfont,$
                                                                                   TITLE='',$
                                                                                   UVALUE={object:self, method:'adjust_lim',$
                                                                                           value:'lock_param',$
                                                                                           loc: t },$
                                                                                   VALUE=self.lock_values[t],$
                                                                                   /positive,$
                                                                                   XSIZE=6,$
                                                                                   /cr_only,$
                                                                                   scr_ysize=30,$
                                                                                   event_pro='specfit_event',$
                                                                                   textid=textid)
           endfor
           
           labels=0L


           

           button = widget_button(bigcol,$
                                  font = self.buttonfont,$
                                  value = 'Run Fit',$
                                  uvalue={object:self,$ 
                                          method:'ButtonEvent',$
                                          value:'Run Fit'})
        endif else begin
           ;; first setup has been done -- base exists
                                ;stop
        endelse
     end
     'quit': begin
        xs = 80
        if (*self.bases)[self->base('Quit')] eq 0 then begin
           ;; this is FULL SETUP
           (*self.bases)[self->base('Quit')] = widget_base((*self.setbases)[self->setbase('menus')], /column, MAP=0)
           label = widget_label((*self.bases)[self->base('Quit')],$
                                value='Really Quit?',$
                                font=self.buttonfont,$
                                /align_center,xsize=xs)
           button = widget_button((*self.bases)[self->base('Quit')],$
                                  font = self.buttonfont,$
                                  value = 'Quit',$
                                  uvalue={object:self, method:'Quit'})
        endif else begin 
           ;; for now do nothing...
        endelse
     end
     else: stop
  endcase
end

pro specfit::widget_setup
  
  ;; master base is a column
  title = 'specFit :: '+self.version+' :: '+last_element(strsplit(self.outdir,'/',/extract))
  self.master_base = widget_base(title=title,/column)
  title=0L

  XManager, 'specfit' $
            , self.master_base $
            , /no_block $
            , cleanup = 'specfit_cleanup'
  
  ;; quit button base
  ;; wide column across the top
  
  frame = 0
  
  if 0 then begin
     (*self.layout_bases)[0] = widget_base(self.master_base,/column,frame=frame)
     
     
     button = widget_button((*self.layout_bases)[0],$
                            font = self.buttonfont,$
                            value = 'Quit',$
                            uvalue={object:self, method:'Quit'})
  endif
  
  (*self.setbases_id)[0:4] = ['master',$
                              'message',$
                              'menus',$
                              'messageB',$
                              'plotting']
  
  ;; plot window 1 base
  (*self.layout_bases)[1] = widget_base(self.master_base,/column,frame=frame)
  
  ;; lb0 actually holds plot and has a frame
  (*self.layout_bases)[0] = widget_base((*self.layout_bases)[1],/column,frame=1)


  ;; layout_base2 is the bottom 2 panels... square plots, messages,
  ;; menu big frame... it is a row base and has two subbases, lb3 and lb4
  (*self.layout_bases)[2] = widget_base(self.master_base,/row,frame=frame)
  
  ;; lb3 holds lb5 and lb6...
  (*self.layout_bases)[3] = widget_base((*self.layout_bases)[2],/column,frame=1)
  
  
  ;; lb4 holds the menu map and menus...
  (*self.layout_bases)[4] = widget_base((*self.layout_bases)[2],/column,frame=1)
  
  ;; lb5 holds two plot windows for square plots of lindividual lines
  ;; etc... it is a row base
  (*self.layout_bases)[5] = widget_base((*self.layout_bases)[3],/row,frame=frame)
  
  ;; lb6 holds lb7 and lb8 which will be for messages and progress bars
  (*self.layout_bases)[6] = widget_base((*self.layout_bases)[3],/row,frame=frame)
  ;; lb7 holds messageA and progbarA
  (*self.layout_bases)[7] = widget_base((*self.layout_bases)[6],/column,frame=frame)
  
  message = 'Initializing code...'
  (*self.setbases)[1] = widget_text((*self.layout_bases)[7], $
                                    font = self.textfont, $
                                    value = message, $
                                    /scroll, $
                                    ysize=6)
  
  self.progbarA = cw_progress((*self.layout_bases)[7],obj_ref=A,/blue,ysize=10d,xsize=250d)     
  
  ;; lb8 holds messageB and progbarB
  (*self.layout_bases)[8] = widget_base((*self.layout_bases)[6],/column,frame=frame)
  
  (*self.setbases)[3] = widget_text((*self.layout_bases)[8], $
                                    font = self.textfont, $
                                    value = message, $
                                    /scroll, $
                                    ysize=6)
  message = 0L
  self.progbarB = cw_progress((*self.layout_bases)[8],obj_ref=B,/blue,ysize=10d,xsize=250d)     
  
  ;; lb4 holds the menubar and then the menu...

  (*self.setbases)[self->setbase('menus')]  = widget_base((*self.layout_bases)[4],frame=0)


  menubar = widget_base((*self.layout_bases)[4],/row)
  
  row = widget_base(menubar,$
                    /row,$
                    /toolbar,$
                    /exclusive,$
                    /base_align_center)
  
  for i=0,n_elements(*self.menus)-1 do begin
     button = widget_button(row,$
                            value=' '+(*self.menus)[i]+' ',$
                            uvalue={object:self, method:'menumap', value:(*self.menus)[i], type:'main'},$
                            /no_release,$
                            font=self.buttonfont)
     if i eq 0 then widget_control, button, /SET_BUTTON
  endfor
  
                                ;(*self.setbases)[0]  = widget_base(self.master_base,/column,frame=1,event_pro='rlris_event',/base_align_center)
                                ;row1 = widget_base((*self.setbases)[0],/row,/align_center)
                                ;self.progbar= cw_progress(row1,obj_ref=y,/blue,ysize=10d,xsize=1000d)     
  
                                ;stop 
  
  self->setup,'fittingoptions'
  self->setup,'setup'
  self->setup,'plotwins'
  
  
end

function specfit::match,type,match
  case type of 
     'infield': return,where(strcmp((*self.infields_id),match))
     else: begin
        print,"unknown match type: "+type
        stop
     endelse
  endcase
end

function specfit::base,match
  return,where(strcmp((*self.bases_id),match))
end

function specfit::infield,match
  return,where(strcmp((*self.infields_id),match))
end

function specfit::setbase,match
  return,where(strcmp((*self.setbases_id),match))
end


pro specfit::resetchisq
  (*self.active_object).chisq*=0
  (*self.active_object).chisq+=abs(sqrt(-1))
  (*self.active_object).modcount = 0d0
  (*self.active_object).goodmodcount = 0d0 
  self.best_chi = [1d20,-1]
  self.curr_chi = 1d20
end



pro specfit::location
  if (self.inext)[0] ne -1 then begin
     (*self.location) = array_indices((*self.active_object).chisq,self.inext)
     if n_elements(*self.location) eq 3 then *self.location = [*self.location,0]
;  stop
  endif else *self.location=-1
end

pro specfit::bestpar,finalres=finalres,normpar=normpar
  ;;  stop
  
  if 1-keyword_set(normpar) then begin
     (*self.active_object).bestpar = 'T='+string((*self.models).grid.params.temps[(*self.location)[0]],format='(i4)')+$
                                     ' g='+string((*self.models).grid.params.gravs[(*self.location)[1]],format='(d5.2)')+$
                                     ' [Z]='+string((*self.models).grid.params.abuns[(*self.location)[2]],format='(d5.2)')+$
                                     ' xit='+string((*self.models).grid.params.turbs[(*self.location)[3]],format='(d3.1)')
     
     (*self.active_object).bestloc = (*self.location)
     
     
     shortpar =  string((*self.models).grid.params.temps[(*self.location)[0]],format='(i4)')+$
                 ' '+string((*self.models).grid.params.gravs[(*self.location)[1]],format='(d5.2)')+$
                 ' '+string((*self.models).grid.params.abuns[(*self.location)[2]],format='(d5.2)')+$
                 ' '+string((*self.models).grid.params.turbs[(*self.location)[3]],format='(d3.1)')
     
                                ; self.message = string(self.best_chi[0],format='(d7.2)')+' '+$
                                ;                string(self.best_chi[2],format='(d7.2)')+' '+$
                                ;                string(self.best_chi[2]/self.best_chi[0],format='(d6.4)')+' '+$
                                ;                $              ;  string(self.inext,format='(i5)')+' '+$
     self.message = shortpar+' '+$
                    string((*self.active_object).modcount,format='(i5)')
     if keyword_set(finalres) then self.message += ' ('+string(finalres,format='(i5)')+')'
     
                                ;print,self.message
     self->message,which='B'
     if (*self.active_object).lockres then self->plot2d
  endif
  if 1-keyword_set(normpar) then shortpar = '   ' else shortpar = ''
  shortpar +=  string((*self.models).grid.params.temps[(*self.location)[0]],format='(i4)')+$
               ' '+string((*self.models).grid.params.gravs[(*self.location)[1]],format='(d5.2)')+$
               ' '+string((*self.models).grid.params.abuns[(*self.location)[2]],format='(d5.2)')+$
               ' '+string((*self.models).grid.params.turbs[(*self.location)[3]],format='(d3.1)')+$
               ' '+string(self.curr_chi,format='(d7.2)')+' '+string(self.curr_chi/self.best_chi[0],format='(d5.2)')
  
                                ; self.message = string(self.best_chi[0],format='(d7.2)')+' '+$
                                ;                string(self.best_chi[2],format='(d7.2)')+' '+$
                                ;                string(self.best_chi[2]/self.best_chi[0],format='(d6.4)')+' '+$
                                ;                $              ;  string(self.inext,format='(i5)')+' '+$
  self.message = shortpar+' '+$
                 string((*self.active_object).modcount,format='(i5)')

  if keyword_set(finalres) then self.message += ' ('+string(finalres,format='(i5)')+')'+' '+string(self.best_chi[0],format='(d7.2)')
                                ;print,self.string
                                ; print,self.message

;;      self->message,which='B'
  ;; print,''
  

end


pro specfit::activeindex
  if (*self.active_object).modcount lt (size(self.initialfits))[2] then begin
     ;; pick the next model to select...
                                ;  if (*self.active_object).modcount eq 0 and self.res_locked then $
                                ;     ifit = (*self.active_object).bestloc else $
     ifit = self.initialfits[*,(*self.active_object).modcount]
     if min(ifit) lt 0 then self.inext = -1 else $
        self.inext = ((*self.active_object).csq_ind)[ifit[0],ifit[1],ifit[2],ifit[3]] ;else $
                                ;   self.inext = ((*self.active_object).csq_ind)[ifit[0],ifit[1],ifit[2]]
  endif else begin
     ;; pick the best edge model
     (*self.active_object).chisq_smooth = smooth((*self.active_object).chisq,3,/nan,/edge)
     iedge = where(finite((*self.active_object).chisq_smooth) and finite((*self.active_object).chisq) eq 0)
     minedge = min(((*self.active_object).chisq_smooth)[iedge],inext)
     self.inext = iedge[inext]
     if (array_indices((*self.active_object).chisq,self.inext))[2] gt 9 then stop
                                ;   stop
     iedge = 0L
     minedge = 0L
  endelse
  self->location
end

pro specfit::bestres
  minchi = min((*self.active_object).chisq,place,/nan)
  (*self.active_object).overallres = ((*self.active_object).bestres)[place]
                                ; stop
end



pro specfit::contcorrect,flux=flux,comp=comp,cont=cont,wave=wave,res=res
  good = where(finite(flux))
  uflux = flux[good]
  ucomp = comp[good]
  uwave = wave[good]
 ; linemask = *(*self.active_object).linemask[good]
                                ; endif else begin
  fullx = findgen(n_elements(flux))
  x = fullx[good]
  corrected = fullx*0d0
  newflux = flux
  if strcmp(self.runtype,'run_diag') or strcmp(self.runtype,'run_pixel') and n_elements(where(finite(flux))) ne n_elements(flux) then begin
     if 1-keyword_set(res) then res = (*self.active_object).overallres
     for i=0,n_elements((*self.linelist).wave)-1 do begin
        ;; if 0 then begin
        ;;    dl = (*self.linelist)[i].wave/res
        ;;    if (*self.linelist)[i].num eq 2 then range = [(*self.linelist)[i].wave-self.doubline*dl,(*self.linelist)[i].wave+self.doubline*dl] else $
        ;;       range = [(*self.linelist)[i].wave-self.singline*dl,(*self.linelist)[i].wave+self.singline*dl]
        ;;    line = where(wave ge range[0] and wave le range[1])
        
        ;;    if line[0] ne -1 then begin
        ;;       while flux[line[0]] gt flux[line[1]] and line[0]-1 ge 0 do line = [line[0]-1,line]
        ;;       while flux[line[n_elements(line)-1]] gt flux[line[n_elements(line)-2]] and line[n_elements(line)-1]+1 le n_elements(flux)-1 do line = [line,line[n_elements(line)-1]+1]
        ;;       line = line[1:n_elements(line)-2]
        ;;    endif
        ;; endif
        
        lm = *(*self.active_object).linemask
        wv =  (*self.linelist)[i].wave
        center = where(abs(wave-wv) eq min(abs(wave-wv)))
        
        line = center[0]+[-2,-1,0,1,2]
        while lm[line[0]] ne 0 and line[0]-1 ge 0 do line = [line[0]-1,line]
        while lm[line[n_elements(line)-1]] ne 0 and line[n_elements(line)-1]+1 le n_elements(flux)-2 do line = [line,line[n_elements(line)-1]+1]
        

        
        if n_elements(line) ge 3 then begin
           line = line[1:n_elements(line)-2]
           goodline = line[where(lm[line] eq 1)]
           
                                ; stop
                                ;stop
           
           lflux = flux[goodline]
           datflux = comp[goodline]
           lwave = wave[goodline]
           lx = fullx[goodline]
                                ;    cross_correlate,datflux,lflux*median(datflux),shift,corr,width=100
           
           sorted = reverse(sort(lflux))
           if n_elements(sorted) lt 4 then contreg = sorted else $
              contreg = [(sorted[where(sorted lt n_elements(line)/2d0)])[0:1],$
                         (sorted[where(sorted ge n_elements(line)/2d0)])[0:1]]
           fit = robust_poly_fit(lwave[contreg],lflux[contreg]/datflux[contreg],1)
           newflux[line] = flux[line]/poly(wave[line],fit)
           corrected[line] = 1
           ;;  if self.lock_mask then stop
           
           if  0 then begin
              stop
              !p.multi = [0,1,1]
              self->startplot,3
              plot,wave,comp,color=(*self.colors).black,psym=10 
              oplot,wave,newflux,color=(*self.colors).red,psym=10
              sharpcorners,thick=!x.thick
              self->endplot,3
              
              self->startplot,2
              plot,wave,comp,color=(*self.colors).black,psym=10,xrange=((max(lwave)-min(lwave))*0.2*[-1,1])+mm(lwave),/xs,title=string(res,format='(i)')
              oplot,wave,newflux,color=(*self.colors).red,psym=10
              sharpcorners,thick=!x.thick
              self->endplot,2
              
                                ; stop
           endif
           
           ;;  stop
           
           ;;  cross_correlate,comp,flux[line],shift,corr,i1=min(line),i2=max(line),width=15
           
           ;;stop
           
        endif
     endfor
     
     flux = newflux
     if (where(corrected eq 0 and finite(flux) eq 1))[0] ne -1 then flux[where(corrected eq 0 and finite(flux) eq 1)] = sqrt(-1) ;; begin
     
     ;;    endif ;; stop
     
     ;; now shift correct...
     icut = 1
     while (where(finite(flux[icut:*])))[0] ne -1 and icut ne 0 do begin
        start = (where(finite(flux[icut:*])))[0]+icut
        finish=start
        while finite(flux[finish]) and finish lt n_elements(flux)-1 do finish++
        finish-=1
        
        if start lt finish then begin
           ;;   if finish eq n_elements(flux) then finish -=1
           
           line = fillarr(1,start,finish)
           lwave = wave[line]
           shift = findgen(3)-1
           corr = shift*0d0
           for t=0,n_elements(shift)-1 do corr[t] = total((comp[line+(shift[t])]-flux[line])^2)
                                ;  if shift[(where(corr eq min(corr)))[0]] ne 0 then stop
           
           fit = robust_poly_fit(shift,corr,2)
           xfit = fillarr(0.1,min(shift),max(shift)+0.1)
           yfit = poly(xfit,fit)
           
           m=min(yfit,p)
           s = median(lwave[1:*]-lwave[0:n_elements(lwave)-1])*xfit[p]
           lwave_fix = lwave-s
           lflux_fix = interpol(flux[line],lwave,lwave_fix)
           flux[line] = lflux_fix
           
                                ;  print,start,finish
           
           
           
           if 0 then begin
              self->startplot,2
              plot,shift,corr,/ys,yrange=mm([corr,yfit])+[-1,1],psym=8,color=(*self.colors).black
              oplot,xfit,yfit,color=(*self.colors).red
              self->endplot,2
              
              self->startplot,1
              plot,wave[start-5:finish+5],comp[start-5:finish+5],color=(*self.colors).black,psym=10,yrange=mm([comp[start-5:finish+5],newflux[start:finish]])+[-0.03,0.015]*median(comp[start-5:finish+5]),xticks=3
              oplot,wave[start-5:finish+5],newflux[start-5:finish+5],color=(*self.colors).red,psym=10
              oplot,wave[start-5:finish+5],flux[start-5:finish+5],color=(*self.colors).blue,psym=10
              vline,[wave[start],wave[finish]],linestyle=2
              sharpcorners,thick=!x.thick
              self->endplot,1
           endif
           
           icut=finish+1
           
        endif else icut = 0
                                ; stop
                                ;stop
        
        
     endwhile
     
     
     ptr_free,((*self.active_object).storecont)
     (*self.active_object).storecont = ptr_new(-1)
     
                                ; stop
     
     
  endif else begin
     if (*self.active_object).lockcont then begin
        fit = *(*self.active_object).bestcont
     endif else begin
        nreg = round(n_elements(uflux)*0.2)
        contregion = where(uflux gt (reverse(uflux[sort(uflux)]))[nreg])
        contregion = [0,1,contregion,n_elements(uwave)-2,n_elements(uwave)-1]
        contregion = (contregion[sort(contregion)])[uniq(contregion[sort(contregion)])]
        contregion = contregion[where(finite(uflux[contregion]) and finite(ucomp[contregion]))]
        

       ;; stop
        ;; plot,x,flux/comp,psym=8,color=colors.black
        ;; oplot,x[contregion],(flux/comp)[contregion],psym=8,color=colors.red
        
        fit = robust_poly_fit(uwave[contregion],(uflux/ucomp)[contregion],self.contorder)
        
        if  (*self.active_object).lockres eq 0 and 0 then begin
           self->startplot,0
           plot,wave,comp,yrange=mm(comp)+[-1,1]*0.1*(max(comp)-min(comp)),psym=10,color=(*self.colors).black
           oplot,wave,flux,color=(*self.colors).orange,psym=10
           oplot,uwave[contregion],ucomp[contregion],psym=8,color=(*self.colors).red 
           oplot,wave,flux/poly(wave,fit),color=(*self.colors).blue,psym=10 
           self->endplot,0
         ;  stop
        endif
                                ; stop
     endelse
     ptr_free,((*self.active_object).storecont)
     (*self.active_object).storecont = ptr_new(fit)
     cont = poly(wave,fit)
     fit=0L
     flux/=cont
  endelse
                                ;stop
                                ; endelse
end






pro specfit::contcorrect_old,flux=flux,comp=comp,cont=cont
  
  x = findgen(n_elements(flux))
  if (*self.active_object).lockcont then begin
     fit = *(*self.active_object).bestcont
  endif else begin
     nreg = round(n_elements(flux)*0.1)
     contregion = where(flux gt (reverse(flux[sort(flux)]))[nreg])
     contregion = [0,1,2,3,4,5,contregion,n_elements(contregion)-5,n_elements(contregion)-4,n_elements(contregion)-3,n_elements(contregion)-2,n_elements(contregion)-1]
     contregion = (contregion[sort(contregion)])[uniq(contregion[sort(contregion)])]
     contregion = contregion[where(finite(flux[contregion]) and finite(comp[contregion]))]
     
     ;; plot,x,flux/comp,psym=8,color=colors.black
     ;; oplot,x[contregion],(flux/comp)[contregion],psym=8,color=colors.red
     
     fit = robust_poly_fit(x[contregion],(flux/comp)[contregion],self.contorder)
  endelse
  ptr_free,((*self.active_object).storecont)
  (*self.active_object).storecont = ptr_new(fit)
  cont = poly(x,fit)
  fit=0L
                                ;stop
  flux/=cont
end



pro specfit::crosscorr,wave,dflux,mflux,waveshift,width=width
  dshift = median(abs(wave[1:n_elements(wave)-1]-wave[0:n_elements(wave)-2]))/8d0
  shiftvals = fillarr(dshift,-1*(width/2d0)*dshift,(width/2d0*dshift))
  shiftch = shiftvals*0d0+sqrt(-1)
  shiftcorr = shiftch
  if n_elements(*(*self.active_object).linemask) eq 1 then mask = intarr(n_elements(dflux))+1
  shiftedflux = dflux
  shiftedwave = wave
  
  
  
  for i=0,n_elements(shiftvals)-1 do begin
     newwave = wave+shiftvals[i]
     newflux = interpol(dflux,newwave,wave)
     
     self->startplot,3
     plot,wave,mflux,/nodata,xrange=[mm(wave[where(mask)])],yrange=mm(mflux[where(mask)]),/ys,/xs
     oplot,wave[where(mask)],mflux[where(mask)],psym=8
     oplot,newwave,newflux,color=(*self.colors).red
     oplot,shiftedwave,shiftedflux,color=(*self.colors).blue
     self->endplot,3
     
     minmax = [max([min(wave),min(newwave)]),min([max(wave),max(newwave)])] 
     good = where(mask eq 1 and wave gt minmax[0] and wave le minmax[1])
     shiftch[i]= total(((mflux-newflux)^2)[good])/n_elements(good)  
     shiftcorr[i] = total(mflux[good]*newflux[good])/n_elements(good)

     if shiftcorr[i] eq max(shiftcorr,/nan) then begin
        shiftedflux = newflux
        shiftedwave = newwave
     endif
     self->startplot,1
     plot,shiftvals,shiftch,psym=8,/xs,/ys
     vline,shiftvals[where(shiftch eq min(shiftch,/nan))],color=(*self.colors).blue
     self->endplot,1
     self->startplot,2
     plot,shiftvals,shiftcorr,psym=8,/xs,/ys
     vline,shiftvals[where(shiftcorr eq max(shiftcorr,/nan))],color=(*self.colors).blue
     self->endplot,2
     
     
     if i mod 50 eq 0 then stop
     
  endfor
  
  
  
  
  
  
  
end


pro specfit::prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,fixwave=fixwave,nofix=nofix,shift=shift,res=res
  ;; find shift
                                ; stop

  place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+self.init_shift)
  swave = ((*self.active_object).spec[place,0])-self.init_shift
  dflux = ((*self.active_object).spec[place,1])
  
  ;; dflux = (*self.dataflux)
  
  
                                ;if n_elements(wave) eq 0 then stop
                                ;if n_elements(flux) eq 0 then stop
                                ;if n_elements(swave) eq 0 then stop
  
  sflux = interpol(flux,wave,swave)
  

  
  if 0 and keyword_set(res) then begin
     ;;   self->linemask,sflux=sflux,swave=swave,finalres=res
     if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then good = where(*(*self.active_object).linemask)
     if good[0] eq -1 then good = findgen(n_elements(sflux))
  endif else good = findgen(n_elements(sflux))
  
  
  sflux *= median(dflux[good]/sflux[good])
  
  modsflux = sflux
  ;; modsflux[where(*(*self.active_object).linemask eq 0)] = 0d0
                                ;stop

                                ;self->crosscorr,swave,dflux,sflux,waveshift,width=500
  
  ;; stop
  region = where(swave ge 1.185 and swave le 1.20)

  
  if 1 then begin
     cross_correlate,dflux,modsflux,shift,corr,width=10,i1=min(region),i2=max(region),/silent
     if !err eq -1 then begin
        ;; stop
        cross_correlate,dflux,modsflux,shift,corr,width=10,i1=0,i2=n_elements(swave)-1,/silent
        !err = 0
     endif
  endif else begin
     stop
     lag = findgen(n_elements(dflux)/2)-0.25*n_elements(dflux)
     res = c_correlate(dflux,sflux,lag)
     shift = max(res,place)
     shift = lag[place]
  endelse
  
  shift*=median(abs(swave[1:n_elements(swave)-1]-swave[0:n_elements(swave)-2]))
  
  ;; stop

                                ;stop

  ;;if !err eq -1 then stop

  if self.stopper then begin
     print,shift
     print, median(dflux/sflux)
     stop
  endif
  ;; calc chi-sq
  place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+shift+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+shift+self.init_shift)
  if n_elements(place) gt n_elements((*self.active_object).bestfix) then place = place[0:n_elements((*self.active_object).bestfix)-1]
  swave = ((*self.active_object).spec[place,0])-shift-self.init_shift
  dflux = ((*self.active_object).spec[place,1])
  derr  = ((*self.active_object).spec[place,2])
  sflux = interpol(flux,wave,swave)
  
  if 1-keyword_set(nofix) then begin
     if n_elements(swave) gt n_elements((*self.active_object).bestfix) then stop
     swave2 = swave+(*self.active_object).bestfix
     dflux = interpol(dflux,swave2,swave) 
     derr = interpol(derr,swave2,swave)
  endif

                                ; if (*self.active_object).lockres then $
                                ;    (*self.active_object).mcgrid[0:n_elements(sflux)-1,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = sflux
end

function specfit::returnactive,loc,short=short
  if keyword_set(short) then return,string(((*self.models).grid.params.temps)[loc[0]],$
                                           ' ',((*self.models).grid.params.gravs)[loc[1]], $
                                           ' ',((*self.models).grid.params.abuns)[loc[2]],$
                                           ' ',((*self.models).grid.params.turbs)[loc[3]],format='(i5,a,d5.2,a,d5.2,a,i3)')
  
  return,string('T=',((*self.models).grid.params.temps)[loc[0]],'         logg=',((*self.models).grid.params.gravs)[loc[1]], $
                '           [Z]=', ((*self.models).grid.params.abuns)[loc[2]],'       xit=', $
                ((*self.models).grid.params.turbs)[loc[3]],format='(a,i5,a,d5.2,a,d5.2,a,i3)')
  
end

pro specfit::printactive
  if self.diag and (*self.location)[0] ne -1 then print,string('T=',((*self.models).grid.params.temps)[(*self.location)[0]],'         logg=', $
                                                               ((*self.models).grid.params.gravs)[(*self.location)[1]],'           [Z]=', $
                                                               ((*self.models).grid.params.abuns)[(*self.location)[2]],'       xit=', $
                                                               ((*self.models).grid.params.turbs)[(*self.location)[3]],format='(a,i5,a,d5.2,a,d5.2,a,i3)')
end



pro specfit::fixwave,swave=swave,sflux=sflux,derr=derr,dflux=dflux,fixed=fixed
  self.colors = ptr_new(specfit_colors())
  !p.background = (*self.colors).white
  !p.color = (*self.colors).black
  
  !p.multi=[0,2,1]
  ;; plot,swave,dflux,color=(*self.colors).black,xrange=[1.155,1.165],psym=10,thick=2
  ;; oplot,swave,sflux,color=(*self.colors).blue,psym=10,thick=2
  go = 0
  flex = 0.00008
  step = 0.000005
  shifts = fillarr(step,-1*flex,flex)
  findmin= fillarr(step/100,-1*flex,flex)
  shiftc = shifts*0d0 + sqrt(-1)
  shiftsc = shifts*0d0 + sqrt(-1)

  waveshift = dblarr(n_elements(swave))
;  waveshift[0:9] = sqrt(-1)
  waveshift[n_elements(swave)-10:n_elements(swave)-1] = sqrt(-1)
  
  for i=10,n_elements(swave)-11,1 do begin
     go = 1
     if (i mod 50) eq 0 then go = 0

     shiftc = shifts*0d0 + sqrt(-1)
     shiftsc = shifts*0d0 + sqrt(-1)
     
     dat = dflux[i-10:i+10]
     dwave = swave[i-10:i+10]
     
     for j=0,n_elements(shifts)-1,1 do begin
        ;; shift dwave, interpol onto swave, calcchi...
        dw = dwave+shifts[j]
        duse = swave+shifts[j]
        
        
        good = where(swave ge min(dw) and swave le max(dw) and finite(dflux))
        ;;  if n_elements(good) lt n_elements(dw) then good = [good,max(good)+1]
        ;; iwave = swave[good]
        datf = interpol(dflux[where(finite(dflux))],duse[where(finite(dflux))],swave,/spline)
        
        
        if good[0] ne -1 then begin
           shiftc[j] = total((sflux[good]-datf[good])^2)/n_elements(good)
           shiftsc[j] = total(sflux[good]*datf[good])/n_elements(good)
        endif else begin
           shiftc[j] = sqrt(-1)
           shiftsc[j] = sqrt(-1)
        endelse
        
        if 0 then begin
           wset,3
           !p.multi=[0,3,1]
           
           plot,swave,dflux,color=(*self.colors).black,xrange=[swave[i]-10*max(shifts),swave[i]+10*max(shifts)],$
                psym=10,thick=2
           oplot,swave,sflux,color=(*self.colors).blue,psym=10,thick=2
           oplot,swave[good],datf[good],psym=10,color=(*self.colors).red,thick=2
           oplot,swave[good],sflux[good],psym=10,color=(*self.colors).green,thick=2
           
           plot,shifts,shiftc,psym=8,color=(*self.colors).black,xrange=mm(shifts)+[-1*step,step],/xs,/ys
           plot,shifts,shiftsc,psym=8,color=(*self.colors).black,xrange=mm(shifts)+[-1*step,step],/xs,/ys
           
           wset,4
           device,copy = [0,0,!d.x_size,!d.y_size,0,0,3]
           !p.multi=[0,2,1]
           if go eq 0 then begin
              print,n_elements(good)
              stop
           endif
        endif
        
     endfor
     ;; fit = robust_poly_fit(shifts,shiftc,3)
     ;; yfit = poly(findmin,fit)
                                ; waveshift[i] = findmin[(where(yfit
                                ; eq min(yfit)))[0]]
     if (where(shiftc eq min(shiftc,/nan)))[0] ne -1 then $
        waveshift[i] = shifts[(where(shiftc eq min(shiftc,/nan)))[0]] else waveshift[i] = sqrt(-1)
                                ; waveshift[i] = shifts[(where(shiftsc eq max(shiftsc)))[0]]
     
     dw = dwave+waveshift[i]
     

     good = where(swave ge min(dw) and swave le max(dw))
     if n_elements(good) lt n_elements(dw) then good = [good,max(good)+1]
     iwave = swave[good]
     datf = interpol(dat,dw,iwave,/spline)
     
     xs = !x
     ys = !y
     ps = !p
     self->startplot,1
     !x.margin=[4,1]
     !y.margin=[3,0.5]
     !p.multi = [0,2,2]
     plot,swave,dflux,color=(*self.colors).black,xrange=[swave[i]-10*max(shifts),swave[i]+10*max(shifts)],$
          psym=10,thick=2,xticks=1,xtitle='um',yticks=1,ytickname=[' ',' '],/xs,/ys,yrange=mm(dflux[where(swave gt swave[i]-50*max(shifts) and swave le swave[i]+50*max(shifts))])
     oplot,swave,sflux,color=(*self.colors).blue,psym=10,thick=2
     oplot,dw,datf,psym=10,color=(*self.colors).green,thick=2
     
     if (where(finite(shiftc)))[0] ne -1 then plot,shifts,shiftc,psym=8,color=(*self.colors).black,xrange=mm(shifts)+[-1*step,step],/xs,xticks=2,xtitle='shift',symsize=.6,yticks=1,ytickname=[' ',' ']
                                ; oplot,findmin,yfit,color=(*self.colors).red
     if finite(waveshift[i]) then  vline,waveshift[i]
     
                                ;self->endplot,1
     
;     self->startplot,0
                                ;    !p.multi=[0,1,1]
     
     !p.multi=[1,1,2]
     plot,swave,waveshift,psym=8,color=(*self.colors).black,xtitle="Wavelength [um]",ytitle="Shift",symsize=.6
     
     cut = 1.1618
     cut2 = 1.21
     one = where(swave lt cut and findgen(n_elements(swave)) le i and finite(waveshift))
     two = where(swave ge cut and findgen(n_elements(swave)) le i and findgen(n_elements(swave)) lt n_elements(swave)-10 and swave lt cut2+0.005 and finite(waveshift))
     three = where(swave ge cut2-0.005 and findgen(n_elements(swave)) le i and findgen(n_elements(swave)) lt n_elements(swave)-10 and finite(waveshift))
     
     if n_elements(one) ge 5 then begin
        if min(swave) lt 1.15 then fit1 = robust_poly_fit(swave[one],waveshift[one],4) else $
           fit1 = robust_poly_fit(swave[one],waveshift[one],2)
        oplot,swave,poly(swave,fit1),color=(*self.colors).blue
     endif
     if n_elements(two) ge 50 then begin
                                ;    if max(swave) gt 1.21 then fit2 = robust_poly_fit(swave[two],waveshift[two],7) else $
        fit2 = robust_poly_fit(swave[two],waveshift[two],5)
        oplot,swave,poly(swave,fit2),color=(*self.colors).red
     endif
     if n_elements(three) ge 50 then begin
                                ;    if max(swave) gt 1.21 then fit2 = robust_poly_fit(swave[two],waveshift[two],7) else $
        fit3 = robust_poly_fit(swave[three],waveshift[three],5)
        oplot,swave,poly(swave,fit3),color=(*self.colors).green
     endif
     vline,cut
     vline,cut2
     
     self->endplot,1
     !x = xs
     !p = ps
     !y = ys
                                ;   !p.multi=[0,2,1]
;stop
     
     ;;  wait,.5
     
     
  endfor
  

  fixed = waveshift
  if (where(swave lt cut))[0] ne -1 then fixed[where(swave lt cut)] = (poly(swave,fit1))[where(swave lt cut)]
  if (where(swave ge cut and swave lt cut2))[0] ne -1 then $
     fixed[where(swave ge cut and swave lt cut2)] = (poly(swave,fit2))[where(swave ge cut and swave lt cut2)]
  if (where(swave ge cut2))[0] ne -1 then fixed[where(swave ge cut2)] = (poly(swave,fit3))[where(swave ge cut2)]
  
  !p.multi=[0,1,1]
  
  
  ;; stop
end

pro specfit::meas_init_shift,res,spec
  *self.location=[2,2,2,2]
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/res)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  region = where(wave ge self.fitrange[0] and wave le self.fitrange[1])
  data = where(spec[*,0] ge  self.fitrange[0] and spec[*,0] le self.fitrange[1])
  sflux = interpol(flux,wave,spec[data,0])
  
  lag = findgen(n_elements(data)/2)-0.25*n_elements(data)
  res = c_correlate(spec[data,1]/median(spec[data,1]),sflux,lag)
  shift = max(res,place)
  shift = lag[place]
  wave = spec[data,0]
  ishift = shift*median(abs(wave[1:n_elements(wave)-1]-wave[0:n_elements(wave)-2]))
  self.init_shift = -1d0*ishift

end

function specfit::fitres_old,modwave,modflux,modres,doplot=doplot,delres=delres
  if keyword_set(doplot) then plot = 1 else plot = 0
  plot=1
  ;;stop
  res = self.baseres*1.5
  if 1-keyword_set(delres) then delres = .95
  no_min = 1
  
  rchi = -1  
  ;; resolution fitting loop
  while no_min eq 1 and res gt 1000 do begin 
     ;; downgrade model to proper resolution:
     ;; 
     if res ne modres then $
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/res)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange $
     else begin
        wave = modwave
        flux = modflux
     endelse
     
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=res
                                ; stop
     ;;  stop ;;XXX
     if 1 then begin
        self->linemask,sflux=sflux,swave=swave,finalres=res,dflux=dflux
        if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
     endif
     
     self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=res

     csq = self->gstatistic_trusterr(swave,sflux,dflux,derr)
          
     if finite(csq) eq 0 then stop                    
     
     if rchi[0] eq -1 then begin
        rchi = csq
        rarr = res
     endif else  begin
        rchi = [rchi,csq]
        rarr = [rarr,res]
     endelse
     ;; print,res
     
     if 0 then begin
        if n_elements(rchi) gt 5 then begin
           if rchi[n_elements(rchi)-1] gt rchi[n_elements(rchi)-3] and $
              rchi[n_elements(rchi)-2] gt rchi[n_elements(rchi)-3] then $
                 no_min = 0
           if min(rarr) lt .25*self.baseres then no_min = -1
        endif
     endif
     if min(rarr) lt .25*self.baseres then no_min = 0
     
     res *= delres        
  endwhile
  
  ;; print,'no_min: ',no_min
  ;; if no_min eq -1 then stop
  
  if plot then begin
     self->startplot,2
     plot,rarr,rchi,psym=8,color=(*self.colors).black,/ys,title='Model #'+string((*self.active_object).modcount,format='(i4)'),xrange=[.25*self.baseres,1.5*self.baseres],/xs,xticks=2,xtickformat='(i5)'
     
  endif
  
  min = min(rchi,place,/nan)
  nums = 5
  if place le nums or place ge n_elements(rchi)-nums then finalres=rarr[place] else begin
     if plot then oplot,rarr[(place-nums):(place+nums)],rchi[(place-nums):(place+nums)],psym=8,color=(*self.colors).red
     fit = robust_poly_fit(rarr[place-nums:place+nums],rchi[place-nums:place+nums],2)
     xcalc = fillarr(10,round(min(rarr)),max(rarr))
     xfit = poly(xcalc,fit)
     if plot then oplot,xcalc,xfit,color=(*self.colors).red
     min = min(xfit,index)
     finalres = xcalc[index]
  endelse
  if plot then begin
     vline,finalres,color=(*self.colors).green
     sharpcorners,thick=!x.thick,color=(*self.colors).black
     self->endplot,2
  endif
  
                                ; stop
  

  if (self.inext)[0] ne -1 then (*self.active_object).bestres[self.inext]=finalres
;  stop
  ;; stop
  return,round(finalres/100d0)*100d0
end







function specfit::fitres,modwave,modflux,modres,doplot=doplot,delres=delres
  if keyword_set(doplot) then plot = 1 else plot = 0
  plot=1
  ;;stop
  ;stop
  res = round(self.baseres/100d0)*100d0
                                ;res = round(self.baseres*1.2/100d0)*100d0
                                ;if 1-keyword_set(delres) then delres = 0.95
  
  no_min = 1
  
  sign = 1
  rchi = -1  
  ;; resolution fitting loop
  ;; establish initial linemask and then leave it be::
  if (*self.active_object).modcount eq 0 and 0 then begin
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/res)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange 
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=res
     self->linemask,sflux=sflux,swave=swave,finalres=res,dflux=dflux
  endif
  
  while no_min eq 1 and min(res) gt 1000 do begin 
     ;; downgrade model to proper resolution:
     ;; 
     if res ne modres then $
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/res)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange $
     else begin
        wave = modwave
        flux = modflux
     endelse
     
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=res
     
     self->linemask,sflux=sflux,swave=swave,finalres=res,dflux=dflux
     if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
     
     self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=res
                                ;if 
     ;;csq = self->fullchi(swave,sflux,dflux,derr)
     csq = self->gstatistic_trusterr(swave,sflux,dflux,derr) ;'',chispec=chispec)
     

     if finite(csq) eq 0 then stop
                                ;  if plot then begin
                                ;     plot,swave,dflux,color=(*self.colors).black,title=res,psym=10
                                ;     oplot,swave,sflux,color=(*self.colors).red,psym=10
                                ;     stop
                                ;  endif
     ;; stop
     
     if rchi[0] eq -1 then begin
        rchi = csq
        rarr = res
     endif else  begin
        rchi = [rchi,csq]
        rarr = [rarr,res]
     endelse
     
     rchi = rchi[sort(rarr)]
     rarr = rarr[sort(rarr)]
     ;; print,res
     
     ;;; switch no min?
     if n_elements(rchi) ge 13 then $
        if total(rchi[0:5] gt min(rchi)) eq 6 and total(rchi[n_elements(rchi)-6:n_elements(rchi)-1] gt min(rchi)) eq 6 then no_min = 0
     
     
     if self.stopper or 0 then begin
        self->plot,swave=swave,sflux=sflux
        self->startplot,2
        plot,rarr,rchi,psym=8,color=(*self.colors).black,/ys,title='Model #'+string((*self.active_object).modcount,format='(i4)'),xrange=mm(rarr)+[-100,100],/xs,xticks=2,xtickformat='(i5)'
        vline,self.baseres,color=(*self.colors).black
        vline,res,color=(*self.colors).blue
;        vline,finalres,color=(*self.colors).green
        sharpcorners,thick=!x.thick,color=(*self.colors).black
        self->endplot,2
        stop
     endif
     
     
     if no_min then $
        if sign gt 0 then res = max(rarr)+100d else res = min(rarr)-100d
     sign *=-1
     
  endwhile
  

  
  ;; print,'no_min: ',no_min
  ;; if no_min eq -1 then stop
  
  min = min(rchi,place,/nan)
  finalres = rarr[place]
  if (self.inext)[0] ne -1 then (*self.active_object).bestres[self.inext]=round(finalres/100d0)*100d0

  if plot then begin
     self->startplot,2
     plot,rarr,rchi,psym=8,color=(*self.colors).black,/ys,title='Model #'+string((*self.active_object).modcount,format='(i4)'),xrange=mm(rarr)+[-100,100],/xs,xticks=2,xtickformat='(i5)'
     vline,self.baseres,color=(*self.colors).black
     ;vline,res,color=(*self.colors).blue
     vline,finalres,color=(*self.colors).green
     sharpcorners,thick=!x.thick,color=(*self.colors).black
     self->endplot,2
     
  endif
  return,round(finalres/100d0)*100d0
  
  stop
  nums = 5
  if place le nums or place ge n_elements(rchi)-nums then finalres=rarr[place] else begin
     if plot then oplot,rarr[(place-nums):(place+nums)],rchi[(place-nums):(place+nums)],psym=8,color=(*self.colors).red
     fit = robust_poly_fit(rarr[place-nums:place+nums],rchi[place-nums:place+nums],2)
     xcalc = fillarr(10,round(min(rarr)),max(rarr))
     xfit = poly(xcalc,fit)
     if plot then oplot,xcalc,xfit,color=(*self.colors).red
     min = min(xfit,index)
     finalres = xcalc[index]
  endelse
  if plot then begin
     vline,finalres,color=(*self.colors).green
     sharpcorners,thick=!x.thick,color=(*self.colors).black
     self->endplot,2
  endif
  
                                ; stop
  

  if (self.inext)[0] ne -1 then (*self.active_object).bestres[self.inext]=round(finalres/100d0)*100d0
;  stop
  ;; stop
  return,round(finalres/100d0)*100d0
end




function specfit::fitres_newerbutold,modwave,modflux,modres,doplot=doplot,delres=delres
  if keyword_set(doplot) then plot = 1 else plot = 0
  plot=1
  ;;stop
  res = round(self.baseres*1.2/100d0)*100d0
  if 1-keyword_set(delres) then delres = 0.95
  no_min = 1
  
  rchi = -1  
  ;; resolution fitting loop
  while no_min eq 1 and res gt 1000 do begin 
     ;; downgrade model to proper resolution:
     ;; 
     if res ne modres then $
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/res)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange $
     else begin
        wave = modwave
        flux = modflux
     endelse
     
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=res
                                ; stop
     ;;  stop ;;XXX
     if 1 then begin
        self->linemask,sflux=sflux,swave=swave,finalres=res,dflux=dflux
        if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
     endif
     
     self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=res
                                ;if 
     csq = self->fullchi(swave,sflux,dflux,derr)
     
     
     if finite(csq) eq 0 then stop
                                ;  if plot then begin
                                ;     plot,swave,dflux,color=(*self.colors).black,title=res,psym=10
                                ;     oplot,swave,sflux,color=(*self.colors).red,psym=10
                                ;     stop
                                ;  endif
     ;; stop
     
     if rchi[0] eq -1 then begin
        rchi = csq
        rarr = res
     endif else  begin
        rchi = [rchi,csq]
        rarr = [rarr,res]
     endelse
     ;; print,res
     
     if 0 then begin
        if n_elements(rchi) gt 5 then begin
           if rchi[n_elements(rchi)-1] gt rchi[n_elements(rchi)-3] and $
              rchi[n_elements(rchi)-2] gt rchi[n_elements(rchi)-3] then $
                 no_min = 0
           if min(rarr) lt .25*self.baseres then no_min = -1
        endif
     endif
     if min(rarr) lt .25*self.baseres then no_min = 0
     
                                ; nres = res*delres
                                ; res = min([nres,res-100])
     res -= 100
                                ;  res *= delres        
  endwhile
  
  ;; print,'no_min: ',no_min
  ;; if no_min eq -1 then stop
  
  min = min(rchi,place,/nan)
  finalres = rarr[place]
  if (self.inext)[0] ne -1 then (*self.active_object).bestres[self.inext]=round(finalres/100d0)*100d0

  if plot then begin
     self->startplot,2
     plot,rarr,rchi,psym=8,color=(*self.colors).black,/ys,title='Model #'+string((*self.active_object).modcount,format='(i4)'),xrange=[.25*self.baseres,1.5*self.baseres],/xs,xticks=2,xtickformat='(i5)'
     vline,finalres,color=(*self.colors).green
     sharpcorners,thick=!x.thick,color=(*self.colors).black
     self->endplot,2
     
  endif
  return,round(finalres/100d0)*100d0
  
  stop
  nums = 5
  if place le nums or place ge n_elements(rchi)-nums then finalres=rarr[place] else begin
     if plot then oplot,rarr[(place-nums):(place+nums)],rchi[(place-nums):(place+nums)],psym=8,color=(*self.colors).red
     fit = robust_poly_fit(rarr[place-nums:place+nums],rchi[place-nums:place+nums],2)
     xcalc = fillarr(10,round(min(rarr)),max(rarr))
     xfit = poly(xcalc,fit)
     if plot then oplot,xcalc,xfit,color=(*self.colors).red
     min = min(xfit,index)
     finalres = xcalc[index]
  endelse
  if plot then begin
     vline,finalres,color=(*self.colors).green
     sharpcorners,thick=!x.thick,color=(*self.colors).black
     self->endplot,2
  endif
  
                                ; stop
  

  if (self.inext)[0] ne -1 then (*self.active_object).bestres[self.inext]=round(finalres/100d0)*100d0
;  stop
  ;; stop
  return,round(finalres/100d0)*100d0
end


pro specfit::plot2d
  !p.multi=[0,3,1]
  ys = !y.margin
  !y.margin = [6,2]
  
  self->color_manage,'bw'
  
  self->startplot,3
  ;;  stop
  
  ;; at each g, xi = 4, plot T vs Z
  ;; g = -0.5

  ;; best model planes, plot T v Z, g v Z, xi v Z
  ;;
  ;;
                                ; print,(*self.active_object).bestloc
  dothis = (*self.active_object).bestloc
  if n_elements(dothis) eq 3 then dothis = [dothis,0]
  
  bestt = (*self.models).grid.params.temps[dothis[0]]
  bestg = (*self.models).grid.params.gravs[dothis[1]]
  bestz = (*self.models).grid.params.abuns[dothis[2]]
  bestx = (*self.models).grid.params.turbs[dothis[3]]
  
  
                                ; stop
  
  besty = bestz
  
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reform(((*self.active_object).chisq)[*,dothis[1],*,dothis[3]])
           
           bestxv = bestt

           ;;    title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;            'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           title = ''
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='Teff'
           xticks=5
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi = reform(((*self.active_object).chisq)[dothis[0],*,*,dothis[3]])

           bestxv = bestg
           xticks = 3
           
           title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
                   'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
                   'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           ;;taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;;tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))
           
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L
           
           xtitle='logg'
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[dothis[0],dothis[1],*,*]),1))

           bestxv = bestx
           xticks=4
           title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
                   'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           title = ''
           
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
                                ;taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))

           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L

           

           xtitle='turb [km/s]'
           ;;  help,ichi
           ;;  stop
        end
     endcase
                                ; title='logg='+string((*self.models).grid.params.gravs[pval],format='(d5.2)')

     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))


        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]


        contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle='[Z] ',title=title,background=255,/xs,/ys,xticks=xticks
        contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
        
        self->color_manage,'color'


        vline,taxis[tval],color=(*self.colors).red
        hline,zaxis[zval],color=(*self.colors).red
        oplot,[bestxv],[besty],psym=8,color=(*self.colors).blue,symsize=1.2
        oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).red,symsize=0.9
        
        
        self->color_manage,'bw'
        
        sharpcorners,thick=!x.thick
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        plot,[0],[0],title=title,background=255
        sharpcorners,thick=!x.thick
     endelse
  endfor
                                ;stop
  self->endplot,3
  !y.margin = ys
end

pro specfit::interpodd,modwave,modflux,modres,fail
  modwave = (*self.models).wave
  modres = (*self.models).res
  ;;
  ;; first try to find a +Z,-Z model
  ;;     must not be on Z edge of grid, and the Z models must not be bad...
  print,(*self.location)[2]
  print,n_elements((*self.models).grid.params.abuns)
  t1 = -1 
  t2 = -1
  if (*self.location)[2] ne 0 and (*self.location)[2] ne n_elements((*self.models).grid.params.abuns)-1 then begin
     ;; both +z, -z models exist
     ;;    are they GOOD models?
     t1 = (*self.location)
     t1[2] +=1
     t2 = (*self.location)
     t2[2] -=1     
     mf1 = (*self.models).flux[*,t1[0],t1[1],t1[2],t1[3]]
     mf2 = (*self.models).flux[*,t2[0],t2[1],t2[2],t2[3]]
     
     bad = 0
     if (where(finite(mf1)))[0] ne -1 then begin
        mfg = where(finite(mf1))
        mw1 = modwave[mfg]
        mf1 = mf1[mfg]
     endif else bad=1
     
     if (where(finite(mf2)))[0] ne -1 then begin
        mfg = where(finite(mf2))
        mw2 = modwave[mfg]
        mf2 = mf2[mfg]
     endif else bad=1
     
     ;; all real and actual spectra...
     if bad eq 0 and stddev(mf1) ne 0 then bad = 0 else bad = 1
     if bad eq 0 and stddev(mf2) ne 0 then bad = 0 else bad = 1
     
     ;; assemble the new spectrum:
     if bad eq 0 then begin
        modwave = mw1
        mf2 = interpol(mf2,mw2,mw1,/spline)
        modflux = modwave*0d0
        for i=0,n_elements(modflux)-1,1 do modflux[i] = average([mf1[i],mf2[i]])
        fail = 0
        print,'fixed odd!'
                                ; stop
        return
     endif
  endif 
  
  ;; ok, go -1
  print,'need new code to fix odd!'
                                ;stop
  
end


pro specfit::getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  fail = 0
  if (*self.location)[0] eq -1 then begin
     fail=1
     modflux = [0,0,0]
  endif else begin
     modwave = (*self.models).wave
     if self.lock_param[3] then modflux = (*self.models).flux[*,(*self.location)[0],(*self.location)[1],(*self.location)[2]] else $
        modflux = (*self.models).flux[*,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]]
     modres = (*self.models).res
     if (where(finite(modflux)))[0] ne -1 then begin
        modwave = modwave[where(finite(modflux))]
        modflux = modflux[where(finite(modflux))]
     endif else fail = 1
  endelse
  
  if stddev(modflux) eq 0 or fail then begin
     self->printactive
     if self.inext ne -1 then (*self.active_object).chisq[self.inext] = 1d10
     if n_elements(modflux) gt 3 then begin
        print,'odd flux model found.'
        self->interpodd,modwave,modflux,modres,fail
     endif
     ;;  fail = 0
     ;;  stop
  endif
end

function specfit::makechiarr,waverange
  return,(*self.active_object).chisq
end

;function specfit::makechiarr,waverange
;  return,(*self.active_object).chisq
;end


function specfit::makechiarr_old,waverange

  chicut = 0.00

  lam1 = 1.2083
  lam2 = 1.1828
  wide = 0.0006
  
  chiarr = (*self.active_object).chisq*0d0
  
  for k=0,n_elements(chiarr)-1,1 do begin
     ai = array_indices(chiarr,k)
     if n_elements(ai) eq 3 then begin
        wave = (*self.active_object).fullchisqw[*,ai[0],ai[1],ai[2]]
        good = where(wave ge waverange[0] and wave le waverange[1] and finite((*self.active_object).fullchisq[*,ai[0],ai[1],ai[2]]))  
        
        if good[0] eq -1 then chiarr[k] = abs(sqrt(-1)) else begin
           wave = (*self.active_object).fullchisqw[good,ai[0],ai[1],ai[2]]
           chi = (*self.active_object).fullchisq[good,ai[0],ai[1],ai[2]]
           chibad1 = where(wave gt lam1-wide and wave lt lam1+wide) 
           chibad2 = where(wave gt lam2-wide and wave lt lam2+wide)
           chibad = [chibad1,chibad2]
           if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
           
           if chibad[0] ne -1 then begin
              chi[chibad] = sqrt(-1)
           endif
           chi = chi[where(finite(chi))]
           if chicut ne 0 then begin
              cval = (chi[reverse(sort(chi))])[chicut*n_elements(chi)]
              chi = chi[where(chi lt cval)]
           endif
        endelse
        chiarr[k] = total(chi)
        if self.grav_avoid eq (*self.models).grid.params.gravs[ai[1]] then chiarr[k] = abs(sqrt(-1))
     endif else begin
        wave = (*self.active_object).fullchisqw[*,ai[0],ai[1],ai[2],ai[3]]
        good = where(wave ge waverange[0] and wave le waverange[1] and finite((*self.active_object).fullchisq[*,ai[0],ai[1],ai[2],ai[3]]))  
                                ;   good2 = where(wave ge waverange[0] and wave le waverange[1])  
        
        if good[0] eq -1 then chiarr[k] = abs(sqrt(-1)) else begin
           wave = (*self.active_object).fullchisqw[good,ai[0],ai[1],ai[2],ai[3]]
           chi = (*self.active_object).fullchisq[good,ai[0],ai[1],ai[2],ai[3]]
           chibad1 = where(wave gt lam1-wide and wave lt lam1+wide) 
           chibad2 = where(wave gt lam2-wide and wave lt lam2+wide)
           chibad = [chibad1,chibad2]
           if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
           if 1 then begin
              if chibad[0] ne -1 then begin
                 chi[chibad] = sqrt(-1)
              endif
              chi = chi[where(finite(chi))]
              cval = (chi[reverse(sort(chi))])[chicut*n_elements(chi)]
              chi = chi[where(chi lt cval)]
              
           endif
           chiarr[k] = total(chi)
        endelse
        if self.lock_param[3] ne 1 then if self.turb_avoid eq (*self.models).grid.params.turbs[ai[3]] then chiarr[k] = abs(sqrt(-1))
        if self.grav_avoid eq (*self.models).grid.params.gravs[ai[1]] then chiarr[k] = abs(sqrt(-1))
     endelse
  endfor
  return,chiarr
end

pro specfit::byparam_analysis,waverange=waverange
  add = 0
  if self.makeplot eq 1 then begin
     add = 1
     self.makeplot = 0
  endif
  
  if self.lock_param[3] then max = 2 else max = 3
  for m=0,max do begin
     case m of
        0: begin
           ;; gravity
           usepar = (*self.models).grid.params.gravs
           parstr = string(usepar,format='(d5.2)')
           parloc = 1
           parhead = '_logg'
           finparams = dblarr(n_elements(usepar),4)
           finchisq = dblarr(n_elements(usepar))+sqrt(-1)
        end
        1: begin
           ;; temperature
           usepar = (*self.models).grid.params.temps
           parstr = string(usepar,format='(i4.4)')
           parloc = 0
           parhead = '_teff'
           finparams = dblarr(n_elements(usepar),4)
           finchisq = dblarr(n_elements(usepar))+sqrt(-1)
        end
        2: begin
           ;; Z
           usepar =(*self.models).grid.params.abuns
           parstr = string(usepar,format='(d5.2)')
           parloc = 2
           parhead = '_abun'
           finparams = dblarr(n_elements(usepar),4)
           finchisq = dblarr(n_elements(usepar))+sqrt(-1)
        end
        3: begin  
           ;; xi
           usepar = (*self.models).grid.params.turbs
           parstr = string(usepar,format='(i2.2)')
           parloc = 3
           parhead = '_mturb'
           finparams = dblarr(n_elements(usepar),4)
           finchisq = dblarr(n_elements(usepar))+sqrt(-1)
        end
     endcase
     
     for t=0,n_elements(usepar)-1 do begin
        use = usepar[t]
        if keyword_set(customchi) then begin
           chiarr = customchi
           for k=0,n_elements(chiarr)-1,1 do $
              if (where(use eq usepar[(array_indices(chiarr,k))[parloc]]))[0] eq -1 then chiarr[k] = abs(sqrt(-1))
        endif else begin
           waverange = [1.16,1.206]
           chiarr = self->makechiarr(waverange)
           for k=0,n_elements(chiarr)-1,1 do $
              if (where(use eq usepar[(array_indices(chiarr,k))[parloc]]))[0] eq -1 then chiarr[k] = abs(sqrt(-1))
           if (where(finite(chiarr)))[0] ne -1 then $
              if max(chiarr(where(finite(chiarr)))) ne 0 then begin
              is = 'xxx'
              seedstring = string((*self.active_object).obj+strtrim(parhead,2)+strtrim(parstr[t],2),waverange[0],waverange[1],format='(a40,d10.3,d10.3)')
              self->custom_analysis_mask,chiarr,string1=string1,string2=string2,string3=seedstring,is=is,params=params,plot=0
              seedstring+= string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
              self->writetoascii,seedstring+'  '+self.runtype+'  '+string(min(chiarr(where(finite(chiarr)))),format='(d30.10)'),6
              finparams[t,*] = params
              finchisq[t] = min(chiarr(where(finite(chiarr))))
           endif   
        endelse
     endfor
     
     weight = (1/finchisq)/total(1/finchisq[where(finite(finchisq))])     
     if (where(finite(weight) eq 0))[0] ne -1 then weight[where(finite(weight) eq 0)] = 0     
     fin  = dblarr(4)
     sfin = dblarr(4)
     for i=0,3 do fin[i]=total(finparams[*,i]*weight)
     for i=0,3 do begin
        good = where(weight gt 0,c)
        if c lt 2 then sfin[i] = 0 else $
           sfin[i]=stddev((finparams[good,i]*weight[good]*n_elements(good)))
     endfor
     
     str = string((*self.active_object).obj+strtrim(parhead,2)+'_all',waverange[0],waverange[1],$
                  '     ',fin[0],'  ',sfin[0],$
                  '     ',fin[1],'  ',sfin[1],$
                  '     ',fin[2],'  ',sfin[2],$
                  '     ',fin[3],'  ',sfin[3],format='(a40,d10.3,d10.3,a,i5,a,i4,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d4.2)')+$
           string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
     self->writetoascii,str+'  '+self.runtype+'             0.0',6
  endfor  
  self.makeplot+=add
  
end


pro specfit::best10_analysis,waverange=waverange
  add = 0
  if self.makeplot eq 1 then begin
     add = 1
     self.makeplot = 0
  endif
  
  finparams = dblarr(10,4)
  finchisq = dblarr(10)+sqrt(-1)
  parhead = ''
  parstr = ''
  chiarr = self->makechiarr(waverange)
  
  best10 = (sort(chiarr))[0:9]

  for i=0,9 do begin
     dothis = array_indices((*self.active_object).chisq,best10[i])
     seedstring = string((*self.active_object).obj+strtrim(parhead,2)+strtrim(parstr,2),waverange[0],waverange[1],format='(a30,d10.3,d10.3)')
     self->custom_analysis_mask,chiarr,string1=string1,string2=string2,string3=seedstring,is=is,params=params,plot=0,dothis=dothis
     finparams[i,*] = params
     finchisq[i] = chiarr[best10[i]]
  endfor

  weight = (1/finchisq)/total(1/finchisq[where(finite(finchisq))])     
  if (where(finite(weight) eq 0))[0] ne -1 then weight[where(finite(weight) eq 0)] = 0     
  fin  = dblarr(4)
  sfin = dblarr(4)
  for i=0,3 do fin[i]=total(finparams[*,i]*weight)
  for i=0,3 do begin
     good = where(weight gt 0,c)
     if c lt 2 then sfin[i] = 0 else $
        sfin[i]=stddev((finparams[good,i]*weight[good]*n_elements(good)))
  endfor
  
  str = string((*self.active_object).obj+strtrim(parhead,2)+'_avg',waverange[0],waverange[1],$
               '     ',fin[0],'  ',sfin[0],$
               '     ',fin[1],'  ',sfin[1],$
               '     ',fin[2],'  ',sfin[2],$
               '     ',fin[3],'  ',sfin[3],format='(a30,d10.3,d10.3,a,i5,a,i4,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d4.2)')+$
        string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
  self->writetoascii,str+'  '+self.runtype+'             0.0',7
  
  fin = median(finparams,dim=1)
  str = string((*self.active_object).obj+strtrim(parhead,2)+'_med',waverange[0],waverange[1],$
               '     ',fin[0],'  ',sfin[0],$
               '     ',fin[1],'  ',sfin[1],$
               '     ',fin[2],'  ',sfin[2],$
               '     ',fin[3],'  ',sfin[3],format='(a30,d10.3,d10.3,a,i5,a,i4,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d4.2)')+$
        string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
  self->writetoascii,str+'  '+self.runtype+'             0.0',7
  
  
  
  self->writetoascii,string(finchisq,format='(d15.5)'),7
  self->writetoascii,string(finparams[*,0],format='(i15)'),7
  self->writetoascii,string(finparams[*,1],format='(d15.3)'),7
  self->writetoascii,string(finparams[*,2],format='(d15.3)'),7
  self->writetoascii,string(finparams[*,3],format='(d15.3)'),7
  self->writetoascii,'*******',7
  
  tenperc = min(finchisq)*1.1
  best10 = where(chiarr le tenperc)
  
  finparams = dblarr(n_elements(best10),4)
  finchisq = dblarr(n_elements(best10))+sqrt(-1)
  parhead = ''
  parstr = ''
  ;;;chiarr = self->makechiarr(waverange)
  
  for i=0,n_elements(best10)-1 do begin
     dothis = array_indices((*self.active_object).chisq,best10[i])
     seedstring = string((*self.active_object).obj+strtrim(parhead,2)+strtrim(parstr,2),waverange[0],waverange[1],format='(a30,d10.3,d10.3)')
     self->custom_analysis_mask,chiarr,string1=string1,string2=string2,string3=seedstring,is=is,params=params,plot=0,dothis=dothis
     finparams[i,*] = params
     finchisq[i] = chiarr[best10[i]]
  endfor
  
  weight = (1/finchisq)/total(1/finchisq[where(finite(finchisq))])     
  if (where(finite(weight) eq 0))[0] ne -1 then weight[where(finite(weight) eq 0)] = 0     
  fin  = dblarr(4)
  sfin = dblarr(4)
  for i=0,3 do fin[i]=total(finparams[*,i]*weight)
  for i=0,3 do begin
     good = where(weight gt 0,c)
     if c lt 2 then sfin[i] = 0 else $
        sfin[i]=stddev((finparams[good,i]*weight[good]*n_elements(good)))
  endfor
  str = string((*self.active_object).obj+strtrim(parhead,2)+'_10%',waverange[0],waverange[1],$
               '     ',fin[0],'  ',sfin[0],$
               '     ',fin[1],'  ',sfin[1],$
               '     ',fin[2],'  ',sfin[2],$
               '     ',fin[3],'  ',sfin[3],format='(a30,d10.3,d10.3,a,i5,a,i4,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d4.2)')+$
        string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
  self->writetoascii,str+'  '+self.runtype+'             0.0',8
  
  self->writetoascii,string(finchisq,format='(d15.5)'),8
  self->writetoascii,string(finparams[*,0],format='(i15)'),8
  self->writetoascii,string(finparams[*,1],format='(d15.3)'),8
  self->writetoascii,string(finparams[*,2],format='(d15.3)'),8
  self->writetoascii,string(finparams[*,3],format='(d15.3)'),8
  self->writetoascii,'*******',8
  
;; stop
  self.makeplot+=add
  
  
end

pro specfit::mc_analysis,waverange=waverange,data=data
  self->message,message='Monte Carlo error estimation v2...',which='B'
  

  if keyword_set(data) then test = stddev((*self.active_object).mc_params_data[*,1]) else test = stddev((*self.active_object).mc_params[*,1])
  
  if test eq 0 then begin
     add = 0
     if self.makeplot eq 1 then begin
        add = 1
        self.makeplot = 0
     endif
     
     objspec = (*self.active_object)
     dir = self.outdir+'/'+'output_'+objspec.obj+'/'
     ps = !p
     xs = !x
     ys = !y

     *self.location =  self->find_bestloc(self.locsize,/doweight) ;;(*self.active_object).bestloc
   
     print,self->find_bestloc(self.locsize,/doweight);,weight=0.25)
;     print,self->find_bestloc(3)
     
;     stop
     
     self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=bwave,sflux=bflux,derr=derr,dflux=dflux        
     self->contcorrect,flux=bflux,comp=dflux,cont=cont,wave=bwave
     

     chiarr = self->makechiarr(waverange)
     if (where(finite(chiarr)))[0] ne -1 then $
        if max(chiarr(where(finite(chiarr)))) ne 0 then self->custom_analysis_mask,chiarr,params=params,sigs=sigs

     bestchi = min(chiarr,/nan)
     
     self->lininterp,params,iwave,iflux ;,/makestop
     good = where(iwave gt min(modwave) and iwave lt max(modwave) and finite(iflux))

     ;; pwave and pflux are PERFECT wave and flux (interpolated model that should
     ;; match the data)
     ;;
     ;; bwave and bflux are BEST wave and flux--model in grid that
     ;; closest matches data
     ;;
     if good[0] ne -1 then begin
        iflux = iflux[good]
        iwave = iwave[good]
        spec_conv5aa,iwave,iflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/(*self.models).res)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=pwave,sflux=pflux,derr=derr,dflux=dflux  
        self->contcorrect,flux=pflux,comp=dflux,cont=cont,wave=pwave

     endif else stop
   ;  stop

    ; self->contcorrect,flux=bflux,comp=dflux,cont=cont,wave=bwave

     self->linemask,sflux=bflux,swave=bwave,finalres=(*self.active_object).overallres,dflux=dflux
     if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then bflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
     
     
  ;   stop
     if 1 then begin
        scale = median((dflux/bflux)[where(finite(bflux))])
        bwt = bwave
        bft = bflux*scale
        
        snr = 10000d0
        csq = 1d10
        while csq-bestchi gt 0.1*bestchi and snr ne 0 do begin
           snr -= 1d0
           noise = randomn(seed,n_elements(pflux))*(median(pflux)/snr)
           pdflux = (pflux+noise)*scale
           perr = (dblarr(n_elements(pflux))+(median(pflux)/snr))*scale
                                ;  pdflux = dflux+noise
                                ;  perr = pdflux/snr
           
           csq = self->gstatistic_trusterr(bwt,bft,pdflux,perr,/mc)
           print,median((dflux/derr)[where(finite(bflux))]),snr,csq,bestchi
                             ;    stop
        endwhile
        if snr eq 0 then begin
           snr = median((dflux/derr)[where(finite(bflux))])
        endif
     endif else begin
        snr = median((dflux/derr)[where(finite(bflux))])
     endelse
     
                                ; if snr gt 1.2*median((dflux/derr)[where(finite(bflux))]) or snr le 0.8*median((dflux/derr)[where(finite(bflux))]) then begin
                                ;    (*self.active_object).refit = 1
                                ;    (*self.active_object).modsnr = snr
                                ;    (*self.active_object).origsnr = median((dflux/derr)[where(finite(bflux))])
                                ; endif
     
     trials = 100
     mc_params = dblarr(4,trials)
     
     min = min((*self.active_object).chisq,x,/nan)
     ;;dothis = array_indices((*self.active_object).chisq,x)
     dothis = self->find_bestloc(self.locsize)
     if n_elements(dothis) eq 3 then doxi = 0 else doxi = 1
     
     taxis = fillarr(self.dt/2d0,mm((*self.models).grid.params.temps))
     tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
     zaxis = fillarr(self.dz/2d0,mm((*self.models).grid.params.abuns))
     zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
     gaxis = fillarr(self.dg/2d0,mm((*self.models).grid.params.gravs))
     gax = 1d0*findgen(n_elements(gaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(gaxis))
     if doxi then begin
        xiaxis = fillarr(self.dx/2d0,mm((*self.models).grid.params.turbs))
        xiax = 1d0*findgen(n_elements(xiaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(xiaxis))
     endif
     
     res = dblarr(3,4,trials) + sqrt(-1)
     allres = res
     if doxi eq 0 then res[*,3,*] = (*self.models).grid.params.turbs
     bestloc =  self->find_bestloc(self.locsize) ;; (*self.active_object).bestloc
     
     swave = bwave
                                ; stop
                                ; pflux = dflux
     ;; if 1 then stop

     if keyword_set(data) then pflux = dflux

     !p.multi=[0,3,2]
     for z=0,trials-1 do begin
        noise = randomn(seed,n_elements(bflux))*((median(bflux)/snr))
        ;; noise -= average(noise)
        newflux = pflux+noise
        newerr = pflux*0d0 + median(newflux)/snr

       ; if z eq 0 then stop

        csq = dblarr(n_elements((*self.models).grid.params.temps),$
                     n_elements((*self.models).grid.params.gravs),$
                     n_elements((*self.models).grid.params.abuns),$
                     n_elements((*self.models).grid.params.turbs))
        
        for i=0,n_elements((*self.models).grid.params.temps)-1 do $
           for j=0,n_elements((*self.models).grid.params.gravs)-1 do $
              for k=0,n_elements((*self.models).grid.params.abuns)-1 do $
                 for l=0,n_elements((*self.models).grid.params.turbs)-1 do begin
           sflux = (*self.active_object).mcgrid[*,i,j,k,l]
           if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
           
           if 0 then begin
              if 1 then begin
                 region = where(swave ge 1.185 and swave le 1.20)  
                 cross_correlate,dflux,sflux,shift,corr,width=10,i1=min(region),i2=max(region),/silent
                 if !err eq -1 then begin
                    ;; stop
                    cross_correlate,dflux,sflux,shift,corr,width=10,i1=0,i2=n_elements(swave)-1,/silent
                    !err = 0
                 endif
              endif else begin
                 lag = findgen(n_elements(dflux)/2)-0.25*n_elements(dflux)
                 resu = c_correlate(dflux,sflux,lag)
                 shift = max(resu,place)
                 shift = lag[place]
              endelse
              shift*=median(abs(swave[1:n_elements(swave)-1]-swave[0:n_elements(swave)-2]))
              
                                ; print,shift
                                ; stop
              newwave = swave+shift
              if n_elements(sflux) lt n_elements(swave) then sflux = [sflux,dblarr(n_elements(swave)-n_elements(sflux))+sqrt(-1)]
              if n_elements(sflux) gt n_elements(swave) then sflux = sflux[0:n_elements(swave)-1] 
              sflux = interpol(sflux,newwave,swave) 
           endif
                                ; stop
       ;    print,mm(sflux),median(sflux)
       ;    print,mm(newflux),median(newflux)
           
        ;   self->contcorrect,flux=sflux,comp=newflux,cont=cont,wave=swave
        ;     print,mm(sflux),median(sflux)
        ;   print,mm(newflux),median(newflux)
                
;stop
           if (where(finite(sflux)))[0] eq -1 then csq[i,j,k,l] = sqrt(-1) else $
              csq[i,j,k,l] =  self->gstatistic_trusterr(swave,sflux,newflux,newerr,/mc)
           
           
           
                                ;
           
                                ; stop
           if z eq 0 then begin
              !p.multi=[0,1,1]
              !y.margin=[4,2]
              self->startplot,0
              self->color_manage,'color'
              xrange=[self.fitrange[0],self.fitrange[1]]
              yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
              yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
              plot,[0],[0],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns'
              
              if self.nomg then begin
                 lam1 = 1.2083
                 lam2 = 1.1828
                 wide = 0.0006
                 
                 y = [!y.crange[0],!y.crange[1],!y.crange[1],!y.crange[0]]
                 x = [lam1+wide,lam1+wide,lam1-wide,lam1-wide]
                 polyfill,x,y,color=(*self.colors).litegray
                 x = [lam2+wide,lam2+wide,lam2-wide,lam2-wide]
                 polyfill,x,y,color=(*self.colors).litegray
                 
                                ;  vline,[lam1,lam2]+wide,color=(*self.colors).violet
                                ;  vline,[lam1,lam2]-wide,color=(*self.colors).violet
              endif
              
              plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,title=csq[i,j,k,l],background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns',/noerase
              oplot,swave,newflux,color=(*self.colors).blue,psym=10
              oplot,swave,sflux,color=(*self.colors).red,psym=10
                                ; oplot,swave,newsflux,color=(*self.colors).green,psym=10
              sharpcorners,thick=!x.thick
              self->endplot,0

             ; stop
              
                                ;    stop
           endif
                                ;   if randomn(seed) lt 0.1 then begin
                                ;      self->startplot,0
                                ;      !p.multi=[0,1,1]
                                ;      plot,swave,newflux,psym=10
                                ;      oplot,swave,sflux,psym=10,color=(*self.colors).blue
                                ;      sharpcorners,thick=!x.thick
                                ;      self->endplot,0
                                ;   endif
        endfor
        
        ;; now slice in each plane of interest...
        xarr = ['temp','temp','temp','abun','abun','grav']
        yarr = ['abun','grav','turb','grav','turb','turb']
        
        chiarr = csq
        bestloc = self->find_bestloc(self.locsize,chiarr=chiarrr)
        if n_elements(bestloc) eq 3 then bestloc = [bestloc,0]
                                ;print,bestloc
                                ;print,self->find_bestloc(3,chiarr=csq)
                                ;stop
        
        for j=0,n_elements(xarr)-1 do begin
           calc = 1
           if doxi eq 0 then if xarr[j] eq 'turb' or yarr[j] eq 'turb' then calc = 0
                                ; if calc eq 0 then stop
           if calc then begin 
              case j of
                 0: resloc = [[0,0],[0,2]]
                 1: resloc = [[1,0],[0,1]]
                 2: resloc = [[2,0],[0,3]] 
                 3: resloc = [[1,2],[1,1]]
                 4: resloc = [[2,2],[1,3]]
                 5: resloc = [[2,1],[2,3]]
              endcase

              case xarr[j] of 
                 'temp': begin
                    xaxis = taxis
                    xax = tax
                    if yarr[j] eq 'abun' then csq_slice = reform(csq[*,bestloc[1],         *,bestloc[3]])
                    if yarr[j] eq 'grav' then csq_slice = reform(csq[*,         *,bestloc[2],bestloc[3]])
                    if yarr[j] eq 'turb' then csq_slice = reform(csq[*,bestloc[1],bestloc[2],         *])
                 end
                 'grav': begin
                    xaxis = gaxis
                    xax = gax
                    if yarr[j] eq 'abun' then csq_slice =                reform(csq[bestloc[0],         *,         *,bestloc[3]])
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,         *,bestloc[2],bestloc[3]]),1))
                    if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],         *,bestloc[2],         *])
                 end
                 'abun': begin
                    xaxis = zaxis
                    xax = zax
                    if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,         *,bestloc[3]]),1))
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],         *,bestloc[3]]),1))
                    if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],bestloc[1],         *,         *])
                 end
                 'turb': begin
                    xaxis = xiaxis
                    xax = xiax
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],bestloc[2],*]),1))
                    if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,bestloc[2],*]),1))
                    if yarr[j] eq 'abun' then csq_slice = reverse(rotate(reform(csq[bestloc[0],bestloc[1],         *,*]),1))
                 end
                 
              endcase 
              case yarr[j] of 
                 'temp': begin
                    yaxis = taxis
                    yax = tax
                 end
                 'grav': begin
                    yaxis = gaxis
                    yax = gax
                 end
                 'abun': begin
                    yaxis = zaxis
                    yax = zax
                 end
                 'turb': begin
                    yaxis = xiaxis
                    yax = xiax
                 end
              endcase
              
              if n_elements(xax) eq 1 then begin
                 result = interpolate(csq_slice,yax,/grid,/missing,cubic=-.5)
                 m = min(result,place)
                 
                 res[resloc[0,0],resloc[1,0],z]=xax[0]
                 res[resloc[0,1],resloc[1,1],z]=yaxis[place]
              endif else if n_elements(yax) eq 1 then begin
                 result = interpolate(csq_slice,xax,/grid,/missing,cubic=-.5)
                 m = min(result,place)
                 
                 res[resloc[0,0],resloc[1,0],z]=xax[place]
                 res[resloc[0,1],resloc[1,1],z]=yax[0]
              endif else begin   
                 if (where(finite(csq_slice) eq 0))[0] ne -1 then csq_slice[where(finite(csq_slice) eq 0)] = max(csq_slice)
                 result = interpolate(csq_slice,xax,yax,/grid,/missing,cubic=-.5)
                 xv = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
                 if (xv)[0] ne -1 then begin
                    
                    yv = (where(result[xv,*] eq min(result,/nan)))[0]
                    xval = xaxis[xv]
                    yval = yaxis[yv]
                    
                    if 1-max(mm(xaxis) eq xval) and   1-max(mm(yaxis) eq yval) then begin
                       res[resloc[0,0],resloc[1,0],z] = xval
                       res[resloc[0,1],resloc[1,1],z] = yval
                    endif else begin
                       res[resloc[0,0],resloc[1,0],z] = sqrt(-1)
                       res[resloc[0,1],resloc[1,1],z] = sqrt(-1)
                    endelse
                    allres[resloc[0,0],resloc[1,0],z] = xval
                    allres[resloc[0,1],resloc[1,1],z] = yval
                 endif else begin 
                    res[resloc[0,0],resloc[1,0],z] = sqrt(-1)
                    res[resloc[0,1],resloc[1,1],z] = sqrt(-1)
                 endelse
              endelse
              
              if z mod 10 eq 0 and (xarr[j] eq 'abun' or yarr[j] eq 'abun') then begin
                 print,'      ',xarr[j],'       ',yarr[j]
                 print,xval,yval
                 !p.multi=[0,1,1]
                 self->startplot,1
                 self->color_manage,'bw'       
                 contour,result,xaxis,yaxis,nlevel=255,/fill,xrange=mm(xaxis),yrange=mm(yaxis),xtitle=xarr[j],ytitle=yarr[j],/xs,/ys,xticks=xticks
                 contour,result,xaxis,yaxis,nlevel=5,/overplot,color=230
                 
                 ;;display,result,xtitle=xarr[j],ytitle=yarr[j],/aspect
                 self->color_manage,'color'
                 oplot,[xval],[yval],color=(*self.colors).blue,psym=8
                 vline,xval,color=(*self.colors).blue
                 hline,yval,color=(*self.colors).blue
                 sharpcorners,thick=!x.thick
                 
                 self->endplot,1
               ;                 stop
              endif
              
              !p.multi=[0,3,1]
              !x.margin=[10,1]
              !y.margin=[5,3]
              self->color_manage,'color'
              
              if (z mod 5) eq 0 and z gt 0 then begin
                 if 1 then begin
                    yrange = mm((res[*,2,0:z])[where(finite(res[*,2,0:z]))])+[-.01,.01]
                    self->startplot,3
                    if (where(finite(res[*,0,0:z])))[0] ne -1 then xrange = mm((res[*,0,0:z])[where(finite(res[*,0,0:z]))])+[-20,20] else xrange = [3400,4400]
                    plot,res[*,0,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='T!deff!n',symsize=0.7
                    hline,average(res[*,2,0:z]),color=(*self.colors).blue
                    vline,average(res[*,0,0:z]),color=(*self.colors).blue
                    ;; xrange=[3400,4400],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                    if (where(finite(res[*,1,0:z])))[0] ne -1 then xrange = mm((res[*,1,0:z])[where(finite(res[*,1,0:z]))])+[-0.1,0.1] else xrange = [-0.5,1.0]
                    plot,res[*,1,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='logg',symsize=0.7
                    hline,average(res[*,2,0:z]),color=(*self.colors).blue
                    vline,average(res[*,1,0:z]),color=(*self.colors).blue
                    ;;,xrange=[-0.5,1.0],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                    if (where(finite(res[*,3,0:z])))[0] ne -1 then xrange=mm((res[*,3,0:z])[where(finite(res[*,3,0:z]))])+[-.1,.1] else xrange = [1,6]
                    plot,res[*,3,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='microturb',symsize=0.7
                    hline,average(res[*,2,0:z]),color=(*self.colors).blue
                    vline,average(res[*,3,0:z]),color=(*self.colors).blue
                    ;;,xrange=[1,6],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                    self->endplot,3
                    !p.multi=[0,1,1]
                 endif 
                 
              endif
;              plot,res[*,1,*],res[*,0,*],psym=8,xrange=[-0.5,1.0],yrange=[3400,4400],/xs,/ys
;              plot,res[*,3,*],res[*,0,*],psym=8,xrange=[1,6],yrange=[3400,4400],/xs,/ys
;              plot,res[*,3,*],res[*,1,*],psym=8,xrange=[1,6],yrange=[-0.5,1.0],/xs,/ys
              
              widget_control,self.progbarB,set_value=(z+1d0)/trials
           endif
        endfor
     endfor
     
     if (where(finite(res[*,3])))[0] eq -1 then use = allres else use = res   
     x=(use[*,3,*])[where(finite(use[*,3,*]))] 
                                ; print,'x',average(x),stddev(x),max([stddev(x),self.dx*self.div/5d0])
     if (where(finite(res[*,0])))[0] eq -1 then use = allres else use = res  
     t=(use[*,0,*])[where(finite(use[*,0,*]))] 
                                ; print,'t',average(t),stddev(t),max([stddev(t),self.dt*self.div/5d0])       
     
     if (where(finite(res[*,1])))[0] eq -1 then use = allres else use = res   
     g=(use[*,1,*])[where(finite(use[*,1,*]))] 
                                ; print,'g',average(g),stddev(g),max([stddev(g),self.dg*self.div/5d0])               
     
     if (where(finite(res[*,2])))[0] eq -1 then use = allres else use = res   
     z=(use[*,2,*])[where(finite(use[*,2,*]))] 
                                ; print,'z',average(z),stddev(z),max([stddev(z),self.dz*self.div/5d0])
                                ; print,'div is',self.div,', trials: ',trials,' snr: ',snr
     
   ;  stop
     if 1-keyword_set(data) then begin
        self.mc_sigs = [max([stddev(t[where(finite(t))]),self.dt*self.div/4d0]),$
                        max([stddev(g[where(finite(g))]),self.dg*self.div/4d0]),$
                        max([stddev(z[where(finite(z))]),self.dz*self.div/4d0]),$
                        max([stddev(x[where(finite(x))]),self.dx*self.div/4d0])]
        
        (*self.active_object).mc_params[*,1] = self.mc_sigs                               
        (*self.active_object).mc_params[*,0] = [average(t[where(finite(t))]),$
                                                average(g[where(finite(g))]),$
                                                average(z[where(finite(z))]),$
                                                average(x[where(finite(x))])]
     endif else begin
        self.mc_sigs_data = [max([stddev(t[where(finite(t))]),self.dt*self.div/4d0]),$
                        max([stddev(g[where(finite(g))]),self.dg*self.div/4d0]),$
                        max([stddev(z[where(finite(z))]),self.dz*self.div/4d0]),$
                        max([stddev(x[where(finite(x))]),self.dx*self.div/4d0])]
        
        (*self.active_object).mc_params_data[*,1] = self.mc_sigs_data                          
        (*self.active_object).mc_params_data[*,0] = [average(t[where(finite(t))]),$
                                                     average(g[where(finite(g))]),$
                                                     average(z[where(finite(z))]),$
                                                     average(x[where(finite(x))])]
        
     endelse

     ;seedstring = string((*self.active_object).obj,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')+$
     ;             '     '+string(params[0],format='(i5)')  +'  '+string(max([stddev(t),self.dt*self.div/4d0]),format='(i4)')+$
     ;             '     '+string(params[1],format='(d5.2)')+'  '+string(max([stddev(g),self.dg*self.div/4d0]),format='(d5.2)')+$
     ;             '     '+string(params[2],format='(d5.2)')+'  '+string(max([stddev(z),self.dz*self.div/4d0]),format='(d5.2)')+$
     ;             '     '+string(params[3],format='(d5.2)')+'  '+string(max([stddev(x),self.dx*self.div/4d0]),format='(d4.2)')+$
     ;             string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
     
    ; seedstring = string((*self.active_object).obj,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')+$
    ;              '     '+string(average(t[where(finite(t))]),format='(i5)')  +'  '+string(max([stddev(t),self.dt*self.div/4d0]),format='(i4)')+$
    ;              '     '+string(average(g[where(finite(g))]),format='(d5.2)')+'  '+string(max([stddev(g),self.dg*self.div/4d0]),format='(d5.2)')+$
    ;              '     '+string(average(z[where(finite(z))]),format='(d5.2)')+'  '+string(max([stddev(z),self.dz*self.div/4d0]),format='(d5.2)')+$
    ;              '     '+string(average(x[where(finite(x))]),format='(d5.2)')+'  '+string(max([stddev(x),self.dx*self.div/4d0]),format='(d4.2)')+$
    ;              string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
     
     self.makeplot+=add
          
     
     !p = ps
     !x = xs
     !y = ys
     lam1 = 0L
     lam2 = 0L
     wide = 0L
     ps = 0L
     xs = 0L
     ys = 0L
     dir = 0L
     objspec = 0L
  endif else begin
     if 1-keyword_set(data) then self.mc_sigs = (*self.active_object).mc_params[*,1] else $
        self.mc_sigs_data = (*self.active_object).mc_params_data[*,1]
  endelse
 
  
  if 1-keyword_set(data) then params = (*self.active_object).mc_params else  params = (*self.active_object).mc_params_data
  seedstring = string((*self.active_object).obj,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')+$
               '     '+string(params[0,0],format='(i5)')  +'  '+string(params[0,1],format='(i4)')+$
               '     '+string(params[1,0],format='(d5.2)')+'  '+string(params[1,1],format='(d5.2)')+$
               '     '+string(params[2,0],format='(d5.2)')+'  '+string(params[2,1],format='(d5.2)')+$
               '     '+string(params[3,0],format='(d5.2)')+'  '+string(params[3,1],format='(d4.2)')+$
               string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
  
  if 1-keyword_set(data) then rt = 5 else rt = 9
  self->writetoascii,seedstring+'  '+self.runtype,rt
  
  widget_control,self.progbarB,set_value=0
  
  ;;    -- error bars are then the ellipses which hold 68% of those
  ;;       100 fitted parameters, centered on the "perfect model"
  ;;       params.
  
                                ; endif else begin
                                ;    stop
                                ;    
                                ; endelse
  
end


pro specfit::new_analysis,waverange=waverange
  self->message,message='Monte Carlo error estimation...',which='B'
  
  add = 0
  if self.makeplot eq 1 then begin
     add = 1
     self.makeplot = 0
  endif
  
  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  ps = !p
  xs = !x
  ys = !y

  ;; basic idea:
  ;; 1)   use best fit model, isolate those planes in the chi-square
  ;;      grid, interpolate to finer model grid and select the "best" fits
  ;;      per plane.
  
  if 1 then begin
     *self.location = (*self.active_object).bestloc
     self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=bwave,sflux=bflux,derr=derr,dflux=dflux        
     ;; self->contcorrect,flux=sflux,comp=dflux,cont=cont
  endif
  
  chiarr = self->makechiarr(waverange)
  if (where(finite(chiarr)))[0] ne -1 then $
     if max(chiarr(where(finite(chiarr)))) ne 0 then self->custom_analysis_mask,chiarr,params=params,sigs=sigs

  ;; 2) -- interpolate a "perfect model" from params
  self->lininterp,params,iwave,iflux ;,/makestop

  good = where(iwave gt min(modwave) and iwave lt max(modwave) and finite(iflux))
  if good[0] ne -1 then begin
     iflux = iflux[good]
     iwave = iwave[good]
     spec_conv5aa,iwave,iflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=pwave,sflux=pflux,derr=derr,dflux=dflux        
     pfluxc = pflux
;  perr = pflux/100 ;; S/N of 100
     ;; self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
     ;; self->contcorrect,flux=pfluxc,comp=dflux,cont=cont
     ;; fluxim = sflux
  endif else stop

                                ; stop
  
  if self.nomg then begin   
     lam1 = 1.2083
     lam2 = 1.1828
     wide = 0.0006
     chibad1 = where(bwave gt lam1-wide and bwave lt lam1+wide) 
     chibad2 = where(bwave gt lam2-wide and bwave lt lam2+wide)
     chibad = [chibad1,chibad2]
     if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
     if chibad[0] ne -1 then bflux[chibad] = sqrt(-1)
     savethese = where(finite(bflux))
     
     bwave = bwave[where(finite(bflux))]
     pwave = pwave[where(finite(bflux))]
     pflux = pflux[where(finite(bflux))]
     dflux = dflux[where(finite(bflux))]
     derr = derr[where(finite(bflux))]
     bflux = bflux[where(finite(bflux))]
                                ;  stop
  endif
  

  if 0 then window
  !p.multi=[0,1,1]
  self->startplot,3
  colors=specfit_colors()
  plot,bwave,bflux,xrange=waverange,color=colors.black,psym=10
  oplot,pwave,pflux,color=colors.blue,psym=10
  oplot,bwave,bflux-pflux+0.3,color=colors.red,psym=10
  self->endplot,3
;endif

  ;;    -- Fit this with the "best model" (for the data)
  ;;    -- increase the S/N of the interpolated model until the
  ;;          chi-square of that "perfect model" matches the
  ;;          chi-square of the "best model" vs data... that is S/N
  ;;          for test
  
  ;; assume SNR of 100
  
  ;; print,
                                ; stop


  
  snr = 100d0
  noise = randomn(seed,n_elements(pflux))*(1d0/snr)
                                ; noise -= average(noise)

  cval = total((((bflux-((pflux+noise)))/(((pflux+noise))/snr))^2d0)[where(bwave le waverange[1] and bwave ge waverange[0])])

  print,cval
  if cval gt min(chiarr) then $
     while cval gt min(chiarr) do begin
     snr-=1
     noise = randomn(seed,n_elements(pflux))*(1d0/snr)
                                ;   noise -= average(noise)
     noise*=0
     cval = total((((bflux-(pflux+noise))/((pflux+noise)/snr))^2)[where(bwave le waverange[1] and bwave ge waverange[0])])
  endwhile else $
     while cval le min(chiarr) do begin
     snr+=1
     noise = randomn(seed,n_elements(pflux))*(1d0/snr)
                                ;  noise -= average(noise)
     noise*=0
     cval = total((((bflux-(pflux+noise))/((pflux+noise)/snr))^2)[where(bwave le waverange[1] and bwave ge waverange[0])])
  endwhile
  
  print,min(chiarr),cval,snr
  if finite(cval) eq 0 then stop
  
  ;; snr = 25

  ;;    -- randomly generate 100 "perfect models" with that S/N and
  ;;       fit to the model grid (should be very fast since no
  ;;       scaling, continuum, etc issues).  extract parameters in the
  ;;       same way as 1), store these. 
  
  newwave = pwave
  trials = 300
  mc_params = dblarr(4,trials)

  if 0 then begin ;; just do the full grid...
     ;; newwave, newflux needs to be fit to the planes that give the
     ;; perfect model.  construct those planes here:

     ;; teff v logg
     tvg = dblarr(n_elements((*self.models).grid.params.temps),$
                  n_elements((*self.models).grid.params.gravs),$
                  n_elements(bflux))
     for i=0,(size(tvg))[1]-1 do $
        for j=0,(size(tvg))[2]-1 do begin
        *self.location = (*self.active_object).bestloc
        (*self.location)[0] = i           ;; temp
        (*self.location)[1] = j           ;; logg
        self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=twave,sflux=tflux        
        tvg[i,j,*]=tflux[savethese]
     endfor
     
     ;; teff v Z
     tvz = dblarr(n_elements((*self.models).grid.params.temps),$
                  n_elements((*self.models).grid.params.abuns),$
                  n_elements(bflux))
     for i=0,(size(tvz))[1]-1 do $
        for j=0,(size(tvz))[2]-1 do begin
        *self.location = (*self.active_object).bestloc
        (*self.location)[0] = i           ;; temp
        (*self.location)[2] = j           ;; Z
        self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=twave,sflux=tflux        
        tvz[i,j,*]=tflux[savethese]
     endfor
     
     ;; teff v xi
     tvx = dblarr(n_elements((*self.models).grid.params.temps),$
                  n_elements((*self.models).grid.params.turbs),$
                  n_elements(bflux))
     for i=0,(size(tvx))[1]-1 do $
        for j=0,(size(tvx))[2]-1 do begin
        *self.location = (*self.active_object).bestloc
        (*self.location)[0] = i           ;; temp
        (*self.location)[3] = j           ;; Z
        self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=twave,sflux=tflux        
        tvx[i,j,*]=tflux[savethese]
     endfor
     
  endif else begin
     grid = dblarr(n_elements(bwave),$
                   n_elements((*self.models).grid.params.temps),$
                   n_elements((*self.models).grid.params.gravs),$
                   n_elements((*self.models).grid.params.abuns),$
                   n_elements((*self.models).grid.params.turbs))                   
     for i=0,n_elements((*self.models).grid.params.temps)-1 do $
        for j=0,n_elements((*self.models).grid.params.gravs)-1 do $
           for k=0,n_elements((*self.models).grid.params.abuns)-1 do $
              for l=0,n_elements((*self.models).grid.params.turbs)-1 do begin
        *self.location = [i,j,k,l]
        self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=twave,sflux=tflux        
        grid[*,i,j,k,l]=tflux[savethese]
     endfor
  endelse
  


  outpars = dblarr(2,trials)

  min = min((*self.active_object).chisq,x,/nan)
  dothis = array_indices((*self.active_object).chisq,x)
  if n_elements(dothis) eq 3 then doxi = 0 else doxi = 1
  

  taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
  tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
  zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
  gaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
  gax = 1d0*findgen(n_elements(gaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(gaxis))
  if doxi then begin
     xiaxis = fillarr(self.dx/10d0,mm((*self.models).grid.params.turbs))
     xiax = 1d0*findgen(n_elements(xiaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(xiaxis))
  endif
  
  if 0 then begin ;;; THIS IS OLD
     for z=0,trials-1 do begin
        noise = randomn(seed,n_elements(bflux))*((1d0/snr))
        ;; noise -= average(noise)
        newflux = pflux+noise
        
        grid = tvz
        gx = 'temp'
        gy = 'abun'
        csq = dblarr((size(grid))[1],(size(grid))[2])
        for i=0,(size(grid))[1]-1 do $
           for j=0,(size(grid))[2]-1 do $
              csq[i,j]= total((((newflux-grid[i,j,*])/(1d0/snr))^2d0))
        
        case gx of 
           'temp': begin
              xaxis = taxis
              xax = tax
           end
           'grav': begin
              xaxis = gaxis
              xax = gax
           end
           'abun': begin
              xaxis = zaxis
              xax = zax
           end
           'turb': begin
              xaxis = xiaxis
              xax = xiax
           end
           
        endcase 
        case gy of 
           'temp': begin
              yaxis = taxis
              yax = tax
           end
           'grav': begin
              yaxis = gaxis
              yax = gax
           end
           'abun': begin
              yaxis = zaxis
              yax = zax
           end
           'turb': begin
              yaxis = xiaxis
              yax = xiax
           end
        endcase
        
        result = interpolate(csq,xax,yax,/grid,/missing,cubic=-.5)
        
        xv = ((where(result eq min(result)))[0] mod (size(result))[1])
        yv = (where(result[xv,*] eq min(result)))[0]
        xval = xaxis[xv]
        yval = yaxis[yv]
        outpars[*,z] = [xval,yval]
        

                                ; print,xval
                                ; print,yval
     endfor
  endif else begin
     res = dblarr(3,4,trials) + sqrt(-1)
     
     if doxi eq 0 then res[*,3,*] = (*self.models).grid.params.turbs

     bestloc = (*self.active_object).bestloc
     
     !p.multi=[0,3,2]
     for z=0,trials-1 do begin
        noise = randomn(seed,n_elements(bflux))*((1d0/snr))
        ;; noise -= average(noise)
        newflux = pflux+noise
        
        csq = dblarr(n_elements((*self.models).grid.params.temps),$
                     n_elements((*self.models).grid.params.gravs),$
                     n_elements((*self.models).grid.params.abuns),$
                     n_elements((*self.models).grid.params.turbs))
        
        for i=0,n_elements((*self.models).grid.params.temps)-1 do $
           for j=0,n_elements((*self.models).grid.params.gravs)-1 do $
              for k=0,n_elements((*self.models).grid.params.abuns)-1 do $
                 for l=0,n_elements((*self.models).grid.params.turbs)-1 do $
                    csq[i,j,k,l] =  total(((newflux-grid[*,i,j,k,l])/(1d0/snr))^2d0)
        
        ;; now slice in each plane of interest...
        xarr = ['temp','temp','temp','abun','abun','grav']
        yarr = ['abun','grav','turb','grav','turb','turb']

        for j=0,n_elements(xarr)-1 do begin
           calc = 1
           if doxi eq 0 then if xarr[j] eq 'turb' or yarr[j] eq 'turb' then calc = 0
                                ; if calc eq 0 then stop
           if calc then begin 
              case j of
                 0: resloc = [[0,0],[0,2]]
                 1: resloc = [[1,0],[0,1]]
                 2: resloc = [[2,0],[0,3]] 
                 3: resloc = [[1,2],[1,1]]
                 4: resloc = [[2,2],[1,3]]
                 5: resloc = [[2,1],[2,3]]
              endcase

              case xarr[j] of 
                 'temp': begin
                    xaxis = taxis
                    xax = tax
                    if yarr[j] eq 'abun' then csq_slice = reform(csq[*,bestloc[1],         *,bestloc[3]])
                    if yarr[j] eq 'grav' then csq_slice = reform(csq[*,         *,bestloc[2],bestloc[3]])
                    if yarr[j] eq 'turb' then csq_slice = reform(csq[*,bestloc[1],bestloc[2],         *])
                 end
                 'grav': begin
                    xaxis = gaxis
                    xax = gax
                    if yarr[j] eq 'abun' then csq_slice =                reform(csq[bestloc[0],         *,         *,bestloc[3]])
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,         *,bestloc[2],bestloc[3]]),1))
                    if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],         *,bestloc[2],         *])
                 end
                 'abun': begin
                    xaxis = zaxis
                    xax = zax
                    if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,         *,bestloc[3]]),1))
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],         *,bestloc[3]]),1))
                    if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],bestloc[1],         *,         *])
                 end
                 'turb': begin
                    xaxis = xiaxis
                    xax = xiax
                    if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],bestloc[2],*]),1))
                    if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,bestloc[2],*]),1))
                    if yarr[j] eq 'abun' then csq_slice = reverse(rotate(reform(csq[bestloc[0],bestloc[1],         *,*]),1))
                 end
                 
              endcase 
              case yarr[j] of 
                 'temp': begin
                    yaxis = taxis
                    yax = tax
                 end
                 'grav': begin
                    yaxis = gaxis
                    yax = gax
                 end
                 'abun': begin
                    yaxis = zaxis
                    yax = zax
                 end
                 'turb': begin
                    yaxis = xiaxis
                    yax = xiax
                 end
              endcase
              
              if n_elements(xax) eq 1 then begin
                 result = interpolate(csq_slice,yax,/grid,/missing,cubic=-.5)
                 m = min(result,place)
                 
                 res[resloc[0,0],resloc[1,0],z]=xax[0]
                 res[resloc[0,1],resloc[1,1],z]=yaxis[place]
              endif else if n_elements(yax) eq 1 then begin
                 result = interpolate(csq_slice,xax,/grid,/missing,cubic=-.5)
                 m = min(result,place)
                 
                 res[resloc[0,0],resloc[1,0],z]=xax[place]
                 res[resloc[0,1],resloc[1,1],z]=yax[0]
              endif else begin
                 
                 
                 
                 
                 result = interpolate(csq_slice,xax,yax,/grid,/missing,cubic=-.5)
                 xv = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
                 if (xv)[0] ne -1 then begin
                    
                    yv = (where(result[xv,*] eq min(result,/nan)))[0]
                    xval = xaxis[xv]
                    yval = yaxis[yv]
                    
                    res[resloc[0,0],resloc[1,0],z] = xval
                    res[resloc[0,1],resloc[1,1],z] = yval
                 endif else begin 
                    res[resloc[0,0],resloc[1,0],z] = sqrt(-1)
                    res[resloc[0,1],resloc[1,1],z] = sqrt(-1)
                 endelse
              endelse
              !p.multi=[0,3,1]
              !x.margin=[10,1]
              !y.margin=[5,3]
              self->color_manage,'color'
              
              if (z mod 24) eq 0 and z gt 0 then begin
                 yrange = mm((res[*,2,0:z])[where(finite(res[*,2,0:z]))])+[-.01,.01]
                 self->startplot,3
                 if (where(finite(res[*,0,0:z])))[0] ne -1 then xrange = mm((res[*,0,0:z])[where(finite(res[*,0,0:z]))])+[-20,20] else xrange = [3400,4400]
                 plot,res[*,0,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='T!deff!n' 
                 ;; xrange=[3400,4400],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                 if (where(finite(res[*,1,0:z])))[0] ne -1 then xrange = mm((res[*,1,0:z])[where(finite(res[*,1,0:z]))])+[-0.1,0.1] else xrange = [-0.5,1.0]
                 plot,res[*,1,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='logg'
                 ;;,xrange=[-0.5,1.0],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                 if (where(finite(res[*,3,0:z])))[0] ne -1 then xrange=mm((res[*,3,0:z])[where(finite(res[*,3,0:z]))])+[-.1,.1] else xrange = [1,6]
                 plot,res[*,3,*],res[*,2,*],psym=8,xrange=xrange,yrange=yrange,/xs,/ys,ytitle='Z',xtitle='microturb'
                 ;;,xrange=[1,6],yrange=[-1,1],/xs,/ys,ytitle='Z',xtitle='T!deff!n'
                 self->endplot,3
                 !p.multi=[0,1,1]
              endif
;              plot,res[*,1,*],res[*,0,*],psym=8,xrange=[-0.5,1.0],yrange=[3400,4400],/xs,/ys
;              plot,res[*,3,*],res[*,0,*],psym=8,xrange=[1,6],yrange=[3400,4400],/xs,/ys
;              plot,res[*,3,*],res[*,1,*],psym=8,xrange=[1,6],yrange=[-0.5,1.0],/xs,/ys
              
              widget_control,self.progbarB,set_value=(z+1d0)/trials
           endif
        endfor
     endfor
  endelse
  
  x=(res[*,3,*])[where(finite(res[*,3,*]))] 
                                ; print,'x',average(x),stddev(x),max([stddev(x),self.dx*self.div/5d0])
  
  t=(res[*,0,*])[where(finite(res[*,0,*]))] 
                                ; print,'t',average(t),stddev(t),max([stddev(t),self.dt*self.div/5d0])       
  
  g=(res[*,1,*])[where(finite(res[*,1,*]))] 
                                ; print,'g',average(g),stddev(g),max([stddev(g),self.dg*self.div/5d0])               
  
  z=(res[*,2,*])[where(finite(res[*,2,*]))] 
                                ; print,'z',average(z),stddev(z),max([stddev(z),self.dz*self.div/5d0])
                                ; print,'div is',self.div,', trials: ',trials,' snr: ',snr
  
  self.mc_sigs = [max([stddev(t),self.dt*self.div/4d0]),$
                  max([stddev(g),self.dg*self.div/4d0]),$
                  max([stddev(z),self.dz*self.div/4d0]),$
                  max([stddev(x),self.dx*self.div/4d0])]
  
  seedstring = string((*self.active_object).obj,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')+$
               '     '+string(params[0],format='(i5)')  +'  '+string(max([stddev(t),self.dt*self.div/4d0]),format='(i4)')+$
               '     '+string(params[1],format='(d5.2)')+'  '+string(max([stddev(g),self.dg*self.div/4d0]),format='(d5.2)')+$
               '     '+string(params[2],format='(d5.2)')+'  '+string(max([stddev(z),self.dz*self.div/4d0]),format='(d5.2)')+$
               '     '+string(params[3],format='(d5.2)')+'  '+string(max([stddev(x),self.dx*self.div/4d0]),format='(d4.2)')+$
               string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')

  seedstring = string((*self.active_object).obj,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')+$
               '     '+string(average(t[where(finite(t))]),format='(i5)')  +'  '+string(max([stddev(t),self.dt*self.div/4d0]),format='(i4)')+$
               '     '+string(average(g[where(finite(g))]),format='(d5.2)')+'  '+string(max([stddev(g),self.dg*self.div/4d0]),format='(d5.2)')+$
               '     '+string(average(z[where(finite(z))]),format='(d5.2)')+'  '+string(max([stddev(z),self.dz*self.div/4d0]),format='(d5.2)')+$
               '     '+string(average(x[where(finite(x))]),format='(d5.2)')+'  '+string(max([stddev(x),self.dx*self.div/4d0]),format='(d4.2)')+$
               string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')

  self->writetoascii,seedstring+'  '+self.runtype,5

  widget_control,self.progbarB,set_value=0
  
  ;;    -- error bars are then the ellipses which hold 68% of those
  ;;       100 fitted parameters, centered on the "perfect model"
  ;;       params.
  
  self.makeplot+=add
  
  !p = ps
  !x = xs
  !y = ys
  lam1 = 0L
  lam2 = 0L
  wide = 0L
  ps = 0L
  xs = 0L
  ys = 0L
  dir = 0L
  objspec = 0L
end

pro specfit::gravity_analysis,waverange=waverange,customchi=customchi,strextra=strextra
  heap = 0
  if heap then help,/heap
  if 1-keyword_set(strextra) then strextra = ''

  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  ps = !p
  xs = !x
  ys = !y
  
  self->color_manage,'color'
  
  lam1 = 1.2083
  lam2 = 1.1828
  wide = 0.0006
  
  *self.location = (*self.active_object).bestloc
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
  
  
  usegrav = [-1.0,-0.5,0,0.5]*1d0
  gravstr = ['-1.0','-0.5','+0.0','+0.5']
  finparams = dblarr(4,4)       ;+sqrt(-1)
  finchisq = dblarr(4)+sqrt(-1)
  
                                ; stop
                                ;(*self.location)[0]

  for t=0,n_elements(usegrav)-1 do begin
     guse = usegrav[t]
                                ;  if (where((*self.active_object).fullchisqw[*,(*self.location)[0],(*self.location)[1].(*self.location)[2]] ge waverange[0] and (*self.active_object).fullchisqw[*,0,0,0] le waverange[1]))[0] ne -1 then begin
     if 1 then begin
        if keyword_set(customchi) then begin
           chiarr = customchi

           for k=0,n_elements(chiarr)-1,1 do begin
              ai = array_indices(chiarr,k)
              if (where(guse eq (*self.models).grid.params.gravs[ai[1]]))[0] eq -1 then chiarr[k] = abs(sqrt(-1))
              
              
           endfor
        endif else begin 
           chiarr = (*self.active_object).chisq*0d0
           
           for k=0,n_elements(chiarr)-1,1 do begin
              ai = array_indices(chiarr,k)
              if n_elements(ai) eq 3 then begin
                 wave = (*self.active_object).fullchisqw[*,ai[0],ai[1],ai[2]]
                 good = where(wave ge waverange[0] and wave le waverange[1] and finite((*self.active_object).fullchisq[*,ai[0],ai[1],ai[2]]))  
                 
                 if good[0] eq -1 then chiarr[k] = abs(sqrt(-1)) else begin
                    wave = (*self.active_object).fullchisqw[good,ai[0],ai[1],ai[2]]
                    chi = (*self.active_object).fullchisq[good,ai[0],ai[1],ai[2]]
                    chibad1 = where(wave gt lam1-wide and wave lt lam1+wide) 
                    chibad2 = where(wave gt lam2-wide and wave lt lam2+wide)
                    chibad = [chibad1,chibad2]
                    if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
                    
                    if chibad[0] ne -1 then begin
                       chi[chibad] = sqrt(-1)
                    endif
                    chi = chi[where(finite(chi))]

                    chiarr[k] = total(chi)/(n_elements(chi))
                 endelse
                 if (where(guse eq (*self.models).grid.params.gravs[ai[1]]))[0] eq -1 then chiarr[k] = abs(sqrt(-1))
                                ;    if self.turb_avoid eq (*self.models).grid.params.turbs[ai[3]] then chiarr[k] = abs(sqrt(-1))
                 if self.grav_avoid eq (*self.models).grid.params.gravs[ai[1]] then chiarr[k] = abs(sqrt(-1))
                 
              endif else begin
                 wave = (*self.active_object).fullchisqw[*,ai[0],ai[1],ai[2],ai[3]]
                 good = where(wave ge waverange[0] and wave le waverange[1] and finite((*self.active_object).fullchisq[*,ai[0],ai[1],ai[2],ai[3]]))  
                 
                 if good[0] eq -1 then chiarr[k] = abs(sqrt(-1)) else begin
                    wave = (*self.active_object).fullchisqw[good,ai[0],ai[1],ai[2],ai[3]]
                    chi = (*self.active_object).fullchisq[good,ai[0],ai[1],ai[2],ai[3]]
                    chibad1 = where(wave gt lam1-wide and wave lt lam1+wide) 
                    chibad2 = where(wave gt lam2-wide and wave lt lam2+wide)
                    chibad = [chibad1,chibad2]
                    if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
                    
                    if chibad[0] ne -1 then begin
                       chi[chibad] = sqrt(-1)
                    endif
                    chi = chi[where(finite(chi))]
                    
                    chiarr[k] = total(chi)/(n_elements(chi))
                 endelse
                 if (where(guse eq (*self.models).grid.params.gravs[ai[1]]))[0] eq -1 then chiarr[k] = abs(sqrt(-1))
                 if self.turb_avoid eq (*self.models).grid.params.turbs[ai[3]] then chiarr[k] = abs(sqrt(-1))
                 if self.grav_avoid eq (*self.models).grid.params.gravs[ai[1]] then chiarr[k] = abs(sqrt(-1))
              endelse
           endfor
        endelse

        
                                ;  stop
        
        if (where(finite(chiarr)))[0] ne -1 then $
           if max(chiarr(where(finite(chiarr)))) ne 0 then begin
           is = 'g'+gravstr[t]+'_'+string(waverange[0],format='(d5.3)')+'to'+string(waverange[1],format='(d5.3)')+strextra
           seedstring = string((*self.active_object).obj+strextra,waverange[0],waverange[1],format='(a25,d10.3,d10.3)')
           self->custom_analysis_mask,chiarr,string1=string1,string2=string2,string3=seedstring,is=is,polyranges=[waverange[0],lam2-wide,lam2+wide,waverange[1]],params=params
           seedstring+= string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
           
           self->writetoascii,seedstring+'  '+self.runtype+'  '+string(min(chiarr(where(finite(chiarr)))),format='(d30.10)'),3
           finparams[t,*] = params
           finchisq[t] = min(chiarr(where(finite(chiarr))))
        endif
                                ;  stop
     endif     
  endfor
  
  
  weight = (1/finchisq)/total(1/finchisq[where(finite(finchisq))])     
  if (where(finite(weight) eq 0))[0] ne -1 then weight[where(finite(weight) eq 0)] = 0     
  fin  = dblarr(4)
  sfin = dblarr(4)
  for i=0,3 do fin[i]=total(finparams[*,i]*weight)
  for i=0,3 do begin
     good = where(weight gt 0,c)
     if c lt 2 then sfin[i] = 0 else $
        sfin[i]=stddev((finparams[good,i]*weight[good]*n_elements(good)))
  endfor
  
  self.grav_param = fin
  self.grav_sigs = sfin

  str = string((*self.active_object).obj+strextra,waverange[0],waverange[1],$
               '     ',fin[0],'  ',sfin[0],$
               '     ',fin[1],'  ',sfin[1],$
               '     ',fin[2],'  ',sfin[2],$
               '     ',fin[3],'  ',sfin[3],format='(a30,d10.3,d10.3,a,i5,a,i4,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d5.2,a,d4.2)')+$
        string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
  self->writetoascii,str+'  '+self.runtype,4
  
                                ; stop  

  finchisq =0L
  weight=0L
  sfin = 0L
  fin = 0L
  str = 0L
  finparams = 0L
  usegrav = 0L
  gravstr = 0L
  
  !p = ps 
  !x = xs 
  !y = ys 

  if heap then help,/heap
  if heap then stop
end

function specfit::find_bestloc,size,chiarr=chiarr,doweight=doweight,weight=weight
  
  
  ;;; 
  ximin = 0
  tmin = 0
  if 1 then begin
     ximin = 3
   ;  tmin = 3600
  endif

  pars =  (*self.models).grid.params
  
  if 1-keyword_set(chiarr) then chiarr = (*self.active_object).chisq ;else stop
  
  ;; t = fillarr(1,1,n_elements((*self.models).grid.params.temps)-2)
  ;; g = fillarr(1,1,n_elements((*self.models).grid.params.gravs)-2)
  ;; z = fillarr(1,1,n_elements((*self.models).grid.params.abuns)-2)
  ;; x = fillarr(1,1,n_elements((*self.models).grid.params.turbs)-2)
  
  if self.lock_param[3] then begin
     minchi = 1d4
     bestloc = -1
     for t=1,n_elements((*self.models).grid.params.temps)-2 do $
        for g=1,n_elements((*self.models).grid.params.gravs)-2 do $
           for z=1,n_elements((*self.models).grid.params.abuns)-2 do begin
        
        center = [t,g,z]
        mc = chiarr[t-1:t+1,g-1:g+1,z-1:z+1]
        ;;    chi = total(mc)/n_elements(where(finite(mc)))
        ;;    if keyword_set(doweight) then
        chi = total(mc)/n_elements(where(finite(mc)))
        ;;if weight eq -1 then chi = chiarr[t,g,z,x]
        ;; if keyword_set(doweight) then stop
        
        if chi lt minchi then begin
          
           minchi = chi
           bestloc = center
           
                                ;     stop
           
                                ;      print,(*self.active_object).bestloc,format='(i2,i2,i2,i2)'
                                ;      print,bestloc,minchi,format='(i2,i2,i2,i2,d8.1)'
           ;; print,'********'
           ;; stop
        endif
     endfor
 

     chiarr = 0L
     (*self.active_object).bestloc = bestloc
     
     (*self.active_object).bestwave = (*self.active_object).fullwave[*,bestloc[0],bestloc[1],bestloc[2]]
     (*self.active_object).bestmod =  (*self.active_object).fullmod[*,bestloc[0],bestloc[1],bestloc[2]]
     (*self.active_object).bestdat =  (*self.active_object).fulldat[*,bestloc[0],bestloc[1],bestloc[2]]
     
     if (where((*self.active_object).bestwave eq 0))[0] ne -1 then begin
        (*self.active_object).bestmod[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
        (*self.active_object).bestdat[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
        (*self.active_object).bestwave[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
     endif
     
  endif else begin
     ;; if 1-keyword_set(weight) then weight = 0.25
     
     ;; hack in:
   ;  weight = -1
                                ;if keyword_set(doweight) then begin
     if 1-keyword_set(weight) then weight = 1.0
     mcweight = dblarr(3,3,3,3)
     mcweight+=weight
     mcweight[1,1,1,1] = 1
                                ;endif
     
     minchi = 1d9
     bestloc = -1
     for t=1,n_elements((*self.models).grid.params.temps)-2 do $
        for g=1,n_elements((*self.models).grid.params.gravs)-2 do $
           for z=1,n_elements((*self.models).grid.params.abuns)-2 do $
              for x=1,n_elements((*self.models).grid.params.turbs)-2 do begin
        center = [t,g,z,x]
        mc = chiarr[t-1:t+1,g-1:g+1,z-1:z+1,x-1:x+1]
        ;mc = chiarr[t-1:t+1,g,z-1:z+1,x-1:x+1]

        ;;    chi = total(mc)/n_elements(where(finite(mc)))
        ;;    if keyword_set(doweight) then
       
        if size gt 1 and (*self.active_object).lockres eq 1 then chi = total(mc)/n_elements(where(finite(mc))) else chi = chiarr[t,g,z,x]
        ;; if keyword_set(doweight) then stop
                                ;  stop
        if chi lt minchi then begin
                                ; stop
           if (pars.(3)[x] ge ximin and pars.(0)[t] ge tmin) or (*self.active_object).lockres eq 0 then begin
              minchi = chi
              bestloc = center
           endif
           ;;     stop
           
           ;;      print,(*self.active_object).bestloc,format='(i2,i2,i2,i2)'
           ;;      print,bestloc,minchi,format='(i2,i2,i2,i2,d8.1)'
           ;; print,'********'
           ;; stop
        endif
     endfor
     ;;  print,"minchi:  ",(*self.active_object).bestloc,format='(a10,i2,i2,i2,i2)'
     ;;  print,'minzone: ',bestloc,minchi,format='(a10,i2,i2,i2,i2,d8.3)'
     ;;  print,'..'
     ;;  stop

     if bestloc[0] eq -1 then stop

     chiarr = 0L
     (*self.active_object).bestloc = bestloc
     
     (*self.active_object).bestwave = (*self.active_object).fullwave[*,bestloc[0],bestloc[1],bestloc[2],bestloc[3]]
     (*self.active_object).bestmod =  (*self.active_object).fullmod[*,bestloc[0],bestloc[1],bestloc[2],bestloc[3]]
     (*self.active_object).bestdat =  (*self.active_object).fulldat[*,bestloc[0],bestloc[1],bestloc[2],bestloc[3]]
     
     if (where((*self.active_object).bestwave eq 0))[0] ne -1 then begin
        (*self.active_object).bestmod[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
        (*self.active_object).bestdat[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
        (*self.active_object).bestwave[where((*self.active_object).bestwave eq 0)] = sqrt(-1)
     endif
                                ; stop
  endelse
                                ; stop
  
  return,bestloc
end

pro specfit::compact_analysis,waverange=waverange,customchi=customchi,customres=customres,strextra=strextra
  if 1-keyword_set(strextra) then strextra = ''
  
  heap = 0
  if heap then help,/heap
  
  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  ps = !p
  xs = !x
  ys = !y
  
  self->color_manage,'color'

  lam1 = 1.2083
  lam2 = 1.1828
  wide = 0.0006
  
  ;; stop
  
 ; bestloc_single = (*self.active_object).bestloc
  
  csq = self->makechiarr_old(waverange)
  
                                ; stop
;  stop

  bestloc_g3 = self->find_bestloc(self.locsize,chiarr=csq)
  
  

  ;;  stop
 ; *self.location = bestloc_g3
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
  
  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=(*self.active_object).overallres
  
  self->writetotex,'',/output_report,/begin_bigtable
  
  chiarr = self->makechiarr_old(waverange)
  

  ;stop

  if max(chiarr(where(finite(chiarr)))) ne 0 then begin
     ;;   seedstring  =string(self.active_num,format='(i)')+' -1  -1 '+$
     ;;                '    '+string(waverange[0],format='(d5.3)')+'    '+string(waverange[1],format='(d5.3)')
     
                                ;    self->custom_analysis,chiarr,good_lambda=waverange,string1=string1,string2=string2,string3=seedstring
                                ;    print,seedstring
     
     seedstring  =string(self.active_num,format='(i)')+' -1  -1 '+$
                  '    '+string(waverange[0],format='(d5.3)')+'    '+string(waverange[1],format='(d5.3)')
     
     is = string(waverange[0],format='(d5.3)')+'to'+string(waverange[1],format='(d5.3)')+strextra
     seedstring = string((*self.active_object).obj+strextra,waverange[0],waverange[1],format='(a35,d10.3,d10.3)')
     self->custom_analysis_mask,chiarr,string1=string1,string2=string2,string3=seedstring,is=is,polyranges=[waverange[0],lam2-wide,lam2+wide,waverange[1]],params=params
     seedstring+= string('          ',self.expected_res,'      ',(*self.active_object).overallres,format='(a,i5.5,a,i5.5)')
     
     
     
     self->writetoascii,seedstring+'  '+self.runtype,1
     self->writetoascii,string((*self.active_object).obj+strextra,waverange[0],waverange[1],' ',format='(a35,d10.3,d10.3,a)')+$
                        string2+string('        ',self.expected_res,'  ',(*self.active_object).overallres,' ',format='(a,i5.5,a,i5.5,a)')+$
                        '  '+self.runtype,2
     
     pars =  (*self.models).grid.params

     if 0 then begin
        l = self->find_bestloc(3)
        print,3
        print,chiarr[l[0],l[1],l[2],l[3]]
        print,pars.(0)[l[0]],pars.(1)[l[1]],pars.(2)[l[2]],pars.(3)[l[3]]
        print,''
        
        print,1
        l = self->find_bestloc(1)
        print,chiarr[l[0],l[1],l[2],l[3]]
        print,pars.(0)[l[0]],pars.(1)[l[1]],pars.(2)[l[2]],pars.(3)[l[3]]
        print,''
     endif
     
     
     ;;  l = self->find_bestloc(3)
     
     if self.expected_res eq 3000 or 1 then begin
        
        ao = (*self.active_object)
        self->color_manage,'color'  
        !p.multi=[0,1,1]
        self->startplot,0
        !x.thick=2
        !y.thick=2
        !p.thick=2
        l = self->find_bestloc(1)
        l2 = self->find_bestloc(3)
        yr =range_find([ao.bestdat,$
                       ao.fullmod[*,l[0],l[1],l[2],l[3]],$
                       ao.fullmod[*,l2[0],l2[1],l2[2],l2[3]]],0.05)
        yr[0] = max([yr[0],0])
        plot,ao.bestwave,ao.bestdat,color=(*self.colors).black,psym=10,yrange=yr,/ys
        
        oplot,ao.fullwave[*,l[0],l[1],l[2],l[3]],ao.fullmod[*,l[0],l[1],l[2],l[3]],color=(*self.colors).red,psym=10
        xyouts,self.fitrange[0]+0.001,yr[0]+0.1*(yr[1]-yr[0]),'l=1 '+string(pars.(0)[l[0]],pars.(1)[l[1]],pars.(2)[l[2]],pars.(3)[l[3]],format='(i4,d+5.1,d+6.2,d4.1)'),color=(*self.colors).red
        
        
                                ;  l2 = self->find_bestloc(3)
        l=l2
        oplot,ao.fullwave[*,l[0],l[1],l[2],l[3]],ao.fullmod[*,l[0],l[1],l[2],l[3]],color=(*self.colors).blue,psym=10
        xyouts,self.fitrange[0]+0.001,yr[0]+0.06*(yr[1]-yr[0]),'l=3 '+string(pars.(0)[l[0]],pars.(1)[l[1]],pars.(2)[l[2]],pars.(3)[l[3]],format='(i4,d+5.1,d+6.2,d4.1)'),color=(*self.colors).blue        
        
        self->endplot,0;stop
        ;;stop
     endif
     
     self->writetotex,string1,/output_report,/write_tobig,string2=string2
  endif
  
  self->writetotex,'',/output_report,/end_bigtable
  
  !p = ps 
  !x = xs 
  !y = ys 
  lam1 = 0L
  lam2 = 0L
  wide = 0L
  ps = 0L
  xs = 0L
  ys = 0L
  dir = 0L
  objspec = 0L
  
  
  
  if heap then help,/heap
  
  if heap then stop             ; stop
end


pro specfit::custom_analysis_mask,csq,string1=string1,string2=string2,$
                                  string3=string3,plot=plot,is=is,polyranges=polyranges,$
                                  params=params,sigs=sigs,dothis=dothis
  
  if keyword_set(is) then extra = is else extra = ''
  if 1-keyword_set(string3) then string3=''
  
  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  
  ps = !p
  xs = !x
  ys = !y
  
  !x.margin=[6,3]
  !y.margin=[3,1]
  title = ''

  ;; dothis = (*self.active_object).bestloc
  if 1-keyword_set(dothis) then begin
     ;; min = min(chisq_arr,x,/nan)
     ;; dothis = array_indices((*self.active_object).chisq,x)
     chiarr = csq
     dothis = self->find_bestloc(self.locsize,chiarr=chiarr)
  endif 
  if n_elements(dothis) eq 3 then dothis = [dothis,0]
  bestloc = dothis

  xarr = ['temp','temp','temp','abun','abun','grav']
  yarr = ['abun','grav','turb','grav','turb','turb']
    
  if n_elements(dothis) eq 3 then doxi = 0 else doxi = 1
  
  taxis = fillarr(self.dt/2d0,mm((*self.models).grid.params.temps))
  tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
  zaxis = fillarr(self.dz/2d0,mm((*self.models).grid.params.abuns))
  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
  gaxis = fillarr(self.dg/2d0,mm((*self.models).grid.params.gravs))
  gax = 1d0*findgen(n_elements(gaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(gaxis))
  if doxi then begin
     xiaxis = fillarr(self.dx/2d0,mm((*self.models).grid.params.turbs))
     xiax = 1d0*findgen(n_elements(xiaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(xiaxis))
  endif

  res = dblarr(3,4)  + sqrt(-1)
  allres = res
  if doxi eq 0 then res[*,3] = (*self.models).grid.params.turbs


  for j=0,n_elements(xarr)-1 do begin
     calc = 1
     if doxi eq 0 then if xarr[j] eq 'turb' or yarr[j] eq 'turb' then calc = 0
     
     case j of
        0: resloc = [[0,0],[0,2]]
        1: resloc = [[1,0],[0,1]]
        2: resloc = [[2,0],[0,3]] 
        3: resloc = [[1,2],[1,1]]
        4: resloc = [[2,2],[1,3]]
        5: resloc = [[2,1],[2,3]]
     endcase
     
     case xarr[j] of 
        'temp': begin
           xaxis = taxis
           xax = tax
           if yarr[j] eq 'abun' then csq_slice = reform(csq[*,bestloc[1],         *,bestloc[3]])
           if yarr[j] eq 'grav' then csq_slice = reform(csq[*,         *,bestloc[2],bestloc[3]])
           if yarr[j] eq 'turb' then csq_slice = reform(csq[*,bestloc[1],bestloc[2],         *])
        end
        'grav': begin
           xaxis = gaxis
           xax = gax
           if yarr[j] eq 'abun' then csq_slice =                reform(csq[bestloc[0],         *,         *,bestloc[3]])
           if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,         *,bestloc[2],bestloc[3]]),1))
           if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],         *,bestloc[2],         *])
        end
        'abun': begin
           xaxis = zaxis
           xax = zax
           if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,         *,bestloc[3]]),1))
           if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],         *,bestloc[3]]),1))
           if yarr[j] eq 'turb' then csq_slice =                reform(csq[bestloc[0],bestloc[1],         *,         *])
        end
        'turb': begin
           xaxis = xiaxis
           xax = xiax
           if yarr[j] eq 'temp' then csq_slice = reverse(rotate(reform(csq[         *,bestloc[1],bestloc[2],*]),1))
           if yarr[j] eq 'grav' then csq_slice = reverse(rotate(reform(csq[bestloc[0],         *,bestloc[2],*]),1))
           if yarr[j] eq 'abun' then csq_slice = reverse(rotate(reform(csq[bestloc[0],bestloc[1],         *,*]),1))
        end
     endcase
     case yarr[j] of 
        'temp': begin
           yaxis = taxis
           yax = tax
        end
        'grav': begin
           yaxis = gaxis
           yax = gax
        end
        'abun': begin
           yaxis = zaxis
           yax = zax
        end
        'turb': begin
           yaxis = xiaxis
           yax = xiax
        end
     endcase
     
     
     if n_elements(xax) eq 1 then begin
        result = interpolate(csq_slice,yax,/grid,/missing,cubic=-.5)
        m = min(result,place)
        
        res[resloc[0,0],resloc[1,0],z]=xax[0]
        res[resloc[0,1],resloc[1,1],z]=yaxis[place]
     endif else if n_elements(yax) eq 1 then begin
        result = interpolate(csq_slice,xax,/grid,/missing,cubic=-.5)
        m = min(result,place)
        
        res[resloc[0,0],resloc[1,0],z]=xax[place]
        res[resloc[0,1],resloc[1,1],z]=yax[0]
     endif else begin   
        if (where(finite(csq_slice) eq 0))[0] ne -1 then csq_slice[where(finite(csq_slice) eq 0)] = max(csq_slice)
        result = interpolate(csq_slice,xax,yax,/grid,/missing,cubic=-.5)
        xv = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
        if (xv)[0] ne -1 then begin
           
           yv = (where(result[xv,*] eq min(result,/nan)))[0]
           xval = xaxis[xv]
           yval = yaxis[yv]
           
           if 1-max(mm(xaxis) eq xval) and   1-max(mm(yaxis) eq yval) then begin
              res[resloc[0,0],resloc[1,0]] = xval
              res[resloc[0,1],resloc[1,1]] = yval
           endif else begin
              res[resloc[0,0],resloc[1,0]] = sqrt(-1)
              res[resloc[0,1],resloc[1,1]] = sqrt(-1)
           endelse
           allres[resloc[0,0],resloc[1,0]] = xval
           allres[resloc[0,1],resloc[1,1]] = yval

        endif else begin 
           res[resloc[0,0],resloc[1,0]] = sqrt(-1)
           res[resloc[0,1],resloc[1,1]] = sqrt(-1)
        endelse
     endelse
     
     if 1 then begin
        print,'      ',xarr[j],'       ',yarr[j]
        print,xval,yval
        !p.multi=[0,1,1]
        self->startplot,1
        self->color_manage,'bw'       
        contour,result,xaxis,yaxis,nlevel=255,/fill,xrange=mm(xaxis),yrange=mm(yaxis),xtitle=xarr[j],ytitle=yarr[j],/xs,/ys,xticks=xticks
        contour,result,xaxis,yaxis,nlevel=5,/overplot,color=230
        
        ;;display,result,xtitle=xarr[j],ytitle=yarr[j],/aspect
        self->color_manage,'color'
        oplot,[xval],[yval],color=(*self.colors).blue,psym=8
        vline,xval,color=(*self.colors).blue
        hline,yval,color=(*self.colors).blue
        sharpcorners,thick=!x.thick
        
        self->endplot,1
     endif
     wait,0.5
     
  endfor
  
  if (where(finite(res[*,3])))[0] eq -1 then use = allres else use = res
  x=(use[*,3])[where(finite(use[*,3]))] 
  if n_elements(x) gt 1 then stdx = stddev(x) else stdx = 0  
  print,'x',average(x),stdx,max([stdx,self.dx*self.div/5d0])
  print,x
  
  if (where(finite(res[*,0])))[0] eq -1 then use = allres else use = res
  t=(use[*,0])[where(finite(use[*,0]))] 
  if n_elements(t) gt 1 then stdt = stddev(t) else stdt = 0
  print,'t',average(t),stdt,max([stdt,self.dt*self.div/5d0])
  print,t
   
  if (where(finite(res[*,1])))[0] eq -1 then use = allres else use = res   
  g=(use[*,1])[where(finite(use[*,1]))] 
  if n_elements(g) gt 1 then stdg = stddev(g) else stdg = 0
  print,'g',average(g),stdg,max([stdg,self.dg*self.div/5d0])
  print,g
  
  if (where(finite(res[*,2])))[0] eq -1 then use = allres else use = res
  z=(use[*,2])[where(finite(use[*,2]))] 
  if n_elements(z) gt 1 then stdz = stddev(z) else stdz = 0
  print,'z',average(z),stdz,max([stdz,self.dz*self.div/5d0])
  print,z
 
  params = [average(t),$
            average(g),$
            average(z),$
            average(x)]
  sigs = [max([stdt,self.dt*self.div/5d0]),$
          max([stdg,self.dg*self.div/5d0]),$
          max([stdz,self.dz*self.div/5d0]),$
          max([stdx,self.dx*self.div/5d0])]
  
  print,params,format='(i6,d+7.2,d+7.2,d5.1)'
  print,sigs,format='(i6,d7.2,d7.2,d5.1)' 
  
  string1 =  extra+' & '+string(params[0],format='(i5)')+' $\pm$ '+string(sigs[0],format='(i4)')+$
             ' & '+string(params[1],format='(d+5.2)')+' $\pm$ '+string(sigs[1],format='(d5.2)')+$
             ' & '+string(params[2],format='(d+5.2)')+' $\pm$ '+string(sigs[2],format='(d5.2)')+$
             ' & '+string(params[3],format='(d5.2)')+' $\pm$ '+string(sigs[3],format='(d4.2)')+$
             ' & '+string(min(csq,/nan),format='(d30.2)')+' & '+$
             string(min(objspec.overallres),format='(i20)')+' & Bi-cubic \\'
  
  
  if n_elements(dothis) eq 4 then $
     string2 = '  '+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+$
               '  '+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+$
               '  '+string((*self.models).grid.params.abuns[dothis[2]],format='(d5.2)')+$
               '  '+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')+$ 
               '  '+string(min(csq,/nan),format='(d20.2)') else $
                  string2 = '  '+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+$
                            '  '+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+$
                            '  '+string((*self.models).grid.params.abuns[dothis[2]],format='(d5.2)')+$
                            '  '+string((*self.models).grid.params.turbs[0],format='(d5.2)')+$ 
                            '  '+string(min(csq,/nan),format='(d20.2)')
  
  
  string3 += '     '+string(params[0],format='(i5)')   +'  '+string(sigs[0],format='(i4)')+$
             '     '+string(params[1],format='(d+5.2)')+'  '+string(sigs[1],format='(d5.2)')+$
             '     '+string(params[2],format='(d+5.2)')+'  '+string(sigs[2],format='(d5.2)')+$
             '     '+string(params[3],format='(d5.2)') +'  '+string(sigs[3],format='(d4.2)')
  

  print,string2
  print,string3
  
end


pro specfit::custom_analysis_mask_old,chisq_arr,good=good,string1=string1,string2=string2,$
                                  string3=string3,plot=plot,is=is,polyranges=polyranges,$
                                  params=params,sigs=sigs,noplot=noplot,dothis=dothis
;  self->message,message=' Analyzing custom index range for '+is
  
  if keyword_set(is) then extra = is else extra = ''
  if 1-keyword_set(string3) then string3=''

  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  
  ps = !p
  xs = !x
  ys = !y
  
  !x.margin=[10,3]
  !y.margin=[4,1]
  title = ''

  ;;dothis = (*self.active_object).bestloc
  if 1-keyword_set(dothis) then begin
     min = min(chisq_arr,x,/nan)
     dothis = array_indices((*self.active_object).chisq,x)
     dothis = self->find_bestloc(self.locsize)
  endif 
  if n_elements(dothis) eq 3 then dothis = [dothis,0]

;  STOP
  
  plot = 0
  if keyword_set(noplot) then plot=0

  garr = sqrt(-1)
  zarr = sqrt(-1)
  tarr = sqrt(-1)
  xarr = sqrt(-1)
  

                                ; self->message,message=' --performing 2d bicubic interpolation for [Z]...'
  aspect_ratio = 3
  xsize=7.5
  !p.multi=[0,3,1]
  file = dir+self.runtype+'_'+'z_p_'+extra+'_abun.eps'
  if self.makeplot then self->plot_open,file,xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           if n_elements(dothis) eq 4 then ichi = reform((chisq_arr)[*,dothis[1],*,dothis[3]]) else ichi = sqrt(-1)
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle='[Z]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           if n_elements(dothis) eq 4 then  ichi = reform((chisq_arr)[dothis[0],*,*,dothis[3]]) else ichi = sqrt(-1)
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           
           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L
           

           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform((chisq_arr)[dothis[0],dothis[1],*,*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           
           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L

           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     if 0 then begin
        print,ichi
        print,'xtitle ',xtitle
        print,'ytitle ',ytitle
        stop
     endif
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1  then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        ;;   if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then begin
         ;  stop
           ichi[flag] = max(ichi[noflag])
                                
        endif
; if flag[0] ne -1 then stop


        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result,/nan)))[0]

        if pval eq 0 then zarr = zaxis[zval] else zarr = [zarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if self.makeplot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if self.makeplot then     plot,[0],[0],title=title,background=255
        if self.makeplot then   sharpcorners,thick=!x.thick
     endelse
  endfor
  if self.makeplot then  self->plot_close



  

                                ; self->message,message=' --performing 2d bicubic interpolation for Teff...'
  if self.makeplot then file = dir+self.runtype+'_'+'z_p_'+extra+'_teff.eps'
  if self.makeplot then self->plot_open,file,xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           if n_elements(dothis) eq 4 then   ichi = reverse(rotate(reform((chisq_arr)[*,dothis[1],*,dothis[3]]),1)) else ichi = sqrt(-1)
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='T!deff!n [K]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           if n_elements(dothis) eq 4 then   ichi =  reverse(rotate(reform((chisq_arr)[*,*,dothis[2],dothis[3]]),1)) else ichi = sqrt(-1)
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))

           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L
           

           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform((chisq_arr)[*,dothis[1],dothis[2],*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
                                ;taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ; taxis = fillarr(self.dx,mm(((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]))

                                ; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))
           
           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L
           

           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then tarr = zaxis[zval] else tarr = [tarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        if self.makeplot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           ;;   stop

           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if self.makeplot then begin
           plot,[0],[0],title=title,background=255
           sharpcorners,thick=!x.thick
        endif
     endelse
  endfor
  if self.makeplot then self->plot_close
  
                                ; self->message,message=' --performing 2d bicubic interpolation for logg...'
  if self.makeplot then begin
     file = dir+self.runtype+'_'+'z_p_'+extra+'_logg.eps'
     self->plot_open,file,xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  endif
  for pval = 0,2 do begin
     case pval of
        0: begin
           if n_elements(dothis) eq 4 then         ichi = reverse(rotate(reform((chisq_arr)[dothis[0],*,*,dothis[3]]),1)) else ichi = sqrt(-1)
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           ;; zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))


           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L


           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='log g'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           if n_elements(dothis) eq 4 then   ichi =  reform((chisq_arr)[*,*,dothis[2],dothis[3]]) else ichi = sqrt(-1)
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           ;; zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L


           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform((chisq_arr)[dothis[0],*,dothis[2],*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
                                ;  zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
                                ;  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))

           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L

                                ; taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))
           
           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L

           

           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then garr = zaxis[zval] else garr = [garr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if self.makeplot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if self.makeplot then begin
           plot,[0],[0],title=title,background=255
           sharpcorners,thick=!x.thick
        endif
     endelse
  endfor  
  if self.makeplot then self->plot_close


  
                                ; self->message,message=' --performing
                                ; 2d bicubic interpolation for
                                ; microturb...'
  if self.makeplot then begin
     file=dir+self.runtype+'_'+'z_p_'+extra+'_turb.eps'
     self->plot_open,file,xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  endif
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reform((chisq_arr)[dothis[0],dothis[1],*,*])
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ;zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))
           
           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L


           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='Microturbulence [km/s]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi =  reform((chisq_arr)[*,dothis[1],dothis[2],*])
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ; zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L


           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reform((chisq_arr)[dothis[0],*,dothis[2],*])
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
                                ;  zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))

           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L

           


           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))

           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L
           

           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then xarr = zaxis[zval] else xarr = [xarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        if self.makeplot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if self.makeplot then begin
           plot,[0],[0],title=title,background=255
           sharpcorners,thick=!x.thick
        endif
     endelse
  endfor
  if self.makeplot then self->plot_close
  

  
                                ;print,tarr,garr,zarr,xarr

  if n_elements(tarr) eq 1 then tarr = [tarr,tarr]
  if n_elements(garr) eq 1 then garr = [garr,garr]
  if n_elements(zarr) eq 1 then zarr = [zarr,zarr]
  if n_elements(xarr) eq 1 then xarr = [xarr,xarr]

  params = [average(tarr),average(garr),average(zarr[where(finite(zarr))]),average(xarr)]

  print,tarr
  print,garr
  print,zarr
  print,xarr
  stop

;;  stop

  sigs =  [stddev(tarr),stddev(garr),stddev(zarr[where(finite(zarr))]),stddev(xarr)]
  if (where(finite(params) eq 0))[0] ne -1 then params[where(finite(params) eq 0)] = self.lock_values[where(finite(params) eq 0)]
  
  if self.lock_param[0] then tarr = [self.lock_values[0],self.lock_values[0]]
  if self.lock_param[1] then garr = [self.lock_values[1],self.lock_values[1]]
  if self.lock_param[2] then zarr = [self.lock_values[2],self.lock_values[2]]
  if self.lock_param[3] then xarr = [self.lock_values[3],self.lock_values[3]]

                                ; print,params
  
  
;;  stop

  if n_elements(tarr) ne 0 and n_elements(garr) ne 0 and n_elements(zarr) ne 0 and n_elements(xarr) ne 0 then begin
     ;;print,tarr,garr,zarr,xarr
     ;;stop

     string1 =  extra+' & '+string(average(tarr),format='(i5)')+' $\pm$ '+string(stddev(tarr),format='(i4)')+$
                ' & '+string(average(garr),format='(d5.2)')+' $\pm$ '+string(stddev(garr),format='(d5.2)')+$
                ' & '+string(average(zarr[where(finite(zarr))]),format='(d5.2)')+' $\pm$ '+string(stddev(zarr[where(finite(zarr))]),format='(d5.2)')+$
                ' & '+string(average(xarr),format='(d5.2)')+' $\pm$ '+string(stddev(xarr),format='(d4.2)')+$
                ' & '+string(min(chisq_arr,/nan),format='(d30.2)')+' & '+$
                string(min(objspec.overallres),format='(i20)')+' & Bi-cubic \\'
     

     ;; print,min(objspec.chisq)
     ;; self->writetotex,string+' \\'
     ;; self->writetoascii,objspec.obj+'  '+string(average(tarr),format='(i5)')+' +/- '+string(stddev(tarr),format='(i4)')+'  '+string(average(garr),format='(d5.2)')+' +/- '+string(stddev(garr),format='(d5.2)')+'  '+string(average(zarr),format='(d5.2)')+' +/- '+string(stddev(zarr),format='(d5.2)')+'  '+string(average(xarr),format='(d5.2)')+' +/- '+string(stddev(xarr),format='(d4.2)')+'   '+string(min(objspec.chisq),format='(d12.2)')+'   '+string(min(objspec.overallres),format='(i7)')
     ;; stop
     
     if n_elements(dothis) eq 4 then $
        string2 = '  '+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+$
                  '  '+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+$
                  '  '+string((*self.models).grid.params.abuns[dothis[2]],format='(d5.2)')+$
                  '  '+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')+$ 
                  '  '+string(min(chisq_arr,/nan),format='(d5.2)') else $
                     string2 = '  '+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+$
                               '  '+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+$
                               '  '+string((*self.models).grid.params.abuns[dothis[2]],format='(d5.2)')+$
                               '  '+string((*self.models).grid.params.turbs[0],format='(d5.2)')+$ 
                               '  '+string(min(chisq_arr,/nan),format='(d5.2)')
     

     string3 += '     '+string(average(tarr),format='(i5)')  +'  '+string(stddev(tarr),format='(i4)')+$
                '     '+string(average(garr),format='(d5.2)')+'  '+string(stddev(garr),format='(d5.2)')+$
                '     '+string(average(zarr[where(finite(zarr))]),format='(d5.2)')+'  '+string(stddev(zarr[where(finite(zarr))]),format='(d5.2)')+$
                '     '+string(average(xarr),format='(d5.2)')+'  '+string(stddev(xarr),format='(d4.2)')

     ;; self->writetotex,string+' & Bi-cubic \\',/output_report,string2=string2

     
     ;; best overall model:
     *self.location = (*self.active_object).bestloc
     self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
     self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
     fluxbo = sflux
     fbot = self->returnactive((*self.active_object).bestloc)

     ;; best model for this range:
     *self.location = dothis
     self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
     self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
     fluxbh = sflux
     fbht = self->returnactive(dothis)
     
     ;; best interpolated model
     if self.interpolate eq 0 then begin
        best = [average(tarr),average(garr),average(zarr[where(finite(zarr))]),average(xarr)]
        ;;   best[where(finite(best) eq 0)] = self.lock_values[where(finite(best) eq 0)]
        
        self->lininterp,best,iwave,iflux
        good = where(iwave gt min(modwave) and iwave lt max(modwave) and finite(iflux))
        if good[0] ne -1 then begin
           iflux = iflux[good]
           iwave = iwave[good]
           spec_conv5aa,iwave,iflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
           self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
           self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
           fluxim = sflux
        endif
        fimt = string('T=',best[0],'   logg=',best[1],'     [Z]=',best[2],'   xit=', best[3],format='(a,i5,a,d5.2,a,d5.2,a,d5.2)')
     endif

     if self.makeplot then begin
        N_per_plot = 200
;;  self->plot_open,file,xsize=6,ysize=9,/inches,/encapsulated,/color
        index=0
        wr = swave[min([n_elements(swave)-1,n_per_plot])]-swave[0]
        yrange = mm(fluxbo)+[-.05,.15]*(max(fluxbo)-min(fluxbo))
        xt= 'Wavelength [um]'
        yt= 'Flux'
        tt = ''
        for i=0,(n_elements(swave)/n_per_plot),1 do begin
           if (i mod 5) eq 0 then begin
              if i ne 0 then self->plot_close
              ival =6
              index++
              file = dir+self.runtype+'_'+'z_p_'+extra+'_wspread'+string(index,format='(i1)')+'.eps'
              self->plot_open,file,xsize=6,ysize=9,/inches,/encapsulated,/color
              !p.charsize=1.5
              !p.multi=[0,1,5]
              !x.thick=4
              !y.thick=4
              !p.thick=4
              !p.charthick=1
              !x.margin=[12,3]
              *self.colors = specfit_colors()
              colors=specfit_colors()
           endif
           
           ival--
           indexrange = n_per_plot*[i,i+1]
           if indexrange[0] ne n_elements(swave) then $
              if indexrange[1] ge n_elements(swave) then plot,swave[indexrange[0]:n_elements(swave)-1],dflux[indexrange[0]:n_elements(swave)-1],$
              yrange=yrange,xrange=swave[indexrange[0]]+[0,wr],/xs,/ys,color=(*self.colors).black,psym=10,thick=5,xtitle=xt,ytitle=yt,title=tt else $
                 plot,swave[indexrange[0]:indexrange[1]],dflux[indexrange[0]:indexrange[1]],$
                      yrange=yrange,/xs,/ys,color=(*self.colors).black,psym=10,thick=5,xtitle=xt,ytitle=yt,title=tt
           
           for t=0,n_elements(polyranges)-1,2 do begin
              x = [max([!x.crange[0],polyranges[t]]),max([!x.crange[0],polyranges[t]]),min([!x.crange[1],polyranges[t+1]]),min([!x.crange[1],polyranges[t+1]])]
              y = [!y.crange[0],!y.crange[1],!y.crange[1],!y.crange[0]]
              if x[0] le !x.crange[1] and x[2] ge !x.crange[0] then begin
                 polyfill,x,y,color=(*self.colors).liteblue
                 !p.multi=[ival,1,5]
                 if indexrange[0] ne n_elements(swave) then $
                    if indexrange[1] ge n_elements(swave) then plot,swave[indexrange[0]:n_elements(swave)-1],dflux[indexrange[0]:n_elements(swave)-1],$
                    yrange=yrange,xrange=swave[indexrange[0]]+[0,wr],/xs,/ys,color=(*self.colors).black,psym=10,/nodata,xtitle=xt,ytitle=yt,title=tt else $
                       plot,swave[indexrange[0]:indexrange[1]],dflux[indexrange[0]:indexrange[1]],$
                            yrange=yrange,/xs,/ys,color=(*self.colors).black,psym=10,/nodata,xtitle=xt,ytitle=yt,title=tt
              endif
           endfor
           
           oplot,swave,dflux,psym=10,color=(*self.colors).black,thick=5
           
           oplot,swave,fluxbh,psym=10,color=(*self.colors).green
           if self.interpolate eq 0 then   if good[0] ne -1 then  oplot,swave,fluxim,psym=10,color=(*self.colors).blue
           oplot,swave,fluxbo,psym=10,color=(*self.colors).red

           ;;  vline,good_lambda,color=(*self.colors).green
           ;;  vline,good_lambda,color=(*self.colors).blue,linestyle=2
           ;;  vline,self.fitrange,color=(*self.colors).red,linestyle=1
           
           case (i mod 5) of
              0: begin
                 sfxyouts,fbot+' [best overall]',.9,.9,legend_perc=[0.05,(*self.colors).red],csize=1.0
                 sfxyouts,fbht+' [best subrange]',.8,.9,legend_perc=[0.05,(*self.colors).green],csize=1.0
              end
              1: begin
                 if self.interpolate eq 0 then  sfxyouts,fimt+' [subrange lin-int]',.9,.9,legend_perc=[0.05,(*self.colors).blue],csize=1.0
              end
              else: tt = ''
           endcase
           

           sharpcorners,thick=!x.thick,color=(*self.colors).black
           
        endfor

        self->plot_close
     endif

  endif
  ;; stop
  
end


function specfit::gstatistic,swave,sflux,dflux,derr,chispec=chispec
  lam2 = 1.1828
  wide = 0.0006
  good = where(finite(sflux) and (swave le lam2-wide or swave ge lam2+wide))
  
  Ck = total((sflux[good]*dflux[good])/dflux[good]^2)/$
       total((sflux[good]*sflux[good])/dflux[good]^2)
  
  Gk = total(((dflux[good]-ck*sflux[good])/dflux[good])^2)
  
  ;; print,'gk, chi: ',gk,total(((dflux[good]-sflux[good])/derr[good])^2)

  return,Gk
end

function specfit::gstatistic_trusterr,swave,sflux,dflux,derr,chispec=chispec,ck=ck,mc=mc
  lam2 = 1.1828
  wide = 0.0006
  good = where(finite(sflux) and (swave le lam2-wide or swave ge lam2+wide))
  bad = where(finite(sflux) eq 0 or (swave gt lam2-wide and swave lt lam2+wide))
  Ck = total((sflux[good]*dflux[good])/derr[good]^2)/$
       total((sflux[good]*sflux[good])/derr[good]^2)
  ;; ck = 1d0

  Gk = total(((dflux[good]-ck*sflux[good])/derr[good])^2)/n_elements(good)
  
  if 1-keyword_set(mc) then begin
     chispec = ((dflux-ck*sflux)/derr)^2
     chispec[bad] = sqrt(-1)
     temp = self->fullchi(swave,sflux,dflux,derr,chispec=chispec)
  endif
  
  
  ;; print,'gk, chi:
  ;; ',gk,total(((dflux[good]-sflux[good])/derr[good])^2) 
  ;;  if not self.lock_param[3] then if ((*self.models).grid.params.turbs)[(*self.location)[3]] ne self.turb_avoid then $
     return,Gk ;else return,1d10
end



function specfit::fullchi,swave,sflux,dflux,derr,chispec=chispec

  if 1-keyword_set(chispec) then csq  = (((dflux-sflux)/derr)^2) else csq = chispec
  if (*self.active_object).lockres then begin
                                ;  stop
     bigr = min([n_elements((*self.active_object).fullchisq[*,0,0,0,0])-1,n_elements(csq)-1])
     (*self.active_object).fullchisq[0:bigr,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = csq[0:bigr]
     (*self.active_object).fullchisqw[0:bigr,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = swave[0:bigr]
;     print,n_elements((*self.active_object).fullchisq[*,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]]),n_elements(csq)
     
  endif
  
  if self.nomg then begin   
     lam1 = 1.2083
     lam2 = 1.1828
     wide = 0.0006
     
     chibad1 = where(swave gt lam1-wide and swave lt lam1+wide) 
     chibad2 = where(swave gt lam2-wide and swave lt lam2+wide)
     chibad = [chibad1,chibad2]
     if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
     if chibad[0] ne -1 then csq[chibad] = sqrt(-1)
                                ;  stop
  endif

  if 0 then begin
     chibad = where(csq gt 50*median(csq))
     if n_elements(chibad) gt 0.05*n_elements(csq) then stop
     if chibad[0] ne -1 then csq[chibad] = sqrt(-1)
  endif
;;  stop
  
  good = where(finite(csq))
  
 ;; chispec=csq
 ;;; stop

  self.string = string(total(csq[where(finite(csq))])/n_elements(where(finite(csq))),mm(csq[where(finite(csq))]),median(csq[where(finite(csq))]),n_elements(where(finite(csq))),format='(d7.2,d9.3,d9.3,d9.3,i5)')
  
  if 0 then begin
     csq = csq[where(finite(csq))]
     cutval = (csq[reverse(sort(csq))])[0.05*n_elements(csq)]
     csq = csq[where(csq lt cutval)]
  endif
  
  if self.stopper then begin
     print,total(csq[where(finite(csq))])/n_elements(where(finite(csq)))
     print,'mm swave: ',mm(swave),mm(swave[good])
     print,'mm sflux: ',mm(sflux),mm(sflux[good])
     print,'mm dflux: ',mm(dflux),mm(dflux[good])
     print,'mm derr: ',mm(derr),mm(derr[good])

     stop
     
  endif
  
  
  if 1 then $                                ;; if this is a 1 then do Reduced Chi-Sq
     if (where(finite(csq)))[0] ne -1 then $ ; and ((*self.models).grid.params.turbs)[(*self.location)[3]] ne self.turb_avoid
        return,total(csq[good])/n_elements(good) else $
           return,1d10 else $
              if (where(finite(csq)))[0] ne -1 then $ ; and ((*self.models).grid.params.turbs)[(*self.location)[3]] ne self.turb_avoid
                 return,total(csq[where(finite(csq))]) else $
                    return,1d10
  
  
end


function specfit::fullchi_old,swave,sflux,dflux,derr,chispec=chispec

  csq  = (((dflux-sflux)/derr)^2)
  if (*self.active_object).lockres then begin
                                ;  stop
     bigr = min([n_elements((*self.active_object).fullchisq[*,0,0,0,0])-1,n_elements(csq)-1])
     (*self.active_object).fullchisq[0:bigr,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = csq[0:bigr]
     (*self.active_object).fullchisqw[0:bigr,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = swave[0:bigr]
;     print,n_elements((*self.active_object).fullchisq[*,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]]),n_elements(csq)
     
  endif
  
  if self.nomg then begin   
     lam1 = 1.2083
     lam2 = 1.1828
     wide = 0.0006
     
     chibad1 = where(swave gt lam1-wide and swave lt lam1+wide) 
     chibad2 = where(swave gt lam2-wide and swave lt lam2+wide)
     chibad = [chibad1,chibad2]
     if (where(chibad ne -1))[0] ne -1 then chibad = chibad[where(chibad ne -1)]
     if chibad[0] ne -1 then csq[chibad] = sqrt(-1)
                                ;  stop
  endif
  if 0 then begin
     chibad = where(csq gt 50*median(csq))
     if n_elements(chibad) gt 0.05*n_elements(csq) then stop
     if chibad[0] ne -1 then csq[chibad] = sqrt(-1)
  endif
;;  stop
  
  
  
  chispec=csq
 ;;; stop

  self.string = string(total(csq[where(finite(csq))])/n_elements(where(finite(csq))),mm(csq[where(finite(csq))]),median(csq[where(finite(csq))]),n_elements(where(finite(csq))),format='(d7.2,d9.3,d9.3,d9.3,i5)')
  
  if 1 then begin
     csq = csq[where(finite(csq))]
     cutval = (csq[reverse(sort(csq))])[0.1*n_elements(csq)]
     csq = csq[where(csq lt cutval)]
  endif

  if 1 then $                                ;; if this is a 1 then do Reduced Chi-Sq
     if (where(finite(csq)))[0] ne -1 then $ ; and ((*self.models).grid.params.turbs)[(*self.location)[3]] ne self.turb_avoid
        return,total(csq[where(finite(csq))])/n_elements(where(finite(csq))) else $
           return,1d10 else $
              if (where(finite(csq)))[0] ne -1 then $ ; and ((*self.models).grid.params.turbs)[(*self.location)[3]] ne self.turb_avoid
                 return,total(csq[where(finite(csq))]) else $
                    return,1d10
  
  
end

pro specfit::chisq_pixel
  width=3
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  if fail then return
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),$
               /quiet,/noinfo,lims=self.fitrange
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=(*self.active_object).overallres
  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=finalres
  csq = self->gstatistic_trusterr(swave,sflux,dflux,derr,chispec=chispec)
  (*self.active_object).chisq[self.inext] = csq 
  self.curr_chi = csq
  self->bestpar,/normpar,finalres=finalres
  ;; self->plot,swave=swave,sflux=sflux
  
  if width eq 1 then begin
     better = where(finite(chispec) and finite((*self.pix_fit)[4,*]) eq 0 or chispec lt (*self.pix_fit)[4,*])
     if better[0] ne -1 then begin
        (*self.pix_fit)[4,better] = chispec[better]
        (*self.pix_fit)[0,better] = (*self.models).grid.params.temps[(*self.location)[0]]
        (*self.pix_fit)[1,better] = (*self.models).grid.params.gravs[(*self.location)[1]]
        (*self.pix_fit)[2,better] = (*self.models).grid.params.abuns[(*self.location)[2]]
        (*self.pix_fit)[3,better] = (*self.models).grid.params.turbs[(*self.location)[3]]     
     endif 
  endif else begin
                                ; stop
     
     for i=0,n_elements((*self.pix_fit)[4,*])-1 do begin
        good = fillarr(1,i-(width-1)/2,i+(width-1)/2)
        good = good[where(good ge 0 and good le n_elements((*self.pix_fit)[4,*])-1)]
        if (where(finite(chispec[good])))[0] ne -1 then begin
           good = good[where(finite(chispec[good]))]
           if total(chispec[good]) lt (*self.pix_fit)[4,i] or finite((*self.pix_fit)[4,i]) eq 0 then $
              (*self.pix_fit)[*,i] = [(*self.models).grid.params.temps[(*self.location)[0]],$
                                      (*self.models).grid.params.gravs[(*self.location)[1]],$
                                      (*self.models).grid.params.abuns[(*self.location)[2]],$
                                      (*self.models).grid.params.turbs[(*self.location)[3]],$
                                      total(chispec[good])]
        endif
     endfor
  endelse
  
  self->startplot,0
  !p.multi=[0,2,2]
  ptr_free,self.colors
  self.colors = ptr_new(specfit_colors())
  
  pars = (*self.models).grid.params
  names = ['T','g','Z','x']
  for i=0,3 do begin
     plot,swave,(*self.pix_fit)[i,*],yrange=mm(pars.(i)),/xs,/ys,color=(*self.colors).black,psym=8,symsize=1,ytitle=names[i]
     oplot,swave,(*self.pix_fit)[i,*],color=(*self.colors).blue,psym=8,symsize=0.8
     sharpcorners,color=(*self.colors).black,thick=!x.thick 
  endfor
  self->endplot,0
  self->startplot,3
  
  ptr_free,self.colors
  self.colors = ptr_new(specfit_colors())
  !p.multi=[0,1,1]
  xrange=[self.fitrange[0],self.fitrange[1]]
  yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
  yrange=yrange+((yrange[1]-yrange[0])*[-.1,.05])
                                ; plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10
  plot,[0],[0],color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns'
  
  
  if self.nomg then begin
     lam1 = 1.2083
     lam2 = 1.1828
     wide = 0.0006

     y = [!y.crange[0],!y.crange[1],!y.crange[1],!y.crange[0]]
     x = [lam1+wide,lam1+wide,lam1-wide,lam1-wide]
     polyfill,x,y,color=(*self.colors).litegray
     x = [lam2+wide,lam2+wide,lam2-wide,lam2-wide]
     polyfill,x,y,color=(*self.colors).litegray
     
                                ;  vline,[lam1,lam2]+wide,color=(*self.colors).violet
                                ;  vline,[lam1,lam2]-wide,color=(*self.colors).violet
  endif
  
  plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,title=(*self.active_object).bestpar,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,/ys,xtitle='Microns',/noerase
                                ; oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).green,psym=10
  oplot,swave,sflux,psym=10,color=(*self.colors).red
  ;;vline,self.fitrange,color=(*self.colors).red
  
  
  abov = !y.crange[0]+0.05*(!y.crange[1]-!y.crange[0])
  for i=0,n_elements((*self.linelist).wave)-1 do xyouts,(*self.linelist)[i].wave,abov,(*self.linelist)[i].type,align=0.5,color=(*self.colors).black
  abov=0L
  
  
  self->endplot,3

  ;;stop
  
  (*self.active_object).fulldat[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),$
                                (*self.location)[0],(*self.location)[1], $
                                (*self.location)[2],(*self.location)[3]] = $
     dflux[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  (*self.active_object).fullmod[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),$
                                (*self.location)[0],(*self.location)[1], $
                                (*self.location)[2],(*self.location)[3]] = $
     sflux[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  (*self.active_object).fullwave[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),$
                                 (*self.location)[0],(*self.location)[1], $
                                 (*self.location)[2],(*self.location)[3]] = $
     swave[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  
  
end

pro specfit::chisq  
                                ; stop
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  if fail then return
  
  if (*self.active_object).lockres eq 0 then begin
     finalres = self->fitres(modwave,modflux,modres)
  endif else finalres = (*self.active_object).overallres
  
  ;Stop
  ;; downgrade model to proper resolution:
  if finalres eq modres then begin
     wave = modwave
     flux = modflux
  endif else spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/finalres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  
  if n_elements(wave) lt 10 then stop
  
  ;; find shift
  ;; sflux = interpol(flux,wave,(*self.active_object).spec[*,0]+shift)
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=finalres
  
                                ; if  (*self.active_object).modcount eq 0 and (*self.active_object).lockres eq 0 then begin
                                ;    self->linemask,sflux=sflux,swave=swave,finalres=finalres,dflux=dflux
  if (where(*(*self.active_object).linemask eq 0))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 0)]=sqrt(-1)
                                ; endif

  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave,res=finalres
  if (where(*(*self.active_object).linemask eq 2))[0] ne -1 then sflux[where(*(*self.active_object).linemask eq 2)]=sqrt(-1)
  
;  csq_o = self->fullchi(swave,sflux,dflux,derr)
  
  
  if (*self.active_object).lockres then begin
     if n_elements(sflux) eq n_elements((*self.active_object).mcgrid[*,0,0,0]) then $
        (*self.active_object).mcgrid[*,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = sflux
     if n_elements(sflux) lt n_elements((*self.active_object).mcgrid[*,0,0,0]) then $
        (*self.active_object).mcgrid[0:n_elements(sflux)-1,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = sflux
     if n_elements(sflux) gt n_elements((*self.active_object).mcgrid[*,0,0,0]) then $
        (*self.active_object).mcgrid[*,(*self.location)[0],(*self.location)[1],(*self.location)[2],(*self.location)[3]] = sflux[0:n_elements((*self.active_object).mcgrid[*,0,0,0])-1]
  endif
  
  csq = self->gstatistic_trusterr(swave,sflux,dflux,derr)
                                ; print,'csq, gstatistic: '+string(csq_o,csq,format='(d8.4,d8.4)')
  
  (*self.active_object).chisq[self.inext] = csq 
  
  self.curr_chi = csq
  self->bestpar,/normpar,finalres=finalres
  
  if self.stopper then begin
     print,self.string
     print,csq
     print,self.best_chi
     
     stop
  endif
  ;; stop
  
  ;; if finite((*self.active_object).chisq[self.inext]) then stop
  
  if self.curr_chi lt self.best_chi[0] then begin
     if (*self.models).grid.params.turbs[(*self.location)[3]] ne self.turb_avoid and $
        (*self.models).grid.params.gravs[(*self.location)[1]] ne self.grav_avoid then begin
        
        
        ptr_free,(*self.active_object).bestcont
        (*self.active_object).bestcont = ptr_new(*(*self.active_object).storecont)
        if (*self.active_object).lockres eq 0 then begin
;     stop

           self.baseres = finalres
                                ; if (*self.active_object).lockres then begin
           if self.do_master then begin
              self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,/nofix 
              self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
              self->fixwave,swave=swave,sflux=sflux,derr=derr,dflux=dflux,fixed=fixed
              (*self.active_object).bestfix = fixed
              
              ;;     if 0 then begin
              newfres = self->fitres(modwave,modflux,modres,delres=0.90)
                                ;print,finalres,newfres
              finalres = newfres
              spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/newfres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
              
              ;;     endif
              dfold = dflux
              self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
              self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
              
              
;;           swave2 = swave+fixed
              ;;         dflux2 = interpol(dflux,swave2,swave) 
              ;;       derr2 = interpol(derr,swave2,swave)
              
                                ; stop
              csq2 = self->fullchi(swave,sflux,dflux,derr)
              self->startplot,1
              plot,swave,dfold,xrange=[1.165,1.170],psym=10,title=csq/csq2,xticks=3
              oplot,swave,dflux,color=(*self.colors).blue,psym=10
              oplot,swave,sflux,color=(*self.colors).red,psym=10
              self->endplot,1
              
              ptr_free,self.master_dflux,self.master_swave,self.master_derr
              self.master_dflux = ptr_new(dflux)
              self.master_swave = ptr_new(swave)
              self.master_derr = ptr_new(derr)
              
           endif else csq2 = csq
        endif else csq2 = csq
        self.store_prevchi = self.best_chi
        self.best_chi = [self.curr_chi, self.inext, csq2]
        (*self.active_object).bestdat = dflux[0:min([n_elements((*self.active_object).bestmod)-1,n_elements(sflux)-1])]
        (*self.active_object).bestmod = sflux[0:min([n_elements((*self.active_object).bestmod)-1,n_elements(sflux)-1])]
        (*self.active_object).bestwave = swave[0:min([n_elements((*self.active_object).bestmod)-1,n_elements(sflux)-1])]
        ;;  (*self.active_object).bestfix = fixed
        
        zero = where( (*self.active_object).bestwave eq 0)
        if zero[0] ne -1 then (*self.active_object).bestwave[zero] = sqrt(-1)
                                ; stop
        self->bestpar,finalres=finalres
        self->plot
     endif 
  endif else self->plot,swave=swave,sflux=sflux
  
  (*self.active_object).fulldat[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),(*self.location)[0],(*self.location)[1], $
                                (*self.location)[2],(*self.location)[3]] = dflux[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  (*self.active_object).fullmod[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),(*self.location)[0],(*self.location)[1], $
                                (*self.location)[2],(*self.location)[3]] = sflux[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  (*self.active_object).fullwave[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1]),(*self.location)[0],(*self.location)[1], $
                                 (*self.location)[2],(*self.location)[3]] = swave[0:min([n_elements((*self.active_object).fullmod[*,0,0,0,0])-1,n_elements(sflux)-1])]
  
  
  ;; stop
  if 0 and (*self.active_object).lockres then begin
     print,csq
     self->printactive
     stop
  endif
  if csq gt 10 and (*self.active_object).lockres then begin
     ;; print,'high chisq...'
     ;; print,csq
     ;; self->printactive
     ;; stop
     
     ;; (*self.active_object).chisq[self.inext] = 1d10
     
  endif
  
end


pro specfit::runfit
  cont = 1     
  test_cont=0
  initfit = 0
  freerescount = min([150d0,n_elements((*self.active_object).chisq)])
  widget_control,self.progbarB,set_value=0
  
  while cont do begin
     perc = (*self.active_object).goodmodcount/n_elements((*self.active_object).chisq)
     if  (*self.active_object).lockres eq 0 then perc = (*self.active_object).goodmodcount/freerescount
     widget_control,self.progbarB,set_value=perc
     self->activeindex
     self->chisq
     if (*self.active_object).lockres then if ((*self.active_object).modcount mod 100) eq 0 then self->plot2d
     
     (*self.active_object).modcount++
     if (self.inext)[0] ne -1 then (*self.active_object).goodmodcount++  
     if (*self.active_object).modcount gt (size(self.initialfits))[2] and initfit eq 0 then begin
        self->message,message= " Initial fit, now smart..."
        test_cont = 1
        initfit = 1
     endif
     
     counter,(*self.active_object).goodmodcount,n_elements((*self.active_object).chisq) 
     ;;  if ((*self.active_object).modcount mod 100) eq 0 then print,self.curr_chi/self.best_chi[0], (*self.active_object).modcount
     
     if test_cont then begin
        ;; conditions for not continuing:   
        if (*self.active_object).goodmodcount ge n_elements((*self.active_object).chisq) then begin
           self->message,message=" Halting -- all models calculated."
           self->message,message=" Halting -- all models calculated.",which='A'
           cont=0
        endif
        if self.curr_chi gt 10*self.best_chi[0] and (*self.active_object).lockres eq 0 and 0 then begin
           self->message,message=" Halting -- minimum explored."
           cont=0
        endif
        if (*self.active_object).goodmodcount gt freerescount-1 and (*self.active_object).lockres eq 0 then begin
           self->message,message=" Halting-- 150 models.  Now lockres."
           self->message,message=" Halting-- 150 models.  Now lockres.",which='A'
           
           cont=0
        endif
     endif
     
  endwhile
  widget_control,self.progbarB,set_value=1
  
  self->message,message="      runfit loop complete. "+string((*self.active_object).goodmodcount,format='(i5)')+" models calculated."
end


pro specfit::aplots,narrow=narrow
  plot = 1
  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ps=!p
  xs=!x
  ys=!y
  
;  if (where((*self.active_object).chisq eq 1d10))[0] ne -1 then $
;     (*self.active_object).chisq[where((*self.active_object).chisq eq 1d10)] = sqrt(-1)
;  waverange = [1.16,1.206]
  

  chiarr = self->makechiarr(waverange)
  
  min = min(chiarr,x,/nan)
  ;dothis = array_indices(chiarr,x)
  
  ;print,dothis, (*self.active_object).bestloc
  dothis = self->find_bestloc(self.locsize)
  
  
  
; dothis = (*self.active_object).bestloc
  
  
  if n_elements(dothis) eq 3 then dothis = [dothis,0]
  
  dothispar = [(*self.models).grid.params.temps[dothis[0]],$
               (*self.models).grid.params.gravs[dothis[1]],$
               (*self.models).grid.params.abuns[dothis[2]],$
               (*self.models).grid.params.turbs[dothis[3]]]
  
  

  garr = sqrt(-1)
  zarr = sqrt(-1)
  tarr = sqrt(-1)
  xarr = sqrt(-1)
  
                                ; self->message,message=' --performing 2d bicubic interpolation for [Z]...'
  if plot then aspect_ratio = 1.5
  if plot then xsize=7
  if plot then !p.multi=[0,1,3]
  if keyword_set(narrow) then add = 'aplot_narrow' else add = 'aplot'
  if plot then self->plot_open,dir+add+'_'+self.runtype+'.eps',xsize=7,ysize=6.8,/inches,/encapsulated,/color

  self->color_manage,'color'


  !x.margin=[7.3,1.5]
  !y.margin=[3.1,1.5]
  
  ;; !p.multi = [0,1,4,3]
  
  place = where((*self.active_object).bestwave gt self.fitrange[0] and (*self.active_object).bestwave lt self.fitrange[1])
  remexp=1
  yrange=range_find([(*self.active_object).bestdat[place],(*self.active_object).bestmod[place]],0.05,remexp=remexp)
  
  plot,[0],[0],xrange=self.fitrange,yrange=yrange,/nodata,xtitle='Wavelength  ['+textoidl('\mu')+'m]',ytitle='Flux [x10!u'+strtrim(string(alog10(remexp),format='(i)'),2)+'!n]',/xs,/ys
  sharpcorners,thick=!x.thick
  oplot,(*self.active_object).bestwave,(*self.active_object).bestdat/remexp,color=(*self.colors).black,psym=10,thick=4
  oplot,(*self.active_object).bestwave,(*self.active_object).bestmod/remexp,color=(*self.colors).red,psym=10,thick=2
                                ; hline,0,color=(*self.colors).gray,linestyle=1
                                ; oplot,(*self.active_object).bestwave,(*self.active_object).bestdat-(*self.active_object).bestmod,color=(*self.colors).black,psym=8,symsize=.3
  sharpcorners,thick=!x.thick

  !p.multi = [6,3,3]
  for pval = 0,5 do begin
     case pval of
        0: begin
           ichi = reform(chiarr[*,dothis[1],*,dothis[3]])
           bestpt = [dothispar[0],dothispar[2]]
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle='Fe/H'
           parx = 0
           pary = 2
                                ; !x.margin=[5,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi = reform(chiarr[dothis[0],*,*,dothis[3]])
           
           bestpt = [dothispar[1],dothispar[2]]
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L

           
           parx = 1
           pary = 2
           
           xtitle='log g' 
           ytitle='Fe/H'

           ;; !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform(chiarr[dothis[0],dothis[1],*,*]),1))
           bestpt = [dothispar[3],dothispar[2]]

           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
                                ;  taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;  tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))

           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L


           parx = 3
           pary = 2

           xtitle='Microturbulence [km/s]'
           ytitle='Fe/H'

                                ; !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end 
        3: begin
           ichi = reform(chiarr[*,*,dothis[2],dothis[3]])
           bestpt = [dothispar[0],dothispar[1]]
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ;  zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
                                ;  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L

           
           
           ytitle='log g'
                                ;  ytitle=''
           
           parx = 0
           pary = 1

           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
                                ;ytitle='[Z]'
                                ; !x.margin=[5,3]
           ;;  help,ichi
           ;; stop 
           print,bestpt
           print,xtitle,ytitle
           
        end

        4: begin
           ichi = reverse(rotate(reform(chiarr[dothis[0],*,dothis[2],*]),1))
           bestpt = [dothispar[3],dothispar[1]]
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ;  zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
                                ;  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L

           parx = 1
           pary = 3

           
           
           xtitle='log g'
                                ;  ytitle=''
           


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L



           xtitle='Microturbulence [km/s]'
           ytitle='log g'
                                ;ytitle='[Z]'
           ;;  help,ichi
           ;; stop
           print,bestpt
           print,xtitle,ytitle
        end

        5: begin

           ichi = reverse(rotate(reform(chiarr[*,dothis[1],dothis[2],*]),1))
           bestpt = [dothispar[3],dothispar[0]] ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ;  zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
                                ;  zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           

           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L

           xtitle='Microturbulence [km/s]'


           parx = 3
           pary = 0

           
                                ;  ytitle=''
           

           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
           ytitle='T!deff!n [K]'
           


                                ; xtitle='Microturbulence [km/s]'
                                ; ytitle=''
                                ;ytitle='[Z]'
           ;;  help,ichi
           ;; stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        ;;   if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
                                ; if flag[0] ne -1 then stop


        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result,/nan)))[0]

        if pval eq 0 then zarr = zaxis[zval] else zarr = [zarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if plot then loadct,0,/silent
        xrange=mm(taxis)
        yrange=mm(zaxis)
        if keyword_set(narrow) then  begin 
           xrange = [taxis[tval]-5*self.mc_sigs[parx],taxis[tval]+5.0*self.mc_sigs[parx]]
           yrange = [zaxis[zval]-5*self.mc_sigs[pary],zaxis[zval]+5.0*self.mc_sigs[pary]]
           yrange[0] = max([min(zaxis),yrange[0]])
           xrange[0] = max([min(taxis),xrange[0]])
           yrange[1] = min([max(zaxis),yrange[1]])
           xrange[1] = min([max(taxis),xrange[1]])
                                ;   stop
        endif
        
        if plot then contour,result,taxis,zaxis,nlevel=500,/fill,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
;;        if plot then contour,result,taxis,zaxis,nlevel=12,/overplot,color=230


                                ;  stop
        ;; do the contours
        minchi = min(result,loc)
        ai = array_indices(result,loc)
        chivals = dblarr(3)
        for sigv=1,3 do begin
           chix = result[where(taxis le taxis[ai[0]]+sigv*self.mc_sigs[parx] and taxis ge taxis[ai[0]]-sigv*self.mc_sigs[parx]),ai[1]]
           chiy = reform(result[ai[0],where(zaxis le zaxis[ai[1]]+sigv*self.mc_sigs[pary] and zaxis ge zaxis[ai[1]]-sigv*self.mc_sigs[pary])])
           chivals[sigv-1] = min([max(chix),max(chiy)])
        endfor
        chivals = chivals[sort(chivals)]
        chivals = chivals[uniq(chivals)]
        
        if plot then contour,result,taxis,zaxis,levels=chivals,/overplot,color=230

        
                                ;indices = 
        

        

        if plot then ptr_free,self.colors
        if plot then self.colors = ptr_new(specfit_colors())
        
        if plot then vline,taxis[tval],color=(*self.colors).blue,thick=3
        if plot then hline,zaxis[zval],color=(*self.colors).blue,thick=3
        if plot then oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
        plotsym,5,1.2,/fill

        if bestpt[0] eq !x.crange[0] then bestpt[0]+=0.01*(!x.crange[1]-!x.crange[0])
        if bestpt[0] eq !x.crange[1] then bestpt[0]-=0.01*(!x.crange[1]-!x.crange[0])
        if bestpt[1] eq !y.crange[1] then bestpt[1]-=0.01*(!y.crange[1]-!y.crange[0])
        if bestpt[1] eq !y.crange[0] then bestpt[1]+=0.01*(!y.crange[1]-!y.crange[0])

        if plot then oplot,[bestpt[0]],[bestpt[1]],psym=8,color=(*self.colors).red,symsize=0.8
        plotsym,0,1.2,/fill

        if plot then loadct,0,/silent
        
        if plot then sharpcorners,thick=!x.thick
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if plot then  plot,[0],[0],title=title,background=255
        if plot then  sharpcorners,thick=!x.thick
     endelse
  endfor

  if 0 then begin
     !p.multi = [1,1,4]
                                ; plot,[0],[0],xrange=[1.16,1.208],yrange=[0,1.1],/nodata,xtitle='Wavelength [um]',ytitle='Norm. Flux'
     
     
     ;; !p.multi = [0,1,4,3]
     self->color_manage,'color'
     
     ;;  get the model at grav_param
     self->lininterp,self.grav_param,iwave,iflux ;,/makestop
     
                                ; stop
     
     good = where(iwave gt 1.14 and iwave lt 1.24 and finite(iflux))
     if good[0] ne -1 then begin
        iflux = iflux[good]
        iwave = iwave[good]
        spec_conv5aa,iwave,iflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/(*self.models).res)^2d0),/quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=pwave,sflux=pflux,derr=derr,dflux=dflux        
        pfluxc = pflux
;  perr = pflux/100 ;; S/N of 100
        ;; self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
        ;; self->contcorrect,flux=pfluxc,comp=dflux,cont=cont
        ;; fluxim = sflux
     endif else stop
     
     
     plot,[0],[0],xrange=[1.187,1.204],yrange=[-0.1,1.1],/nodata,xtitle='Wavelength  ['+textoidl('\mu')+'m]',ytitle='Norm. Flux',/xs,/ys
     ;; sharpcorners,thick=!x.thick
     oplot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,psym=10,thick=4
     oplot,pwave,pfluxc,color=(*self.colors).blue,psym=10,thick=2
     hline,0,color=(*self.colors).gray,linestyle=1
     oplot,(*self.active_object).bestwave,(*self.active_object).bestdat-pfluxc,color=(*self.colors).black,psym=8,symsize=.3
     
     sharpcorners,thick=!x.thick
  endif
  
  if plot then self->plot_close
  ;stop
  
                                ; stop



  ;stop
  
  
  !p=ps
  !x=xs
  !y=ys  
  
end

pro specfit::analysis


  heap = 0
  if heap then help,/heap
  
  plot = 0
  self->message,message=' Analyzing results...'
  objspec = (*self.active_object)
  dir = self.outdir+'/'+'output_'+objspec.obj+'/'
  ;;spawn,'mkdir '+self.outdir+'/'+dir
  
  if plot then begin
     ps = !p
     xs = !x
     ys = !y
     
     !p.multi=[0,1,1]
     
     aspect_ratio=1.5
     xsize=6
     self->plot_open,dir+'plot_specfit_full.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
     *self.colors = specfit_colors()
     !x.thick=4
     !y.thick=4
     !p.charsize=1
     !p.charthick=1
     !x.margin=[12,3]
     xrange=mm(objspec.spec[*,0])
     
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.01,.01])
                                ; plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
                                ;    xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=2
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
          xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=2
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).red,psym=10,thick=2
     vline,self.fitrange,color=(*self.colors).blue,thick=4
     sharpcorners,color=(*self.colors).black,thick=!x.thick
     self->plot_close
     
     self->plot_open,dir+'plot_specfit_fitrange.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
     *self.colors = specfit_colors()
     
     xrange=[self.fitrange[0]-.005,self.fitrange[1]+.005]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.01,.01])
;  plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
                                ;      xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=4
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
          xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=3
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).red,psym=10,thick=3
     vline,self.fitrange,color=(*self.colors).blue,thick=4
     sharpcorners,color=(*self.colors).black,thick=!x.thick  
     self->plot_close
     
     xsize = 9
     aspect_ratio=2.5
     
     self->plot_open,dir+'plot_specfit_fitrange_wide.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
     *self.colors = specfit_colors()

     xrange=[self.fitrange[0]-.005,self.fitrange[1]+.005]
     yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
     yrange=yrange+((yrange[1]-yrange[0])*[-.01,.01])
;  plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
                                ;      xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=4
     plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
          xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=3
     oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).red,psym=10,thick=3
     vline,self.fitrange,color=(*self.colors).blue,thick=4
     sharpcorners,color=(*self.colors).black,thick=!x.thick  
     self->plot_close

;  stop
     
     xsize=6
     aspect_ratio=1.5

     
                                ; !p.charsize=1.5
     xrange=[1.1458,1.1558]
     !p.multi=[0,1,1]
     for i=0,4 do begin
        self->plot_open,dir+'plot_specfit_spreadbig_'+string(i,format='(i1)')+'.eps',xsize=xsize,ysize=(xsize/aspect_ratio),/inches,/encapsulated,/color
        *self.colors = specfit_colors()
        
        xrange+=0.01
        yrange=mm((*self.active_object).spec[where((*self.active_object).spec[*,0] ge xrange[0] and (*self.active_object).spec[*,0] le xrange[1]),1])
        yrange=yrange+((yrange[1]-yrange[0])*[-.01,.01])
        plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
             xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=2
        

                                ;plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
                                ;      xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=4
        oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).red,psym=10,thick=4
        vline,self.fitrange,color=(*self.colors).blue,thick=4
        sharpcorners,color=(*self.colors).black,thick=!x.thick  
        self->plot_close
     endfor
     
                                ; npoints = 500*objspec.overallres
                                ; stop
     !p.charsize=1.5
     self->plot_open,dir+'plot_specfit_spread.eps',xsize=6,ysize=9,/inches,/encapsulated,/color
     *self.colors = specfit_colors()

     xrange=[1.13,1.14]
     !p.multi=[0,1,5]
     for i=0,9 do begin
        xrange+=0.01
        if (where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1]))[0] ne -1 then begin
           yrange=mm((*self.active_object).bestdat[where((*self.active_object).bestwave ge xrange[0] and (*self.active_object).bestwave le xrange[1])])
           yrange=yrange+((yrange[1]-yrange[0])*[-.01,.01])
           plot,(*self.active_object).bestwave,(*self.active_object).bestdat,color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,thick=3,$
                xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]'     
           
           
                                ; plot,(*self.active_object).spec[*,0],(*self.active_object).spec[*,1],color=(*self.colors).black,background=(*self.colors).white,xrange=xrange,yrange=yrange,/xs,psym=10,$
                                ;        xtitle='Wavelength ['+textoidl('\mu')+'m]',ytitle='Flux [erg/s/cm!u2!n/A]',thick=4
           oplot,(*self.active_object).bestwave,(*self.active_object).bestmod,color=(*self.colors).red,psym=10,thick=3
           vline,self.fitrange,color=(*self.colors).blue,thick=4
           sharpcorners,color=(*self.colors).black,thick=!x.thick 

           if i eq 4 then begin 
              self->plot_close
              self->plot_open,dir+'plot_specfit_spread2.eps',xsize=6,ysize=9,/inches,/encapsulated,/color
              *self.colors = specfit_colors()
           endif
        endif
     endfor
     self->plot_close
     
     !p = ps
     !x = xs
     !y = ys

     
     !x.margin=[10,3]
     !y.margin=[4,1]
     title = ''
  endif


  dothis = (*self.active_object).bestloc
  print,dothis
  dothis =  self->find_bestloc(self.locsize)
  print,dothis
  if n_elements(dothis) eq 3 then dothis = [dothis,0]

  

  
  if self.diag then print,'find a way to zero out the bad models..'
  if self.diag then print,mm((*self.active_object).chisq)
  if (where((*self.active_object).chisq eq 1d10))[0] ne -1 then $
     (*self.active_object).chisq[where((*self.active_object).chisq eq 1d10)] = sqrt(-1)
  if self.diag then  print,mm((*self.active_object).chisq)



  ;stop

  if 0 then begin
     print,mm((*self.active_object).chisq)
     spread = stddev((*self.active_object).chisq,/nan)
     med = median((*self.active_object).chisq)
     if (where((*self.active_object).chisq gt med+5*spread))[0] ne -1 then $
        (*self.active_object).chisq[where((*self.active_object).chisq gt med+5*spread)] = sqrt(-1)
     print,mm((*self.active_object).chisq)
  endif 
;  stop
  
  garr = sqrt(-1)
  zarr = sqrt(-1)
  tarr = sqrt(-1)
  xarr = sqrt(-1)
                                ; self->message,message=' --performing 2d bicubic interpolation for [Z]...'
  if plot then  aspect_ratio = 3
  if plot then xsize=7.5
  if plot then !p.multi=[0,3,1]
  if plot then self->plot_open,dir+'plot_2d_abun.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reform(((*self.active_object).chisq)[*,dothis[1],*,dothis[3]])
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle='[Z]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi = reform(((*self.active_object).chisq)[dothis[0],*,*,dothis[3]])
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))


           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))

           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L

           
           
           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[dothis[0],dothis[1],*,*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           zaxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(zaxis))
                                ;  taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;  tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))

           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L



           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        ;;   if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then begin
     ;      stop
           
           ichi[flag] = max(ichi[noflag])        
        endif
                                ; if flag[0] ne -1 then stop


        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result,/nan)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result,/nan)))[0]

        if pval eq 0 then zarr = zaxis[zval] else zarr = [zarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if plot then loadct,0,/silent
        if plot then contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
        if plot then contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
        
        if plot then ptr_free,self.colors
        if plot then self.colors = ptr_new(specfit_colors())
        if plot then vline,taxis[tval],color=(*self.colors).blue,thick=3
        if plot then hline,zaxis[zval],color=(*self.colors).blue,thick=3
        if plot then oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
        if plot then loadct,0,/silent
        
        if plot then sharpcorners,thick=!x.thick
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if plot then  plot,[0],[0],title=title,background=255
        if plot then  sharpcorners,thick=!x.thick
     endelse
  endfor
  if plot then self->plot_close
  


  

                                ; self->message,message=' --performing 2d bicubic interpolation for Teff...'
  if plot then self->plot_open,dir+'plot_2d_teff.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[*,dothis[1],*,dothis[3]]),1))
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='T!deff!n [K]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi =  reverse(rotate(reform(((*self.active_object).chisq)[*,*,dothis[2],dothis[3]]),1))
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
                                ;     taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
                                ;     tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))


           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L

           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[*,dothis[1],dothis[2],*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           zaxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(zaxis))
                                ;taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L


           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then tarr = zaxis[zval] else tarr = [tarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if plot then  begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           ;;   stop

           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if plot then plot,[0],[0],title=title,background=255
        if plot then  sharpcorners,thick=!x.thick
     endelse
  endfor
  if plot then  self->plot_close
  
                                ; self->message,message=' --performing 2d bicubic interpolation for logg...'
  if plot then  self->plot_open,dir+'plot_2d_logg.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[dothis[0],*,*,dothis[3]]),1))
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           ;; zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))


           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L


           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='log g'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi =  reform(((*self.active_object).chisq)[*,*,dothis[2],dothis[3]])
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
           ;; zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))


           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L

           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reverse(rotate(reform(((*self.active_object).chisq)[dothis[0],*,dothis[2],*]),1))
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
           ;; zaxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(zaxis))


           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           zaxis = fillarr(self.dg,mm(loggv))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(loggv)-1)/n_elements(zaxis))
           loggv = 0L


                                ;   taxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;   tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(taxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           taxis = fillarr(self.dx,mm(turbval))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(turbval)-1)/n_elements(taxis))
           turbval = 0L


           xtitle='Microturbulence [km/s]'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then garr = zaxis[zval] else garr = [garr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if plot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           sharpcorners,thick=!x.thick
        endif

        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if plot then  plot,[0],[0],title=title,background=255
        if plot then  sharpcorners,thick=!x.thick
     endelse
  endfor
  if plot then self->plot_close
  
  
                                ; self->message,message=' --performing 2d bicubic interpolation for microturb...'
  if plot then  self->plot_open,dir+'plot_2d_turb.eps',xsize=xsize,ysize=xsize/aspect_ratio,/inches,/encapsulated,/color
  for pval = 0,2 do begin
     case pval of
        0: begin
           ichi = reform(((*self.active_object).chisq)[dothis[0],dothis[1],*,*])
           ;; title = 'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ; zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ; zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L

           
           taxis = fillarr(self.dz,mm((*self.models).grid.params.abuns))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.abuns)-1)/n_elements(taxis))
           xtitle='[Z]'
           ytitle='Microturbulence [km/s]'
           !x.margin=[8,3]
           ;;  help,ichi
           ;; stop
        end
        1: begin
           ichi =  reform(((*self.active_object).chisq)[*,dothis[1],dothis[2],*])
           ;; title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;         'turb='+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')
                                ;zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L

           
           taxis = fillarr(self.dt,mm((*self.models).grid.params.temps))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.temps)-1)/n_elements(taxis))
           xtitle='T!deff!n [K]'
           ytitle=''
           !x.margin=[5,3]
           ;; help,ichi
           ;; stop
        end
        2: begin
           ichi = reform(((*self.active_object).chisq)[dothis[0],*,dothis[2],*])
           ;;  title = 'teff='+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+' '+$
           ;;          'logg='+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')
                                ;   zaxis = fillarr(self.dx,mm((*self.models).grid.params.turbs))
                                ;   zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements((*self.models).grid.params.turbs)-1)/n_elements(zaxis))


           turbval = ((*self.models).grid.params.turbs)[where((*self.models).grid.params.turbs ne self.turb_avoid)]
           zaxis = fillarr(self.dx,mm(turbval))
           zax = 1d0*findgen(n_elements(zaxis))*(1d0*(n_elements(turbval)-1)/n_elements(zaxis))
           turbval = 0L

           
           ;; taxis = fillarr(self.dg,mm((*self.models).grid.params.gravs))
           ;; tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements((*self.models).grid.params.gravs)-1)/n_elements(taxis))
           
           loggv =  ((*self.models).grid.params.gravs)[where((*self.models).grid.params.gravs ne self.grav_avoid)]
           taxis = fillarr(self.dg,mm(loggv))
           tax = 1d0*findgen(n_elements(taxis))*(1d0*(n_elements(loggv)-1)/n_elements(taxis))
           loggv = 0L

           
           xtitle='log g'
           ytitle=''
           !x.margin=[5,3]
           ;;  help,ichi
           ;;  stop
        end
     endcase
     
     if (where(finite(ichi) eq 1))[0] ne -1 and n_elements(tax) gt 1 and n_elements(zax) gt 1 then begin
        flag = where(finite(ichi) eq 0)
        noflag = where(finite(ichi))
        if flag[0] ne -1 then ichi[flag] = max(ichi,/nan)*10
        if flag[0] ne -1 then ichi[flag] = max(ichi[noflag])
        
        result = interpolate(ichi,tax,zax,/grid,/missing,cubic=-.5)
                                ;   if (where(result ge 0))[0] ne -1 then result[where(result lt 0)] = max(result[where(result ge 0)])
        if flag[0] ne -1 then result[flag] = max(result[noflag])
        
        tval = ((where(result eq min(result)))[0] mod (size(result))[1])
        zval = (where(result[tval,*] eq min(result)))[0]

        if pval eq 0 then xarr = zaxis[zval] else xarr = [xarr,zaxis[zval]]
                                ; print,result[tval,zval]
                                ;print,taxis[tval],zaxis[zval]
        
        if plot then begin
           loadct,0,/silent
           contour,result,taxis,zaxis,nlevel=1000,/fill,xrange=mm(taxis),yrange=mm(zaxis),xtitle=xtitle,ytitle=ytitle,background=255,/xs,/ys
           contour,result,taxis,zaxis,nlevel=12,/overplot,color=230
           
           ptr_free,self.colors
           self.colors = ptr_new(specfit_colors())
           vline,taxis[tval],color=(*self.colors).blue,thick=3
           hline,zaxis[zval],color=(*self.colors).blue,thick=3
           oplot,[taxis[tval]],[zaxis[zval]],psym=8,color=(*self.colors).blue,symsize=0.8
           loadct,0,/silent
           
           sharpcorners,thick=!x.thick
        endif
        ;;      if flag[0] ne -1 and n_elements(noflag) gt 10 then stop
     endif else begin
        if plot then  plot,[0],[0],title=title,background=255
        if plot then  sharpcorners,thick=!x.thick
     endelse
  endfor
  if plot then   self->plot_close
  
  if 0 then begin

     print,'z'
     print,zarr
     print,average(zarr[where(finite(zarr))]),stddev(zarr[where(finite(zarr))])
     
     print,'T'
     print,tarr
     print,average(tarr),stddev(tarr)
     
     print,'g'
     print,garr
     print,average(garr),stddev(garr)
     
     print,'xi'
     print,xarr
     print,average(xarr),stddev(xarr)
     
  endif  

  if n_elements(tarr) eq 1 then tarr = [tarr,tarr]
  if n_elements(garr) eq 1 then garr = [garr,garr]
  if n_elements(zarr) eq 1 then zarr = [zarr,zarr]
  if n_elements(xarr) eq 1 then xarr = [xarr,xarr]


  string = objspec.obj+' & '+string(average(tarr),format='(i5)')+' $\pm$ '+string(stddev(tarr),format='(i4)')+' & '+string(average(garr),format='(d5.2)')+' $\pm$ '+string(stddev(garr),format='(d5.2)')+' & '+string(average(zarr[where(finite(zarr))]),format='(d5.2)')+' $\pm$ '+string(stddev(zarr[where(finite(zarr))]),format='(d5.2)')+' & '+string(average(xarr),format='(d5.2)')+' $\pm$ '+string(stddev(xarr),format='(d4.2)')+' & '+string(min(objspec.chisq),format='(d30.2)')+' & '+string(min(objspec.overallres),format='(i20)')
                                ; print,min(objspec.chisq)
  self->writetotex,string+' \\'
  self->writetoascii,objspec.obj+'  '+string(average(tarr),format='(i5)')+' +/- '+string(stddev(tarr),format='(i4)')+'  '+string(average(garr),format='(d5.2)')+' +/- '+string(stddev(garr),format='(d5.2)')+'  '+string(average(zarr[where(finite(zarr))]),format='(d5.2)')+' +/- '+string(stddev(zarr[where(finite(zarr))]),format='(d5.2)')+'  '+string(average(xarr),format='(d5.2)')+' +/- '+string(stddev(xarr),format='(d4.2)')+'   '+string(min(objspec.chisq),format='(d12.2)')+'   '+string(min(objspec.overallres),format='(i7)')+'  '+self.runtype,0
                                ; stop
  
  string2 = ' & '+string((*self.models).grid.params.temps[dothis[0]],format='(i5)')+$
            ' & '+string((*self.models).grid.params.gravs[dothis[1]],format='(d5.2)')+$
            ' & '+string((*self.models).grid.params.abuns[dothis[2]],format='(d5.2)')+$
            ' & '+string((*self.models).grid.params.turbs[dothis[3]],format='(d5.2)')+$ 
            ' & '+string(min(objspec.chisq),format='(d30.2)')+$
            ' & '+string(min(objspec.overallres),format='(i7)')+' & best model \\'
  
  self->writetotex,string+' & Bi-cubic \\',/output_report,string2=string2
  

  if heap then help,/heap
  if heap then stop
end

pro specfit::generalplotsetup
  !x.margin = self.xmargin
  !y.margin = self.ymargin
  !p.multi = [0,1,1]
  !p.font = 0

  self->color_manage,'color'
end


pro specfit::control_run

  if self.run_diag then begin
     self.runtype='run_diag'
     self->run
  endif

  if self.run_full then begin
     self.runtype='run_full'
     self->run
  endif

  if self.run_diag_focus then begin
     self.runtype='run_diag_focus'
     self->run_diag_focus
  endif

  if self.run_pixel then begin
     self.runtype='run_pixel'
     self->run_pixel
  endif

  if self.made_active then ptr_free,(*self.active_object).linemask,$
                                    (*self.active_object).storecont,$
                                    (*self.active_object).bestcont,$
                                    (*self.active_object).header
  
end


function specfit::meas_depthwidth,dwgrid 
  params =  (*self.models).grid.params
  
  a=n_elements(params.temps)
  b=n_elements(params.gravs)
  c=n_elements(params.abuns)
  d=n_elements(params.turbs)
  
  parinfo = replicate({fixed: 0,$
                       limited: dblarr(2),$
                       limits: dblarr(2)},4)
  ;;parinfo[3].fixed = 1
  parinfo[3].fixed = 1
  parinfo[2].limited=[1,0]
  parinfo[2].limits=[0,0]
  
  
  dwdat = {linelist: *self.linelist,$
           depths:    dblarr(n_elements(*self.linelist),a,b,c,d),$
           widths:    dblarr(n_elements(*self.linelist),a,b,c,d)}
  
  place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+self.init_shift)
  swave = ((*self.active_object).spec[place,0])-self.init_shift
  dflux = ((*self.active_object).spec[place,1])
  
  if 0 then begin
     med = median(dflux)
     std = stddev(dflux)
     temp = dflux
     temp[where(temp gt med+std*0.8 or temp lt med-std*0.8)]=0d0
     nreg = round(n_elements(dflux)*0.20)
     contregion = where(temp gt (reverse(temp[sort(temp)]))[nreg])
     contregion = [0,1,2,3,4,5,contregion,n_elements(contregion)-5,n_elements(contregion)-4,n_elements(contregion)-3,n_elements(contregion)-2,n_elements(contregion)-1]
     contregion = (contregion[sort(contregion)])[uniq(contregion[sort(contregion)])]
     contregion = contregion[where(finite(dflux[contregion]))]
     fit = robust_poly_fit(swave[contregion],dflux[contregion],3)
  endif

  total = n_elements(dwgrid.depths)*1d0
  count = 0d0
  
  plotnext = 0
  plotit = 0
  
  for i=0,a-1,1 do $
     for j=0,b-1,1 do $
        for k=0,c-1,1 do $ 
           for l=0,d-1,1 do begin
     ;;    stop


     flux = interpol(dwgrid.flux[*,i,j,k,l],dwgrid.wave,swave)
     nreg = round(n_elements(flux)*0.20)
     
     contregion = where(flux gt (reverse(flux[sort(flux)]))[nreg])
     contregion = [0,1,2,3,4,5,contregion,n_elements(contregion)-5,n_elements(contregion)-4,n_elements(contregion)-3,n_elements(contregion)-2,n_elements(contregion)-1]
     contregion = (contregion[sort(contregion)])[uniq(contregion[sort(contregion)])]
     contregion = contregion[where(finite(flux[contregion]))]
     fit = robust_poly_fit(swave[contregion],dflux[contregion]/flux[contregion],3)
     
                                ; contregion = dwmod.contregion[*,i,j,k,l]
                                ; fit = robust_poly_fit(swave[contregion],dflux[contregion],2)
     
     ;;contregion = 
     dfluxuse = dflux/poly(swave,fit)
     widget_control,self.progbarB,set_value=count/total
     
     bigwave = swave
     bigflux = dfluxuse

     if plotnext then begin
        plotit=1
        plotnext=0
        self->startplot,0
        !p.multi=[0,n_elements(*self.linelist),1]
     endif
     
     ;;dwdat = dwgrid.depths *0d0  
     for m=0,n_elements(*self.linelist)-1 do begin
        if (count mod 1000) eq 0 then counter,count,total,/timeleft,starttime=starttime
        
        count++
        if (count mod 100) eq 0 then plotnext = 1

        dl = (*self.linelist)[m].wave/dwgrid.res
        lr = [(*self.linelist)[m].wave-dl/1.5d0,(*self.linelist)[m].wave+dl/1.5d0]
        range = [(*self.linelist)[m].wave-3*dl,(*self.linelist)[m].wave+3*dl]
        wave=bigwave[where(bigwave le range[1] and bigwave ge range[0])]
        flux=bigflux[where(bigwave le range[1] and bigwave ge range[0])]
        
        depth =  min(flux[where(wave ge lr[0] and wave le lr[1])])
        core = wave[(where(flux eq depth))[0]]
        
        linemeas = where(wave ge core-dl/1.4d0 and wave le core+dl/1.4d0)
                                ;  if n_elements(linemeas) lt 7 then begin
        linemeas = where(wave eq core)
        linemeas = [-3,-2,-1,0,1,2,3]+linemeas[0]
                                ;  endif
        
                                ;  print,n_elements(linemeas)
        if n_elements(linemeas) lt 5 then stop
                                ; stop
        
        params = mpfit('zg_gaussfit',[1d0-depth,(*self.linelist)[m].wave,dl/2,1d0],funct={lambda:wave[linemeas], flux:flux[linemeas]},parinfo=parinfo,/quiet)
                                ;  creg = where(wave le (*self.linelist)[m].wave-dl or wave ge (*self.linelist)[m].wave+dl)           
        
        dwdat.depths[m,i,j,k,l] = depth
        
                                ;stop
                                ;  dwdat.depths[m] = params[0]+1
        dwdat.widths[m,i,j,k,l] = params[2]
                                ;  dwgrid.cont[*,m,i,j,k,l] = robust_poly_fit(wave[creg],flux[creg],1)
        if plotit then begin
           
           ;;if (count mod 100) eq 0 then begin
           plot,wave,flux,yrange=[0,1.005],/ys,psym=10,/xs,xticks=2,xrange=[(*self.linelist)[m].wave-0.0005,(*self.linelist)[m].wave+0.0005],title=dwgrid.res ;,/xs,psym=10
           oplot,wave[linemeas],flux[linemeas],color=(*self.colors).blue,psym=8,symsize=0.8
           oplot,wave,params[3]+ (params[0]*exp(-1*(wave-params[1])^2/(2*params[2]^2))),color=(*self.colors).blue,thick=2
                                ; oplot,wave[creg],flux[creg],psym=8,symsize=0.8,color=(*self.colors).red
                                ; oplot,wave,poly(wave,dwgrid.cont[*,m,i,j,k,l]),color=(*self.colors).red,thick=2
           oplot,lr,[depth,depth],color=(*self.colors).green
           hline,(*self.datdepths)[m],color=(*self.colors).red
           sharpcorners,thick=!x.thick
        endif
        
        
                                ; counter,count,total,/timeleft,starttime=starttime
        ;; endif          
        ;;stop
                                ;    wait,1
     endfor

     if plotit then begin
        self->endplot,0
        plotit = 0
        !p.multi=[0,1,1]
     endif
     
  endfor
  
  
  
  
                                ;stop
  ptr_free,self.datdepths
  self.datdepths = ptr_new(dwdat.depths[*,0,0,0,0])
  return,dwdat
end

function specfit::get_depthwidth,file,res
  params =  (*self.models).grid.params
  a=n_elements(params.temps)
  b=n_elements(params.gravs)
  c=n_elements(params.abuns)
  d=n_elements(params.turbs)
  
  

  if file_test(file) then restore,file else begin
     self->message,message='.... creating... ',which='B'
     
                                ; stop
                                ;  stop
     
     mf = (*self.models).flux[*,0,0,0,0]
     mw = (*self.models).wave
     mr = (*self.models).res
     fwhm = sqrt((1.1/res)^2-(1.1/mr)^2)
     
     spec_conv5aa,mw,mf,wave,flux,fwhm=fwhm,/quiet,/noinfo,lims=self.fitrange
     
     dwgrid = {res:       res,$
               params:    params,$
               linelist: *self.linelist,$
               depths:    dblarr(n_elements(*self.linelist),a,b,c,d),$
               widths:    dblarr(n_elements(*self.linelist),a,b,c,d),$
               wave: wave,$
               flux: dblarr(n_elements(flux),a,b,c,d)}
;;               contregion: dblarr(0.2*n_elements(flux)+12,a,b,c,d)}
     
     
     total = n_elements(dwgrid.depths)*1d0
     count = 0d0
     modtot = n_elements(dwgrid.depths[0,*,*,*,*])
     
     parinfo = replicate({fixed: 0,$
                          limited: dblarr(2),$
                          limits: dblarr(2)},4)
     parinfo[3].fixed = 1
     parinfo[2].limited=[1,0]
     parinfo[2].limits=[0,0]
     plotnext = 0
     plotit = 0
     starttime = systime(/sec)
     for i=0,a-1,1 do $
        for j=0,b-1,1 do $
           for k=0,c-1,1 do $ 
              for l=0,d-1,1 do begin
        if (count mod 100) eq 0 then counter,count,modtot,/timeleft,starttime=starttime
        mf = (*self.models).flux[*,i,j,k,l]
        spec_conv5aa,mw,mf,wave,flux,fwhm=fwhm,/quiet,/noinfo,lims=self.fitrange
        
        ;;stop
        
        nreg = round(n_elements(flux)*0.20)
        contregion = where(flux gt (reverse(flux[sort(flux)]))[nreg])
        contregion = [0,1,2,3,4,5,contregion,n_elements(contregion)-5,n_elements(contregion)-4,n_elements(contregion)-3,n_elements(contregion)-2,n_elements(contregion)-1]
        contregion = (contregion[sort(contregion)])[uniq(contregion[sort(contregion)])]
        contregion = contregion[where(finite(flux[contregion]))]
        fit = robust_poly_fit(wave[contregion],flux[contregion],3)
        ;;   dwgrid.contregion[*,i,j,k,l] = contregion
        
        dwgrid.flux[*,i,j,k,l] = flux/poly(wave,fit)
        count++
        
     endfor
     
  endelse
  
  total = n_elements(dwgrid.depths)*1d0
  count = 0d0
  
  
  
  plotnext = 0
  plotit = 0
  starttime = systime(/sec)
  for i=0,a-1,1 do $
     for j=0,b-1,1 do $
        for k=0,c-1,1 do $ 
           for l=0,d-1,1 do begin
     
     flux = dwgrid.flux[*,i,j,k,l]
     wave = dwgrid.wave
     
     if 0 then begin
        bigwave=wave
        bigflux=flux/poly(wave,fit)
     endif else begin
                                ; stop
        place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+self.init_shift)
        bigwave =  ((*self.active_object).spec[place,0])-self.init_shift
        bigflux = interpol(flux[where(wave ge self.fitrange[0] and wave le self.fitrange[1])],wave[where(wave ge self.fitrange[0] and wave le self.fitrange[1])],bigwave)
     endelse
     ;;stop
     
     if plotnext then begin
        plotit=1
        plotnext=0
        self->startplot,0
        !p.multi=[0,n_elements(*self.linelist),1]
     endif
     
     for m=0,n_elements(*self.linelist)-1 do begin
        if (count mod 1000) eq 0 then counter,count,total,/timeleft,starttime=starttime
        
        count++
        if (count mod 100) eq 0 then plotnext = 1
        
        
        if plotit eq 0 then widget_control,self.progbarB,set_value=count/total
        
        dl = (*self.linelist)[m].wave/res
        lr = [(*self.linelist)[m].wave-dl/1.5d0,(*self.linelist)[m].wave+dl/1.5d0]
        range = [(*self.linelist)[m].wave-3*dl,(*self.linelist)[m].wave+3*dl]
        wave=bigwave[where(bigwave le range[1] and bigwave ge range[0])]
        flux=bigflux[where(bigwave le range[1] and bigwave ge range[0])]
        
        
        
        depth =  min(flux[where(wave ge lr[0] and wave le lr[1])])
        core = wave[(where(flux eq depth))[0]]
        
        linemeas = where(wave ge core-dl/1.4d0 and wave le core+dl/1.4d0)
                                ; if n_elements(linemeas) lt 7 then begin
        linemeas = where(wave eq core)
        linemeas = [-3,-2,-1,0,1,2,3]+linemeas[0]
                                ; endif
        
                                ;  print,n_elements(linemeas)
;           stop
        
        params = mpfit('zg_gaussfit',[1d0-depth,(*self.linelist)[m].wave,dl/2,1d0],funct={lambda:wave[linemeas], flux:flux[linemeas]},parinfo=parinfo,/quiet)
                                ;  creg = where(wave le (*self.linelist)[m].wave-dl or wave ge (*self.linelist)[m].wave+dl)           
        
        dwgrid.depths[m,i,j,k,l] = depth
                                ;dwgrid.depths[m,i,j,k,l] = params[0]+1
        dwgrid.widths[m,i,j,k,l] = params[2]
                                ;  dwgrid.cont[*,m,i,j,k,l] = robust_poly_fit(wave[creg],flux[creg],1)
        
        if plotit then begin
           plot,wave,flux,/ys,/xs,title=(*self.linelist)[m].type+' '+string(m,format='(i3)'),psym=10,xticks=2,yrange=[0.0,1.005],$
                xrange=[(*self.linelist)[m].wave-0.0005,(*self.linelist)[m].wave+0.0005] ;,/ys,title=file,psym=10
           oplot,wave[linemeas],flux[linemeas],color=(*self.colors).blue,psym=8,symsize=0.8
           oplot,wave,params[3]+ (params[0]*exp(-1*(wave-params[1])^2/(2*params[2]^2))),color=(*self.colors).blue,thick=2
                                ; oplot,wave[creg],flux[creg],psym=8,symsize=0.8,color=(*self.colors).red
                                ; oplot,wave,poly(wave,dwgrid.cont[*,m,i,j,k,l]),color=(*self.colors).red,thick=2
           oplot,lr,[dwgrid.depths[m,i,j,k,l],dwgrid.depths[m,i,j,k,l]],color=(*self.colors).green
           hline,(*self.datdepths)[m],color=(*self.colors).red
           sharpcorners,thick=!x.thick
        endif
        
     endfor
     if plotit then begin
        self->endplot,0
        plotit = 0
     endif
     
  endfor
  
  save,dwgrid,filename=file
  !p.multi=[0,1,1]
  
  return,dwgrid
end

function specfit::analyze_dw,dwmod,dwdat,csq=csq
  params =  (*self.models).grid.params
  a=n_elements(params.temps)
  b=n_elements(params.gravs)
  c=n_elements(params.abuns)
  d=n_elements(params.turbs)
  
  csq = dwmod.depths*0
  
  for i=0,a-1,1 do $
     for j=0,b-1,1 do $
        for k=0,c-1,1 do $ 
           for l=0,d-1,1 do $
              for m=0,n_elements(*self.linelist)-1 do $
                 csq[m,i,j,k,l] = (dwdat.depths[m,i,j,k,l]-dwmod.depths[m,i,j,k,l])^2d0
  
  
  print,''
  csqtot = total(csq,1)
  minimum = min(csqtot,place,/nan)

  print,'MIN CSQ: '+string(minimum,format='(d)')

  b = array_indices(csqtot,place)
  print,params.temps[b[0]],params.gravs[b[1]],params.abuns[b[2]],params.turbs[b[3]]
  bwide = dwmod.widths[*,b[0],b[1],b[2],b[3]]
  dwide = dwdat.widths[*,b[0],b[1],b[2],b[3]]
  
  ptr_free,self.datdepths
  self.datdepths = ptr_new(dwdat.depths[*,b[0],b[1],b[2],b[3]])

  (*self.active_object).bestloc = b

  delta = sqrt(abs(bwide^2-dwide^2))
  good = where(bwide gt 0 and dwide gt 0 and finite(delta))
  
  
  print,bwide[good]
  print,dwide[good]
  print,delta[good]

  delta = delta[good]
  avg = median(delta)
  if average(bwide-dwdat.widths) gt 0 then avg*=-1 ;; this is when the data widths are WIDER than the 
  
                                ; average = average(delta[where(finite(delta))])
                                ; del_res = round(average/100d0)*100
  ;;round(newres/100d0)*100
  ;;print,newres
                                ; print,bwide-dwdat.widths
                                ; if average(bwide-dwdat.widths) lt 0 then del_res*=-1
                                ; print,del_res
                                ;stop

                                ; stop


                                ;stop
  return,avg
end

pro specfit::run_diag_focus

  save_linelist = (*self.linelist).wave
  save_linename = (*self.linelist).type

  linelist=save_linelist
  linename = save_linename

  linelist = [1.169,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
  linename = ['FeI','TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ]
  
                                ; linelist = [1.169,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
                                ; linename = ['FeI','TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ] 
  
  ptr_free,self.linelist
  self.linelist=ptr_new(replicate({type: '', wave: 0d0},n_elements(linelist)))
  for i=0,n_elements(linelist)-1 do begin
     (*self.linelist)[i].type = linename[i]
     (*self.linelist)[i].wave = linelist[i]
  endfor
  linelist =0L
  linename =0L

  self->message,message='****'+self.runtype+'****',which='A'
  
  for i=n_elements(*self.objects)-1d0,0,-1 do begin
     self->generalplotsetup        
     for j=0,3 do begin
        self->startplot,j
        plot,[0],[0],/nodata,xtitle='X axis',ytitle='Y axis'
        sharpcorners,thick=!x.thick
        self->endplot,j
     endfor
     
     self.active_num = i+1

     widget_control,self.progbarA,set_value=i/(n_elements(*self.objects))
     
     self->makeobject,(*self.objects)[i]
     self->message,message = 'Fit '+last_element(strsplit((*self.objects)[i],'/',/extract)),which='A'
     
;     stop
     rfres = 5000d0
     rfres = self.baseres       ;  self.baseres
     ptr_free,self.datdepths
     self.datdepths = ptr_new(dblarr(n_elements(*self.linelist)))

     ptr_free,self.location
     self.location = ptr_new([n_elements((*self.models).grid.params.temps)/2d0,$
                              n_elements((*self.models).grid.params.gravs)/2d0,$
                              n_elements((*self.models).grid.params.abuns)/2d0,$
                              n_elements((*self.models).grid.params.turbs)/2d0])
     self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
     spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/rfres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
     self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,shift=shift        
     self.init_shift = shift
     
     dir = 'output_'+(*self.active_object).obj
     spawn,'mkdir '+dir
     savefile=self.outdir+'/output_'+(*self.active_object).obj+'/specfit_'+self.version+'_'+self.runtype+'_objspec.sav'
     
     
     base_models = '/Users/zgazak/Projects/MassiveStars/RSGs/SPECFIT/focus_grids/'
     if file_test(base_models,/directory) eq 0 then spawn,'mkdir '+base_models
     start = 1
     cont = 1
     tried = rfres
     countdown = 3
     while cont gt 0 do begin
        ;; does the model grid exist that we will try next?
        file = 'focus_R'+string(rfres,format='(i5.5)')+'.sav'
        self->message,message='..running '+file,which='B'
        
        dwmod = self->get_depthwidth(base_models+file,rfres)
        if 1 then $             ;  if start then $
           dwdat = self->meas_depthwidth(dwmod)
                                ;     start = 0
                                ;  endif
        rfres_delta = self->analyze_dw(dwmod,dwdat,csq=csq)
        rfres_depthchi = min(total(csq,1),place,/nan)
        

        if start then trydelta = rfres_delta else trydelta = [trydelta,rfres_delta]
        if start then tried= rfres else tried = [tried,rfres]
        if start then csqs = rfres_depthchi else csqs = [csqs,rfres_depthchi]
        if start then start = 0
        
        self->startplot,3
        plot,[tried],[trydelta],psym=8,xrange=mm(tried)+[-100,100],/xs,yrange=mm(trydelta)+[-0.1,0.1]*(max(trydelta)-min(trydelta)),/ys,xtickformat='(i5)'
        hline,0,linestyle=2
        sharpcorners,thick=!x.thick
        self->endplot,3
        wait,.5
                                ;p   stop

                                ;  stop

        if abs(rfres_delta) eq rfres_delta then sign = 1 else sign = -1
        rfres_new = round((1.2/sqrt(sign*rfres_delta^2+(1.2/rfres)^2))/100d0)*100
        print,rfres,rfres_delta,rfres_new
        print,countdown
        print,''
                                ;   stop
        if rfres_new eq rfres then cont = 0 else begin
           rfres = rfres_new
           if min(trydelta) lt 0 and max(trydelta) gt 0 then countdown-=1
           if countdown eq 0 then cont = -2
           if max(rfres eq tried) then begin
              rfres = 0.5*(tried[n_elements(tried)-1]+rfres)
              rfres = round(rfres/100d0)*100
              if max(rfres eq tried) then cont = -3 else print,"replacing with: ",rfres
           endif 
        endelse
        
        
                                ;print,'test continue reasoning'
                                ;stop
        ;; cont test
     endwhile


     print,'cont val:  (-3,-2): jumping around... (-1): passed zero... fit exact (0): settled on good fit'
     print,'cont val: ',cont
     print,tried
     print,trydelta
     print,csqs

     if cont lt 0 then begin
        st = sort(tried)
        sorttry = tried[st]
        sortdelt = trydelta[st]
        cross = 0
        if sortdelt[0] lt 0 then  while sortdelt[cross] lt 0 DO cross+=1 ELSE $
           while sortdelt[cross] gt 0 DO cross+=1 ;   cross = max(where(sortdelt gt 0))
        cross--

        if 1 then begin
           try = fillarr(10,sorttry[cross],sorttry[cross+1])
           tryy = interpol(sortdelt[cross:cross+1],sorttry[cross:cross+1],try)
           minval = min(abs(tryy),place)
           rfres = try[place]
           
           rfres = tried[where(csqs eq min(csqs))]
           
           ;; does the model grid exist that we will try next?
           file = 'focus_R'+string(rfres,format='(i5.5)')+'.sav'
           self->message,message='..running '+file,which='B'
           
           dwmod = self->get_depthwidth(base_models+file,rfres)
           dwdat = self->meas_depthwidth(dwmod)

           rfres_delta = self->analyze_dw(dwmod,dwdat,csq=csq)
           
           trydelta = [trydelta,rfres_delta]
           tried = [tried,rfres]
           
           self->startplot,3
           plot,[tried],[trydelta],psym=8,xrange=mm(tried)+[-100,100],/xs,yrange=mm(trydelta)+[-0.1,0.1]*(max(trydelta)-min(trydelta)),/ys
           hline,0,linestyle=2
           sharpcorners,thick=!x.thick
           self->endplot,3
           
           help,csq
           
           (*self.active_object).overallres = rfres
           self->gravity_analysis,waverange=[1.16,1.206],customchi=total(csq,1)
           self->compact_analysis,waverange=[1.16,1.206],customchi=total(csq,1)
           
           for type=0,2 do begin
              case type of
                 0: ext = 'FeI'
                 1: ext = 'TiI'
                 2: ext = 'SiI'
              endcase
              
              chi2 = csq[where(strcmp(ext,(*self.linelist).type)),*,*,*,*]
              self->gravity_analysis,waverange=[1.16,1.206],customchi=total(chi2,1),strextra='_'+ext
              self->compact_analysis,waverange=[1.16,1.206],customchi=total(chi2,1),strextra='_'+ext
           endfor  
        endif else begin
           ;; in here we want to organize things on both sides of the boundary.
           print,cross
           print,  sorttry[cross:cross+1] ;,sorttry[cross+1]
           stop   
        endelse
        
        
                                ;stop
     endif
     if cont eq 0 then begin

        help,csq
        
        (*self.active_object).overallres = rfres
        self->gravity_analysis,waverange=[1.16,1.206],customchi=total(csq,1)
        self->compact_analysis,waverange=[1.16,1.206],customchi=total(csq,1)
        
        for type=0,2 do begin
           case type of
              0: ext = 'FeI'
              1: ext = 'TiI'
              2: ext = 'SiI'
           endcase
           
           chi2 = csq[where(strcmp(ext,(*self.linelist).type)),*,*,*,*]
           self->gravity_analysis,waverange=[1.16,1.206],customchi=total(chi2,1),strextra='_'+ext
           self->compact_analysis,waverange=[1.16,1.206],customchi=total(chi2,1),strextra='_'+ext
        endfor  

     endif
     
                                ; stop 
  endfor


  linelist=save_linelist
  linename = save_linename

  ptr_free,self.linelist
  self.linelist=ptr_new(replicate({type: '', wave: 0d0},n_elements(linelist)))
  for i=0,n_elements(linelist)-1 do begin
     (*self.linelist)[i].type = linename[i]
     (*self.linelist)[i].wave = linelist[i]
  endfor
  linelist =0L
  linename =0L


end


pro specfit::run_pixel
  self->message,message='****'+self.runtype+'****',which='A'
  widget_control,self.progbarA,set_value=0
  for i=0,n_elements(*self.objects)-1 do begin
     self->generalplotsetup
     for j=0,3 do begin
        self->startplot,j
        plot,[0],[0],/nodata,xtitle='X axis',ytitle='Y axis'
        sharpcorners,thick=!x.thick
        self->endplot,j
     endfor
     
     widget_control,self.progbarA,set_value=i/(n_elements(*self.objects))
     self.active_num = i+1
     self->makeobject,(*self.objects)[i]
     self->message,message = 'Fit '+last_element(strsplit((*self.objects)[i],'/',/extract)),which='A'
     
     dir = 'output_'+(*self.active_object).obj
     spawn,'mkdir '+dir
     
     savefile=self.outdir+'/output_'+(*self.active_object).obj+'/specfit_'+self.version+'_'+self.runtype+'_objspec.sav'
     
     ;; step 1 is to fit for resolution using only diagnostic lines..
     if file_test(savefile) eq 0 then begin  
        if self.run_diag eq 0 or 1 then begin
           new = 1
           self->resetchisq
           self->activeindex
           
           if self.res_locked eq 0 then begin
              self->message,message=" 1) resolution fitting",which='A'
              self->message,message=" 1) fit for resolution",which='B'
              self->runfit
              store_best = (*self.active_object).bestloc
              self->bestres
           endif else   (*self.active_object).overallres = self.expected_res
           
           (*self.active_object).lockres=1
           self->startplot,2
           plot,[0],[0],/nodata
           sharpcorners,thick=!x.thick
           self->endplot,2
           if self.lock_cont then (*self.active_object).lockcont=1
           
        endif else begin
           ;; we did run_diag, just adopt all of the best fits etc
           ;; from that.
                                ; stop
           (*self.active_object).overallres = (*self.store4pix).res
           (*self.active_object).lockres=1
           self->startplot,2
           plot,[0],[0],/nodata
           sharpcorners,thick=!x.thick
           self->endplot,2
           
        endelse
        
        ;; step 2 is to loop through all models, fit continuum, and at each
        ;; model check to see if there are any better pixel fits.  If so,
        ;; overwrite the "best parameters" for that pixel.
        
        ;; use the diagnostic lines to measure shift, use the full
        ;; spectrum to measure continuum.
                                ;  spawn,'say check this zach'
                                ;  stop
        
        ;; SETUP::
        self->switchmaster
        self->resetchisq
        self->activeindex
        self.lock_mask = 1
        self.use_master = 1
        self->message,message=" 2) Running with best resolution of "+string((*self.active_object).overallres,format='(d8.2)')
        self->message,message="    The best model is "+self->returnactive((*self.active_object).bestloc,/short)
        self.message='2) R='+string((*self.active_object).overallres,format='(i5)')+" ["+self->returnactive((*self.active_object).bestloc,/short)+"]"
        self->message,which='A'
        store_best = (*self.active_object).bestloc
        ;; END SETUP
        
        ptr_free,self.pix_fit
        
        self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
        spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),$
                     /quiet,/noinfo,lims=self.fitrange
        self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux,res=(*self.active_object).overallres
        

        pixfitarr = dblarr(5,n_elements(swave))
        pixfitarr += sqrt(-1)
        self.pix_fit = ptr_new(pixfitarr)
        
        
        widget_control,self.progbarB,set_value=0
        cont = 1     
        while cont do begin
           perc = (*self.active_object).goodmodcount/n_elements((*self.active_object).chisq)
           if  (*self.active_object).lockres eq 0 then perc = (*self.active_object).goodmodcount/n_elements((*self.active_object).chisq)
           widget_control,self.progbarB,set_value=perc
           self->activeindex
           self->chisq_pixel
           
           (*self.active_object).modcount++
           if (self.inext)[0] ne -1 then (*self.active_object).goodmodcount++  
           
           if (*self.active_object).goodmodcount ge n_elements((*self.active_object).chisq) then begin
              self->message,message=" Halting -- all models calculated."
              self->message,message=" Halting -- all models calculated.",which='A'
              cont=0
           endif
           
        endwhile
        widget_control,self.progbarB,set_value=1
        
        self->message,message="      runfit loop complete. "+$
                      string((*self.active_object).goodmodcount,format='(i5)')+" models calculated."
        

        
        window,xsize=800,ysize=1000
        !p.multi=[0,1,4]
        !x.margin = [10,10]
        pars = (*self.models).grid.params
        names = ['T','g','Z','x']
        for i=0,3 do begin
           plot,swave,(*self.pix_fit)[i,*],yrange=mm(pars.(i))+[-1,1]*0.1*(max(pars.(i))-min(pars.(i))),$
                /xs,/ys,color=(*self.colors).black,psym=8,symsize=1,ytitle=names[i],$
                title=string(average((*self.pix_fit)[i,where(finite((*self.pix_fit)[i,*]))]),$
                             stddev((*self.pix_fit)[i,where(finite((*self.pix_fit)[i,*]))]),format='(d7.2,d7.2)'),/noerase
           oplot,swave,(*self.pix_fit)[i,*],color=(*self.colors).blue,psym=8,symsize=0.8
           plot,(*self.active_object).bestwave,(*self.active_object).bestdat,yrange=mm((*self.active_object).bestdat)+[-1,1]*0.1*(max((*self.active_object).bestdat)-min((*self.active_object).bestdat))
           axis,!x.crange[1],!y.crange[1],color=(*self.colors).black,yaxis=1,ytickv=([0,0.25,.5,0.75,1.0]*(!y.crange[1]-!y.crange[0]))+!y.crange[0],ytitle='Flux'
           
           sharpcorners,color=(*self.colors).black,thick=!x.thick 
        endfor
        stop
        

        
        
     endif else begin ;; a savefile for this object is already completed...
        new = 0
        if self.made_active then ptr_free,(*self.active_object).linemask,$
                                          (*self.active_object).storecont,$
                                          (*self.active_object).bestcont,$
                                          (*self.active_object).header
        ptr_free,self.active_object
        restore,savefile 
        
        self.active_object = ptr_new(objspec)
        restored=1
        self->message,message=' Restoring '+objspec.obj+' savefile '+savefile
        
        ptr_free,objspec.linemask,$
                 objspec.storecont,$
                 objspec.bestcont,$
                 objspec.header
        objspec=0L
     endelse
     
     ;; step 3 is the analysis stage... just first make a simple plot of
     ;; flux v wavelength, t v w, gvw,zvw,xvw
     
     
     
     
  endfor
  stop
end

pro specfit::run
  self->message,message='****'+self.runtype+'****',which='A'
  
  self->writetotex,'lcccccc',s_table=['Object','T$_{eff}$ [K]','log g','[Z]','$\xi$ [km/s]','$\chi^{2}_{min}$','R [$\frac{\lambda}{\delta\lambda}$]']
  
                                ; stop
  widget_control,self.progbarA,set_value=0
  
  refit = 0
  for i=0,n_elements(*self.objects)-1d0 do begin
     restored=0
     heap = 0
     if heap then help,/heap
     
     self->generalplotsetup
     
     if heap then help,/heap
     
     for j=0,3 do begin
        self->startplot,j
        plot,[0],[0],/nodata,xtitle='X axis',ytitle='Y axis'
        sharpcorners,thick=!x.thick
        self->endplot,j
     endfor

     if heap then self->heaphelp,'1A'


     if refit eq 0 then begin
        widget_control,self.progbarA,set_value=i/(n_elements(*self.objects))
        self.active_num = i+1
        self->makeobject,(*self.objects)[i]
        self->message,message = 'Fit '+last_element(strsplit((*self.objects)[i],'/',/extract)),which='A'
     endif else begin
        stop
        i-=1
        self.active_num = i+1
        sno = (*self.active_object).origsnr
        snn = (*self.active_object).modsnr
        self->makeobject,(*self.objects)[i]
        self->message,message = 'ReFit (snr mod)'+last_element(strsplit((*self.objects)[i],'/',/extract)),which='A'
        self->message,message='Orig S/N: '+string(sno),which='A'
        self->message,message='New  S/N: '+string(snn),which='A'
        (*self.active_object).spec[*,2] =  (*self.active_object).spec[*,1]/$
                                           median((*self.active_object).bestdat)/snn
        spawn,'mv '+self.outdir+'/output_'+(*self.active_object).obj+'/specfit_'+self.version+'_'+self.runtype+'_objspec.sav '$
              +self.outdir+'/output_'+(*self.active_object).obj+'/specfit_'+self.version+'_'+self.runtype+'_objspect.sav'
                                ; (*self.active_object).refit = 0
        refit = 0
     endelse
     
     dir = 'output_'+(*self.active_object).obj
     spawn,'mkdir '+dir
     
     savefile=self.outdir+'/output_'+(*self.active_object).obj+'/specfit_'+self.version+'_'+self.runtype+'_objspec.sav'
     
; if i eq 2 then begin
     if file_test(savefile) eq 0 then begin    ;; and if i eq 2 is TEMPORARY!!!  
        new = 1
        self->resetchisq
        self->activeindex
        
        if self.res_locked eq 0 then begin
           self->message,message=" 1) resolution fitting",which='A'
           self->message,message=" 1) fit for resolution",which='B'
           self->runfit
           store_best = (*self.active_object).bestloc
           
           ;;  stop
           
           self->bestres
        endif else   (*self.active_object).overallres = self.expected_res
        
      ;  (*self.active_object).lockres=1
        self->startplot,2
        plot,[0],[0],/nodata
        sharpcorners,thick=!x.thick
        self->endplot,2
        
        if self.lock_cont then (*self.active_object).lockcont=1
        ;;  self->resetchisq
        ;;  self->activeindex
        ;;  self->switchmaster
        ;;  self.use_master = 1
        
        ;;  self->message,message="   2) Running with best resolution of "+string((*self.active_object).overallres,format='(d8.2)')
        ;;  self->runfit
        
        cont = 1
        self.first = 1
        
        tried=0
        while cont do begin
                                ;  print,store_best, (*self.active_object).bestloc 
                                ;  store_best = (*self.active_object).bestloc
           
           
           store_best = self->find_bestloc(self.locsize)
           (*self.active_object).lockres=1
           
           ;;stop
           
           self->switchmaster
           
           self->resetchisq
           self->activeindex
           
           self.lock_mask = 1
           self.use_master = 1
           

           self->message,message=" 2) Running with best resolution of "+string((*self.active_object).overallres,format='(d8.2)')
           self->message,message="    The best model is "+self->returnactive((*self.active_object).bestloc,/short)
           
           self.message='2) R='+string((*self.active_object).overallres,format='(i5)')+" ["+self->returnactive((*self.active_object).bestloc,/short)+"]"
           self->message,which='A'
                                ;store_best = (*self.active_object).bestloc
           
                                ;  print,store_best
                                ;   stop
           
           self->runfit
           newbest = self->find_bestloc(self.locsize)
           if n_elements(newbest) eq 3 then begin
              if store_best[0] ne -1 then $
                 if store_best[0] eq newbest[0] and $
                 store_best[1] eq newbest[1] and $
                 store_best[2] eq newbest[2] then cont=0 else tried+=1
              
           endif else $
              if store_best[0] ne -1 then $
                 if store_best[0] eq newbest[0] and $
              store_best[1] eq newbest[1] and $
              store_best[2] eq newbest[2] and $
              store_best[3] eq newbest[3] then cont=0 else tried+=1
           
           
           
           print,store_best,newbest
                                ;  stop
           if tried ge 4 then cont = 0
           
        endwhile
        self.lock_mask = 0
        self->save_object
     endif else begin
        if heap then self->heaphelp,'4A'
        new = 0
        if self.fullskipsaves eq 0 then begin
           if heap then self->heaphelp,'3A'
           
           if self.made_active then ptr_free,(*self.active_object).linemask,$
                                             (*self.active_object).storecont,$
                                             (*self.active_object).bestcont,$
                                             (*self.active_object).header
           ptr_free,self.active_object
           restore,savefile 
           
           self.active_object = ptr_new(objspec)
           restored=1
           
           if self.clearmc then begin
              (*self.active_object).mc_params *= 0
              (*self.active_object).mc_params_data *= 0
           endif
            ;  endif

           ;; ptr_free,self.active_object
           
           self->message,message=' Restoring '+objspec.obj+' savefile '+savefile
           
           ptr_free,objspec.linemask,$
                    objspec.storecont,$
                    objspec.bestcont,$
                    objspec.header
           objspec=0L
           
           if heap then self->heaphelp,'6A'
        endif else self->message,message=' NOT Restoring savefile.  fullskipsaves active.'
     endelse
     
     if heap then self->heaphelp,'5A'
     
     if heap then stop
     ;;stop
     
                                ; stop
     if self.runtype eq 'run_diag' then begin
        ptr_free,self.store4pix 
        self.store4pix = ptr_new({res: (*self.active_object).overallres})
     endif
     

     if self.fullskipsaves eq 0 then begin
        if self.do_master and restored eq 0 then self->save_fits
        self->analysis
        
        if self.skipsaves eq 0 or new then begin
           if self.full_analysis then begin
              self->rev_nomg_range_analysis
              self->mask_analysis_gravtest
                                ;  self->rev_nomg_range_analysis
              self->rev_range_analysis
              self->nomg_range_analysis
              self->range_analysis
              self->mask_analysis
              self->line_analysis
           endif else begin
              case self.runtype of
                 'run_diag': begin
                    ;; self->quadlin,waverange=[1.160,1.206]
                    self->compact_analysis,waverange=[1.160,1.206]
                   ; self->best10_analysis,waverange=[1.160,1.206]
                                ;  self->gravity_analysis,waverange=[1.160,1.206]
                                ;  self->byparam_analysis,waverange=[1.160,1.206]
                    self->mc_analysis,waverange=[1.160,1.206]
                    self->mc_analysis,waverange=[1.160,1.206],/data
                    
                    ;; self->new_analysis,waverange=[1.160,1.206]
                    ;; self->aplots,/narrow
                    ;; self->aplots
                 end
                 'run_full': begin
                    ;; self->quadlin,waverange=[1.160,1.206]
                    ;; self->compact_analysis,waverange=[1.156,1.206]
                    self->compact_analysis,waverange=[1.160,1.206]
                    ;self->best10_analysis,waverange=[1.160,1.206]
                                ;  self->gravity_analysis,waverange=[1.160,1.206]
                                ;  self->byparam_analysis,waverange=[1.160,1.206]
                    self->mc_analysis,waverange=[1.160,1.206]
                    self->mc_analysis,waverange=[1.160,1.206],/data
                    
                    ;; self->new_analysis,waverange=[1.160,1.206]
                    ;; self->aplots,/narrow     
                    self->aplots
                    self->save_object
                    
                    waverange = [1.16,1.205]
                    self->compact_analysis,waverange=waverange

               ;     waverange = [1.16,1.205]
               ;     self->compact_analysis,waverange=waverange

                    waverange = [1.16,1.200]
                    self->compact_analysis,waverange=waverange

                    waverange = [1.166,1.206]
                    self->compact_analysis,waverange=waverange

                    waverange = [1.166,1.205]
                    self->compact_analysis,waverange=waverange

                    waverange = [1.166,1.200]
                    self->compact_analysis,waverange=waverange

                    waverange = [1.165,1.205]
                    self->compact_analysis,waverange=waverange

                    waverange = [1.164,1.205]
                    self->compact_analysis,waverange=waverange

                    if 0 then begin
                       ttt = 1.16
                       while ttt lt 1.206 do begin
                          self->compact_analysis,waverange=ttt+[0,0.01]
                          ttt+=0.01
                       endwhile
                       ttt = 1.16
                       while ttt lt 1.206 do begin
                          self->compact_analysis,waverange=ttt+[0,0.005]
                          ttt+=0.005
                       endwhile
                       
                       start = 1.16
                       fin = 1.206
                       step = 0.005
                       count = 1
                       while start+step*count lt fin do begin
                          self->compact_analysis,waverange=[start,start+(step*count)]
                          count++
                       endwhile
                       
                       count = 1
                       while fin-(step*count) gt start do begin
                          self->compact_analysis,waverange=[fin-(step*count),fin]
                          count++
                       endwhile
                                ;stop
                    endif
                       
                    
                 end
                 else: stop
              endcase
           endelse
        endif                   ;else ;self->compact_analysis,waverange=[1.16,1.208]
        
      ;  stop
        self->save_object
     ;   stop
        
        self->writetotex,/clos_output,/output_report
        ;; stop
        
     endif 
                                ; endif
     
     ;stop

     ptr_free,self.models
     self.models = ptr_new()
                                ;    if (*self.active_object).refit then refit=0
  endfor 
  widget_control,self.progbarA,set_value=1


  self->writetotex,/c_table
  self->writetotex,/close
  ;;close,/all
end


pro specfit::save_fits
  
  spec =  (*self.active_object).spec
  place = where((*self.active_object).spec[*,0] ge self.fitrange[0]+self.init_shift and (*self.active_object).spec[*,0] le self.fitrange[1]+self.init_shift)
  spec = spec[place,*]
  
  hdr = *(*self.active_object).header
  sxaddpar,hdr,'expres',(*self.active_object).overallres
  
  mwrfits,spec,self.outdir+'/output_fits'+'/FixW_'+(*self.active_object).obj+'.fits',hdr,/create
  
  *self.location = (*self.active_object).bestloc
  self->getmodel,modwave=modwave,modflux=modflux,modres=modres,fail=fail
  spec_conv5aa,modwave,modflux,wave,flux,fwhm=sqrt((1.1/(*self.active_object).overallres)^2d0-(1.1/modres)^2d0),/quiet,/noinfo,lims=self.fitrange
  self->prepdata,wave,flux,swave=swave,sflux=sflux,derr=derr,dflux=dflux        
  self->contcorrect,flux=sflux,comp=dflux,cont=cont,wave=swave
  
;  dflux *=cont
  spec = dblarr(n_elements(swave),3)
  spec[*,0] = swave
  spec[*,1] = dflux*cont
  spec[*,2] = derr*cont
  mwrfits,spec,self.outdir+'/output_fits'+'/FixWC_'+(*self.active_object).obj+'.fits',hdr,/create
  spec = 0L
  hdr = 0L
  place = 0L
  swave = 0L
  
end


pro specfit::save_object
  objspec = (*self.active_object)
  self->message,message=' Saving '+objspec.obj+'.'
  dir = 'output_'+objspec.obj
                                ; spawn,'mkdir '+dir
  save,objspec,filename=dir+'/specfit_'+self.version+'_'+self.runtype+'_objspec.sav'
  objspec=0L
  dir = 0L
;;  stop
end

function specfit::INIT,version=version,obj_dir=obj_dir,expected_res=expected_res,$
                       model_grid=model_grid,fitrange=fitrange,cont_order=cont_order,$
                       init_shift=init_shift,chunkfit=chunkfit,outdir=outdir,lockres=lockres,$
                       ignore_wave=ignore_wave,skipsaves=skipsaves,long_analysis=long_analysis,$
                       mg=mg,fullskipsave=fullskipsave,do_only=do_only,fit_res=fit_res,orig_res=orig_res,$
                       turb_avoid=turb_avoid,lock_cont=lock_cont,grav_avoid=grav_avoid,auto_start=auto_start,$
                       no_plots=no_plots,lock_array=lock_array,lock_values=lock_values,fitmethods=fitmethods,$
                       sp_line=sp_line,stopper=stopper,justfe=justfe,locsize=locsize,linemask=linemask,$
                       clearmc= clearmc
  !p.font=0
  !EXCEPT=0 ;; disable math error warning
  plotsym,0,1.2,/fill
  self.diag = 1
  self.heap = 1
  ;;short_analysis=1


  if keyword_set(clearmc) then self.clearmc = 1
  ptr_free,self.linemask
  if 1-keyword_set(linemask) then self.linemask = ptr_new({active: 0}) else self.linemask= ptr_new(linemask)
     
  ;stop
     
     

  
  
  
  self.singline= 1.3d0
  self.doubline= 1.5d0
  
  if keyword_set(locsize) then self.locsize=locsize else self.locsize=1
  
  self.run_full = 0
  self.run_diag = 0
  self.run_diag_focus = 0
  self.run_pixel = 1
  if keyword_set(fitmethods) then begin
     self.run_full = fitmethods[0]
     self.run_diag = fitmethods[1]
     self.run_diag_focus = fitmethods[2]
     self.run_pixel = fitmethods[3]
  endif
  
  if keyword_set(stopper) then self.stopper = 1


  self.xmargin = [6.5,2]
  self.ymargin = [3.0,2.0]

  if keyword_set(turb_avoid) then self.turb_avoid = turb_avoid else self.turb_avoid = 0
  if self.turb_avoid ne 0 then self.interpolate=1
  if keyword_set(grav_avoid) then self.grav_avoid = grav_avoid else self.grav_avoid = -99d0
  if keyword_set(lock_cont) then self.lock_cont=1 else self.lock_cont=0
  if keyword_set(lock_array) then self.lock_param = lock_array else self.lock_param = [0,0,0,0]
  if keyword_set(lock_values) then self.lock_values = lock_values else self.lock_values = [-99,-99,-99,-99]
  ;; print,self.lock_param
  ;; print,self.lock_values

  self.made_active = 0
  
  ;; self.dz = 0.25d0/20d0
  ;; self.dT = 100d0/20d0
  ;; self.dg = 0.5d0/20d0
  ;; self.dx = 1d0/20d0
  
  self.div = 10d0
  self.dz = 0.25d0/self.div
  self.dT = 100d0/self.div
  self.dg = 0.5d0/self.div
  self.dx = 1d0/self.div
  

  
  
  self.logfileA = -1
  self.logfileB = -1

  self.full_analysis = 0
  self.nomg = 1
  
  if keyword_set(fit_res) then self.fit_res = ptr_new(fit_res)
  if keyword_set(do_only) then self.do_only = do_only
  if 1-keyword_set(long_analysis) then self.full_analysis = 0
  if 1-keyword_set(ignore_wave) then self.do_master = 1
  if keyword_set(mg) then self.nomg=0
  if keyword_set(expected_res) then self.expected_res = expected_res else self.expected_res=15000d0
  if keyword_set(init_shift) then self.init_shift=init_shift
  if keyword_set(lockres) then self.res_locked = 1
  self.version = version
  if 1-keyword_set(obj_dir) then spawn,'pwd',obj_dir
  
  self.directory = obj_dir
  self.saved_grids = '/Users/zgazak/Projects/MassiveStars/RSGs/SPECFIT/saved_grids/'
  if 1-keyword_set(model_grid) then self.modgrid= '/Users/zgazak/Projects/MassiveStars/RSGs/SSC/RSG_Maria/package_models/Maria_20130318_J.sav' else $
     self.modgrid = model_grid
  if 1-keyword_set(fitrange) then begin
     fitrange=[1.156,1.206]
     ;; fitrange=[1.163,1.208]
  endif
  self.fitrange=fitrange
  
  ;; self.contorder=1
  if keyword_set(cont_order) then self.contorder = cont_order else self.contorder=1
  
  ptr_free,self.location
  self.location = ptr_new(-1)
  self.baseres = self.expected_res  
  ;; self.baseres = 20000d0

  if keyword_set(skipsaves) then self.skipsaves = 1
  if keyword_set(fullskipsave) then self.fullskipsaves = 1
  
  if 1-keyword_set(outdir) then $
     self.outdir = 'specfit_'+curr_date(format='yyyymmdd_hhmm') else $
        self.outdir = outdir
  ;; spawn,'mkdir '+self.outdir
  
  if self.skipsaves then begin
     openw,lun,'temp_specfit_log_A.txt',/get_lun,width=2500,bufsize=0,/append 
     openw,lun2,'temp_specfit_log_B.txt',/get_lun,width=2500,bufsize=0,/append 
  endif else begin
     openw,lun,'temp_specfit_log_A.txt',/get_lun,width=2500,bufsize=0
     openw,lun2,'temp_specfit_log_B.txt',/get_lun,width=2500,bufsize=0
  endelse
  self.logfileA = lun
  self.logfileB = lun2
  

  ;;; WIDGET SETUP
  self.buttonfont = ''
  self.textfont=''
  self.smallfont=''
  

  menus = ['Setup','Fitting Options','Analysis','Plotting','Quit']
  self.cr = string(10B)
  
  ptr_free,self.colors,self.plot_windows,self.sliders,$
           self.buttons,self.menus,self.bases,self.extra_windows,$
           self.paths,self.fields,self.bases_id,$
           self.mainbases,self.setbases,self.setbases_id,$
           self.layout_bases,self.infields,self.infields_id
  self.colors = ptr_new(specfit_colors())
  
  self.infields    = ptr_new(lonarr(6))
  self.infields_id = ptr_new(strarr(6))
  
  ;; DEFINE INFIELDS
  (*self.infields_id) = ['lam_min',$
                         'lam_max',$
                         'lock_teff',$
                         'lock_logg',$
                         'lock_z',$
                         'lock_xi']
  
  self.plot_windows = ptr_new(replicate({x: 0d, y: 0d, w_id: 0L, pw_id: 0L, $
                                         window:0L, pix_window: 0L, xrange: [0d0,0d0], $
                                         yrange: [0d0,0d0]},10))
  
  basenum = 5
  
  self.sliders = ptr_new(replicate({id: 0},10))
  self.menus = ptr_new(menus)
  self.bases = ptr_new(lonarr(n_elements(*self.menus)))
  self.bases_id = ptr_new(menus)
  self.mainbases = ptr_new(lonarr(basenum))
  self.setbases = ptr_new(lonarr(basenum))
  self.setbases_id = ptr_new(strarr(basenum))
  self.layout_bases = ptr_new(lonarr(11))

  self.extra_windows = ptr_new(lonarr(basenum))
  self.buttons = ptr_new(lonarr(20))
  self.paths = ptr_new(strarr(basenum))
  self.fields = ptr_new(dblarr(basenum,2))
  
  self->widget_setup
  
  if keyword_set(auto_start) then self.auto_start = 1

  widget_control,(*self.bases)[self->base('Setup')],MAP=1        
  widget_control,self.master_base, set_UVALUE=self 
  
  self->setup,'plotting'
  
  ;; cleanup
  menus = 0L
  
  ;; linelist = [1.16383,1.169,1.1884,1.18828,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
  ;; linename = ['FeI'  ,'FeI','FeI' ,'FeI'  ,'TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ]
  
  ;; high resolution or fit diag.  
  linelist = [1.169,1.18835,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
  linename = ['FeI','FeI'  ,'TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ]
  linenum = [    1,     2,      1,      1,      1,      1,      1,      1]

  if keyword_set(sp_line) then begin
     ;; high resolution or fit diag.  
     linelist = [1.169,1.18835,1.1893,1.19495,1.19733,1.19842,1.19916]
     linename = ['FeI','FeI'  ,'TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ]
     linenum = [    1,     2,      1,      1,      1,      1,      1]
  endif
  
  if keyword_set(justfe) then begin
     ;; high resolution or fit diag.  
     linelist = [1.169,1.18835,1.19733]
     linename = ['FeI','FeI'  ,'FeI'  ]
     linenum =  [    1,      2,      1]
  endif
  
  ;; linelist = [1.169,1.1884,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
  ;; linename = ['FeI','FeI' ,'TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ]
  
  
  ;; linelist = [1.169,1.1893,1.19495,1.19733,1.19842,1.19916,1.20315]
  ;; linename = ['FeI','TiI' ,'TiI'  ,'FeI'  ,'SiI'  ,'SiI'  ,'SiI'  ]
  
  
  ptr_free,self.linelist
  self.linelist=ptr_new(replicate({type: '', wave: 0d0, num: 0d0},n_elements(linelist)))
  for i=0,n_elements(linelist)-1 do begin
     (*self.linelist)[i].type = linename[i]
     (*self.linelist)[i].wave = linelist[i]
     (*self.linelist)[i].num = linenum[i]
     
  endfor
  linelist =0L
  linename =0L
  
  self->start
  
  self.plot_test=0
  if 1-keyword_set(no_plots) then self.makeplot = 1 
 ; self.makeplot=1
  if self.auto_start then begin
     self->message,message='Auto Starting...',which='B'
     self->message,message='Auto Starting...',which='A'
     self->buttonevent,{object:self, method:'ButtonEvent', value:'Set Spec Dir'},/manual
     self->buttonevent,{object:self, method:'ButtonEvent', value:'Load Grid', field:2},/manual
     self->buttonevent,{object:self, method:'ButtonEvent', value:'Output Dir', field:3},/manual
     ;;  stop
     self->run_button_code
     ;; self->quit
  endif
  
  return,1
end


pro specfit::realize_windows
  ;; plot windows must be realized AFTER the widget is
  ;; activated... this organizes the process
  !p.color = (*self.colors).black
  !p.background = (*self.colors).white
  
  self->generalplotsetup
  
  for i=0,3 do begin
     widget_control, (*self.plot_windows)[i].window, Get_Value=wid
     (*self.plot_windows)[i].w_id=wid
     
     window, xsize=(*self.plot_windows)[i].x $
             , ysize=(*self.plot_windows)[i].y $
             , /pixmap,/free
     (*self.plot_windows)[i].pix_window = !d.window
     
     self->startplot,i
     plot,[0],[0],/nodata,xtitle='X axis',ytitle='Y axis'
     sharpcorners,thick=!x.thick
     self->endplot,i
  endfor
  
end

pro specfit::start
  centertlb,self.master_base
  widget_control,self.master_base,/realize
  widget_control,self.master_base, XOffset=0, YOffset=0
  
  self->realize_windows
  
  
  self->message,message = 'specFit '+self.version+' A',which='A'
  self->message,message='Global Progress',which='A'  
  self->message,message=' ## Continuum order = '+string(self.contorder,format='(i3)'),which='A'
  if self.turb_avoid ne 0 then self->message,message=' ## Xi avoid = '+string(self.turb_avoid,format='(i3)'),which='A'
  if self.grav_avoid ne -99d0 then self->message,message=' ## logg avoid = '+string(self.grav_avoid,format='(i3)'),which='A'
  if self.lock_cont then self.message = ' ## Locking Continuum' else self.message = ' ## Not locking continuum'
  self->message,which='A'
  self->message,message=' ## Minimum smoothing = '+string(self.locsize,format='(i3)'),which='A'
  
  
  self->message,message='specFit '+self.version+' B',which='B'
  self->message,message='Local Progress',which='B'

end


pro specfit::message,event,message=message,which=which
  if keyword_set(message) then self.message = message
  if 1-keyword_set(which) then which='B'
  case which of
     'A': begin
        widget_control,(*self.setbases)[self->setbase('message')],/append,$
                       set_value='('+curr_date(format='mm:ss')+') '+self.message
        ;; print,self.message
        if self.logfileA ne -1 then printf,self.logfileA,'('+curr_date(format='hh:mm:ss')+') '+self.message
     end
     'B': begin
        widget_control,(*self.setbases)[self->setbase('messageB')],/append,$
                       set_value='('+curr_date(format='mm:ss')+') '+self.message
        ;; print,self.message
        if self.logfileB ne -1 then printf,self.logfileB,'('+curr_date(format='hh:mm:ss')+') '+self.message
     end
     else: stop
  endcase
  self.message=''
  
end


pro specfit__define
  struct = {specfit,$
            clearmc: 0L,$
            string: '',$
            stopper: 0L,$
            first: 0L,$
            version: '',$
            diag: 0L,$
            heap: 0L,$
            colors: ptr_new(), $
            linemask: ptr_new(),$
            $ ;; data and models
            objects: ptr_new(),$
            models: ptr_new(),$
            active_object: ptr_new(),$
            active_num: 0,$
            modgrid: '',$
            directory: '',$
            location: ptr_new(),$
            cr: '',$
            datdepths: ptr_new(),$
            $ ;; fit parameters
            lock_param: dblarr(4),$
            lock_values: dblarr(4),$
            interpolate: 0,$
            expected_res: 0d0,$
            fitrange: dblarr(2),$
            baseres: 0d0,$
            resample: 0,$
            lockres: 0,$
            res_locked: 0,$
            initshift: 0,$
            contorder: 0,$
            fit_res: ptr_new(),$
            turb_avoid: 0d0,$
            grav_avoid: 0d0,$
            lock_mask:0,$
            $ ;; fitting
            existing_best: 0L,$
            runtype: '',$
            run_full: 0,$
            run_diag: 0,$
            diag_mask: ptr_new(),$
            run_diag_focus: 0,$
            run_pixel: 0,$
            linelist: ptr_new(),$
            $ ;; fitting2
            lock_cont: 0,$
            inext: 0d0,$
            initialfits: dblarr(4,81),$
            curr_chi: 0d0,$
            best_chi: [0d0, 0d0, 0d0],$
            store_prevchi: [0d0, 0d0, 0d0],$
            skipsaves: 0,$
            fullskipsaves: 0,$
            xmargin: dblarr(2),$
            ymargin: dblarr(2),$
            locsize: 0,$
            $ ;; housekeeping
            makeplot: 0L,$
            message: '',$
            logfileA: 0,$
            logfileB: 0,$
            texfile: 0,$
            asciifile: intarr(10),$
            outdir: '',$
            saved_grids: '',$
            base_plot: '',$
            init_shift: 0d0,$
            master_dflux: ptr_new(),$
            master_derr: ptr_new(),$
            master_swave: ptr_new(),$
            do_master: 0,$
            use_master: 0,$
            full_analysis: 0,$
            nomg: 0,$
            singline: 2d0,$
            doubline: 2.5d0,$
            made_active:0,$
            $ ;; grid
            div: 5d0,$
            dz: 0d0,$
            dT: 0d0,$
            dg: 0d0,$
            dx: 0d0,$
            $ ;; end
            curr_plot: '',$
            do_only: 0,$
            $ ;; pixel stuff
            store4pix: ptr_new(),$
            pix_fit: ptr_new(),$
            $ ;; Widget stuff
            master_base: 0L,$
            plot_windows: ptr_new(),$
            sliders: ptr_new(),$
            buttons: ptr_new(),$
            menus: ptr_new(),$
            bases: ptr_new(),$      
            bases_ID: ptr_new(),$
            setbases: ptr_new(),$   
            setbases_ID: ptr_new(),$
            layout_bases: ptr_new(),$
            mainbases: ptr_new(),$            
            extra_windows: ptr_new(),$
            paths: ptr_new(),$
            fields: ptr_new(),$
            infields: ptr_new(),$
            infields_id: ptr_new(),$
            progbarA: 0L,$
            progbarB: 0L,$
            messageA: '',$
            messageB: '',$  
            buttonfont: '',$
            textfont: '',$
            smallfont: '',$
            grav_param: dblarr(4),$
            grav_sigs: dblarr(4),$
            mc_sigs: dblarr(4),$
            mc_sigs_data: dblarr(4),$
            $ ;; closer
            plot_test: 0L,$
            exists: 1L, $
            auto_start: 0L $
           }
end



pro specfit,_REF_EXTRA=_extra
  ;; current version: v3.0
  ;;   speed enhancements, allow chunk fitting (wavelength errors)
  ;; v3.10: added a non cont-lock mode...
  ;; v3.11: added a grav_avoid and turb_avoid mainly to block grav =
  ;;        1.0 and turb = 1.0
  ;; v3.12: add an interpolate over bad model code.
  ;; v4.00: adding new fitting method, diag_line
  ;; v4.01: removed first iron line, that area generally blacked out
  ;;        due to telluric contamination
  ;; v4.02: added option to lock parameters
  ;; v5.00: try out new non reduced chisq routines....
  ;; v7.00: diag_line... something is not right, try to fix here
  ;; v8.00: add by-pixel best model solutions
  ;; v9.00: add better bestlocation finding
  ;; v10.0: optimize bestloc, fix how models are selected to remove
  ;;        those which go negative as well as those which have 
  ;;        emission features.
  specfit = obj_new('specfit',_EXTRA=_extra,version='v12.0')
end

