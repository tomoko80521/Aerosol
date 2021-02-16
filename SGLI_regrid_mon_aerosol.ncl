; SGLI_regrid_mon_aerosol.ncl
;
; regrid SGLI AOT data (monthly mean) into 1 deg spatial resolution 

begin

  PLOT  = False

  setfileoption("nc", "FileStructure", "Advanced")

  diri  = "/Users/input_directory/"
  keyi  = "GC1SG1_20200101D01M_D0000_3MSG_"
  fili  = systemfunc("cd "+diri+" ; ls "+keyi+"*AOT*.h5")
  var           = (/"AOTL", "AOTO"/)
  varname       = (/"AROT_land_AVE", "AROT_ocean_AVE"/)

  diro  = "/Users/output_directory/"    ; directory for netCDF output
  filo  = var+"_mon_mean_20200101.nc"                   ; Output file

  ; For plotting
  pltDir        = "/Users/image_directory/"     ; directory for plot output
  pltName	= "SGLI_AOD_202001"                      ; plot name
  pltType	= "png"                                 ; plot type

  ; Miscellaneous
  nvar  = dimsizes(var)
  plot  = new(nvar, "graphic")

  csv_file = "/Users/tkoyama/Research/SGLI/"+pltName+".csv"

  wks = gsn_open_wks(pltType, pltDir+pltName)

  do i = 0, nvar-1
     ;************************************************
     ; read in data
     ;************************************************
     fi         = addfile(diri+fili(i), "r")
     g          = fi=>/Image_data
     g_atts     = getvaratts(g)
     x          = g->$varname(i)$
     printVarSummary(x)

     buff       = where(x.ne.x@Error_DN, x*x@Slope + x@Offset, -999.9)

     nlat       = g@$g_atts(7)$
     mlon       = g@$g_atts(8)$

     d          = 180.0/nlat
     lat        = 90.0 - ispan(0, nlat-1, 1)*d - d/2.0
     lon	= -180.0 + ispan(0, mlon-1, 1)* d + d/2.0
     lat@units	= "degrees_north"
     lon@units	= "degrees_east"
     lat!0      = "lat"
     lon!0      = "lon"
     lat@long_name      = "latitude"
     lon@long_name      = "longitude"
     lat@axis	= "Y"
     lon@axis	= "X"

     buff!0     = "lat"
     buff!1     = "lon"
     buff&lat	= lat
     buff&lon	= lon

     buff@_FillValue    = -999.9

     lat        := lat(::-1)                                  ; reverse arrays in the lateral direction
     buff 	:= buff(::-1,:)

     nnewlat    = 180
     mnewlon    = 360
     newlat     = fspan(-89.5,89.5,nnewlat)
     newlon	= fspan(-179.5,179.5,mnewlon)
     newlat@units       = "degrees_north"
     newlon@units	= "degrees_east"
     newlat!0	= "lat"
     newlon!0	= "lon"
     newlat@longname    = "latitude"
     newlon@longname    = "longitude"

     ;Interpolates from high resolution rectilinear grids to low resolution recitlinear grids using local area averaging.
     opt        = True
     opt@critpc = 50
     newval	= area_hi2lores_Wrap(lon,lat,buff,True,1,newlon,newlat,opt)

     newval!0	= "lat"
     newval!1	= "lon"
     newval&lat	= newlat
     newval&lon	= newlon

     system("/bin/rm -f " + diro + filo(i))                     ; remove if exists
     fout  = addfile (diro + filo(i), "c")                      ; netCDF name output

     ;===================================================================
     ; explicitly declare file definition mode. Improve efficiency.
     ;===================================================================
     setfileoption(fout, "DefineMode", True)

     ;===================================================================
     ; create global attributes of the file
     ;===================================================================
     fAtt               = True            ; assign file attributes
     ;fAtt@title         = "NCL Efficient Approach to netCDF Creation"
     fAtt@source_file   = fili(i)
     ;fAtt@Conventions   = "None"
     fAtt@creation_date = systemfunc("date")
     fileattdef( fout, fAtt )            ; copy file attributes

     ;===================================================================
     ; predefine the coordinate variables and their dimensionality
     ; Note: to get an UNLIMITED record dimension, we set the dimensionality
     ; to -1 (or the actual size) and set the dimension name to True.
     ;===================================================================
     dimNames = (/"lat", "lon"/)
     dimSizes = (/nnewlat,  mnewlon/)
     dimUnlim = (/False, False/)
     filedimdef(fout,dimNames,dimSizes,dimUnlim)

     if (i.eq.0) then
        filevardef(fout, "AOTL", typeof(newval), getvardims(newval))    ; write branch
        filevarattdef(fout, "AOTL", newval)
     else
        filevardef(fout, "AOTO", typeof(newval), getvardims(newval))    ; copy variable attributes
        filevarattdef(fout, "AOTO", newval)
     end if

     filevardef(fout, "lat", typeof(newlat), getvardims(newlat))
     filevardef(fout, "lon", typeof(newlon), getvardims(newlon))

     filevarattdef(fout, "lat", newlat)                                 ; copy lat attributes
     filevarattdef(fout, "lon", newlon)                                 ; copy lon attributes

     fout->lat          = (/newlat/)
     fout->lon          = (/newlon/)
     if (i.eq.0) then
        fout->AOTL      = (/newval/)
        AOTL    := newval
     else
        fout->AOTO      = (/newval/)
        AOTO    := newval
     end if

     ;===================================================================
     ; explicitly exit file definition mode. **NOT REQUIRED**
     ;===================================================================
     setfileoption(fout,"DefineMode",False)

     ;****************************************************************************
     ; Generate panel entries
     ;****************************************************************************
     if (PLOT) then
        res                = True
        res@gsnDraw        = False                         ; do not draw picture
        res@gsnFrame       = False                         ; do not advance frame
        res@cnFillOn       = True                          ; turn on color fill
        res@cnFillPalette  = "WhiteBlueGreenYellowRed"
        res@cnLevelSelectionMode	= "ManualLevels"        ; set manual contour levels
        res@cnMinLevelValF = 0.0                           ; set min contour level
        res@cnMaxLevelValF = 2.1                           ; set max contour level
        res@cnLevelSpacingF= 0.15                          ; set contour spacing

        res@cnFillMode     = "RasterFill"                  ; Raster Mode
        res@cnLinesOn      = False                         ; turn off contour lines
        res@lbLabelBarOn   = False                         ; no individual label bars

        res@gsnLeftString  = varname(i)

        plot(i)    = gsn_csm_contour_map(wks, newval, res)
     end if
  end do

  ;*****************************************************************
  dims	= dimsizes(AOTL)
  lat1d	= ndtooned(conform_dims(dims,newlat,0))
  lon1d	= ndtooned(conform_dims(dims,newlon,1))
  AOTL1d	= ndtooned(AOTL)
  AOTO1d	= ndtooned(AOTO)

  ;printVarSummary(AAEL)
  ;printVarSummary(lat1d)

  field_names	= (/lat@long_name, lon@long_name, varname(0), varname(1)/)
  header	= [/str_join(field_names,",")/]

  fn_csv	= csv_file
  system("rm -rf "+fn_csv)
  write_table(fn_csv, "w", header, "%s")

  alist	= [/lat1d, lon1d, AOTL1d, AOTO1d/]
  format	="%g, %g, %g, %g"
  write_table(fn_csv, "a", alist, format)

;*****************************************************************
; Draw the panel and label bars
;*****************************************************************
  if (PLOT) then
     resP                       = True
     resP@gsnFrame              = False
     resP@gsnMaximize           = True
     resP@gsnPanelBottom        = 0.045
     resP@gsnPanelLabelBar      = True
     resP@gsnPanelMainString    = "SGLI Aerosol Jan 2020"

     gsn_panel(wks,plot,(/2,2/),resP)

     frame(wks)
  end if

end
