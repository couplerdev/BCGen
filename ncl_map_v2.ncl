load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   f = addfile("./my_case.cpl.hi.2001-01-02-00000.nc", "r")
   var = f->x2lnd_Sa_v_00000003(0,:,:)
   var!0 = "lat"
   var!1 = "lon"
   var&lat = f->domlnd_lat_00000001(0,:,0)
   var&lon = f->domlnd_lon_00000002(0,0,:)
   var&lat@units = "degrees north"
   var&lon@units = "degrees east"
   print(var&lon)

   wks = gsn_open_wks("png", "test")
  
   res                         = True
   res@gsnAddCyclic = False

   res@tiMainString            = "test the var"

   plot = gsn_csm_contour_map(wks,var, res)

end
