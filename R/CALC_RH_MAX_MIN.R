# convert fortran code to R for thi hourly calcs

dewtab <- function(t, r) {
  # ESTIMATING DEWPOINT FROM MEAN DAILY TEMPERATURE (T) AND     
  # DIURNAL TEMPERATURE RANGE. (R)                               
  # THIS FUNCTION APPROXIMATES VALUES IN THE TABLE 1            
  # ON PAGE 413 . LINACRE E.T. (1977) 'A SIMPLE FORMULA FOR      
  # ESTIMATING EVAPORATION RATES IN VARIOUS CLIMATES. USING      
  # TEMPERATURE DATA ALONE.' AGRIC.METEOROL. 18:409-424         
  #  P.G.JONES CIAT 25-09-81   
  
  u = t - 6                                                             
  
  # Ed Linacre's table can't handle temperatures less than
  # 6 degrees. Do the most conservative thing
  if (t < 6) { d = t/2
  } elseif {(t < 0) 
    d = t }
  # NOTE dewpoint does not really become negative, but this
  # this fits with the function edevap where we want
  # the term 15.0*(T-DEWTAB(T,R) to become zero for negative
  # temperatures. Otherwise we get excessively negative
  # evaps.
  v = r-6                                                              
  d = 3 + 0.0677 * v * u                                                
  if (d/(4.518 - .3752*v) > 1) {
    d = 3 + (1.4954*v + (.5774 + (.5444+.0419*u)*v)*u - 6.047) / (u + 5.54)
  }  
  d = t-max(d,0.0) # copes with pathological diurnal temperate ranges                                                  
  
}

vapres <- function(t) {
  
  # P. G. Jones Waen Associates Ltd, Dolgellau 8 Jan 2015
  
  # 					from
  # Wright, J. M. 1997 Rawinsonde and Pibal observations. In Federal meteorological
  # handbook No. 3. FCM-H3-1997. Office of Federal Coordinator
  # for Meteorological Services and Supporting Research
  # (OFCM), Washington DC, USA
  #  Result in pascals NOT kPa
  
  v = 611.21 * exp(17.502 * t/(240.97 + t))
}

tmax <- xxx
tmin <- yyy
for (mth in  1:12) {
  # tmax = newc.temp(mth) + newc.diurn(mth)/2
  # tmin = newc.temp(mth) - newc.diurn(mth)/2
  td = dewtab(newc.temp(mth),newc.diurn(mth))
  im(col,row,mth,1) = min(1000.0*vapres(td)/vapres(tmax) , 1000.0)  # RHn
  im(col,row,mth,2) = min(1000.0*vapres(td)/vapres(tmin) , 1000.0)  # RHx
  im(col,row,mth,3) = tmin * 10.0
  im(col,row,mth,4) = tmax * 10.0
}

for ( itipo in 1:4) {
  for( mth in 1:12) {
    ix = 0
    in = 1000
    outfile(27:32) = tipo(itipo)
    outfile(39:40) = xonth(mth)
    # print *,outfile		
    # open (12,file=outfile(1:len_trim(outfile))//'.rst',form='binary')
    for (i in 1:1800)
      # write (12) im(1:4320,i,mth,itipo)
      
  }
}


for (i in 1:4320) {
  for (j in 1:1800) {
    if(im(i,j,mth,itipo).gt.ix) ix = im(i,j,mth,itipo)
    if(im(i,j,mth,itipo).lt.in) in = im(i,j,mth,itipo)
  }
}

# call fixup_rdc_file (outfile,in,ix,4320,1800)
# end do
# end do