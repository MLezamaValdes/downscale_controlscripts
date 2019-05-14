library(MODIS)

#EarthdataLogin() # urs.earthdata.nasa.gov download MODIS data from LP DAAC

lap = "D:/MODIS_MDV/MOD_package/"

MODISoptions(lap, outDirPath = file.path(lap, "PROCESSED")
             , MODISserverOrder = c("LPDAAC", "LAADS"), quiet = TRUE,
             gdalPath='c:/OSGeo4W64/bin')

### download data
getTile()

getHdf("MOD11_L2",begin = "2018.01.19", end = "2018.01.19",
       extent=getTile()) 

### process data (extract NDVI only)
runGdal(Job="testJob","MOD13Q1",begin = "2016.01.01", end = "2016.03.01",
                tileH = 18:19, tileV = 4
        , SDSstring = "1000000000000000000000") 
