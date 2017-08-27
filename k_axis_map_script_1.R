# simple generic map for k-axis stations

# raadtools can be used for sourcing environmental data directly rather than loading from local files, but you have to be on the aad network (or using an raadtools rstudio server)
# library(raadtools)

# remainign packages should all be available on CRAN
library(graticule)
library(raster)
library(rworldxtra)
data(countriesHigh)
# library(rworldmap)
# data(countriesHigh)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)

# projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"


# plot extent
ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) # extent for zoomed map
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent
wc <- crop(countriesHigh, ras.ext)
wcp <-spTransform(wc, CRS(prj))
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))

# K-Axis stations
ks <- readRDS("k_axis_oceanog_summ.Rda")
coordinates(ks) <- c("longitude", "latidue")
projection(ks) <- "+proj=longlat +datum=WGS84"
ks <- spTransform(ks, CRS(prj))

# bathy
  # reading in using raadtools if available
  # etopo
  #cbathy<- crop(readtopo("etopo2"), ras.ext2)
  # or gebco
  #cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
  # or if file is present locally as raster tif
  cbathy <- raster("bathy.tif")
  # lines
  cbc   <- rasterToContour(cbathy, levels=c(-500,-1000,-2000,-3000, -6000))
  cbct  <- spTransform(cbc, CRS(prj))
  # for filled
  e <- extent(-5411853 , 6235554, -1577161,  1358628)
  pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))
  

# for filled bathy
add_bathy <- function()  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  plotcols <- gray.colors(9, alpha=0.6)
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}

# ice
  # reding in with raadtools if loaded
  # icejan <- readice("2016-01-15")
  # icenov <- readice("2015-11-15")
  # or if the files are present locally as raster tifs
  icejan <- raster("icejan.tif")
  icenov <- raster("icenov.tif")

pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4))
pin<- projectRaster(icenov, raster(e, crs = prj, res = 1e4))

# fronts
library(orsifronts)
ofp<- spTransform(orsifronts, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# if there are locations you want to show
# (note that the structure() call below just does the same thing as reading a table of locations from text/csv)
locs <- structure(list(Cruise = structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 
2L), .Label = c("", "V2", "V3"), class = "factor"), Station = structure(c(7L, 
3L, 4L, 9L, 6L, 2L, 8L, 5L), .Label = c("", "R23", "R29", "R37", 
"R8", "T11", "T20", "T29", "T40"), class = "factor"), Type = structure(c(3L, 
2L, 2L, 3L, 3L, 2L, 3L, 2L), .Label = c("", "R", "T"), class = "factor"), 
    Date.Time = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L), .Label = c("", 
    "2016-01-01T00:00", "2016-12-21T00:00"), class = "factor"), 
    Long = c(81.886667, 83.353333, 83.580267, 63.9278, 83.8392, 
    85.033367, 77.616784, 110.0646846), Lat = c(-59.921667, -57.93, 
    -60.319233, -66.17905, -64.266017, -61.933375, -61.283533, 
    -66.1313351), Depth = c(220L, 20L, 10L, 20L, 15L, 10L, 60L, 
    160L), Species = structure(c(3L, 4L, 4L, 2L, 2L, 3L, 2L, 
    5L), .Label = c("", "E. superba", "E. triacantha", "E. vallentini", 
    "E.crystallorophias"), class = "factor")), .Names = c("Cruise", 
"Station", "Type", "Date.Time", "Lon", "Lat", "Depth", "Species"
), row.names = c(NA, 8L), class = "data.frame")
# convert to spatial points df
coordinates(locs) <- c("Lon", "Lat")
# define projection as longlat
projection(locs) <- "+proj=longlat +datum=WGS84"
# reproject to map
locs <- spTransform(locs, CRS(prj))

# TODO:
# orsi fronts
# max/min ice extent
# subset to midoc (/RMT/CTD) stations
# add labelling

pdf("K_axis_stations_map.pdf", width=4.5, height=4)
op<- par(mar=rep(0,4), oma=rep(0.5,4))
plot(wcp, border=NA)

add_bathy() # filled bathy
plot(cbct, col="grey", add=T) # bathy contours
# ice and fronts
plot(ofp, add = TRUE, col="#053061", lty=3, lwd=2)
plot(rasterToContour(pij, lev = 15),add = TRUE, lty=1, col="#6BAED6", lwd=1.5)
plot(rasterToContour(pin, lev = 15),add = TRUE, lty=1, col="#6BAED6", lwd=1.5)

plot(wp, add=T, col="darkgrey", border=F)
plot(graticule(lons = xx, lats = yy,  proj = prj), add=T, lty=2, col="gray40")
lines(ks$longitude, ks$latidue)
# can add large points for midoc stations by un-commenting line below
#points(ks, pch=19, col="gray20")
g1labs <- graticule_labels(lons=c(150, 120,90,60), xline=180, yline=-50, proj=projection(prj))
text(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), pos=3, cex=0.8, col="gray30")
g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=60, yline=-50, proj=projection(prj))
text(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=1, cex=0.8,col="gray30")

# to add extra points for other locations (locs dataframe)
points(locs, col="blue", pch = 19)
# or subsets
points(locs[locs$Cruise=="V2",], pch = 19, col="red")
# if you want labels
#text(locs, labels=locs$Species, pos=2, font=3)

# can add labels... position will need teaking
#text(60,-61.75,"SACCF",cex=1,adj=0,srt=-20)
#text(60,-65.25,"SB",cex=cx,adj=0)

box()
par(op)
dev.off()
