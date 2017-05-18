# simple generic map for k-axis stations

library(raadtools)
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
ras.ext   <- raster(xmn=60, xmx=110, ymn=-70, ymx=-40) # extent for zoomed map
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



# bathy lines 
cbathy<- crop(readtopo("etopo2"), ras.ext2)
#cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
cbc   <- rasterToContour(cbathy, levels=c(-500,-1000,-2000,-3000, -6000))
cbct  <- spTransform(cbc, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# TODO:
# subset to midoc (/RMT/CTD) stations
# add labelling

#pdf("K_axis_stations_map.pdf", width=6, height=4)
#op<- par(mar=rep(0,4), oma=rep(0.5,4))
plot(wcp, border=NA)
plot(cbct, col="grey", add=T)
plot(wp, add=T, col="darkgrey", border=F)
plot(graticule(lons = xx, lats = yy,  proj = prj), add=T, lty=2, col="gray40")
lines(ks$longitude, ks$latidue)
points(ks, pch=19, col="gray20")
g1labs <- graticule_labels(lons=c(150, 120,90,60), xline=180, yline=-50, proj=projection(prj))
text(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), pos=3, cex=0.8, col="gray30")
g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=60, yline=-50, proj=projection(prj))
text(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=1, cex=0.8,col="gray30")
#par(op)
#dev.off()