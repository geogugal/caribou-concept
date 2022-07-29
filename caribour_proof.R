#'---
#'author: Jugal Patel
#'date: 08/2019
#'title: From BC to Baffin, common movement across Caribou sub-species 
#'---

#'The following script demonstrates segmentation on trajectories of a herd of South Peace Northern Caribou. A subspecies that is distinct from caribou found on Baffin Island
#'---

# Directory ----
#setwd("/lustre03/project/6004645/jugal/papio/open/4")
setwd("C:/Users/jugal/Dropbox/Coursework/Geography of Nunavut (GEOG 301)")

# Generate artificial DEM ----
library(raster)

# Create 100x100 random raster with a Z range of 500-1500
DEM <- raster(ncols=100, nrows=100, xmn=0)
DEM[] <- runif(ncell(DEM), min=500, max=1500)  

Water <- raster(ncols=100, nrows=100, xmn=0)
Water[] <- runif(ncell(Water), min=500, max=1500) 

# Function for Gaussian kernal smoothing -- we need this to make our fabricated environmental features to be 'believable'
GaussianKernel <- function(sigma=s, n=d) {
  m <- matrix(nc=n, nr=n)
  col <- rep(1:n, n)
  row <- rep(1:n, each=n)
  x <- col - ceiling(n/2)
  y <- row - ceiling(n/2)
  m[cbind(row, col)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
  m / sum(m)
}

# Create autocorrelated raster using 9x9 Gaussian Kernel with a sigma of 1
DEM.sim <- focal(DEM, w=GaussianKernel(sigma=1, n=9)) # raster to represent elevation 
Water.sim <- focal(Water, w=GaussianKernel(sigma=1, n=9)) # raster to represent distance to water

# Plot results
#par(mfcol=c(1,2))
plot(DEM, main="Random Raster")
plot(DEM.sim, main="Autocorrelated Raster (sigma=1, n=9)") #will use this raster to predict on later
plot(Water.sim, main="Autocorrelated Raster (water; sigma=1, n=9)")

# Import: movement data ----
spn_herd <- read.csv("spnCaribou.csv")
detour_herd <- read.csv("detour_herd.csv") 

# Fabricate data ----
# South Peace Northern Herd
dist_water <- runif(245459, min=10, max=300)
elevation <- runif(245459, min=800, max=1200)

# Detour herd 
dist_water_detour <- runif(1119, min=10, max=300)
elevation_detour <- runif(1119, min=800, max=1200)

# Clean data ----
#South Peace North Herd
spn_herd$OID <- spn_herd$event.id
spn_herd$longitude <- spn_herd$location.long
spn_herd$latitude <- spn_herd$location.lat
spn_herd$ID <- spn_herd$comments
spn_herd$dist_water <- dist_water
spn_herd$elevation <- elevation

spn_herd <- subset(spn_herd, select = c("OID",
                                        "longitude",
                                        "latitude",
                                        "dist_water",
                                        "elevation",
                                        "ID"))

#Detour Herd -- omitted from final modelling for now 
detour_herd$timestamp <- as.POSIXct(paste(detour_herd$DATE, detour_herd$TIME), format="%Y-%m-%d %H:%M:%S")
detour_herd$longitude <- detour_herd$LONGITUDE
detour_herd$latitude <- detour_herd$LATITUDE
detour_herd$dist_water <- dist_water_detour
detour_herd$elevation <- elevation_detour

detour_herd <- subset(detour_herd, select = c("longitude", 
                                              "latitude", 
                                              "dist_water",
                                              "elevation",
                                              "ID",
                                              "timestamp"))

# Incorporate Inuit hunting points ----
spn_herd$deployID <- 1
spn_herd$deployID[sample(nrow(spn_herd), 122729)] <- 0

detour_herd$deployID <- 1
detour_herd$deployID[sample(nrow(detour_herd), 559)] <- 0

# Plot SPN herd ----
plot(spn_herd$longitude, 
     spn_herd$latitude,
     xlab = "Longitude (not projected)",
     ylab = "Latitude (not projected)",
     main = "Caribou locations; hunted in red",
     col=factor(spn_herd$deployID))

# Plot Detour herd ----

# Prepare for scikit-learn ----
spn_herd_class <- subset(detour_herd, select = c("deployID",
                                                 "dist_water",
                                                 "elevation",
                                                 "ID"))

write.csv(spn_herd, "spn_class.csv")

detour_herd <- subset(detour_herd, select = c("deployID", 
                                              "dist_water", 
                                              "elevation", 
                                              "ID"))

write.csv(detour_herd, "detour_class.csv")

# Rasterize decision boundaries ---- 
myFun <- function(x) {ifelse(x > val, -1*(1-gini), 1*(1-gini))}

# Root node
# Inputs from classification tree 
gini <- 0.5

# Inputs for raster calculation 
x <- DEM.sim
val <- 1095.5

node_0 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_0)

# Child node 1
# Inputs from classification tree 
gini <- 0.5

# Inputs for raster calculation 
x <- DEM.sim
val <- 1087.173

node_1 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_1)

# Child node 2
# Inputs from classification tree 
gini <- 0.5

# Inputs for raster calculation 
x <- water.sim
val <- 295.084

node_2 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_2)

# Child node 3
# Inputs from classification tree 
gini <- 0.5

# Inputs for raster calculation 
x <- water.sim
val <- 290.895

node_3 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_3)

# Child node 4
# Inputs from classification tree 
gini <- 0.5

# Inputs for raster calculation 
x <- DEM.sim
val <- 1082.369

node_4 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_4)

# Child node 5
# Inputs from classification tree 
gini <- 0.278

# Inputs for raster calculation 
x <- DEM.sim
val <- 989.27

node_5 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_5)

# Child node 6
# Inputs from classification tree 
gini <- 0.48

# Inputs for raster calculation 
x <- DEM.sim
val <- 888.429

node_6 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_6)

# Child node 7
# Inputs from classification tree 
gini <- 0.32

# Inputs for raster calculation 
x <- water.sim
val <- 91.136

node_7 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_7)

# Child node 8
# Inputs from classification tree 
gini <- 0.142

# Inputs for raster calculation 
x <- DEM.sim
val <- 1087.836

node_8 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_8)

# Child node 9
# Inputs from classification tree 
gini <- 0.444

# Inputs for raster calculation 
x <- DEM.sim
val <- 1087.544

node_9 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_9)

# Child node 10
# Inputs from classification tree 
gini <- 0.493

# Inputs for raster calculation 
x <- water.sim
val <- 38.328

node_10 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_10)

# Child node 11
# Inputs from classification tree 
gini <- 0.384

# Inputs for raster calculation 
x <- DEM.sim
val <- 1129.906

node_11 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_11)

# Child node 12
# Inputs from classification tree 
gini <- 0.133

# Inputs for raster calculation 
x <- water.sim
val <- 22.17

node_12 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_12)

# Child node 13
# Inputs from classification tree 
gini <- 0.497

# Inputs for raster calculation 
x <- water.sim
val <- 15.762

node_13 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_13)

# Child node 14
# Inputs from classification tree 
gini <- 0.245

# Inputs for raster calculation 
x <- water.sim
val <- 21.817

node_14 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_14)

# Child node 15
# Inputs from classification tree 
gini <- 0.463

# Inputs for raster calculation 
x <- water.sim
val <- 22.61

node_15 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_15)

# Child node 16
# Inputs from classification tree 
gini <- 0.497

# Inputs for raster calculation 
x <- DEM.sim
val <- 1095.861

node_16 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_16)

# Child node 17
# Inputs from classification tree 
gini <- 0.32

# Inputs for raster calculation 
x <- DEM.sim
val <- 1092.812

node_17 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_17)

# Child node 18
# Inputs from classification tree 
gini <- 0.444

# Inputs for raster calculation 
x <- water.sim
val <- 177.433

node_18 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_18)

# Child node 19
# Inputs from classification tree 
gini <- 0.494

# Inputs for raster calculation 
x <- water.sim
val <- 71.169

node_19 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_19)

# Child node 20
# Inputs from classification tree 
gini <- 0.463

# Inputs for raster calculation 
x <- water.sim
val <- 64.833

node_20 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_20)

# Child node 21
# Inputs from classification tree 
gini <- 0.485

# Inputs for raster calculation 
x <- water.sim
val <- 294.612

node_21 <- overlay((x), fun = myFun)

# Visual checks
image(x)
image(node_21)

# Stack rasters to create a behaviour selection surface
r_stack <- stack(node_0, 
                 node_1,
                 node_2, 
                 node_3,
                 node_4,
                 node_5,
                 node_6,
                 node_7,
                 node_8,
                 node_9,
                 node_10,
                 node_11,
                 node_12,
                 node_13,
                 node_14,
                 node_15,
                 node_16,
                 node_17,
                 node_18,
                 node_19,
                 node_20,
                 node_21)

s_nodes <- sum(r_stack)
image(s_nodes)

# Normalize - since we want to directly compare this with output from the sedentary classifier 
rasterRescale <- function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

ss_nodes <- rasterRescale(s_nodes)
image(ss_nodes)

writeRaster(ss_nodes, 
            filename = "huntingselection",
            "GTiff", 
            overwrite = T)

r_huntingselection <- raster("huntingselection.tif") #movement selection
image(r_huntingselection, 
      xlab = "x (coloumns; not projected)", 
      ylab = "y (rows; not projected)",
      col = hcl.colors(100, palette = "Greens 2", rev = T),
      main = "Where the Caribou offer themselves: Rangifer behaviour selection")

selection_pdf <- image(r_huntingselection, 
                       xlab = "x (coloumns; not projected)", 
                       ylab = "y (rows; not projected)",
                       col = hcl.colors(100, palette = "Greens 2", rev = T),
                       main = "Where the Caribou offer themselves: Rangifer behaviour selection")

pdf("huntingselection.pdf")
image(r_huntingselection, 
      xlab = "x (coloumns; not projected)", 
      ylab = "y (rows; not projected)",
      col = hcl.colors(100, palette = "Greens 2", rev = T),
      main = "Where the Caribou offer themselves: Rangifer behaviour selection")
dev.off()

image(Water.sim)