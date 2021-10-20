Packages <- c("exifr", "tidyr", "sf", "tidyverse", "mapview", "leafpop", "htmlwidgets", "leafem", "leaflet.extras", "lwgeom")
pacman::p_load(Packages, character.only = TRUE)


umd <- sf::st_read("C:/Users/valty/Documents/vinna/GIS/lmi/IS50V/IS_50V_MORK_170062020/IS_50V_MORK_170062020.gpkg",layer="mork_umdaemi_flakar")
nvumd <- umd[umd$umdaemisyslumanns=="Umdæmi sýslumannsins á Norðurlandi vestra",]
Island <- sf::st_read("docs/maps/iceland.gpkg")



dirs <- list.dirs("C:/Users/valty/Documents/vinna/nnv/vöktun/2021")
image_files <- list.files("C:/Users/valty/Documents/vinna/nnv/vöktun/2021", full.names = TRUE,recursive = T, pattern = ".JPG|.JPEG|.tif")

punktar <- read_exif(image_files,tags = "GPSPosition")
punktarB <- separate(punktar, GPSPosition, into = c("lat", "lon"), sep = "\\s") %>% mutate(lat=as.numeric(lat), lon=as.numeric(lon))
punktarB <- punktarB[!is.na(punktarB$lat) & !is.na(punktarB$lon),]
punktarC <- st_as_sf(punktarB, coords = c("lon", "lat"), crs = 'WGS84')


mapview(nvumd,col.regions="#cb5600",map.types="Stamen.TerrainBackground", legend = FALSE)+mapview(punktarC,legend=F)
map <- mapview(nvumd,col.regions="#cb5600",map.types="Stamen.TerrainBackground", legend = FALSE)+mapview(punktarC,legend=F,popup = leafpop::popupImage(image_files))
img <- "images/nnvlogo.png"
map %>% leafem::addLogo(img, width = '20%', height = '25%',offset.y = 20,offset.x = 80,alpha = 0.7) %>% leaflet.extras::addFullscreenControl(pseudoFullscreen = T)

#tif
rel <- raster("docs/maps/Land.tif")
rel_spdf <- as(rel, "SpatialPixelsDataFrame")
rel <- as.data.frame(rel_spdf)

#png
ping <- brick("docs/maps/Land_georeferenced.png")


ggplot() +
  geom_sf(data = Island, aes(fill=objectid),colour=NA) +
  geom_sf(data = nvumd ) +
  geom_raster(data = rel,aes_string(x = "x", y = "y", alpha = "Land")) +
  scale_alpha(name = "", range = c(0.6, 0), guide = F) 







library(rayshader)

#Here, I load a map with the raster package.

localtif = raster::stack('docs/maps/Land.tif')


#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()














Ext=c(Xmin=2428893.845,
      Xmax=2789299.166,
      Ymin=221548.859,
      Ymax=476380.905)

library(raster)
library(rayshader)
#Load QGIS georeference image (see https://www.qgistutorials.com/en/docs/3/georeferencing_basics.html)
streams <- png::readPNG("docs/maps/utlinur.png")
testisland = raster::stack("docs/maps/Layout 1.png")
#morkin = sf::st_read("docs/maps/morkin.shp")#%>% rgdal::project('+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs')
#Set bounding box for final map (cut off edges without data, introduced via reprojection)
#island_bb = raster::extent(testisland)
cropped_island = raster::crop(testisland, extent(Ext))

#Convert to RGB array
island_array = as.array(testisland)

#Load elevation data, sourced from GEBCO
raster1 = raster::raster("docs/maps/NVumd.tif")

#Reproject and crop elevation data to historical map coordinate system
#reprojected_island = raster::projectRaster(raster1, crs=raster::crs(testisland))
#cropped_reprojected_island = raster::crop(reprojected_island,island_bb)

#Reduce the size of the elevation data, for speed
#small_island_matrix = resize_matrix(as.matrix(cropped_reprojected_island), scale = 0.4)
small_island_matrix = resize_matrix(as.matrix(raster1), scale = 0.4)
#Remove bathymetry data
water_island = small_island_matrix
water_island[is.na(water_island)] = 0
water_island[water_island < 0]=0
water_island = t(small_island_matrix)

#Compute shadows
ambient_layer = ambient_shade(water_island, zscale = 10, multicore = TRUE, maxsearch = 200)
ray_layer = ray_shade(water_island, zscale = 20, multicore = TRUE)

#Plot in 3D
(island_array/255) %>%
  add_overlay(streams) %>%
  #add_shadow(ray_layer,0.3) %>%
  #add_shadow(ambient_layer,0) %>%
  plot_3d(water_island, zscale = 100, fov = 0, theta = 0, phi = 90, windowsize = c(1000, 800), zoom = .6,baseshape = "circle")

#Render snapshot with depth of field
render_depth(focus=0.982,focallength = 4000)


morkin = sf::st_read("docs/maps/umd.shp")
#Plot in 2D
(island_array/255) %>%
  add_overlay(streams) %>%
  #add_shadow(ray_layer,0.3) %>%
  #add_shadow(ambient_layer,0) %>%
  plot_map()

# tjekka á labels
#render_label(montereybay, x = 350, y = 160, z = 1000, zscale = 50,
#             text = "Moss Landing", textsize = 2, linewidth = 5)




herepath <- "C:/Users/valty/Documents/vinna/nnv/vöktun/2021"
image_files <- list.files(herepath,pattern = "JPEG|JPG", recursive = T,full.names = T) %>% 
  read_exif(tags = "GPSPosition") %>% 
  drop_na() %>% 
  separate(GPSPosition, into = c("lat", "lon"), sep = "\\s") %>% 
  mutate(lat=as.numeric(lat), lon=as.numeric(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = '+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs')
xy <- st_coordinates(image_files)

render_points(extent = attr(water_island,"extent"),
              lat = xy[,1], long = xy[,2],
              altitude = z_out, zscale=50, color="red")
render_highquality(point_radius = 1,sample_method="stratified")

