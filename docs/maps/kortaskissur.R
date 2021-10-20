Ext=c(Xmin=2428893.845,
      Xmax=2789299.166,
      Ymin=221548.859,
      Ymax=476380.905)

testisland = raster::stack("docs/maps/NVumd.tif")
cropped_island = raster::crop(testisland, extent(Ext))



terrain = cropped_island
terrain %>% 
  matrix(nrow = ncol(.), ncol = nrow(.)) ->
  terrain
streams <- png::readPNG("docs/maps/utlinur.png")
land <- png::readPNG("docs/maps/Layout 1.png")

sun <- 90
rayshade <- ray_shade(terrain, sunangle = sun, lambert = TRUE, multicore = TRUE)
ambshade <- ambient_shade(terrain, multicore = TRUE)
terrain %>% 
  sphere_shade(sunangle = sun, texture = "desert") %>% 
  add_overlay(land) %>% 
  add_overlay(streams) %>%
  #add_shadow(rayshade, 0.7) %>% 
  #add_shadow(ambshade, 0.7) %>% 
  plot_3d(terrain, zscale = 10, solid = TRUE, theta = 225, phi = 30, zoom = 0.7)


(terrain/255) %>% 
  #add_overlay(land) %>% 
  #add_overlay(streams) %>%
  #add_shadow(ray_layer,0.3) %>%
  #add_shadow(ambient_layer,0) %>%
  plot_map()





herepath <- "C:/Users/valty/Documents/vinna/nnv/vöktun/2021"
image_files <- list.files(herepath,pattern = "JPEG|JPG", recursive = T,full.names = T) %>% 
  read_exif(tags = "GPSPosition") %>% 
  drop_na() %>% 
  separate(GPSPosition, into = c("lat", "lon"), sep = "\\s") %>% 
  mutate(lat=as.numeric(lat), lon=as.numeric(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"))

xy <- st_coordinates(image_files) %>% as.data.frame() %>%
    st_as_sf(coords = c("X", "Y")) %>%
    lwgeom::st_transform_proj(crs = "+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs") %>% 
  st_coordinates() %>% as.data.frame() -> xy
  #st_set_crs("+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs")
  
xy %>% as.matrix() %>% 
  rgdal::project("+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs") %>% 
  as.data.frame() -> xy

xy %>% 
  #as.matrix() %>% 
  #rgdal::project("+proj=lcc +lat_0=65 +lon_0=-19 +lat_1=64.25 +lat_2=65.75 +x_0=2700000 +y_0=300000 +ellps=GRS80 +units=m +no_defs") %>% 
  magrittr::subtract(extent(terrain) %>% 
             as.matrix() %>% 
             t() %>% 
             magrittr::extract(rep(1, nrow(xy)),)) %>% 
  magrittr::divide_by(res(raster1)[1]) %>% # assumes square cells
  round() %>% 
  as_tibble() %>% 
  transmute(x = X, y = Y) %>% 
  bind_cols(xy) ->
  sites

render_label(water_island, x = , y = unname(xy[1,2]) , z = 1000, zscale = 50,
             text = "V", textsize = 2, linewidth = 5)

xy %>%  
  #st_as_sf(coords = 1:2, crs = 8088) %>% 
  dplyr::select(X, Y) %>% 
  pmap(function(X, Y, terrain) {
    render_label(terrain, "", x, y, z = 3400, zscale = 30, 
                 text = "V",relativez = TRUE)
  }, terrain = terrain) %>% 
  invisible()
