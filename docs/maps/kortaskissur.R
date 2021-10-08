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
