#devtools::install_github("ryantimpe/brickr")
library(brickr)
library(rayshader)
library(jpeg)

thing<-jpeg::readJPEG("MeadowLake.jpg") %>% 
  image_to_bricks(img_size = 125) 
thing %>% display_set()

thing %>% collect_3d() %>% 
  display_3d(fov=0,theta=-20,phi=30,windowsize=c(1000,800),zoom=0.7)

render_snapshot()

