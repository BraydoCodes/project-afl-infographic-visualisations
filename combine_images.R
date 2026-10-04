library(imager)
# prerequistite for this file to work requires afl_ladder_table.R and by_the_numbers.R to be in memory 

l_t <- load.image(table_image_location)

# only currently supports windows
vis_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", vis_location)

v_t <- load.image(vis_path)

a_t <- imlist(l_t, v_t) 
plot(a_t, axes=FALSE) # currently manually export a pdf in portrait