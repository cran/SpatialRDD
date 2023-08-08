## ---- include = FALSE---------------------------------------------------------
# create vignettes with usethis::use_vignette("my-vignette")
# browse vignettes with browseVignettes()
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(SpatialRDD); data(cut_off, polygon_full, polygon_treated)
library(tmap)
set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full, 1000)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation

## ---- message = FALSE---------------------------------------------------------

tm_rotate.sf10 <- shift_border(border = cut_off, operation = "rotate", angle = 10)
tm_rotate.sf25 <- shift_border(border = cut_off, operation = "rotate", angle = 25)
tm_rotate.sf45 <- shift_border(border = cut_off, operation = "rotate", angle = 45)

tm_shape(polygon_full) + tm_polygons() + tm_shape(cut_off) + tm_lines() + 
  tm_shape(tm_rotate.sf10) + tm_lines(col = "red") + 
  tm_shape(tm_rotate.sf25) + tm_lines(col = "red") + 
  tm_shape(tm_rotate.sf45) + tm_lines(col = "red")


## ---- message = FALSE---------------------------------------------------------

tm_scale.sf.4 <- shift_border(border = cut_off, operation = "scale", scale = .4)
tm_scale.sf.7 <- shift_border(border = cut_off, operation = "scale", scale = .7)
tm_scale.sf1.5 <- shift_border(border = cut_off, operation = "scale", scale = 1.5)

tm_shape(polygon_full) + tm_polygons() + tm_shape(cut_off) + tm_lines() + 
  tm_shape(tm_scale.sf.4) + tm_lines(col = "blue") + 
  tm_shape(tm_scale.sf.7) + tm_lines(col = "red") + 
  tm_shape(tm_scale.sf1.5) + tm_lines(col = "red")


## ---- fig.show='hold', message = FALSE----------------------------------------

tm_shift.sf3 <- shift_border(border = cut_off, operation = "shift", shift = c(3000, 0))
tm_shift.sf6 <- shift_border(border = cut_off, operation = "shift", shift = c(6000, 0))
tm_shift.sf_4 <- shift_border(border = cut_off, operation = "shift", shift = c(-4000, 0))


tm_shape(polygon_full) + tm_polygons() + tm_shape(cut_off) + tm_lines() + 
  tm_shape(tm_shift.sf3) + tm_lines(col = "red") + 
  tm_shape(tm_shift.sf6) + tm_lines(col = "red") + 
  tm_shape(tm_shift.sf_4) + tm_lines(col = "blue")  


tm_shift.sf_42 <- shift_border(border = cut_off, operation = "shift", shift = c(-4000, -2000))
tm_shift.sf_44 <- shift_border(border = cut_off, operation = "shift", shift = c(-4000, -4000))

tm_shape(polygon_full) + tm_polygons() + tm_shape(cut_off) + tm_lines() + 
  tm_shape(tm_shift.sf_42) + tm_lines(col = "red") + 
  tm_shape(tm_shift.sf_44) + tm_lines(col = "red") + 
  tm_shape(tm_shift.sf_4) + tm_lines(col = "blue")  

## -----------------------------------------------------------------------------

tm_placebo.sf1 <- shift_border(border = cut_off, operation = c("shift", "scale"), shift = c(-5000, -3000), scale = .85)
tm_placebo.sf2 <- shift_border(border = cut_off, operation = c("shift", "scale"), shift = c(4000, 2000), scale = 1.1)
tm_placebo.sf3 <- shift_border(border = cut_off, operation = c("shift", "scale"), shift = c(6000, 3000), scale = 1.2)
tm_shape(polygon_full) + tm_polygons() + tm_shape(cut_off) + tm_lines() + 
  tm_shape(tm_placebo.sf1) + tm_lines(col = "red") +
  tm_shape(tm_placebo.sf2) + tm_lines(col = "red") +
  tm_shape(tm_placebo.sf3) + tm_lines(col = "red")

tm_shift.sf <- shift_border(border = cut_off, operation = c("shift", "rotate", "scale"), 
                              shift = c(-10000, -1000), angle = 0, scale = .9)
tm_shape(cut_off) + tm_lines() + tm_shape(tm_shift.sf) + tm_lines(col = "red")


## -----------------------------------------------------------------------------
polygon1 <- cutoff2polygon(data = points_samp.sf, cutoff = tm_placebo.sf1, orientation = c("west", "west"), endpoints = c(.8, .2) # corners = 0,
                           #     crs = 32643
                           )
polygon2 <- cutoff2polygon(data = points_samp.sf, cutoff = tm_placebo.sf2, orientation = c("west", "west"), endpoints = c(.8, .2) # corners = 0,
                           #     crs = 32643
                           )
polygon3 <- cutoff2polygon(data = points_samp.sf, cutoff = tm_placebo.sf3, orientation = c("west", "west"), endpoints = c(.8, .2) # corners = 0,
                           #     crs = 32643
                           )

tm_shape(polygon_full) + tm_polygons() + 
  tm_shape(polygon_treated) + tm_polygons(col = "grey") + 
  tm_shape(cut_off) + tm_lines(col = "red") +
  tm_shape(polygon1) + tm_polygons(alpha = .3) +
  tm_shape(polygon2) + tm_polygons(alpha = .3) +
  tm_shape(polygon3) + tm_polygons(alpha = .3) 


