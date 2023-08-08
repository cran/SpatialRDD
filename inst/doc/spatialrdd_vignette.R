## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = F
)
# change options for readable regression output (changed back at the end of vignette!)
old <- options("scipen" = 100, "digits" = 2)
# write this vignette with ctrl shift D, that builds all the vignettes

## ---- message = FALSE, warning = FALSE----------------------------------------
library(SpatialRDD)
library(dplyr) # more intuitive data wrangling
library(stargazer) # easy way to make model output look more appealing (R-inline, html, or latex)
library(sf)

## ---- eval = FALSE------------------------------------------------------------
#  st_geometry(any.sf.object) <- NULL

## ---- eval = FALSE------------------------------------------------------------
#  st_set_geometry(any.sf.object, NULL)

## ---- eval = FALSE------------------------------------------------------------
#  mydata.sf <- st_read("path/to/file.shp")

## ---- eval = FALSE------------------------------------------------------------
#  mydata.sf <- st_as_sf(loaded_file, coords = c("longitude", "latitude"), crs = projcrs)
#  # just the EPSG as an integer or a proj4string of the desired CRS

## -----------------------------------------------------------------------------
data(cut_off, polygon_full, polygon_treated)
library(tmap)
tm_shape(polygon_full) + tm_polygons() + 
  tm_shape(polygon_treated) + tm_polygons(col = "grey") + 
  tm_shape(cut_off) + tm_lines(col = "red")


## -----------------------------------------------------------------------------
set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full, 1000)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation
# visualise results together with the line that represents our RDD cutoff
tm_shape(points_samp.sf) + tm_dots() + tm_shape(cut_off) + tm_lines(col = "red")

## ---- warning = FALSE---------------------------------------------------------
points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
tm_shape(points_samp.sf) + tm_dots("treated", palette = "Set1") + tm_shape(cut_off) + tm_lines(col = "red")

## ---- warning = FALSE---------------------------------------------------------
# first we define a variable for the number of "treated" and control which makes the code more readable in the future
NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
# the treated areas get a 10 percentage point higher literacy rate
points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
# and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
# we draw from a normal with mean 0 and a standard devation of 0.1
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 1]
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 0]

# let's also add a placebo outcome that has no jump
points_samp.sf$placebo <- rnorm(nrow(points_samp.sf), mean = 1, sd = .25)

# visualisation split up by groups
library(ggplot2)
ggplot(points_samp.sf, aes(x = education)) + geom_histogram(binwidth = .01) + facet_grid(treated ~ .)

## -----------------------------------------------------------------------------
list(lm(education ~ treated, data = points_samp.sf),
     lm(placebo   ~ treated, data = points_samp.sf)) %>% stargazer::stargazer(type = "text")

## -----------------------------------------------------------------------------
points_samp.sf$dist2cutoff <- as.numeric(sf::st_distance(points_samp.sf, cut_off))

## ---- warning = FALSE, message = FALSE----------------------------------------
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) + 
  tm_shape(cut_off) + tm_lines()


## -----------------------------------------------------------------------------
lm(education ~ treated, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) %>% stargazer::stargazer(type = "text")

## -----------------------------------------------------------------------------
points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
# give the non-treated one's a negative score
points_samp.sf$distrunning[points_samp.sf$treated == 0] <- -1 * points_samp.sf$distrunning[points_samp.sf$treated == 0]
ggplot(data = points_samp.sf, aes(x = distrunning, y = education)) + geom_point() + geom_vline(xintercept = 0, col = "red")

## -----------------------------------------------------------------------------
library(rdrobust)
summary(rdrobust(points_samp.sf$education, points_samp.sf$distrunning, c = 0))

## ---- out.width='\\textwidth', fig.width = 7, fig.height = 5------------------
rdplot(points_samp.sf$education, points_samp.sf$distrunning, c = 0, ci = 95, 
       kernel = "triangular", y.label = "education", x.label = "distance to border")

## ---- eval = FALSE------------------------------------------------------------
#  library(rddapp)
#  summary(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"))

## ---- out.width='\\textwidth', fig.width = 7, fig.height = 5, eval = FALSE----
#  plot(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"), fit_line = c("quadratic", "optimal"), bin_n = 50)

## ---- fig.show='hold'---------------------------------------------------------
points_samp.sf$segment10 <- border_segment(points_samp.sf, cut_off, 10)
points_samp.sf$segment15 <- border_segment(points_samp.sf, cut_off, 15)
tm_shape(points_samp.sf) + tm_dots("segment10", size = 0.1) + tm_shape(cut_off) + tm_lines()
tm_shape(points_samp.sf) + tm_dots("segment15", size = 0.1) + tm_shape(cut_off) + tm_lines()

## -----------------------------------------------------------------------------
points_samp.sf$segment5 <- border_segment(points_samp.sf, cut_off, 5)
tm_shape(points_samp.sf) + tm_dots("segment5", size = 0.1) + tm_shape(cut_off) + tm_lines()

## -----------------------------------------------------------------------------
library(lfe)
list(lfe::felm(education ~ treated | factor(segment15) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]),
lfe::felm(education ~ treated | factor(segment5) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ])
) %>% stargazer::stargazer(type = "text")


## -----------------------------------------------------------------------------
borderpoints.sf <- discretise_border(cutoff = cut_off, n = 50)
borderpoints.sf$id <- 1:nrow(borderpoints.sf)
# exclude some of the borderpoints with little n so that the vignette can compile without warning:
#borderpoints.sf <- borderpoints.sf %>% slice(c(5,10,20,30,40))
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) +
  tm_shape(cut_off) + tm_lines() +
  tm_shape(borderpoints.sf) + tm_symbols(shape = 4, size = .3)

## ---- warning = FALSE---------------------------------------------------------
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10, spatial.object = F)
knitr::kable(results)

## ---- warning = FALSE, fig.width = 7, fig.height = 5--------------------------
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10)
plotspatialrd(results, map = T)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
plotspatialrd(results, map = F)

## -----------------------------------------------------------------------------
placebocut_off.1 <- shift_border(cut_off, operation = "shift", shift = c(3000, 3000))
placeboborderpoints.1 <- discretise_border(cutoff = placebocut_off.1, n = 50)
tm_shape(points_samp.sf) + tm_dots("treated", palette = "Set1")  + tm_shape(placeboborderpoints.1) + tm_symbols(shape = 4, size = .3) + tm_shape(placebocut_off.1) + tm_lines()

## -----------------------------------------------------------------------------
placebo.poly.1 <- cutoff2polygon(data = points_samp.sf, cutoff = placebocut_off.1, orientation = c("west", "west"), endpoints = c(.8, .2))

tm_shape(placebo.poly.1) + tm_polygons(alpha = .3)


## -----------------------------------------------------------------------------
points_samp.sf$treated1 <- assign_treated(data = points_samp.sf, polygon = placebo.poly.1, id = "id")
sum(points_samp.sf$treated == 0 & points_samp.sf$treated1 == 1) # number of villages that switched treatment status
tm_shape(points_samp.sf) + tm_dots("treated1", palette = "Set1")  + tm_shape(placeboborderpoints.1) + tm_symbols(shape = 4, size = .3) + tm_shape(placebocut_off.1) + tm_lines()

## ---- warning = FALSE, fig.width = 7, fig.height = 5--------------------------
results1 <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = placeboborderpoints.1, treated = "treated1", minobs = 10)
plotspatialrd(results1, map = T)

## -----------------------------------------------------------------------------
points_samp.sf$segment.1.5 <- border_segment(points_samp.sf, placebocut_off.1, 5) # assigning new segments based on now cutoff
points_samp.sf$dist2cutoff1 <- as.numeric(sf::st_distance(points_samp.sf, placebocut_off.1)) # recompute distance to new placebo cutoff

list(
  lm(education ~ treated1, data = points_samp.sf[points_samp.sf$dist2cutoff1 < 3000, ]),
  lfe::felm(education ~ treated1 | factor(segment.1.5) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff1 < 3000, ])
) %>% stargazer::stargazer(type = "text")

## ---- include = FALSE---------------------------------------------------------
# change back options
options(old)

