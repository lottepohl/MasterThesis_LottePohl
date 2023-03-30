library(sf)


bathy_contours$geom[[1]]


st_cast(bathy_contours$geom[1], "POLYGON")
