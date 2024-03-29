% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_datasets.R
\docType{data}
\name{study_region_ex}
\alias{study_region_ex}
\title{Study region for example data}
\format{
A \code{sf} spatial features polygon
}
\usage{
study_region_ex
}
\description{
Boundary region for small example data set defined in terms of (x,y) vertices of a polygon.
}
\examples{
library(ggplot2)
library(sf)

# Convert stems to sf object
census_1_ex_sf <- census_1_ex \%>\%
  st_as_sf(coords = c("gx", "gy"))

# Plot stems with plot boundary
ggplot() +
  geom_sf(data = study_region_ex) +
  geom_sf(data = study_region_bw, color = "red", fill = "transparent")
}
\seealso{
Other example data objects: 
\code{\link{blocks_ex}},
\code{\link{census_1_ex}},
\code{\link{census_2008_bw}},
\code{\link{census_2014_bw}},
\code{\link{census_2_ex}},
\code{\link{comp_bayes_lm_ex}},
\code{\link{focal_vs_comp_ex}},
\code{\link{growth_ex}},
\code{\link{growth_spatial_ex}},
\code{\link{growth_toy}},
\code{\link{species_bw}},
\code{\link{study_region_bw}}
}
\concept{example data objects}
\keyword{datasets}
