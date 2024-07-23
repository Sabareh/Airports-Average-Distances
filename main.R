haversine <- function(lat1, long1, lat2, long2, unit="km"){
  radius <- 6378
  delta.phi <- to.radians(lat2 - lat1)
  delta.lamba <- to.radians(long2 - long1)
  phil <- to.radians(lat1)
  phi2 <- to.radians(lat2)
  term1 <- sin(delta.phi/2) ^ 2
  term2 <- cos(phi1) * cos(phi2) * sin(delta.lambda/2) ^ 2
  the.terms <- term1 + term2
  delta.sigma <- 2 * atan2(sqrt(the.terms), sqrt(1-the.terms))
  distance <- radius * delta.sigma
  if(unit=="km") return(distance)
  if(unit=="miles") return(0.621371*distance)
}

to.radians <- function(degrees){
  degrees * pi / 180
}
set.seed(1)
the.url <- "http://opendata.socrata.com/api/views/rxrh-4cxm/rows.csv?accessType=DOWNLOAD"
all.airport.locs <- read.csv(the.url, stringsAsFactors=FALSE)

library(magrittr)
library(assertr)

CHECKS <- . %>%
    verify(nrow(.) == 13429) %>%
    verify(names(.) %in% c("locationID", "Latitude", "Longitude")) %>%
    assert(within_bounds(0, 90), Latitude) %>% 
    assert(within_bounds(0, 180), Longitude)
all.airport.locs <- CHECKS(all.airport.locs)

#starting off with 400 airports

smp.size <- 400

#choosing a random sample of airports

random.sample <- sample((1:nrow(all.airport.locs)), smp.size)
airports.locs <- all.airport.locs[random.sample, ]

row.names(airports.locs) <- NULL
haed(airports.locs)


