## Necessary libraries
library(plyr)

## Function to compute statistics for a given set of metrics
enumerateMetrics <- function(vars) {
    var_names <- lapply(vars, as.name)
    
    exprs <- lapply(var_names, function(var) substitute(mean(var, na.rm = TRUE), list(var = var)))
    names(exprs) <- vars
    
    exprs
}

## Read in the data
clean.all <- read.csv("../data/sotc.csv")
comm.facts <- read.csv("../data/CommunityFacts.csv")
latlons <- read.csv("../data/city_latlon.csv")
metrics <- read.csv("../data/metrics.csv")

## Build the expression for enumerateMetrics
expr <- enumerateMetrics(as.character(metrics$var_name))
expr$TOTALRESP <- quote(length(GENDER))

## Set the arguments and build the data frame for ALL, 2008, 2009, and 2010
base_args <- list(quote(clean.all), quote(.(QSB, URBAN_GR)), quote(summarise))
clean.all.city <- eval(as.call(c(quote(ddply), base_args, expr)))
base_args <- list(quote(subset(clean.all, source == "sotc08")), quote(.(QSB, URBAN_GR)), quote(summarise))
clean.2008.city <- eval(as.call(c(quote(ddply), base_args, expr)))
base_args <- list(quote(subset(clean.all, source == "sotc09")), quote(.(QSB, URBAN_GR)), quote(summarise))
clean.2009.city <- eval(as.call(c(quote(ddply), base_args, expr)))
base_args <- list(quote(subset(clean.all, source == "sotc10")), quote(.(QSB, URBAN_GR)), quote(summarise))
clean.2010.city <- eval(as.call(c(quote(ddply), base_args, expr)))

## Merge the community facts and the SOTC data
comm.facts.merge <- merge(comm.facts, latlons, by.x = "Community", by.y = "QSB")
clean.all.city.merge <- merge(clean.all.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2008.city.merge <- merge(clean.2008.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2009.city.merge <- merge(clean.2009.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2010.city.merge <- merge(clean.2010.city, comm.facts.merge, by.x = "QSB", by.y = "Community")

## Save the resulting data that will be used into an RData file
save(metrics, clean.all, comm.facts, clean.all.city.merge, clean.2008.city.merge, clean.2009.city.merge, clean.2010.city.merge, file = "../data/clean_sotc.RData")
