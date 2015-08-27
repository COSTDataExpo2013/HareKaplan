## Necessary libraries
library(plyr)

# Calculate the mean value for all metrics at once
enumerateMetrics <- function(...) {
    return(paste(unlist(lapply(c(...), function(met){return(paste(met, " = mean(", met, ", na.rm = TRUE", ")", sep = ""))})), collapse = ", "))
}

## Read in the data
clean.all <- read.csv("../data/sotc.csv")
comm.facts <- read.csv("../data/CommunityFacts.csv")
latlons <- read.csv("../data/city_latlon.csv")

## Create data frame containing metric variable names and display names
metrics <- data.frame(var_name = c("CCE", "AESTHETI", "SOCIAL_C", "INVOLVEM", "EDUCATIO", "SOCIAL_O", "OPENNESS", "SAFETY", "BASIC_SE", "ECONOMY", "LEADERSH"),
                      disp_name = c("Community Attachment", "Aesthetics", "Social Capital", "Civic Involvement", "Education", "Social Offerings", "Openness", "Safety", "Basic Services", "Economy", "Leadership"))

## Calculate the metrics across all years and for each individual year
expr.all <- paste(as.expression(paste("ddply(clean.all, .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(as.character(metrics$var_name)), ", TOTALRESP = length(GENDER))", sep = "")
expr.2008 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc08\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(as.character(metrics$var_name)), ", TOTALRESP = length(GENDER))", sep = "")
expr.2009 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc09\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(as.character(metrics$var_name)), ", TOTALRESP = length(GENDER))", sep = "")
expr.2010 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc10\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(as.character(metrics$var_name)), ", TOTALRESP = length(GENDER))", sep = "")
clean.all.city <- eval(parse(text = expr.all))
clean.2008.city <- eval(parse(text = expr.2008))
clean.2009.city <- eval(parse(text = expr.2009))
clean.2010.city <- eval(parse(text = expr.2010))

## Merge the community facts and the SOTC data
comm.facts.merge <- merge(comm.facts, latlons, by.x = "Community", by.y = "QSB")
clean.all.city.merge <- merge(clean.all.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2008.city.merge <- merge(clean.2008.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2009.city.merge <- merge(clean.2009.city, comm.facts.merge, by.x = "QSB", by.y = "Community")
clean.2010.city.merge <- merge(clean.2010.city, comm.facts.merge, by.x = "QSB", by.y = "Community")

save(metrics, clean.all, comm.facts, clean.all.city.merge, clean.2008.city.merge, clean.2009.city.merge, clean.2010.city.merge, file = "../data/clean_sotc.RData")
