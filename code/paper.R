## Necessary libraries
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(maps)

###
### Data Step
###
load("../data/clean_sotc.RData")

## Bind all the data together
all.years.city <- rbind(cbind(year=rep(2008, nrow(clean.2008.city.merge)), clean.2008.city.merge),
                        cbind(year=rep(2009, nrow(clean.2009.city.merge)), clean.2009.city.merge),
                        cbind(year=rep(2010, nrow(clean.2010.city.merge)), clean.2010.city.merge),
                        cbind(year=rep("Aggregate", nrow(clean.all.city.merge)), clean.all.city.merge)
)

## Melt the data to put it into an easier format for plotting
all.melt <- subset(melt(all.years.city, id.vars = c("year", "Region", "QSB")), variable %in% as.character(metrics$var_name))
all.melt$disp_name <- apply(all.melt, 1, function(a) {metrics$disp_name[metrics$var_name == as.character(a["variable"])]})
all.melt$disp_name <- factor(all.melt$disp_name, levels = unique(as.character(all.melt$disp_name)))

###
### Functions to create Correlation Matrix for CommuniD3
###
## Get the correlation of a particular city's metric with community attachment
getCityCor <- function(full.data, city, metric) {
    city_avg <- subset(full.data, QSB == city)[,c(metric, "CCE")]
    cor1 <- cor(city_avg, use = "complete.obs")[1,2]
    
    return(cor1)
}

## Get the correlation of a particular urbanicity's metric with community attachment
getUrbanCor <- function(full.data, urbanicity, metric) {
    urban_avg <- subset(full.data, URBAN_GR == urbanicity)[,c(metric, "CCE")]
    cor2 <- cor(urban_avg, use = "complete.obs")[1,2]
    
    return(cor2)
}

## Get the correlation of a particular region's metric with community attachment
getRegionCor <- function(full.data, region, metric) {
    region_avg <- subset(full.data, Region == region)[,c(metric, "CCE")]
    cor3 <- cor(region_avg, use = "complete.obs")[1,2]
    
    return(cor3)
}

## Get the correlation of a metric with community attachment
getOverallCor <- function(full.data, metric) {
    return(cor(full.data[,c(metric, "CCE")], use = "complete.obs")[1,2])
}

## Get the correlation matrix of a particular year and all metrics
getCorMat <- function(full.data, year) {
    test <- data.frame(Year = year, City = clean.all.city[,1], sapply(metrics[-1], function(met){sapply(clean.all.city[,1], function(cty){getCityCor(full.data, cty, met)})}))
    test2 <- data.frame(Year = year, City = unique(clean.all$URBAN_GR), sapply(metrics[-1], function(met){sapply(unique(clean.all$URBAN_GR), function(urb){getUrbanCor(full.data, urb, met)})}))
    test3 <- data.frame(Year = year, City = unique(comm.facts$Region), sapply(metrics[-1], function(met){sapply(unique(comm.facts$Region), function(reg){getRegionCor(full.data, reg, met)})}))
    test4 <- data.frame(Year = year, City = "All Cities", t(sapply(metrics[-1], function(met){getOverallCor(full.data, met)})))
    
    rbind(test, test2, test3, test4)
}

###
### Map
###
states <- map_data("state")

qplot(long, lat, data = states, group = group, geom = "polygon", fill = I("grey")) + 
    geom_path(colour = I("white")) +
    geom_point(data = clean.all.city.merge, inherit.aes = FALSE, size = I(7), aes(x = lons, y = lats, colour = Region, shape = Region, fill = Region)) +
    scale_shape_manual(values = c(15:18, 25)) +
    theme_bw() +
    theme(aspect.ratio = 1/1.75, legend.position = "bottom") +
    theme(axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

###
### Southeast Region
###
myrtle.dat <- cbind(subset(all.melt, year == "Aggregate"), Community = with(subset(all.melt, year == "Aggregate"), ifelse(QSB == "Myrtle Beach, SC", "Myrtle Beach, SC", ifelse(Region == "Southeast", "Southeast", "Other"))))
myrtle.dat$disp_name <- factor(myrtle.dat$disp_name, levels = unique(as.character(myrtle.dat$disp_name)))
myrtle.dat$Community.f <- factor(myrtle.dat$Community, levels = c("Myrtle Beach, SC", "Southeast", "Other"))
myrtle.dat$order <- with(subset(all.melt, year == "Aggregate"), ifelse(QSB == "Myrtle Beach, SC", 2, ifelse(Region == "Southeast", 1, 0)))

## Plot metrics, highlighting Myrtle Beach as particular interesting
ggplot() + 
    geom_line(data = subset(myrtle.dat, Community == "Other"), 
              aes(x = disp_name, y = as.numeric(value), group = QSB, colour = Community.f), 
                  size = I(1)) + 
    geom_point(data = subset(myrtle.dat, Community == "Other"),
               aes(x = disp_name, y = as.numeric(value), 
               colour = Community.f), size = I(2), inherit.aes = FALSE) +
    geom_line(data = subset(myrtle.dat, Community == "Southeast"),
              aes(x = disp_name, y = as.numeric(value), group = QSB, 
              colour = Community.f), inherit.aes = FALSE, size = 1) + 
    geom_point(data = subset(myrtle.dat, Community == "Southeast"),
              aes(x = disp_name, y = as.numeric(value), 
               colour = Community.f), size = I(2), inherit.aes = FALSE) +
    geom_line(data = subset(myrtle.dat, Community == "Myrtle Beach, SC"),
              aes(x = disp_name, y = as.numeric(value), group = QSB, 
              colour = Community.f), inherit.aes = FALSE, size = 1.5) + 
    geom_point(data = subset(myrtle.dat, Community == "Myrtle Beach, SC"),
               aes(x = disp_name, y = as.numeric(value), 
               colour = Community.f), size = I(4), inherit.aes = FALSE) +
    xlab("") + ylab("Metric Value - Aggregated Years") +
    scale_colour_manual(name = "Community",
                        values = c("#0cae3a","darkgrey","#0066cc"),
                        breaks = c("Myrtle Beach, SC", "Southeast", "Other"),
                        labels = c("Myrtle Beach, SC", "Southeast", "Other")) + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1))

## Same plot, slightly different color scheme
ggplot(data = myrtle.dat, mapping = aes(x = disp_name, y = as.numeric(value), 
                                      group = paste(order, QSB, sep=""), colour = Community)) +
    geom_line(aes(size = Community)) + 
    scale_size_manual(name = "Community",
                      values = c(1.5, 1, 1),
                      labels = c("Myrtle Beach, SC", "Southeast", "Other")) +
    geom_point(aes(size = Community)) + 
    xlab("") + ylab("Metric Value - Aggregated Years") +
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
    scale_colour_manual(name = "Community",
                        values = c("#FF3300","#000066","darkgrey"),
                        labels = c("Myrtle Beach, SC", "Southeast", "Other")) +
    scale_size_manual(name = "Community",
                        values = c(4, 2, 2),
                        labels = c("Myrtle Beach, SC", "Southeast", "Other")) 


###
### Rust Belt Region
###
clean.all.merge <- merge(clean.all, comm.facts, by.x = "QSB", by.y = "Community")
rust.dat <- subset(clean.all.merge, Region == "Rust Belt")
rust.dat$detroit_f <- rust.dat$QSB == "Detroit, MI"
rust.dat$year <- with(rust.dat, ifelse(source == "sotc08", 2008, ifelse(source == "sotc09", 2009, 2010)))

## Plot the economy across years in the rust belt
ggplot() +
    geom_density(data = rust.dat, aes(x = ECONOMY, group = factor(year), fill = factor(year), colour = factor(year)), position = "dodge", alpha = I(0.4)) +
    facet_wrap(~QSB) + 
    xlab("Economy") + ylab("Density of Responses") + scale_colour_discrete(name = "year") + scale_fill_discrete(name = "year") + theme_bw()


###
### Great Plains Region
###
plains.dat <- subset(clean.all.merge, Region == "Great Plains")
all.years.city$plainsQSB <- with(all.years.city, ifelse(as.character(Region) == "Great Plains", as.character(QSB), "Other"))

## Calculate the mean value of community attachment across subsets using ddploy
plains.dat <- ddply(melt(all.years.city[,c("year","CCE","EDUCATIO","plainsQSB")], id.vars = c("year", "plainsQSB")), .(year, plainsQSB, variable), summarise, value = mean(as.numeric(value)))
plains.dat$disp_name <- with(plains.dat, ifelse(variable == "CCE", "Community Attachment", "Education"))
plains.dat$disp_name <- factor(plains.dat$disp_name, levels = c("Education", "Community Attachment"))
plains.dat$plainsQSB <- factor(plains.dat$plainsQSB, levels = c("Aberdeen, SD", "Duluth, MN", "Grand Forks, ND", "St. Paul, MN", "Wichita, KS", "Other"))

## Clean up the data a little bit
plains.clean <- cbind(subset(plains.dat, variable == "EDUCATIO"), CCE = subset(plains.dat, variable == "CCE")[,4])
names(plains.clean)[2] <- "Community"

## Plot of education versus community attachment in the Great Plains region
ggplot(data = plains.clean) +
    geom_smooth(aes(x = as.numeric(value), y = as.numeric(CCE)), method = "lm", alpha = 0.25) +
    geom_point(aes(x = as.numeric(value), y = as.numeric(CCE), colour = Community, size = year)) +
    scale_size_manual(name = "Year",
                      values = c(1,2,3,6),
                      labels = c("2008","2009","2010","Aggregate")) +
    ylab("Community Attachment") + xlab("Education") +
    theme(axis.text.y = element_blank())


###
### West Region
###
west.dat <- subset(clean.all.merge, Urbanicity == "Very high urbanicity-medium population")
west.dat$QSB <- factor(west.dat$QSB, levels = c("Boulder, CO", "Akron, OH", "Bradenton, FL", "Long Beach, CA", "Gary, IN"))

## Binned density plot of high urbanicity medium population communities
ggplot(data = west.dat) +
    #geom_jitter(aes(x = OPENNESS, y= CCE, colour=Region)) + facet_wrap(~QSB, nrow=3) +
    geom_bin2d(binwidth = c(0.15, 0.15), aes(x = OPENNESS, y = CCE)) + facet_wrap(~QSB, nrow = 2) +
    scale_fill_gradient(low = I("white"), high = I("red")) +
    ylab("Community Attachment") + xlab("Openness") + ggtitle("Very high urbanicity-medium population Communities")


###
### Deep South Region
###
south.dat <- cbind(subset(all.years.city, year != "Aggregate"), Community = with(subset(all.years.city, year != "Aggregate"), ifelse(QSB == "Biloxi, MS", "Biloxi, MS", ifelse(Region == "Deep South", "Deep South", "Other"))))
south.dat$Community <- factor(south.dat$Community, levels = c("Biloxi, MS", "Deep South", "Other"))
south.dat.melt <- melt(south.dat[, c("year", "QSB", "Community", "SAFETY", 'SOCIAL_O', "CCE")], id.vars = c("year", "QSB", "Community"))
south.dat.melt$disp_name <- apply(south.dat.melt, 1, function(x) ifelse(x["variable"] == "CCE", "Community Attachment", ifelse(x["variable"] == "SOCIAL_O", "Social Offerings", "Safety")))
south.dat.melt$disp_name <- factor(south.dat.melt$disp_name, levels = c("Safety", "Social Offerings", "Community Attachment"))

## Plot safety, social offerings, and community attachment over time in Deep South, highlighting Biloxi.
ggplot() + 
    geom_line(data = subset(south.dat.melt, Community == "Other"), 
              aes(x = year, y = as.numeric(value), group = QSB, colour = Community), 
              size = 1) + 
    geom_point(data = subset(south.dat.melt, Community == "Other"),
               aes(x = year, y = as.numeric(value), 
                   colour = Community), size = 2, inherit.aes = FALSE) +
    geom_line(data = subset(south.dat.melt, Community == "Deep South"),
              aes(x = year, y = as.numeric(value), group = QSB, 
                  colour = Community), inherit.aes = FALSE, size = 1) + 
    geom_point(data = subset(south.dat.melt, Community == "Deep South"),
               aes(x = year, y = as.numeric(value), 
                   colour = Community), size = I(2), inherit.aes = FALSE) +
    geom_line(data = subset(south.dat.melt, Community == "Biloxi, MS"),
              aes(x = year, y = as.numeric(value), group = QSB, 
                  colour = Community), inherit.aes = FALSE, size = 1.5) + 
    geom_point(data = subset(south.dat.melt, Community == "Biloxi, MS"),
               aes(x = year, y = as.numeric(value), 
                   colour = Community), size = I(4), inherit.aes = FALSE) + facet_wrap(~disp_name, scales="free_y") + 
    xlab("") + ylab("") + 
    scale_colour_manual(name = "Community",
                        values = c("#FF3300","#000066","darkgrey"),
                        breaks = c("Biloxi, MS", "Deep South", "Other"),
                        labels = c("Biloxi, MS", "Deep South", "Other"))
