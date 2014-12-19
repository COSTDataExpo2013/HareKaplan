library(shiny)
library(plyr)
library(rjson)
library(ggplot2)

enumerateMetrics <- function(...) {
    return(paste(unlist(lapply(c(...), function(met){return(paste(met, " = mean(", met, ", na.rm = TRUE", ")", sep = ""))})), collapse = ", "))
}

getUrbanity <- function(data, city) {
    return(subset(data, QSB == city)$URBAN_GR)
}

generatePlot <- function(data, city, metric) {
    require(ggplot2)
    
    city_avg <- subset(data, QSB == city)[,metric]
    urban_avg <- mean(subset(data, URBAN_GR == getUrbanity(data, city))[,metric])
    overall_avg <- mean(data[,metric])
    
    x <- factor(c(city, as.character(getUrbanity(data, city)), "All Cities"), levels = c(city, as.character(getUrbanity(data, city)), "All Cities"))
    y <- c(city_avg, urban_avg, overall_avg)
    
    qplot(x, y, geom = "bar", stat = "identity", alpha = c(I(1), I(0.8), I(0.2)), fill = c(I("gold"), I("darkred"), I("darkred"))) +
        xlab("Community/Region") +
        ylab(metric)
}

generatePlot2 <- function(data, city, metric) {
    require(ggplot2)
    
    sorted.data <- data[order(data[,metric]), ]
    QSB.strs <- as.character(sorted.data$QSB)
    sorted.data$sameUrbanicity <- (sorted.data$URBAN_GR == getUrbanity(data, city))
    sorted.data$sortedQSB <- factor(sorted.data$QSB, levels = QSB.strs)
    sorted.data$urbanFac <- "Other"
    sorted.data$urbanFac[sorted.data$sameUrbanicity] <- "Same"
    sorted.data$urbanFac[sorted.data$QSB == city] <- "City"
    sorted.data$urbanFac <- factor(sorted.data$urbanFac, levels = c("Other", "Same", "City"))
    sorted.data$isCity <- sorted.data$QSB == city
    
    ggplot(data = sorted.data, aes_string(x = "sortedQSB", y = metric)) +
        geom_point(data = sorted.data, aes(size = sameUrbanicity, alpha = urbanFac, colour = isCity)) +
        scale_colour_manual(values = c(I("darkred"), I("gold"))) +
        scale_size_discrete(range = c(3, 5)) +
        scale_alpha_discrete(range = c(0.2, 1)) +
        coord_flip() +
        xlab("Community") +
        ylab(metric)    
}

generatePlot3 <- function(data, city, metric) {
    sorted.data <- data[order(-data[,metric]), ]
    QSB.strs <- as.character(sorted.data$QSB)
    sorted.data$sortedQSB <- factor(sorted.data$QSB, levels = QSB.strs)
    
    sorted.data$isCity <- sorted.data$QSB == city
    sorted.data$sameUrbanicity <- (sorted.data$URBAN_GR == getUrbanity(data, city))
    
    ggplot(data = subset(sorted.data, sameUrbanicity), aes_string(x = "sortedQSB", y = metric)) +
        geom_bar(data = subset(sorted.data, sameUrbanicity), stat = "identity", aes(fill = isCity)) +
        scale_fill_manual(values = c(I("darkred"), I("gold"))) +
        theme(axis.text.x = element_text(angle=90))
}

generatePlot4 <- function(full.data, city, metric) {
    sorted.data <- full.data
    QSB.strs <- as.character(sorted.data$QSB)
    sorted.data$sortedQSB <- factor(sorted.data$QSB, levels = QSB.strs)
    
    sorted.data$isCity <- sorted.data$QSB == city
    sorted.data$sameUrbanicity <- (sorted.data$URBAN_GR == getUrbanity(data, city))
    
    ggplot(data = subset(sorted.data, sameUrbanicity), aes_string(x = "sortedQSB", y = metric)) +
        geom_boxplot(data = subset(sorted.data, sameUrbanicity), aes(fill = isCity)) +
        scale_fill_manual(values = c(I("darkred"), I("gold"))) +
        coord_flip()
}

generatePlot5 <- function(data, full.data, city, metric) {
    metricMax <- sapply(metrics, getMax, data = full.data)    
    
    scaledInd <- sapply(metrics, function(x){data[,x] / metricMax[names(metricMax) == x]})
    new.data <- data.frame(data$QSB, data$URBAN_GR, scaledInd)
    
    data.interest <- subset(new.data, data.QSB == city)
    data.interest <- data.interest[-c(1, 2)]
    data.interest <- sort(data.interest, decreasing = TRUE)

    data.urbanicity <- subset(new.data, data.URBAN_GR == getUrbanity(data, city))
    data.all <- colMeans(data.urbanicity[-c(1, 2)])
    
    x <- factor(names(data.interest), levels = names(data.interest))
    y <- as.numeric(data.interest)
    z <- x == metric
    
    data.all.sorted <-data.frame(Value = data.all, Var = names(data.all))
    data.all.sorted <- data.all.sorted[match(data.all.sorted$Var, x), ]
    yy <- as.numeric(data.all)
    zz <- xx == metric
        
    qplot(x = x, y = y, label = x, geom = "text", colour = z) +
        theme(legend.position = "off") +
        scale_colour_manual(values = c(I("darkred"), I("gold"))) +
        geom_text(data = data.all.sorted, alpha = I(0.3), aes(x = xx, y = yy, label = xx, colour = zz)) +
        xlab("") +
        ylab("Value")
}

generateTable <- function(data, city, metric) {
    city_avg <- subset(data, QSB == city)[,metric]
    urban_avg <- mean(subset(data, URBAN_GR == getUrbanity(data, city))[,metric])
    overall_avg <- mean(data[,metric])
    
    x <- factor(c(city, as.character(getUrbanity(data, city)), "All Cities"), levels = c(city, as.character(getUrbanity(data, city)), "All Cities"))
    y <- c(city_avg, urban_avg, overall_avg)
    
    df <- data.frame(x,y)
    names(df) <- c("Community/Region", metric)
    
    return(df)
}

getCityCor <- function(full.data, city, metric) {
    city_avg <- subset(full.data, QSB == city)[,c(metric, "CCE")]
    cor1 <- cor(city_avg, use = "complete.obs")[1,2]
    
    return(cor1)
}

getUrbanCor <- function(full.data, urbanicity, metric) {
    urban_avg <- subset(full.data, URBAN_GR == urbanicity)[,c(metric, "CCE")]
    cor2 <- cor(urban_avg, use = "complete.obs")[1,2]
                            
    return(cor2)
}

getRegionCor <- function(full.data, region, metric) {
    region_avg <- subset(full.data, Region == region)[,c(metric, "CCE")]
    cor3 <- cor(region_avg, use = "complete.obs")[1,2]
    
    return(cor3)
}

getOverallCor <- function(full.data, metric) {
    return(cor(full.data[,c(metric, "CCE")], use = "complete.obs")[1,2])
}

getCorMat <- function(full.data, year) {
    test <- data.frame(Year = year, City = clean.all.city[,1], sapply(metrics[-1], function(met){sapply(clean.all.city[,1], function(cty){getCityCor(full.data, cty, met)})}))
    test2 <- data.frame(Year = year, City = unique(clean.all$URBAN_GR), sapply(metrics[-1], function(met){sapply(unique(clean.all$URBAN_GR), function(urb){getUrbanCor(full.data, urb, met)})}))
    test3 <- data.frame(Year = year, City = unique(comm.facts$Region), sapply(metrics[-1], function(met){sapply(unique(comm.facts$Region), function(reg){getRegionCor(full.data, reg, met)})}))
    test4 <- data.frame(Year = year, City = "All Cities", t(sapply(metrics[-1], function(met){getOverallCor(full.data, met)})))
    
    rbind(test, test2, test3, test4)
}

clean.all <- read.csv("data/sotc.csv")
corr.dat <- read.csv("data/CEcor.csv")
comm.facts <- read.csv("data/CommunityFacts.csv")

#metrics <- c("CCE", "SAFETY", "EDUCATIO", "LEADERSH", "AESTHETI", "ECONOMY", "SOCIAL_O", "SOCIAL_C", "BASIC_SE", "INVOLVEM", "OPENNESS")
metrics <- c("CCE", "SOCIAL_O", "OPENNESS", "AESTHETI", "EDUCATIO", "BASIC_SE", "LEADERSH", "ECONOMY", "SAFETY", "SOCIAL_C", "INVOLVEM")
getMax <- function(data, met) {return(max(data[,met], na.rm = TRUE))}

getRegion <- function(city) {
    return(subset(comm.facts, Community == city)$Region)
}

# regions <- sapply(clean.all$QSB, getRegion)
# clean.all <- data.frame(clean.all, Region = regions)
# 
# cor.2008 <- getCorMat(subset(clean.all, source == "sotc08"), "2008")
# cor.2009 <- getCorMat(subset(clean.all, source == "sotc09"), "2009")
# cor.2010 <- getCorMat(subset(clean.all, source == "sotc10"), "2010")
# cor.agg <- getCorMat(clean.all, "Aggregate")
# 
# cor.all <- rbind(cor.2008, cor.2009, cor.2010, cor.agg)

#meangf <- mean(subset(clean.all, QSB == "Grand Forks, ND")$EDUCATIO, na.rm = TRUE)
#meanab <- mean(subset(clean.all, QSB == "Aberdeen, SD")$EDUCATIO, na.rm = TRUE)
#meandu <- mean(subset(clean.all, QSB == "Duluth, MN")$EDUCATIO, na.rm = TRUE)
#meansp <- mean(subset(clean.all, QSB == "St. Paul, MN")$EDUCATIO, na.rm = TRUE)
#meanwi <- mean(subset(clean.all, QSB == "Wichita, KS")$EDUCATIO, na.rm = TRUE)
#meanother <- mean(subset(clean.all, Region != "Great Plains")$EDUCATIO, na.rm = TRUE)

#theseCities <- c("Grand Forks, ND", "Aberdeen, SD", "St. Paul, MN", "Duluth, MN", "Wichita, KS", "Other")
#thisdf <- data.frame(City = factor(theseCities, levels = rev(theseCities)), MeanEducation = c(meangf, meanab, meansp, meandu, meanwi, meanother))

#qplot(City, MeanEducation, data = thisdf, geom = "bar", fill = City, stat = "identity") +
#    ylim(c(0, 3)) +
#    coord_flip() +
#    scale_fill_manual(values = rev(c("gold", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "black"))) +
#    theme(legend.position = "bottom")

#ggsave("Hmm.png")

addResourcePath('data', '~/ShinyApps/DataExpo2013/data')
addResourcePath('css', '~/ShinyApps/DataExpo2013/css')

shinyServer(function(input, output) {
    data <- function() {
        expr.all <- paste(as.expression(paste("ddply(clean.all, .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(metrics), ", TOTALRESP = length(GENDER))", sep = "")
        expr.2008 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc08\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(metrics), ", TOTALRESP = length(GENDER))", sep = "")
        expr.2009 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc09\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(metrics), ", TOTALRESP = length(GENDER))", sep = "")
        expr.2010 <- paste(as.expression(paste("ddply(subset(clean.all, source == \"sotc10\"), .(QSB, URBAN_GR), summarise, ")), enumerateMetrics(metrics), ", TOTALRESP = length(GENDER))", sep = "")
        
        #2008 doesn't have thriving or struggli, new metric needed?
        clean.all.city <- eval(parse(text = expr.all))
        clean.2008.city <- eval(parse(text = expr.2008))
        clean.2009.city <- eval(parse(text = expr.2009))
        clean.2010.city <- eval(parse(text = expr.2010))
        
        lats <- c(37.688889, 30.440608, 44.953703, 40.793395, 37.338978, 39.952335, 26.705621, 33.689060, 33.080143, 25.788969, 32.840695, 33.768321, 38.040584, 47.925257, 41.593370, 41.079273, 46.786672, 42.331427, 32.460976, 34.000710, 35.227087, 27.498928, 40.014986, 30.396032, 41.081445, 45.464698)   
        lons <- c(-97.336111, -84.286079, -93.089958, -77.860001, -121.894955, -75.163789, -80.036430, -78.886694, -83.232099, -80.226439, -83.632402, -118.195617, -84.503716, -97.032855, -87.346427, -85.139351, -92.100485, -83.045754, -84.987709, -81.034814, -80.843127, -82.574819, -105.270546, -88.885308, -81.519005, -98.486483)
        
        clean.all.city$lats <- rev(lats)
        clean.all.city$lons <- rev(lons)
        clean.2008.city$lats <- rev(lats)
        clean.2008.city$lons <- rev(lons)
        clean.2009.city$lats <- rev(lats)
        clean.2009.city$lons <- rev(lons)
        clean.2010.city$lats <- rev(lats)
        clean.2010.city$lons <- rev(lons)
        

        clean.all.city.merge <- merge(clean.all.city, comm.facts, by.x = "QSB", by.y = "Community")
        clean.2008.city.merge <- merge(clean.2008.city, comm.facts, by.x = "QSB", by.y = "Community")
        clean.2009.city.merge <- merge(clean.2009.city, comm.facts, by.x = "QSB", by.y = "Community")
        clean.2010.city.merge <- merge(clean.2010.city, comm.facts, by.x = "QSB", by.y = "Community")

        
        return(list(data_json = toJSON(unname(split(clean.all.city.merge, 1:nrow(clean.all.city)))), 
                    data_json_2008 = toJSON(unname(split(clean.2008.city.merge, 1:nrow(clean.2008.city)))),
                    data_json_2009 = toJSON(unname(split(clean.2009.city.merge, 1:nrow(clean.2009.city)))),
                    data_json_2010 = toJSON(unname(split(clean.2010.city.merge, 1:nrow(clean.2010.city)))),
                    data_json_corr = toJSON(unname(split(corr.dat, 1:nrow(corr.dat))))))
    }
    
    output$d3io <- reactive({ data() })
})
