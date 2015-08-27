library(shiny)
library(rjson)

addResourcePath("data", "data")
load("data/clean_sotc.RData")
corr.dat <- read.csv("data/CEcor.csv")

data <- list(data_json = toJSON(unname(split(clean.all.city.merge, 1:nrow(clean.all.city.merge)))), 
                data_json_2008 = toJSON(unname(split(clean.2008.city.merge, 1:nrow(clean.2008.city.merge)))),
                data_json_2009 = toJSON(unname(split(clean.2009.city.merge, 1:nrow(clean.2009.city.merge)))),
                data_json_2010 = toJSON(unname(split(clean.2010.city.merge, 1:nrow(clean.2010.city.merge)))),
                data_json_corr = toJSON(unname(split(corr.dat, 1:nrow(corr.dat)))))

shinyServer(function(input, output) {
    output$d3io <- reactive({ data })
})
