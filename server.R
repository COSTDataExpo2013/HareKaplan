library(shiny)
library(rjson)

## Load the relevant data
addResourcePath("data", "data")
load("data/clean_sotc.RData")
corr.dat <- read.csv("data/CEcor.csv")

## Bind together the data in JSON format
data <- list(data_json = toJSON(list(all = unname(split(clean.all.city.merge, 1:nrow(clean.all.city.merge))), 
                                    `2008` = unname(split(clean.2008.city.merge, 1:nrow(clean.2008.city.merge))),
                                    `2009` = unname(split(clean.2009.city.merge, 1:nrow(clean.2009.city.merge))),
                                    `2010` = unname(split(clean.2010.city.merge, 1:nrow(clean.2010.city.merge))))),
                data_json_corr = toJSON(unname(split(corr.dat, 1:nrow(corr.dat)))))

shinyServer(function(input, output) {
    
    ## Output a reactive version of this data
    output$d3io <- reactive(data)
    
})
