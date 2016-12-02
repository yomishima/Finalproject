# Finalproject

library( ggmap)
library("dplyr")
library(data.table)
library("shiny")
library(leaflet)
library("fossil")
dat <- read.csv("~/MEI HAN/PAI/Final projects/201501-citibike-tripdata/201501-citibike-tripdata.csv")
dat$age <- 2016 - dat$birth.year

frequencytable <- data.frame(table(dat$start.station.name, dat$end.station.name))
colnames(frequencytable) <- c("start.station.name", "end.station.name", "frequency")
end.station <- unique(data.frame (dat$end.station.name,dat$end.station.latitude,dat$end.station.longitude))
start.station <- unique(data.frame(dat$start.station.name, dat$start.station.latitude,dat$start.station.longitude))
dat.map.start <- merge(frequencytable, start.station , by.x = c("start.station.name"), by.y = c("dat.start.station.name"), all.x = T )
dat.map <- merge(dat.map.start, end.station , by.x = c("end.station.name"), by.y = c("dat.end.station.name"), all.x = T )
colnames(dat.map) <- c("start.station.name", "end.station.name", "frequency","end.station.latitude", "end.station.longitude", "start.station.latitude","start.station.longitude")
dat.map.rank <- data.table(dat.map, key = 'start.station.name')
dat.map.group <- transform(dat.map.rank, Rank = ave (frequency, start.station.name, FUN = function(x) + rank (-x, ties.method ="min")))
dat.map.ten <- dat.map.group

nyc <- get_map( location='new york city, ny', zoom = 10, color="bw" )

selectInput( 
             inputId= 'StartStation', 
             label='Select Start Station',
             choices = unique(dat.map.ten$start.station.name),
             selected = "1 Ave & E 15 St"
           )
sliderInput("RankRange", label = ("Rank Range"), min = 1, 
        max = 100, value = 10)
sliderInput("Zoom", label = ("Zoom Bar"), min = 10, 
        max = 20, value = 12)

renderPlot({
  nyc <- get_map( location='new york city, ny', zoom = input$Zoom, color="bw" ) 
  ggmap(nyc)
  start_point <- ggmap(nyc, extent= "device") 
  start_point <- start_point + geom_point( data=dat.map.ten[dat.map.ten$start.station.name== input$StartStation & dat.map.ten$Rank <= input$RankRange], 
                                       aes( x=start.station.longitude, y=start.station.latitude), 
                                       
                                       color="black",
                                       size=3)
start_point
 end_point <- start_point + geom_point( data=dat.map.ten[dat.map.ten$start.station.name== input$StartStation & dat.map.ten$Rank <= input$RankRange], 
                                       aes( x=end.station.longitude, y=end.station.latitude), 
                                       color="blue",
                                       size=3)
 end_point 
end_point + geom_leg(aes(x = start.station.longitude, y = start.station.latitude, xend = end.station.longitude, yend = end.station.latitude, size = frequency, color = Rank, size = 6), data = dat.map.ten[dat.map.ten$start.station.name== input$StartStation & dat.map.ten$Rank <= input$RankRange] )+ guides(shape=guide_legend(override.aes=list(size=20)))
}, height = 2000, width = 2000)
