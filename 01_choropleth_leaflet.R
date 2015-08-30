source("00_read_and_clean.R")
library(maptools) # Read and plot maps

# Map of municipalities in the peninsula, the Balearic Islands, Ceuta & Melilla
map_main <- readShapeSpatial("maps/SIANE_CARTO_BASE_S_3M/anual/20130101/mncp_peninsula.shp")
# There are several layers of data that can be accessed in the SHP file
# map_main$... and map_main@...

# Match IDs in the map to the municipality codes
match <- mncp_total[match(map_main@data$ID_INE, mncp_total$code),]

# Define color rules for plotting
library(RColorBrewer)

findColor <- function(x) {
    mncp_colors <- brewer.pal(8, "BrBG")
    if (is.na(x)){
        return("#d6604d")
    } else if ( x > 1e6) {
        return(mncp_colors[1])
    } else if ( x > 5e5) {
        return(mncp_colors[2])
    } else if ( x > 1e5) {
        return(mncp_colors[3])
    } else if ( x > 5e4) {
        return(mncp_colors[4])
    } else if ( x > 1e4) {
        return(mncp_colors[5])
    } else if ( x > 5e3) {
        return(mncp_colors[6])
    } else if ( x > 2500) {
        return(mncp_colors[7])
    } else {return(mncp_colors[8])}
}

# Apply color rules
mncp_colors <- sapply(match$y2014, findColor)


###############
# Leaflet map #
###############
library(leaflet)

# Information on the pop-up: municipality name and population
map_info <- paste(match$municipality, "</br>Population: ",
                  formatC(match$y2014, big.mark = ",", format = "d"))


leaf_map <- leaflet(data = map_main) %>%
    addPolygons(fillColor = mncp_colors, fillOpacity = 100, stroke = FALSE,
                smoothFactor = 0.5,
                popup = map_info) %>%
    addLegend("bottomright", colors = c(brewer.pal(8, "BrBG"), "#d6604d"),
              opacity = 1,
              title = "Spanish Population 2014",
              labels = c("n \u003E 1,000,000",
                         "500,000 \u003C n \u2264 1,000,000",
                         "100,000 \u003C n \u2264 500,000",
                         "50,000 \u003C n \u2264 100,000",
                         "10,000 \u003C n \u2264 50,000",
                         "5,000 \u003C n \u2264 10,000",
                         "2,500 \u003C n \u2264 5,000",
                         "n \u003C 2,500",
                         "No data"))

# Save map as an html file
library(htmlwidgets)
saveWidget(leaf_map, file = "leaf_map.html", selfcontained = FALSE)

# Credit goes to Nathan Yau who has really helpful tutorials on maps
# in his site: flowingdata.com