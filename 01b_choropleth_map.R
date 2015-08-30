# Load the data frames
source("00_read_and_clean.R")
# Focus on total population
rm(mncp_tmen, mncp_twomen)
library(maptools) # Read and plot maps

# Map of municipalities in the peninsula, the Balearic Islands, Ceuta & Melilla
map_main <- readShapeSpatial("maps/SIANE_CARTO_BASE_S_3M/anual/20130101/mncp_peninsula.shp")
# There are several layers of data
# that can be accessed in the SHP file using the '$' and '@' operators
# e.g. map_main$... and map_main@...


# Match IDs and link to municipality polygons
match <- mncp_total[match(map_main@data$ID_INE, mncp_total$code),]

# Define color rules
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

# Apply the findColors function to the 2014 data
mncp_colors <- sapply(match$y2007, findColor)

# Plot map
plot(map_main, col = mncp_colors, lwd=0.025) # lwd=0.025  0.4
# Append legend
legend(x = -11, y = 41.5, # Position
       legend = c("n \u003E 1,000,000",
                  "500,000 \u003C n \u2264 1,000,000",
                  "100,000 \u003C n \u2264 500,000",
                  "50,000 \u003C n \u2264 100,000",
                  "10,000 \u003C n \u2264 50,000",
                  "5,000 \u003C n \u2264 10,000",
                  "2,500 \u003C n \u2264 5,000",
                  "n \u003C 2,500",
                  "No data"),
       fill = c(brewer.pal(8, "BrBG")[1:8], "#d6604d"),
       cex = 0.6,
       bty = "n",
       horiz = FALSE
       )
# Append title
title("Population distribution 2007\nby municipality size")

# Credit goes to Nathan Yau who has really helpful tutorials on maps
# in his site: flowingdata.com