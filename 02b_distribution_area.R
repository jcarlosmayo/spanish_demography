source("00_read_and_clean.R")
library(foreign)
library(ggplot2)
library(reshape2)
library(maptools)
library(RColorBrewer)

# Map of municipalities in the peninsula, the Balearic Islands, Ceuta & Melilla
map_main <- readShapeSpatial("maps/SIANE_CARTO_BASE_S_3M/anual/20130101/mncp_peninsula.shp")
# I will use the Shape_Area variable as a proxy to examine municipality areas
# although by establishing a ratio between values from the Shape_Area variable and
# the municipality area as shown by Wikipedia, there seems to be an error of
# about 1.5 square kilometres in the few municipalities I checked.

# Extract the ID and area variables to a data frame
map_area <- data.frame("id" = map_main$ID_INE, "area" = map_main$Shape_Area)
map_area$id <- as.character(map_area$id)

# Merge data frames
mncp_area <- merge(x = mncp_total,
                   y = map_area,
                   by.x = "code", by.y = "id")


# Melt data frame
melt_mncp_area <- melt(mncp_area, id = c('code', 'province', 'municipality', 'area'),
                       variable.name = "year",
                       value.name = "population")



#####################
# Contingency table #
#####################

head(melt_mncp_area)

melt_mncp_area$interval <- cut(melt_mncp_area$population,
                               breaks = c(0, 2500, 5e3, 1e4, 5e4, 1e5, 5e5, 1e6,
                                          max(melt_mncp_area$population, na.rm = TRUE)),
                               labels = c("< 2,500", "2,500 - 5,000", "5,000-10,000",
                                          "10,000-50,000", "50,000-100,000",
                                          "100,000-500,000", "500,000-1,000,000",
                                          "> 1,000,000"))


int_level <- levels(melt_mncp_area$interval)

t_area <- c()

for (i in 1998:2014){
    
    annum <- paste("y", i, sep="")
    
    # Subset the data for each year
    sub_data <-subset(melt_mncp_area, year == annum)
    
    # Get the sums of areas by the population interval for the corresponding year
    sum_area <- tapply(sub_data$area, sub_data$interval, sum)
    
    # Append the data to a data frame
    t_area <- cbind(t_area, sum_area)
}

rm(i, annum, sub_data, sum_area)


# The variable names correspond to the year
colnames(t_area) <- c(1998:2014)

# Percentage contingency table
t_area_pcnt <- prop.table(t_area, 2) * 100


# To plot it we need to melt data frame using the levels of the population
melt_t_area <- melt(t_area_pcnt)
names(melt_t_area) <- c("level", "year", "area_pcnt")

# Reorder the levels
melt_t_area$level <- factor(melt_t_area$level, 
                            levels = c("> 1,000,000",
                                       "500,000-1,000,000",
                                       "100,000-500,000",
                                       "50,000-100,000", 
                                       "10,000-50,000",
                                       "5,000-10,000",   
                                       "2,500 - 5,000",
                                       "< 2,500"),
                            ordered = TRUE)


########
# Plot #
########

ggplot(melt_t_area,
       aes(x = year, y = area_pcnt,
           fill = level)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = brewer.pal(8, "BrBG"),
                      breaks = rev(levels(melt_t_area$level)))


###################
# Send to plot.ly #
###################
library(plotly) 
# set_credentials_file("username", "apikey")
py <- plotly()

area_percent <- ggplot(melt_t_area,
                      aes(x = year, y = area_pcnt,
                          fill = level)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = brewer.pal(8, "BrBG"),
                          breaks = rev(levels(melt_t_area$level)))

py$ggplotly(area_percent)