source("00_read_and_clean.R")
library(ggplot2)
library(foreign)
library("reshape2")
library(RColorBrewer)

# Melt data frame
melt_mncp_total <- melt(mncp_total, id = c('province', 'municipality', 'code'),
                        variable.name = "year",
                        value.name = "population")


#####################
# Contingency table #
#####################

cntgcy_table <- data.frame()
for (i in 1998:2014){
    
    year <- paste("y", i, sep="")
    
    intervals <- cut(mncp_total[,year],
                     breaks = c(0, 2500, 5e3, 1e4, 5e4, 1e5, 5e5, 1e6,
                                max(mncp_total[,year], na.rm = TRUE)),
                     labels = c("< 2,500", "2,500 - 5,000", "5,000-10,000",
                                "10,000-50,000", "50,000-100,000",
                                "100,000-500,000", "500,000-1,000,000",
                                "> 1,000,000"))
    
    interv_level <- levels(intervals)
    
    data <- c()
    
    # Percentage of population for each interval level
    for (j in 1:length(interv_level)){
        
        total_level <- sum(mncp_total[which(intervals == interv_level[j]) ,year], na.rm = TRUE)
        
        percent <- total_level / sum(mncp_total[, year], na.rm = TRUE) * 100
        
        data <- c(data, percent)
    }
    
    cntgcy_table <- rbind(cntgcy_table, data)
}

rm(i, j, data, year, total_level, percent, intervals)

# Transpose contingency table
cntgcy_table <- as.data.frame(t(cntgcy_table))

names(cntgcy_table) <- c(1998:2014)
row.names(cntgcy_table) <- interv_level

rm(interv_level)


# To plot it we need to melt data frame using the levels of the population
cntgcy_table$level <- row.names(cntgcy_table)
melt_cntgcy_table <- melt(cntgcy_table, id = "level")


# Factor and order the levels
melt_cntgcy_table$level <- factor(melt_cntgcy_table$level, 
                                  levels = c("> 1,000,000",
                                             "500,000-1,000,000",
                                             "100,000-500,000",
                                             "50,000-100,000", 
                                             "10,000-50,000",
                                             "5,000-10,000",   
                                             "2,500 - 5,000",
                                             "< 2,500"),
                                  ordered = TRUE)


melt_cntgcy_table$variable <- factor(melt_cntgcy_table$variable,
                                     levels = c(2014:1998),
                                     ordered = TRUE)


########
# Plot #
########

ggplot(melt_cntgcy_table,
       aes(x = variable, y = value,
           fill = level)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = brewer.pal(8, "BrBG"),
                      breaks = rev(levels(melt_cntgcy_table$level)))



###################
# Send to plot.ly #
###################
library(plotly) 
# set_credentials_file("username", "apikey") 
py <- plotly()

pop_percent <- ggplot(melt_cntgcy_table,
                      aes(x = variable, y = value,
                          fill = level)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = brewer.pal(8, "BrBG"),
                      breaks = rev(levels(melt_cntgcy_table$level)))

py$ggplotly(pop_percent)


####################
# Annual variation #
####################

# Empty vector to add annual variation
ann_var <- c()

# Substract the values of each two consecutive years
# 1999 - 1998, 2000 - 1999...
for (i in 1:16){
    
     j = i + 1
     x = cntgcy_table[,j] - cntgcy_table[,i]
     
     ann_var <- c(ann_var, x)
}

rm(i, j, x)

# Transform to data frame
ann_var <- as.data.frame(ann_var)
names(ann_var) <- "pcnt_var"

# Add years
ann_var$year <- rep(c(1999:2014), each = 8)

# Add levels
ann_var$level <- factor(cntgcy_table$level, 
                        levels = c("> 1,000,000",
                                   "500,000-1,000,000",
                                   "100,000-500,000",
                                   "50,000-100,000", 
                                   "10,000-50,000",
                                   "5,000-10,000",   
                                   "2,500 - 5,000",
                                   "< 2,500"),
                        ordered = TRUE)


# Plot annual variation
ggplot(ann_var, aes(x = year, y = pcnt_var, color = level)) +
    geom_line() +
    coord_cartesian(ylim = c(-2, 2)) +
    scale_color_manual(values = brewer.pal(8, "BrBG"))

    geom_rect(xmin = 2007, xmax = 2008, ymin = -2, ymax = 2,
              fill = "gray", alpha = 0.8)


    geom_vline(xintercept = c(2007, 2008), linetype = "dashed")
# The annual variation is at most of 1%, there are no significant changes from year to year.