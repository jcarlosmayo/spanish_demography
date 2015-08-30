library(readxl)
# Load the file names in the data directory
files <- dir("INE_data")

# Empty data frames where to append the information
mncp_total <- data.frame() # Total population in Spanish municipalities
mncp_tmen <- data.frame() # Total MEN population in Spanish municipalities
mncp_twomen <- data.frame() # Total WOMEN population in Spanish municipalities

# Open and extract info from Excel files
for (i in files){
    
    # Create file name
    file <- paste("INE_data", i, sep ="/")
    
    # Read data
    data <- read_excel(file, sheet = 1, skip = 7, col_names = TRUE)
    
    # Transform character data types to numeric
    for (j in 2:ncol(data)){
        if (class(data[,j]) == "character"){
            data[,j] <- as.numeric(data[,j])
        }
    }
    
    # Append province name
    province <- gsub("_1996_2014.xls", "", i)
    
    # Split data by type, total, men and women
    # and append province
    total <- cbind(province, data[, 1:20])
    men <- cbind(province, data[, c(1, 21:39)])
    women <- cbind(province, data[, c(1, 40:58)])
    
    # Append to corresponding data frame
    mncp_total <- rbind(mncp_total, total)
    mncp_tmen <- rbind(mncp_tmen, men)
    mncp_twomen <- rbind(mncp_twomen, women)
}

rm(i, j, file, files, province, total, men, women, data) # Delete redundant variables

# Most of the variable names are numeric, which causes problems, change to 'y' + year

year <- names(mncp_tmen)[3:21]
names(mncp_total) <- c("province", "municipality", paste("y", year, sep=""))
names(mncp_tmen) <- c("province", "municipality", paste("y", year, sep=""))
names(mncp_twomen) <- c("province", "municipality", paste("y", year, sep=""))
rm(year)

# Check that municipality codes are ok
table(substr(mncp_tmen$municipality, 1, 2))
# Revise the codes containing text: "En", "La" and "No"

# EN
y <- which(substr(mncp_tmen$municipality, 0, 2) == "En")
mncp_tmen[y,]

# Delete the values in 'y' in all data frames
mncp_tmen <- mncp_tmen[-y,]
mncp_total <- mncp_total[-y,]
mncp_twomen <- mncp_twomen[-y,]

# LA
y <- which(substr(mncp_tmen$municipality, 0, 2) == "La")
mncp_tmen[y,]
# Take into account the note in Asturias and Madrid:
# Las cifras de 1996 están referidas a 1 de mayo y  las demás a 1 de enero.

# Delete the values in 'y' in all data frames
mncp_tmen <- mncp_tmen[-y,]
mncp_total <- mncp_total[-y,]
mncp_twomen <- mncp_twomen[-y,]

# NO
y <- which(substr(mncp_tmen$municipality, 0, 2) == "No")
mncp_tmen[y,]
# 'No' referes to the 'Notas' field that does not contain any information
mncp_tmen <- mncp_tmen[-y,]
mncp_total <- mncp_total[-y,]
mncp_twomen <- mncp_twomen[-y,]

# There are several NA values corresponding to the few rows between the end of the data
# and the notes in the excel file
# delete them
y <- which(is.na(mncp_tmen$municipality))
mncp_tmen <- mncp_tmen[-y, ]

y <- which(is.na(mncp_total$municipality))
mncp_total <- mncp_total[-y, ]

y <- which(is.na(mncp_twomen$municipality))
mncp_twomen <- mncp_twomen[-y, ]

rm(y)

# Extract ID code to a separate variable
mncp_total$code <- substr(mncp_total$municipality, 0,5)
mncp_tmen$code <- substr(mncp_tmen$municipality, 0,5)
mncp_twomen$code <- substr(mncp_twomen$municipality, 0,5)

# Remove ID code from the municipality variable
mncp_total$municipality <- substr(mncp_total$municipality, 7, 50)
mncp_tmen$municipality <- substr(mncp_tmen$municipality, 7, 50)
mncp_twomen$municipality <- substr(mncp_twomen$municipality, 7, 50)