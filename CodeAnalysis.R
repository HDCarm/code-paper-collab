# Delete the whole environment by deleting a list that includes everything
rm(list = ls())
# Get the current working directory
oldwd <- getwd()
# Get the path of the current .R file and set it as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####################################
##### ACTIVATE NEEDED PACKAGES #####
####################################

# List of packages to install and activate
packages <- c("readxl","tidyverse","writexl","forcats","cowplot","extrafont")
# Loop through the packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) { # if the package is not installed, install it
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE) # activate package
}
rm(packages, pkg)

#####################
#### IMPORT DATA ####
#####################

gdpcapita <- read_excel("datafiles/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_4901640.xls", "Data", skip=3) # import Data sheet skipping the first 3 rows
  gdpcapita <- rename (gdpcapita, ctr = "Country Code",#rename country code to ctr
                       country = "Country Name", #rename country name to country
                       GDPPC = "2020") #rename 2020 gdp/capita to have text in the header

data <- read_excel("datafiles/List of countries.xlsx") 
  data <- rename (data, sdgscore = "2020 SDG Index Score",
                  country = "Jurisdiction") # rename 2020 SDG Index Score to sdgscore
  data[data==0] <- NA # replace 0 values with NA
  data <- na.omit(data) # delete all rows containing NAs
  #Check whether data remains unmatched by country  
  missing <- subset(data, !(country %in% gdpcapita$country)) # are observations in data that are not in gdpcapita
  data$country <- gsub("Czech Republic", "Czechia", data$country) # Replacing Czeck Republic with Czechia
  data$country <- gsub("Turkey", "Turkiye", data$country) # Replacing Turkey with Turkiye
  rm(missing) # delete missing variable 
  # Rename Regions according to O-Ponce
  data$Region <- gsub("South-Eastern Asia|Southern Asia", "South-Southeastern Asia", data$Region) # replace South-Eastern Asia and Southern Asia with South-Southeastern Asia
  data$Continent <- gsub("Caribbean|Latin America", "Latin American (inc. the Caribbean)", data$Continent) # replace Caribbean and Latin America with Latin American (inc. the Caribbean)
  data$Continent <- gsub("Europe", "European", data$Continent) # replace Europe with European
  data$Classification <- gsub("Transition","In transition",data$Classification)

data <- merge(data, gdpcapita[, c("country","GDPPC") ], by="country") #merge dataframes (only using country and GDPPC column)
rm(gdpcapita) # delete redundant dataframe 

sdg2020 <- read_excel("datafiles/SDR2020Database.xlsx", "SDR2020 Data")
  names(sdg2020) <- tolower(names(sdg2020)) # turn all columnames to lower capitals
  sdg2020 <- rename (sdg2020, ctr = "id", # rename id with ctr
                     sdg2020score = "2020 sdg index score") # rename 2020 SDG index score with sdg2020score
  sdg2020 <- sdg2020[, c(1:3, 584:600)] # extract columns with country data and sdg scores (deleting more granular data)
  colnames(sdg2020) <- gsub(" ", "", colnames(sdg2020)) # deleting all spaces in the columnames
  # Check whether data remains unmatched by country  
  missing <- subset(data, !(country %in% sdg2020$country)) # are observations in data that are not in sdg2020
  sdg2020$country <- gsub("Czech Republic", "Czechia", sdg2020$country) # Replacing Czeck Republic with Czechia
  sdg2020$country <- gsub("Turkey", "Turkiye", sdg2020$country) # Replacing Turkey with Turkiye
  rm(missing) # delete missing variable 

data <- merge(data, sdg2020, by = "country") #merge data and sdg data 
World <- data # write data into World in order to have the result output-labelling correct
rm(sdg2020, data) # delete redundant datasets

# add the venezuelian gdp data :https://www.imf.org/en/Publications/WEO/weo-database/2022/October/weo-report?c=299,&s=NGDPDPC,&sy=2020&ey=2020&ssm=1&scsm=1&scc=0&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1
World$GDPPC[World$ctr == "VEN"] <- 1608.284 # write into column GDPPC row (ctr=VEN)


# Import SDG Mapping
sdgmapping <- read_excel("datafiles/sdgmapping.xlsx")
sdgmapping$sdgcat <- str_replace_all(sdgmapping$sdgcat, c("economic"="Economy", "social"="Society", "environmental"="Biosphere")) # replacing the first with th second

# Import clustermapping  
clustermapping <- read_excel("datafiles/clustermapping.xlsx")
abbreviations <- paste0(as.character(clustermapping$clustershort),": ",as.character(clustermapping$cluster),", ", sep="", collapse="")

############################
##### DATASET CREATION #####
############################

# write a list with all columnnames that should be used to create filtered datasets in the following
columns <- c("Region", "Continent", "Classification", "Income") 
# create an empty list for subsets to be created
subset_names <- list()

# loop through every column in the columns list
for (column in columns) {
  #  extracting a list with unique values in each of the respective columns in the World data
  unique_values <- unique(World[, column])
  
  # loop through every value in the unique_values list
  for (value in unique_values) {
    # creating a dataset that is filtered based on the current column and respective value
    subset_name <- paste0(column, "_", value) # create a name for the dataset based on column and unique value
    assign(subset_name, World[World[, column] == value, ]) # assign the subset the filtered data
    
    # add the created dataset_name to the list of dataset_names
    subset_names <- c(subset_names, subset_name)
  }
}
#create 'missing' lower-middle-low income set
`Income_Lower-middle-Low` <- rbind(`Income_Lower-middle`, Income_Low)
# add the World dataset name to the dataset_names right in the beginning
subset_names <- append(subset_names, "World", after = 0)
subset_names <- append(subset_names, "Income_Lower-middle-Low")

# delete working variables used in the last loop
rm(column, columns, subset_name, unique_values, value)

##############################
#### AUTOMATIC REGRESSION ####
##############################

# write a list with all dependent variables
dependent_vars <- c("sdgscore",paste0("goal", 1:17, "score")) # the paste0 function writes the character combination for the numbers 1 to 17
independent_vars <- "~ PD + IND + MAS + UNA + LTO + IVR + log(GDPPC)" # write the static independent part
# create a dataframe for the results
result <- data.frame()
# loop through each dependent variable and data frame
for (dep_var in dependent_vars) {
  for (subset_name in subset_names) {
    # Get the subset data frame
    subset_df <- get(subset_name)
    
    # check if the dependent variable in the subset has any non-NA values in the subset data frame
    if (sum(!is.na(subset_df[[dep_var]])) == 0) {
      # Skip this regression if there are no non-NA values / if there are ONLY NAs
      next
    }
    
    # Perform the regression
    formula <- as.formula(paste(dep_var, independent_vars)) # creating a formula by pasting together dependent and independent variable
    reg <- lm(formula, data = subset_df)
    
    #RESULT EXTRACTION
    coefficients <- coef(summary(reg)) # extract coefficients from the dummary of the regression (more data tat coef)
    coefficient <- round(coefficients[,1], 2) # round the coeffients to 2 digits after the comma
    significance <- ifelse(coefficients[, 4] < 0.001, "***", ifelse(coefficients[, 4] < 0.01, "**", ifelse(coefficients[, 4] < 0.05, "*", ""))) #rebuilding the * significance display
    r_squared <- summary(reg)$r.squared # extracting the R^2
    r_squared <- round(r_squared, digits=3)# round the R^2 to 3 digits
    # if the fstatistic is not NUll, we extract the latter (might be Null if completely insignificant or too little observations in a subset)
    if (!is.null(summary(reg)$fstatistic)) {
      fstat <- t(summary(reg)$fstatistic)
      fstat <- round(fstat, digits=3) # round the fstat to 3 digits
      qf <- qf(0.95, fstat[1,2], fstat[1,3]) # calculate the critical F-value (sign. level, degrees of freedom numerator, degrees of freedom denumerator)
      qf <- round(qf, digits = 3) # round the critical F-value to 3 digits after the comma
      qf <- as.data.frame(qf)} # transform into dataframe
    extract <- cbind(coefficient, significance) # put columns together
    extract <- as.data.frame(extract) # transform into dataframe
    extract <- unite(extract, reg, coefficient, significance, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
    extract <- t(extract) # transpose extract
    extract <- as.data.frame(extract) # transform into dataframe
    name <- paste0(dep_var, "_", subset_name) # chain dependent variable and subset name
    name <- as.data.frame(name) # transform into dataframe
    n <- count(subset_df) # count rows (sample size) 
    ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
           extract <- cbind(name, n, extract,r_squared, fstat, qf), 
           extract <- cbind(name, n, extract, r_squared))
    
    result <- bind_rows(result, extract) # add the current extract to the result
    fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
    rm(coefficients, coefficient, significance, r_squared, fstat, extract, qf, name, n) # remove working variables from the loop
  }
}
# Delete working dataframes
dataset_names <- ls(pattern = "Region|Continent|Income|Classification")
for (name in dataset_names) {
  rm(list=name)}
#remove working variables
rm(dep_var, dependent_vars, formula, subset_name, subset_df, subset_names, independent_vars, dataset_names, reg, name) # delete working variables for the loop


###############################
##### RESULT MANIPULATION #####
###############################

# enrich the result dataframe with the sdg mapping
result$sdgcat <- sdgmapping$sdgcat[match(gsub("\\D", "", result$name), sdgmapping$sdggoal)]# write into result$sdgcat the value that is found by matching the number in result$name with sdgmapping$sdggoal
result$sdgname <- sdgmapping$sdgname[match(gsub("\\D", "", result$name), sdgmapping$sdggoal)] # gsub deletes all non-numeric values in result$name

result <- rename(result, F = "value")

#extract significant predictors into one column for easier reporting
result$significant_predictors <- paste0( 
  ifelse(grepl("\\*", result$PD), paste0("PD: ", result$PD, ", "), ""), #if the value in the column contains a* copy the column name and value with a comma
  ifelse(grepl("\\*", result$IND), paste0("IND: ", result$IND, ", "), ""),
  ifelse(grepl("\\*", result$MAS), paste0("MAS: ", result$MAS, ", "), ""),
  ifelse(grepl("\\*", result$UNA), paste0("UNA: ", result$UNA, ", "), ""),
  ifelse(grepl("\\*", result$LTO), paste0("LTO: ", result$LTO, ", "), ""),
  ifelse(grepl("\\*", result$IVR), paste0("IVR: ", result$IVR, ", "), ""),
  ifelse(grepl("\\*", result$`log(GDPPC)`), paste0("log(GDPPC): ", result$`log(GDPPC)`, ", "), "")
)

result$significant_predictors <- sub(",([^,]*)$", "", result$significant_predictors) # looks for a comma followed by any number of non-comma characters at the end of the string. We then replace this with an empty string
#extract dependent variable and cluster name from name column
result$depvar <- gsub("_.*", "", result$name) # replace every character (.*) after the _ with ""
result$group <- stringr::str_extract(result$name, "(?<=_)[^_]*(?=_?)") # everything and including the first underscore. until the next underscore or nothing
result$cluster <- gsub(".*_", "", result$name) # replace every character (.*) before the _ with ""
result$cluster <- gsub("NA", "", result$cluster) # delete the NA#s produces due to the world observations with only one _

## extracting only significant models
result1 <- subset(result, F > qf) #extracting models with significant f-value (f-value<critical f-value)
if(all(is.na(result1$V1))) { # Löschen von Spalte V1, wenn sie nur NA enthält
  V1col <- which(colnames(result) == "V1") # extract colnumber for V1
  result1 <- subset(result1, select = -V1col) # overwrite result1 with subset of itself without V1 column
  rm(V1col) # delete working variable
}

#extract result with only sign. coef
result2 <- result1
for (i in c("(Intercept)","PD","IND","UNA","LTO","MAS","IVR","log(GDPPC)")) {
  result2[[i]][!grepl("\\*", result2[[i]])] <- ""
}
rm(i)
## extracting a slim result
result3 <- result1[, c("depvar","cluster","r_squared","significant_predictors","F") ]

## extract SDG category dataset
result4 <- result1[,c("sdgcat","depvar","cluster","PD","IND","UNA","LTO","MAS","IVR","log(GDPPC)")]
if(all(is.na(result4$cluster))) {# if there are NA's produced by the cluster matching the code stops
  stop("Missing Clusters")
}
result4$cluster <- clustermapping$clustershort[match(result4$cluster, clustermapping$cluster)]# write into result4$cluster the value that is found by matching result$cluster with clustermapping$clustershort
result4$depvar <- paste0("#",gsub("\\D", "", result4$depvar)) # overwrite depvar with # and everything but text in the cell

for (i in c("PD", "IND", "UNA", "LTO", "MAS", "IVR", "log(GDPPC)")) { # running a loop over all coefficient columns
  result4[[i]] <- ifelse(grepl("\\*", result4[[i]]), ifelse(result4[[i]] > 0,paste0("+",result4$cluster),paste0("-",result4$cluster)), "")
  #if the value contains * then next loop or "". next loop, if positiv -> "+" and cluster otherwise "-" and cluster
}
rm(i)

result4 <- filter(result4, !is.na(sdgcat)) # delete rows with NA in sdgcat (World values)
result4$cluster <- NULL # delete cluster column

result4 <- mutate(result4, depvar = factor(depvar, levels = c(paste0("#", 1:17)))) # turning depvar into a factor and specifying the order
result4 <- arrange(result4, depvar) #ordering accoring to depvar / no nee as depvar is created in the correct order bc of the loop
result4 <-result4 %>%
  group_by(sdgcat, depvar) %>%
  summarize(across(c(PD, IND, MAS, UNA, LTO, IVR, `log(GDPPC)`),
                   ~ paste(.x[.x != ""], collapse = ", ")))

#extract cluster dataset
result5 <- result1[,c("name", "cluster","n")] # specify columns to be extracted
result5 <- result5[!duplicated(result5$cluster),] # overwrite with everything but the dublicated values
# extract test between first and sexond underscore if the formula <> NA else ""
result5$name <- ifelse(is.na(str_extract(result5$name, "(?<=_)[^_]+(?=_)")),"",str_extract(result5$name, "(?<=_)[^_]+(?=_)")) #(?<=_) everything up to and including the first underscore [^_]+ everthing after the first underscore (?=_) ends the match with the underscore 
result5 <- mutate(result5, name = factor(name, levels = c("Income","Classification","Continent","Region"))) # turning depvar into a factor and specifying the order
result5 <- arrange(result5, name) #ordering accoring to depvar / no nee as depvar is created in the correct order bc of the loop

# Delete working dataframes
rm(clustermapping, sdgmapping)
#Export resulttables
dataset_names <- lapply(ls(pattern = "result"), get) # gets all the dataframes with the word result in it 
write_xlsx(dataset_names,"output/result.xlsx")  # writes all the dtaframe sin the list into the stated file
rm(dataset_names) # deletes the working variable

#################
##### PLOTS #####
#################

# SDG x GDPPC logarithmic transformation differences (might or might not use the smooth variant)
sdgxloggdpc <- ggplot(World) + aes(log(GDPPC),sdg2020score) + 
  geom_point() + 
  geom_smooth(method=lm, level=0.95, colour="darkgray") + 
  ylim (40,100)  + 
  ggtitle("SDG score x log of Gross domestic product per capita") +
  theme_linedraw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10)) 

sdgxgdpc <- ggplot(World) + aes(GDPPC,sdg2020score) + 
  geom_point() + 
  geom_smooth(method=lm, level=0.95, colour="darkgray") + 
  ylim (40,100) + 
  ggtitle("SDG score x Gross domestic product per capita") + 
  theme_linedraw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10)) 
plot_sdgGDPPClm <- plot_grid(sdgxgdpc,sdgxloggdpc)
ggsave(filename = "output/plot_sdgGDPPClm.png", 
       plot = plot_sdgGDPPClm, 
       device = "png", 
       width = 8.5, height = 4.25)
rm(sdgxgdpc,sdgxloggdpc)


setwd(oldwd) # set the working directory to the initial value
rm(oldwd)
