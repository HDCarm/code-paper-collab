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
packages <- c("readxl","tidyverse","writexl")
# Loop through the packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
rm(packages, pkg)


#####################
#### IMPORT DATA ####
#####################

vsm <- read_excel("datafiles/6-dimensions-for-website-2015-08-16 adapt.xls")
  vsm <- filter(vsm, !is.na(pdi))
  vsm <- filter(vsm, !is.na(ltowvs))
  vsm <- filter(vsm, !is.na(ivr))
  vsm <- rename(vsm, PD = pdi,IND = idv, MAS= mas, UNA =uai, LTO=ltowvs,IVR=ivr)

Ponce <- read_excel("datafiles/List of countries.xlsx")
  Ponce <- rename (Ponce, sdgscore = "2020 SDG Index Score")
  Ponce[Ponce==0] <- NA
  Ponce <- na.omit(Ponce)
  Ponce <- rename (Ponce, country = "Jurisdiction")

SDG2020 <- read_excel("datafiles/SDR2020Database.xlsx", "SDR2020 Data")
  names(SDG2020) <- tolower(names(SDG2020))
  SDG2020 <- rename (SDG2020, ctr = "id", sdg2020score = "2020 sdg index score")  

# Merge Dataframes
data <- merge(SDG2020, vsm, by="ctr")
data2 <- merge(SDG2020, Ponce, by="country")
rm(vsm, Ponce, SDG2020)

############################################
#### REGRESSIONEN & RESULT MANIPULATION ####
############################################

result <- data.frame() # define result as data.frame to use it later

# sdg score dependent on vsm (n=60)
reg <- lm(sdg2020score ~ PD + IND + MAS + UNA + LTO + IVR, data)
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
n <- count(data) # count rows (sample size) 
ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
       extract <- cbind(n, extract,r_squared, fstat, qf), 
       extract <- cbind(n, extract, r_squared))
result <- bind_rows(result, extract) # add the current extract to the result
fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
rm(coefficients, coefficient, significance, r_squared, fstat, extract, qf, reg) 

# sdg score dependent on Ponce n=90
reg <- lm(sdg2020score ~ PD + IND + MAS + UNA + LTO + IVR, data2)
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
n <- count(data2) # count rows (sample size) 
ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
       extract <- cbind(n, extract,r_squared, fstat, qf), 
       extract <- cbind(n, extract, r_squared))
result <- bind_rows(result, extract) # add the current extract to the result
fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
rm(coefficients, coefficient, significance, r_squared, fstat, extract, qf, reg) 

# add the regression results by Ordonez-Ponce (2022) manually 
extract <- data.frame(
  n=114,
  PD=-0.07,
  IND=0.15,
  UNA=0.11,
  LTO=0.09,
  r_squared=0.53,
  value=19.910,
  numdf=6,
  dendf=108,
  qf = 0
)
  extract$qf <- qf(0.95, extract$numdf, extract$dendf)
  
# aligning data types
extract <- as.data.frame(lapply(extract, as.character), stringsAsFactors = FALSE)
result <- as.data.frame(lapply(result, as.character), stringsAsFactors = FALSE)

# add the extract to the result
result <- (bind_rows(result, extract))
  result <- rename (result, '(Intercept)' = "X.Intercept.")
  rm(extract, n)

# export the result as xlsx
write_xlsx(result,"output/resultComparison.xlsx")

setwd(oldwd) # set the working directory to the initial value
rm(oldwd)
