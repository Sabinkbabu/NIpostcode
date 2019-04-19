                                  # DATA SCIENCE PROJECT               
                                 # CA2- NI POSTCODE AND CRIME DATA


# Sabin Kaleeckal Babu
# MSc in Big Data Analytics (L00144505)


# Importing the .csv file into a dataframe called NI_postcode_data

NI_postcode_data <- read.csv('NIPostcodes.csv', header = FALSE)
NI_postcode_data
# Showing the total number of rows, structure and first 10 rows of the data

nrow(NI_postcode_data)
str(NI_postcode_data)
head(NI_postcode_data, n=10)

# Adding Titles for each attributes of the data

names(NI_postcode_data)[1] <- 'Organisation Name'
names(NI_postcode_data)[2] <- 'Sub-building Name'
names(NI_postcode_data)[3] <- 'Building Name'
names(NI_postcode_data)[4] <- 'Number'
names(NI_postcode_data)[5] <- 'Primary Thorfare'
names(NI_postcode_data)[6] <- 'Alt Thorfare' 
names(NI_postcode_data)[7] <- 'Secondary Thorfare'
names(NI_postcode_data)[8] <- 'Locality'
names(NI_postcode_data)[9] <- 'Townland'
names(NI_postcode_data)[10] <- 'Town'
names(NI_postcode_data)[11] <- 'County'
names(NI_postcode_data)[12] <- 'Postcode'
names(NI_postcode_data)[13] <- 'x-coordinates'
names(NI_postcode_data)[14] <- 'y-coordinates'
names(NI_postcode_data)[15] <- 'Primary Key'

# Recoding the null values with NA because removing the null values results in loss of major part of data which will affect the final outcome of process

NI_postcode_data[NI_postcode_data == ""] <- NA

# Finding the sum and mean of missing values for each attribute in the dataset

sapply(NI_postcode_data, function(missing_data) sum(is.na(missing_data)))
sapply(NI_postcode_data, function(mean_missing_values) mean(is.na(mean_missing_values)))

# Modifying the County attribute to the categorising factor

NI_postcode_data$County <- factor(NI_postcode_data$County, levels = c('ANTRIM', 'ARMAGH', 'DOWN', 'FERMANAGH', 'LONDONDERRY', 'TYRONE'))

# To check whether County attribute is a factor or not

str(NI_postcode_data$Postcode)
levels(NI_postcode_data$County)

# Moving the Primay Key attribute to the start of the dataset

NI_postcode_data <- NI_postcode_data[ , c(15, 1:14)]
head(NI_postcode_data, 5) 

# Creating a new dataset called Limavady_data

Limavady_data <- NI_postcode_data[ which(NI_postcode_data$Locality == 'Limavady', NI_postcode_data$Townland == 'Limavady' 
                         & NI_postcode_data$Town == 'Limavady'), ]

head(Limavady_data, 5)

write.csv(Limavady_data, "C:\\Users\\sabin\\Documents\\NIpostcode\\Limavady.csv", row.names = F)

# Writing the dataset into a csv file

write.csv(NI_postcode_data, "C:\\Users\\sabin\\Documents\\NIpostcode\\CleanNIPostcodeData.csv", row.names = F)

str(Limavady_data)

# To choose the files in the working directory

location_path <- choose.files(default = "", caption = "Select files", multi = TRUE, filters = Filters, index = nrow(Filters))
location_path

# Converting the csv ciles into a single dataframe using base R functions

AllNICrimeData = do.call(rbind, lapply(location_path, function(crime_data) read.csv(crime_data, stringsAsFactors = FALSE)))
AllNICrimeData  

# Saving the dataset into a csv file called AllNICrimeData

write.csv(AllNICrimeData, "C:\\Users\\sabin\\Documents\\NIpostcode\\AllNICrimeData.csv", row.names = F ) 

# Showing the number of rows in AllNICrimeData

AllNICrimeData <- read.csv("C:\\Users\\sabin\\Documents\\NIpostcode\\AllNICrimeData.csv", header = TRUE)

nrow(AllNICrimeData)

str(AllNICrimeData)

# Removing the unwanted attributes

AllNICrimeData[, c('Crime.ID', 'Reported.by', 'Falls.within', 
                     'LSOA.code', 'LSOA.name', 'Last.outcome.category', 
                     'Context')] <- list(NULL)
AllNICrimeData

str(AllNICrimeData)

# Factorising the Crime type attribute

AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)

str(AllNICrimeData)

# Removing the 'On or near' phrase from the Location attribute value

AllNICrimeData$Location <- gsub( "On or near ", "", AllNICrimeData$Location)

AllNICrimeData$Location <- factor(AllNICrimeData$Location)

# Assigning NA's to empty Location attributes

AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA

sum(is.na(AllNICrimeData$Location))

# creating a subset of AllNICrimeData with location not contains NA

new_data <- subset(AllNICrimeData, !is.na(Location), select = c(Month, Longitude, Latitude, Location, Crime.type))
new_data
sum(is.na(new_data$Location))

# selecting 1000 random samples and saving it to a new data frame called random_crime_sample

random_crime_sample <- new_data[sample(1:nrow(new_data), 1000, replace = FALSE),]  
random_crime_sample

nrow(random_crime_sample)

# Importing cleanNIPostcode data for the reference of Postcodes

library(dplyr)

cleanNIPostcode <- read.csv("C:\\Users\\sabin\\Documents\\NIpostcode\\CleanNIPostcodeData.csv")

# Inorder to compare the primary.Thorfare and Location attributes both of them should be of same structure and string format
# so changing both of the attributes to uppercase string format

random_crime_sample$Location <- toupper(random_crime_sample$Location)
cleanNIPostcode$Primary.Thorfare <- toupper(cleanNIPostcode$Primary.Thorfare)
head(random_crime_sample, 2)
head(cleanNIPostcode, 2)

# creating a new_postcode dataset with Primary thorfare and Postcode attributes from cleanNIPostcode datset

new_postcode <- cleanNIPostcode[, c(6, 13)]
head(new_postcode, 10)

# Finding the popular postcode using ddpply function

library(plyr)
new_postcode <- ddply(new_postcode, .(Primary.Thorfare, Postcode), nrow)
head(new_postcode, 10)
str(new_postcode)

# Assigning the column names to dataset

colnames(new_postcode) <- c("Primary.Thorfare", "Postcode", "Number_of_Postcodes")

# Removing duplicates in the new_postcode data  

new_postcode <- new_postcode[!duplicated(new_postcode$Primary.Thorfare),]

# to check that postcode function has got any null values

sum(is.na(new_postcode$Postcode))

# creating a new postcode data without any null values for postcodes

new_postcode <- subset(new_postcode, !is.na(Postcode), select = c(Primary.Thorfare, Postcode, Number_of_Postcodes))
new_postcode

# finding the postcodes available for the locations
# Shwoing the number of postcodes available for locations
# to find the popularity

new_postcode <- ddply(new_postcode, .(Primary.Thorfare, Postcode), nrow)
colnames(new_postcode) <- c("Primary.Thorfare", "Postcode", "Number_of_Postcodes")
str(new_postcode)

# Creating a new column called Postcode in random_crime_sample

random_crime_sample$Postcode <- NA
head(random_crime_sample, 5)

# Adding values to the Postcode attribute
# It only adds the when the location and primary thorfare attributes of both datasets match each other

random_crime_sample$Postcode <- new_postcode$Postcode[match(random_crime_sample$Location, new_postcode$Primary.Thorfare)]
str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 5)

# Saving the random crime sample dataset to an external csv file

write.csv(random_crime_sample, 'C:\\Users\\sabin\\Documents\\NIpostcode\\random_crime_sample.csv') 

# creating datasets

updated_random_sample <- random_crime_sample 
chart_data <- updated_random_sample

# Sorting the data with respect to Postcode and Crime type

chart_data[order(chart_data$Postcode == 'BT1', chart_data$Crime.type),  ]
chart_data

# Filtering the data to a new chart dataset which contains postcode = 'BT1'

new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type),  ]
str(new_chart)

# Summary of crime type with respect to Postcode = 'BT1'

BT1_summary_chart <- as.data.frame(summary(new_chart$Crime.type))
BT1_summary_chart

# Creating the barplot for crime type in chart_data with postcode = BT1

barplot(BT1_summary_chart[,1], col='red' , las=1, names.arg="",main = "Crime Summary for Postcode BT1", xlab = 'Crime type',
                     ylab="Crime Count")
text(BT1_summary_chart[,1], labels = rownames(BT1_summary_chart))





