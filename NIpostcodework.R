# Importing the .csv file into a dataframe called NI_postcode_data

NI_postcode_data <- read.csv('NIPostcodes.csv', header = F)
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

AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA

sum(is.na(AllNICrimeData$Location))

new_data <- subset(AllNICrimeData, !is.na(Location), select = c(Month, Longitude, Latitude, Location, Crime.type))
new_data
sum(is.na(new_data$Location))

random_crime_sample <- new_data[sample(1:nrow(new_data), 1000, replace = FALSE),]  
random_crime_sample

nrow(random_crime_sample)

library(dplyr)

cleanNIPostcode <- read.csv("C:\\Users\\sabin\\Documents\\NIpostcode\\CleanNIPostcodeData.csv")

# In order to compare change both Primary_thorfare and Location to upper case

random_crime_sample$Location <- toupper(random_crime_sample$Location)
cleanNIPostcode$Primary.Thorfare <- toupper(cleanNIPostcode$Primary.Thorfare)
head(random_crime_sample, 2)
head(cleanNIPostcode, 2)

new_postcode <- cleanNIPostcode[, c(6, 13)]
head(new_postcode, 10)

library(plyr)
new_postcode <- ddply(new_postcode, .(Primary.Thorfare, Postcode), nrow)
head(new_postcode, 10)
str(new_postcode)

colnames(new_postcode) <- c("Primary.Thorfare", "Postcode", "Number_of_Postcodes")

# Removing duplicates in the new_postcode data  

new_postcode <- new_postcode[!duplicated(new_postcode$Primary.Thorfare),]
sum(is.na(new_postcode$Postcode))
new_postcode <- subset(new_postcode, !is.na(Postcode), select = c(Primary.Thorfare, Postcode, Number_of_Postcodes))
new_postcode
new_postcode <- ddply(new_postcode, .(Primary.Thorfare, Postcode), nrow)
colnames(new_postcode) <- c("Primary.Thorfare", "Postcode", "Number_of_Postcodes")
str(new_postcode)
# Creating a new column called Postcode in random_crime_sample

random_crime_sample$Postcode <- NA
head(random_crime_sample, 5)

# Adding values to the Postcode attribute 

random_crime_sample$Postcode <- new_postcode$Postcode[match(random_crime_sample$Location, new_postcode$Primary.Thorfare)]
str(random_crime_sample)
nrow(random_crime_sample)

write.csv(random_crime_sample, 'C:\\Users\\sabin\\Documents\\NIpostcode\\random_crime_sample.csv') 

updated_random_sample <- random_crime_sample 
chart_data <- updated_random_sample

# Sorting the data with respect to Postcode and Crime type

chart_data[order(chart_data$Postcode == 'BT1', chart_data$Crime.type),  ]
chart_data

# Creating a new chart dataset which contains postcode = 'BT1'

new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type),  ]
str(new_chart)

# Summary of crime type with respect to Postcode and location

crime_type <- data_frame(new_chart$Crime.type)
crime_type <- ddply(crime_type, .(new_chart$Crime.type), nrow)
colnames(crime_type) <- c('Crime_type', 'Count')
crime_type

# Creating the barplot

crime_data <- table(chart_data$Crime.type) 
barplot(crime_data, main = 'Frequency of crime type',
        xlab = 'Crime type',
        ylab = 'Frequency',
        col= "blue")


