library(Hmisc)
library(readxl)
library(dplyr)
library(ggplot2)

bacData <- read.csv("BacInscriere2014_sesiunea_I_0_0_copy.csv", sep = "\t", dec = ",")
summary(bacData)

schoolCodes <- read_excel("2013_04_02_Codurile_SIRUES.xls")
summary(schoolCodes)
str(schoolCodes)

cities <- read.csv("orase.csv", stringsAsFactors = FALSE)

# Convert school codes to numeric and capitalize the city names
schoolCodesCleaned <- schoolCodes %>%
    mutate(SIRUES_CODE = as.numeric(SIRUES_CODE),
           CITY = capitalize(tolower(DESCRIPTION))) %>%
    select(-DESCRIPTION)


bacSchoolData <- left_join(bacData, schoolCodesCleaned,
                           by = c("Unitate..SIRUES." = "SIRUES_CODE"))

# Have all the school codes been found?
sum(is.na(bacSchoolData$CODE))

citiesCleaned <- mutate(cities, NUME = capitalize(tolower(NUME)))

fullDataSet <- left_join(bacSchoolData, citiesCleaned, by = c("CITY" = "NUME"))

# How many cities were not matched?
sum(is.na(fullDataSet$JUDET))

# Too many to ignore...
ct <- fullDataSet %>%
    filter(is.na(fullDataSet$JUDET)) %>%
    select(CITY) %>%
    distinct()

# Remove special characters from city names
bacSchoolDataCleaned <- bacSchoolData %>%
    mutate(CITY = gsub("â", "a", CITY)) %>%
    mutate(CITY = gsub("Î", "I", CITY)) %>%
    mutate(CITY = gsub("Bucuresti sectorul \\d", "Municipiul bucuresti", CITY))

fullDatasetAllCities <- left_join(bacSchoolDataCleaned, citiesCleaned,
                                  by = c("CITY" = "NUME", "CODE" = "JUDET.AUTO"))

sum(is.na(fullDatasetAllCities$JUDET))

# Convert some columns to factors for easier analysis
cleanDataset <- fullDatasetAllCities %>%
    mutate(CITY = as.factor(CITY),
           JUDET = as.factor(JUDET))


sum(is.na(cleanDataset$Medie))

# What is the overall passing rate?
mean(!is.na(cleanDataset$Medie))

# What is the distribution of the final grades?
ggplot(cleanDataset, aes(x = Medie)) +
    scale_x_continuous(breaks = seq(5, 10, 0.5)) +
    geom_histogram(binwidth = 0.25, color = "black", fill = "#5760AB") +
    ggtitle("Final Grades") +
    xlab("Grade")

# Is there a difference between the final grades by sex?
ggplot(cleanDataset, aes(x = Sex, y = Medie)) +
    geom_boxplot(binwidth = 0.25, color = "black", fill = "gray") +
    ggtitle("Grades by Sex") +
    ylab("Grade")

# Can we get a clearer representation?
ggplot(cleanDataset, aes(x = Medie)) +
    geom_density(aes(fill = Sex), color = "black", alpha = 0.5) +
    ggtitle("Density Curves")

# What can we say about those who failed?
ggplot(cleanDataset, aes(x = Sex)) +
    geom_bar(aes(fill = Sex), color = "black") +
    ggtitle("Failures by Sex")

# Now let's aggregate the data by county
countyData <- cleanDataset %>%
    group_by(JUDET) %>%
    summarise(total = n(),
              passingRate = sum(!is.na(Medie)) / n(),
              averageGrade = mean(Medie, na.rm = T)) %>%
    arrange(desc(passingRate))

# What are the counties with the highest passing rate?
head(countyData)

# What are the schools with the highest passing rate?
schoolData <- cleanDataset %>%
    group_by(LONG_DESCRIPTION) %>%
    summarise(total = n(),
              passingRate = sum(!is.na(Medie)) / n(),
              averageGrade = mean(Medie, na.rm = T),
              city = first(CITY),
              county = first(JUDET)) %>%
    filter(total > 100) %>%
    arrange(desc(passingRate), desc(averageGrade))

head(schoolData)

# How about cities?
cityData <- cleanDataset %>%
    group_by(CITY) %>%
    summarise(total = n(),
              passingRate = sum(!is.na(Medie)) / n(),
              averageGrade = mean(Medie, na.rm = T)) %>%
    filter(total > 100) %>%
    arrange(desc(passingRate))

head(cityData)
