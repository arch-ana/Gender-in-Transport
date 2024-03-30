#######CLEANING CENSUS DATA######

# Installing required packages
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyverse")

# Loading libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(rstudioapi)

# Setting work directory automatically
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# Uncomment to set working directory manually
# setwd("C:/Users/Asus/OneDrive/Desktop/WRI/TechnicalAssignmentR")

# Loading Excel data of state and changing it to a dataframe
city_data <- read_excel("Maharashtra.xlsx")
city_data <- data.frame(city_data)

# Naming variables
names(city_data)[1] <- 'table_name'
names(city_data)[2] <- 'state_code'
names(city_data)[3] <- 'district_code'
names(city_data)[4] <- 'tot_rur_urb'
names(city_data)[5] <- 'area_name'
names(city_data)[6] <- 'mode'
names(city_data)[7] <- 'persons_total'
names(city_data)[8] <- 'male_total'
names(city_data)[9] <- 'female_total'
names(city_data)[10] <- 'persons_notravel'
names(city_data)[11] <- 'male_notravel'
names(city_data)[12] <- 'female_notravel'
names(city_data)[13] <- 'persons_0-1'
names(city_data)[14] <- 'male_0-1'
names(city_data)[15] <- 'female_0-1'
names(city_data)[16] <- 'persons_2-5'
names(city_data)[17] <- 'male_2-5'
names(city_data)[18] <- 'female_2-5'
names(city_data)[19] <- 'persons_6-10'
names(city_data)[20] <- 'male_6-10'
names(city_data)[21] <- 'female_6-10'
names(city_data)[22] <- 'persons_11-20'
names(city_data)[23] <- 'male_11-20'
names(city_data)[24] <- 'female_11-20'
names(city_data)[25] <- 'persons_21-30'
names(city_data)[26] <- 'male_21-30'
names(city_data)[27] <- 'female_21-30'
names(city_data)[28] <- 'persons_31-50'
names(city_data)[29] <- 'male_31-50'
names(city_data)[30] <- 'female_31-50'
names(city_data)[31] <- 'persons_51+'
names(city_data)[32] <- 'male_51+'
names(city_data)[33] <- 'female_51+'
names(city_data)[34] <- 'persons_notstated'
names(city_data)[35] <- 'male_notstated'
names(city_data)[36] <- 'female_notstated'

# Removing unnecesary data
city_data <- subset (city_data, select = -c(1:3, 7:12))
city_data <- city_data[-c(1,2,3,4,5), ]

# Converting required character columns to numeric columns
city_data <- city_data %>% 
  mutate_at(c(4:27), as.numeric)

# Filtering rows and columns to get required data
city_data <- city_data %>%
  filter((area_name == "Mumbai" | area_name == "Mumbai Suburban"
          | area_name == "Nashik" | area_name == "Pune" | area_name == "Nagpur"
          | area_name == "Thane") & (tot_rur_urb == "Urban"))

city_data <- city_data %>%
  filter(mode != "All Modes" & mode != "No travel")

# Removing data on persons as it repeats as 'male' and 'female'
city_data <- dplyr::select(city_data, -dplyr::contains("persons_"))

# Tidying data by converting it from wide form to long form using pivot function
city_data <- city_data %>% pivot_longer(
  cols= "male_0-1" : "female_notstated",
  names_to = c("gender", "distance"), 
  names_pattern = "(male|female)_([0-9]+\\-[0-9]+|[0-9]+\\+|notstated)",
  values_to = "count"
)

#View(city_data)

#####VISUALIZING DATA FROM CITY_DATA#####

#Function to create a percentage stacked barplot for a city that takes three arguments-
# name of city, gender and title of the barplot

visualize <- function (city, gen, title) {
  
  # Filtering city data for specific cities and gender combinations
  viz_one <- city_data %>%
    filter(area_name == city & gender == gen & distance != "notstated")
  
  # Removing redundant variables (tot_rur_urb, area_name, gender)
  viz_one <- subset (viz_one, select = c(3,5,6))
  
  # Pivot data to construct percentage stacked barplot with distance on the y axis
  viz_one <- viz_one %>%
    pivot_wider(names_from = distance, values_from = count)
  
  # Removing additional column with serial numbers
  viz_one <- subset (viz_one, select = -c(1))
  
  # Assigning row names as per required in visualization
  rownames(viz_one) <- c("On foot", "Bicycle", "MTW", "Car", "IPT/Taxi", "Bus", "Train", "Water transport", "Others")
  
  # Transform this data in %
  data_percentage <- apply(viz_one, 2, function(x){x*100/sum(x,na.rm=T)})
  
  # Make a stacked barplot
  barplot(data_percentage, col=brewer.pal(9, "Paired") , border="white", legend = TRUE,
          xlab="Distance Travelled", ylab= "Mode Wise Percentage of Travellers",
          xlim=c(0, ncol(data_percentage) + 4), 
          main = title,
          legend.text=TRUE,
          args.legend=list(
          x=ncol(data_percentage) + 4,
          y=max(colSums(data_percentage)),
          bty = "n"))
}

#Un-comment required city and gender for specific visualizations

#visualize("Nashik", "female", "Nashik Female")
#visualize("Nashik", "male", "Nashik Male")
#visualize("Mumbai Suburban", "female", "Mumbai Suburban Female")
#visualize("Mumbai Suburban", "male", "Mumbai Suburban Male")
#visualize("Mumbai", "female", "Mumbai Female")
#visualize("Mumbai", "male", "Mumbai Male")
#visualize("Nagpur", "female", "Nagpur Female")
#visualize("Nagpur", "male", "Nagpur Male")
#visualize("Thane", "female", "Thane Female")
#visualize("Thane", "male", "Thane Male")
#visualize("Pune", "female", "Pune Female")
#visualize("Pune", "male", "Pune Male")