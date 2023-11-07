#create a matrix
fruits <- matrix(c(1, 2, 1, 3, 1, 1, 3, 1, 2),
                 ncol = 3)

#or
fruits2 <- rbind(c(1 ,2, 1),
                c(3, 1, 1),
                c(3, 1, 2))

#indexing a value --> [] works same as [[]]
fruits[2]
fruits2[2]
fruits[[2]]
fruits2[[2]]

#Name a data.frame
fruits_df <- data.frame(apples = c(1, 2, 1),
                      pears = c("a", "a", "b"),
                      bananas = c(TRUE, TRUE, TRUE))

#Indexing a data.frame
fruits_df [2:3,3]
fruits_df$pears[2:3]
fruits_df[[2]][2:3]




#------------Data Set: Energy production across Europe per Power Unit-----------

# Read the CSV file
df_data_energy_day2 <- read.csv("C:/Users/eling/Documents/EAGLE/Introduction_to_Programming/Intro_to_Programming_Tutorium/MB12/data/day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")

#Explore/filter the dataset. Check out columns and what they tell. Try to
#understand the dataset.
names(df_data_energy_day2)
head(df_data_energy_day2)
#find all countries that are covered
names(table(df_data_energy_day2$MapCode))
#number of countries or energy grids
length(unique(df_data_energy_day2$MapCode)) #!!!!clean countries!!!!
#change germany
#df_data_energy_day2$MapCode <- gsub("a little bit of ", "", df$Language)

#number of records
dim(df_data_energy_day2)
nrow(df_data_energy_day2)

#what type/structure/class is this?
df_data_energy_day2$DateTime
typeof(df_data_energy_day2$DateTime) #character means it's not interpreted
#so it cannot be analysed
class(df$DateTime) # make this sotable (= not character)
df_data_energy_day2$DateTimePOS <- as.POSIXct(df_data_energy_day2$DateTime)

#make first scatter plot
#plot installed Generation Capacity vs. actual Generation Output
plot(
  x = df_data_energy_day2$InstalledGenCapacity,
  y = df_data_energy_day2$ActualGenerationOutput
)

#we have outliers in genoutput: removing all cases where genoutput is higher
#than installed capacity
nrow_df_not_cleaned <- nrow(df_data_energy_day2)
df_no_outliers <- df_data_energy_day2[df_data_energy_day2$ActualGenerationOutput < df_data_energy_day2$InstalledGenCapacity * 4,]
nrow(df_no_outliers)

#run the plot again with df_no_outliers
plot(
  x = df_no_outliers$InstalledGenCapacity,
  y = df_no_outliers$ActualGenerationOutput
)



