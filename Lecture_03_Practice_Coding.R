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
#what class is the dataframe?: character
class(df_data_energy_day2$DateTime) # make this sortable (= not character)
#transform into POSIXct
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

# we still see some outliers and we already see how the data is distributed

#-------------------------------------------------------------------------------

# TASK: Create plots using plot and ggplot syntax. Find out more information
# about our dataset!
# for example: 
# - plot the number of power plants per production type, or
# - production capacity per type

plants <- df_no_outliers[,c("GenerationUnitEIC", "ProductionTypeName")]
counts <- table(plants$ProductionTypeName)

barplot(counts, horiz = T )
barplot(sort(counts), horiz = T, las = 1,
        col = rainbow(length(counts)))

length(counts)
plants <- unique(df$GenerationUnitEIC)

#create graphics device for saving
dev.off()
png("entsoe_prdtype_units.png", width = 700, height = 700)
par(mar = c(6, 15, 4, 4))
barplot(sort(counts), horiz = T, las = 1,
        col = rainbow(length(counts)))
title(main = "Number of records per production type\nacross the EU, 1st week of Aug. 2020",
      xlab = "Number of records")
dev.off()

#plot 2: production capacity per type
#aggregation or production capacity per type
df_agg_type <- aggregate(
  df_data_energy_day2$InstalledGenCapacity,
  by = list(df_data_energy_day2$ProductionTypeName),
  FUN = sum
)
#view the first 6 lines of the new df
head(df_agg_type)
#overrade the column names
colnames(df_agg_type) <- c(
  "ProductionTypeName",
  "InstalledGenCapacity_sum"
)

df_agg_type$InstalledGenCapacity_sum <- df_agg_type$InstalledGenCapacity_sum * 0.001




# ---------------TASK 1: plot this a barplot in ggplot2-------------------------
# Your ggplot code with the custom color palette and other settings
#-------------------------------------------------------------------------------
#order the dataframe by installed capacity
#install the necessary libraries
library(dplyr)

# Sort the DataFrame by InstalledGenCapacity_sum in descending order
df_agg_type <- df_agg_type %>%
  arrange(InstalledGenCapacity_sum)

# Convert ProductionTypeName to a factor with levels based on the order of InstalledGenCapacity_sum
df_agg_type$ProductionTypeName <- factor(df_agg_type$ProductionTypeName, levels = df_agg_type$ProductionTypeName)

View(df_agg_type)


#df_agg_type <- df_agg_type[order(-df_agg_type$InstalledGenCapacity_sum), ]
#df_agg_type$ProductionTypeName <- factor(df_agg_type$ProductionTypeName, levels = unique(df_agg_type$ProductionTypeName))

gg <- ggplot(df_agg_type, aes(x = ProductionTypeName, y = InstalledGenCapacity_sum, fill = ProductionTypeName)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_y_continuous(
    breaks = seq(0, 27000, 1000),  # Set x-axis breaks at intervals of 1000
    expand = c(0, 0.5)  # Set the expansion for minor grid lines
#    labels = percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 27000)) +  # Set y-axis limits from 0 to 26310
  labs(x = "Production Type", y = "Sum of Installed Generator Capacity", title = "Sum of Installed Generator Capacity of different Production Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(color = "darkgray", linetype = "dotted", linewidth = 0.71),
        panel.grid.minor.x = element_blank()) +
  guides(fill = "none") + # Remove the color scale/legend
  coord_flip() + #flip the x and y axes
  geom_hline(yintercept = seq(0, 27000, 500), linetype = "dotted", color = "darkgray", size = 0.5)

# Print the plot
print(gg)


#-----------------------CONTINUE ON SLIDE 18 - LECTURE 04 ----------------------