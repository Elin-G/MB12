# reading line by line
poll <- readLines("poll.md")
typeof(poll) #what data type (in R called mode) is this?
dim(poll) #what dimension does this object have
length(poll) #what is the length of the object

#index by element
poll[1] #how to see the first element in poll
poll #uses the default print to show everything in poll

#exclude/remove every empty line -> every fourth line in file
#filter/exclude empty elements
#== means is
#!= means is not
poll <- poll[poll != ""] #we want to match everything that is not an empty vector/element
#it gives true or false for every element, saying is it empty or not. then it
#prints only the ones that are not empty

#convert this vector into a dataframe, best one is matrix for a vector
#convert into matrix
df <- as.data.frame(matrix(poll, byrow = T, ncol = 4))
df
dim(df)
colnames(df) <- c("Name", "Experience", "Language", "OS")
colnames(df)

#View(df)

#clean the name column (to look at a specific column use dollar sign)
#gsub replaces the first symbol with the next. so nothing instead of #
gsub("#", "", df$Name)
df$Name <- gsub("#", "", df$Name) #overwrite the Name column
df$Name <- gsub("ushakova", "Ushakova", df$Name)
#trim whitespace function of R trims the beginning and end of the string
trimws(df$Name)
df$Name <- trimws(df$Name) #overwrite the Name column
#View(df)

#clean Experience
#"[*]" makes everything inside the brackets literal, so it can be deleted
df$Experience <- gsub("[*]", "", df$Experience)
df$Experience <- trimws(df$Experience)
df$Experience[df$Experience == "0-5"] <- "2.5"
df$Experience <- as.numeric(df$Experience)

# Calculate the percentage and create a new column "Experience_Percent"
df$Experience_Percent <- (df$Experience / 10) * 100

#create a bar graph of the experience
library(ggplot2)
library(scales)

# Sort the DataFrame by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Create a custom color palette with darker shades of green
darker_greens <- c("#C7E8C9", "#00CC00", "#008800", "#003300", "#003300")

# Define custom breakpoints based on the desired order of colors
breaks <- c(0, 10, 20, 25, 80, 100)

# Create a factor for colors based on the custom palette and custom breakpoints
df$color_factor <- cut(df$Experience_Percent, breaks = breaks, labels = FALSE)

# Your ggplot code with the custom color palette and other settings
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent, fill = factor(color_factor))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = darker_greens) +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", size = 0.71),
        panel.grid.minor.y = element_blank()) +
  guides(fill = FALSE)  # Remove the color scale/legend

# Print the plot
print(gg)



#clean Language
df$Language <- gsub("[*]", "", df$Language)
#this is dangerous, since hard-coded: if the file changes, code breaks
#least reliable way to do it, as when the MD file changes, the line changes and it won't work anymore)
#df$Language[4] <- "Python, JavaScript"
#df$Language[11] <- "R-Studio"

df$Language <- trimws(df$Language)
df$Language[df$Language == "no one"] <- "None"

df$Language <- gsub("a little bit of ", "", df$Language)
df$Language <- gsub("\\(but just done basic analysis\\)", "", df$Language)
df$Language <- gsub("\\(tiny bit\\)", "", df$Language)


#make everything longer case
df$Language <- tolower(df$Language)

#Task 1: how can we make a vector from df$Language that contains each language
#mentioned as a single element.
# e.g.: python, r, javascript, rstudio, r, python, java

#make all names the same
df$Language <- gsub("javascript", "java", df$Language)
df$Language <- gsub("java script", "java", df$Language)

df$Language <- gsub("java", "javascript", df$Language)
df$Language <- gsub("html and sql", "html, sql", df$Language)
df$Language <- gsub("r-studio", "rstudio", df$Language)        # NOTE: do it separately and deactivate, or it will fuck up the language_vector
df$Language <- gsub("gee \\(javascript\\)", "gee, javascript", df$Language)
df$Language <- trimws(df$Language)

#Capitalising each word in df$Language
#library(tools)
df$Language <- toTitleCase(df$Language)
df$Language <- gsub("Javascript", "JavaScript", df$Language)
df$Language <- gsub("Html", "HTML", df$Language)
df$Language <- gsub("Sql", "SQL", df$Language)
df$Language <- gsub("Rstudio", "RStudio", df$Language)
df$Language <- gsub("Matlab", "MATLAB", df$Language)
df$Language <- gsub("Gee", "GEE", df$Language)
df$Language <- gsub("Qgis", "QGIS", df$Language)





# Split the 'Language' column by the comma and create a vector
language_vector <- unlist(strsplit(df$Language, ", "))

# Print the resulting vector
print(language_vector)

print(df)

View(df)

#Task 2: create a wordcloud
#install the package wordcloud load it, and if you can try to make a wordcloud
#from the Language vector from Task 1.

# Install wordcloud package
#install.packages("wordcloud")
#library(wordcloud)

# Install the required packages
#install.packages("tm")
#install.packages("slam")

# Load the necessary packages
#library(wordcloud)
#library(tm)  # Load the 'tm' package

#look at RColorBrewer for colour combinations
display.brewer.all(colorblindFriendly = TRUE)

# Create a word cloud
wordcloud(words = language_vector, min.freq = 1, scale = c(3, 0.7), colors = brewer.pal(8, "Paired"))




#Task 3: clean up OS
#make everything lower case
df$OS <- gsub("[*]", "", df$OS)
df$OS <- trimws(df$OS)

print(df$OS)

df$OS <- tolower(df$OS)
df$OS <- gsub("windows 11", "windows", df$OS)
df$OS <- gsub("windows 10", "windows", df$OS)
df$OS <- gsub("window10", "windows", df$OS)
df$OS <- gsub("windows10", "windows", df$OS)
df$OS <- gsub("window11", "windows", df$OS)
df$OS <- gsub("64-bit", "", df$OS)
df$OS <- gsub(",", "", df$OS)
df$OS <- gsub("\\((.*?)\\)", "\\1", df$OS)
df$OS <- gsub("mac ios", "ios", df$OS)
df$OS <- gsub("macos", "ios", df$OS)
df$OS <- gsub(" bootcamp", "", df$OS)
df$OS <- gsub(" and", ",", df$OS)
df$OS <- gsub("windows ", "windows", df$OS)
#Capitalising each word in df$OS
#library(tools)
df$OS <- toTitleCase(df$OS)
df$OS <- gsub("Ios", "IOS", df$OS)

print(df$OS)
 
# Split the 'OS' column by the comma and create a vector
os_vector <- unlist(strsplit(df$OS, ", "))

# Print the resulting vector
print(os_vector)

#create a pie chart
#install color brewer package
install.packages("RColorBrewer")
# Load the RColorBrewer library
library(RColorBrewer)

# Specify the palette name and number of colors
n_colors <- 4
color_palette <- rev(brewer.pal(n_colors, "YlOrRd"))  # Reverse the YlOrRd palette

# Step 1: Count the frequencies of words
os_count <- table(os_vector)

# Step 2: Create a data frame from word frequencies
os_count_df <- as.data.frame(os_count)
colnames(os_count_df) <- c("OS", "Frequency")

# Step 3: Create a pie chart
pie(os_count_df$Frequency, labels = os_count_df$OS, col = color_palette,
    main = "Operating Systems of EAGLES
    8th Gen.")




#TEST

# Calculate the percentage and create a new column "Experience_Percent"
df$Experience_Percent <- (df$Experience / 10) * 100

# Sort the DataFrame in descending order by "Experience_Percent"
df <- df[order(-df$Experience_Percent), ]

# Get unique values in the "Experience_Percent" column
unique_experience_values <- unique(df$Experience_Percent)

# Create a custom color palette with distinct colors for each unique value
green_bar_palette <- rev(brewer.pal(length(unique_experience_values), "Greens"))

# Assign colors to bars based on the unique values
bar_colors <- green_bar_palette[match(df$Experience_Percent, unique_experience_values)]

# Set the y-axis limits and breaks
y_limits <- c(0, 100)

# Set the margin and adjust the axis title position
par(mar = c(9, 5, 2, 2), mgp = c(6, 1, 0))  # Set the margin (bottom, left, top, right)

# Create the graph with Experience_Percent
barplot(df$Experience_Percent, names.arg = df$Name, xlab = "Name of Student", ylab = "",
        col = bar_colors, main = "",
        ylim = y_limits, las = 2, cex.names = 0.7,
        cex.axis = 0.7, axisnames = TRUE)  # Adjust font size (0.7 is an example, you can change it)

# Add main titles separately for x and y axes
title(main = "Programming Experience of EAGLES 8th Gen.", line = 0.5)
title(ylab = "Programming Experience [%]", line = 2.5)

# Overlay gridlines on top of the bars
for (i in seq(0, 100, 5)) {
  lines(x = c(0, nrow(df) + 1), y = c(i, i), col = "darkgray", lty = 2, lwd = 0.5)
}

# Restore the default margin and axis title position
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))



#test graph with ggplot2 --> creates a scale for colour

library(ggplot2)
library(scales)

# Assuming your DataFrame is named "df" with columns "Name" and "Experience_Percent"

# Sort the DataFrame by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Your ggplot code with explicitly set y-axis breaks, labels, and adjusted line color
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent, fill = Experience_Percent)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", guide = guide_colorbar(nbin = 10)) +
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", size = 0.71),
        panel.grid.minor.y = element_blank())

# Print the plot
print(gg)





#test3 --> is too light

library(ggplot2)
library(scales)

# Sort the DataFrame by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Define a color palette with the same colors for bars
color_palette <- scales::brewer_pal(palette = "Greens")(length(df$Experience_Percent))

# Create a factor to ensure a discrete scale
df$color_factor <- cut(df$Experience_Percent, breaks = length(color_palette), labels = FALSE)

# Your ggplot code with explicitly set y-axis breaks, labels, and adjusted line color
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent, fill = factor(color_factor))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", size = 0.71),
        panel.grid.minor.y = element_blank())

# Print the plot
print(gg)




library(ggplot2)

# Sort the DataFrame by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Create a custom color palette with darker shades of green
darker_greens <- c("#C7E8C9", "#00CC00", "#008800", "#006400", "#004400")

# Your ggplot code with the custom color palette and other settings
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = scales::percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", size = 0.71),
        panel.grid.minor.y = element_blank())

# Print the plot
print(gg)


library(ggplot2)
library(scales)

# Sort the DataFrame by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Create a custom color palette with darker shades of green
darker_greens <- c("#C7E8C9", "#00CC00", "#006400", "#003300", "#003300")

# Create a factor for colors based on the custom palette
df$color_factor <- cut(df$Experience_Percent, breaks = length(darker_greens), labels = FALSE)

# Your ggplot code with the custom color palette and other settings
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent, fill = factor(color_factor))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = darker_greens) +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = percent_format(scale = 1)  # Use percentage labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", size = 0.71),
        panel.grid.minor.y = element_blank()) +
  guides(fill = FALSE)  # Remove the color scale/legend

# Print the plot
print(gg)





