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
View(df)

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
  labs(x = "Name", y = "Experience", title = "Programming Experience of EAGLES 8th Gen.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", linewidth = 0.71),
        panel.grid.minor.y = element_blank()) +
  guides(fill = "none")  # Remove the color scale/legend

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


print(df)

View(df)

#Task 2: create a wordcloud
#install the package wordcloud load it, and if you can try to make a wordcloud
#from the Language vector from Task 1.

# Install the required packages
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("wordcloud2")
#install.packages("tm")

# Load the necessary packages
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

# Split the 'Language' column by the comma and create a vector
#Create a vector containing only the text
language_vector <- unlist(strsplit(df$Language, ", "))
View(language_vector)

# Remove any leading/trailing spaces and empty strings
language_vector <- language_vector[language_vector != ""]

# If you want to include single-letter words, you can adjust the min_word_length
min_word_length <- 0

#Create a corpus
corpus_Language <- Corpus(VectorSource(language_vector))
View(corpus_Language)

#Create a document-term-matrix: dataframe containing each word in first column
# and frequency in second column
#Create a document term matric with TermDocumentMatric function of tm package
dtm_Language <- TermDocumentMatrix(corpus_Language, control = list(wordLengths = c(min_word_length, Inf)))
matrix_Language <- as.matrix(dtm_Language)
words_Language <- sort(rowSums(matrix_Language), decreasing = TRUE)
df_Language <- data.frame(word = names(words_Language), freq = words_Language)
View(dtm_Language)
View(df_Language)
View(language_vector)

#look at RColorBrewer for colour combinations that are colourblind friendly
display.brewer.all(colorblindFriendly = TRUE)

# Create a word cloud
wordcloud(
  words = df_Language$word,
  freq = df_Language$freq,
  min.freq = 1,
  scale = c(3, 0.7), 
  colors = brewer.pal(8, "Paired"),
  random.order = FALSE #added to prevent warnings
)

View(df_Language)




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
#install.packages("RColorBrewer")
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

# Calculate percentages
os_count_df$Percentage <- os_count_df$Frequency / sum(os_count_df$Frequency) * 100

# Step 3: Create a pie chart with percentages
labels <- paste(os_count_df$OS, " (", round(os_count_df$Percentage, 1), "%)")

pie(os_count_df$Frequency, labels = labels, col = color_palette,
    main = "Operating Systems of EAGLES
    8th Gen.")
