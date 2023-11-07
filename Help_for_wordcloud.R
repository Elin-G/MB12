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

#Capitalising each word in df$Language
library(tools)
df$Language <- toTitleCase(df$Language)
df$Language <- gsub("Javascript", "JavaScript", df$Language)
df$Language <- gsub("Html", "HTML", df$Language)
df$Language <- gsub("Sql", "SQL", df$Language)
df$Language <- gsub("Rstudio", "RStudio", df$Language)
df$Language <- gsub("Matlab", "MATLAB", df$Language)
df$Language <- gsub("Gee", "GEE", df$Language)
df$Language <- gsub("Qgis", "QGIS", df$Language)



# Create a text corpus from the character vector
corpus <- Corpus(VectorSource(df$Language))

# Create a custom transformation function to retain single-letter words
retain_single_letter_words <- content_transformer(function(doc) {
  words <- unlist(strsplit(doc, " "))  # Split text into words
  words <- words[nchar(words) >= 1]  # Retain words with at least one character
  paste(words, collapse = " ")  # Reconstruct the document
})

# Apply the custom transformation function to the corpus
corpus <- tm_map(corpus, retain_single_letter_words)

# Extract the resulting character vector from the corpus
df$Language <- sapply(corpus, as.character)


# Print the resulting vector
print(language_vector)

print(df)

View(df)

#Task 2: create a wordcloud
#install the package wordcloud load it, and if you can try to make a wordcloud
#from the Language vector from Task 1.

# Install wordcloud package
install.packages("wordcloud")

# Install the required packages
install.packages("tm")
install.packages("slam")

# Load the necessary packages
library(wordcloud)
library(tm)

#look at RColorBrewer for colour combinations
display.brewer.all(colorblindFriendly = TRUE)

# Create a word cloud
wordcloud(words = language_vector, min.freq = 1, scale = c(3, 0.7), colors = brewer.pal(8, "Paired"))
