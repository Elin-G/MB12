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
y_breaks <- seq(0, 100, 5)

# Set the margin and adjust the axis title position
par(mar = c(9, 5, 2, 2), mgp = c(6, 1, 0))  # Set the margin (bottom, left, top, right)

# Create an empty plot to set up the axis
plot(1, type = "n", xlim = c(0, length(df$Name) + 1), ylim = y_limits, xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# Add horizontal grid lines at every 5% point
abline(h = seq(0, 100, 5), col = "darkgray", lty = 2, lwd = 0.5)

# Create the graph with Experience_Percent
barplot(df$Experience_Percent, names.arg = df$Name, add = TRUE, col = bar_colors, border = NA)

# Add main titles separately for x and y axes
title(main = "Programming Experience of EAGLES 8th Gen.", line = 0.5)
title(ylab = "Programming Experience [%]", line = 2.5)

# Restore the default margin and axis title position
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
