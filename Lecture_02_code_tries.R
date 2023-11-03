#perfect graph, but in only light blue

# Sort the dataframe by Experience_Percent in descending order
df <- df[order(-df$Experience_Percent), ]

# Your ggplot code with explicitly set y-axis breaks, labels, and adjusted line color
gg <- ggplot(df, aes(x = reorder(Name, -Experience_Percent), y = Experience_Percent)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Name", y = "Experience Percent", title = "Programming Experience of EAGLES 8th Gen.") +
  scale_y_continuous(
    breaks = seq(0, 100, 5),  # Set y-axis breaks at 5% intervals
    labels = paste0(seq(0, 100, 5), "%")  # Labels for the breaks
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits from 0 to 100
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray", linetype = "dotted", linewidth = 0.71))


# Print the plot
print(gg)



#Perfect code


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
