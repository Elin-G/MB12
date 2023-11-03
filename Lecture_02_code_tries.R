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