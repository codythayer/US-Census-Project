# Salary Box Plot


# Reorder Boxplots
region_order = c("Europe", "North America", "Africa", "Middle East", "Asia", 
                 "Oceanic", "Latin America")
region_order = rev(region_order)
salary_box$sub_region = factor(salary_box$sub_region, region_order)



sub_region_box <- ggplot(salary_box, aes(x=sub_region, y=ave_earnings, fill=sub_region, label=country)) + 
  ylab("Average Income") +
  xlab("") +
  geom_boxplot() + coord_flip() + 
  geom_boxplot(color="black") + 
  scale_fill_viridis_d() +
  geom_point(shape=16, position=position_jitter(0.2), colour="black", alpha=.25) +
    theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) +
  theme(panel.border = element_rect(fill=NA, colour = "gray90", size = 0.5, linetype = "solid")) + 
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(legend.position="none") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()))

sub_region_box

# Plot using plotly instead of ggplot2
ggplotly(sub_region_box)
