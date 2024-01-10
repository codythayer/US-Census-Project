# Objective: Marriage + Children Plots

# For R Studio Output
ave_kids_plot = ggplot(family_df, aes(y=reorder(country, -ave_kids), x=ave_kids, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Number of Children per Person") +
  scale_x_continuous(expand = c(0,0), limits = c(-1.5, 0), position = "bottom", 
                     breaks = seq(-2,0,1),
                     #Make the labels positive
                     labels = seq(-2,0,1) %>% abs()) + 
  theme(plot.title = element_text(hjust='1', vjust='0.5', face="bold", color="black", 
                                  size=9, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=12, angle=0))


partnered_plot = ggplot(family_df, aes(x=reorder(country, -ave_kids), y=percent_partnered, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Percent of Population Partnered") +
  theme(plot.title = element_text(vjust='0.5', face="bold", color="black", 
                                  size=9, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.ticks.length.y = unit(0, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=12, angle=0)) +
  coord_flip() +
  scale_x_discrete(position = "top", labels=NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45))


plot_grid(ave_kids_plot, partnered_plot, axis="l")


# Notes on plot:
# adjust +/- on reorder to change order


# For R Markdown html output (Change text sizing)
ave_kids_plot_2 = ggplot(family_df, aes(y=reorder(country, -ave_kids), x=ave_kids, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Number of Children per Person") +
  scale_x_continuous(expand = c(0,0), limits = c(-1.5, 0), position = "bottom", 
                     breaks = seq(-2,0,1),
                     #Make the labels positive
                     labels = seq(-2,0,1) %>% abs()) + 
  theme(plot.title = element_text(hjust='.90', vjust='0.5', face="plain", color="black", 
                                  size=25, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=20, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=20, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=22, angle=0))


partnered_plot_2 = ggplot(family_df, aes(x=reorder(country, -ave_kids), y=percent_partnered, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Percent of Population Partnered") +
  theme(plot.title = element_text(hjust='.04', vjust='0.5', face="plain", color="black", 
                                  size=25, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.ticks.length.y = unit(0, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=20, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=20, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=22, angle=0)) +
  coord_flip() +
  scale_x_discrete(position = "top", labels=NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45))



plot_grid(ave_kids_plot_2, partnered_plot_2, axis="l")



#################################################################################

# Sub Regions (RMD version)

# For R Markdown html output (Change text sizing)
ave_kids_reg_plot = ggplot(family_reg_df, aes(y=reorder(sub_region, -ave_kids), x=ave_kids, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Number of Children per Person") +
  scale_x_continuous(expand = c(0,0), limits = c(-2, 0), position = "bottom", 
                     breaks = seq(-2,0,1),
                     #Make the labels positive
                     labels = seq(-2,0,1) %>% abs()) + 
  theme(plot.title = element_text(hjust='1', vjust='0.5', face="plain", color="black", 
                                  size=9, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=12, angle=0))


partnered_reg_plot = ggplot(family_reg_df, aes(x=reorder(sub_region, -ave_kids), y=percent_partnered, fill=ave_kids)) + 
  scale_fill_viridis() +
  geom_bar(stat="identity") +
  labs(x="",y="") +
  ggtitle("Percent of Population Partnered") +
  theme(plot.title = element_text(vjust='0.5', face="plain", color="black", 
                                  size=9, angle=0), legend.position="none") +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.ticks.length.y = unit(0, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=12, angle=0)) +
  coord_flip() +
  scale_x_discrete(position = "top", labels=NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45))



plot_grid(ave_kids_reg_plot, partnered_reg_plot, axis="l")

ggplotly(ave_kids_reg_plot)
ggplotly(partnered_reg_plot)
  