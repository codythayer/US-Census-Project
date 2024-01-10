# Objective Distribution and Sampling Plots


## Average Earning Distribution

ave_earning_dist_2 = ggplot(salary_by_country, aes(x=ave_earnings)) + geom_histogram(aes(y = ..density..), bins = 40, color='darkgrey', fill="darkgrey") + 
  xlab("Average Earnings") +
  ylab("Density") + 
  geom_density(fill="red",  alpha=0.2) +
  geom_vline(xintercept = mean(salary_by_country$ave_earnings), linetype="dashed", color='darkblue', size = .8) + 
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  theme(panel.border = element_rect(fill=NA, colour = "gray90", 
                                    size = 0.5, linetype = "solid")) +
  theme(axis.ticks = element_line(color="gray90", size=0.5)) + 
  theme(axis.ticks.length = unit(0.18, "cm")) + 
  theme(axis.line = element_line(colour = "gray90", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", size=6, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=6, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", size=8, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black",  angle=0)) +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale()))


plot(ave_earning_dist_2)


################################################################################

## Central Limit Theorem

clt_10 = ggplot(xbar_df, aes(x=xbar_df[, 2])) + geom_histogram(bins = 30, color=colors_6[1], fill=colors_6[1]) + 
  ggtitle('Sample Size = 10') +
  xlab("Average Earnings") +
  ylab("Count") + 
  ylim(c(0, 200)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(18000, 35000), labels = label_dollar(scale_cut = cut_short_scale()))


clt_20 = ggplot(xbar_df, aes(x=xbar_df[, 3])) + geom_histogram(bins = 30, color=colors_6[2], fill=colors_6[2]) + 
  ggtitle('Sample Size = 20') +
  xlab("Average Earnings") +
  ylab("Count") + 
  ylim(c(0, 200)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(18000, 35000), labels = label_dollar(scale_cut = cut_short_scale()))


clt_30 = ggplot(xbar_df, aes(x=xbar_df[, 4])) + geom_histogram(bins = 30, color=colors_6[3], fill=colors_6[3]) + 
  ggtitle('Sample Size = 30') +
  xlab("Average Earnings") +
  ylab("Count") + 
  ylim(c(0, 200)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(18000, 35000), labels = label_dollar(scale_cut = cut_short_scale()))

clt_40 = ggplot(xbar_df, aes(x=xbar_df[, 5])) + geom_histogram(bins = 30, color=colors_6[4], fill=colors_6[4]) + 
  ggtitle('Sample Size = 40') +
  xlab("Average Earnings") +
  ylab("Count") + 
  ylim(c(0, 200)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(18000, 35000), labels = label_dollar(scale_cut = cut_short_scale()))


grid.arrange(clt_10, clt_20, clt_30, clt_40, nrow = 2, ncol = 2)  


################################################################################

# Sampling

head(salary_by_country)
nrow(salary_by_country)
head(sample_srswor)
head(sample_sys)
head(sample_strata)


pop_hist = ggplot(salary_by_country, aes(ave_earnings)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color=colors_5[1], fill=colors_5[1]) + 
  ggtitle('Population') +
  xlab("Average Earnings") +
  ylab("Density") + 
  ylim(c(0, 0.0001)) +
  geom_vline(xintercept = mean(sample_sys$ave_earnings), linetype="dashed", color='darkblue', size = 0.8) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(10000, 55000), labels = label_dollar(scale_cut = cut_short_scale()))

plot(pop_hist)

srswor_hist = ggplot(sample_srswor, aes(ave_earnings)) + geom_histogram(aes(y = ..density..), bins = 20, color=colors_5[2], fill=colors_5[2]) + 
  ggtitle('Simple Random Sampling') +
  xlab("Average Earnings") +
  ylab("Density") + 
  ylim(c(0, 0.0001)) +
  geom_vline(xintercept = mean(sample_sys$ave_earnings), linetype="dashed", color='darkblue', size = 0.8) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(10000, 55000), labels = label_dollar(scale_cut = cut_short_scale()))

plot(srswor_hist)

sys_hist = ggplot(sample_sys, aes(ave_earnings)) + geom_histogram(aes(y = ..density..), bins = 20, color=colors_5[3], fill=colors_5[3]) + 
  ggtitle('Unequal Systematic Sampling') +
  xlab("Average Earnings") +
  ylab("Density") + 
  ylim(c(0, 0.0001)) +
  geom_vline(xintercept = mean(sample_sys$ave_earnings), linetype="dashed", color='darkblue', size = 0.8) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(10000, 55000), labels = label_dollar(scale_cut = cut_short_scale()))

plot(sys_hist)

strata_hist = ggplot(sample_strata, aes(ave_earnings)) + geom_histogram(aes(y = ..density..), bins = 20, color=colors_5[4], fill=colors_5[4]) + 
  ggtitle('Stratified Sampling') +
  xlab("Average Earnings") +
  ylab("Density") + 
  ylim(c(0, 0.0001)) +
  geom_vline(xintercept = mean(sample_sys$ave_earnings), linetype="dashed", color='darkblue', size = 0.8) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.ticks = element_line(color="black", size=0.5)) + 
  theme(axis.ticks.length = unit(0.12, "cm")) + 
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) + 
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=8, angle=0)) + 
  theme(axis.title = element_text(face="plain", color="black", 
                                  size=9, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=11, angle=0)) +
  scale_x_continuous(limits = c(10000, 55000), labels = label_dollar(scale_cut = cut_short_scale()))

plot(strata_hist)

grid.arrange(pop_hist, srswor_hist, sys_hist, strata_hist, nrow = 2, ncol = 2) 



