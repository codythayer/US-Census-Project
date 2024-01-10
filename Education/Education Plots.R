# Objective: Education

# Plots

# 1:25 stacked bar plot, percent
edu_25_plot = plot_ly(edu_25_per, y=~country, x=~percent, color=~education_level, colors = colors_4, type = "bar") %>%
  layout(barmode="stack") %>%
  layout(yaxis = list(title="", categoryorder = "array", categoryarray = edu_25_per$country, 
                      linecolor = "white")) %>%
  layout(yaxis = list(minor = list(showgrid = FALSE))) %>%
  layout(xaxis = list(title=""), legend = list(title=list(text='Education Level')))
#   layout(xaxis = list(ticksuffix="%"))
edu_25_plot

# sub_region stacked bar plot, percent
edu_region_plot = plot_ly(edu_sr_per, y=~sub_region, x=~percent, color=~education_level, colors = colors_4, type = "bar") %>%
  layout(barmode="stack") %>%
  layout(yaxis = list(title="", categoryorder = "array", categoryarray = edu_sr_per$sub_region, 
                      linecolor = "white")) %>%
  layout(yaxis = list(minor = list(showgrid = FALSE))) %>%
  layout(xaxis = list(title=""), legend = list(title=list(text='Education Level')))
edu_region_plot