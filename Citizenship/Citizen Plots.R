# Citizenship Plots


decades = c("Pre 1950", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "Post 2009")

# Stacked Bar Chart Mexico (Percent of decade pop.)

mex_citizen = plot_ly(
  data = mex_citizenship_per,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  layout(title = list(text="Mexican Citizenship Ratios by Decade", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="Decade of Entry into the USA", 
                      categoryorder = "array", categoryarray = decades))
# layout(legend = list(title=list(text='Citizenship Status')))






# Stacked Bar Chart by Sub Region
# Asia
asia_plot = plot_ly(
  data = asia_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Asia", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)

africa_plot = plot_ly(
  data = africa_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)

europe_plot = plot_ly(
  data = europe_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)

na_plot = plot_ly(
  data = na_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)


la_plot = plot_ly(
  data = la_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)

me_plot = plot_ly(
  data = me_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)


ocean_plot = plot_ly(
  data = ocean_cit,
  x = ~entry_decade,
  y = ~percent,
  color = ~citizenship,
  colors = c(colors_3[1], colors_3[2]),
  type = "bar"
) %>% 
  layout(barmode = "stack", bargap = 0.075, width=0.2)  %>%
  # layout(title = list(text="Africa", x=0.054, y=0.97)) %>%
  layout(yaxis = list(title="", ticksuffix="%")) %>%
  layout(xaxis = list(title="U.S. Entry Decade", 
                      categoryorder = "array", categoryarray = decades)) %>% 
  layout(showlegend = FALSE)



annotations = list( 
  list( 
    x = 0.00,  
    y = 0.98,  
    text = "Latin America",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.343,  
    y = 0.98,  
    text = "Oceania",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.6744,  
    y = 0.98,  
    text = "Europe",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.00,  
    y = 0.635,  
    text = "Middle East",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.343,  
    y = 0.635,  
    text = "Asia",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.6744,  
    y = 0.635,  
    text = "Africa",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.0,  
    y = 0.305,  
    text = "North America",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "left",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))


citizenship_by_region <- subplot(la_plot,  ocean_plot, europe_plot, 
                                 me_plot, asia_plot,africa_plot, 
                                 na_plot, 
                                 nrows = 3,
                                 shareX = TRUE, shareY = TRUE, margin = c(0.01, 0.03, 0.01, 0.05)) %>% 
  layout(annotations = annotations) 

citizenship_by_region


















################################################################################

# Line Graph by Country

lineplot = ggplot(data = country_per_citizen_df, aes(x=entry_decade, y=percent, group=country)) +
  scale_color_viridis_d() +
  geom_line(aes(color=country)) +
  geom_point(aes(color=country)) +
  theme(panel.grid.major = element_line(color="gray90"), panel.grid.minor = element_line(color="gray94"),
        panel.background = element_blank(), axis.line = element_line(colour = "gray90", size =0.5)) + 
  xlab("") +
  ylab("Percent Non-Citizen") + 
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
                                  size=12, angle=0)) +
  theme(plot.title = element_text(face="plain", color="black", 
                                  size=14, angle=0)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(colour="white", size=12, 
                                    face="bold")) +
  scale_y_continuous(labels = scales::percent)


ggplotly(lineplot)
