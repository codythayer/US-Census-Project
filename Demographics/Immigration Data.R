# Objective: Total Immigration


# Construct base immigration df
citizen = read.csv("ancestry_data - citizenship_2.csv")
aggr_cols = c("us_born", "pre_1950", "X1950_1959", "X1960_1969", "X1970_1979", 
              "X1980_1989", "X1990_1999", "X2000_2009", "post_2009")

imm_df = country_aggr(df_1 = citizen, aggregate_funtion = 'sum', aggregate_columns = aggr_cols)

imm_df = merge(regions_df, citizen, by = 'ancestry')
imm_df = aggregate(imm_df[, aggr_cols], by=list(country = imm_df[, "country"],
                                                     sub_region = imm_df[, "sub_region"],
                                                     region = imm_df[, "region"]), 
                    FUN=sum)

tibble(imm_df)


################################################################################

# Creating Different Subsets dfs

# Starting df
head(imm_df)
# Remove first row, NA column from original data pulled from Census Bureau
imm_df = imm_df[-1,]
# Remove us_born before summing rows.
imm_df = select(imm_df, -us_born)

sum_cols = c(which(colnames(imm_df)=="pre_1950"): which(colnames(imm_df)=="post_2009"))
imm_sum_df = cbind(imm_df, 
                   total_immigration = rowSums(imm_df[, sum_cols]))
head(imm_sum_df)
imm_sum_df = select(imm_sum_df, -pre_1950, -X1950_1959, -X1960_1969, -X1970_1979, 
                    -X1980_1989, -X1990_1999, -X2000_2009, -post_2009)
head(imm_sum_df)

imm_sum_df



# Remove NULLs
null_rows = which(imm_sum_df$country == '')
imm_sum_df = imm_sum_df[-(null_rows),]
null_rows = which(imm_df$country == '')
imm_df = imm_df[-(null_rows),]

# Resultant dfs to use throughout code for country grouping.
tibble(imm_df) # total immigration by decade for each country
tibble(imm_sum_df) # total immigration for each country


################################################################################

# Creating Plotting dfs

# Mexico
mex_df  = subset(imm_sum_df, country %in% c("Mexico"))
mex_df

# Top 25 Countries
order.index = order(imm_sum_df$total_immigration, decreasing=TRUE)
imm_df_ordered = imm_sum_df[order.index, ]
imm_25_df = imm_df_ordered[1:25, ]
imm_26_df = imm_df_ordered[2:26, ] # Excluding Mexico
imm_26_df$country


# Country Subset
pie_chart_subset = imm_25_df
pie_chart_subset$country[20:25] = "Other"
pie_chart_subset = group_by(pie_chart_subset, country) %>% 
  summarise(total_immigration = sum(total_immigration)) %>% arrange(desc(total_immigration))
pie_chart_subset

# Sub Region Subset
imm_subregion_df = aggregate(list(total_immigration = imm_sum_df[, "total_immigration"]), 
                             by=list(sub_region = imm_sum_df[, "sub_region"], region = imm_sum_df[, "region"]), 
                             FUN=sum)
imm_subregion_df = imm_subregion_df %>% arrange(desc(total_immigration))
imm_subregion_df
# order.index = order(imm_subregion_df$total_immigration, decreasing=TRUE)
# imm_subregion_df_ordered = imm_subregion_df[order.index, ]
# head(imm_subregion_df_ordered)


################################################################################

# Pie Charts

# Country

m <- list(
  l = 50,
  r = 50,
  b = 175,
  t = 50,
  pad = 50
)

pc_country <- plot_ly(pie_chart_subset, labels = ~country, values = ~total_immigration, rotation = 45, 
               textposition = 'outside', textinfo = 'label+percent', 
               marker = list(colors = viridis(25),
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = list(text='U.S. Immigration', 
                                   xref="paper", xanchor='middle', yref="paper", yanchor="middle", x=0.5, y=0.5),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(margin = m) # margin on outside to make sure you see text


# Sub Regions

m2 <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 50,
  pad = 50
)

pc_region <- plot_ly(imm_subregion_df, labels = ~sub_region, values = ~total_immigration, 
               textposition = 'outside', textinfo = 'label+percent', 
               marker = list(colors = viridis(10)[1:7],
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = list(text='U.S. Immigration', 
                                   xref="paper", xanchor='middle', yref="paper", yanchor="middle", x=0.5, y=0.5),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(margin = m2) # margin on outside to make sure you see text



# Resultant Plots
pc_country
pc_region
