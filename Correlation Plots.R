# Correlation Plots



# Create master df.

# citizenship percents (all)
citizen_all_df = country_citizen_per %>%
  group_by(country, citizenship) %>%
  summarise(pop = sum(pop))
tibble(citizen_all_df)

citizen_all_totals = citizen_all_df %>%
  group_by(country) %>%
  summarise(total_pop = sum(pop))
tibble(citizen_all_totals)

citizen_all = merge(citizen_all_df, citizen_all_totals, by = "country")
citizen_all = tibble(citizen_all) %>% 
  filter(citizenship == "us_citizen"); tibble(citizen_all)
citizen_all = cbind(citizen_all, citizen_percent = citizen_all$pop/citizen_all$total_pop *100)
citizen_all = select(citizen_all, country, citizen_percent)
tibble(citizen_all)

# family (all)
family_all = merge(couple_df, ave_kids_df, by = "country")
family_all = select(family_all, country, percent_partnered, ave_kids_per_person)
tibble(family_all)

# total immigration (all)
imm_all = select(imm_sum_df, country, sub_region, total_immigration) 
tibble(imm_all)

# education (all)
edu_all = filter(edu_all_per, education_level == "4_college_degree") %>%
  select(country, percent)
colnames(edu_all)[2] = "college_degree_percent" 
tibble(edu_all)

# income (all)
salary_all = select(salary_by_country, country, ave_earnings)
tibble(salary_all)



# All Dataframe
all_df = merge(imm_all, salary_all, by = "country")
all_df = merge(all_df, edu_all, by = "country")
all_df = merge(all_df, citizen_all, by = "country")
all_df = merge(all_df, family_all, by = "country")
colnames(all_df) = c("country", "sub_region", "total_imm_pop", "ave_earnings", 
                     "college_deg_per", "us_citizen_per", "partner_percent", "ave_num_children")
colnames(all_df)
tibble(all_df)



# Correlation Matrix
install.packages("corrplot")
library(corrplot)

cor_matrix = all_df[, 4:8]
colnames(cor_matrix) = c("Ave. Earnings", "%Pop College Degree", 
                         "%Pop US Citizen", "%Pop Partnered", "Ave. Number of Children")
cor_matrix = cor(cor_matrix)
cor_matrix_plot = corrplot(cor_matrix , type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, col=viridis(25)[1:20])







# Children vs. Income
slope <- 2e-05
all_df$size <- sqrt(all_df$total_imm_pop * slope)

children_income = plot_ly(all_df, x = ~ave_earnings, y = ~ave_num_children, color = ~sub_region, 
        size = ~size, colors = viridis(7),
        type = 'scatter', mode = 'markers', 
        sizes = c(10, 100),
        marker = list(symbol = 'circle', sizemode = 'diameter',
                      line = list(width = 2, color = 'black')),
        text = ~paste('Country:', country, '<br>Ave. Children:', round(ave_num_children, 2), '<br>Ave. Earnings:', round(ave_earnings, 2),
                      '<br>Immigrant Pop.:', total_imm_pop)) %>%
layout(xaxis = list(title = 'Average Earnings',
                    range = c(0, 60000),
                    zerolinewidth = 2,
                    tickwidth = 1.5,
                    ticklen = 5,
                    tickprefix="$",
                    gridwidth = 2),
       yaxis = list(title = 'Average Number of Children',
                    range = c(0, 3),
                    zerolinewidth = 2,
                    tickwidth = 1.5,
                    ticklen = 5,
                    gridwith = 2))

children_income


# Edu vs. Income
edu_income = plot_ly(all_df, x = ~ave_earnings, y = ~college_deg_per, color = ~sub_region, 
                     size = ~size, colors = viridis(7),
                     type = 'scatter', mode = 'markers', 
                     sizes = c(10, 100),
                     marker = list(symbol = 'circle', sizemode = 'diameter',
                                   line = list(width = 2, color = 'black')),
                     text = ~paste('Country:', country, '<br>Percent Pop. with College Degree:', round(college_deg_per, 2), 
                                   '<br>Ave. Earnings:', round(ave_earnings, 2),
                                   '<br>Immigrant Pop.:', total_imm_pop)) %>%
  layout(xaxis = list(title = 'Average Earnings',
                      range = c(0, 60000),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      tickprefix="$",
                      gridwidth = 2),
         yaxis = list(title = 'Population with College Degree',
                      range = c(0, 50),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwith = 2))

edu_income


# citizen vs. income
citizen_income = plot_ly(all_df, x = ~ave_earnings, y = ~us_citizen_per, color = ~sub_region, 
                        size = ~size, colors = viridis(7),
                        type = 'scatter', mode = 'markers', 
                        sizes = c(10, 100),
                        marker = list(symbol = 'circle', sizemode = 'diameter',
                                      line = list(width = 2, color = 'black')),
                        text = ~paste('Country:', country, '<br>Pop. with US Citizen.:', round(us_citizen_per, 2), 
                                      '<br>Ave. Earnings:', round(ave_earnings, 2),
                                      '<br>Immigrant Pop.:', total_imm_pop)) %>%
  layout(xaxis = list(title = 'Average Earnings',
                      range = c(0, 60000),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      tickprefix="$",
                      gridwidth = 2),
         yaxis = list(title = 'Population with U.S. Citizenship',
                      range = c(0, 95),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwith = 2))

citizen_income

# marriage vs. income
marriage_income = plot_ly(all_df, x = ~ave_earnings, y = ~partner_percent, color = ~sub_region, 
                         size = ~size, colors = viridis(7),
                         type = 'scatter', mode = 'markers', 
                         sizes = c(10, 100),
                         marker = list(symbol = 'circle', sizemode = 'diameter',
                                       line = list(width = 2, color = 'black')),
                         text = ~paste('Country:', country, '<br>Pop. Partnered:', round(partner_percent, 2), 
                                       '<br>Ave. Earnings:', round(ave_earnings, 2),
                                       '<br>Immigrant Pop.:', total_imm_pop)) %>%
  layout(xaxis = list(title = 'Average Earnings',
                      range = c(0, 60000),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 0,
                      tickprefix="$",
                      gridwidth = 2),
         yaxis = list(title = 'Population Partnered',
                      range = c(20, 45),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwith = 2))

marriage_income



# marriage vs. kids
marriage_kids = plot_ly(all_df, y = ~partner_percent, x = ~ave_num_children, color = ~sub_region, 
                          size = ~size, colors = viridis(7),
                          type = 'scatter', mode = 'markers', 
                          sizes = c(10, 100),
                          marker = list(symbol = 'circle', sizemode = 'diameter',
                                        line = list(width = 2, color = 'black')),
                          text = ~paste('Country:', country, '<br>Pop. Partnered:', round(partner_percent, 2), 
                                        '<br>Ave. Children:', round(ave_num_children, 2),
                                        '<br>Immigrant Pop.:', total_imm_pop)) %>% 
  layout(xaxis = list(title = 'Average Number of Children',
                      range = c(0, 2.5),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 0,
                      gridwith = 2),
         yaxis = list(title = 'Population Partnered',
                      range = c(20, 45),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwith = 2))

marriage_kids



# citizen vs. education
citizen_edu = plot_ly(all_df, y = ~college_deg_per, x = ~us_citizen_per, color = ~sub_region, 
                         size = ~size, colors = viridis(7),
                         type = 'scatter', mode = 'markers', 
                         sizes = c(10, 100),
                         marker = list(symbol = 'circle', sizemode = 'diameter',
                                       line = list(width = 2, color = 'black')),
                         text = ~paste('Country:', country, '<br>Pop. with US Citizen.:', round(us_citizen_per, 2), 
                                       '<br>Percent Pop. with College Degree:', round(college_deg_per, 2),
                                       '<br>Immigrant Pop.:', total_imm_pop)) %>%
  layout(yaxis = list(title = 'Population with College Degree',
                      range = c(0, 50),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwidth = 2),
         xaxis = list(title = 'Population with U.S. Citizenship',
                      range = c(0, 90),
                      zerolinewidth = 2,
                      tickwidth = 1.5,
                      ticklen = 5,
                      ticksuffix="%",
                      gridwith = 2))

citizen_edu


