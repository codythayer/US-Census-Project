# Objective: Salary

# Creating base Salary df
salary = read.csv("ancestry_data - salary.csv")
head(salary)
colnames(salary)
# Remove first three, they are "suppressed data"
salary = salary[-3:-1,]
# salary[1:4, 1:4]


# 3. Melt all columns to row. (Cartesian > Indexed Dataframe structure)
reshape_df = melt(salary, id = c("X"))
# EDIT (needed to change this here, changed when split in other function)
colnames(reshape_df)[2] = c("entry_decade")
head(reshape_df)
reshape_df$entry_decade[1]
# Check data

# 5. Move ancestry variable into own column
reshape_2_df = cbind(reshape_df, ancestry = "")
head(reshape_2_df)
for (row in 1:nrow(reshape_df)) {
  a = 1 + (3 * (row - 1))
  b = 2 + (3 * (row - 1))
  c = 3 + (3 * (row - 1))
  reshape_2_df$ancestry[b:c] = reshape_2_df[a,1]
}
head(reshape_2_df)

# Remove empty rows with the ancestry variable in it.
del_seq = seq(from = 1, to = nrow(reshape_2_df)-2, by = 3)
reshape_2_df = reshape_2_df[-del_seq,]
# Remove ->
head(reshape_2_df)
reshape_2_df$ancestry = gsub('-> ','', reshape_2_df$ancestry)
head(reshape_2_df)

# EDIT
# Shuffle around column positions
reshape_3_df = reshape_2_df %>% relocate(ancestry, .before = colnames(reshape_2_df)[1])
head(reshape_3_df)


# 7. Adjust decade column variable
unique(reshape_3_df$entry_decade)
levels(reshape_3_df$entry_decade)
# EDIT using levels() functions (difference from when we split columns)
levels(reshape_3_df$entry_decade) = c("born_us", "before_1950","1950_1959", "1960_1969",
                                      "1970_1979", "1980_1989", "1990_1999", "2000_2009", 
                                      "post_2009")
levels(reshape_3_df$entry_decade)
salary_df = reshape_3_df

# ENDING DATA FRAME
tibble(salary_df)


################################################################################

# Merge salary data with country data

decade_country_aggr <- function (df_1, aggregate_funtion, aggregate_columns) {
  # Merge
  df = merge(regions_df, df_1, by = 'ancestry')
  
  # Aggregate by region_scope, using function argument
  aggr_df = aggregate(df[, aggregate_columns], by=list(entry_decade = df[, "entry_decade"], 
                                                       country = df[, "country"],
                                                       sub_region = df[, "sub_region"],
                                                       region = df[, "region"]), 
                      FUN=aggregate_funtion)
  
  print(head(aggr_df))
  
  # Check for NULL
  null_rows = which(aggr_df[, 'country'] == '')
  print(paste("Removed NULL rows in dataset:", toString(null_rows)))
  aggr_df = aggr_df[-(null_rows),]
  
  return (aggr_df)
}

salary_df = drop_na(salary_df)
# # Check Removed NA rows: 
# head(salary_df)
# which(is.na(salary_df$value))
# salary[which(is.na(salary_df$value)),]
# salary_df[which(is.na(salary_df$value)),]
# which(is.na(salary_df$value))
# nrow(salary_df)
# salary_df[803,]

salary_2_df = decade_country_aggr(df_1 = salary_df, aggregate_funtion = "mean", aggregate_columns = c('value'))
colnames(salary_2_df)[which(colnames(salary_2_df) == 'x')] = c("ave_earnings")

tibble(salary_2_df)


################################################################################

# Creating Plotting dfs

# Mexico
mex_salary_df = salary[which(salary$country == "Mexico"),]

# Salary by All Countries for box plots
salary_by_country = salary_2_df %>% 
  filter(entry_decade != "born_us") %>% # Remove us_born
  group_by(country, sub_region) %>%
  summarise(ave_earnings = mean(ave_earnings)); tibble(salary_by_country)

salary_box = arrange(salary_by_country, desc(salary_by_country$ave_earnings))
salary_box = salary_box[0:-1,] # remove outliers
tibble(salary_box)


