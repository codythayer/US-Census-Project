# Objective: Education


# Construct base education df
edu = read.csv("ancestry_data - education.csv")

# RESHAPE CSV
# 1. Move outer column name, concatenate with inner column names
for (col in 2:64) {
  edu[1,col] = paste(colnames(edu)[col], "/",  edu[1,col])
  print(edu[1, col])
}

# 2. Make Row 1 the Column Names (concatenated inner and outer columns)
colnames(edu) = edu[1,]
# Remove first three, they are "suppressed data"
edu = edu[-3:-1,]
# edu[1:4, 1:4]

# 3. Melt all columns to row. (Cartesian > Indexed Dataframe structure)
melt_data = melt(edu, id = c(""))
# head(melt_data)

# CHECK Data (should be 130,297)
# colnames(melt_data) = c("citizenship_status", "entry_decade", "pop")
# melt_data[266,]
# # Check Data (should be 63)
# nrow(melt_data[which(melt_data$citizenship_status == "-> Somali"),])

# 4. Split melted column
colnames(melt_data) = c("citizenship_status", "entry_decade", "pop")
melt_data = separate(melt_data, col=entry_decade, c('entry_decade', 'education_level'), sep = "/")

# 5. Move ancestry variable into own column
edu_df = cbind(melt_data, ancestry = "")
head(edu_df)
for (row in 1:nrow(melt_data)) {
  a = 1 + (3 * (row - 1))
  b = 2 + (3 * (row - 1))
  c = 3 + (3 * (row - 1))
  edu_df$ancestry[b:c] = edu_df[a,1]
}
head(edu_df)

# Remove empty rows with the ancestry variable in it.
del_seq = seq(from = 1, to = nrow(edu_df)-2, by = 3)
edu_df = edu_df[-del_seq,]
# Remove ->
head(edu_df)
edu_df$ancestry = gsub('-> ','', edu_df$ancestry)
head(edu_df)
# Shuffle around column positions
edu_df = edu_df %>% relocate(ancestry, .before = citizenship_status)
head(edu_df)



# 7. Adjust decade column variable
# Long crude, brute force for loop to change all entry_decade data entries
# unique(edu_df$entry_decade)
for (row in 1:nrow(edu_df)) {
  if (edu_df$entry_decade[row] == "N.A..Born.in.the.US. "
      | edu_df$entry_decade[row] == "X.1 "
      | edu_df$entry_decade[row] == "X.2 " 
      | edu_df$entry_decade[row] == "X.3 "
      | edu_df$entry_decade[row] == "X.4 "
      | edu_df$entry_decade[row] == "X.5 "
      | edu_df$entry_decade[row] == "X.6 ") {
    edu_df$entry_decade[row] = "born_us"
  } else if (edu_df$entry_decade[row] == "Before.1950 "
             | edu_df$entry_decade[row] == "X.7 "
             | edu_df$entry_decade[row] == "X.8 " 
             | edu_df$entry_decade[row] == "X.9 "
             | edu_df$entry_decade[row] == "X.10 "
             | edu_df$entry_decade[row] == "X.11 "
             | edu_df$entry_decade[row] == "X.12 ") {
    edu_df$entry_decade[row] = "before_1950"
  } else if (edu_df$entry_decade[row] == "X1950...1959 "
             | edu_df$entry_decade[row] == "X.13 "
             | edu_df$entry_decade[row] == "X.14 " 
             | edu_df$entry_decade[row] == "X.15 "
             | edu_df$entry_decade[row] == "X.16 "
             | edu_df$entry_decade[row] == "X.17 "
             | edu_df$entry_decade[row] == "X.18 ") {
    edu_df$entry_decade[row] = "1950_1959"
  } else if (edu_df$entry_decade[row] == "X1960...1969 "
             | edu_df$entry_decade[row] == "X.19 "
             | edu_df$entry_decade[row] == "X.20 " 
             | edu_df$entry_decade[row] == "X.21 "
             | edu_df$entry_decade[row] == "X.22 "
             | edu_df$entry_decade[row] == "X.23 "
             | edu_df$entry_decade[row] == "X.24 ") {
    edu_df$entry_decade[row] = "1960_1969"
  } else if (edu_df$entry_decade[row] == "X1970...1979 "
             | edu_df$entry_decade[row] == "X.25 "
             | edu_df$entry_decade[row] == "X.26 " 
             | edu_df$entry_decade[row] == "X.27 "
             | edu_df$entry_decade[row] == "X.28 "
             | edu_df$entry_decade[row] == "X.29 "
             | edu_df$entry_decade[row] == "X.30 ") {
    edu_df$entry_decade[row] = "1970_1979"
  } else if (edu_df$entry_decade[row] == "X1980...1989 "
             | edu_df$entry_decade[row] == "X.31 "
             | edu_df$entry_decade[row] == "X.32 " 
             | edu_df$entry_decade[row] == "X.33 "
             | edu_df$entry_decade[row] == "X.34 "
             | edu_df$entry_decade[row] == "X.35 "
             | edu_df$entry_decade[row] == "X.36 ") {
    edu_df$entry_decade[row] = "1980_1989"
  } else if (edu_df$entry_decade[row] == "X1990...1999 "
             | edu_df$entry_decade[row] == "X.37 "
             | edu_df$entry_decade[row] == "X.38 " 
             | edu_df$entry_decade[row] == "X.39 "
             | edu_df$entry_decade[row] == "X.40 "
             | edu_df$entry_decade[row] == "X.41 "
             | edu_df$entry_decade[row] == "X.42 ") {
    edu_df$entry_decade[row] = "1990_1999"
  } else if (edu_df$entry_decade[row] == "X2000...2009 " 
             | edu_df$entry_decade[row] == "X.43 "
             | edu_df$entry_decade[row] == "X.44 " 
             | edu_df$entry_decade[row] == "X.45 "
             | edu_df$entry_decade[row] == "X.46 "
             | edu_df$entry_decade[row] == "X.47 "
             | edu_df$entry_decade[row] == "X.48 ") {
    edu_df$entry_decade[row] = "2000_2009"
  } else if (edu_df$entry_decade[row] == "X2010.or.later "
             | edu_df$entry_decade[row] == "X.49 "
             | edu_df$entry_decade[row] == "X.50 " 
             | edu_df$entry_decade[row] == "X.51 "
             | edu_df$entry_decade[row] == "X.52 "
             | edu_df$entry_decade[row] == "X.53 "
             | edu_df$entry_decade[row] == "X.54 ") {
    edu_df$entry_decade[row] = "post_2009"
  } 
}

head(edu_df[which(edu_df$entry_decade == "post_2009"),])

# Adjust Education Level Column
unique(edu_df$education_level)
edu_df$education_level[which(edu_df$education_level == " no_highschool")] = "1_hs_no_degree"
edu_df$education_level[which(edu_df$education_level == " highschool_ged")] = "2_hs_degree"
edu_df$education_level[which(edu_df$education_level == " highschool_reg")] = "2_hs_degree"
edu_df$education_level[which(edu_df$education_level == " college_some_no_degree")] = "3_college_no_degree"
edu_df$education_level[which(edu_df$education_level == " college_degree")] = "4_college_degree"
edu_df$education_level[which(edu_df$education_level == " masters")] = "5_post_college_degree"
edu_df$education_level[which(edu_df$education_level == " doctorate")] = "5_post_college_degree"
unique(edu_df$education_level)

# Make pop column numeric
edu_df$pop = as.numeric(edu_df$pop)
tibble(edu_df)

# Group by education level because changed the column, need to sum new groups I combined.
edu_df = edu_df %>% group_by(ancestry, citizenship_status, entry_decade, education_level) %>%
  summarise(pop = sum(pop)); edu_df

# ENDING DATA FRAME
tibble(edu_df)


#################################################################################

# Merge with country info

edu_decade_country_aggr <- function (df_1, aggregate_funtion, aggregate_columns) {
  # Merge
  df = merge(regions_df, df_1, by = 'ancestry')
  
  # Aggregate by region_scope, using function argument
  aggr_df = aggregate(df[, aggregate_columns], by=list(entry_decade = df[, "entry_decade"],
                                                       education_level = df[, "education_level"],
                                                       citizenship_status = df[,'citizenship_status'],
                                                       country = df[, "country"],
                                                       sub_region = df[, "sub_region"],
                                                       region = df[, "region"]), 
                      FUN=aggregate_funtion)
  return (aggr_df)
}


region_edu_df = edu_decade_country_aggr(edu_df, "sum", c("pop"))
tibble(region_edu_df)

# Remove Born_US
region_edu_df = region_edu_df %>% filter(!entry_decade %in% c("born_us"))
colnames(region_edu_df)[which(colnames(region_edu_df) == 'x')] = c("pop"); region_edu_df

# Resultant df
tibble(region_edu_df)


#################################################################################

# Arrange dfs for plotting

# all countries subset, percent
imm_25_df$country
edu_all_df = region_edu_df %>% 
  select(country, education_level, pop) %>% 
  group_by(country, education_level) %>% 
  summarise(pop = sum(pop))
tibble(edu_25_df)

edu_all_totals = edu_all_df %>% 
  group_by(country) %>% 
  summarise(total_pop = sum(pop))

edu_all_per = merge(edu_all_df, edu_all_totals, by = "country")
edu_all_per = cbind(edu_all_per, percent = round(edu_all_per$pop/edu_all_per$total_pop * 100, 5))
edu_all_per = arrange(edu_all_per, education_level, percent); edu_all_per
tibble(edu_all_per)


# 1:25 subset, education levels
imm_25_df$country
edu_25_df = region_edu_df %>% 
  select(country, education_level, pop) %>% 
  group_by(country, education_level) %>% 
  summarise(pop = sum(pop))
edu_25_df = subset(edu_25_df, country %in% imm_25_df$country)
tibble(edu_25_df)

# 1:25 subset, percent
edu_25_totals = edu_25_df %>% 
  group_by(country) %>% 
  summarise(total_pop = sum(pop))

edu_25_per = merge(edu_25_df, edu_25_totals, by = "country")
edu_25_per = cbind(edu_25_per, percent = round(edu_25_per$pop/edu_25_per$total_pop * 100, 5))
edu_25_per = arrange(edu_25_per, education_level, percent); edu_25_per
tibble(edu_25_per)

# sub regions, percent
edu_sr_df = region_edu_df %>% 
  select(sub_region, education_level, pop) %>% 
  group_by(sub_region, education_level) %>% 
  summarise(pop = sum(pop)) %>% 
  filter(!sub_region == c(""))
tibble(edu_sr_df)

edu_sr_totals = edu_sr_df %>% 
  group_by(sub_region) %>% 
  summarise(total_pop = sum(pop))
edu_sr_totals

edu_sr_per = merge(edu_sr_df, edu_sr_totals, by = "sub_region")
edu_sr_per = cbind(edu_sr_per, percent = round(edu_sr_per$pop/edu_sr_per$total_pop * 100, 5))
edu_sr_per = arrange(edu_sr_per, education_level, percent); 
tibble(edu_sr_per)





