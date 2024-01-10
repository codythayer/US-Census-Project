# Objective: Citizenship


# Create base citizenship df
citizen = read.csv("ancestry_data - citizenship_2.csv")
tibble(citizen)
citizen_reshape_df = melt(citizen, value.name = "pop", id = c("ancestry", "citizenship"))
colnames(citizen_reshape_df)[which(colnames(citizen_reshape_df) == 'variable')] = c("entry_decade")
tibble(citizen_reshape_df)

# Merge with country info
citizenship_df = merge(regions_df, citizen_reshape_df, by = 'ancestry')
citizenship_df
citizenship_df = aggregate(citizenship_df[, "pop"], by=list(entry_decade = citizenship_df[, "entry_decade"],
                                                            citizenship = citizenship_df[, "citizenship"],
                                                            country = citizenship_df[, "country"],
                                                            sub_region = citizenship_df[, "sub_region"],
                                                            region = citizenship_df[, "region"]), 
                           FUN=sum)

# Check (Checks out)
citizenship_df[which(citizenship_df$country == "Albania"),]

# Change name of column
colnames(citizenship_df)[which(colnames(citizenship_df) == 'x')] = c("pop")

# Fix entry_decade variable
levels(citizenship_df$entry_decade)
levels(citizenship_df$entry_decade) = c("born_us", "before_1950","1950_1959", "1960_1969",
                                        "1970_1979", "1980_1989", "1990_1999", "2000_2009", 
                                        "post_2009")

# # Adjusting variables for plotting (easier than deal with plot_ly)
levels(citizenship_df$entry_decade) = c("born_us", "Pre 1950", "1950s", "1960s", "1970s", 
                                        "1980s", "1990s", "2000s", "Post 2009")

tibble(citizenship_df)
# Remove born_us
citizenship_df = citizenship_df[which(citizenship_df$entry_decade != "born_us"), ]

# Final df
tibble(citizenship_df)


################################################################################

# Creating Different Subsets dfs

## Mexico
mex_citizenship_df = citizenship_df[which(citizenship_df$country == "Mexico"),]
decade_pop_mex_df = aggregate(mex_citizenship_df[, "pop"], by=list(entry_decade = mex_citizenship_df[, "entry_decade"],
                                                                   country = mex_citizenship_df[, "country"]), FUN=sum)

mex_citizenship_df_per = merge(mex_citizenship_df, decade_pop_mex_df, by = "entry_decade")
colnames(mex_citizenship_df_per )[which(colnames(mex_citizenship_df_per ) == 'x')] = c("decade_pop")

mex_citizenship_per = cbind(mex_citizenship_df_per , 
                            percent = round(mex_citizenship_df_per$pop/mex_citizenship_df_per$decade_pop *100, 2))
tibble(mex_citizenship_per)


## Country
decade_pop_df = aggregate(citizenship_df[, "pop"], by=list(entry_decade = citizenship_df[, "entry_decade"],
                                                           country = citizenship_df[, "country"]), FUN=sum)
colnames(decade_pop_df)[which(colnames(decade_pop_df) == 'x')] = c("total_pop")
citizen_per = merge(citizenship_df, decade_pop_df, by.x = c("country", "entry_decade"), 
                    by.y = c("country", "entry_decade"))
citizen_per = cbind(citizen_per, percent = round(citizen_per$pop/citizen_per$total_pop * 100, 5))

# # Check:
# citizen_per[which(citizen_per$country == "Sweden"),]
# country_citizen_per = citizen_per
tibble(country_citizen_per)


## Sub Region
sub_region_cit = citizen_per %>% 
  group_by(sub_region, citizenship, entry_decade) %>% 
  summarise(pop = sum(pop))

# # Check: sum(pop) worked
# sum(sub_region_cit[which(sub_region_cit$sub_region == "Africa"),4])
# sum(citizen_per[which(citizen_per$sub_region == "Africa"),6])
# sub_region_cit

# Get Total Pop
reg_decade_pop_df = aggregate(citizenship_df[, "pop"], by=list(entry_decade = citizenship_df[, "entry_decade"],
                                                               sub_region = citizenship_df[, "sub_region"]), FUN=sum)
colnames(reg_decade_pop_df)[which(colnames(reg_decade_pop_df) == 'x')] = c("total_pop")
reg_citizen_per = merge(sub_region_cit, reg_decade_pop_df, by.x = c("sub_region", "entry_decade"), 
                        by.y = c("sub_region", "entry_decade"))

# Remove Empty Sub Region
reg_citizen_per = subset(reg_citizen_per, (reg_citizen_per$sub_region != ""))
reg_citizen_per = cbind(reg_citizen_per, percent = round(reg_citizen_per$pop/reg_citizen_per$total_pop * 100, 5))

# # Check
# reg_citizen_per[which(reg_citizen_per$sub_region == "Oceanic"),]
# asia_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Asia")); tibble(asia_cit)
# africa_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Africa"))
# europe_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Europe"))
# na_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "North America"))
# la_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Latin America")); tibble(la_cit)
# me_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Middle East")); tibble(me_cit)
# ocean_cit = subset(reg_citizen_per, (reg_citizen_per$sub_region == "Oceanic")); tibble(ocean_cit)
tibble(reg_citizen_per)


################################################################################

# Creating Plotting dfs

# Country citizenship rates line graph
country_citizen_per_df = subset(country_citizen_per, country_citizen_per$country != "")
country_per_citizen_df = subset(country_citizen_per_df, country_citizen_per_df$citizenship == "not_us_citizen")
country_per_citizen_df = subset(country_per_citizen_df, country %in% imm_25_df$country)
tibble(country_per_citizen_df)

decades = c("Pre 1950", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "Post 2009")
country_per_citizen_df = country_per_citizen_df %>%
  arrange(match(entry_decade, decades))

country_per_citizen_df$percent = country_per_citizen_df$percent/100
tibble(country_per_citizen_df)

