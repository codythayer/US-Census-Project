# Objective: Children


# Construct base children df
kids = read.csv("ancestry_data - kids.csv")
head(kids)

# Create population column (number of people in survey)
kids = cbind(kids, country_pop = rowSums(kids[,2:14])); kids

# Mulitple by number of kids
num_kids = kids
for (num in 0:12){
  col = num + 2
  num_kids[, col] = num_kids[, col] * num
}
head(num_kids)

# Reshape to Long Form
num_kids = num_kids %>% 
  gather(variable, num_kids, X0_kids : X12_plus_kids); head(num_kids)

kids_df = num_kids %>% group_by(ancestry, country_pop) %>% 
  summarise(total_kids = sum(num_kids)); kids_df

# Merge with country info
kids_df = country_aggr(kids_df, "sum", c("total_kids", "country_pop"))
colnames(kids_df)[which(colnames(kids_df) == 'x')] = c("total_kids"); kids_df
nrow(kids_df)

# Average kids
head(kids_df)
ave_kids_df = kids_df %>% summarise(country, total_kids, country_pop, ave_kids_per_person = total_kids/country_pop) %>%
  arrange(desc(ave_kids_per_person))
tibble(ave_kids_df)
