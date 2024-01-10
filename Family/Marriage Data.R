# Objective: Marriage

# Contruct base partner df
marriage = read.csv("ancestry_data - marriage.csv")
partner = read.csv("ancestry_data - partner.csv")
couple = merge(marriage, partner, by = "ancestry")
head(couple)

couple = cbind(couple, partnered = 0)
couple = cbind(couple, single = 0)
for (row in 1:nrow(couple)) {
  couple$partnered[row] = couple$married[row] + couple$unmarried_partner[row]
  couple$single[row] = couple$not_married[row] + couple$no_unmarried_partner[row]
}
head(couple)

desired_cols = c("ancestry", "partnered", "single")
couple = couple[, desired_cols]
head(couple)

df = merge(regions_df, couple, by = 'ancestry')
couple_df = aggregate(df[, c("partnered", "single")], by=list(country = df[, "country"],
                                                     sub_region = df[, "sub_region"],
                                                     region = df[, "region"]), FUN=sum)

tibble(couple_df)


################################################################################

# Creating Plotting dfs

# I'm going to not include NAs as part of population or as single.
couple_df = cbind(couple_df,
                  percent_partnered = round(couple_df$partnered/(couple_df$single + couple_df$partnered)*100, 2))
tibble(couple_df)

couple_df = cbind(couple_df, per_single = round(couple_df$single/(couple_df$single + couple_df$partnered)*100, 2))
tibble(couple_df)


