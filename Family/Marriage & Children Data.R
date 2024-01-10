# Objective: Marriage + Children



# Merge partner and children dfs


# Contruct dataframe
tibble(couple_df)
tibble(ave_kids_df)

imm_25_df


# Country
couple_25_df = filter(couple_df, country %in% imm_25_df$country)
ave_kids_25_df = filter(ave_kids_df, country %in% imm_25_df$country)
family = merge(couple_25_df, ave_kids_25_df, by = "country")
family = select(family, country, percent_partnered, ave_kids_per_person) %>%
  arrange(desc(ave_kids_per_person))
family

family$ave_kids_per_person = (family$ave_kids_per_person * -1)
family$ave_kids_per_person

family_df = data.frame(country=family$country,
                       ave_kids=family$ave_kids_per_person,
                       percent_partnered=family$percent_partnered)



# Sub Regions
family_reg = merge(couple_df, ave_kids_df, by = "country")
family_reg = select(family_reg, sub_region, single, partnered, ave_kids_per_person) %>%
  group_by(sub_region) %>% 
  summarise(partnered = sum(partnered), single = sum(single), ave_kids_per_person = mean(ave_kids_per_person))
family_reg = cbind(family_reg, percent_partnered = family_reg$partnered/(family_reg$partnered + family_reg$single) * 100)
tibble(family_reg)

family_reg$ave_kids_per_person = (family_reg$ave_kids_per_person * -1)
family_reg$ave_kids_per_person

family_reg_df = data.frame(sub_region=family_reg$sub_region,
                       ave_kids=family_reg$ave_kids_per_person,
                       percent_partnered=family_reg$percent_partnered)
