# Objective Distribution and Sampling


# Starting df
tibble(salary_by_country)
unique(salary_by_country$ave_earnings)


################################################################################

## Median Earnings Distribution

plot(ave_earning_dist)


################################################################################

## Central Limit Theorem

# Print Population Stats
cat("Population", " Mean = ", mean(salary_by_country$ave_earnings),
    " SD = ", sd(salary_by_country$ave_earnings))

set.seed(1883)
samples = 1000
sample_sizes = c(10, 20, 30, 40)

xbar = numeric(samples)
xbar_df = data.frame(xbar)
for (size in sample_sizes) {
  row = which(sample_sizes == size)
  for (i in 1:samples) {
    # Replace = FALSE means you will not pick the same value twice, because you will not replace when it is picked.
    xbar[i] <- mean(sample(salary_by_country$ave_earnings, size, replace = FALSE))
  }
  # Store xbar 
  xbar_df = cbind(xbar_df, xbar)

  # Print mean and std.
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

# xbar_df plotting
colnames(xbar_df) = c(0, 10, 20, 30, 40)


################################################################################

# Sampling Techniques
set.seed(1883)
salary_s = salary_2_df %>% group_by(country, sub_region) %>% summarise(ave_earnings = mean(ave_earnings))
head(salary_s)
sample_size = 50


# SRWSOR
s = srswor(sample_size, nrow(salary_s))
s = which(s != 0)
sample_srswor = salary_s[s,]
sample_srswor
# mean(sample_srswor$ave_earnings)
# sd(sample_srswor$ave_earnings)

# Systematic (Inclusion Probabilities)
head(imm_sum_df) # from Total Immigration Objective
salary_sys = merge(salary_s, imm_sum_df, by = "country") %>% select(country, ave_earnings, total_immigration)
head(salary_sys)


pik = inclusionprobabilities(salary_sys$total_immigration, 50)
pik

s_sys = UPsystematic(pik)
sample_sys = salary_sys[s_sys != 0, ]
sample_sys
# mean(sample_sys$ave_earnings)
# sd(sample_sys$ave_earnings)

# Stratified
sub_region_freq = table(salary_s$sub_region)
sub_region_freq

# if increase this to 75, North America frequency becomes 1 and it works. Doesnt work at 50.
# strata_sizes = round(75 * sub_region_freq/sum(sub_region_freq), 4)
# or if you round so North America is 1

strata_sizes = round(50 * sub_region_freq/sum(sub_region_freq), 0)
strata_sizes

order.index = order(salary_s$sub_region)
order.index
data = salary_s[order.index, ]
data
colnames(data)
unique(data$sub_region)
c("sub_region")
data$sub_region
data

st = strata(data, stratanames = c("sub_region"), size=strata_sizes, method = 'srswor')
nrow(st)
st

sample_strata = getdata(data, st)
sample_strata
# mean(sample_strata$ave_earnings)
# sd(sample_strata$ave_earnings)


