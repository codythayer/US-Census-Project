# Final Project

# All libraries used
library(plyr)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(reshape2)
library(prob)
library(sampling)
library(gridExtra)  
library(scales)
library(viridis)
library(cowplot)

options(scipen=5)

# Color Palettes (viridis)
show_col(viridis_pal()(25))
colors = c(viridis(25)[1], viridis(25)[4], viridis(25)[6], viridis(25)[8])
colors_2 = c(viridis(25)[9], viridis(25)[11], viridis(25)[13], viridis(25)[15])
colors_3 = c(viridis(25)[1], viridis(25)[8])
colors_4 = c(viridis(25)[1], viridis(25)[5], viridis(25)[12], viridis(25)[18])
colors_5 = c(viridis(25)[3], viridis(25)[7], viridis(25)[18], viridis(25)[25])
colors_6 = c(viridis(25)[4], viridis(25)[10], viridis(25)[15], viridis(25)[19])



# Country Information Dataframe:

create_regions_df <- function (df) {
  # Create Regional Info Dataframe
  colnames(df)[which(colnames(df) == "sub.region")] = "sub_region"
  colnames(df)[which(colnames(df) == "name")] = "country"
  head(df)
  
  # Create own regional grouping. inter_region
  Europe = c("Europe", "Western Europe", "Northern Europe", "Southern Europe")
  df$sub_region[which(df$sub_region %in% Europe)] = "Europe"
  Asia = c("Asia", "Central Asia", "Eastern Asia", "South-eastern Asia", "Southern Asia")
  df$sub_region[which(df$sub_region %in% Asia)] = "Asia"
  Africa = c("Africa", "Western Africa", "Northern Africa", "Sub-Saharan Africa")
  df$sub_region[which(df$sub_region %in% Africa)] = "Africa"
  Oceanic = c("Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia")
  df$sub_region[which(df$sub_region %in% Oceanic)] = "Oceania"
  North_America = c("Northern America")
  df$sub_region[which(df$sub_region %in% North_America)] = "North America"
  Latin_America = c("Latin America and the Caribbean")
  df$sub_region[which(df$sub_region %in% Latin_America)] = "Latin America"
  Middle_East = c("Western Asia", "Eastern Europe")
  df$sub_region[which(df$sub_region %in% Middle_East)] = "Middle East"
  
  # # Drop ancestry (Can;t do this here, need ancestry to join dfs)
  # df = select(df, -ancestry)
  # Shuffle Columns
  df = df %>% relocate(sub_region, .before = "region")
  head(df)
  regions_df = df
}


country_info_df = read.csv("ancestry_data - regions.csv")
regions_df = create_regions_df(country_info_df)
head(regions_df)

# Check Results
regions_df[which(regions_df$sub_region == ""),]
head(regions_df)





################################################################################

# Construct base dataframe
# This is the NEW FUNCTION
# 1. Merge with regional info
# 2. Aggregate by country.
country_aggr <- function (df_1, aggregate_funtion='sum', aggregate_columns) {
  # Merge
  df = merge(regions_df, df_1, by = 'ancestry')
  head(df)
  
  colnames(regions_df)
  # Aggregate by region_scope, using function argument
  aggr_df = aggregate(df[, aggregate_columns], by=list(country = df[, "country"],
                                                       sub_region = df[, "sub_region"],
                                                       region = df[, "region"]), 
                      FUN=sum)
  
  print(head(aggr_df))
  
  # Check for NULL
  null_rows = which(aggr_df[, country] == '')
  print(paste("Removed NULL rows in dataset:", toString(null_rows)))
  aggr_df = aggr_df[-(null_rows),]
  
  return (aggr_df)
}





citizen = read.csv("ancestry_data - citizenship_2.csv")
aggr_cols = c("us_born", "pre_1950", "X1950_1959", "X1960_1969", "X1970_1979", "X1980_1989", "X1990_1999", "X2000_2009", "post_2009")
imm_df = country_aggr(df_1 = citizen, aggregate_funtion = 'sum', aggregate_columns = aggr_cols)

imm_df

