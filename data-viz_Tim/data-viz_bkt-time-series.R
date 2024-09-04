library(here)
library(tidyverse)
library(RColorBrewer)
library(viridis)

# Read in data
rm(list = ls())
data.n <- read.csv(here("data", "n.csv"))
trendOut <- read.csv(here("data", "trendOut.csv"))
predSiteAttributes <- read.csv(here("data", "predSiteAttributes.csv"))
jagsData <- readRDS(here("data", "jagsData.rds"))

# Prepare data frame with counts and site info
df <- data.n %>%
  group_by(catchment, SiteID, year, AGE) %>% # group by
  filter(!is.na(N)) %>% # remove NA counts
  summarize(N.tot = sum(N)) %>% # total count across passes
  left_join(trendOut, by = c("SiteID", "catchment")) %>%# add site covariates
  mutate(AGE = as.factor(AGE),
         catchment = as.character(catchment))

# reorder SiteID factor
df$streamSite <- paste0(df$STREAM, " (", df$SiteID, ")")
df$streamSite <- factor(df$streamSite, levels = unique(df$streamSite))
df$streamSite <- reorder(df$streamSite, df$Elev_m, decreasing = TRUE)

# plot(df$SiteID, df$Elev_m) # quick check that SiteID factor is ordered by descending elevation)



# Plot survey counts by year, for each site
# (1 graph per catchment)
pdf(file = here("data-viz_Tim", "count-time-series-by-site_log.pdf"),
    width = 6, height = 6)
for(i in 1:length(unique(df$catchment))) {
  # Plot by site, color by elevation
  g <- ggplot(data = df[df$catchment == unique(df$catchment)[i],],
              mapping = aes(x = year, y = N.tot+1, color = streamSite)) +
    geom_point() +
    geom_line() + 
    labs(x = "Year", y = "Total count (N+1)", color='Site') +
    scale_color_viridis(discrete = TRUE, option = "viridis") +
    facet_wrap(~paste("Age", AGE), scales = "free_y", ncol = 1) +
    scale_y_log10()
  print(g)
}
dev.off()

pdf(file = here("data-viz_Tim", "count-time-series-by-site_raw.pdf"),
    width = 6, height = 6)
for(i in 1:length(unique(df$catchment))) {
  # Plot by site, color by elevation
  g <- ggplot(data = df[df$catchment == unique(df$catchment)[i],],
              mapping = aes(x = year, y = N.tot, color = streamSite)) +
    geom_point() +
    geom_line() + 
    labs(x = "Year", y = "Total count (N)", color='Site') +
    scale_color_viridis(discrete = TRUE, option = "viridis") +
    facet_wrap(~paste("Age", AGE), scales = "free_y", ncol = 1)
  print(g)
}
dev.off()
