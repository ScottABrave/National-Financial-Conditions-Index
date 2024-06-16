library(tidyverse)
library(ggplot2)
library(ggpubr)
library(httr)

# Download latest data
r <- httr::GET("https://www.chicagofed.org/-/media/publications/nfci/nfci-data-series-csv.csv")
bin <- httr::content(r, "raw")
writeBin(bin,here::here("NFCI.csv"))

indexes <- read.csv(here::here("NFCI.csv")) %>%
              mutate(date = as.Date(Friday_of_Week,format="%m/%d/%Y"),
                     nfci_adj = (NFCI-median(NFCI))/mad(NFCI))

nfci <- ggplot(indexes)+
  geom_hline(yintercept=0, color = 'black') +
  geom_line(aes(date, NFCI, color = "blue"), linewidth = 0.5) +
  labs(x="", y = "Std. Devs.", title = "National Financial Conditions Index", 
       subtitle = "standard deviations from average conditions",
       caption = "Centered with the sample mean and scaled by sample standard deviation.") +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = '4 year') +
  scale_color_identity(name = "",
                       breaks = c("blue"),
                       labels = c("NFCI"),
                       guide = "legend") +
  theme_light() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust=0.5))

nfci_adj <- ggplot(indexes)+
  geom_hline(yintercept=0, color = 'black') +
  geom_line(aes(date, nfci_adj, color = "blue"), linewidth = 0.5) +
  labs(x="", y = "Std. Devs.", title = "Rescaled National Financial Conditions Index", 
       subtitle = "standard deviations from typical conditions", 
       caption = "Centered with the sample median and scaled by 1.4826*sample median absolute deviation.") +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = '4 year') +
  scale_color_identity(name = "",
                       breaks = c("blue"),
                       labels = c("NFCI"),
                       guide = "legend") +
  theme_light() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust=0.5))

figure <- ggarrange(nfci,nfci_adj, ncol = 1, nrow = 2)

annotate_figure(figure,
                bottom = text_grob("Source: https://www.chicagofed.org/research/data/nfci/about", color = "black", size = 10))
