library(tidyverse)
library(haven)
library(extrafont)

loadfonts(device = "win")


mdp <- read_dta("https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/!find?id=03d4277d-4f23-42eb-8269-7c9ff746f613-33.31")

# If not working, read mpd2020.dta in the repository

# | country == "Ireland" | country == "Portugal" | 

mdp %>%
  filter((country == "Georgia" | country == "Armenia" | country == "Ukraine" | country == "Poland" | country == "Slovakia" | country == "Estonia") & year > 1979)%>%
  mutate(year = lubridate::mdy(paste0("01-", "-01-", year)))%>%
  group_by(country)%>%
  mutate(max_gdp = gdppc[year == "1989-01-01"]) %>%
  ggplot(aes(year, gdppc, group = country, color=country))+
    geom_line(size=1)+
    geom_hline(aes(yintercept=max_gdp), linetype = "dashed")+
    scale_color_manual(values=c("#e41a1c","#377eb8", "#4daf4a", "#3d405b", "#984ea3", "#ff7f00"))+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
    labs(title="Historical GDP (PPP) Estimates in 2018 US Dollars",
         subtitle = "The dashed line represents the 1991 level of GDP",
         caption="Maddison Project Database, version 2020. Bolt, Jutta and Jan Luiten van Zanden (2020),\n'Maddison style estimates of the evolution of the world economy. A new 2020 update'")+
    theme_bw()+
    facet_wrap(~country)+
    theme(axis.title = element_blank(),
        text = element_text(family= "FiraGO"),
        legend.position = "none",
        strip.text = element_text(size=12, family="FiraGO", face="bold"),
        plot.title = element_text(size=14, face="bold", family="FiraGO"),
        plot.subtitle = element_text(size=12, family="FiraGO"),
        axis.text = element_text(size=10, face="bold", family="FiraGO", color = "black"))

ggsave("mdp_hist.png", width = 10, height=5)

# pwt <- read_dta("pwt100.dta")
