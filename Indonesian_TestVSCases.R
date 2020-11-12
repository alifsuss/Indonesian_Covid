if (!require("pacman")) install.packages("pacman")

#Make sure you need this
setwd("~/R/Indonesian_Covid")

pacman::p_load(GGally, corrplot, ggpmisc, PGRdup, datasets, pacman, tidyverse, rio, dplyr, ggplot2, magrittr)
#IMPORTING ONLINE DATA
df <- import("Book1.csv")
df <- as_tibble(df)

#DECLARING TODAY'S DATE
today.date = Sys.Date()

df_id <- df 
df_id %<>%
  mutate(date = V1) %>%
  select(date, kasusbaru = "Kasus baru", totalkasus = "Total kasus", kasusaktif = "Kasus aktif", sembuhbaru = SembuhBaru, sembuh = Sembuh, meninggalbaru = MeninggalBaru, meninggal = Meninggal, spesimen = Spesimen, dites = "Orang yang dites")
df_id$date <- paste(df_id$date, "2000", sep = "-")          #Adding year to the column
df_id$date <- as.Date(df_id$date, format = "%d-%b-%y")      #SETTING UP THE dateRep AS DATE
as_tibble(df_id)

#DELETING comma
df_id[,-c(1)] <- sapply(df_id[,-c(1)], function(x) as.numeric(gsub(",","",x)))
df_id <- na.omit(df_id)


#PLOTTING AND FITTING MODEL
my.formula <- y ~ x
ggplot(df_id, aes(y = kasusbaru)) + 
  geom_point(aes(x = dites, color="spesimen ditest"),size = 3) +
  geom_point(aes(x = spesimen, color="total specimen"),size = 3) +
  geom_smooth(aes(x = dites), method = "lm", se = TRUE, color = "red", formula = my.formula) +
  stat_poly_eq(aes(x = dites, label = paste(..eq.label.., ..rr.label.., sep ="~~~")),
               formula = my.formula, parse = TRUE, size = 5) +
  theme(
    axis.line = element_line(size = 1),
    axis.text=element_text(size=17),
    axis.title=element_text(size=17,face="bold"),
    legend.text=element_text(size=17),
    legend.justification=c(1,1),
    legend.position=c(0.5,0.85),
    legend.title= element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Spesimen") +
  ylab("Kasus Baru") +
  ggsave(filename = paste0("Indonesia_test_vs_kasus", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 5.3 , height = 8.49, dpi = 300, units = "in")
