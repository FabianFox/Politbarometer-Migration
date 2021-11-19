# Load/install packages
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "fst", "cowplot")

# Load data
barometer.orig <- import("./data/ZA2391_v12-0-0.dta")

# Serialize file for faster loading
# write.fst(barometer.df, "./data/barometer.fst")
barometer.df <- read.fst("./data/barometer.fst")

# Labels
label_v33 <- tibble(
  value = attributes(barometer.orig$v33)$labels,
  label_v33 = names(value))

label_v34 <- tibble(
  value = attributes(barometer.orig$v34)$labels,
  label_v34 = names(value))

# Wrangle
barometer.df <- barometer.df %>%
  select(month = v3, year = v4, erstnennung = v33, zweitnennung = v34, repweight = 78, gesweight = 81)

# Add labels
barometer.df <- barometer.df %>%
  left_join(y = label_v33, by = c("erstnennung" = "value")) %>%
  left_join(y = label_v34, by = c("zweitnennung" = "value")) %>%
  mutate(across(c(label_v33, label_v34), ~replace(.x, .x %in% c("nicht erhoben", "KA", "weiß nicht"), NA_character_)),
         across(c(label_v33, label_v34), ~if_else(str_detect(.x, "Asyl|Ausländer"), c("Migration"), .x)),
         migration = case_when(label_v33 == "Migration" | label_v34 == "Migration" ~1,
                               TRUE ~ 0)) %>%
  group_by(month, year) %>%
  summarise(pct = weighted.mean(migration, w = repweight)) %>%
  ungroup() %>%
  mutate(date = zoo::as.yearmon(str_c(month, "/", year), "%m/%Y"))

# Politbarometer: Migration
polit.fig <- ggplot(barometer.df, aes(x = date, y = pct)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Wichtige Probleme in Deutschland", 
       subtitle = 'Nennung von "Ausländer" und "Asylanten, Asyl"\n(max. zwei Nennungen)',
       caption = 'Daten: Forschungsgruppe Wahlen "Politbarometer"') +
  theme_minimal_grid()

# Export
ggsave("./figures/Politbarometer.tiff", polit.fig, height = 7, width = 20, units = "cm")
