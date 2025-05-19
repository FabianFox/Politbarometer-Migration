# Wichtige Probleme in Deutschland - Politbarometer

# Load/install packages
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "hrbrthemes", "lubridate", "sjlabelled")

# Load data
barometer.orig <- import("./data/ZA2391_v15-0-0.dta")

# Wrangle
barometer.df <- barometer.orig %>%
  janitor::clean_names() %>%
  mutate(across(c(v33, v34), ~as_character(.x), .names = "{.col}_chr")) %>%
  select(month = v3, year = v4, erstnennung = v33_chr, zweitnennung = v34_chr, 
         repweight = 78)

# Add labels
barometer.df <- barometer.df %>%
  mutate(across(c(erstnennung, zweitnennung), ~replace(.x, .x %in% 
                                                         c("nicht erhoben", "KA", "weiß nicht"), 
                                                       NA_character_)),
         across(c(erstnennung, zweitnennung), ~if_else(str_detect(.x, "Asyl|Ausländer"), 
                                                  c("Migration"), 
                                                  .x)),
         across(c(erstnennung, zweitnennung), ~if_else(str_detect(.x, "Energie|Hitze|Hochwasser"), 
                                                       c("Energe/Klima"), 
                                                       .x)),
         migration = case_when(
           erstnennung == "Migration" | zweitnennung == "Migration" ~ 1, TRUE ~ 0),
         arbeitslosigkeit = case_when(
           erstnennung == "Arbeitslosigkeit" | zweitnennung == "Arbeitslosigkeit" ~ 1, TRUE ~ 0),
         wirtschaftslage = case_when(
           erstnennung == "Wirtschaftslage" | zweitnennung == "Wirtschaftslage" ~ 1, TRUE ~ 0),
         energie_klima = case_when(erstnennung == "Energie/Klima" | zweitnennung == "Energie/Klima" ~ 1, TRUE ~ 0)) %>%
  group_by(month, year) %>%
  summarise(across(c(migration, arbeitslosigkeit, wirtschaftslage, energie_klima), 
                   ~weighted.mean(.x, w = repweight) * 100,
                   .names = "{.col}")) %>%
  ungroup() %>%
  mutate(date = parse_date_time(str_c(month, "/", year), "m/Y")) %>%
  pivot_longer(cols = c(migration, arbeitslosigkeit, wirtschaftslage, energie_klima), names_to = "concern", 
               values_to = "pct") %>%
  select(-c(month, year))

# Add data for 2023f. (Part I)
barometer_new.df <- import("https://www.forschungsgruppe.de/Umfragen/Politbarometer/Langzeitentwicklung_-_Themen_im_Ueberblick/Politik_II/9_Probleme_1.xlsx",
                           skip = 7) %>%
  tibble() %>%
  rename(date = 1) %>%
  janitor::clean_names() %>%
  .[-1,] %>%
  pivot_longer(cols = -1, names_to = "concern", values_to = "pct") %>%
  mutate(concern = if_else(concern == "auslander_integration_fluchtlinge", "migration", concern),
         concern = if_else(concern == "energie_versorgung_klima", "energie_klima", concern)) %>%
  filter(concern %in% c("migration", "energie_klima"), year(date) > 2022)

# Add data for 2022f. (Part II)
barometer_new1.df <- import("https://www.forschungsgruppe.de/Umfragen/Politbarometer/Langzeitentwicklung_-_Themen_im_Ueberblick/Politik_II/10_Probleme_2.xlsx",
                            skip = 7) %>%
  rename(date = 1) %>%
  janitor::clean_names() %>%
  .[-1,] %>%
  pivot_longer(cols = -1, names_to = "concern", values_to = "pct") %>%
  filter(concern %in% c("wirtschaftslage", "arbeitslosigkeit"), year(date) > 2022)

# Merge
barometer.df <- barometer.df %>%
  bind_rows(list(barometer_new.df, barometer_new1.df))

# Add direct labels
barometer.lbl <- barometer.df %>% 
  group_by(concern) %>% 
  arrange(desc(date)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>%
  mutate(label = str_to_sentence(concern),
         date = ymd(date),
         colour = case_match(label,
                             "Migration" ~ "#0098D4",
                             "Arbeitslosigkeit" ~ "#003C76",
                             "Wirtschaftslage" ~ "#7E8015"))
  
# Politbarometer: Migration
polit.fig <- barometer.df %>%
  filter(year(date) >= 2000) %>%
  mutate(concern = factor(concern, 
                          levels = c("migration", 
                                     "arbeitslosigkeit", 
                                     "wirtschaftslage"),
                          labels = c("Migration", 
                                     "Arbeitslosigkeit",
                                     "Wirtschaftslage")),
         date = ymd(date),
         colour = case_match(concern,
                             "Migration" ~ "#0098D4",
                             "Arbeitslosigkeit" ~ "#003C76",
                             "Wirtschaftslage" ~ "#7E8015")) %>%
  ggplot(aes(x = date, y = pct)) +
  geom_line(aes(colour = colour), size = 1.5) + 
  geom_text(data = barometer.lbl, aes(label = label, colour = colour), vjust = 0, hjust = 0, family = "Tahoma", size = 8) +
#  geom_vline(aes(xintercept = as.Date("2023/09/09")), linewidth = 1.5, alpha = .5, colour = "grey25", linetype = "dashed") +
 # annotate("text", x = as.Date("2023/10/30"), y = 85, label = "Erhebungs-\nzeitraum", vjust = 0, hjust = 0, family = "Tahoma", size = 6) +
  scale_x_date(breaks = c(seq.Date(as.Date("1990/01/01"), as.Date("2025/05/02"), "5 years")), date_labels = "%Y") +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  scale_colour_identity() +
  coord_cartesian(expand = TRUE, clip = "off") +
  labs(x = "", y = "", colour = "",
#       title = "<span style = 'font-size:20pt; font-family:Tahoma;'>Wichtige Probleme in Deutschland</span><br>
#       Relativer Anteil von <span style = 'color:#0098D4;'>Migration</span>,
#       <span style = 'color:#003C76;'>Arbeitslosigkeit</span> und
#       <span style = 'color:#7E8015;'>Wirtschaftslage</span> unter allen Nennungen",
#       subtitle = 'Befragte können maximal zwei Probleme nennen.',
       caption = '') +
  theme_ipsum(ticks = TRUE, strip_text_size = 16, subtitle_size = 18,
              caption_size = 16, caption_face = "plain", grid = "y",
              axis_title_size = 18, base_family = "Tahoma", base_size = 20) +
  theme(legend.position = "none", 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0),
        text = element_text(family = "Tahoma", colour = "black"),
        axis.text = element_text(colour = "black"),
        plot.title = ggtext::element_markdown(size = 20, lineheight = 1.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.5),
        axis.ticks.y = element_line(colour = "black", size = 0.5),
        plot.margin = margin(30, 125, 10, 10))

# Export
ggsave("./figures/Politbarometer.png", polit.fig, height = 22, width = 36, units = "cm", bg = "white", device = ragg::agg_png, dpi = "retina")
