


library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(gganimate)
library(grid)
library(gridExtra)


CarDatasetFull = read.csv("C:/Users/Utente/Desktop/ALL/Projects/R/CarDatasetProject/data_full.csv")


# %in% è un operatore di confronto in R che verifica se un valore è presente in un vettore o in un insieme di valori.
CarDatasetClean <- CarDatasetFull %>%
  filter(!brand %in% c("BUIC", "GENESIS", "GMC", "INFINITI", "LADA", "LINCOLN", "MAYBACH", "MERCURY", "NIO", "OLDSMOBILE", "PERODUA")) %>%
  mutate(torqueClean = as.numeric(str_extract(torque, "\\d+"))) %>%
  select(brand, model, production_years, segment, engine_specs_title, cylinders, displacement, power, fuel, drive_type, combined, torqueClean)



CarDatasetClean <- CarDatasetClean %>% 
  mutate(Horsepower = as.numeric(str_extract(str_match(power, "\\d+\\.?\\d* HP"), "\\d+\\.?\\d*"))) %>%
  filter(displacement != "") %>%
  mutate(displacement = as.numeric(str_replace(displacement, " cm3", ""))) %>%
  mutate(combined = as.numeric(gsub("(^\\d+\\.?\\d?)|( mpg US)|(\\(|\\))|( L/100Km)", "", combined))) %>%
  mutate(combined = 100 / combined) %>% # conversione da l/100km a km/l
  filter(displacement <= 10000) %>%
  filter(!is.na(Horsepower)) %>%
  filter(Horsepower >= 50 & Horsepower < 1500) %>%
  mutate(horsepower_per_liter = Horsepower / (displacement / 1000)) %>% 
  mutate(cylinders_number = as.numeric(str_remove_all(cylinders, "[A-Za-z]"))) %>%
  mutate(start_year_production = as.numeric(str_extract(production_years, "^\\d+"))) %>%
  mutate(end_year_production = as.numeric(str_extract(production_years, "\\d+$"))) %>%
  mutate(year_segment = case_when(
    start_year_production < 1980 ~ "< 1980",
    start_year_production >= 1980 & start_year_production < 2001 ~ "1980-2001",
    start_year_production >= 2001 & start_year_production < 2011 ~ "2001-2011",
    start_year_production >= 2011 & start_year_production < 2015  ~ "2011-2015",
    start_year_production >= 2015  ~ "2015-2023",
    TRUE ~ NA_character_
  )) %>%
  mutate(start_year_production = as.numeric(start_year_production))




CarDatasetClean_bigTimeGaps <- CarDatasetClean %>% 
  mutate(year_segment = case_when(
    start_year_production < 2001 ~ "1950-2001",
    start_year_production >= 2001 & start_year_production < 2011 ~ "2001-2011",
    start_year_production > 2011 ~ "2011-2023",
    TRUE ~ NA_character_
  )) %>%
  na.omit()

grafico_dispersione <- ggplot(data = CarDatasetClean_bigTimeGaps, aes(x = Horsepower, y = displacement, color = factor(year_segment))) +
  geom_point(alpha = 0.6, size = 1.5) +
  #geom_smooth(method = lm, se = FALSE, color = "black", size = 0.9) + 
  xlab("Horsepower") +
  ylab("Displacement") +
  scale_color_manual(values = c("1950-2001" = "#f8766d", "2001-2011" = "#00bfc4", "2011-2023" = "#f564e3"),
                     guide = guide_legend(title = "Periodo Temporale")) +
  theme_minimal()






# creazione dataframe per rapporto cilindrata-cavlli e periodo temporale

frm_averages <- CarDatasetClean %>%
  group_by(year_segment) %>%
  summarize(mean_horsepower_per_liter = mean(horsepower_per_liter, na.rm = TRUE),
            mean_horsepower = mean(Horsepower, na.rm = TRUE),
            mean_displacement = mean(displacement, na.rm = TRUE)) %>%
  select(year_segment, mean_horsepower_per_liter, mean_horsepower, mean_displacement)


# Creazione del grafico a linee
plotline_averages <- ggplot(data = frm_averages, aes(x = year_segment, y = mean_horsepower_per_liter, group = 1)) +
  geom_line(aes(color = year_segment), linewidth = 2.5) +
  geom_point() +
  ylab("Cavalli per litro") +
  labs(color = "Periodo temporale", x = "") +
  scale_color_manual(values = c("< 1980"="#f8766d", "1980-2001"="#b79f00", "2001-2011"="#00ba38", "2011-2015"="#00bfc4", "2015-2023"="#619cff", "2015-2023"= "#f564e3")) +
  theme_minimal()







newData <- data.frame(time = c(1980, 2001, 2011, 2015, 2023),
                      cv = c(158, 184, 218, 249, 285),
                      cc = c(3019, 2582, 2642, 2555, 2508))

cilindrataNelTempo <- ggplot(newData, aes(x = time)) +
  geom_line(aes(y = cc), color = "blue", linewidth = 2) +
  ggtitle("Cilindrata") +
  theme_minimal()

potenzaNelTempo <- ggplot(newData, aes(x = time)) +
  geom_line(aes(y = cv), color = "red", linewidth = 2) +
  scale_y_continuous(breaks = seq(min(newData$cv), max(newData$cv), by = 20)) +
  ggtitle("Cavalli") +
  theme_minimal()







# Creazione dataframe per rapporto cilindrata-cavlli e periodo temporale con salto di 1 anno

frm_averages_smallTimeGap <- CarDatasetClean %>%
  group_by(start_year_production) %>%
  mutate(year_segment = case_when(
    start_year_production < 1993 ~ "No Euro, < 1993",
    start_year_production >= 1993 & start_year_production < 1997 ~ "Euro 1, 1993-1997",
    start_year_production >= 1997 & start_year_production < 2001 ~ "Euro 2, 1997-2001",
    start_year_production >= 2001 & start_year_production < 2006 ~ "Euro 3, 2001-2006",
    start_year_production >= 2006 & start_year_production < 2009 ~ "Euro 4, 2006-2009",
    start_year_production >= 2009 & start_year_production < 2015 ~ "Euro 5, 2009-2015",
    start_year_production >= 2015 ~ "Euro 6, 2015-2023",
    TRUE ~ NA_character_
  )) %>%
  summarize(mean_horsepower_per_liter = mean(horsepower_per_liter, na.rm = TRUE),
            mean_horsepower = mean(Horsepower, na.rm = TRUE),
            mean_displacement = mean(displacement, na.rm = TRUE),
            mean_combined = mean(combined, na.rm = TRUE),
            norma_Euro = year_segment) %>%
  filter(start_year_production >= 1980) %>%
  select(start_year_production, mean_horsepower_per_liter, mean_horsepower, mean_displacement, mean_combined, norma_Euro)


frm_averages_smallTimeGap$norma_Euro <- factor(frm_averages_smallTimeGap$norma_Euro)

plotline_averages_smallTimeGap <- ggplot(data = frm_averages_smallTimeGap, aes(x = start_year_production, y = mean_horsepower_per_liter, group = 1)) +
  geom_line(aes(color = norma_Euro), linewidth = 2.5) +
  geom_point() +
  ylab("Cavalli per litro") +
  labs(color = "Norma Euro", x = "") +
  scale_x_continuous(breaks = c(1980, 1993, 1997, 2001, 2006, 2009, 2015, 2023)) +
  theme_minimal() +
  scale_color_manual(values = c("No Euro, < 1993"="#f8766d", 
                                "Euro 1, 1993-1997"="#b79f00", 
                                "Euro 2, 1997-2001"="#00ba38", 
                                "Euro 3, 2001-2006"="#00bfc4", 
                                "Euro 4, 2006-2009"="#619cff", 
                                "Euro 5, 2009-2015"="#f564e3", 
                                "Euro 6, 2015-2023"="#9361e3"))






# Media dei cavalli per litro per ciascun segmento
frm_segment <- CarDatasetClean %>%
  filter(segment != "Roadster & Convertible") %>%
  mutate(segment = ifelse(segment %in% c("Compact", "Compact SUV"), "Compact", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Medium", "Medium MPV"), "Medium", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Coupe", "Coupe Cabrio", "Premium Coupe"), "Coupe", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Small", "Small MPV"), "Small", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Entry Premium", "Lower Premium", "Medium Premium", "Upper Premium"), "Premium", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Large", "Large MPV"), "Large", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Large SUV", "Medium SUV", "Small SUV", "Premium SUV"), "SUV", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Fullsize Pickup", "Heavy Duty Pickup", "Midsize Pickup", "Small Pickup"), "Pickup", segment)) %>%
  mutate(segment = ifelse(segment %in% c("Small", "Mini"), "Small", segment)) %>%
  group_by(segment) %>% 
  summarize(
    horsepower_per_liter = mean(horsepower_per_liter, na.rm = TRUE),
    consumption = mean(combined, na.rm = TRUE),
    displacement = mean(displacement, na.rm = TRUE)
  )

# Trova il segmento con il valore più elevato di horsepower_per_liter
segment_max <- frm_segment %>%
  filter(horsepower_per_liter == max(horsepower_per_liter)) %>%
  pull(segment)


# Trova il segmento con il valore più basso di horsepower_per_liter
segment_min <- frm_segment %>%
  filter(horsepower_per_liter == min(horsepower_per_liter)) %>%
  pull(segment)


# Aggiungi una variabile per identificare i segmenti massimi e minimi
frm_segment <- frm_segment %>%
  mutate(segment_color = ifelse(segment %in% c(segment_max, segment_min), "Extreme", "Normal"))


# Crea il grafico
segments_plot <- ggplot(data = frm_segment, aes(x = segment, y = horsepower_per_liter, fill = segment_color)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("Normal" = "#7EDFDC", "Extreme" = "#DE8F8A")) +  # Imposta i colori
  labs(x = "Segmento dell'auto", y = "Cavalli per litro (HP/L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none") +
  geom_text(aes(label = ifelse(segment %in% c(segment_max, segment_min), round(horsepower_per_liter, 2), "")),   
            vjust = -0.5, size = 4)  # Aggiungi l'etichetta con il valore massimo






# Trova il segmento con il valore più elevato di consumption
segment_max_consumption <- frm_segment %>%
  filter(consumption == max(consumption)) %>%
  pull(segment)

# Trova il segmento con il valore più basso di consumption
segment_min_consumption <- frm_segment %>%
  filter(consumption == min(consumption)) %>%
  pull(segment)

# Aggiungi una variabile per identificare i segmenti massimi e minimi di consumption
frm_segment <- frm_segment %>%
  mutate(segment_color = ifelse(segment %in% c(segment_max_consumption, segment_min_consumption), "Extreme", "Normal"))

# Crea il grafico basato su consumption
consumption_plot <- ggplot(data = frm_segment, aes(x = segment, y = consumption, fill = segment_color)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("Normal" = "#7EDFDC", "Extreme" = "#DE8F8A")) +
  labs(x = "Segmento dell'auto", y = "Consumo medio ( km/l )") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none") +
  geom_text(aes(label = ifelse(segment %in% c(segment_max_consumption, segment_min_consumption), round(consumption, 2), "")),
            vjust = -0.5, size = 4)




# Grafico dispersione
HpL_Consumption_dispersionPlot <- ggplot(data = frm_segment, aes(x = horsepower_per_liter, y = consumption, color = segment, label = segment, size = displacement)) +
  geom_point(show.legend = FALSE) +
  geom_text(aes(label = segment), vjust = -1.5, size = 4, show.legend = FALSE) +
  xlab("Cavalli per litro (HP/L)") +
  ylab("Consumo (km/l)") +
  scale_color_discrete(name = "Segmento") + 
  scale_size_continuous(range = c(3, 10), name = "Displacement") + 
  coord_cartesian(ylim = c(5, 20)) +
  theme(legend.position = "none") +
  theme_minimal()













