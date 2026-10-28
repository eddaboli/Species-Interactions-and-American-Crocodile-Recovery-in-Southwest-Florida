########################################
#   Wildlife in nesting areas
#   By: Edison D. Bonilla-Liberato
#    Date: 4/16/2025
######################################

rm(list = ls())
dev.off()

# Libraries----
library(tidyverse)
library(lubridate)
library(rgbif)
library(fs)
library(rredlist)
library(scales)
library(patchwork)
library(writexl)
library(janitor)
library(cowplot)

# Creating folders----
dir()
dir_create("Tables")
dir_create("Figures")
# Calling and formatting data----
df <- read.csv("General.csv")
head(df)
View(df)
# Looking taxonomic information----
str(df$Specie2) 
df <- df %>%
  mutate(Specie2 = as.character(Specie2))
names(df)
str(df$Specie2)

df_long <- df %>%
  pivot_longer(cols = c("Specie1", "Specie2"), names_to = "SpecieColumn", values_to = "Species") %>%
  filter(!is.na(Species))
table(df_long$SpecieColumn)
species_all <- df_long %>%
  distinct(Species) %>%
  pull()
tax_matches <- map_dfr(species_all, ~ name_backbone(name = .x) %>% as_tibble())
tax_info <- tax_matches %>%
  select(verbatim_name, kingdom, phylum, class, order, family, genus, species) %>%
  distinct() %>%
  mutate(
    order = if_else(
      is.na(order) & class %in% c("Crocodylia", "Squamata", "Testudines"),
      class,
      order
    ),
    class = if_else(
      class %in% c("Crocodylia", "Squamata", "Testudines"),
      "Reptilia",
      class
    )
  ) %>%
  rename(MatchedName = species)


species_to_check <- tax_info$MatchedName[!is.na(tax_info$MatchedName)]

df_long <- left_join(df_long, tax_info, by = c("Species" = "verbatim_name"))
View(df_long)
# Dividing datetime to add Year and Month----
df_long <- df_long %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    Month = month(DateTime, label = TRUE),
    Year = year(DateTime),
    Mes = Month
  )
YEARS_KEEP  <- c(2011, 2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
MONTHS_KEEP <- c("Mar","Apr","May","Jun","Jul","Aug")

df_long <- df_long %>%
  filter(CameraID != "Personal",
         Species  != "Crocodylus acutus",
         Year %in% YEARS_KEEP,
         as.character(Month) %in% MONTHS_KEEP) %>%
  mutate(
    Month = factor(as.character(Month), levels = MONTHS_KEEP),
    Place = na_if(trimws(Place), "")
  ) %>%
  filter(!is.na(Place)) %>%
  droplevels()


# Defining richness per area and camera----
df_long <- df_long %>%
  filter(First_observation == TRUE, CameraID != "Personal", Species != "Crocodylus acutus")

species_first_obs <- df_long %>%
  filter(
    First_observation == TRUE,
    !is.na(Species),
    Species != "Crocodylus acutus",
    Species != "Muridae sp.",
    str_count(Species, "\\s") == 1  
  ) %>%
  group_by(Species) %>%
  summarise(FirstObsCount = n(), .groups = "drop") %>%
  mutate(
    Total = sum(FirstObsCount),
    Frequency = round((FirstObsCount / Total) * 100, 2)
  ) %>%
  arrange(desc(Frequency))

print(species_first_obs)
View(species_first_obs)
write_csv(species_first_obs, "Tables/first_observation_frequencies.csv")

#records per 100 trap-days---- 
df0 <- df_long

df_long <- df_long %>%
  filter(Year %in% c(2011, 2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) 

registros_por_Place <- df_long %>%
  group_by(Place) %>%
  summarise(Registros = n(), .groups = "drop")

noches_manual <- tibble(
  Place = c("Cursie Lake", "Marco"),
  Noches_activas = c(481, 306)  
)

camara_res_completo <- registros_por_Place %>%
  left_join(noches_manual, by = "Place") %>%
  mutate(Registros_100_noches = (Registros / Noches_activas) * 100)

View(camara_res_completo)

#camera and place results----
#Species by camera
species_by_camera <- df_long %>%
  filter(!is.na(Species)) %>%
  distinct(Place, CameraID, Species) %>%
  arrange(Place, CameraID, Species)

write_csv(species_by_camera, "Tables/list_species_by_camera.csv")

#Species per month
tabla_especies_mes <- df_long %>%
  filter(!is.na(Species)) %>%
  group_by(Place, Year, Month) %>%
  summarise(
    Especies = paste(sort(unique(Species)), collapse = "; "),
    n_especies = n_distinct(Species),
    Registros = n(),
    .groups = "drop"
  ) %>%
  arrange(Place, Year, Month)

View(tabla_especies_mes)
write_csv(tabla_especies_mes, "Tables/especies_por_mes_lista.csv")

#Presence matrix by month
niveles_mes <- levels(df_long$Month)  
matriz_presencia <- df_long %>%
  filter(!is.na(Species)) %>%
  distinct(Species, Month) %>%             
  mutate(presente = 1L) %>%
  pivot_wider(
    names_from = Month,
    values_from = presente,
    values_fill = 0
  ) %>%
  arrange(Species)

View(matriz_presencia)
write_csv(matriz_presencia, "Tables/matriz_presencia_Species_x_Month.csv")

#Richness by place
rich_place <- df_long %>%
  group_by(Place) %>%
  summarise(
    n_species = n_distinct(.data$Species),
    n_genus = n_distinct(.data$genus),
    n_orders = n_distinct(.data$order),
    n_families = n_distinct(.data$family),
    n_classes = n_distinct(.data$class),
  )
write_csv(rich_place, "Tables/summary_richness_by_place.csv")

# Frequence by place
species_freq_place <- df_long %>%
  filter(
    First_observation == TRUE,
    !is.na(Species),
    Species != "Crocodylus acutus",
    Species != "Muridae sp.",
    str_count(Species, "\\s") == 1  
  )%>%
  group_by(Place, Species) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Place) %>%
  mutate(
    Total = sum(Count),
    Frequency = round((Count / Total) * 100, 2)
  ) %>%
  arrange(Place, desc(Frequency))

write_csv(species_freq_place, "Tables/species_frequency_by_place.csv")

#Richness by camera
rich_camera <- df_long %>%
  group_by(Place, CameraID) %>%
  summarise(
    n_species = n_distinct(.data$Species),
    n_genus = n_distinct(.data$genus),
    n_orders = n_distinct(order),
    n_families = n_distinct(family),
    n_classes = n_distinct(class)
  )
write_csv(rich_camera, "Tables/summary_richness_by_camera.csv")

#Monthly richness 
rich_monthly <- df_long %>%
  group_by(Place, CameraID, Month) %>%
  summarise(Richness = n_distinct(Species), .groups = "drop")

write_csv(rich_monthly, "Tables/richness_monthly_by_camera.csv")

#Summary by place
df_base <- df_long
df_long <- df_long %>%
  mutate(Place = na_if(trimws(Place), "")) %>%
  filter(!is.na(Place))

taxon_place <- df_long %>%
  group_by(Species, Place) %>%
  summarise(Observed = n() > 0, .groups = "drop") %>%
  pivot_wider(names_from = Place, values_from = Observed, values_fill = FALSE)

taxon_summary <- df_long %>%
  select(class,order,family, genus, Species) %>%
  distinct()

taxon_final <- left_join(taxon_summary, taxon_place, by = "Species")

write_csv(taxon_final, "Tables/Taxonomic_summary_by_place.csv")

species_by_place <- df_long %>%
  filter(!is.na(Species)) %>%
  distinct(Place, Species) %>%
  arrange(Place, Species)

write_csv(species_by_place, "Tables/list_species_by_place.csv")

df_long <- df_long %>%
  filter(Year %in% c(2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025))
rich_monthly <- df_long %>%
  filter(!(Month %in% c("Jan", "Feb", "Oct", "Nov", "Dec"))) %>%
  group_by(Place, Month) %>%
  summarise(Richness = n_distinct(Species), .groups = "drop")

# Graphs-----
#Figure 3
df <- read_csv("Tables/interactions.csv", locale = locale(encoding = "latin1"))

df_long <- df %>%
  pivot_longer(
    cols = -Specie,
    names_to = "Categoria",
    values_to = "Porcentaje"
  ) %>%
  mutate(
    Minutos = coalesce(Porcentaje, 0),
    Porcentaje = pmin(pmax(Porcentaje, 0), 100),
    Categoria = str_replace_all(Categoria, "_", " ")
  )

orden_especies <- df_long %>%
  group_by(Specie) %>%
  summarise(Total = sum(Porcentaje), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(Specie)

df_long <- df_long %>%
  mutate(Specie = factor(Specie, levels = orden_especies))

ggplot(df_long, aes(x = Porcentaje, y = Specie, fill = Categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "Feeding/foraging" = "#1874CD",
      "Resting"         = "#98F5FF",
      "Nesting"         = "#CAE1FF",
      "Nest predation"  = "#EE3B3B",
      "Nest inspection" = "#FFD700",
      "Nest disturbance" = "violet",
      "Transiting"      = "#9ACD32"
    )
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1),    
    limits = c(0, 25),
    breaks = seq(0, 100, by = 3),
    expand = c(0, 0)
  ) +
  labs(
    x = "Representativity of nest interactions",
    y = "Specie",
    fill = "Interaction"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", colour = "black"),
    axis.text.y = element_text(face = "italic")
  )
ggsave(filename = "Figures/Figure 3.jpg", plot = last_plot(), width = 1500, height = 2000, 
       units = "px", dpi =250)

ggsave(filename = "Figures/Figure 3-poster.jpg", plot = last_plot(), width = 2300, height = 2500, 
       units = "px", dpi = 350)
#FIGURE 5
class_map <- c("Aves" = "Birds",
               "Mammalia" = "Mammals",
               "Reptilia" = "Reptiles")

month_levels <- c("Mar","Apr","May","Jun","Jul","Aug")

counts_taxa_month <- df_base %>%
  filter(Year %in% c(2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
         Month %in% month_levels,
         First_observation == TRUE,
         !is.na(Species), !is.na(class), !is.na(Month)) %>%
  mutate(
    Class = dplyr::recode(class, !!!class_map, .default = NA_character_),
    Month = factor(Month, levels = month_levels)
  ) %>%
  filter(Class %in% c("Birds", "Mammals", "Reptiles")) %>%
  count(Month, Class, name = "Count") %>%
  tidyr::complete(Month, Class, fill = list(Count = 0))

col_class <- c("Birds" = "#1f77b4",
               "Mammals" = "#ff7f0e",
               "Reptiles" = "#2ca02c")

A <- ggplot(counts_taxa_month, aes(x = Month, y = Count, fill = Class)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = col_class, name = "Class") +
  labs(title= "(a)",x = "Month", y = "Number of records") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
A
#fIGURE 5B
class_map  <- c("Aves"="Birds","Mammalia"="Mammals","Reptilia"="Reptiles")
col_class  <- c("Birds"="#1f77b4","Mammals"="#ff7f0e","Reptiles"="#2ca02c")
place_labs <- c("Cursie Lake"="McIlvane Marsh","Marco"="Marco Airport")
month_levels <- c("Mar","Apr","May","Jun","Jul","Aug")

spec_by_place_month_class <- df_base %>%
  filter(Year %in% c(2017,2019,2020,2021,2022,2023,2024,2025),
         Month %in% month_levels,
         !is.na(Species), !is.na(class), !is.na(Month), !is.na(Place)) %>%
  mutate(
    Class = dplyr::recode(class, !!!class_map, .default = NA_character_),
    Month = factor(Month, levels = month_levels)
  ) %>%
  filter(Class %in% c("Birds","Mammals","Reptiles")) %>%
  group_by(Place, Month, Class) %>%
  summarise(n_species = n_distinct(Species), .groups = "drop") %>%
  tidyr::complete(Place, Month, Class, fill = list(n_species = 0))

B <- ggplot(spec_by_place_month_class,
                 aes(x = Month, y = n_species, fill = Class)) +
  geom_col(width = 0.65) +
  facet_wrap(~ Place, nrow = 1, labeller = labeller(Place = place_labs)) +
  scale_fill_manual(values = col_class, name = "Class") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title="(b)",x = "Month", y = "Number of species") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right", aspect.ratio = 1)

month_levels <- levels(df_base$Month)

df_interaction <- read.csv("Tables/Interaction_month.csv", check.names = FALSE)
months <- c("Mar","Apr","May","Jun","Jul","Aug")

dat_long <- df_interaction %>%
  pivot_longer(all_of(months), names_to = "Month", values_to = "Count") %>%
  mutate(
    Count = replace_na(as.numeric(Count), 0),
    Month = factor(Month, levels = months)
  )
dat_long <- dat_long %>%
  mutate(Area = factor(Area, levels = c("McIlvane Marsh", "Marco Airport")))


dat_sum <- dat_long %>%
  group_by(Area, Month, Category) %>%
  summarise(Count = sum(Count), .groups = "drop")

col_inter <- c(
  "Feeding/foraging"="#56B4E9","Resting"="#E69F00","Nesting"="#009E73",
  "Nest predation"="#D55E00","Nest inspection"="#8B8B83",
  "Nest disturbance"="#104E8B","Transiting"="#9BCD9B"
)

C<- ggplot(dat_sum, aes(Month, Count, fill = Category)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Area, nrow = 1) +
  scale_fill_manual(values = col_inter, name = "Interaction type") +
  labs(title="(c)",x = "Month", y = "Number of interaction records") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right", aspect.ratio = 1)

dat_sum_D <- dat_long %>%
  group_by(Group, Month, Category) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(Group,
                  Month = factor(Month, levels = levels(dat_long$Month)),
                  Category,
                  fill = list(Count = 0)) %>%
  mutate(
    Group = factor(Group, levels = c("Birds","Mammals","Reptiles"))
  )

D <- ggplot(dat_sum_D, aes(Month, Count, fill = Category)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Group, nrow = 1) +
  scale_fill_manual(values = col_inter, name = "Interaction type") +
  labs(title = "(d)", x = "Month", y = "Number of interaction records") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

design <- c(
  area(1, 1, 2),
  area(1, 2),
  area(2, 2),
  area(3, 1, 3, 2))

A + B + C + D + plot_layout(design = design)

ggsave("Figures/Figure_5.jpg",
       plot = last_plot(), width = 2500, height = 2000, 
       units = "px", dpi =250)

B
ggsave("Figures/Figure_POSTER.jpg",
       plot = last_plot(), width = 6600, height = 2600, units = "px", dpi = 500)
#####################
View(df_base)
month_levels <- levels(df_base$Month)

records_species_month_long <- df_base %>%
  filter(Year %in% c(2011, 2017, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
         !is.na(Species), !is.na(Month),
         First_observation == TRUE) %>%
  mutate(Month = factor(Month, levels = month_levels)) %>%
  group_by(Species, Month) %>%
  summarise(Records = n(), .groups = "drop") %>%
  arrange(Species, Month)

records_species_month_wide <- records_species_month_long %>%
  tidyr::pivot_wider(
    names_from  = Month,
    values_from = Records,
    values_fill = 0
  )

month_cols_in_table <- intersect(month_levels, names(records_species_month_wide))

records_species_month_wide <- records_species_month_wide %>%
  mutate(Total = rowSums(across(all_of(month_cols_in_table)))) %>%
  arrange(desc(Total))

readr::write_csv(records_species_month_wide,
                 "Tables/records_per_species_by_month_WIDE.csv")

month_levels <- levels(df0$Month)

records_species_month_long <- df0 %>%
  filter(!is.na(Species), !is.na(Month),
         First_observation == TRUE) %>%
  mutate(Month = factor(Month, levels = month_levels)) %>%
  group_by(Species, Month) %>%
  summarise(Records = n(), .groups = "drop") %>%
  arrange(Species, Month)

records_species_month_wide <- records_species_month_long %>%
  tidyr::pivot_wider(
    names_from  = Month,
    values_from = Records,
    values_fill = 0
  )

month_cols_in_table <- intersect(month_levels, names(records_species_month_wide))

records_species_month_wide <- records_species_month_wide %>%
  mutate(Total = rowSums(across(all_of(month_cols_in_table)))) %>%
  arrange(desc(Total))

readr::write_csv(records_species_month_wide,
                 "Tables/records_per_species_by_month_WIDE_2.csv")


