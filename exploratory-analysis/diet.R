library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

fat_data <- read.csv("data/individual/Fat_Supply_Quantity_Data.csv")
kcal_data <- read.csv("data/individual/Food_Supply_kcal_Data.csv")
kg_data <- read.csv("data/individual/Food_Supply_Quantity_kg_Data.csv")
protein_data <- read.csv("data/individual/Protein_Supply_Quantity_Data.csv")
ideal_diet_data <- read.csv("data/individual/ideal.csv")

obesity_data <- kg_data %>%
  select(Country, Obesity)

top_5_obes <- obesity_data %>%
  top_n(5, Obesity) %>%
  arrange(desc(Obesity)) %>%
  mutate(Rank = order(desc(Obesity))) %>%
  mutate(Obesity = paste0(Obesity, "%", sep = ""))

# ----------------------------------------------------------------------
ideal_diet <- ideal_diet_data %>%
  select(Country, Grains, Vegetables, Fruits, Protein) %>%
  filter(Country == "Kiribati" | Country == "Samoa" |
    Country == "United States of America" | Country == "Kuwait" |
    Country == "Saudi Arabia") %>%
  pivot_longer(cols = Grains:Protein)

ideal_diet_plot <-
  ggplot(ideal_diet, aes(fill = name, y = value, x = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Recommended Simple Daily Dietary Intake") +
  labs(
    x = "High Obesity Countries",
    y = "Percentage of Total Food Intake"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(fill = "Daily Diet Intake") +
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC79A7", "#56B4E9"))

# ----------------------------------------------------------------------
kg <- kg_data %>%
  mutate(Vegetable = Vegetables + Starchy.Roots) %>%
  mutate(Protein = Eggs + Fish..Seafood + Meat + Pulses + Treenuts) %>%
  rename(
    Grains = Cereals...Excluding.Beer,
    Fruits = Fruits...Excluding.Wine
  ) %>%
  select(Country, Grains, Vegetable, Fruits, Protein)

obes_kg <- kg %>%
  filter(Country == "Kiribati" | Country == "Samoa" |
    Country == "United States of America" | Country == "Kuwait" |
    Country == "Saudi Arabia")

obes_kg_data <- obes_kg %>%
  pivot_longer(cols = Grains:Protein)

obes_kg_plot <- ggplot(obes_kg_data, aes(fill = name, y = value, x = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Diet Intake Quantity in High Obesity Countries") +
  labs(
    x = "High Obesity Countries",
    y = "Percentage of Total Food Intake Quantity (kg)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(fill = "Daily Diet Intake") +
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC79A7", "#56B4E9"))

# ----------------------------------------------------------------------
fat <- fat_data %>%
  mutate(Vegetable = Vegetables + Starchy.Roots) %>%
  mutate(Protein = Eggs + Fish..Seafood + Meat + Pulses + Treenuts) %>%
  rename(
    Grains = Cereals...Excluding.Beer,
    Fruits = Fruits...Excluding.Wine
  ) %>%
  select(Country, Grains, Vegetable, Fruits, Protein)

obes_fat <- fat %>%
  filter(Country == "Kiribati" | Country == "Samoa" |
    Country == "United States of America" | Country == "Kuwait" |
    Country == "Saudi Arabia")

obes_fat_data <- obes_fat %>%
  pivot_longer(cols = Grains:Protein)

obes_fat_plot <-
  ggplot(obes_fat_data, aes(fill = name, y = value, x = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Fat Intake Quantity in High Obesity Countries") +
  labs(
    x = "High Obesity Countries",
    y = "Percentage of Total Fat Intake Quantity"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(fill = "Daily Diet Intake") +
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC79A7", "#56B4E9"))


# ----------------------------------------------------------------------
kcal <- kcal_data %>%
  mutate(Vegetable = Vegetables + Starchy.Roots) %>%
  mutate(Protein = Eggs + Fish..Seafood + Meat + Pulses + Treenuts) %>%
  rename(
    Grains = Cereals...Excluding.Beer,
    Fruits = Fruits...Excluding.Wine
  ) %>%
  select(Country, Grains, Vegetable, Fruits, Protein)

obes_kcal <- kcal %>%
  filter(Country == "Kiribati" | Country == "Samoa" |
    Country == "United States of America" | Country == "Kuwait" |
    Country == "Saudi Arabia")

obes_kcal_data <- obes_kcal %>%
  pivot_longer(cols = Grains:Protein)

obes_kcal_plot <-
  ggplot(obes_kcal_data, aes(fill = name, y = value, x = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Energy (kcal) Intake Quantity in High Obesity Countries") +
  labs(
    x = "High Obesity Countries",
    y = "Percentage of Total Energy Intake (kcal)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(fill = "Daily Diet Intake") +
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC79A7", "#56B4E9"))


# ----------------------------------------------------------------------
protein <- protein_data %>%
  mutate(Vegetable = Vegetables + Starchy.Roots) %>%
  mutate(Protein = Eggs + Fish..Seafood + Meat + Pulses + Treenuts) %>%
  rename(
    Grains = Cereals...Excluding.Beer,
    Fruits = Fruits...Excluding.Wine
  ) %>%
  select(Country, Grains, Vegetable, Fruits, Protein)

obes_protein <- protein %>%
  filter(Country == "Kiribati" | Country == "Samoa" |
    Country == "United States of America" | Country == "Kuwait" |
    Country == "Saudi Arabia")

obes_protein_data <- obes_protein %>%
  pivot_longer(cols = Grains:Protein) %>%
  rename(Diet = name, Intake = value)

obes_protein_plot <-
  ggplot(obes_protein_data, aes(fill = Diet, y = Intake, x = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Protein Intake Quantity in High Obesity Countries") +
  labs(x = "High Obesity Countries", y = "Percentage of Total Protein Intake") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(fill = "Daily Diet Intake") +
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC79A7", "#56B4E9"))
