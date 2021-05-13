library("dplyr")
library("ggplot2")
library("tidyr")


data <- read.csv("../data/work-life/mental-heath-in-tech-2014.csv")

data <- data %>%
  drop_na(work_interfere, seek_help)

heatmap_data <- data %>%
  select(work_interfere, seek_help)



treatment_table <- table(heatmap_data)

treatment_df <- data.frame(treatment_table)

treatment_df$work_interfere <- factor(treatment_df$work_interfere,
                                      levels = c("Never", "Rarely", "Sometimes",
                                                 "Often"))



work_help_heatmap <- ggplot(data = treatment_df) +
  geom_tile(mapping = aes(x = seek_help, y = work_interfere, fill = Freq),
            show.legend = TRUE) +
  scale_fill_gradient2("number of employees", low = "white",
                       mid = "light blue", high = "dark blue")

sometimes_count <- nrow(heatmap_data[heatmap_data$work_interfere ==
                                       "Sometimes",])

seek_help_count <- nrow(heatmap_data[heatmap_data$seek_help == "No",])

total_respondents <- nrow(heatmap_data)

