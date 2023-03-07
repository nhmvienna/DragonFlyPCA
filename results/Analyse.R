library(readxl)
library(tidyverse)
library(viridis)
library(MASS)

DATA <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Statistik eDNA_Lobau.xlsx")

colnames(DATA)

Plot <- ggplot(DATA, aes(y = eDNA, x = water_volume_ml, col = Site_code)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE
    ) +
    theme_bw()

Plot

test <- glm(eDNA ~ water_volume_ml * Site_code * Filter_Type,
    family = "binomial",
    data = DATA
)

anova(test, test = "Chisq")


### see here: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/



full.model <- lm(eDNA ~ (.)^ncol(DATA) - 1, data = DATA)

step.model <- stepAIC(full.model,
    direction = "both",
    trace = FALSE
)
anova(step.model)
