library(readxl)
library(tidyverse)
library(viridis)
library(MASS)
library(cowplot)

DATA <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Statistik eDNA_Lobau.xlsx")

colnames(DATA)

Plot1 <- ggplot(DATA, aes(y = eDNA, x = Imagines)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = FALSE
    ) +
    theme_bw()

Plot1


Plot <- ggplot(DATA, aes(x = Imagines, y = Exuvien_Summe)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "lm",
        se = TRUE
    ) +
    theme_bw()

Plot

### Hier mittle ich die eDNA Probability über alle sample-dates zur Visualisierung!!

DATA.means <- DATA %>%
    group_by(Site_code, Art, Exuvien_Summe, Imagines) %>%
    summarise(mean.eDNA = mean(eDNA), N = n())


Plot2 <- ggplot(DATA.means, aes(x = Imagines, y = mean.eDNA, col = Art)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = FALSE
    ) +
    ylim(0, 1) +
    theme_bw()

Plot2

test <- glm(eDNA ~ water_volume_ml * Site_code * Filter_Type,
    family = "binomial",
    data = DATA
)

anova(test, test = "Chisq")


## Stepwise regression model for ALL factors and ALL interactions. PUUUUHHHH, this takes some time
### see here: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/


DATA$Site_code <- as.factor(DATA$Site_code)
DATA$Filternummer <- as.factor(DATA$Filternummer)
DATA$Art <- as.factor(DATA$Art)


summary(DATA)

##
full.model <- lmer(eDNA ~ water_volume_ml + Filter_Type + Exuvien_Summe + Imagines + (1 | Site_code), data = DATA)

Anova(full.model, type = 3)
step.model <- stepAIC(full.model,
    direction = "both",
    trace = FALSE
)
anova(step.model)
