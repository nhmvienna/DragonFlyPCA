library("viridis")
library("MASS")
library(cowplot)
library("FactoMineR")
## For PCA's on steroids
library("tidyverse")
## to make ggplots
library("corrplot")
## to make nice correlation plots
library("gridExtra")
## to plot side by side
library("factoextra")
## to beautify the PCA plots
library("readxl")
## to read EXCEL worksheets
library(car)
install.packages("cAIC4")
library(cAIC4)

# install.packages("Matrix")
library(Matrix)
# install.packages("lme4")
library(lme4)

DATA <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Statistik eDNA_Lobau.xlsx")


# 1# Paarweise Korrelation
DATAcor_neu <- round(
    cor(DATA[, 4:15]), 2
)


pdf("/media/inter/mkapun/projects/DragonFlyPCA/results/DATAcor_neu.pdf",
    width = 10,
    height = 10
)



DATAcor_neu <- corrplot(DATAcor_neu,
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45
)

dev.off()


DATA$Site_code <- as.factor(DATA$Site_code)
DATA$Filternummer <- as.factor(DATA$Filternummer)
DATA$Art <- as.factor(DATA$Art)
DATA$Ordnung <- as.factor(DATA$Ordnung)


summary(DATA)



# 2# ANOVA (chemische Parameter + Site_code)
full.model <- glmer(
    eDNA ~ Leitfaehigkeit_mS + Wassertemperatur_C +
        pH_Wert + (1 | Site_code),
    data = DATA,
    family = "binomial"
)

Anova(full.model, type = 3)


# 3# ANOVA (Vegetation, Wasservolumen + Site_code)
full.model <- glmer(
    eDNA ~ Submerse_Vegetation + Emerse_Vegetation +
        water_volume_ml + (1 | Site_code),
    data = DATA,
    family = "binomial"
)

Anova(full.model, type = 3)




# 4# ANOVA (Exuvien + Site_code)
full.model <- glm(eDNA ~ Exuvien_Summe + Site_code,
    data = DATA,
    family = "binomial"
)

Anova(full.model, type = 3)
step.model <- stepAIC(full.model,
    direction = "both",
    trace = FALSE
)
Anova(step.model, type = 3)

# 5a# ggplot um verschiedene Zusammenhänge darzustellen

Plot <- ggplot(filter(DATA, Site_code != "L07"), aes(y = eDNA, x = water_volume_ml)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE
    ) +
    theme_bw()

Plot

# 5b#
Plot <- ggplot(DATA, aes(y = eDNA, x = Submerse_Vegetation)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE
    ) +
    theme_bw()

Plot

# 5c#
Plot <- ggplot(DATA, aes(y = eDNA, x = Exuvien_Summe)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE
    ) +
    theme_bw()

Plot

### now combine both datasets and plot :-)

DATA.lobau <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Statistik eDNA_Lobau.xlsx")
DATA.wienerwald <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Statistik eDNA_Wienerwald.xlsx")

head(DATA)
colnames(DATA.wienerwald)

NEW.lobau <- data.frame("eDNA" = DATA.lobau$eDNA)
NEW.lobau$Site <- rep("Lobau", nrow(NEW.lobau))
NEW.lobau$wasser_vol <- DATA.lobau$water_volume_ml
NEW.wienerwald <- data.frame("eDNA" = DATA.wienerwald$eDNA)
NEW.wienerwald$wasser_vol <- DATA.wienerwald$Water_Volume_ml
NEW.wienerwald$Site <- rep("Wienerwald", nrow(NEW.wienerwald))
NEW <- rbind(NEW.lobau, NEW.wienerwald)


Plot <- ggplot(NEW, aes(y = eDNA, x = wasser_vol, col = Site)) +
    geom_point(alpha = 0.2, size = 2) +
    geom_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE
    ) +
    theme_bw()

Plot
