## PCA analysis tutorial

## see also here: http://www.sthda.com/english/wiki/wiki.php?id_contents=7851 and here: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

install.packages("FactoMineR")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("factoextra")
install.packages("readxl")

## clear environment
rm(list = ls())

library("FactoMineR") ## For PCA's on steroids
library("tidyverse") ## to make ggplots
library("corrplot") ## to make nice correlation plots
library("gridExtra") ## to plot side by side
library("factoextra") ## to beautify the PCA plots
library("readxl") ## to read EXCEL worksheets

## set working directory
setwd("/media/inter/mkapun/projects/DragonFlyPCA/PCA_Tutorial")

## read Dataset which contains information on sampling ID, Country, Latitude, Longitude followed by 19 environmental variables from WorldClim (https://www.worldclim.org/)

DATA <- read.table("Dataset.txt",
  sep = "\t",
  header = T
)

DATA2 <- read_excel("/media/inter/mkapun/projects/DragonFlyPCA/data/Kopie von ENDBERICHT Statistik eDNA_Lobau_06.12.22_Martin.xlsx")
## We set the first column as the rownames so that the IDs can be later used for plotting

rownames(DATA) <- DATA[, 1]

## (1) At first, we need to test if the environmental variables (which are the last 19 columns in the Dataset) are intercorrelated. We therefore calculate pairwise correlations and round these to two decimals. Then we plot the correlation matrix.

DATA.cor <- round(
  cor(DATA[, 5:ncol(DATA)]),
  2
)

DATA2.noNA <- na.omit(DATA2[, 1:17])

DATA2.cor <- round(
  cor(DATA2.noNA[, 4:12]),
  2
)

pdf("DATA2_cor.pdf",
  width = 10,
  height = 10
)

DATA2.cor <- corrplot(DATA2.cor,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45
)
dev.off()

## indeed, several of the variables appear to be positively and negatively correlated. Thus, it does not make sense to analyze them separately.

## (2) We therefore perform a PCA to reduce the dimensionality of the dataset and summarize redundant variables

DATA.PCA <- PCA(DATA[, 5:ncol(DATA)],
  scale.unit = T,
  graph = F
)

DATA2.PCA <- PCA(DATA2.noNA[, 4:12],
  scale.unit = T,
  graph = F
)


# note, that I set scale.unit to True, this is necessary because the environmental variables are measured at different scales (°C, mm precipitation, etc.) and need to be normalized to avoid biased results. scale.unit=T standard normalizes the data so that mean=0 and SD=1

## (a) Test how much variance is explained by each of the PC axes.

DATA.PCA.eigenvalues <- DATA.PCA$eig
DATA2.PCA.eigenvalues <- DATA2.PCA$eig

## the second column contains information about the variance explained by each component and the third column contains information about the cummulative variance. We will plot the latter information to see how many PC-axes explain more than 80% of the total variance. As a rule of thumb, these PC-Axes are considered informative, and all the others are usually ignored

pdf("DATA.PCA.var.pdf",
  height = 10,
  width = 15
)

barplot(DATA.PCA.eigenvalues[, 3],
  names.arg = 1:nrow(DATA.PCA.eigenvalues),
  main = "Variances",
  xlab = "Principal Components",
  ylab = "Percentage of variances",
  col = "steelblue"
)

# Add connected line segments to the plot
lines(
  x = 1:nrow(DATA.PCA.eigenvalues),
  DATA.PCA.eigenvalues[, 3],
  type = "b",
  pch = 19,
  col = "red"
)
# Add horizontal line at 80%
abline(
  h = 80,
  col = "black",
  lty = 2,
  lwd = 2
)

dev.off()

pdf("DATA2.PCA.var.pdf",
  height = 10,
  width = 15
)

barplot(DATA2.PCA.eigenvalues[, 3],
  names.arg = 1:nrow(DATA2.PCA.eigenvalues),
  main = "Variances",
  xlab = "Principal Components",
  ylab = "Percentage of variances",
  col = "steelblue"
)

# Add connected line segments to the plot
lines(
  x = 1:nrow(DATA2.PCA.eigenvalues),
  DATA2.PCA.eigenvalues[, 3],
  type = "b",
  pch = 19,
  col = "red"
)
# Add horizontal line at 80%
abline(
  h = 80,
  col = "black",
  lty = 2,
  lwd = 2
)

dev.off()

### we now clearly see, that the fist three explain more than 80%. We will focus on those.

## (a) Plot the loadings for axes 1,2 and 3
DATA.PCA.load.1.2 <- fviz_pca_var(DATA.PCA,
  axes = c(1, 2),
  repel = T
) +
  theme_bw()
DATA.PCA.load.1.3 <- fviz_pca_var(DATA.PCA,
  axes = c(1, 3),
  repel = T
) +
  theme_bw()

ggsave("DATA.PCA.load.pdf",
  arrangeGrob(DATA.PCA.load.1.2,
    DATA.PCA.load.1.3,
    ncol = 2
  ),
  width = 15,
  height = 7
)

## Note, the direction of the arrows show whether the variable is correlated with Dim1 (along the X-axis) and Dim2 (along the Y-axis). This plot also contains information if this correlation is positive (right side for Dim1; upper side for Dim2/Dim3) or negative (left side for Dim1; lower side for Dim2/Dim3). The length of the arrow indicates the strength of the correlation (0% - 100%, i.e. 0-1).


## (c) Now we plot the PC-scores of Axes 1,2 and 3 to see how the samples differ with respect to these axes (equivalent to the environmental variables summarized by the axes as described above in 2.b)
DATA.PCA.ind.1.2 <- fviz_pca_ind(DATA.PCA,
  axes = c(1, 2),
  repel = T
) +
  theme_bw()

DATA.PCA.ind.2.3 <- fviz_pca_ind(DATA.PCA,
  axes = c(2, 3),
  repel = T
) +
  theme_bw()

ggsave("DATA.PCA.samples.pdf",
  ggarrange(DATA.PCA.ind.1.2,
    DATA.PCA.ind.2.3,
    nrow = 1
  ),
  width = 15,
  height = 7
)

## These plots indicate that Dim1 mostly separates southern (Spanish and Turkish) locations on the right from more central locations on the left. Dim2, in contrast, separates locations with oceanic (UK) at the top and continental climate at the bottom.

## (3) Finally, we append the three PC axes to the original dataset and export the table

DATA.new <- cbind(DATA, DATA.PCA$ind$coord[, 1:3])
write.table(DATA.new,
  "Dataset_new.txt",
  quote = F,
  row.names = F
)

### Done :-)

head(DATA2)

Species = "Sympetrum sanguineum"

DATA2.species <- DATA2 %>%
  filter(Art == Species)

DATA2$variable <- DATA2[["Water Volume (ml)"]]

Test <- glm(eDNA ~ variable,
  family = "binomial",
  data = DATA2
)

DATA2$vs <- predict(Test, DATA2, type = "response")

plot(DATA2$variable, DATA2$eDNA,
  col = rgb(0, 0, 1, 0.1),
  pch = 16
)
plot(DATA2$variable, DATA2$vs,
  col = rgb(0, 0, 1, 0.1),
  pch = 16
)
