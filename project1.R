getwd()
setwd("C:/Users/ASUS/Desktop/ml-g6/ST -4052/Data Analysis project 1")

####### Packages ###############
#install.packages("FactoMineR")
#install.packages("factoextra")

####### Libraries ###############

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
#library(GGally)
library(readr)
#library(ggjoy)
#library(ggpubr)
library(corrplot)
library(cowplot)
library(glue)
library(car)
library(gplots)
library(cluster)
library(factoextra)
library(MASS)

####### Loading the training set ###############

data = read.csv("C:/Users/ASUS/Desktop/ml-g6/ST -4052/Data Analysis project 1/train_set_no_encode.csv")
head(data)
str(data)
data$Cloud_Cover <- as.factor(data$Cloud_Cover)
data$Season <- as.factor(data$Season)
data$Location <- as.factor(data$Location)
data$Weather_Type <- as.factor(data$Weather_Type)

str(data)

X = data[, -dim(data)[2]]
y = data[, dim(data)[2]]

res.famd <- FAMD(X, graph = F)
print(res.famd)


eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)

#decision boundries 
factor_scores <- res.famd$ind$coord
factor_scores <- as.data.frame(factor_scores)

colnames(factor_scores)

ggplot(factor_scores, aes(x = Dim.1, y = Dim.2, color = as.factor(y))) +
  geom_point() +
  labs(title = "Factor Scores Plot", x = "Dimension 1", y = "Dimension 2", color = "Class") +
  theme_minimal()


lda_model <- lda(as.factor(y) ~ Dim.1 + Dim.2, data = factor_scores)

xrange <- range(factor_scores$Dim.1)
yrange <- range(factor_scores$Dim.2)
grid <- expand.grid(Dim.1 = seq(xrange[1], xrange[2], length = 100),
                    Dim.2 = seq(yrange[1], yrange[2], length = 100))

grid$pred <- predict(lda_model, newdata = grid)$class

ggplot(factor_scores, aes(x = Dim.1, y = Dim.2, color = as.factor(y))) +
  geom_point() +
  geom_contour(data = grid, aes(z = as.numeric(pred)), color = "black") +
  labs(title = "Factor Scores with Decision Boundaries", x = "Dimension 1", y = "Dimension 2", color = "Class") +
  theme_minimal()

var <- get_famd_var(res.famd)
var

# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the dimensions
head(var$contrib)

# Plot of variables
fviz_famd_var(res.famd,col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var


fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var",repel = TRUE)



quali.var <- get_famd_var(res.famd, "quali.var")
quali.var


fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


ind <- get_famd_ind(res.famd)
ind

fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


fviz_mfa_ind(res.famd,
             habillage = "Label", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)

fviz_mfa_ind(res.famd, geom="point", habillage = "Location")
fviz_mfa_ind(res.famd, geom="point", habillage = "Cloud_Cover")
fviz_mfa_ind(res.famd, geom="point", habillage = "Season")



####  checking clusters are avilable or not ######################
 
#fviz_nbclust(res.famd$ind$coord, kmeans, method = "wss") +
  #geom_vline(xintercept = 4, linetype = 2) +
  #labs(subtitle = "Elbow method")

fviz_nbclust(res.famd$ind$coord, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

set.seed(10)
res.km = kmeans(res.famd$ind$coord, centers=5, nstart=25, iter.max=50)

silhouette_scores <- silhouette(res.km$cluster, dist(res.famd$ind$coord))
avg_silhouette <- mean(silhouette_scores[, 3])
print(avg_silhouette)
fviz_silhouette(silhouette_scores)

fviz_mfa_ind(res.famd, habillage = as.factor(res.km$cluster), 
             palette = c("darkred", "indianred2", "salmon", "blue", "green"), 
             addEllipses = TRUE, repel = TRUE, geom = "point")

###### gvif ######
model <- glm(Weather_Type ~ ., data = data, family = binomial)
summary(model)     
vif_values <- vif(model)
print(as.data.frame(vif_values))
