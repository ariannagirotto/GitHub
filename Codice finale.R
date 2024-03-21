########### DIVORCE PREDICTION########### 
### Clavarino - Girotto - Gomez - Peri - Reyes - Rosa ###

###
library(readr)
library(corrplot) 
library(ggplot2)
library(DT)
library(caret)
library(nnet)
library(glmnet)
library(MASS)
library(tree)
library(dplyr)
###

divorce_data <- read.csv("divorce_data.csv", sep = ";")
datatable(divorce_data)
sum(divorce_data[,55])

############
divorce_data[, 31:54] <- lapply(divorce_data[, 31:54], function(x) {
  ifelse(x == 0, 4,
         ifelse(x == 1, 3,
                ifelse(x == 3, 1,
                       ifelse(x == 4, 0, x))))
})
############

# add 1 to all values in columns except the 55th column and 
#combine them to the 55th column to avoid division by zero.
divorce_data <- data.frame(divorce_data[,-55]+1, divorce_data[,55])
names(divorce_data)[55] <- "Divorce"

str(divorce_data)


##### EXPLORATORY ANALYSIS #####

### MISSING DATA CHECK
# are there any missing values?
sum(is.na(divorce_data))
# no

# All responses were collected on a 5 point scale 
# (1=Never, 2=Seldom, 3=Averagely, 4=Frequently, 5=Always).

# Checking for values other than these

valori_consentiti <- 1:5

for (col in names(divorce_data)) {
  valori_unici <- unique(divorce_data[[col]])
  valori_non_consentiti <- setdiff(valori_unici, valori_consentiti)
  
  if (length(valori_non_consentiti) > 0) {
    cat("Nel dataset ci sono valori non consentiti nella colonna", col, ":", valori_non_consentiti, "\n")
  }
}

# No values other than those expected


### DESCRIPTIVE STATISTICS

summary(training)

## BOXPLOT for each variable
ggplot(data = divorce_data, aes(x = factor(Divorce), y = Q31, fill = factor(Divorce))) +
  geom_boxplot(notch = TRUE, fill = c("#ffcb77", "#48cfae") , outlier.colour = "#fe6d73") +
  theme_minimal() +
  labs(x = "Divorce", fill = "Divorce") +
  theme(legend.title = element_blank())

# Creazione di una lista di y-values
y_values <- paste0("Q", 1:54)

# Loop per generare e salvare i boxplot per ciascun valore di y
for (y in y_values) {
  plot <- ggplot(data = divorce_data, aes(x = factor(Divorce), y = !!sym(y), fill = factor(Divorce))) +
    geom_boxplot(notch = TRUE, fill = c("#ffcb77", "#48cfae") , outlier.colour = "#fe6d73") +
    theme_minimal() +
    labs(x = "Divorce", y = y, fill = "Divorce") +
    theme(legend.title = element_blank())
  
  # Salvataggio del boxplot come file immagine
  ggsave(filename = paste0("boxplot_", y, ".png"), plot = plot)
}


## HISTOGRAM for each variable

ist_plot <- ggplot(data = divorce_data, aes(x = Q31, fill = factor(Divorce))) +
  geom_bar(width = 0.5) + 
  facet_wrap(~factor(Divorce)) +
  scale_fill_manual(values = c("#227c9d", "#fe6d73")) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.background = element_rect(fill = NA)) +  
  labs(title = "I can tell you what kind of stress my spouse is facing in her/his life.")

ist_plot
ggsave(filename = paste0("istogramma_Q24",".png"),plot=ist_plot)

### HISTOGRAM for each variable divided by group

# Split into divorced and non divorced sets

## Group 1: Friendship and Intimacy
gruppo1 <- divorce_data[,c(1:5, 8, 9, 55)]

divorziati_1 <- gruppo1%>%filter(gruppo1$Divorce == 0)
sposati_1 <- gruppo1%>%filter(gruppo1$Divorce == 1)


# Unisci i dati di tutte le variabili in un unico vettore
divor_1 <- c(divorziati_1$Q1, divorziati_1$Q2, divorziati_1$Q3, divorziati_1$Q4, 
             divorziati_1$Q5, divorziati_1$Q8, divorziati_1$Q9)
density_data_div_1 <- density(divor_1)

# Calcola il valore massimo della densità di kernel
max_density_div_1 <- max(density_data_div_1$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_1 * 2.1)  # Aumenta il limite superiore del 20% rispetto al massimo della densità


## Unisci i dati di tutte le variabili in un unico vettore
spos_1 <- c(sposati_1$Q1, sposati_1$Q2, sposati_1$Q3, sposati_1$Q4, 
            sposati_1$Q5, sposati_1$Q8, sposati_1$Q9)
density_data_spos_1 <- density(spos_1)

# Calcola il valore massimo della densità di kernel
max_density_spos_1 <- max(density_data_spos_1$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_1 * 1.2) 


# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_1, main = "FRIENDSHIP AND INTIMACY", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", ylim = ylimd)
lines(density_data_div_1, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_1, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", ylim = ylims)
lines(density_data_spos_1, col = "#111344", lwd = 2)



## Gruppo 2: Knowledge of the spouse
gruppo2 <- divorce_data[,c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 55)]

divorziati_2 <- gruppo2%>%filter(gruppo2$Divorce == 0)
sposati_2 <- gruppo2%>%filter(gruppo2$Divorce == 1)

divor_2 <- c(divorziati_2$Q21, divorziati_2$Q22, divorziati_2$Q23, divorziati_2$Q24, 
             divorziati_2$Q25, divorziati_2$Q26, divorziati_2$Q27, divorziati_2$Q28,
             divorziati_2$Q29, divorziati_2$Q30, divorziati_2$Q55)
density_data_div_2 <- density(divor_2)


# Calcola il valore massimo della densità di kernel
max_density_div_2 <- max(density_data_div_2$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_2 * 5)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore
spos_2 <- c(sposati_2$Q21, sposati_2$Q22, sposati_2$Q23, sposati_2$Q24, 
            sposati_2$Q25, sposati_2$Q26, sposati_2$Q27, sposati_2$Q28,
            sposati_2$Q29, sposati_2$Q30, sposati_2$Q55)
density_data_spos_2 <- density(spos_2)

# Calcola il valore massimo della densità di kernel
max_density_spos_2 <- max(density_data_spos_2$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_2 * 1.2) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_2, main = "KNOWLEDGE OF THE SPOUSE", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylimd)
lines(density_data_div_2, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_2, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylims)
lines(density_data_spos_2, col = "#111344", lwd = 2)



## Gruppo 3: indifferenza:
gruppo3 <- divorce_data[,c(6,7, 55)]

divorziati_3 <- gruppo3%>%filter(gruppo3$Divorce == 0)
sposati_3 <- gruppo3%>%filter(gruppo3$Divorce == 1)

divor_3 <- c(divorziati_3$Q6, divorziati_3$Q7, divorziati_3$Q55)
density_data_div_3 <- density(divor_3)


# Calcola il valore massimo della densità di kernel
max_density_div_3 <- max(density_data_div_3$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_3 * 3)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore
spos_3 <- c(sposati_3$Q6, sposati_3$Q7, sposati_3$Q55)
density_data_spos_3 <- density(spos_3)

# Calcola il valore massimo della densità di kernel
max_density_spos_3 <- max(density_data_spos_3$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_3 * 1.2) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_3, main = "INDIFFERENCE", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylimd)
lines(density_data_div_3, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_3, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylims)
lines(density_data_spos_3, col = "#111344", lwd = 2)


## Gruppo 4: Emotional intensity:
gruppo4 <- divorce_data[,c(31, 32, 33, 34, 35, 36, 37, 38, 55)]

divorziati_4 <- gruppo4%>%filter(gruppo4$Divorce == 0)
sposati_4 <- gruppo4%>%filter(gruppo4$Divorce == 1)

divor_4 <- c(divorziati_4$Q31, divorziati_4$Q32, divorziati_4$Q33,divorziati_4$Q34,
             divorziati_4$Q35, divorziati_4$Q36, divorziati_4$Q37, divorziati_4$Q38,
             divorziati_4$Q55)
density_data_div_4 <- density(divor_4)


# Calcola il valore massimo della densità di kernel
max_density_div_4 <- max(density_data_div_4$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_4 * 4)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore

spos_4 <- c(sposati_4$Q31, sposati_4$Q32, sposati_4$Q33, sposati_4$Q34,
            sposati_4$Q35, sposati_4$Q36, sposati_4$Q37, sposati_4$Q38,
            sposati_4$Q55)
density_data_spos_4 <- density(spos_4)

# Calcola il valore massimo della densità di kernel
max_density_spos_4 <- max(density_data_spos_4$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_4 * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_4, main = "EMOTIONAL INTENSITY", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylimd)
lines(density_data_div_4, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_4, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylims)
lines(density_data_spos_4, col = "#111344", lwd = 2)

## Gruppo 5: Conflict Scales:
gruppo5 <- divorce_data[,c(39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55)]

divorziati_5 <- gruppo5%>%filter(gruppo5$Divorce == 0)
sposati_5 <- gruppo5%>%filter(gruppo5$Divorce == 1)

divor_5 <- c(divorziati_5$Q39, divorziati_5$Q40, divorziati_5$Q41,divorziati_5$Q42,
             divorziati_5$Q43, divorziati_5$Q44, divorziati_5$Q45, divorziati_5$Q46,
             divorziati_5$Q47, divorziati_5$Q48, divorziati_5$Q49, divorziati_5$Q50,
             divorziati_5$Q51, divorziati_5$Q52, divorziati_5$Q53, divorziati_5$Q54,
             divorziati_5$Q55)
density_data_div_5 <- density(divor_5)


# Calcola il valore massimo della densità di kernel
max_density_div_5 <- max(density_data_div_5$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_5 * 4)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore

spos_5 <-  c(sposati_5$Q39, sposati_5$Q40, sposati_5$Q41,sposati_5$Q42,
             sposati_5$Q43, sposati_5$Q44, sposati_5$Q45, sposati_5$Q46,
             sposati_5$Q47, sposati_5$Q48, sposati_5$Q49, sposati_5$Q50,
             sposati_5$Q51, sposati_5$Q52, sposati_5$Q53, sposati_5$Q54,
             sposati_5$Q55)
density_data_spos_5 <- density(spos_5)

# Calcola il valore massimo della densità di kernel
max_density_spos_5 <- max(density_data_spos_5$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_5 * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_5, main = "CONFLICT SCALES", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylimd)
lines(density_data_div_5, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_5, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylims)
lines(density_data_spos_5, col = "#111344", lwd = 2)

## Gruppo 6: Shared Meaning System:
gruppo6 <- divorce_data[,c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 55)]

divorziati_6 <- gruppo6%>%filter(gruppo6$Divorce == 0)
sposati_6 <- gruppo6%>%filter(gruppo6$Divorce == 1)

divor_6 <- c(divorziati_6$Q10, divorziati_6$Q11, divorziati_6$Q12,divorziati_6$Q13,
             divorziati_6$Q14, divorziati_6$Q15, divorziati_6$Q16, divorziati_6$Q17,
             divorziati_6$Q18, divorziati_6$Q19, divorziati_6$Q20, divorziati_6$Q55)
density_data_div_6 <- density(divor_6)


# Calcola il valore massimo della densità di kernel
max_density_div_6 <- max(density_data_div_6$y)

# Imposta i limiti dell'asse y dell'istogramma
ylimd <- c(0, max_density_div_6 * 4)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore

spos_6 <- c(sposati_6$Q10, sposati_6$Q11, sposati_6$Q12,sposati_6$Q13,
            sposati_6$Q14, sposati_6$Q15, sposati_6$Q16, sposati_6$Q17,
            sposati_6$Q18, sposati_6$Q19, sposati_6$Q20, sposati_6$Q55)
density_data_spos_6 <- density(spos_6)

# Calcola il valore massimo della densità di kernel
max_density_spos_6 <- max(density_data_spos_6$y)

# Imposta i limiti dell'asse y dell'istogramma
ylims <- c(0, max_density_spos_6 * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_6, main = "SHARED MEANING SYSTEM ", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylimd)
lines(density_data_div_6, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_6, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylims)
lines(density_data_spos_6, col = "#111344", lwd = 2)



### Split into training and test sets
set.seed(1234)
train_ind <- createDataPartition(divorce_data[,"Divorce"], p = 0.8, list = FALSE)
train <- divorce_data[train_ind, ]
test <- divorce_data[-train_ind, ]


##### TRAINING
### PCA for variable groups
# divide the variables into 5 homogeneous groups based on their meaning

#Training group
## Group 1: Friendship and Intimacy
gruppo1_train <- train[,c(1:5, 8, 9)]

## Group 2: Knowledge of the spouse
gruppo2_train <- train[,c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]

## Group 3: Indifference towards the spouse
gruppo3_train <- train[,c(6,7)]

## Group 4: Aggressiveness during discussions
gruppo4_train <- train[,c(31, 32, 33, 34, 35, 36, 37, 38)]

## Group 5: The Conflict Scales
gruppo5_train <- train[,c(39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54)]

## Group 6: The Shared Meaning System
gruppo6_train <- train[,c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)]

# Test Group
gruppo1_test <- test[,c(1:5, 8, 9)]
gruppo2_test <- test[,c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]
gruppo3_test <- test[,c(6,7)]
gruppo4_test <- test[,c(31, 32, 33, 34, 35, 36, 37, 38)]
gruppo5_test <- test[,c(39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54)]
gruppo6_test <- test[,c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)]


### PCA TRAINING

# PCA Group 1 training
pca.train1 <- princomp(gruppo1_train, cor = T)

summary(pca.train1)

biplot(pca.train1)
abline(h=0, v=0)

pca.train1$loadings
pca.train1$scores

#the first principal component is a composite index of friendship and intimacy
#high values of comp1 indicate friendship and intimacy within the couple
#it is evident from the biplot that married individuals have higher comp1 values

friend_int <- pca.train1$scores[,1]

# PCA Group 2 training
pca.train2 <- princomp(gruppo2_train, cor=T)

summary(pca.train2)

biplot(pca.train2)
abline(h=0, v=0)

pca.train2$loadings
pca.train2$scores

#the first principal component is a composite index of knowledge
#high values of comp1 indicate greater knowledge of the partner
#it is evident from the biplot that married individuals have higher comp1 values

knowledge <- pca.train2$scores[,1]

# PCA Group 3 training
pca.train3 <- princomp(gruppo3_train, cor=T)

summary(pca.train3)

biplot(pca.train3)
abline(h=0, v=0)

pca.train3$loadings
pca.train3$scores

#the first principal component is a composite index of indifference
#high values of comp1 indicate indifference towards the partner
#it is evident from the biplot that married individuals have higher comp1 values


indifference <- pca.train3$scores[,1]


# PCA Group 4 training
pca.train4 <- princomp(gruppo4_train, cor=T)

summary(pca.train4)

biplot(pca.train4)
abline(h=0, v=0)

pca.train4$loadings
pca.train4$scores

#the first principal component is a composite index of aggressiveness during discussions
#high values of comp1 indicate poor ability in anger management during discussions
#it is evident from the biplot that divorced individuals have higher comp1 values

aggression <- pca.train4$scores[,1]


# PCA Group 5 training
pca.train5 <- princomp(gruppo5_train, cor=T)

summary(pca.train5)

biplot(pca.train5)
abline(h=0, v=0)

pca.train5$loadings
pca.train5$scores

#the first principal component is a composite index of discordant conflict management ability
#high values of comp1 indicate poor ability in conflict management
#it is evident from the biplot that divorced individuals have higher comp1 values

conflict_manag <- pca.train5$scores[,1]


# PCA Group 6 training
pca.train6 <- princomp(gruppo6_train, cor=T)

summary(pca.train6)

biplot(pca.train6)
abline(h=0, v=0)

pca.train6$loadings
pca.train6$scores

#the first principal component is a composite index of agreement on shared goals
#high values of comp1 indicate a high level of shared values
#it is evident from the biplot that married individuals have higher comp1 values

shared_goals <- pca.train6$scores[,1]

#training data frame
train.df <- data.frame(friend_int, 
                       knowledge, 
                       indifference, 
                       aggression, 
                       conflict_manag,
                       shared_goals,
                       train[,55])

### PCA TEST
pca.test1 <- predict(pca.train1, newdata = gruppo1_test)
pca.test2 <- predict(pca.train2, newdata = gruppo2_test)
pca.test3 <- predict(pca.train3, newdata = gruppo3_test)
pca.test4 <- predict(pca.train4, newdata = gruppo4_test)
pca.test5 <- predict(pca.train5, newdata = gruppo5_test)
pca.test6 <- predict(pca.train6, newdata = gruppo6_test)

friend_int_test <- pca.test1[,1]
knowledge_test <- pca.test2[,1]
indifference_test <- pca.test3[,1]
aggression_test <- pca.test4[,1]
conflict_manag_test <- pca.test5[,1]
shared_goals_test <- pca.test6[,1]


test.df <- data.frame(friend_int_test, 
                      knowledge_test, 
                      indifference_test, 
                      aggression_test, 
                      conflict_manag_test,
                      shared_goals_test,
                      test[,55])

colnames(train.df)[7] <- "Divorce"
colnames(test.df)[7] <- "Divorce"


# reasonably balanced both in training and test
round(prop.table(table(train.df$Divorce)), 2)
round(prop.table(table(test.df$Divorce)), 2)


#### TRAINING

## CORRELATION ANALYSIS AND MARGINAL DISTRIBUTIONS
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(train.df), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         diag=FALSE 
)

cor(train.df)
# too high correlations!



### tradurre in inglese la parte di istogrammi divorced e non divorced


########Copia:
### Ridge regression Arianna
### Logistic Regression Arianna
### radom foste gruppi, buon risultato Arianna

## PCA Tot Sara



