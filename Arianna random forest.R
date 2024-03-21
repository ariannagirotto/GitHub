
###
library(readr)
library(corrplot) 
library(ggplot2)

###


divorce_data <- read.csv("divorce_data.csv", sep = ";")


########### FASE 1. ANALISI PRELIMINARE ############

### VERIFICA DELLA TIPOLOGIA DI VARIABILI
# devono essere tutte type factor, dato che le risposte al sondaggio possono
# essere solo: 0, 1, 2, 3, 4, 5
# anche la variabile risposta "Divorce" deve essere type factor

divorce_data <- data.frame(divorce_data[,-55]+1, divorce_data[,55])
names(divorce_data)[55] <- "Divorce"

divorce_data[, 31:54] <- lapply(divorce_data[, 31:54],
                                function(x) ifelse(x == 1, 5, ifelse(x == 2, 4, x)))
####scambio puntaggi da Q31 a Q54#####


str(divorce_data)

# tutte le variabili sono type int: le trasformo in type factor

# for (col in names(divorce_data)) {
#   if (is.numeric(divorce_data[[col]])) {
#     divorce_data[[col]] <- as.factor(divorce_data[[col]])
#   }
# }

# str(divorce_data)
# da fare dopo se no non si può calcolare la correlazione



### DIVIDIAMO IN TRAINING E TEST

train_size <- floor(0.80*nrow(divorce_data))
set.seed(12)

train_index <- sample(seq_len(nrow(divorce_data)), size = train_size)
training <- divorce_data[train_index, ]

test <- divorce_data[-train_index, ]


# salvo i dati originali perima di effettuare pulizia dati
training_original <- training
test_original <- test


# il problema è bilanciato?
# Vediamo se le proporzioni tra le due classi coincidono nei vari dataset
round(prop.table(table(training$Divorce)), 2)
round(prop.table(table(test$Divorce)), 2)
# bilanciato


############# FASE 2. ANALISI ESPLORATIVE ################

### VERIFICA MISSING 
# ci sono valori mancanti?
sum(is.na(divorce_data))
# no

# All responses were collected on a 5 point scale 
# (0=Never, 1=Seldom, 2=Averagely, 3=Frequently, 4=Always).


# controllo se ci sono valori diversi da questi

valori_consentiti <- 0:5

for (col in names(divorce_data)) {
  valori_unici <- unique(divorce_data[[col]])
  valori_non_consentiti <- setdiff(valori_unici, valori_consentiti)
  
  if (length(valori_non_consentiti) > 0) {
    cat("Nel dataset ci sono valori non consentiti nella colonna", col, ":", valori_non_consentiti, "\n")
  }
}

# nessun valore diverso da quelli previsti

##### -> TRAINING

### STATISTICHE DESCRITTIVE

## BOXPLOT PER OGNI DOMANDA
ggplot(data = divorce_data, aes(x = factor(Divorce), y = Q1, fill = factor(Divorce))) +
  geom_boxplot(notch = TRUE, fill = "#ffcb77", outlier.colour = "#fe6d73") +
  theme_minimal() +
  labs(x = "Divorce", fill = "Divorce") +
  theme(legend.title = element_blank())

# Creazione di una lista di y-values
y_values <- paste0("Q", 1:54)

# Loop per generare e salvare i boxplot per ciascun valore di y
for (y in y_values) {
  plot <- ggplot(data = divorce_data, aes(x = factor(Divorce), y = !!sym(y), fill = factor(Divorce))) +
    geom_boxplot(notch = TRUE, fill = "#ffcb77", outlier.colour = "#fe6d73") +
    theme_minimal() +
    labs(x = "Divorce", y = y, fill = "Divorce") +
    theme(legend.title = element_blank())
  
  # Salvataggio del boxplot come file immagine
  ggsave(filename = paste0("boxplot_", y, ".png"), plot = plot)
}


## ISTOGRAMMI PER OGNI DOMANDA

ist_plot <- ggplot(data = divorce_data, aes(x = Q24, fill = factor(Divorce))) +
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
ggsave(filename = paste0("istogramma_Q200",".png"),plot=ist_plot)


# dividi il dataset tra divorziati e non

library(dplyr)

## Gruppo 1: Friendship and Intimacy:
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
ylim <- c(0, max_density * 2.1)  # Aumenta il limite superiore del 20% rispetto al massimo della densità


## Unisci i dati di tutte le variabili in un unico vettore
spos_1 <- c(sposati_1$Q1, sposati_1$Q2, sposati_1$Q3, sposati_1$Q4, 
            sposati_1$Q5, sposati_1$Q8, sposati_1$Q9)
density_data_spos_1 <- density(spos_1)

# Calcola il valore massimo della densità di kernel
max_density_spos_1 <- max(density_data_spos_1$y)

# Imposta i limiti dell'asse y dell'istogramma
ylim <- c(0, max_density * 1.2) 


# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_1, main = "FRIENDSHIP AND INTIMACY", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", ylim = ylim)
lines(density_data_div_1, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_1, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", ylim = ylim)
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
ylim <- c(0, max_density * 5)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore
spos_2 <- c(sposati_2$Q21, sposati_2$Q22, sposati_2$Q23, sposati_2$Q24, 
            sposati_2$Q25, sposati_2$Q26, sposati_2$Q27, sposati_2$Q28,
            sposati_2$Q29, sposati_2$Q30, sposati_2$Q55)
density_data_spos_2 <- density(spos_2)

# Calcola il valore massimo della densità di kernel
max_density_spos_2 <- max(density_data_spos_2$y)

# Imposta i limiti dell'asse y dell'istogramma
ylim <- c(0, max_density * 1.2) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_2, main = "KNOWLEDGE OF THE SPOUSE", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_div_2, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_2, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylim)
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
ylim <- c(0, max_density * 3)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore
spos_3 <- c(sposati_3$Q6, sposati_3$Q7, sposati_3$Q55)
density_data_spos_3 <- density(spos_3)

# Calcola il valore massimo della densità di kernel
max_density_spos_3 <- max(density_data_spos_3$y)

# Imposta i limiti dell'asse y dell'istogramma
ylim <- c(0, max_density * 1.2) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_3, main = "INDIFFERENCE", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_div_3, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_3, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylim)
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
ylim <- c(0, max_density * 4)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore

spos_4 <- c(sposati_4$Q31, sposati_4$Q32, sposati_4$Q33, sposati_4$Q34,
           sposati_4$Q35, sposati_4$Q36, sposati_4$Q37, sposati_4$Q38,
           sposati_4$Q55)
density_data_spos_4 <- density(spos_4)

# Calcola il valore massimo della densità di kernel
max_density_spos_4 <- max(density_data_spos_4$y)

# Imposta i limiti dell'asse y dell'istogramma
ylim <- c(0, max_density * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_4, main = "EMOTIONAL INTENSITY", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_div_4, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_4, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylim)
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
ylim <- c(0, max_density * 4)  
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
ylim <- c(0, max_density * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_5, main = "CONFLICT SCALES", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_div_5, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_5, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylim)
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
ylim <- c(0, max_density * 4)  
xlim <- c(1, 5)
## Unisci i dati di tutte le variabili in un unico vettore

spos_6 <- c(sposati_6$Q10, sposati_6$Q11, sposati_6$Q12,sposati_6$Q13,
            sposati_6$Q14, sposati_6$Q15, sposati_6$Q16, sposati_6$Q17,
            sposati_6$Q18, sposati_6$Q19, sposati_6$Q20, sposati_6$Q55)
density_data_spos_6 <- density(spos_6)

# Calcola il valore massimo della densità di kernel
max_density_spos_6 <- max(density_data_spos_6$y)

# Imposta i limiti dell'asse y dell'istogramma
ylim <- c(0, max_density * 4) 
xlim <- c(1, 5)

# Imposta le dimensioni della finestra grafica
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 2))  # Imposta i margini della figura (bottom, left, top, right)

# Plot dell'istogramma per il gruppo divorziato
hist(divor_6, main = "SHARED MEANING SYSTEM ", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#c6302c", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_div_6, col = "#111344", lwd = 2)


# Plot dell'istogramma per il gruppo sposato
hist(spos_6, main = "", xlab = "Answer", ylab = "Density", freq = FALSE, 
     col = "#227c9d", border = "white", xlim = xlim, ylim = ylim)
lines(density_data_spos_6, col = "#111344", lwd = 2)

### ANALISI CORRELAZIONE

# da fare dopo la PCA


###### PCA ###### 

matrice_correlazione <- cor(divorce_data)
matrice_correlazione

library(ggplot2)
library(reshape2)
melted_correlation <- reshape2::melt(matrice_correlazione)
melted_correlation$value <- melted_correlation$value

# Crea un heatmap della matrice di correlazione
ggplot(data = melted_correlation, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lemonchiffon", high = "orangered3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap della Matrice di Correlazione",
       x = "Variabile",
       y = "Variabile")


acp <- princomp(training[,-55], cor=T)
summary(acp)
screeplot(acp)

plot(acp$scores)
text(acp$scores, rownames(nuovi_dati))
abline(h=0, v=0)

biplot(acp)


x <- prcomp(training, scale. = TRUE)
ses <- x$rotation
ses
options(max.print = 10000000)

loadings_1 <- reshape2::melt(ses)
loadings_1$value <- loadings_1$value
loadings_1

melted_correlation <- reshape2::melt(matrice_correlazione)
melted_correlation$value <- melted_correlation$value

# Crea un heatmap della matrice di correlazione
ggplot(data = loadings_1, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label=round(value, 2)), size=2) +
  scale_fill_gradient(low = "lemonchiffon", high = "orangered4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap della Componenti Principali",
       x = "Variabile",
       y = "Variabile")
       

## seleziono le prime 10 componenti nella plot
loadings_10PCA <- head(loadings_1, n = 550)
ggplot(data = loadings_10PCA, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label=round(value, 2)), size=2) +
  scale_fill_gradient(low = "lemonchiffon", high = "orangered4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap della Componenti Principali",
       x = "Variabile",
       y = "Variabile")

#valori loadings per prime 10 PC
ses_10PCA <- ses[,1:10]
ses_10PCA

## Eigenvalues
# number of row and num of column 
n <- nrow(divorce_data)
p <- ncol(divorce_data)


# mean and std
medie <- colMeans(divorce_data)
scarto <- apply(divorce_data, 2, sd)
descriptive<-round(cbind(medie, sigma),2)

## PCA stating from covariance matrix
# covariance:
sigma <- cov(divorce_data)
options(max.print=10000000000000000000000000000000000000000000000000)
sigma


# The goal is to calculate the eigenvalues and eigenvectors of the calculated covariance matrix:
eigen(sigma)
autoval <- eigen(sigma)$values
autovec <- eigen(sigma)$vectors

## PCA starting form correlation matrix
# correlation:
rho <- cor(divorce_data)

# The goal is to calculate the eigenvalues and eigenvectors of the calculated correlation matrix:
eigen(rho)
autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

# Analysis of the eigenvalues
pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
pvarsp

###PCA 10 gruppi

gruppo1 <- training[,c(1,3,4,11)]
gruppo2 <- training[,c(5,8,9)]
gruppo3 <- training[,c(6,7)]
gruppo4 <- training[,c(10,12:20)]
gruppo5 <- training[,21:30]
gruppo6 <- training[,31:41]
gruppo7 <- training[,39:40]
gruppo8 <- training[,42:47]
gruppo9 <- training[,48:51]
gruppo10 <- training[,52:54]





