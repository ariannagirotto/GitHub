
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

plot <- ggplot(data = divorce_data, aes(x = Q1, fill = factor(Divorce))) +
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
  labs(title = "If one of us apologizes when our discussion deteriorates, the discussion ends.")

plot








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





