
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
summary(training)


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

melted_correlation <- reshape2::melt(matrice_correlazione)
melted_correlation$value <- melted_correlation$value

# Crea un heatmap della matrice di correlazione
ggplot(data = loadings_1, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label=round(value, 2)), size=1.5) +
  scale_fill_gradient(low = "lemonchiffon", high = "orangered4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap della Componenti Principali",
       x = "Variabile",
       y = "Variabile")
       


