library(tidyverse)
library(magrittr)
library(lavaan)
library(lavaanPlot)
library(mice)
library(PerformanceAnalytics)
library(psych)
library(apaTables)
library(knitr)
library(kableExtra)
library(lavaan)
library(lavaanPlot)
library(haven)
library(mvnmle)
library(Amelia)
library(olsrr)
library(jtools)
library(moments)
library(lmtest)


# Vorbereitung des Datensatzes -----------------
## Einlesen ----
load("C://Users//leonw//Downloads//datasetMerged.RData")
dataroh <- datasetMerged

##Datenaufbereitung----
#hierfür Reihenfolge in der BA beachten
#Aussortieren der Fälle mit zu vielen fehlenden Werten und Studienabbruch
### Studienabbruch
t4_interest_1 <- datasetMerged %>% filter(T4_Interest == 1)

dataset_cleaned <- datasetMerged %>% 
  filter(!(T4_Interest == 1 & 
             is.na(T5_Selfperception_V1) & is.na(T5_ImportanceValue_V1) & is.na(T5_Selfperception_V2) & 
             is.na(T5_PerceivedDifficulty) & is.na(T5_Surprise) & is.na(T5_Joy) & is.na(T5_Confusion) & 
             is.na(T5_Curiosity) & is.na(T5_Boredom) & is.na(T5_Anxiety) & is.na(T5_Frustration) & 
             is.na(T5_Interest) & is.na(T6_Selfperception_V1) & is.na(T6_ImportanceValue_V1) & 
             is.na(T6_Selfperception_V2) & is.na(T6_PerceivedDifficulty) & is.na(T6_IntrinsicValue_V1) & 
             is.na(T6_Surprise) & is.na(T6_Joy) & is.na(T6_Confusion) & is.na(T6_Curiosity) & 
             is.na(T6_Boredom) & is.na(T6_Anxiety) & is.na(T6_Frustration) & is.na(T6_Interest)))

View(dataset_cleaned)
alle <-datasetMerged
datasetMerged <- dataset_cleaned
 

###Relevante Spalten auswählen ----
datasetMerged <- datasetMerged%>% select(UserID:T5_Interest) %>% rowwise() %>% 
  mutate(nNA_T1toT5=sum(is.na(across(T1_Selfperception_V1:T5_Interest)))) 


###Entfernen der Fälle ohne Werte von T2-T5 ----
datasetMerged <- datasetMerged[datasetMerged$nNA_T1toT5!=58,] 


### Auswählen der Fälle: mindestens md 3 von 5 Ts ----
datasetMerged <- datasetMerged[datasetMerged$nNA_T1toT5<=24,] 
datasetWithoutMissings <- datasetMerged[datasetMerged$nNA_T1toT5<=0,]


### Überprüfen: Straightlining ----
check_straightlining <- function(data, columns) {
  columns <- columns[columns %in% names(data)]
  
  # Prüft, ob alle nicht-NA Antworten gleich sind
  apply(data[columns], 1, function(x) {
    unique_non_na <- unique(na.omit(x))
    length(unique_non_na) == 1
  })
}


straightlining_indices <- integer(0)

messzeitpunkte_spalten <- list(
  T1 = c("T1_Selfperception_V1", "T1_ImportanceValue_V1", "T1_Surprise", "T1_Joy", "T1_Confusion", 
         "T1_Curiosity", "T1_Boredom", "T1_Anxiety", "T1_Frustration", "T1_Interest"),
  T2 = c("T2_Selfperception_V1", "T2_ImportanceValueV1", "T2_Selfperception_V2", "T2_PerceivedDifficulty", 
         "T2_Surprise", "T2_Joy", "T2_Confusion", "T2_Curiosity", "T2_Boredom", "T2_Anxiety", "T2_Frustration", "T2_Interest"),
  T3 = c("T3_SelfperceptionV1", "T3_ImportanceValueV1", "T3_SelfperceptionV2", "T3_PerceivedDifficulty", 
         "T3_Surprise", "T3_Joy", "T3_Confusion", "T3_Curiosity", "T3_Boredom", "T3_Anxiety", "T3_Frustration", "T3_Interest"),
  T4 = c("T4_Selfperception_V1", "T4_ImportanceValueV1", "T4_Selfperception_V2", "T4_PerceivedDifficulty", 
         "T4_Surprise", "T4_Joy", "T4_Confusion", "T4_Curiosity", "T4_Boredom", "T4_Anxiety", "T4_Frustration", "T4_Interest"),
  T5 = c("T5_Selfperception_V1", "T5_ImportanceValueV1", "T5_Selfperception_V2", "T5_PerceivedDifficulty", 
         "T5_Surprise", "T5_Joy", "T5_Confusion", "T5_Curiosity", "T5_Boredom", "T5_Anxiety", "T5_Frustration", "T5_Interest"),
  T6 = c("T6_Selfperception_V1", "T6_ImportanceValue_V1", "T6_Selfperception_V2", "T6_PerceivedDifficulty", "T6_IntrinsicValue_V1", 
         "T6_Surprise", "T6_Joy", "T6_Confusion", "T6_Curiosity", "T6_Boredom", "T6_Anxiety", "T6_Frustration", "T6_Interest")
)


# Prüft Straightlining für jeden Messzeitpunkt
for (zeitpunkt in names(messzeitpunkte_spalten)) {
  columns <- messzeitpunkte_spalten[[zeitpunkt]]
  straightlining_at_t <- check_straightlining(datasetMerged, columns)
  
  # Speichert die Indizes der Teilnehmer mit Straightlining
  straightlining_indices <- c(straightlining_indices, which(straightlining_at_t))
}

# Entfernt Duplikate, falls ein Teilnehmer in mehr als einem Messzeitpunkt Straightlining zeigt
straightlining_indices <- unique(straightlining_indices)

# Entfernt Teilnehmer mit Straightlining aus dem Datensatz
datasetMerged <- datasetMerged[-straightlining_indices, ]

cat("Anzahl entfernter Teilnehmer aufgrund von Straightlining:", length(straightlining_indices), "\n")

###Überprüfen: Tendenz zu Extremwerten ----
# Auswahl der relevanten Spalten (19 bis 76)
selected_columns <- datasetMerged[, 19:76]

# Anzahl der extremen Antworten (1 und 5) pro Befragten
extreme_values_count <- rowSums(selected_columns == 1 | selected_columns == 5, na.rm = TRUE)

# Gesamtanzahl der Antworten pro Befragter (ohne NAs)
total_answers <- rowSums(!is.na(selected_columns))

# Prozentsatz der extremen Antworten pro Befragter
percent_extreme_values <- (extreme_values_count / total_answers) * 100

# DataFrame mit den Ergebnissen
result <- data.frame(
  ID = 1:nrow(datasetMerged),
  extreme_values_count = extreme_values_count,
  percent_extreme_values = percent_extreme_values
)

# Festlegung eines Schwellenwerts für die Identifikation der Extremwerttendenz
threshold <- 80  # Beispiel: Befragte mit mehr als 30% extremen Antworten



# Identifizierung der Befragten mit Extremwerttendenz
extreme_respondents <- result[result$percent_extreme_values > threshold, ]

# Entfernen der Befragten mit Extremwerttendenz aus dem ursprünglichen Datensatz
indices_to_remove <- c(7, 16, 139, 264)

# Entfernen Sie die entsprechenden Zeilen aus dem Datensatz
datasetMerged <- datasetMerged[-indices_to_remove, ]



### Überprüfung: Tendenz zur Mitte ----

check_central_tendency <- function(data, columns, central_value = 3) {
  columns <- columns[columns %in% names(data)]
  
  # Anzahl der mittleren Antworten pro Befragten
  central_values_count <- rowSums(data[columns] == central_value, na.rm = TRUE)
  
  # Prozentsatz der mittleren Antworten pro Befragten
  percent_central_values <- (central_values_count / rowSums(!is.na(data[columns]))) * 100
  
  return(percent_central_values)
}

# Auswahl der relevanten Spalten (17 bis 74)
selected_columns <- datasetMerged[, 17:74]

# Berechnung des Prozentsatzes der mittleren Antworten
percent_central_values <- check_central_tendency(datasetMerged, colnames(selected_columns))

# DataFrame mit den Ergebnissen
result_central <- data.frame(
  ID = 1:nrow(datasetMerged),
  percent_central_values = percent_central_values
)

# Festlegung eines Schwellenwerts für die Identifikation der Tendenz zur Mitte
central_threshold <- 70  # Beispiel: Befragte mit mehr als 75% mittleren Antworten

# Identifizierung der Befragten mit zentraler Tendenz
central_respondents <- result_central[result_central$percent_central_values > central_threshold, ]
central_respondents



### Umbenennen der Variablen ------
rename_vars <- function(name) {
  # Ersetzt das Muster "T[Zeit]_[Variable]" mit "[Variable].[Zeit]"
  sub("T([0-9]+)_(.*)", "\\2.\\1", name)
}


names(datasetMerged) <- sapply(names(datasetMerged), rename_vars)

#aussortiert
datasetMerged$Pretest<-NULL
datasetMerged$Posttest<-NULL


### Complete_test----
Complete_test <- datasetMerged[c(1:74)]
Complete_test <- as.data.frame(Complete_test)


###Wie viele fehlende Werte?------
#The percentage of NA in each column of Before_test
(colMeans(is.na(datasetMerged)))*100
rowSums(is.na(datasetMerged))

range((colMeans(is.na(Complete_test)))*100)
barplot((colMeans(is.na(Complete_test)))*100)
(colMeans(is.na(Complete_test)))*100
# Angenommen, Ihr Datenrahmen heißt datasetMerged
# Berechnen der Anzahl der fehlenden Werte im gesamten Datensatz
missing_values_total <- sum(is.na(dataroh))

# Berechnen der Gesamtzahl der mögdatasetMerged# Berechnen der Gesamtzahl der möglichen Werte
total_values_total <- nrow(Complete_test) * ncol(Complete_test)

# Berechnen des Prozentsatzes der fehlenden Werte
percentage_missing_total <- (missing_values_total / total_values_total) * 100

# Ausgabe des Prozentsatzes
percentage_missing_total



###Während der Intervention (between tasks) ----

Between_test <- datasetMerged2[c(1,2,3,8, 10, 12, 13, 29:76,90)]
Between_test <-as.data.frame(Between_test)
#Between_test <- Between_test %>% mutate(across(c(1:49),factor)) 
str(Between_test_lowperformer)
summary(Between_test_lowperformer)

#Prozentsatz der NA in jeder Spalte
(colMeans(is.na(Between_test_Dashboard)))*100
range((colMeans(is.na(Between_test_Dashboard)))*100)
barplot((colMeans(is.na(Between_test_Dashboard)))*100)


#Multiple Imputation ----
md.pattern(Before_test)
md.pattern(Between_test)
md.pattern(After_test)

##Visualieren der Missings----
install.packages("VIM") # für die Visualierung der missing
library(VIM)
### Before ----
mice_plot <- aggr(Before_test, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Before_test), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

### Middle ----
mice_plot <- aggr(Between_test, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Between_test), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

### After ----
mice_plot <- aggr(After_test, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(After_test), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


## Die Imputation ----
imputedComplete <- mice(Complete_test, m=30, maxit=30,method = 'pmm', seed =500)
imputedComplete <- mice(filtered_dataset, m=30, maxit=30,method = 'pmm', seed =500)
##Überprüfung: Konvergenz ----
plot(imputedComplete)
densityplot(imputedComplete, ~ Selfperception_V1.1+ ImportanceValue_V1.1 + Surprise.1 + Joy.1 + Confusion.1+Curiosity.1+Boredom.1 + Anxiety.1+Frustration.1 + Interest.1)
densityplot(imputedComplete, ~ Selfperception_V1.2 + Selfperception_V2.2 + ImportanceValue_V1.2 + Surprise.2 + Joy.2 + Confusion.2 + Curiosity.2 + Boredom.2 + Anxiety.2 + Frustration.2 + Interest.2)
densityplot(imputedComplete, ~ Selfperception_V1.3 + Selfperception_V2.3 + ImportanceValue_V1.3 + Surprise.3 + Joy.3 + Confusion.3 + Curiosity.3 + Boredom.3 + Anxiety.3 + Frustration.3 + Interest.3)
densityplot(imputedComplete, ~ Selfperception_V1.4 + Selfperception_V2.4 + ImportanceValue_V1.4 + Surprise.4 + Joy.4 + Confusion.4 + Curiosity.4 + Boredom.4 + Anxiety.4 + Frustration.4 + Interest.4)
densityplot(imputedComplete, ~ Selfperception_V1.5 + Selfperception_V2.5 + ImportanceValue_V1.5 + Surprise.5 + Joy.5 + Confusion.5 + Curiosity.5 + Boredom.5 + Anxiety.5 + Frustration.5 + Interest.5)

## Items zu Variablen in Complete -----
long2 <- complete(imputedComplete, action='long', include=TRUE)
long2$Selfperception.1 <-(long2$Selfperception_V1.1)
long2$TaskValue.1 <- long2$ImportanceValue_V1.1

long2$TaskValue.2 <- (long2$ImportanceValue_V1.2 + long2$Interest.2) / 2
long2$TaskValue.2 <- (long2$ImportanceValue_V1.2 + long2$Interest.2)/2
long2$Selfperception.2 <- (long2$Selfperception_V1.2 + long2$Selfperception_V2.2) / 2



long2$TaskValue.3 <- (long2$ImportanceValue_V1.3 + long2$Interest.3) / 2
long2$Selfperception.3 <- (long2$Selfperception_V1.3 + long2$Selfperception_V2.3) / 2


long2$TaskValue.4 <- (long2$ImportanceValue_V1.4 + long2$Interest.4) / 2
long2$Selfperception.4 <- (long2$Selfperception_V1.4 + long2$Selfperception_V2.4) / 2


long2$TaskValue.5 <- (long2$ImportanceValue_V1.5 + long2$Interest.5) / 2
long2$Selfperception.5 <- (long2$Selfperception_V1.5 + long2$Selfperception_V2.5) / 2




## Zeitpunkte zusammen in Complete ------
long2$Selfperception <- long2$Selfperception.2 + long2$Selfperception.3 + long2$Selfperception.4 + long2$Selfperception.5
long2$TaskValue <- long2$TaskValue.2+long2$TaskValue.3+long2$TaskValue.4+long2$TaskValue.5
long2$PerceivedDifficulty <- long2$PerceivedDifficulty.2 + long2$PerceivedDifficulty.3 + long2$PerceivedDifficulty.4 + long2$PerceivedDifficulty.5
long2$Surprise <- long2$Surprise.2 + long2$Surprise.3 + long2$Surprise.4 + long2$Surprise.5
long2$Joy <- long2$Joy.2 + long2$Joy.3 + long2$Joy.4 + long2$Joy.5
long2$Confusion <- long2$Confusion.2 + long2$Confusion.3 + long2$Confusion.4 + long2$Confusion.5
long2$Curiosity <- long2$Curiosity.2 + long2$Curiosity.3 + long2$Curiosity.4 + long2$Curiosity.5
long2$Boredom <- long2$Boredom.2 + long2$Boredom.3 + long2$Boredom.4 + long2$Boredom.5
long2$Anxiety <- long2$Anxiety.2 + long2$Anxiety.3 + long2$Anxiety.4 + long2$Anxiety.5
long2$Frustration <- long2$Frustration.2 + long2$Frustration.3 + long2$Frustration.4 + long2$Frustration.5

impComplete <- as.mids(long2)

#Regressionsvoraussetzungen und Hypothesenüberprüfung -----
##Hypothese 1a ----

impCompleteDashboardNurMitFeedback <- impComplete %>%
  filter(!(Dashboard == 1 & Feedback == 0))
impCompleteDashboardNurMitFeedback <- impCompleteDashboardNurMitFeedback %>%
  filter(!(UserID == "learner1016" | UserID == "learner1062" | UserID == "learner1112" | 
             UserID == "learner1816" | UserID == "learner2073" | UserID == "learner2074" |
             UserID == "learner2178" | UserID == "learner2192" |
             UserID == "learner827" | UserID == "learner912"| UserID == "learner2186"| UserID == "learner2185" | 
             UserID == "learner1007" |UserID == "learner1077"|UserID == "learner2087"|UserID == "learner2080"))


model <- with(data = impCompleteDashboardNurMitFeedback, exp = lm(FinalCRUScore~ Selfperception*Dashboard))
summary(pool(model))
pooled_model <- pool(model)
View(datasetWithoutMissings)
robust_se <- lapply(model$analyses, function(x) {
  sqrt(diag(vcovHC(x, type = "HC3")))
}) #robuste Standardfehler berechenen

pooled_results <- summary(pooled_model)


robust_se_combined <- sapply(1:nrow(pooled_results), function(i) {
  sqrt(mean(sapply(1:length(robust_se), function(j) robust_se[[j]][i]^2))) #robusten Standardfehler berechnen
})

#Koeffiziententabelle
pooled_results$std.error <- robust_se_combined
pooled_results$statistic <- pooled_results$estimate / pooled_results$std.error
pooled_results$p.value <- 2 * pt(-abs(pooled_results$statistic), df = pooled_results$df)
pooled_results$conf.low <- pooled_results$estimate - qt(0.975, df = pooled_results$df) * pooled_results$std.error
pooled_results$conf.high <- pooled_results$estimate + qt(0.975, df = pooled_results$df) * pooled_results$std.error

# Ausgabe der neuen Koeffiziententabelle
print(pooled_results)

##Regressionsvoraussetzung H1a ----
comp <- complete (impCompleteDashboardNurMitFeedback,30)#verschiedene anschauen, mindestens 5
model <- lm(FinalCRUScore ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
qqnorm(rstandard(model))
qqline(rstandard(model)) # passt
bptest(model) #passt nach entfernen der Ausreißer
ols_plot_cooksd_chart(model)
coeftest(model, vcov = vcovHC(model, type = "HC3"))

model <- lm(Selfperception ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
qqnorm(rstandard(model))
qqline(rstandard(model)) # geht so aber nah genug dran (zentraler Grenzwertsatz)
bptest(model) #passt
raintest(model) #passt


model <- lm(TaskValue ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))

qqnorm(rstandard(model))
qqline(rstandard(model)) #  geht so aber nah genug dran (zentraler Grenzwertsatz)
bptest(model) #passt
raintest(model)


model <- lm(Anxiety ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model)) #nicht normalverteilt
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) # passt nicht
bptest(model) #passt
raintest(model)


model <- lm(Boredom ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt grade so
qqnorm(rstandard(model))
qqline(rstandard(model)) #keine ahnung was da los ist
raintest(model) #passt


model <- lm(Confusion ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) ##nja
raintest(model) #passt

model <- lm(Curiosity ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) ##passt
raintest(model) #passt, aber knapp

model <- lm(Frustration ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) ##nicht normalverteilt
raintest(model) #passt

model <- lm(Joy ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) ##passt
raintest(model) #passt

model <- lm(Surprise ~ Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) #nja
raintest(model) #passt

## Hypothese 2 ----
model <- with(data = impCompleteDashboardNurMitFeedback, exp = lm(FinalCRUScore ~ TaskValue*Dashboard))
summary(pool(model))

## Regressionsvoraussetzung H2 ----
model <- lm(FinalCRUScore ~ TaskValue*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt 
qqnorm(rstandard(model))
qqline(rstandard(model)) # close enough

## Hypothese 3a----
model <- with(data = impCompleteDashboardNurMitFeedback, exp = lm(FinalCRUScore ~ Selfperception*Dashboard))
summary(pool(model))

## Regressionsvoraussetzung H3a ----
model <- lm(FinalCRUScore ~ Selfperception*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) #annähernd normalverteilt
raintest(model) # Linearität passt


#robuste Standardfehler nutzen

## Regressionsvoraussetzung H3b ----


model <- lm(FinalCRUScore ~ Anxiety*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model)) #annähernd normalverteilt
qqnorm(rstandard(model))
qqline(rstandard(model)) 
bptest(model) #passt 
model <- lm(FinalCRUScore ~ Boredom*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt 
qqnorm(rstandard(model))
qqline(rstandard(model)) #passt scho

model <- lm(FinalCRUScore ~ Confusion*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model)) #passt


model <- lm(FinalCRUScore ~ Curiosity*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt

qqnorm(rstandard(model))
qqline(rstandard(model)) ##passt

model <- lm( FinalCRUScore~ Frustration*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt nicht
qqnorm(rstandard(model))
qqline(rstandard(model)) ##passt schon
ols_plot_cooksd_chart(model)
model <- lm(FinalCRUScore ~ Joy*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) ##passt

model <- lm(FinalCRUScore ~ Surprise*Dashboard, data=comp)
hist(rstandard(model))
hist(residuals(model))
bptest(model) #passt
qqnorm(rstandard(model))
qqline(rstandard(model)) #nah genug


#Wie kann ich Cooks Distanzen überprüfen?
cooks_distances <- cooks.distance(model)
ols_plot_cooksd_chart(model)
plot(cooks_distances, main = "Cook's Distances", xlab = "Observation", ylab = "Cook's Distance")
abline(h = 4/(nrow(comp) - length(coef(model))), col = "red") # Schwellenwertlinie hinzufügen

potential_outliers <- which(cooks_distances > 4/(nrow(comp) - length(coef(model))))
potential_outliers


#ttest----

# Extract the imputed datasets and define the Radiation variable  
# as a factor variable

imp.dataset <- lapply(1:30, function(i) {
  dataset <- complete(impC, action = i)
  #dataset$Feedback <- factor(dataset$Feedback)
  return(dataset)
})

#zuerst .5
mi.t.test(imp.dataset, x = "Selfperception.5", y = "Selfperception.1", paired = TRUE)
mi.t.test(imp.dataset, x = "TaskValue.5", y = "TaskValue.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Anxiety.5", y = "Anxiety.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Boredom.5", y = "Boredom.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Frustration.5", y = "Frustration.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Joy.5", y = "Joy.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Curiosity.5", y = "Curiosity.1", paired = TRUE)
mi.t.test(imp.dataset, x = "Surprise.5", y = "Surprise.1", paired = TRUE)
mi.t.test(imp.dataset, x = "TaskValue.1", y = "TaskValue.5", paired = TRUE)


impD <-  filter(impCompleteDashboardNurMitFeedback, Dashboard == 1)
impC <-  filter(impCompleteDashboardNurMitFeedback, Dashboard == 0)

compD <-  filter(comp, Dashboard == 1)
compC <-  filter(comp, Dashboard == 0)

mean(compD$TaskValue.1)
mean(compD$TaskValue.5)
mean(compC$TaskValue.1)
mean(compC$TaskValue.5)

mean(a$TaskValue.1)
mean(a$TaskValue.5)
mean(b$TaskValue.1)
mean(b$TaskValue.5)
a <- complete (impCompleteDashboardNurMitFeedback, 3)
b<- complete (impCompleteDashboardNurMitFeedback, 10)
c<- complete (impCompleteDashboardNurMitFeedback, 27)
d<- complete (impHighperformer, 17)
View(d)

#weiterführende Analysen: 
impHighperformer <- filter (impComplete, FinalCRUScore > 100)
model <- with(data = impHighperformer, exp = lm(Joy~ FinalCRUScore*Dashboard))
summary(pool(model))


#Reliabilität
datasetMerged3 <- datasetMerged %>%
  filter(!(UserID == "learner1016" | UserID == "learner1062" | UserID == "learner1112" | 
             UserID == "learner1816" | UserID == "learner2073" | UserID == "learner2074" |
             UserID == "learner2178" | UserID == "learner2192" |
             UserID == "learner827" | UserID == "learner912"| UserID == "learner2186"| UserID == "learner2185" | 
             UserID == "learner1007" |UserID == "learner1077"|UserID == "learner2087"|UserID == "learner2080"))

cor(datasetMerged$Selfperception_V2.5, datasetMerged$Selfperception_V1.5, use="complete.obs"))


alpha(subset(comp, select = c(Selfperception_V1.6, Selfperception_V2.6)), check.keys =TRUE)

alpha(subset(datasetMerged, select = c(T6_Selfperception_V1,T6_Selfperception_V2)), check.keys =TRUE, use = "complete.obs")
View(datasetMerged)




## Items zu Variablen in Complete -----
long2 <- complete(imputedComplete, action='long', include=TRUE)
long2$Selfperception.1 <-(long2$Selfperception_V1.1)
long2$TaskValue.1 <- long2$ImportanceValue_V1.1

long2$TaskValue.2 <- (long2$ImportanceValue_V1.2 + long2$Interest.2) / 2
long2$TaskValue.2 <- (long2$ImportanceValue_V1.2 + long2$Interest.2)/2
long2$Selfperception.2 <- (long2$Selfperception_V1.2 + long2$Selfperception_V2.2) / 2



long2$TaskValue.3 <- (long2$ImportanceValue_V1.3 + long2$Interest.3) / 2
long2$Selfperception.3 <- (long2$Selfperception_V1.3 + long2$Selfperception_V2.3) / 2


long2$TaskValue.4 <- (long2$ImportanceValue_V1.4 + long2$Interest.4) / 2
long2$Selfperception.4 <- (long2$Selfperception_V1.4 + long2$Selfperception_V2.4) / 2


long2$TaskValue.5 <- (long2$ImportanceValue_V1.5 + long2$Interest.5) / 2
long2$Selfperception.5 <- (long2$Selfperception_V1.5 + long2$Selfperception_V2.5) / 2




## wg Imputation erst im Nachhinen
datasetMerged$TaskValue.2 <- (datasetMerged$ImportanceValue_V1.2 + datasetMerged$Interest.2) / 2
datasetMerged$Selfperception.2 <- (datasetMerged$Selfperception_V1.2 + datasetMerged$Selfperception_V2.2) / 2

datasetMerged$TaskValue.3 <- (datasetMerged$ImportanceValue_V1.3 + datasetMerged$Interest.3) / 2
datasetMerged$Selfperception.3 <- (datasetMerged$Selfperception_V1.3 + datasetMerged$Selfperception_V2.3) / 2

datasetMerged$TaskValue.4 <- (datasetMerged$ImportanceValue_V1.4 + datasetMerged$Interest.4) / 2
datasetMerged$Selfperception.4 <- (datasetMerged$Selfperception_V1.4 + datasetMerged$Selfperception_V2.4) / 2

datasetMerged$TaskValue.5 <- (datasetMerged$ImportanceValue_V1.5 + datasetMerged$Interest.5) / 2
datasetMerged$Selfperception.5 <- (datasetMerged$Selfperception_V1.5 + datasetMerged$Selfperception_V2.5) / 2

datasetMerged$Selfperception <- datasetMerged$Selfperception.2 + datasetMerged$Selfperception.3 + datasetMerged$Selfperception.4 + datasetMerged$Selfperception.5
datasetMerged$TaskValue <- datasetMerged$TaskValue.2 + datasetMerged$TaskValue.3 + datasetMerged$TaskValue.4 + datasetMerged$TaskValue.5
datasetMerged$PerceivedDifficulty <- datasetMerged$PerceivedDifficulty.2 + datasetMerged$PerceivedDifficulty.3 + datasetMerged$PerceivedDifficulty.4 + datasetMerged$PerceivedDifficulty.5
datasetMerged$Surprise <- datasetMerged$Surprise.2 + datasetMerged$Surprise.3 + datasetMerged$Surprise.4 + datasetMerged$Surprise.5
datasetMerged$Joy <- datasetMerged$Joy.2 + datasetMerged$Joy.3 + datasetMerged$Joy.4 + datasetMerged$Joy.5
datasetMerged$Confusion <- datasetMerged$Confusion.2 + datasetMerged$Confusion.3 + datasetMerged$Confusion.4 + datasetMerged$Confusion.5
datasetMerged$Curiosity <- datasetMerged$Curiosity.2 + datasetMerged$Curiosity.3 + datasetMerged$Curiosity.4 + datasetMerged$Curiosity.5
datasetMerged$Boredom <- datasetMerged$Boredom.2 + datasetMerged$Boredom.3 + datasetMerged$Boredom.4 + datasetMerged$Boredom.5
datasetMerged$Anxiety <- datasetMerged$Anxiety.2 + datasetMerged$Anxiety.3 + datasetMerged$Anxiety.4 + datasetMerged$Anxiety.5
datasetMerged$Frustration <- datasetMerged$Frustration.2 + datasetMerged$Frustration.3 + datasetMerged$Frustration.4 + datasetMerged$Frustration.5



describe(datasetMerged$Selfperception.2)
describe(datasetMerged$TaskValue.2)

describe(datasetMerged$Surprise.2)
describe(datasetMerged$Joy.2)
describe(datasetMerged$Confusion.2)
describe(datasetMerged$Curiosity.2)
describe(datasetMerged$Boredom.2)
describe(datasetMerged$Anxiety.2)
describe(datasetMerged$Frustration.2)
describe(datasetMerged$Selfperception.3)
describe(datasetMerged$TaskValue.3)

describe(datasetMerged$Surprise.3)
describe(datasetMerged$Joy.3)
describe(datasetMerged$Confusion.3)
describe(datasetMerged$Curiosity.3)
describe(datasetMerged$Boredom.3)
describe(datasetMerged$Anxiety.3)
describe(datasetMerged$Frustration.3)
describe(datasetMerged$Selfperception.4)
describe(datasetMerged$TaskValue.4)

describe(datasetMerged$Surprise.4)
describe(datasetMerged$Joy.4)
describe(datasetMerged$Confusion.4)
describe(datasetMerged$Curiosity.4)
describe(datasetMerged$Boredom.4)
describe(datasetMerged$Anxiety.4)
describe(datasetMerged$Frustration.4)
describe(datasetMerged$Selfperception.5)
describe(datasetMerged$TaskValue.5)

describe(datasetMerged$Surprise.5)
describe(datasetMerged$Joy.5)
describe(datasetMerged$Confusion.5)
describe(datasetMerged$Curiosity.5)
describe(datasetMerged$Boredom.5)
describe(datasetMerged$Anxiety.5)
describe(datasetMerged$Frustration.5)



hist(datasetMerged$FinalCRUScore,
     main = "Verteilung der erreichten Punktzahl",
     xlab = "erreichte Punktzahl",
     ylab = "Häufigkeit")


hist(datasetMerged$Selfperception, 
     main = "Verteilung der wahrgenommenen Aufgabenleistung",
     xlab = "wahrgenommene Aufgabenleistung",
     ylab = "Häufigkeit")


hist(datasetMerged$TaskValue, 
     main = "Verteilung des wahrgenommenen Aufgabenwertes",
     xlab = "wahrgenommener Aufgabenwert",
     ylab = "Häufigkeit")


hist(datasetMerged$Surprise, 
     main = "Verteilung Überraschung",
     xlab = "Überraschung",
     ylab = "Häufigkeit")


hist(datasetMerged$Joy, 
     main = "Verteilung Freude",
     xlab = "Freude",
     ylab = "Häufigkeit")

hist(datasetMerged$Confusion, 
     main = "Verteilung Verwirrung",
     xlab = "Verwirrung",
     ylab = "Häufigkeit")

hist(datasetMerged$Curiosity, 
     main = "Verteilung Neugier",
     xlab = "Neugier",
     ylab = "Häufigkeit")

hist(datasetMerged$Boredom, 
     main = "Verteilung Langeweile",
     xlab = "wahrgenommener Aufgabenwert",
     ylab = "Häufigkeit")

hist(datasetMerged$Anxiety, 
     main = "Verteilung Ängstlichkeit",
     xlab = "Ängstlichkeit",
     ylab = "Häufigkeit")

hist(datasetMerged$Frustration, 
     main = "Verteilung des Frustration",
     xlab = "Frustration",
     ylab = "Häufigkeit")

#Korrelationstabellen Subgruppen
d <- datasetMerged  %>% filter (Dashboard == 1)
View(k)
subset_cor <- subset(d, 
                     select = c(FinalCRUScore, Selfperception, TaskValue, Surprise, Joy, Confusion, Curiosity, Boredom, Anxiety, Frustration
))

apa.cor.table(subset_cor, filename = "dashboardkorr.doc",
              table.number = 85)
