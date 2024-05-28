######################Preparation######################

#Load relevant packages

install.packages("psych")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ivreg")
install.packages("AER")
install.packages("openxlsx")
install.packages("rio")
install.packages("writexl") 
install.packages("openxlsx")
install.packages("utils")
install.packages("haven")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("broom")
install.packages("AICcmodavg")
install.packages("GPArotation")
install.packages("readr")
install.packages("psych")
install.packages("psychTools")
install.packages("psy")
install.packages("nFactors")
install.packages("summarytools")
install.packages("Hmisc")
install.packages("parameters")
install.packages("performance")
install.packages("olsrr")
install.packages("RTwitterV2")
install.packages("academictwitteR")

install.packages("broom.mixed")
install.packages("stargazer")

install.packages("officer")
install.packages("flextable")

library(psych)
library(ggplot2)
library(dplyr)
library(ivreg)
library(AER)
library(openxlsx)
library(rio)
library(writexl) 
library(openxlsx)
library(utils)
library(haven)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(GPArotation)
library(readr)
library(psych)
library(psychTools)
library(psy)
library(nFactors)
library(summarytools)
library(Hmisc)
library(parameters)
library(performance)
library(olsrr)
library(RTwitterV2)
library(academictwitteR)
library(ltm)

library(psych)
library(ggplot2)
library(dplyr)
library(ivreg)
library(AER)

library(PerformanceAnalytics) # for chart.Correlation (to visualise correlations among variables)
library(glmmTMB) # for beta regressions
library(factoextra) # for PCA visualisations
library(ggeffects) # to visualise model predictions
library(lattice) # xyplots

install.packages('TMB', type = 'source')
######################Sample######################

#Upload the relevant data set

library("readxl")
data_tweet <- read_xlsx('~/Desktop/Tabelle Database/LIWC-22 Results - Data_Tweet - LIWC Analysis.xlsx')
data_title <- read_xlsx('~/Desktop/Tabelle Database/LIWC-22 Results - Data_Title - LIWC Analysis.xlsx')
total <- rbind(data_tweet,data_title)
total$media <-ifelse(total$media<4,"broadsheets", ifelse(total$media<7,"tabloids", "magazines"))
total$type <- c(rep("tweet",length=nrow(data_tweet)),rep("title",length=nrow(data_title)))
total$newspaper <-ifelse(total$media == 1,"Times",
                         ifelse(total$media == 2,"Telegraph",
                                ifelse(total$media == 3,"Guardian",
                                       ifelse(total$media == 4,"Sun",
                                              ifelse(total$media == 5,"Daily_Mail",
                                                     ifelse(total$media == 6,"Daily_Express",
                                                            ifelse(total$media == 7,"Economist",
                                                                   ifelse(total$media == 8,"Tribune", "Prospect"))))))))
total$newspaper <- as.factor(total$newspaper)
newspapers <- c("Times", "Telegraph", "Guardian", "Sun", "Daily Mail", "Daily Express", "Economist", "Tribune", "Prospect")
total$type <- as.factor(total$type)

total$media[total$media == 1] <- 'Times'
total$media[total$media == 2] <- 'Telegraph'
total$media[total$media == 3] <- 'Guardian'
total$media[total$media == 4] <- 'Sun'
total$media[total$media == 5] <- 'Daily_Mail'
total$media[total$media == 6] <- 'Daily_Express'
total$media[total$media == 7] <- 'Economist'
total$media[total$media == 8] <- 'Tribune'
total$media[total$media == 9] <- 'Prospect'
total$media <- as.factor(total$media)
print(total$media)



######################DATA ARTICLE######################
library("readxl")
data_article <- read_xlsx('~/Desktop/Tabelle Database/LIWC-22 Results - Data_Article - LIWC Analysis.xlsx')

data_article <- data_article %>%
  mutate(Category_article = case_when(
    media %in% 1:3 ~ "Broadsheet_article",
    media %in% 4:6 ~ "Tabloid_article",
    media %in% 7:9 ~ "Magazine_article",
    TRUE ~ NA_character_  # gestisce eventuali valori non previsti
  ))

library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
numeric_cols_article <- data_article[, c("Tone", "tone_pos", "tone_neg", "emotion", 
                                         "emo_pos", "emo_neg", "Emoji", "Exclam", "adj", 
                                         "WPS", "WC", "BigWords", "focusfuture", 
                                         "focuspresent", "focuspast", "conj", "Cognition", 
                                         "Analytic", "quantity", "number", "det", "allnone", 
                                         "cogproc", "Authentic", "pronoun", "ppron", "i", 
                                         "we", "you", "shehe", "they", "ipron", "Apostro", 
                                         "AllPunc", "QMark", "OtherP", "Comma", "Period")]


article_pca <- scale(numeric_cols_article)
pca_result_article <- prcomp(article_pca,scale=T)

summary(pca_result_article)
biplot(pca_result_article)
plot(pca_result_article)
prcomp(data_article)
print(pca_result_article)

fviz_eig(pca_result_article) 
fviz_pca_ind(pca_result_article,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_article,col.ind=total$media,axes=c(3,4)) 
dim_pca_article <- fviz_pca_var(pca_result_article,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)





library(dplyr)
# Calcolo delle statistiche descrittive per ogni categoria
desc_stats <- data_article %>%
  group_by(Category_article) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}"))
print(desc_stats)
View(desc_stats)


library(dplyr)

# Assicurati che data_article sia caricato correttamente e che 'Category_article' sia la colonna delle categorie
# Calcolo delle statistiche descrittive per ogni categoria
desc_stats <- data_article %>%
  group_by(Category_article) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}"))
# Visualizzare la tabella completa nel Viewer di RStudio
View(desc_stats)
# Esportare la tabella delle statistiche descrittive in un file CSV
write.csv(desc_stats, "desc_stats.csv", row.names = FALSE)
# Imposta il percorso al Desktop su macOS
path_to_desktop <- "~/Desktop/desc_stats.csv"
# Esporta il file CSV al Desktop
write.csv(desc_stats, path_to_desktop, row.names = FALSE)




########tab_model(desc_stats)


library(dplyr)
# Calcolo delle statistiche descrittive per ogni categoria per le colonne da 7 a 124
desc_stats <- data_article %>%
  group_by(Category_article) %>%
  summarise(across(7:124, list(mean = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}"))
print(desc_stats)


library(stats)
library(dplyr)
library(stats)
library(dplyr)
library(stats)

library(dplyr)
library(stats)

# Rinominare temporaneamente la colonna "function"
data_article <- data_article %>%
  rename(function_temp = function)

# Inizializzazione del dataframe per raccogliere i risultati
results <- data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop per eseguire ANOVA o Kruskal-Wallis su ciascuna delle variabili da 7 a 124
for (i in 7:124) {
  current_var <- names(data_article)[i]
  
  # Preparazione dei dati per il test, usando all_of per la selezione sicura delle colonne
  data_for_test <- data_article %>%
    select(all_of(c("Category_article", current_var))) %>%
    na.omit()  # Rimuove le righe con valori NA
  
  # Controlla se tutti i valori sono identici
  if (length(unique(data_for_test[[current_var]])) > 1) {
    # Test di normalità per ciascuna categoria
    shapiro_tests <- lapply(split(data_for_test[[current_var]], data_for_test$Category_article), shapiro.test)
    if (all(sapply(shapiro_tests, function(x) x$p.value) > 0.05)) {
      # Test ANOVA se tutte le categorie passano il test di normalità
      formula <- as.formula(paste0(current_var, " ~ Category_article"))
      anova_res <- aov(formula, data = data_for_test)
      anova_summary <- summary(anova_res)
      F_value <- anova_summary[[1]]$`F value`[1]
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      results <- bind_rows(results, data.frame(Variable = current_var, F_value = F_value, p_value = p_value))
    } else {
      # Kruskal-Wallis se qualsiasi categoria non passa il test di normalità
      formula <- as.formula(paste0(current_var, " ~ Category_article"))
      kruskal_res <- kruskal.test(formula, data = data_for_test)
      results <- bind_rows(results, data.frame(Variable = current_var, F_value = NA, p_value = kruskal_res$p.value))
    }
  } else {
    # Aggiungi a results con NA se non ci sono abbastanza dati variabili per un test significativo
    results <- bind_rows(results, data.frame(Variable = current_var, F_value = NA, p_value = NA))
  }
}

# Ripristinare il nome originale della colonna "function"
data_article <- data_article %>%
  rename(function = function_temp)

# Visualizzazione dei risultati
print(results)



# Visualizzazione dei risultati
print(results)







# Filtraggio delle variabili con differenze significative
significant_vars <- results[results$p_value < 0.05, ]
print(significant_vars)
library(ggplot2)
# Visualizzazione con boxplot per le variabili significative
for (var in significant_vars$Variable) {
  ggplot(data_article, aes(x = Category_article, y = .data[[var]], fill = Category_article)) +
    geom_boxplot() +
    labs(title = paste("Distribuzione di", var, "per Categoria di Articolo"),
         x = "Categoria di Articolo", y = var) +
    theme_minimal() -> p
  print(p)
}





library(ggplot2)
# Boxplot per visualizzare le distribuzioni delle variabili
ggplot(data_article, aes(x = Category_article, y = Tone, fill = Category_article)) +
  geom_boxplot() +
  labs(title = "Distribuzione di Tone per Categoria di Articolo", x = "Categoria di Articolo", y = "Tone")





numeric_cols_article <- data_article[, c(7:124)]
article_pca <- scale(numeric_cols_article)
pca_result_article <- prcomp(article_pca,scale=T)
# compute pca
pc_article <- prcomp(data_article[,7:124],scale=T)
# look at pca summary
summary(pc_article)
# look at loadings
round(pc_article$rotation,3)
# visualise percentage of total variance explained by each component
fviz_eig(pc_article) 
fviz_pca_ind(pc_article,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pc_article,col.ind=total$media,axes=c(3,4)) # show different axes

dim_pca_article <- fviz_pca_var(pc_article,
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
ggsave("~/Desktop/plot/dim_pca_article.png", plot = dim_pca_article, dpi = 300, width = 20, height = 16, units = "cm")




######################HYPOTHESIS######################

######################Colors######################
#9cd1c7  celeste
#fffcbb  giallo
#bdbbd7  viola
#ec8677  rosso
#89b0d0  blu
#f3b670  arancio
#bcdb78 verde
#f5d0e4  rosa
#d9d9d9  grigio
#E69F00  arancione
#56B4E9  turchese
#9cd1c7  verdino
#bdbbd7  violettino

custom_colors <- c("Tone_Frequency_Headline" = "#B57EDC", "Tone_Frequency_Status" = "#FFBF00")
custom_colors <- c("Tone_Frequency_Headline" = "#98FF98", "Tone_Frequency_Status" = "#D00000")
custom_colors <- c("Tone_Frequency_Headline" = "#FFD1DC", "Tone_Frequency_Status" = "#808000")
custom_colors <- c("Tone_Frequency_Headline" = "#7FFFD4", "Tone_Frequency_Status" = "#C04000")
custom_colors <- c("Tone_Frequency_Headline" = "#CCCCFF", "Tone_Frequency_Status" = "#DAA520")
custom_colors <- c("Tone_Frequency_Headline" = "#D8BFD8", "Tone_Frequency_Status" = "#CD7F32")
custom_colors <- c("Tone_Frequency_Headline" = "#0F52BA", "Tone_Frequency_Status" = "#C80815")
custom_colors <- c("Tone_Frequency_Headline" = "#003366", "Tone_Frequency_Status" = "#FFD700")
custom_colors <- c("Tone_Frequency_Headline" = "#228B22", "Tone_Frequency_Status" = "#B87333")
custom_colors <- c("Tone_Frequency_Headline" = "#36454F", "Tone_Frequency_Status" = "#CD7F32")
custom_colors <- c("Tone_Frequency_Headline" = "#000080", "Tone_Frequency_Status" = "#FFFDD0")
custom_colors <- c("Tone_Frequency_Headline" = "#6A5ACD", "Tone_Frequency_Status" = "#F5F5DC")
custom_colors <- c("Tone_Frequency_Headline" = "#008080", "Tone_Frequency_Status" = "#F5DEB3")
custom_colors <- c("Tone_Frequency_Headline" = "#C0C0C0", "Tone_Frequency_Status" = "#996666")
custom_colors <- c("Tone_Frequency_Headline" = "#14213D", "Tone_Frequency_Status" = "#996666")
custom_colors <- c("Tone_Frequency_Headline" = "#003153", "Tone_Frequency_Status" = "#E2725B")
custom_colors <- c("Tone_Frequency_Headline" = "#004953", "Tone_Frequency_Status" = "#C08081")
custom_colors <- c("Tone_Frequency_Headline" = "#2F4F4F", "Tone_Frequency_Status" = "#BC8F8F")
custom_colors <- c("Tone_Frequency_Headline" = "#414A4C", "Tone_Frequency_Status" = "#CFCFC4")
custom_colors <- c("Tone_Frequency_Headline" = "#444C38", "Tone_Frequency_Status" = "#BC987E")
custom_colors <- c("Tone_Frequency_Headline" = "#353839", "Tone_Frequency_Status" = "#EFBBCC")
custom_colors <- c("Tone_Frequency_Headline" = "#004953", "Tone_Frequency_Status" = "#C08081")
custom_colors <- c("Tone_Frequency_Headline" = "#BC987E", "Tone_Frequency_Status" = "#004953")
custom_colors <- c("Tone_Frequency_Headline" = "#A3C1E9", "Tone_Frequency_Status" = "#E59876")
custom_colors <- c("Tone_Frequency_Headline" = "#B0D3EE", "Tone_Frequency_Status" = "#E6A088")
custom_colors <- c("Tone_Frequency_Headline" = "#89C2D9", "Tone_Frequency_Status" = "#DE887A")
custom_colors_1 <- c("Celeste" = "#9cd1c7", "Giallo" = "#fffcbb")
custom_colors_2 <- c("Viola" = "#bdbbd7", "Rosso" = "#ec8677")
custom_colors_3 <- c("Blu" = "#89b0d0", "Arancio" = "#f3b670")
custom_colors_4 <- c("Verde" = "#bcdb78", "Rosa" = "#f5d0e4")
custom_colors_5 <- c("Grigio" = "#d9d9d9", "Arancione" = "#E69F00")
custom_colors_6 <- c("Turchese" = "#56B4E9", "Verdino" = "#9cd1c7")
custom_colors_7 <- c("Violettino" = "#bdbbd7", "Blu" = "#89b0d0")
custom_colors <- c("Tone_Frequency_Headline" = "#D8A873", "Tone_Frequency_Status" = "#49796B")
custom_colors <- c("Tone_Frequency_Headline" = "#B87333", "Tone_Frequency_Status" = "#004953")
custom_colors <- c("Tone_Frequency_Headline" = "#f5d0e4", "Tone_Frequency_Status" = "#bcdb78")
custom_colors <- c("Tone_Frequency_Headline" = "#D7ACB1", "Tone_Frequency_Status" = "#bcdb78")
custom_colors <- c("Tone_Frequency_Headline" = "#D7ACB1", "Tone_Frequency_Status" = "#A8BDA5")
custom_colors <- c("Tone_Frequency_Headline" = "#E8B9C3", "Tone_Frequency_Status" = "#B4CCB9")
custom_colors <- c("Tone_Frequency_Headline" = "#E69F00", "Tone_Frequency_Status" = "#808000")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#E69F00")
custom_colors <- c("Tone_Frequency_Headline" = "#D8A873", "Tone_Frequency_Status" = "#E6A4A6")
custom_colors <- c("Tone_Frequency_Headline" = "#f3b670", "Tone_Frequency_Status" = "#9cd1c7")
custom_colors <- c("Tone_Frequency_Headline" = "#B87333", "Tone_Frequency_Status" = "#004953")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#6B8E23")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#40817D")
custom_colors <- c("Tone_Frequency_Headline" = "#AC6B25", "Tone_Frequency_Status" = "#355E55")
custom_colors <- c("Tone_Frequency_Headline" = "#CD7F32", "Tone_Frequency_Status" = "#30675B")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#7B9E3C")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#89A94E")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#92B358")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#9CBD64")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#A5C76F")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#AFD17B")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#B8DB86")
custom_colors <- c("Tone_Frequency_Headline" = "#CD7F32", "Tone_Frequency_Status" = "#30675B")
custom_colors <- c("Tone_Frequency_Headline" = "#AC6B25", "Tone_Frequency_Status" = "#355E55")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#3A6B5F")
custom_colors <- c("Tone_Frequency_Headline" = "#BF6A30", "Tone_Frequency_Status" = "#487E70")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#40817D")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#FFF8E1")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#003366")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#6B8E23")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#6B8E23")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#B87333")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#D7A86E")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#CD7F32")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#704214")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#B0735A")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#BE8257")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#36454F")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#9EA476")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#6C97A3")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#CC5500")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#8E4585")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#CD9575")

custom_colors <- c("Tone_Frequency_Headline" = "#14213D", "Tone_Frequency_Status" = "#996666")
custom_colors <- c("Tone_Frequency_Headline" = "#003153", "Tone_Frequency_Status" = "#E2725B")
custom_colors <- c("Tone_Frequency_Headline" = "#004953", "Tone_Frequency_Status" = "#C08081")
custom_colors <- c("Tone_Frequency_Headline" = "#2F4F4F", "Tone_Frequency_Status" = "#BC8F8F")
custom_colors <- c("Tone_Frequency_Headline" = "#414A4C", "Tone_Frequency_Status" = "#CFCFC4")
custom_colors <- c("Tone_Frequency_Headline" = "#444C38", "Tone_Frequency_Status" = "#BC987E")
custom_colors <- c("Tone_Frequency_Headline" = "#353839", "Tone_Frequency_Status" = "#EFBBCC")
custom_colors <- c("Tone_Frequency_Headline" = "#004953", "Tone_Frequency_Status" = "#C08081")
custom_colors <- c("Tone_Frequency_Headline" = "#BC987E", "Tone_Frequency_Status" = "#C08081")

######################final palettes######################
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#9CBD64")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#004953")
custom_colors <- c("Tone_Frequency_Headline" = "#6B8E23", "Tone_Frequency_Status" = "#30675B")
custom_colors <- c("Tone_Frequency_Headline" = "#E6A4A6", "Tone_Frequency_Status" = "#B87333")
custom_colors <- c("Tone_Frequency_Headline" = "#BE8257", "Tone_Frequency_Status" = "#6B8E23")

custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#004953")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#6B8E23")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#CD7F32")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#704214")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#4C2F2F")

custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#FFA500")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#FFCC99")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#ea801c")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#FAA307")


custom_colors <- c("Tone_Frequency_Headline" = "#023E8A", "Tone_Frequency_Status" = "#6A040F")
custom_colors <- c("Tone_Frequency_Headline" = "#0096C7", "Tone_Frequency_Status" = "#9D0208")
custom_colors <- c("Tone_Frequency_Headline" = "#00B4D8", "Tone_Frequency_Status" = "#DC2F02")
custom_colors <- c("Tone_Frequency_Headline" = "#90E0EF", "Tone_Frequency_Status" = "#F48C06")
custom_colors <- c("Tone_Frequency_Headline" = "#CAF0F8", "Tone_Frequency_Status" = "#FAA307")
custom_colors <- c("Tone_Frequency_Headline" = "#555B6E", "Tone_Frequency_Status" = "#FF006E")
custom_colors <- c("Tone_Frequency_Headline" = "#89B0AE", "Tone_Frequency_Status" = "#8338EC")
custom_colors <- c("Tone_Frequency_Headline" = "#BEE3DB", "Tone_Frequency_Status" = "#3A86FF")
custom_colors <- c("Tone_Frequency_Headline" = "#FFD6BA", "Tone_Frequency_Status" = "#FB5607")
custom_colors <- c("Tone_Frequency_Headline" = "#40817D", "Tone_Frequency_Status" = "#FFBE0B")
custom_colors <- c("Tone_Frequency_Headline" = "#D8E2DC", "Tone_Frequency_Status" = "#FFA500")
custom_colors <- c("Tone_Frequency_Headline" = "#FFE5D9", "Tone_Frequency_Status" = "#FFA500")
custom_colors <- c("Tone_Frequency_Headline" = "#FFCAD4", "Tone_Frequency_Status" = "#FFA500")
custom_colors <- c("Tone_Frequency_Headline" = "#F4ACB7", "Tone_Frequency_Status" = "#FFA500")
custom_colors <- c("Tone_Frequency_Headline" = "#9D8189", "Tone_Frequency_Status" = "#FFA500")

 
######################Emotionality######################
#######H1####### 
#Status messages present a higher degree of emotionality than titles and subtitles.

#######h1a#######
#Status messages have a more positive tone than titles and subtitles.(Tone)

# Initialize lists to collect results
tone_frequency_status <- list()
tone_frequency_headline <- list()
# Loop to calculate the average tone for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average tone in status messages
  mean_tone_status <- mean(status_current$Tone, na.rm = TRUE)
  tone_frequency_status[[i]] <- mean_tone_status
  # Calculate the average tone in headlines
  mean_tone_headline <- mean(headlines_current$Tone, na.rm = TRUE)
  tone_frequency_headline[[i]] <- mean_tone_headline
}
# Create a dataframe for the results
tone_frequency_df <- data.frame(
  Newspaper = newspapers,
  Tone_Frequency_Status = unlist(tone_frequency_status),
  Tone_Frequency_Headline = unlist(tone_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
tone_frequency_melted <- melt(tone_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Tone")
# Display the original data frame to verify the inclusion of both types of data
print(tone_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(tone_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
tone_frequency_melted$Newspaper <- factor(tone_frequency_melted$Newspaper,
                                          levels = c("Times", "Telegraph", "Guardian",
                                                     "Sun", "Daily Mail", "Daily Express",
                                                     "Economist", "Tribune", "Prospect"))
tone_frequency_melted$Type <- factor(tone_frequency_melted$Type, levels = c("Tone_Frequency_Headline", "Tone_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Tone_Frequency_Headline" = "#568687", "Tone_Frequency_Status" = "#16485e")

install.packages("extrafont")
library(ggplot2)
library(extrafont)
font_import()
# Load fonts for use with plotting if not already loaded
loadfonts(device = "win")  # Adjust depending on your OS
# Create the ggplot object with all text elements set to Times New Roman
plot_h1a <- ggplot(tone_frequency_melted, aes(x = Newspaper, y = `Average Tone`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL,
       x = "Newspaper",
       y = "Average Tone") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),# Axis titles
        legend.text = element_text(size=12),
        plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5, margin = margin(b = 10, unit = "pt")),  # Plot title settings
        plot.margin = margin(t = 5, r = 5, b = 30, l = 5, unit = "pt"))# Move legend to the bottom
# Save the plot with the adjusted settings
print(plot_h1a)
ggsave("~/Desktop/plot/h1a.png", plot = plot_h1a, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
tone_frequency_status <- list()
tone_frequency_headline <- list()
# Loop to calculate the average tone for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average tone in status messages
  mean_tone_status <- mean(as.numeric(status_current$Tone), na.rm = TRUE)
  tone_frequency_status[[i]] <- mean_tone_status
  # Calculate the average tone in headlines
  mean_tone_headline <- mean(as.numeric(headlines_current$Tone), na.rm = TRUE)
  tone_frequency_headline[[i]] <- mean_tone_headline
}
# Create a dataframe for the results
tone_frequency_df <- data.frame(
  Newspaper = newspapers,
  Tone_Frequency_Status = unlist(tone_frequency_status),
  Tone_Frequency_Headline = unlist(tone_frequency_headline)
)
# Create a new column for newspaper categories
tone_frequency_df$Category <- ifelse(tone_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                                     ifelse(tone_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data <- aggregate(cbind(Tone_Frequency_Status, Tone_Frequency_Headline) ~ Category, data = tone_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
aggregated_melted <- melt(aggregated_data, id.vars = "Category", variable.name = "Type", value.name = "Average Tone")
# Adjust Type factor levels to control plot order
aggregated_melted$Type <- factor(aggregated_melted$Type, levels = c("Tone_Frequency_Headline", "Tone_Frequency_Status"))
# Load required libraries
library(ggplot2)
category_order <- c("Broadsheets", "Tabloids", "Magazines")
# Custom colors
custom_colors <- c("Tone_Frequency_Headline" = "#568687", "Tone_Frequency_Status" = "#16485e")
library(ggplot2)
library(extrafont)
# Create bar plot
# Load necessary libraries
library(ggplot2)
library(extrafont)
# Adapted ggplot object for aggregated data
plot_h1a_aggregated <- ggplot(aggregated_melted, aes(x = Category, y = `Average Tone`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # No title as per your request
       x = "Newspaper Category",
       y = "Average Tone") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Adjust text settings for X-axis
        axis.text.y = element_text(size = 12),  # Adjust text settings for Y-axis
        axis.title = element_text(size = 14),  # Adjust axis title settings
        legend.text = element_text(size=12),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +  # Adjust plot margins if necessary
  scale_x_discrete(limits = category_order) +  # Ensure categories are in the desired order
  guides(fill = guide_legend(reverse = FALSE))
# Save the plot with adjusted settings in Times New Roman
ggsave("~/Desktop/plot/h1a_aggregated.png", plot = plot_h1a_aggregated, dpi = 300, width = 20, height = 16, units = "cm")


######Beta regression for Tone######
# look at distribution of the outcome variable 
hist(total$Tone) # not normally distributed, values bound between 0 and 100 -> beta regression may be a good choice
# could there be a relationship with predictor variables of interest
library("lattice")
bwplot(Tone ~ media | type, total)
### run the model
total$Tone1 <- (total$Tone * 100 - min(total$Tone * 100) + 0.5) / (max(total$Tone * 100) - min(total$Tone * 100) + 1)
# run the beta regression model
#m1 <- glmmTMB (Tone ~ media * relevel(type,ref="tweet"), total, family = beta_family(link = "logit")) #does not seem to work
library(glmmTMB)
mh1a <- glmmTMB(Tone1 ~ media * type, data = total, 
              family = beta_family(link = "logit"))
# model summary
summary(mh1a)
### model selection (potentially)
# interactions don't seem relevant: check!
#m1b <- glmmTMB (Analytic ~ media + type, total, family = beta_family(link = "logit"))
mh1a_g <- glmmTMB(Tone1 ~ media + type, data = total, 
               family = beta_family(link = "logit"))
summary(mh1a_g)
# compare the two models
anova_h1a <- anova(mh1a, mh1a_g) # m1 seems just about better based on p value, but AIC difference is only 2, so i would pick simpler model
print(anova_h1a)


install.packages("sjPlot")
library(sjPlot)
# Crea la tabella per il modello mh1a
tab_model(mh1a_g, 
          show.se = TRUE, show.stat = TRUE, show.p = TRUE,
          file = "model_results_h1a.doc")
# Il file "model_results.doc" sarà creato nella directory corrente e può essere aperto direttamente con Word.
browseURL("model_results_h1a.doc")

library(officer)
library(flextable)
# Convertire i risultati dell'ANOVA in un data frame
anova_table <- as.data.frame(anova_h1a)
anova_table$Model <- c("mh1a: Tone ~ media * type", "mh1a_g: Tone ~ media + type")
# Riordinare le colonne per avere il modello all'inizio
anova_table <- anova_table[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Creare un documento Word
doc <- read_docx()
# Creare una tabella con flextable
anova_ft <- flextable(anova_table)
# Impostare lo stile della tabella
anova_ft <- set_header_labels(anova_ft,
                              Model = "Model",
                              Df = "Df",
                              AIC = "AIC",
                              BIC = "BIC",
                              logLik = "logLik",
                              deviance = "deviance",
                              Chisq = "Chisq",
                              `Chi Df` = "Chi Df",
                              `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft <- font(anova_ft, fontname = "Times New Roman", part = "all")
anova_ft <- fontsize(anova_ft, size = 12, part = "all")
anova_ft <- autofit(anova_ft)
# Aggiungere la tabella al documento Word
doc <- body_add_flextable(doc, value = anova_ft)
# Salvare il documento Word
print(doc, target = "anova_results_h1a.docx")
# Aprire il documento Word
browseURL("anova_results_h1a.docx")
                     


### visualise model predictions
library(ggeffects)
pred <- ggpredict(mh1a_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1,jitter=0.05,dot.size = 1) # ignore warning messages
h1a_plot_pred <- plot(pred)
print(h1a_plot_pred)
#media+type
ggsave("~/Desktop/plot/h1a_plot_pred.png", plot = h1a_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



#######h1aa#######
# h1aa: Status messages have a more positive tone and contain more positive emotions than titles and subtitles.

# Initialize lists to collect results for both tone and emotion
tone_pos_frequency_status <- list()
tone_neg_frequency_status <- list()
tone_pos_frequency_headline <- list()
tone_neg_frequency_headline <- list()
emo_pos_frequency_status <- list()
emo_neg_frequency_status <- list()
emo_pos_frequency_headline <- list()
emo_neg_frequency_headline <- list()
# Loop to calculate the average tone and emotion for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average positive and negative tone in status messages
  mean_tone_pos_status <- mean(as.numeric(status_current$tone_pos), na.rm = TRUE)
  mean_tone_neg_status <- mean(as.numeric(status_current$tone_neg), na.rm = TRUE)
  tone_pos_frequency_status[[i]] <- mean_tone_pos_status
  tone_neg_frequency_status[[i]] <- mean_tone_neg_status
  # Calculate the average positive and negative tone in headlines
  mean_tone_pos_headline <- mean(as.numeric(headlines_current$tone_pos), na.rm = TRUE)
  mean_tone_neg_headline <- mean(as.numeric(headlines_current$tone_neg), na.rm = TRUE)
  tone_pos_frequency_headline[[i]] <- mean_tone_pos_headline
  tone_neg_frequency_headline[[i]] <- mean_tone_neg_headline
  # Calculate the average positive and negative emotion in status messages
  mean_emo_pos_status <- mean(as.numeric(status_current$emo_pos), na.rm = TRUE)
  mean_emo_neg_status <- mean(as.numeric(status_current$emo_neg), na.rm = TRUE)
  emo_pos_frequency_status[[i]] <- mean_emo_pos_status
  emo_neg_frequency_status[[i]] <- mean_emo_neg_status
  # Calculate the average positive and negative emotion in headlines
  mean_emo_pos_headline <- mean(as.numeric(headlines_current$emo_pos), na.rm = TRUE)
  mean_emo_neg_headline <- mean(as.numeric(headlines_current$emo_neg), na.rm = TRUE)
  emo_pos_frequency_headline[[i]] <- mean_emo_pos_headline
  emo_neg_frequency_headline[[i]] <- mean_emo_neg_headline
}
# Create a dataframe for the results, combining all measures
tone_emo_data <- data.frame(
  Newspaper = rep(newspapers, each = 4),
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 4),
  Type = rep(c("Tone Positive", "Tone Negative", "Emotion Positive", "Emotion Negative"), times = 9),
  Frequency_Status = c(unlist(tone_pos_frequency_status), unlist(tone_neg_frequency_status), unlist(emo_pos_frequency_status), unlist(emo_neg_frequency_status)),
  Frequency_Headline = c(unlist(tone_pos_frequency_headline), unlist(tone_neg_frequency_headline), unlist(emo_pos_frequency_headline), unlist(emo_neg_frequency_headline))
)
# Melt the data for plotting
library(reshape2)
melted_data <- melt(tone_emo_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Load required libraries
library(ggplot2)
library(ggeffects)
# Define custom colors: two shades of green for positive, and two shades of red for negative
custom_colors <- c("Tone Positive" = "#16485e",  
                   "Tone Negative" = "#6A040F",  
                   "Emotion Positive" = "#568687",  
                   "Emotion Negative" = "#9D0208") 
# Ensure that the categories are in the correct order
melted_data$Category <- factor(melted_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Create the bar plot with customized settings
plot <- ggplot(melted_data, aes(x = Category, y = AverageValue, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~MessageType, scales = "free_y", labeller = labeller(MessageType = c(Frequency_Status="Status Message", Frequency_Headline="Headline"))) +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
# Save the plot
ggsave("~/Desktop/plot/comparison_tone_emo.png", plot = plot, dpi = 300, width = 22, height = 18, units = "cm")


###h1a.4
# Load necessary libraries
library(ggplot2)
library(reshape2)
# Combine tone and emotion data for each newspaper category
tone_emo_data <- data.frame(
  Newspaper = rep(newspapers, each = 2 * 4),  # Each newspaper will have both headline and status pairs
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 2 * 4),
  Type = rep(c("Tone Positive", "Tone Negative", "Emotion Positive", "Emotion Negative"), times = 2 * 9),
  MessageType = rep(c("Headline", "Status Message"), each = 4, times = 9),
  AverageValue = c(unlist(tone_pos_frequency_headline), unlist(tone_pos_frequency_status),
                   unlist(tone_neg_frequency_headline), unlist(tone_neg_frequency_status),
                   unlist(emo_pos_frequency_headline), unlist(emo_pos_frequency_status),
                   unlist(emo_neg_frequency_headline), unlist(emo_neg_frequency_status))
)
tone_emo_data$Fill <- factor(
  tone_emo_data$Fill,
  levels = c(
    "Emotion Negative - Headline", "Emotion Negative - Status Message",
    "Tone Negative - Headline", "Tone Negative - Status Message",
    "Emotion Positive - Headline", "Emotion Positive - Status Message",
    "Tone Positive - Headline", "Tone Positive - Status Message")
)
# Define custom colors, with headlines having softer tones and status messages richer tones
custom_colors <- c(
  "Tone Positive - Headline" = "#C08081",  
  "Tone Positive - Status Message" = "#6B8E23",
  "Tone Negative - Headline" = "#CD7F32",
  "Tone Negative - Status Message" = "#004953",
  "Emotion Positive - Headline" = "#BC987E",  
  "Emotion Positive - Status Message" = "#89A94E",  
  "Emotion Negative - Headline" = "#d68a55",  
  "Emotion Negative - Status Message" = "#30675B"
)


##custom_colors <- c(
  "Tone Positive - Headline" = "#C08081",  
  "Tone Positive - Status Message" = "#6B8E23",
  "Tone Negative - Headline" = "#d68a55",  
  "Tone Negative - Status Message" = "#004953",  
  "Emotion Positive - Headline" = "#E6A4A6",  
  "Emotion Positive - Status Message" = "#89A94E",  
  "Emotion Negative - Headline" = "#BC987E",  
  "Emotion Negative - Status Message" = "#30675B")
# Create a combined column for type and message type interaction
tone_emo_data$Category <- factor(tone_emo_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
tone_emo_data$Fill <- with(tone_emo_data, interaction(Type, MessageType, sep = " - "))
# Order Fill levels
tone_emo_data$Fill <- factor(tone_emo_data$Fill, levels = c(
  "Tone Positive - Headline", "Tone Positive - Status Message",
  "Tone Negative - Headline", "Tone Negative - Status Message",
  "Emotion Positive - Headline", "Emotion Positive - Status Message",
  "Emotion Negative - Headline", "Emotion Negative - Status Message"
))
# Create the bar plot with customized settings
plot_aa <- ggplot(tone_emo_data, aes(x = Category, y = AverageValue, fill = Fill)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_y") +
  labs(title = NULL, x = "Newspaper Category", y = "Average Value") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(reverse = FALSE))
print(plot_aa)
# Save the plot
ggsave("~/Desktop/plot/tone_emo_side_by_side.png", plot = plot_aa, dpi = 300, width = 28, height = 24, units = "cm")


###### Beta regression analysis setup for tones and emotions ######
#tone_pos
hist(total$tone_pos)
bwplot(tone_pos ~ media | type, total)
total$tone_pos1 <- (total$tone_pos / 30) * 0.98 + 0.01
mtone_pos <- glmmTMB(tone_pos1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mtone_pos)
mtone_pos_g <- glmmTMB(tone_pos1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mtone_pos_g)
anova_tone_pos <- anova(mtone_pos,mtone_pos_g)

# Create a table for the model mtone_pos_g
tab_model(mtone_pos_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_tone_pos.doc")
# Browse the created document
browseURL("model_results_tone_pos.doc")
# Convert the ANOVA results into a data frame
anova_table_tone_pos <- as.data.frame(anova_tone_pos)
anova_table_tone_pos$Model <- c("mtone_pos: adj1 ~ media * type", "mtone_pos_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_tone_pos <- anova_table_tone_pos[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_tone_pos <- read_docx()
# Create a table with flextable
anova_ft_tone_pos <- flextable(anova_table_tone_pos)
# Set the style of the table
anova_ft_tone_pos <- set_header_labels(anova_ft_tone_pos, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_tone_pos <- font(anova_ft_tone_pos, fontname = "Times New Roman", part = "all")
anova_ft_tone_pos <- fontsize(anova_ft_tone_pos, size = 12, part = "all")
anova_ft_tone_pos <- autofit(anova_ft_tone_pos)
# Add the table to the Word document
doc_tone_pos <- body_add_flextable(doc_tone_pos, value = anova_ft_tone_pos)
# Save the Word document
print(doc_tone_pos, target = "anova_results_tone_pos.docx")
# Browse the created document
browseURL("anova_results_tone_pos.docx")


#tone_neg
hist(total$tone_neg)
bwplot(tone_neg ~ media | type, total)
total$tone_neg1 <- (total$tone_neg / 100) * 0.98 + 0.01
mtone_neg <- glmmTMB(tone_neg1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mtone_neg)
mtone_neg_g <- glmmTMB(tone_neg1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mtone_neg_g)
anova_tone_neg <- anova(mtone_neg, mtone_neg_g)


# Create a table for the model mtone_neg_g
tab_model(mtone_neg_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_tone_neg.doc")
# Browse the created document
browseURL("model_results_tone_neg.doc")
# Convert the ANOVA results into a data frame
anova_table_tone_neg <- as.data.frame(anova_tone_neg)
anova_table_tone_neg$Model <- c("mtone_neg: adj1 ~ media * type", "mtone_neg_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_tone_neg <- anova_table_tone_neg[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_tone_neg <- read_docx()
# Create a table with flextable
anova_ft_tone_neg <- flextable(anova_table_tone_neg)
# Set the style of the table
anova_ft_tone_neg <- set_header_labels(anova_ft_tone_neg, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_tone_neg <- font(anova_ft_tone_neg, fontname = "Times New Roman", part = "all")
anova_ft_tone_neg <- fontsize(anova_ft_tone_neg, size = 12, part = "all")
anova_ft_tone_neg <- autofit(anova_ft_tone_neg)
# Add the table to the Word document
doc_tone_neg <- body_add_flextable(doc_tone_neg, value = anova_ft_tone_neg)
# Save the Word document
print(doc_tone_neg, target = "anova_results_tone_neg.docx")
# Browse the created document
browseURL("anova_results_tone_neg.docx")


#emo_pos
hist(total$emo_pos)
bwplot(emo_pos ~ media | type, total)
total$emo_pos1 <- (total$emo_pos / 15) * 0.98 + 0.01
memo_pos <- glmmTMB(emo_pos1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(memo_pos)
memo_pos_g <- glmmTMB(emo_pos1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(memo_pos_g)
anova_emo_pos <- anova(memo_pos, memo_pos_g)

# Create a table for the model memo_pos_g
tab_model(memo_pos_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_emo_pos.doc")
# Browse the created document
browseURL("model_results_emo_pos.doc")
# Convert the ANOVA results into a data frame
anova_table_emo_pos <- as.data.frame(anova_emo_pos)
anova_table_emo_pos$Model <- c("memo_pos: adj1 ~ media * type", "memo_pos_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_emo_pos <- anova_table_emo_pos[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_emo_pos <- read_docx()
# Create a table with flextable
anova_ft_emo_pos <- flextable(anova_table_emo_pos)
# Set the style of the table
anova_ft_emo_pos <- set_header_labels(anova_ft_emo_pos, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_emo_pos <- font(anova_ft_emo_pos, fontname = "Times New Roman", part = "all")
anova_ft_emo_pos <- fontsize(anova_ft_emo_pos, size = 12, part = "all")
anova_ft_emo_pos <- autofit(anova_ft_emo_pos)
# Add the table to the Word document
doc_emo_pos <- body_add_flextable(doc_emo_pos, value = anova_ft_emo_pos)
# Save the Word document
print(doc_emo_pos, target = "anova_results_emo_pos.docx")
# Browse the created document
browseURL("anova_results_emo_pos.docx")


#emo_neg
hist(total$emo_neg)
bwplot(emo_neg ~ media | type, total)
total$emo_neg1 <- (total$emo_neg / 25) * 0.98 + 0.01
memo_neg <- glmmTMB(emo_neg1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(memo_neg)
memo_neg_g <- glmmTMB(emo_neg1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(memo_neg_g)
anova_emo_neg <- anova(memo_neg, memo_neg_g)

# Create a table for the model memo_neg_g
tab_model(memo_neg_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_emo_neg.doc")
# Browse the created document
browseURL("model_results_emo_neg.doc")
# Convert the ANOVA results into a data frame
anova_table_emo_neg <- as.data.frame(anova_emo_neg)
anova_table_emo_neg$Model <- c("memo_neg: adj1 ~ media * type", "memo_neg_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_emo_neg <- anova_table_emo_neg[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_emo_neg <- read_docx()
# Create a table with flextable
anova_ft_emo_neg <- flextable(anova_table_emo_neg)
# Set the style of the table
anova_ft_emo_neg <- set_header_labels(anova_ft_emo_neg, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_emo_neg <- font(anova_ft_emo_neg, fontname = "Times New Roman", part = "all")
anova_ft_emo_neg <- fontsize(anova_ft_emo_neg, size = 12, part = "all")
anova_ft_emo_neg <- autofit(anova_ft_emo_neg)
# Add the table to the Word document
doc_emo_neg <- body_add_flextable(doc_emo_neg, value = anova_ft_emo_neg)
# Save the Word document
print(doc_emo_neg, target = "anova_results_emo_neg.docx")
# Browse the created document
browseURL("anova_results_emo_neg.docx")



# Combine datasets first
total <- rbind(data_tweet, data_title)
total$type <- rep(c("Status", "Headline"), c(nrow(data_tweet), nrow(data_title)))
# Create the 'Category' column correctly after ensuring 'media' column is properly set
total$Category <- factor(
  ifelse(total$media %in% c(1, 2, 3), "Broadsheets",
         ifelse(total$media %in% c(4, 5, 6), "Tabloids", "Magazines"))
)
# Verify the structure and table of categories
str(total)
table(total$Category)
# Ensure library is loaded
library(glmmTMB)
# Check and ensure 'tone_pos' and 'tone_neg' are present and correctly manipulated
if("tone_pos" %in% names(total) && "tone_neg" %in% names(total)) {
  # Adjust scaling if necessary
  total$tone_pos1 <- (total$tone_pos - min(total$tone_pos)) / (max(total$tone_pos) - min(total$tone_pos))
  total$tone_neg1 <- (total$tone_neg - min(total$tone_neg)) / (max(total$tone_neg) - min(total$tone_neg))
  # Create a new variable that is the difference but adjusted to be within 0 and 1
  total$tone_diff <- total$tone_pos1 - total$tone_neg1
  total$tone_diff <- (total$tone_diff - min(total$tone_diff)) / (max(total$tone_diff) - min(total$tone_diff))
  total$tone_diff <- 0.01 + 0.98 * total$tone_diff  # Rescale to be strictly between 0.01 and 0.99
  # Fit the beta regression model
  model_tone <- glmmTMB(tone_diff ~ media * type, data = total, family = beta_family(link = "logit"))
  summary(model_tone)
} else {
  cat("Required columns for analysis are missing.\n")
}
model_tone_g <- glmmTMB(tone_diff ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_tone_g)


# Emotion Positive vs. Negative
summary(total$emo_pos)
summary(total$emo_neg)
# Adjust scaling to ensure values are between 0 and 1
total$emo_pos1 <- (total$emo_pos - min(total$emo_pos)) / (max(total$emo_pos) - min(total$emo_pos))
total$emo_neg1 <- (total$emo_neg - min(total$emo_neg)) / (max(total$emo_neg) - min(total$emo_neg))
# Create a new variable that is the difference but adjusted to be within 0 and 1
total$emo_diff <- total$emo_pos1 - total$emo_neg1
total$emo_diff <- (total$emo_diff - min(total$emo_diff)) / (max(total$emo_diff) - min(total$emo_diff))
total$emo_diff <- 0.01 + 0.98 * total$emo_diff  # Rescale to be strictly between 0.01 and 0.99
# Fit the beta regression model
model_emo <- glmmTMB(emo_diff ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_emo)



library(ggeffects)
# Predictions for tone difference
preds_tone <- ggpredict(model_tone, terms = c("media", "type"))
plot(preds_tone)
# Predictions for emotion difference
preds_emo <- ggpredict(model_emo, terms = c("media", "type"))
plot(preds_emo)
# Save plots if needed
ggsave("~/Desktop/plot/preds_tone_comparison.png", plot = plot(preds_tone), dpi = 300, width = 8, height = 6, units = "in")
ggsave("~/Desktop/plot/preds_emo_comparison.png", plot = plot(preds_emo), dpi = 300, width = 8, height = 6, units = "in")






# Preparare i dati
total$tone_pos1 <- (total$tone_pos / 30) * 0.98 + 0.01
total$tone_neg1 <- (total$tone_neg / 100) * 0.98 + 0.01
total$emo_pos1 <- (total$emo_pos / 15) * 0.98 + 0.01
total$emo_neg1 <- (total$emo_neg / 25) * 0.98 + 0.01
# Modelli di regressione beta
mtone_pos <- glmmTMB(tone_pos1 ~ media * type, data = total, family = beta_family(link = "logit"))
mtone_pos_1 <- glmmTMB(tone_pos1 ~ media + type, data = total, family = beta_family(link = "logit"))
mtone_neg <- glmmTMB(tone_neg1 ~ media * type, data = total, family = beta_family(link = "logit"))
mtone_neg_1 <- glmmTMB(tone_neg1 ~ media + type, data = total, family = beta_family(link = "logit"))
memo_pos <- glmmTMB(emo_pos1 ~ media * type, data = total, family = beta_family(link = "logit"))
memo_pos_1 <- glmmTMB(emo_pos1 ~ media + type, data = total, family = beta_family(link = "logit"))
memo_neg <- glmmTMB(emo_neg1 ~ media * type, data = total, family = beta_family(link = "logit"))
memo_neg_1 <- glmmTMB(emo_neg1 ~ media + type, data = total, family = beta_family(link = "logit"))
# Calcolare i risultati dell'ANOVA
anova_tone_pos <- anova(mtone_pos, mtone_pos_1)
anova_tone_neg <- anova(mtone_neg, mtone_neg_1)
anova_emo_pos <- anova(memo_pos, memo_pos_1)
anova_emo_neg <- anova(memo_neg, memo_neg_1)
# Funzione per creare la tabella e aggiungerla al documento Word
create_anova_table <- function(anova_results, model_names) {
  anova_table <- as.data.frame(anova_results)
  anova_table$Model <- model_names
  anova_table <- anova_table[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
  # Creare una tabella con flextable
  anova_ft <- flextable(anova_table)
  anova_ft <- set_header_labels(anova_ft,
                                Model = "Model",
                                Df = "Df",
                                AIC = "AIC",
                                BIC = "BIC",
                                logLik = "logLik",
                                deviance = "deviance",
                                Chisq = "Chisq",
                                `Chi Df` = "Chi Df",
                                `Pr(>Chisq)` = "Pr(>Chisq)")
  anova_ft <- font(anova_ft, fontname = "Times New Roman", part = "all")
  anova_ft <- fontsize(anova_ft, size = 12, part = "all")
  anova_ft <- autofit(anova_ft)
  anova_ft <- width(anova_ft, width = 17 / 2.54)  # Impostare la larghezza a 17 cm (convertita in pollici)
  return(anova_ft)
}
# Creare un documento Word
doc <- read_docx()
# Creare tabelle per ciascun modello
anova_tone_pos_table <- create_anova_table(anova_tone_pos, c("mtone_pos: tone_pos1 ~ media * type", "mtone_pos_1: tone_pos1 ~ media + type"))
anova_tone_neg_table <- create_anova_table(anova_tone_neg, c("mtone_neg: tone_neg1 ~ media * type", "mtone_neg_1: tone_neg1 ~ media + type"))
anova_emo_pos_table <- create_anova_table(anova_emo_pos, c("memo_pos: emo_pos1 ~ media * type", "memo_pos_1: emo_pos1 ~ media + type"))
anova_emo_neg_table <- create_anova_table(anova_emo_neg, c("memo_neg: emo_neg1 ~ media * type", "memo_neg_1: emo_neg1 ~ media + type"))
# Aggiungere le tabelle al documento Word
doc <- body_add_flextable(doc, value = anova_tone_pos_table)
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, value = anova_tone_neg_table)
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, value = anova_emo_pos_table)
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, value = anova_emo_neg_table)
# Salvare il documento Word
print(doc, target = "anova_results_multiple_models.docx")
# Aprire il documento Word
browseURL("anova_results_multiple_models.docx")










#######h1b#######
#Tabloid status messages contain more emoji than magazine status messages.(Emoji)

#linear regression??
#family = poisson (my variable is a count)
#+ (1 | newpaper)

#Frequency
emoji_frequency <- table(total$type, total$Emoji)
print(emoji_frequency)

hist(data_tweet$Emoji)
#no emoji in headlines
# Filter data for headlines only
headlines_data <- total[total$type == "Headline", ]
# Calculate the sum of Emoji usage in headlines
emoji_in_headlines <- sum(headlines_data$Emoji)
print(paste("Total emojis in headlines:", emoji_in_headlines))
# Check if any headlines have emojis
if (emoji_in_headlines > 0) {
  print("There are emojis in some headlines.")
} else {
  print("No emojis are present in headlines.")
}

# Calculate the frequencies
emoji_frequency <- table(total$type, total$Emoji)
print(emoji_frequency)
# Map numbers to corresponding newspapers
newspapers <- c("Times", "Telegraph", "Guardian", "Sun", "Daily Mail", "Daily Express", "Economist", "Tribune", "Prospect")
# List to collect results
average_emojis <- list()
# Loop to calculate the average number of emojis per newspaper
for (i in 1:9) {
  tweets_current <- data_tweet[data_tweet$media == i, ]
  mean_emoji_current <- ifelse(nrow(tweets_current) > 0, mean(tweets_current$Emoji, na.rm = TRUE), NA)
  average_emojis[[i]] <- mean_emoji_current
}
# Create a dataframe for the results
average_emojis_df <- data.frame(
  Newspaper = newspapers,
  Average_Emoji = unlist(average_emojis)
)
# Order the newspapers according to the specified sequence
average_emojis_df$Newspaper <- factor(average_emojis_df$Newspaper, levels = newspapers)
# Load ggplot2 if not already loaded
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
# Load necessary libraries
library(ggplot2)
library(extrafont)
# Bar chart of average emoji per newspaper without a title and in Times New Roman
plot_h1b <- ggplot(average_emojis_df, aes(x = Newspaper, y = Average_Emoji, fill = Newspaper)) +
  geom_col() +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        plot.title = element_blank(),  # Remove title
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  labs(x = "Newspaper", y = "Average Proportion of Emoji") +  # Adjust label texts
  scale_fill_brewer(palette = "Spectral")  # Use a palette of colors to differentiate the newspapers
# Save the plot with adjusted settings in Times New Roman
ggsave("~/Desktop/plot/h1b.png", plot = plot_h1b, dpi = 300, width = 20, height = 16, units = "cm")


# Kruskal-Wallis test to compare emoji usage across different types of newspapers
kruskal.test(Emoji ~ media, data = data_tweet)
# Post-hoc analysis if Kruskal-Wallis test is significant
if (kruskal.test(Emoji ~ media, data = data_tweet)$p.value < 0.05) {
  print(pairwise.wilcox.test(data_tweet$Emoji, data_tweet$media, p.adjust.method = "bonferroni"))
}


# Test di Shapiro-Wilk per la variabile "Emoji"
shapiro.test(data_tweet$Emoji)
#Emoji is not normally distributed, the Shapiro-Wilk test resulted significative
# Test di Kruskal-Wallis if "Emoji" is not normally distributed
kruskal_result <- kruskal.test(Emoji ~ media, data=data_tweet)
kruskal_result



#######h1c#######
# h1c: Status messages contain more adjectives than titles and subtitles (adj).

# Initialize lists to collect results
adjective_frequency_status <- list()
adjective_frequency_headline <- list()
# Loop to calculate the average frequency of adjectives for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of adjectives in status messages
  mean_adj_status <- mean(status_current$adj, na.rm = TRUE)
  adjective_frequency_status[[i]] <- mean_adj_status
  # Calculate the average frequency of adjectives in headlines
  mean_adj_headline <- mean(headlines_current$adj, na.rm = TRUE)
  adjective_frequency_headline[[i]] <- mean_adj_headline
}
# Create a dataframe for the results
adjective_frequency_df <- data.frame(
  Newspaper = newspapers,
  Adjective_Frequency_Status = unlist(adjective_frequency_status),
  Adjective_Frequency_Headline = unlist(adjective_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
adjective_frequency_melted <- melt(adjective_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Adjectives")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
adjective_frequency_melted$Newspaper <- factor(adjective_frequency_melted$Newspaper,
                                               levels = c("Times", "Telegraph", "Guardian",
                                                          "Sun", "Daily Mail", "Daily Express",
                                                          "Economist", "Tribune", "Prospect"))
adjective_frequency_melted$Type <- factor(adjective_frequency_melted$Type, levels = c("Adjective_Frequency_Headline", "Adjective_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Adjective_Frequency_Headline" = "#568687", "Adjective_Frequency_Status" = "#16485e")
# Create bar plot
plot_h1c <- ggplot(adjective_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Adjectives`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper",
       y = "Average Frequency of Adjectives") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h1c.png", plot = plot_h1c, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
adjective_frequency_status <- list()
adjective_frequency_headline <- list()
# Loop to calculate the average frequency of adjectives for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of adjectives in status messages
  mean_adj_status <- mean(status_current$adj, na.rm = TRUE)
  adjective_frequency_status[[i]] <- mean_adj_status
  # Calculate the average frequency of adjectives in headlines
  mean_adj_headline <- mean(headlines_current$adj, na.rm = TRUE)
  adjective_frequency_headline[[i]] <- mean_adj_headline
}
# Create a dataframe for the results
adjective_frequency_df <- data.frame(
  Newspaper = newspapers,
  Adjective_Frequency_Status = unlist(adjective_frequency_status),
  Adjective_Frequency_Headline = unlist(adjective_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
adjective_frequency_melted <- melt(adjective_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Adjectives")
# Display the original data frame to verify the inclusion of both types of data
print(adjective_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(adjective_frequency_melted, 18))


# Initialize lists to collect results
adjective_frequency_status <- list()
adjective_frequency_headline <- list()
# Loop to calculate the average frequency of adjectives for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of adjectives in status messages
  mean_adj_status <- mean(as.numeric(status_current$adj), na.rm = TRUE)
  adjective_frequency_status[[i]] <- mean_adj_status
  # Calculate the average frequency of adjectives in headlines
  mean_adj_headline <- mean(as.numeric(headlines_current$adj), na.rm = TRUE)
  adjective_frequency_headline[[i]] <- mean_adj_headline
}
# Create a dataframe for the results
adjective_frequency_df <- data.frame(
  Newspaper = newspapers,
  Adjective_Frequency_Status = unlist(adjective_frequency_status),
  Adjective_Frequency_Headline = unlist(adjective_frequency_headline)
)
# Create a new column for newspaper categories
adjective_frequency_df$Category <- ifelse(adjective_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                                          ifelse(adjective_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
adjective_frequency_df$Category <- factor(adjective_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_adj <- aggregate(cbind(Adjective_Frequency_Status, Adjective_Frequency_Headline) ~ Category, data = adjective_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
aggregated_melted_adj <- melt(aggregated_data_adj, id.vars = "Category", variable.name = "Type", value.name = "Average Frequency of Adjectives")
# Adjust Type factor levels to control plot order
aggregated_melted_adj$Type <- factor(aggregated_melted_adj$Type, levels = c("Adjective_Frequency_Headline", "Adjective_Frequency_Status"))
# Load required libraries
library(ggplot2)
# Custom colors
custom_colors_adj <- c("Adjective_Frequency_Headline" = "#568687", "Adjective_Frequency_Status" = "#16485e")
# Create bar plot
plot_h1c_aggregated <- ggplot(aggregated_melted_adj, aes(x = Category, y = `Average Frequency of Adjectives`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper Category",
       y = "Average Frequency of Adjectives") +
  scale_fill_manual(values = custom_colors_adj, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h1c_aggregated.png", plot = plot_h1c_aggregated, dpi = 300, width = 20, height = 18, units = "cm")


###### Beta regression for adj (h1c) ######
# Histogram to examine distribution
hist(total$adj)
# Boxplot to look at relationship with predictor variables
bwplot(adj ~ media | type, total)
# Scale adj to be between 0 and 1 and adjust for beta regression requirements
# First scale adj to be between 0 and 1
total$adj1 <- (total$adj - min(total$adj)) / (max(total$adj) - min(total$adj))
# Now adjust the scale to ensure values are strictly between 0 and 1 for beta regression
total$adj1 <- 0.01 + 0.98 * total$adj1
# Check the summary to ensure no values are exactly 0 or 1
summary(total$adj1)
# Run the beta regression model
library(glmmTMB)
mh1c <- glmmTMB(adj1 ~ media * type, data = total, family = beta_family(link = "logit"))
# Check the summary of the model
summary(mh1c)
# Run the beta regression model without interaction
mh1c_g <- glmmTMB(adj1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh1c_g)
# Compare the two models
anova_h1c <- anova(mh1c, mh1c_g)
print(anova_h1c)
# Install and load necessary libraries for creating tables
library(sjPlot)
library(officer)
library(flextable)
# Create a table for the model mh1c_g
tab_model(mh1c_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h1c.doc")
# Browse the created document
browseURL("model_results_h1c.doc")
# Convert the ANOVA results into a data frame
anova_table_h1c <- as.data.frame(anova_h1c)
anova_table_h1c$Model <- c("mh1c: adj1 ~ media * type", "mh1c_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h1c <- anova_table_h1c[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h1c <- read_docx()
# Create a table with flextable
anova_ft_h1c <- flextable(anova_table_h1c)
# Set the style of the table
anova_ft_h1c <- set_header_labels(anova_ft_h1c, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h1c <- font(anova_ft_h1c, fontname = "Times New Roman", part = "all")
anova_ft_h1c <- fontsize(anova_ft_h1c, size = 12, part = "all")
anova_ft_h1c <- autofit(anova_ft_h1c)
# Add the table to the Word document
doc_h1c <- body_add_flextable(doc_h1c, value = anova_ft_h1c)
# Save the Word document
print(doc_h1c, target = "anova_results_h1c.docx")
# Browse the created document
browseURL("anova_results_h1c.docx")


#######h1d#######
#Status messages contain more exclamation marks than titles and subtitles (Exclam).

#h1d
# Initialize lists to collect results
exclamation_frequency_status <- list()
exclamation_frequency_headline <- list()
# Loop to calculate the average frequency of exclamation marks for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of exclamation marks in status messages
  mean_exclam_status <- mean(status_current$Exclam, na.rm = TRUE)
  exclamation_frequency_status[[i]] <- mean_exclam_status
  # Calculate the average frequency of exclamation marks in headlines
  mean_exclam_headline <- mean(headlines_current$Exclam, na.rm = TRUE)
  exclamation_frequency_headline[[i]] <- mean_exclam_headline
}
# Create a dataframe for the results
exclamation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Exclamation_Frequency_Status = unlist(exclamation_frequency_status),
  Exclamation_Frequency_Headline = unlist(exclamation_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
exclamation_frequency_melted <- melt(exclamation_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Exclamation Marks")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
exclamation_frequency_melted$Newspaper <- factor(exclamation_frequency_melted$Newspaper,
                                                 levels = c("Times", "Telegraph", "Guardian",
                                                            "Sun", "Daily Mail", "Daily Express",
                                                            "Economist", "Tribune", "Prospect"))
exclamation_frequency_melted$Type <- factor(exclamation_frequency_melted$Type, levels = c("Exclamation_Frequency_Headline", "Exclamation_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Exclamation_Frequency_Headline" = "#568687", "Exclamation_Frequency_Status" = "#16485e")
# Create bar plot
plot_h1d <- ggplot(exclamation_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Exclamation Marks`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper",
       y = "Average Frequency of Exclamation Marks") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h1d.png", plot = plot_h1d, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
exclamation_frequency_status <- list()
exclamation_frequency_headline <- list()
# Loop to calculate the average frequency of exclamations for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of exclamations in status messages
  mean_exclam_status <- mean(as.numeric(status_current$Exclam), na.rm = TRUE)
  exclamation_frequency_status[[i]] <- mean_exclam_status
  # Calculate the average frequency of exclamations in headlines
  mean_exclam_headline <- mean(as.numeric(headlines_current$Exclam), na.rm = TRUE)
  exclamation_frequency_headline[[i]] <- mean_exclam_headline
}
# Create a dataframe for the results
exclamation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Exclamation_Frequency_Status = unlist(exclamation_frequency_status),
  Exclamation_Frequency_Headline = unlist(exclamation_frequency_headline)
)
# Create a new column for newspaper categories
exclamation_frequency_df$Category <- ifelse(exclamation_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                                            ifelse(exclamation_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
exclamation_frequency_df$Category <- factor(exclamation_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_exclam <- aggregate(cbind(Exclamation_Frequency_Status, Exclamation_Frequency_Headline) ~ Category, data = exclamation_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
aggregated_melted_exclam <- melt(aggregated_data_exclam, id.vars = "Category", variable.name = "Type", value.name = "Average Frequency of Exclamations")
# Adjust Type factor levels to control plot order
aggregated_melted_exclam$Type <- factor(aggregated_melted_exclam$Type, levels = c("Exclamation_Frequency_Headline", "Exclamation_Frequency_Status"))
# Load required libraries
library(ggplot2)
# Custom colors
custom_colors_exclam <- c("Exclamation_Frequency_Headline" = "#568687", "Exclamation_Frequency_Status" = "#16485e")
# Create bar plot
plot_h1d_aggregated <- ggplot(aggregated_melted_exclam, aes(x = Category, y = `Average Frequency of Exclamations`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper Category",
       y = "Average Frequency of Exclamation Marks") +
  scale_fill_manual(values = custom_colors_exclam, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h1d_aggregated.png", plot = plot_h1d_aggregated, dpi = 300, width = 15, height = 13, units = "cm")


###### Beta regression for Exclam (h1d) ######
# Histogram to examine distribution
hist(total$Exclam)
# Boxplot to look at relationship with predictor variables
bwplot(Exclam ~ media | type, total)
# Scale Exclam to be between 0 and 1 and adjust for beta regression requirements
total$Exclam1 <- (total$Exclam / 100) * 0.98 + 0.01
summary(total$Exclam1)
# Run the beta regression model with interaction
mh1d <- glmmTMB(Exclam1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh1d)
# Run the beta regression model without interaction
mh1d_g <- glmmTMB(Exclam1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh1d_g)
# Compare the two models
anova(mh1d, mh1d_g)

# Create a table for the model mh1c_g
tab_model(mh1d_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_hd.doc")
# Browse the created document
browseURL("model_results_h1d.doc")
# Preparare i dati
total$Exclam1 <- (total$Exclam / 100) * 0.98 + 0.01
# Modelli di regressione beta
mh1d <- glmmTMB(Exclam1 ~ media * type, data = total, family = beta_family(link = "logit"))
mh1d_g <- glmmTMB(Exclam1 ~ media + type, data = total, family = beta_family(link = "logit"))
# Calcolare i risultati dell'ANOVA
anova_h1d <- anova(mh1d, mh1d_g)
# Convertire i risultati dell'ANOVA in un data frame
anova_table_h1d <- as.data.frame(anova_h1d)
anova_table_h1d$Model <- c("mh1d: Exclam1 ~ media * type", "mh1d_g: Exclam1 ~ media + type")
# Riordinare le colonne per avere il modello all'inizio
anova_table_h1d <- anova_table_h1d[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Creare un documento Word
doc_h1d <- read_docx()
# Creare una tabella con flextable
anova_ft_h1d <- flextable(anova_table_h1d)
# Impostare lo stile della tabella
anova_ft_h1d <- set_header_labels(anova_ft_h1d,
                                  Model = "Model",
                                  Df = "Df",
                                  AIC = "AIC",
                                  BIC = "BIC",
                                  logLik = "logLik",
                                  deviance = "deviance",
                                  Chisq = "Chisq",
                                  `Chi Df` = "Chi Df",
                                  `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h1d <- font(anova_ft_h1d, fontname = "Times New Roman", part = "all")
anova_ft_h1d <- fontsize(anova_ft_h1d, size = 12, part = "all")
anova_ft_h1d <- autofit(anova_ft_h1d)
anova_ft_h1d <- width(anova_ft_h1d, width = 17 / 2.54)  # Impostare la larghezza a 17 cm (convertita in pollici)
# Aggiungere la tabella al documento Word
doc_h1d <- body_add_flextable(doc_h1d, value = anova_ft_h1d)
# Salvare il documento Word
print(doc_h1d, target = "anova_results_h1d.docx")
# Aprire il documento Word
browseURL("anova_results_h1d.docx")


# Visualize model predictions
library(ggeffects)
pred_d <- ggpredict(mh1d_g, terms = c("media", "type"))
h1d_plot_pred <- plot(pred_d, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h1d_plot_pred.png", plot = h1d_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


######################Language complexity######################
#######H2#######
#Status messages are linguistically less complex than headlines and subheadings.

#######h2a#######
# h2a: the average sentence length for status messages is shorter than the one for titles and subtitles.(WPS)

# Initialize lists to collect results
sentence_length_status <- list()
sentence_length_headline <- list()
# Loop to calculate the average sentence length for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average sentence length in status messages
  mean_wps_status <- mean(status_current$WPS, na.rm = TRUE)
  sentence_length_status[[i]] <- mean_wps_status
  # Calculate the average sentence length in headlines
  mean_wps_headline <- mean(headlines_current$WPS, na.rm = TRUE)
  sentence_length_headline[[i]] <- mean_wps_headline
}
# Create a dataframe for the results
sentence_length_df <- data.frame(
  Newspaper = newspapers,
  Sentence_Length_Status = unlist(sentence_length_status),
  Sentence_Length_Headline = unlist(sentence_length_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
sentence_length_melted <- melt(sentence_length_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average WPS")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
sentence_length_melted$Newspaper <- factor(sentence_length_melted$Newspaper,
                                           levels = c("Times", "Telegraph", "Guardian",
                                                      "Sun", "Daily Mail", "Daily Express",
                                                      "Economist", "Tribune", "Prospect"))
sentence_length_melted$Type <- factor(sentence_length_melted$Type, levels = c("Sentence_Length_Headline", "Sentence_Length_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Sentence_Length_Headline" = "#80ae9a", "Sentence_Length_Status" = "#122740")
# Create bar plot
plot_h2a <- ggplot(sentence_length_melted, aes(x = Newspaper, y = `Average WPS`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper",
       y = "Average Words Per Sentence") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2a.png", plot = plot_h2a, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
wps_frequency_status <- list()
wps_frequency_headline <- list()
# Loop to calculate the average words per sentence for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average words per sentence in status messages
  mean_wps_status <- mean(as.numeric(status_current$WPS), na.rm = TRUE)
  wps_frequency_status[[i]] <- mean_wps_status
  # Calculate the average words per sentence in headlines
  mean_wps_headline <- mean(as.numeric(headlines_current$WPS), na.rm = TRUE)
  wps_frequency_headline[[i]] <- mean_wps_headline
}
# Create a dataframe for the results
wps_frequency_df <- data.frame(
  Newspaper = newspapers,
  WPS_Frequency_Status = unlist(wps_frequency_status),
  WPS_Frequency_Headline = unlist(wps_frequency_headline)
)
# Create a new column for newspaper categories
wps_frequency_df$Category <- ifelse(wps_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                                    ifelse(wps_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
wps_frequency_df$Category <- factor(wps_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_wps <- aggregate(cbind(WPS_Frequency_Status, WPS_Frequency_Headline) ~ Category, data = wps_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
aggregated_melted_wps <- melt(aggregated_data_wps, id.vars = "Category", variable.name = "Type", value.name = "Average Words Per Sentence")
# Adjust Type factor levels to control plot order
aggregated_melted_wps$Type <- factor(aggregated_melted_wps$Type, levels = c("WPS_Frequency_Headline", "WPS_Frequency_Status"))
# Load required libraries
library(ggplot2)
# Custom colors
custom_colors_wps <- c("WPS_Frequency_Headline" = "#80ae9a", "WPS_Frequency_Status" = "#122740")
# Create bar plot
plot_h2a_aggregated <- ggplot(aggregated_melted_wps, aes(x = Category, y = `Average Words Per Sentence`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper Category",
       y = "Average Words Per Sentence") +
  scale_fill_manual(values = custom_colors_wps, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
ggsave("~/Desktop/plot/h2a_aggregated.png", plot = plot_h2a_aggregated, dpi = 300, width = 20, height = 16, units = "cm")



# Check if both types of data are present in the original data frames
print(wps_frequency_df)
print(word_count_df)
# Redo the melting process to include both status and headline data
length_melted <- melt(length_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average WPS")
word_count_melted <- melt(word_count_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Word Count")
# Display the first few rows of the new melted data to verify correct inclusion
print(head(length_melted, 18))  # Display enough rows to show both types
print(head(word_count_melted, 18))


###### Beta regression for WPS (h2a) ######
hist(total$WPS)
bwplot(WPS ~ media | type, total)
total$WPS1 <- (total$WPS * 100 - min(total$WPS * 100) + 0.5) / (max(total$WPS * 100) - min(total$WPS * 100) + 1)
summary(total$WPS1)
mh2a <- glmmTMB(WPS1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2a)
mh2a_g <- glmmTMB(WPS1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2a_g)
anova(mh2a, mh2a_g)


# Create a table for the model mh1c_g
tab_model(mh2a, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2a.doc")
# Browse the created document
browseURL("model_results_h2a.doc")

library(sjPlot)
library(officer)
library(flextable)
# Preparare i dati
total$WPS1 <- (total$WPS * 100 - min(total$WPS * 100) + 0.5) / (max(total$WPS * 100) - min(total$WPS * 100) + 1)
# Modelli di regressione beta
mh2a <- glmmTMB(WPS1 ~ media * type, data = total, family = beta_family(link = "logit"))
mh2a_g <- glmmTMB(WPS1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2a)
summary(mh2a_g)
# Calcolare i risultati dell'ANOVA
anova_h2a <- anova(mh2a, mh2a_g)
summary(anova_h2a)
# Convertire i risultati dell'ANOVA in un data frame
anova_table_h2a <- as.data.frame(anova_h2a)
anova_table_h2a$Model <- c("mh2a: WPS ~ media * type", "mh2a_g: WPS ~ media + type")
# Riordinare le colonne per avere il modello all'inizio
anova_table_h2a <- anova_table_h2a[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Creare un documento Word
doc_h2a <- read_docx()
# Creare una tabella con flextable
anova_ft_h2a <- flextable(anova_table_h2a)
# Impostare lo stile della tabella
anova_ft_h2a <- set_header_labels(anova_ft_h2a,
                                  Model = "Model",
                                  Df = "Df",
                                  AIC = "AIC",
                                  BIC = "BIC",
                                  logLik = "logLik",
                                  deviance = "deviance",
                                  Chisq = "Chisq",
                                  `Chi Df` = "Chi Df",
                                  `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2a <- font(anova_ft_h2a, fontname = "Times New Roman", part = "all")
anova_ft_h2a <- fontsize(anova_ft_h2a, size = 12, part = "all")
anova_ft_h2a <- autofit(anova_ft_h2a)
anova_ft_h2a <- width(anova_ft_h2a, width = 17 / 2.54)  # Impostare la larghezza a 17 cm (convertita in pollici)
# Aggiungere la tabella al documento Word
doc_h2a <- body_add_flextable(doc_h2a, value = anova_ft_h2a)
# Salvare il documento Word
print(doc_h2a, target = "anova_results_h2a.docx")
# Aprire il documento Word
browseURL("anova_results_h2a.docx")


library(ggeffects)
pred_h2a <- ggpredict(mh2a_g, terms = c("media", "type"))
h2a_plot_pred <- plot(pred_h2a, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
ggsave("~/Desktop/plot/h2a_plot_pred.png", plot = h2a_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2aa#######
# h2aa: Status messages are shorter than titles and subtitles.(WC)

# Initialize lists to collect results
word_count_status <- list()
word_count_headline <- list()
# Loop to calculate the average word count for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average word count in status messages
  mean_wc_status <- mean(status_current$WC, na.rm = TRUE)
  word_count_status[[i]] <- mean_wc_status
  # Calculate the average word count in headlines
  mean_wc_headline <- mean(headlines_current$WC, na.rm = TRUE)
  word_count_headline[[i]] <- mean_wc_headline
}
# Create a dataframe for the results
word_count_df <- data.frame(
  Newspaper = newspapers,
  Word_Count_Status = unlist(word_count_status),
  Word_Count_Headline = unlist(word_count_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
word_count_melted <- melt(word_count_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Word Count")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
word_count_melted$Newspaper <- factor(word_count_melted$Newspaper,
                                      levels = c("Times", "Telegraph", "Guardian",
                                                 "Sun", "Daily Mail", "Daily Express",
                                                 "Economist", "Tribune", "Prospect"))
word_count_melted$Type <- factor(word_count_melted$Type, levels = c("Word_Count_Headline", "Word_Count_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Word_Count_Headline" = "#80ae9a", "Word_Count_Status" = "#122740")
# Create bar plot
plot_h2aa <- ggplot(word_count_melted, aes(x = Newspaper, y = `Average Word Count`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper",
       y = "Average Word Count") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2aa.png", plot = plot_h2aa, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
wc_frequency_status <- list()
wc_frequency_headline <- list()
# Loop to calculate the average word count for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average word count in status messages
  mean_wc_status <- mean(as.numeric(status_current$WC), na.rm = TRUE)
  wc_frequency_status[[i]] <- mean_wc_status
  # Calculate the average word count in headlines
  mean_wc_headline <- mean(as.numeric(headlines_current$WC), na.rm = TRUE)
  wc_frequency_headline[[i]] <- mean_wc_headline
}
# Create a dataframe for the results
wc_frequency_df <- data.frame(
  Newspaper = newspapers,
  WC_Frequency_Status = unlist(wc_frequency_status),
  WC_Frequency_Headline = unlist(wc_frequency_headline)
)
# Create a new column for newspaper categories
wc_frequency_df$Category <- ifelse(wc_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                                   ifelse(wc_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
wc_frequency_df$Category <- factor(wc_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_wc <- aggregate(cbind(WC_Frequency_Status, WC_Frequency_Headline) ~ Category, data = wc_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
aggregated_melted_wc <- melt(aggregated_data_wc, id.vars = "Category", variable.name = "Type", value.name = "Average Word Count")
# Adjust Type factor levels to control plot order
aggregated_melted_wc$Type <- factor(aggregated_melted_wc$Type, levels = c("WC_Frequency_Headline", "WC_Frequency_Status"))
# Load required libraries
library(ggplot2)
# Custom colors
custom_colors_wc <- c("WC_Frequency_Headline" = "#80ae9a", "WC_Frequency_Status" = "#122740")
# Create bar plot
plot_h2aa_aggregated <- ggplot(aggregated_melted_wc, aes(x = Category, y = `Average Word Count`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper Category",
       y = "Average Word Count") +
  scale_fill_manual(values = custom_colors_wc, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2aa_aggregated.png", plot = plot_h2aa_aggregated, dpi = 300, width = 20, height = 16, units = "cm")


###### Beta regression for WC (h2aa) ######
# Histogram to examine distribution
hist(total$WC)
# Boxplot to look at relationship with predictor variables
bwplot(WC ~ media | type, total)
# Correct the scaling issue by applying a more conservative transformation
total$WC1 <- (total$WC * 100 - min(total$WC * 100) + 0.5) / (max(total$WC * 100) - min(total$WC * 100) + 1)
# Verify the transformation
summary(total$WC1)  # Ensure values are now strictly within (0, 1)
# Run the beta regression model with interaction
mh2aa <- glmmTMB(WC1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2aa)
# Run the beta regression model without interaction
mh2aa_g <- glmmTMB(WC1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2aa_g)
# Compare the two models
anova_h2aa <- anova(mh2aa, mh2aa_g)

# Create a table for the model mh2aa
tab_model(mh2aa, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2aa.doc")
# Browse the created document
browseURL("model_results_h2aa.doc")

# Convert the ANOVA results into a data frame
anova_table_h2aa <- as.data.frame(anova_h2aa)
anova_table_h2aa$Model <- c("mh2aa: adj1 ~ media * type", "mh2aa_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2aa <- anova_table_h2aa[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2aa <- read_docx()
# Create a table with flextable
anova_ft_h2aa <- flextable(anova_table_h2aa)
# Set the style of the table
anova_ft_h2aa <- set_header_labels(anova_ft_h2aa, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2aa <- font(anova_ft_h2aa, fontname = "Times New Roman", part = "all")
anova_ft_h2aa <- fontsize(anova_ft_h2aa, size = 12, part = "all")
anova_ft_h2aa <- autofit(anova_ft_h2aa)
# Add the table to the Word document
doc_h2aa <- body_add_flextable(doc_h2aa, value = anova_ft_h2aa)
# Save the Word document
print(doc_h2aa, target = "anova_results_h2aa.docx")
# Browse the created document
browseURL("anova_results_h2aa.docx")



# Visualize model predictions
library(ggeffects)
pred_h2aa <- ggpredict(mh2aa_g, terms = c("media", "type"))
h2aa_plot_pred <- plot(pred_h2aa, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2aa_plot_pred.png", plot = h2aa_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2b#######
#Status messages contain less Big Words than headlines and subheadings. (BigWords)

# Initialize lists to collect results
big_words_frequency_status <- list()
big_words_frequency_headline <- list()
# Loop to calculate the average frequency of Big Words for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of Big Words in status messages
  mean_big_words_status <- mean(status_current$BigWords, na.rm = TRUE)
  big_words_frequency_status[[i]] <- mean_big_words_status
  # Calculate the average frequency of Big Words in headlines
  mean_big_words_headline <- mean(headlines_current$BigWords, na.rm = TRUE)
  big_words_frequency_headline[[i]] <- mean_big_words_headline
}
# Create a dataframe for the results
big_words_frequency_df <- data.frame(
  Newspaper = newspapers,
  Big_Words_Frequency_Status = unlist(big_words_frequency_status),
  Big_Words_Frequency_Headline = unlist(big_words_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
big_words_frequency_melted <- melt(big_words_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Big Words")
# Display the original data frame to verify the inclusion of both types of data
print(big_words_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(big_words_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
big_words_frequency_melted$Newspaper <- factor(big_words_frequency_melted$Newspaper,
                                               levels = c("Times", "Telegraph", "Guardian",
                                                          "Sun", "Daily Mail", "Daily Express",
                                                          "Economist", "Tribune", "Prospect"))
big_words_frequency_melted$Type <- factor(big_words_frequency_melted$Type, levels = c("Big_Words_Frequency_Headline", "Big_Words_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Big_Words_Frequency_Headline" = "#80ae9a", "Big_Words_Frequency_Status" = "#122740")
# Create bar plot
plot_h2b <- ggplot(big_words_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Big Words`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove the title by setting it to NULL
       x = "Newspaper",
       y = "Average Frequency of Big Words") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2b.png", plot = plot_h2b, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
big_words_frequency_status <- list()
big_words_frequency_headline <- list()
# Loop to calculate the average frequency of Big Words for each newspaper
for (i in 1:9) {
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  mean_big_words_status <- mean(as.numeric(status_current$BigWords), na.rm = TRUE)
  big_words_frequency_status[[i]] <- mean_big_words_status
  mean_big_words_headline <- mean(as.numeric(headlines_current$BigWords), na.rm = TRUE)
  big_words_frequency_headline[[i]] <- mean_big_words_headline
}
# Create a dataframe for the results
big_words_frequency_df <- data.frame(Newspaper = newspapers, Big_Words_Frequency_Status = unlist(big_words_frequency_status), Big_Words_Frequency_Headline = unlist(big_words_frequency_headline))
# Create a new column for newspaper categories
big_words_frequency_df$Category <- ifelse(big_words_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets", ifelse(big_words_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
big_words_frequency_df$Category <- factor(big_words_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_bigwords <- aggregate(cbind(Big_Words_Frequency_Status, Big_Words_Frequency_Headline) ~ Category, data = big_words_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {install.packages("reshape2")}
library(reshape2)
aggregated_melted_bigwords <- melt(aggregated_data_bigwords, id.vars = "Category", variable.name = "Type", value.name = "Average Frequency of Big Words")
# Adjust Type factor levels to control plot order
aggregated_melted_bigwords$Type <- factor(aggregated_melted_bigwords$Type, levels = c("Big_Words_Frequency_Headline", "Big_Words_Frequency_Status"))
# Load required libraries
library(ggplot2)
# Custom colors
custom_colors_bigwords <- c("Big_Words_Frequency_Headline" = "#80ae9a", "Big_Words_Frequency_Status" = "#122740")
# Create bar plot
plot_h2b_aggregated <- ggplot(aggregated_melted_bigwords, aes(x = Category, y = `Average Frequency of Big Words`, fill = Type)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL, x = "Newspaper Category", y = "Average Frequency of Big Words") + 
  scale_fill_manual(values = custom_colors_bigwords, labels = c("Headline", "Status Message")) + 
  theme_minimal() + 
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))# Save the plot
ggsave("~/Desktop/plot/h2b_aggregated.png", plot = plot_h2b_aggregated, dpi = 300, width = 20, height = 16, units = "cm")


###### Beta regression for BigWords (h2b) ######
# Histogram to examine distribution
hist(total$BigWords)
# Boxplot to look at relationship with predictor variables
bwplot(BigWords ~ media | type, total)
# Scale BigWords to be between 0 and 1 and adjust for beta regression requirements
total$BigWords1 <- (total$BigWords / 100) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2b <- glmmTMB(BigWords1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2b)
# Run the beta regression model without interaction
mh2b_g <- glmmTMB(BigWords1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2b_g)
# Compare the two models
anova_h2b <- anova(mh2b, mh2b_g)


# Create a table for the model mh2b_g
tab_model(mh2b_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2b.doc")
# Browse the created document
browseURL("model_results_h2b.doc")
# Convert the ANOVA results into a data frame
anova_table_h2b <- as.data.frame(anova_h2b)
anova_table_h2b$Model <- c("mh2b: adj1 ~ media * type", "mh2b_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2b <- anova_table_h2b[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2b <- read_docx()
# Create a table with flextable
anova_ft_h2b <- flextable(anova_table_h2b)
# Set the style of the table
anova_ft_h2b <- set_header_labels(anova_ft_h2b, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2b <- font(anova_ft_h2b, fontname = "Times New Roman", part = "all")
anova_ft_h2b <- fontsize(anova_ft_h2b, size = 12, part = "all")
anova_ft_h2b <- autofit(anova_ft_h2b)
# Add the table to the Word document
doc_h2b <- body_add_flextable(doc_h2b, value = anova_ft_h2b)
# Save the Word document
print(doc_h2b, target = "anova_results_h2b.docx")
# Browse the created document
browseURL("anova_results_h2b.docx")




# Visualize model predictions
library(ggeffects)
pred_h2b <- ggpredict(mh2b_g, terms = c("media", "type"))
h2b_plot_pred <- plot(pred_h2b, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2b_plot_pred.png", plot = h2b_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2c#######
#Status messages contain forward referencing compared to titles and subtitles (focusfuture).

# Initialize lists to collect results
forward_ref_status <- list()
forward_ref_headline <- list()
# Loop to calculate the average frequency of forward referencing for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of forward referencing in status messages
  mean_focusfuture_status <- mean(status_current$focusfuture, na.rm = TRUE)
  forward_ref_status[[i]] <- mean_focusfuture_status
  # Calculate the average frequency of forward referencing in headlines
  mean_focusfuture_headline <- mean(headlines_current$focusfuture, na.rm = TRUE)
  forward_ref_headline[[i]] <- mean_focusfuture_headline
}
# Create a dataframe for the results
forward_ref_df <- data.frame(
  Newspaper = newspapers,
  Forward_Ref_Frequency_Status = unlist(forward_ref_status),
  Forward_Ref_Frequency_Headline = unlist(forward_ref_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
forward_ref_melted <- melt(forward_ref_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Forward Referencing")
# Display the original data frame to verify the inclusion of both types of data
print(forward_ref_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(forward_ref_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
forward_ref_melted$Newspaper <- factor(forward_ref_melted$Newspaper,
                                       levels = c("Times", "Telegraph", "Guardian",
                                                  "Sun", "Daily Mail", "Daily Express",
                                                  "Economist", "Tribune", "Prospect"))
forward_ref_melted$Type <- factor(forward_ref_melted$Type, levels = c("Forward_Ref_Frequency_Headline", "Forward_Ref_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Forward_Ref_Frequency_Headline" = "#80ae9a", "Forward_Ref_Frequency_Status" = "#122740")
# Create bar plot
plot_h2c <- ggplot(forward_ref_melted, aes(x = Newspaper, y = `Average Frequency of Forward Referencing`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove the title
       x = "Newspaper",
       y = "Average Frequency of Forward Referencing") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2c.png", plot = plot_h2c, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
focusfuture_frequency_status <- list()
focusfuture_frequency_headline <- list()
# Loop to calculate the average frequency of future-focused language for each newspaper
for (i in 1:9) {
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  mean_focusfuture_status <- mean(as.numeric(status_current$focusfuture), na.rm = TRUE)
  focusfuture_frequency_status[[i]] <- mean_focusfuture_status
  mean_focusfuture_headline <- mean(as.numeric(headlines_current$focusfuture), na.rm = TRUE)
  focusfuture_frequency_headline[[i]] <- mean_focusfuture_headline
}
# Create a dataframe for the results
focusfuture_frequency_df <- data.frame(Newspaper = newspapers, FocusFuture_Frequency_Status = unlist(focusfuture_frequency_status), FocusFuture_Frequency_Headline = unlist(focusfuture_frequency_headline))
# Create a new column for newspaper categories
focusfuture_frequency_df$Category <- ifelse(focusfuture_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets", 
                                            ifelse(focusfuture_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
focusfuture_frequency_df$Category <- factor(focusfuture_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_focusfuture <- aggregate(cbind(FocusFuture_Frequency_Status, FocusFuture_Frequency_Headline) ~ Category, data = focusfuture_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {install.packages("reshape2")}
library(reshape2)
aggregated_melted_focusfuture <- melt(aggregated_data_focusfuture, id.vars = "Category", variable.name = "Type", value.name = "Average Frequency of Future Focus")
# Adjust Type factor levels to control plot order
aggregated_melted_focusfuture$Type <- factor(aggregated_melted_focusfuture$Type, levels = c("FocusFuture_Frequency_Headline", "FocusFuture_Frequency_Status"))
# Load required libraries
library(ggplot2)
custom_colors_focusfuture <- c("FocusFuture_Frequency_Headline" = "#80ae9a", "FocusFuture_Frequency_Status" = "#122740")
# Create bar plot
plot_h2c_aggregated <- ggplot(aggregated_melted_focusfuture, aes(x = Category, y = `Average Frequency of Future Focus`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove the title
       x = "Newspaper Category",
       y = "Average Frequency of Forward Referencing") +
  scale_fill_manual(values = custom_colors_focusfuture, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2c_aggregated.png", plot = plot_h2c_aggregated, dpi = 300, width = 20, height = 16, units = "cm")

###### Beta regression for focusfuture (h2c) ######
# Histogram to examine distribution
hist(total$focusfuture)
# Boxplot to look at relationship with predictor variables
bwplot(focusfuture ~ media | type, total)
# Scale focusfuture to be between 0 and 1 and adjust for beta regression requirements
total$focusfuture1 <- (total$focusfuture / 20) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2c <- glmmTMB(focusfuture1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2c)
# Run the beta regression model without interaction
mh2c_g <- glmmTMB(focusfuture1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2c_g)
# Compare the two models
anova_h2c <- anova(mh2c, mh2c_g)



# Create a table for the model mh2c_g
tab_model(mh2c_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2c.doc")
# Browse the created document
browseURL("model_results_h2c.doc")
# Convert the ANOVA results into a data frame
anova_table_h2c <- as.data.frame(anova_h2c)
anova_table_h2c$Model <- c("mh2c: adj1 ~ media * type", "mh2c_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2c <- anova_table_h2c[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2c <- read_docx()
# Create a table with flextable
anova_ft_h2c <- flextable(anova_table_h2c)
# Set the style of the table
anova_ft_h2c <- set_header_labels(anova_ft_h2c, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2c <- font(anova_ft_h2c, fontname = "Times New Roman", part = "all")
anova_ft_h2c <- fontsize(anova_ft_h2c, size = 12, part = "all")
anova_ft_h2c <- autofit(anova_ft_h2c)
# Add the table to the Word document
doc_h2c <- body_add_flextable(doc_h2c, value = anova_ft_h2c)
# Save the Word document
print(doc_h2c, target = "anova_results_h2c.docx")
# Browse the created document
browseURL("anova_results_h2c.docx")



# Visualize model predictions
library(ggeffects)
pred_h2c <- ggpredict(mh2c_g, terms = c("media", "type"))
h2c_plot_pred <- plot(pred_h2c, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2c_plot_pred.png", plot = h2c_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2cc#######
#Levels of focus future/present/past in status messages vs. titles and subtitles (focusfuture, focuspast, focuspresent).

# Initialize lists to collect results for focus on future, past, and present
focus_future_status <- list()
focus_past_status <- list()
focus_present_status <- list()
focus_future_headline <- list()
focus_past_headline <- list()
focus_present_headline <- list()
# Loop to calculate the average focus levels for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average focus on future, past, and present in status messages
  mean_focus_future_status <- mean(as.numeric(status_current$focusfuture), na.rm = TRUE)
  mean_focus_past_status <- mean(as.numeric(status_current$focuspast), na.rm = TRUE)
  mean_focus_present_status <- mean(as.numeric(status_current$focuspresent), na.rm = TRUE)
  focus_future_status[[i]] <- mean_focus_future_status
  focus_past_status[[i]] <- mean_focus_past_status
  focus_present_status[[i]] <- mean_focus_present_status
  # Calculate the average focus on future, past, and present in headlines
  mean_focus_future_headline <- mean(as.numeric(headlines_current$focusfuture), na.rm = TRUE)
  mean_focus_past_headline <- mean(as.numeric(headlines_current$focuspast), na.rm = TRUE)
  mean_focus_present_headline <- mean(as.numeric(headlines_current$focuspresent), na.rm = TRUE)
  focus_future_headline[[i]] <- mean_focus_future_headline
  focus_past_headline[[i]] <- mean_focus_past_headline
  focus_present_headline[[i]] <- mean_focus_present_headline
}
# Create a dataframe for the results, combining all measures
focus_data <- data.frame(
  Newspaper = rep(newspapers, each = 3),
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 3),
  Type = rep(c("Focus Future", "Focus Past", "Focus Present"), times = 9),
  Frequency_Status = c(unlist(focus_future_status), unlist(focus_past_status), unlist(focus_present_status)),
  Frequency_Headline = c(unlist(focus_future_headline), unlist(focus_past_headline), unlist(focus_present_headline))
)
# Melt the data for plotting
library(reshape2)
melted_focus_data <- melt(focus_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Load required libraries
library(ggplot2)
library(ggeffects)
# Define custom colors for visual distinction
custom_colors_focus <- c("Focus Future" = "#122740",
                         "Focus Past" = "#b5d1ae",
                         "Focus Present" = "#326677")
# Ensure that the categories are in the correct order
melted_focus_data$Category <- factor(melted_focus_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Adjust the 'Type' factor to specify the order of levels directly
melted_focus_data$Type <- factor(melted_focus_data$Type, levels = c("Focus Past", "Focus Present", "Focus Future"))
# Update facet labels to be more descriptive
updated_labels <- c(Frequency_Status="Status Message", Frequency_Headline="Headline")
# Create the plot
focus_plot <- ggplot(melted_focus_data, aes(x = Category, y = AverageValue, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~MessageType, scales = "free_y", labeller = labeller(MessageType = c(Frequency_Status = "Status Message", Frequency_Headline = "Headline"))) +
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors_focus) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
# Save the plot
ggsave("~/Desktop/plot/comparison_focus_time.png", plot = focus_plot, dpi = 300, width = 24, height = 20, units = "cm")



##########################################NEW focus future, past, present#############################################
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
# Assuming focus_data is already created and is correct
# Melt the data
melted_focus_data <- melt(focus_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Ensure the correct order of categories and types
melted_focus_data$Category <- factor(melted_focus_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
melted_focus_data$Type <- factor(melted_focus_data$Type, levels = c("Focus Past", "Focus Present", "Focus Future"))
melted_focus_data$MessageType <- factor(melted_focus_data$MessageType, levels = c("Frequency_Headline", "Frequency_Status"), labels = c("Headline", "Status Message"))
# Create a combined factor for fill mapping
melted_focus_data$Fill <- interaction(melted_focus_data$Type, melted_focus_data$MessageType, sep = ": ")
melted_focus_data$Fill <- with(melted_focus_data, interaction(Type, MessageType, sep = ": "))
melted_focus_data$Fill <- factor(
  melted_focus_data$Fill,
  levels = c(
    "Focus Past: Headline", "Focus Past: Status Message",
    "Focus Present: Headline", "Focus Present: Status Message",
    "Focus Future: Headline", "Focus Future: Status Message"
  )
)
# Define custom colors matching the exact interaction terms
custom_colors_focus <- c(
  "Focus Past: Headline" = "#568687",    # Light orange for headline
  "Focus Past: Status Message" = "#16485e", # Dark orange for status
  "Focus Present: Headline" = "#80ae9a",   # Light blue for headline
  "Focus Present: Status Message" = "#122740", # Dark blue for status
  "Focus Future: Headline" = "#b5d1ae",   # Light green for headline
  "Focus Future: Status Message" = "#326677"  # Dark green for status
)
# Plotting
focus_plot1 <- ggplot(melted_focus_data, aes(x = Category, y = AverageValue, fill = Fill)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_y") +
  labs(title = NULL, x = "Newspaper Category", y = "Average Value") +
  scale_fill_manual(values = custom_colors_focus, 
                    labels = c(
                      "Focus Past: Headline", "Focus Past: Status Message",
                      "Focus Present: Headline", "Focus Present: Status Message",
                      "Focus Future: Headline", "Focus Future: Status Message"
                    )) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12)) +
  guides(fill = guide_legend(title = "Focus and Message Type", 
                             reverse = FALSE, 
                             order = TRUE)) # Ensure grouping
# Display the plot
print(focus_plot1)
# Save the plot
ggsave("~/Desktop/plot/comparison_focus_time.png", plot = focus_plot1, dpi = 300, width = 24, height = 20, units = "cm")




###### Beta regression analysis setup for future, present and past ######
# Ensure data is appropriately scaled to fit beta regression requirements
total$focus_future1 <- (total$focusfuture - min(total$focusfuture)) / (max(total$focusfuture) - min(total$focusfuture))
total$focus_past1 <- (total$focuspast - min(total$focuspast)) / (max(total$focuspast) - min(total$focuspast))
total$focus_present1 <- (total$focuspresent - min(total$focuspresent)) / (max(total$focuspresent) - min(total$focuspresent))
# Adjust scales slightly to ensure all values are strictly between 0 and 1
total$focus_future1 <- 0.01 + 0.98 * total$focus_future1
total$focus_past1 <- 0.01 + 0.98 * total$focus_past1
total$focus_present1 <- 0.01 + 0.98 * total$focus_present1
# Define the model for future focus
mh2c <- glmmTMB(focus_future1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2c)
mh2c_g <- glmmTMB(focus_future1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2c_g)
anova_h2c <- anova(mh2c, mh2c_g)


# Create a table for the model mh2c_g
tab_model(mh2c_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2c.doc")
# Browse the created document
browseURL("model_results_h2c.doc")
# Convert the ANOVA results into a data frame
anova_table_h2c <- as.data.frame(anova_h2c)
anova_table_h2c$Model <- c("mh2c: adj1 ~ media * type", "mh2c_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2c <- anova_table_h2c[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2c <- read_docx()
# Create a table with flextable
anova_ft_h2c <- flextable(anova_table_h2c)
# Set the style of the table
anova_ft_h2c <- set_header_labels(anova_ft_h2c, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2c <- font(anova_ft_h2c, fontname = "Times New Roman", part = "all")
anova_ft_h2c <- fontsize(anova_ft_h2c, size = 12, part = "all")
anova_ft_h2c <- autofit(anova_ft_h2c)
# Add the table to the Word document
doc_h2c <- body_add_flextable(doc_h2c, value = anova_ft_h2c)
# Save the Word document
print(doc_h2c, target = "anova_results_h2c.docx")
# Browse the created document
browseURL("anova_results_h2c.docx")

# Define the model for past focus
mh2c_past <- glmmTMB(focus_past1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2c_past)
mh2c_past_g <- glmmTMB(focus_past1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2c_past_g)
anova_h2c_past <- anova(mh2c_past, mh2c_past_g)

# Create a table for the model mh2c_past_g
tab_model(mh2c_past, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2c_past.doc")
# Browse the created document
browseURL("model_results_h2c_past.doc")
# Convert the ANOVA results into a data frame
anova_table_h2c_past <- as.data.frame(anova_h2c_past)
anova_table_h2c_past$Model <- c("mh2c_past: adj1 ~ media * type", "mh2c_past_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2c_past <- anova_table_h2c_past[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2c_past <- read_docx()
# Create a table with flextable
anova_ft_h2c_past <- flextable(anova_table_h2c_past)
# Set the style of the table
anova_ft_h2c_past <- set_header_labels(anova_ft_h2c_past, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2c_past <- font(anova_ft_h2c_past, fontname = "Times New Roman", part = "all")
anova_ft_h2c_past <- fontsize(anova_ft_h2c_past, size = 12, part = "all")
anova_ft_h2c_past <- autofit(anova_ft_h2c_past)
# Add the table to the Word document
doc_h2c_past <- body_add_flextable(doc_h2c_past, value = anova_ft_h2c_past)
# Save the Word document
print(doc_h2c_past, target = "anova_results_h2c_past.docx")
# Browse the created document
browseURL("anova_results_h2c_past.docx")


# Define the model for present focus
mh2c_future <- glmmTMB(focus_present1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2c_future)
mh2c_future_g <- glmmTMB(focus_present1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2c_future_g)
anova_h2c_future <- anova(mh2c_future, mh2c_future_g)

# Create a table for the model mh2c_future_g
tab_model(mh2c_future, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2c_future.doc")
# Browse the created document
browseURL("model_results_h2c_future.doc")
# Convert the ANOVA results into a data frame
anova_table_h2c_future <- as.data.frame(anova_h2c_future)
anova_table_h2c_future$Model <- c("mh2c_future: adj1 ~ media * type", "mh2c_future_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2c_future <- anova_table_h2c_future[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2c_future <- read_docx()
# Create a table with flextable
anova_ft_h2c_future <- flextable(anova_table_h2c_future)
# Set the style of the table
anova_ft_h2c_future <- set_header_labels(anova_ft_h2c_future, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2c_future <- font(anova_ft_h2c_future, fontname = "Times New Roman", part = "all")
anova_ft_h2c_future <- fontsize(anova_ft_h2c_future, size = 12, part = "all")
anova_ft_h2c_future <- autofit(anova_ft_h2c_future)
# Add the table to the Word document
doc_h2c_future <- body_add_flextable(doc_h2c_future, value = anova_ft_h2c_future)
# Save the Word document
print(doc_h2c_future, target = "anova_results_h2c_future.docx")
# Browse the created document
browseURL("anova_results_h2c_future.docx")


#######h2d#######
#Status messages contain fewer conjunctions than titles and subtitles. (conj)

# Initialize lists to collect results
conjunction_frequency_status <- list()
conjunction_frequency_headline <- list()
# Loop to calculate the average frequency of conjunctions for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of conjunctions in status messages
  mean_conj_status <- mean(status_current$conj, na.rm = TRUE)
  conjunction_frequency_status[[i]] <- mean_conj_status
  # Calculate the average frequency of conjunctions in headlines
  mean_conj_headline <- mean(headlines_current$conj, na.rm = TRUE)
  conjunction_frequency_headline[[i]] <- mean_conj_headline
}
# Create a dataframe for the results
conjunction_frequency_df <- data.frame(
  Newspaper = newspapers,
  Conjunction_Frequency_Status = unlist(conjunction_frequency_status),
  Conjunction_Frequency_Headline = unlist(conjunction_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
conjunction_frequency_melted <- melt(conjunction_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Conjunctions")
# Display the original data frame to verify the inclusion of both types of data
print(conjunction_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(conjunction_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
conjunction_frequency_melted$Newspaper <- factor(conjunction_frequency_melted$Newspaper,
                                                 levels = c("Times", "Telegraph", "Guardian",
                                                            "Sun", "Daily Mail", "Daily Express",
                                                            "Economist", "Tribune", "Prospect"))
conjunction_frequency_melted$Type <- factor(conjunction_frequency_melted$Type, levels = c("Conjunction_Frequency_Headline", "Conjunction_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Conjunction_Frequency_Headline" = "#80ae9a", "Conjunction_Frequency_Status" = "#122740")
# Create bar plot
plot_h2d <- ggplot(conjunction_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Conjunctions`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Conjunctions") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2d.png", plot = plot_h2d, dpi = 300, width = 20, height = 16, units = "cm")


# Initialize lists to collect results
conj_frequency_status <- list()
conj_frequency_headline <- list()
# Loop to calculate the average frequency of conjunctions for each newspaper
for (i in 1:9) {
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  mean_conj_status <- mean(as.numeric(status_current$conj), na.rm = TRUE)
  conj_frequency_status[[i]] <- mean_conj_status
  mean_conj_headline <- mean(as.numeric(headlines_current$conj), na.rm = TRUE)
  conj_frequency_headline[[i]] <- mean_conj_headline
}
# Create a dataframe for the results
conj_frequency_df <- data.frame(Newspaper = newspapers, Conj_Frequency_Status = unlist(conj_frequency_status), Conj_Frequency_Headline = unlist(conj_frequency_headline))
# Create a new column for newspaper categories
conj_frequency_df$Category <- ifelse(conj_frequency_df$Newspaper %in% c("Times", "Telegraph", "Guardian"), "Broadsheets", 
                                     ifelse(conj_frequency_df$Newspaper %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines"))
# Ensure the category ordering
conj_frequency_df$Category <- factor(conj_frequency_df$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Aggregate data by category
aggregated_data_conj <- aggregate(cbind(Conj_Frequency_Status, Conj_Frequency_Headline) ~ Category, data = conj_frequency_df, FUN = mean, na.rm = TRUE)
# Melt the aggregated data for plotting
if (!require(reshape2)) {install.packages("reshape2")}
library(reshape2)
aggregated_melted_conj <- melt(aggregated_data_conj, id.vars = "Category", variable.name = "Type", value.name = "Average Frequency of Conjunctions")
# Adjust Type factor levels to control plot order
aggregated_melted_conj$Type <- factor(aggregated_melted_conj$Type, levels = c("Conj_Frequency_Headline", "Conj_Frequency_Status"))
# Load required libraries
library(ggplot2)
custom_colors_conj <- c("Conj_Frequency_Headline" = "#80ae9a", "Conj_Frequency_Status" = "#122740")
# Create bar plot
plot_h2d_aggregated <- ggplot(aggregated_melted_conj, aes(x = Category, y = `Average Frequency of Conjunctions`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Conjunctions") +
  scale_fill_manual(values = custom_colors_conj, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2d_aggregated.png", plot = plot_h2d_aggregated, dpi = 300, width = 20, height = 16, units = "cm")


###### Beta regression for conj (h2d) ######
# Histogram to examine distribution
hist(total$conj)
# Boxplot to look at relationship with predictor variables
bwplot(conj ~ media | type, total)
# Scale conj to be between 0 and 1 and adjust for beta regression requirements
total$conj1 <- (total$conj / 30) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2d <- glmmTMB(conj1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2d)
# Run the beta regression model without interaction
mh2d_g <- glmmTMB(conj1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2d_g)
# Compare the two models
anova_h2d <- anova(mh2d, mh2d_g)


# Create a table for the model mh2d_g
tab_model(mh2d, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2d.doc")
# Browse the created document
browseURL("model_results_h2d.doc")
# Convert the ANOVA results into a data frame
anova_table_h2d <- as.data.frame(anova_h2d)
anova_table_h2d$Model <- c("mh2d: adj1 ~ media * type", "mh2d_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2d <- anova_table_h2d[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2d <- read_docx()
# Create a table with flextable
anova_ft_h2d <- flextable(anova_table_h2d)
# Set the style of the table
anova_ft_h2d <- set_header_labels(anova_ft_h2d, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2d <- font(anova_ft_h2d, fontname = "Times New Roman", part = "all")
anova_ft_h2d <- fontsize(anova_ft_h2d, size = 12, part = "all")
anova_ft_h2d <- autofit(anova_ft_h2d)
# Add the table to the Word document
doc_h2d <- body_add_flextable(doc_h2d, value = anova_ft_h2d)
# Save the Word document
print(doc_h2d, target = "anova_results_h2d.docx")
# Browse the created document
browseURL("anova_results_h2d.docx")


# Visualize model predictions
library(ggeffects)
pred_h2d <- ggpredict(mh2d_g, terms = c("media", "type"))
h2d_plot_pred <- plot(pred_h2d, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2d_plot_pred.png", plot = h2d_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



#######h2e#######
#Status messages have a lower level of cognitive processes than titles and subtitles.(cogproc)

# Initialize lists to collect results
cognitive_process_status <- list()
cognitive_process_headline <- list()
# Loop to calculate the average level of cognitive processes for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average level of cognitive processes in status messages
  mean_cogproc_status <- mean(status_current$cogproc, na.rm = TRUE)
  cognitive_process_status[[i]] <- mean_cogproc_status
  # Calculate the average level of cognitive processes in headlines
  mean_cogproc_headline <- mean(headlines_current$cogproc, na.rm = TRUE)
  cognitive_process_headline[[i]] <- mean_cogproc_headline
}
# Create a dataframe for the results
cognitive_process_df <- data.frame(
  Newspaper = newspapers,
  Cognitive_Process_Status = unlist(cognitive_process_status),
  Cognitive_Process_Headline = unlist(cognitive_process_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
cognitive_process_melted <- melt(cognitive_process_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Cognitive Processes")
# Display the original data frame to verify the inclusion of both types of data
print(cognitive_process_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(cognitive_process_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
cognitive_process_melted$Newspaper <- factor(cognitive_process_melted$Newspaper,
                                             levels = c("Times", "Telegraph", "Guardian",
                                                        "Sun", "Daily Mail", "Daily Express",
                                                        "Economist", "Tribune", "Prospect"))
cognitive_process_melted$Type <- factor(cognitive_process_melted$Type, levels = c("Cognitive_Process_Headline", "Cognitive_Process_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Cognitive_Process_Headline" = "#80ae9a", "Cognitive_Process_Status" = "#122740")
# Create bar plot
plot_h2e <- ggplot(cognitive_process_melted, aes(x = Newspaper, y = `Average Frequency of Cognitive Processes`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Cognitive Processes") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2e.png", plot = plot_h2e, dpi = 300, width = 20, height = 16, units = "cm")



#Initialize lists to collect results
cogproc_frequency_status<-list()
cogproc_frequency_headline<-list()
#Loop to calculate the average frequency of cognitive processes for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_cogproc_status<-mean(as.numeric(status_current$cogproc),na.rm=TRUE)
  cogproc_frequency_status[[i]]<-mean_cogproc_status
  mean_cogproc_headline<-mean(as.numeric(headlines_current$cogproc),na.rm=TRUE)
  cogproc_frequency_headline[[i]]<-mean_cogproc_headline
}
#Create a dataframe for the results
cogproc_frequency_df<-data.frame(Newspaper=newspapers,CogProc_Frequency_Status=unlist(cogproc_frequency_status),CogProc_Frequency_Headline=unlist(cogproc_frequency_headline))
#Create a new column for newspaper categories
cogproc_frequency_df$Category<-ifelse(cogproc_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(cogproc_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
cogproc_frequency_df$Category<-factor(cogproc_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_cogproc<-aggregate(cbind(CogProc_Frequency_Status,CogProc_Frequency_Headline)~Category,data=cogproc_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_cogproc<-melt(aggregated_data_cogproc,id.vars="Category",variable.name="Type",value.name="Average Frequency of Cognitive Processes")
#Adjust Type factor levels to control plot order
aggregated_melted_cogproc$Type<-factor(aggregated_melted_cogproc$Type,levels=c("CogProc_Frequency_Headline","CogProc_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_cogproc<-c("CogProc_Frequency_Headline"="#80ae9a","CogProc_Frequency_Status"="#122740")
#Create bar plot
plot_h2e_aggregated <- ggplot(aggregated_melted_cogproc, aes(x = Category, y = `Average Frequency of Cognitive Processes`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Cognitive Processes") +
  scale_fill_manual(values = custom_colors_cogproc, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2e_aggregated.png",plot=plot_h2e_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for cogproc (h2e) ######
# Histogram to examine distribution
hist(total$cogproc)
# Boxplot to look at relationship with predictor variables
bwplot(cogproc ~ media | type, total)
# Scale cogproc to be between 0 and 1 and adjust for beta regression requirements
total$cogproc1 <- (total$cogproc / 50) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2e <- glmmTMB(cogproc1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2e)
# Run the beta regression model without interaction
mh2e_g <- glmmTMB(cogproc1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2e_g)
# Compare the two models
anova_h2e <- anova(mh2e, mh2e_g)


# Create a table for the model mh2e_g
tab_model(mh2e, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2e.doc")
# Browse the created document
browseURL("model_results_h2e.doc")
# Convert the ANOVA results into a data frame
anova_table_h2e <- as.data.frame(anova_h2e)
anova_table_h2e$Model <- c("mh2e: adj1 ~ media * type", "mh2e_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2e <- anova_table_h2e[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2e <- read_docx()
# Create a table with flextable
anova_ft_h2e <- flextable(anova_table_h2e)
# Set the style of the table
anova_ft_h2e <- set_header_labels(anova_ft_h2e, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2e <- font(anova_ft_h2e, fontname = "Times New Roman", part = "all")
anova_ft_h2e <- fontsize(anova_ft_h2e, size = 12, part = "all")
anova_ft_h2e <- autofit(anova_ft_h2e)
# Add the table to the Word document
doc_h2e <- body_add_flextable(doc_h2e, value = anova_ft_h2e)
# Save the Word document
print(doc_h2e, target = "anova_results_h2e.docx")
# Browse the created document
browseURL("anova_results_h2e.docx")




# Visualize model predictions
library(ggeffects)
pred_h2e <- ggpredict(mh2e_g, terms = c("media", "type"))
h2e_plot_pred <- plot(pred_h2e, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2e_plot_pred.png", plot = h2e_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2e_cognition#######
#Status messages have a lower level of cognitive processes than titles and subtitles.(Cognition)

# Initialize lists to collect results
cognitive_process_status <- list()
cognitive_process_headline <- list()
# Loop to calculate the average level of cognitive processes for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average level of cognitive processes in status messages
  mean_cognition_status <- mean(status_current$Cognition, na.rm = TRUE)
  cognitive_process_status[[i]] <- mean_cognition_status
  # Calculate the average level of cognitive processes in headlines
  mean_cognition_headline <- mean(headlines_current$Cognition, na.rm = TRUE)
  cognitive_process_headline[[i]] <- mean_cognition_headline
}
# Create a dataframe for the results
cognitive_process_df <- data.frame(
  Newspaper = newspapers,
  Cognitive_Process_Status = unlist(cognitive_process_status),
  Cognitive_Process_Headline = unlist(cognitive_process_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
cognitive_process_melted <- melt(cognitive_process_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Level of Cognition")
# Display the original data frame to verify the inclusion of both types of data
print(cognitive_process_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(cognitive_process_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
cognitive_process_melted$Newspaper <- factor(cognitive_process_melted$Newspaper,
                                             levels = c("Times", "Telegraph", "Guardian",
                                                        "Sun", "Daily Mail", "Daily Express",
                                                        "Economist", "Tribune", "Prospect"))
cognitive_process_melted$Type <- factor(cognitive_process_melted$Type, levels = c("Cognitive_Process_Headline", "Cognitive_Process_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Cognitive_Process_Headline" = "#80ae9a", "Cognitive_Process_Status" = "#122740")
# Create bar plot
plot_h2e_cognition <- ggplot(cognitive_process_melted, aes(x = Newspaper, y = `Average Level of Cognition`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Level of Cognition") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2e_cognition.png", plot = plot_h2e_cognition, dpi = 300, width = 20, height = 16, units = "cm")

#Initialize lists to collect results
cognition_frequency_status<-list()
cognition_frequency_headline<-list()
#Loop to calculate the average frequency of Cognition for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_cognition_status<-mean(as.numeric(status_current$Cognition),na.rm=TRUE)
  cognition_frequency_status[[i]]<-mean_cognition_status
  mean_cognition_headline<-mean(as.numeric(headlines_current$Cognition),na.rm=TRUE)
  cognition_frequency_headline[[i]]<-mean_cognition_headline
}
#Create a dataframe for the results
cognition_frequency_df<-data.frame(Newspaper=newspapers,Cognition_Frequency_Status=unlist(cognition_frequency_status),Cognition_Frequency_Headline=unlist(cognition_frequency_headline))
#Create a new column for newspaper categories
cognition_frequency_df$Category<-ifelse(cognition_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(cognition_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
cognition_frequency_df$Category<-factor(cognition_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_cognition<-aggregate(cbind(Cognition_Frequency_Status,Cognition_Frequency_Headline)~Category,data=cognition_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_cognition<-melt(aggregated_data_cognition,id.vars="Category",variable.name="Type",value.name="Average Level of Cognition")
#Adjust Type factor levels to control plot order
aggregated_melted_cognition$Type<-factor(aggregated_melted_cognition$Type,levels=c("Cognition_Frequency_Headline","Cognition_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_cognition<-c("Cognition_Frequency_Headline"="#80ae9a","Cognition_Frequency_Status"="#122740")
#Create bar plot
plot_h2e_cognition_aggregated <- ggplot(aggregated_melted_cognition, aes(x = Category, y = `Average Level of Cognition`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Level of Cognition") +
  scale_fill_manual(values = custom_colors_cognition, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set global font family to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2e_cognition_aggregated.png",plot=plot_h2e_cognition_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for Cognition (h2e_cognition) ######
# Histogram to examine distribution
hist(total$Cognition)
# Boxplot to look at relationship with predictor variables
bwplot(Cognition ~ media | type, total)
# Scale Cognition to be between 0 and 1 and adjust for beta regression requirements
total$Cognition1 <- (total$Cognition / 50) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2e_cognition <- glmmTMB(Cognition1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2e_cognition)
# Run the beta regression model without interaction
mh2e_cognition_g <- glmmTMB(Cognition1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2e_cognition_g)
# Compare the two models
anova(mh2e_cognition, mh2e_cognition_g)



# Visualize model predictions
library(ggeffects)
pred_h2e_cognition <- ggpredict(mh2e_cognition_g, terms = c("media", "type"))
h2e_cognition_plot_pred <- plot(pred_h2e_cognition, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2e_cognition_plot_pred.png", plot = h2e_cognition_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2e_all#######
#Status messages have a higher level of all-none referents than titles and subtitles.(allnone)

# Initialize lists to collect results
allnone_frequency_status <- list()
allnone_frequency_headline <- list()
# Loop to calculate the average level of all-none referents for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average level of all-none referents in status messages
  mean_allnone_status <- mean(status_current$allnone, na.rm = TRUE)
  allnone_frequency_status[[i]] <- mean_allnone_status
  # Calculate the average level of all-none referents in headlines
  mean_allnone_headline <- mean(headlines_current$allnone, na.rm = TRUE)
  allnone_frequency_headline[[i]] <- mean_allnone_headline
}
# Create a dataframe for the results
allnone_frequency_df <- data.frame(
  Newspaper = newspapers,
  All_None_Frequency_Status = unlist(allnone_frequency_status),
  All_None_Frequency_Headline = unlist(allnone_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
allnone_frequency_melted <- melt(allnone_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of All-None Referents")
# Display the original data frame to verify the inclusion of both types of data
print(allnone_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(allnone_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
allnone_frequency_melted$Newspaper <- factor(allnone_frequency_melted$Newspaper,
                                             levels = c("Times", "Telegraph", "Guardian",
                                                        "Sun", "Daily Mail", "Daily Express",
                                                        "Economist", "Tribune", "Prospect"))
allnone_frequency_melted$Type <- factor(allnone_frequency_melted$Type, levels = c("All_None_Frequency_Headline", "All_None_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("All_None_Frequency_Headline" = "#80ae9a", "All_None_Frequency_Status" = "#122740")
# Create bar plot
plot_h2e_all <- ggplot(allnone_frequency_melted, aes(x = Newspaper, y = `Average Frequency of All-None Referents`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of All-None Referents") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2e_all.png", plot = plot_h2e_all, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
allnone_frequency_status<-list()
allnone_frequency_headline<-list()
#Loop to calculate the average frequency of allnone for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_allnone_status<-mean(as.numeric(status_current$allnone),na.rm=TRUE)
  allnone_frequency_status[[i]]<-mean_allnone_status
  mean_allnone_headline<-mean(as.numeric(headlines_current$allnone),na.rm=TRUE)
  allnone_frequency_headline[[i]]<-mean_allnone_headline
}
#Create a dataframe for the results
allnone_frequency_df<-data.frame(Newspaper=newspapers,AllNone_Frequency_Status=unlist(allnone_frequency_status),AllNone_Frequency_Headline=unlist(allnone_frequency_headline))
#Create a new column for newspaper categories
allnone_frequency_df$Category<-ifelse(allnone_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(allnone_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
allnone_frequency_df$Category<-factor(allnone_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_allnone<-aggregate(cbind(AllNone_Frequency_Status,AllNone_Frequency_Headline)~Category,data=allnone_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_allnone<-melt(aggregated_data_allnone,id.vars="Category",variable.name="Type",value.name="Average Frequency of AllNone")
#Adjust Type factor levels to control plot order
aggregated_melted_allnone$Type<-factor(aggregated_melted_allnone$Type,levels=c("AllNone_Frequency_Headline","AllNone_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_allnone<-c("AllNone_Frequency_Headline"="#80ae9a","AllNone_Frequency_Status"="#122740")
#Create bar plot
plot_h2e_all_aggregated <- ggplot(aggregated_melted_allnone, aes(x = Category, y = `Average Frequency of AllNone`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of All-None Referents") +
  scale_fill_manual(values = custom_colors_allnone, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2e_all_aggregated.png",plot=plot_h2e_all_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for allnone (h2e_all) ######
# Histogram to examine distribution
hist(total$allnone)
# Boxplot to look at relationship with predictor variables
bwplot(allnone ~ media | type, total)
# Scale allnone to be between 0 and 1 and adjust for beta regression requirements
total$allnone1 <- (total$allnone / 25) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2e_all <- glmmTMB(allnone1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2e_all)
# Run the beta regression model without interaction
mh2e_all_g <- glmmTMB(allnone1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2e_all_g)
# Compare the two models
anova(mh2e_all, mh2e_all_g)
# Visualize model predictions
library(ggeffects)
pred_h2e_all <- ggpredict(mh2e_all_g, terms = c("media", "type"))
h2e_all_plot_pred <- plot(pred_h2e_all, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2e_all_plot_pred.png", plot = h2e_all_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2f#######
#Status messages present a lower level of analytical thinking than titles and subtitles.(Analytic)

# Initialize lists to collect results
analytical_thinking_status <- list()
analytical_thinking_headline <- list()
# Loop to calculate the average level of analytical thinking for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average level of analytical thinking in status messages
  mean_analytic_status <- mean(status_current$Analytic, na.rm = TRUE)
  analytical_thinking_status[[i]] <- mean_analytic_status
  # Calculate the average level of analytical thinking in headlines
  mean_analytic_headline <- mean(headlines_current$Analytic, na.rm = TRUE)
  analytical_thinking_headline[[i]] <- mean_analytic_headline
}
# Create a dataframe for the results
analytical_thinking_df <- data.frame(
  Newspaper = newspapers,
  Analytical_Thinking_Status = unlist(analytical_thinking_status),
  Analytical_Thinking_Headline = unlist(analytical_thinking_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
analytical_thinking_melted <- melt(analytical_thinking_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Level of Analytical Thinking")
# Display the original data frame to verify the inclusion of both types of data
print(analytical_thinking_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(analytical_thinking_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
analytical_thinking_melted$Newspaper <- factor(analytical_thinking_melted$Newspaper,
                                               levels = c("Times", "Telegraph", "Guardian",
                                                          "Sun", "Daily Mail", "Daily Express",
                                                          "Economist", "Tribune", "Prospect"))
analytical_thinking_melted$Type <- factor(analytical_thinking_melted$Type, levels = c("Analytical_Thinking_Headline", "Analytical_Thinking_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Analytical_Thinking_Headline" = "#80ae9a", "Analytical_Thinking_Status" = "#122740")
# Create bar plot
plot_h2f <- ggplot(analytical_thinking_melted, aes(x = Newspaper, y = `Average Level of Analytical Thinking`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Level of Analytical Thinking") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2f.png", plot = plot_h2f, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
analytic_frequency_status<-list()
analytic_frequency_headline<-list()
#Loop to calculate the average frequency of Analytic for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_analytic_status<-mean(as.numeric(status_current$Analytic),na.rm=TRUE)
  analytic_frequency_status[[i]]<-mean_analytic_status
  mean_analytic_headline<-mean(as.numeric(headlines_current$Analytic),na.rm=TRUE)
  analytic_frequency_headline[[i]]<-mean_analytic_headline
}
#Create a dataframe for the results
analytic_frequency_df<-data.frame(Newspaper=newspapers,Analytic_Frequency_Status=unlist(analytic_frequency_status),Analytic_Frequency_Headline=unlist(analytic_frequency_headline))
#Create a new column for newspaper categories
analytic_frequency_df$Category<-ifelse(analytic_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(analytic_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
analytic_frequency_df$Category<-factor(analytic_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_analytic<-aggregate(cbind(Analytic_Frequency_Status,Analytic_Frequency_Headline)~Category,data=analytic_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_analytic<-melt(aggregated_data_analytic,id.vars="Category",variable.name="Type",value.name="Average Frequency of Analytic")
#Adjust Type factor levels to control plot order
aggregated_melted_analytic$Type<-factor(aggregated_melted_analytic$Type,levels=c("Analytic_Frequency_Headline","Analytic_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_analytic<-c("Analytic_Frequency_Headline"="#80ae9a","Analytic_Frequency_Status"="#122740")
#Create bar plot
plot_h2f_aggregated <- ggplot(aggregated_melted_analytic, aes(x = Category, y = `Average Frequency of Analytic`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Level of Analytical Thinking") +
  scale_fill_manual(values = custom_colors_analytic, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2f_aggregated.png",plot=plot_h2f_aggregated,dpi=300,width=20,height=16,units="cm")

###### Beta regression for Analytic (h2f) ######
# Histogram to examine distribution
hist(total$Analytic)
# Boxplot to look at relationship with predictor variables
bwplot(Analytic ~ media | type, total)
# Scale Analytic to be between 0 and 1 and adjust for beta regression requirements
total$Analytic1 <- (total$Analytic / 100) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2f <- glmmTMB(Analytic1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2f)
# Run the beta regression model without interaction
mh2f_g <- glmmTMB(Analytic1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2f_g)
# Compare the two models
anova_h2f <- anova(mh2f, mh2f_g)


# Create a table for the model mh2f_g
tab_model(mh2f_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2f.doc")
# Browse the created document
browseURL("model_results_h2f.doc")
# Convert the ANOVA results into a data frame
anova_table_h2f <- as.data.frame(anova_h2f)
anova_table_h2f$Model <- c("mh2f: adj1 ~ media * type", "mh2f_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2f <- anova_table_h2f[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2f <- read_docx()
# Create a table with flextable
anova_ft_h2f <- flextable(anova_table_h2f)
# Set the style of the table
anova_ft_h2f <- set_header_labels(anova_ft_h2f, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2f <- font(anova_ft_h2f, fontname = "Times New Roman", part = "all")
anova_ft_h2f <- fontsize(anova_ft_h2f, size = 12, part = "all")
anova_ft_h2f <- autofit(anova_ft_h2f)
# Add the table to the Word document
doc_h2f <- body_add_flextable(doc_h2f, value = anova_ft_h2f)
# Save the Word document
print(doc_h2f, target = "anova_results_h2f.docx")
# Browse the created document
browseURL("anova_results_h2f.docx")



# Visualize model predictions
library(ggeffects)
pred_h2f <- ggpredict(mh2f_g, terms = c("media", "type"))
h2f_plot_pred <- plot(pred_h2f, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2f_plot_pred.png", plot = h2f_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2g#######
#Status messages present more determiners than titles and subtitles.(det)

# Initialize lists to collect results
determiner_frequency_status <- list()
determiner_frequency_headline <- list()
# Loop to calculate the average frequency of determiners for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of determiners in status messages
  mean_det_status <- mean(status_current$det, na.rm = TRUE)
  determiner_frequency_status[[i]] <- mean_det_status
  # Calculate the average frequency of determiners in headlines
  mean_det_headline <- mean(headlines_current$det, na.rm = TRUE)
  determiner_frequency_headline[[i]] <- mean_det_headline
}
# Create a dataframe for the results
determiner_frequency_df <- data.frame(
  Newspaper = newspapers,
  Determiner_Frequency_Status = unlist(determiner_frequency_status),
  Determiner_Frequency_Headline = unlist(determiner_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
determiner_frequency_melted <- melt(determiner_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Determiners")
# Display the original data frame to verify the inclusion of both types of data
print(determiner_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(determiner_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
determiner_frequency_melted$Newspaper <- factor(determiner_frequency_melted$Newspaper,
                                                levels = c("Times", "Telegraph", "Guardian",
                                                           "Sun", "Daily Mail", "Daily Express",
                                                           "Economist", "Tribune", "Prospect"))
determiner_frequency_melted$Type <- factor(determiner_frequency_melted$Type, levels = c("Determiner_Frequency_Headline", "Determiner_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Determiner_Frequency_Headline" = "#80ae9a", "Determiner_Frequency_Status" = "#122740")
# Create bar plot
plot_h2g <- ggplot(determiner_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Determiners`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Determiners") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2g.png", plot = plot_h2g, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
det_frequency_status<-list()
det_frequency_headline<-list()
#Loop to calculate the average frequency of determiners for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_det_status<-mean(as.numeric(status_current$det),na.rm=TRUE)
  det_frequency_status[[i]]<-mean_det_status
  mean_det_headline<-mean(as.numeric(headlines_current$det),na.rm=TRUE)
  det_frequency_headline[[i]]<-mean_det_headline
}
#Create a dataframe for the results
det_frequency_df<-data.frame(Newspaper=newspapers,Det_Frequency_Status=unlist(det_frequency_status),Det_Frequency_Headline=unlist(det_frequency_headline))
#Create a new column for newspaper categories
det_frequency_df$Category<-ifelse(det_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(det_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
det_frequency_df$Category<-factor(det_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_det<-aggregate(cbind(Det_Frequency_Status,Det_Frequency_Headline)~Category,data=det_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_det<-melt(aggregated_data_det,id.vars="Category",variable.name="Type",value.name="Average Frequency of Determiners")
#Adjust Type factor levels to control plot order
aggregated_melted_det$Type<-factor(aggregated_melted_det$Type,levels=c("Det_Frequency_Headline","Det_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_det<-c("Det_Frequency_Headline"="#80ae9a","Det_Frequency_Status"="#122740")
#Create bar plot
plot_h2g_aggregated <- ggplot(aggregated_melted_det, aes(x = Category, y = `Average Frequency of Determiners`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Determiners") +
  scale_fill_manual(values = custom_colors_det, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2g_aggregated.png",plot=plot_h2g_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for det (h2g) ######
# Histogram to examine distribution
hist(total$det)
# Boxplot to look at relationship with predictor variables
bwplot(det ~ media | type, total)
# Scale det to be between 0 and 1 and adjust for beta regression requirements
total$det1 <- (total$det / 40) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2g <- glmmTMB(det1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2g)
# Run the beta regression model without interaction
mh2g_g <- glmmTMB(det1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2g_g)
# Compare the two models
anova_h2g <- anova(mh2g, mh2g_g)


# Create a table for the model mh2g_g
tab_model(mh2g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2g.doc")
# Browse the created document
browseURL("model_results_h2g.doc")
# Convert the ANOVA results into a data frame
anova_table_h2g <- as.data.frame(anova_h2g)
anova_table_h2g$Model <- c("mh2g: adj1 ~ media * type", "mh2g_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2g <- anova_table_h2g[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2g <- read_docx()
# Create a table with flextable
anova_ft_h2g <- flextable(anova_table_h2g)
# Set the style of the table
anova_ft_h2g <- set_header_labels(anova_ft_h2g, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2g <- font(anova_ft_h2g, fontname = "Times New Roman", part = "all")
anova_ft_h2g <- fontsize(anova_ft_h2g, size = 12, part = "all")
anova_ft_h2g <- autofit(anova_ft_h2g)
# Add the table to the Word document
doc_h2g <- body_add_flextable(doc_h2g, value = anova_ft_h2g)
# Save the Word document
print(doc_h2g, target = "anova_results_h2g.docx")
# Browse the created document
browseURL("anova_results_h2g.docx")



# Visualize model predictions
library(ggeffects)
pred_h2g <- ggpredict(mh2g_g, terms = c("media", "type"))
h2g_plot_pred <- plot(pred_h2g, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2g_plot_pred.png", plot = h2g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2h#######
#Status messages have more exclamation marks than titles and subtitles.(Exclam)

# Initialize lists to collect results
exclamation_frequency_status <- list()
exclamation_frequency_headline <- list()
# Loop to calculate the average frequency of exclamation marks for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of exclamation marks in status messages
  mean_exclam_status <- mean(status_current$Exclam, na.rm = TRUE)
  exclamation_frequency_status[[i]] <- mean_exclam_status
  # Calculate the average frequency of exclamation marks in headlines
  mean_exclam_headline <- mean(headlines_current$Exclam, na.rm = TRUE)
  exclamation_frequency_headline[[i]] <- mean_exclam_headline
}
# Create a dataframe for the results
exclamation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Exclamation_Frequency_Status = unlist(exclamation_frequency_status),
  Exclamation_Frequency_Headline = unlist(exclamation_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
exclamation_frequency_melted <- melt(exclamation_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Exclamation Marks")
# Display the original data frame to verify the inclusion of both types of data
print(exclamation_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(exclamation_frequency_melted, 18))

#h1d
# Initialize lists to collect results
exclamation_frequency_status <- list()
exclamation_frequency_headline <- list()
# Loop to calculate the average frequency of exclamation marks for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of exclamation marks in status messages
  mean_exclam_status <- mean(status_current$Exclam, na.rm = TRUE)
  exclamation_frequency_status[[i]] <- mean_exclam_status
  # Calculate the average frequency of exclamation marks in headlines
  mean_exclam_headline <- mean(headlines_current$Exclam, na.rm = TRUE)
  exclamation_frequency_headline[[i]] <- mean_exclam_headline
}
# Create a dataframe for the results
exclamation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Exclamation_Frequency_Status = unlist(exclamation_frequency_status),
  Exclamation_Frequency_Headline = unlist(exclamation_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
exclamation_frequency_melted <- melt(exclamation_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Exclamation Marks")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
exclamation_frequency_melted$Newspaper <- factor(exclamation_frequency_melted$Newspaper,
                                                 levels = c("Times", "Telegraph", "Guardian",
                                                            "Sun", "Daily Mail", "Daily Express",
                                                            "Economist", "Tribune", "Prospect"))
exclamation_frequency_melted$Type <- factor(exclamation_frequency_melted$Type, levels = c("Exclamation_Frequency_Headline", "Exclamation_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Exclamation_Frequency_Headline" = "#85C1E9", "Exclamation_Frequency_Status" = "#E59866")
# Create bar plot
plot_h2h <- ggplot(exclamation_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Exclamation Marks`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Frequency of Exclamation Marks: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Average Frequency of Exclamation Marks") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h2h.png", plot = plot_h2h, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
exclam_frequency_status<-list()
exclam_frequency_headline<-list()
#Loop to calculate the average frequency of Exclam for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_exclam_status<-mean(as.numeric(status_current$Exclam),na.rm=TRUE)
  exclam_frequency_status[[i]]<-mean_exclam_status
  mean_exclam_headline<-mean(as.numeric(headlines_current$Exclam),na.rm=TRUE)
  exclam_frequency_headline[[i]]<-mean_exclam_headline
}
#Create a dataframe for the results
exclam_frequency_df<-data.frame(Newspaper=newspapers,Exclam_Frequency_Status=unlist(exclam_frequency_status),Exclam_Frequency_Headline=unlist(exclam_frequency_headline))
#Create a new column for newspaper categories
exclam_frequency_df$Category<-ifelse(exclam_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(exclam_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
exclam_frequency_df$Category<-factor(exclam_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_exclam<-aggregate(cbind(Exclam_Frequency_Status,Exclam_Frequency_Headline)~Category,data=exclam_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_exclam<-melt(aggregated_data_exclam,id.vars="Category",variable.name="Type",value.name="Average Frequency of Exclam")
#Adjust Type factor levels to control plot order
aggregated_melted_exclam$Type<-factor(aggregated_melted_exclam$Type,levels=c("Exclam_Frequency_Headline","Exclam_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_exclam<-c("Exclam_Frequency_Headline"="#85C1E9","Exclam_Frequency_Status"="#E59866")
#Create bar plot
plot_h2h_aggregated<-ggplot(aggregated_melted_exclam,aes(x=Category,y=`Average Frequency of Exclam`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of Exclamations: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of Exclamations")+scale_fill_manual(values=custom_colors_exclam,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h2h_aggregated.png",plot=plot_h2h_aggregated,dpi=300,width=22,height=18,units="cm")

###### Beta regression for Exclam (h2h) ######
# Histogram to examine distribution
hist(total$Exclam)
# Boxplot to look at relationship with predictor variables
bwplot(Exclam ~ media | type, total)
# Scale Exclam to be between 0 and 1 and adjust for beta regression requirements
total$Exclam1 <- (total$Exclam / 100) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2h <- glmmTMB(Exclam1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2h)
# Run the beta regression model without interaction
mh2h_g <- glmmTMB(Exclam1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2h_g)
# Compare the two models
anova(mh2h, mh2h_g)
# Visualize model predictions
library(ggeffects)
pred_h2h <- ggpredict(mh2h_g, terms = c("media", "type"))
h2h_plot_pred <- plot(pred_h2h, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2h_plot_pred.png", plot = h2h_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2i#######
#Status messages have more quantifiers than titles and subtitles.(quantity)

# Initialize lists to collect results
quantifier_frequency_status <- list()
quantifier_frequency_headline <- list()
# Loop to calculate the average frequency of quantifiers for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of quantifiers in status messages
  mean_quantity_status <- mean(status_current$quantity, na.rm = TRUE)
  quantifier_frequency_status[[i]] <- mean_quantity_status
  # Calculate the average frequency of quantifiers in headlines
  mean_quantity_headline <- mean(headlines_current$quantity, na.rm = TRUE)
  quantifier_frequency_headline[[i]] <- mean_quantity_headline
}
# Create a dataframe for the results
quantifier_frequency_df <- data.frame(
  Newspaper = newspapers,
  Quantifier_Frequency_Status = unlist(quantifier_frequency_status),
  Quantifier_Frequency_Headline = unlist(quantifier_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
quantifier_frequency_melted <- melt(quantifier_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Quantifiers")
# Display the original data frame to verify the inclusion of both types of data
print(quantifier_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(quantifier_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
quantifier_frequency_melted$Newspaper <- factor(quantifier_frequency_melted$Newspaper,
                                                levels = c("Times", "Telegraph", "Guardian",
                                                           "Sun", "Daily Mail", "Daily Express",
                                                           "Economist", "Tribune", "Prospect"))
quantifier_frequency_melted$Type <- factor(quantifier_frequency_melted$Type, levels = c("Quantifier_Frequency_Headline", "Quantifier_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Quantifier_Frequency_Headline" = "#80ae9a", "Quantifier_Frequency_Status" = "#122740")
# Create bar plot
plot_h2i <- ggplot(quantifier_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Quantifiers`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Quantifiers") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2i.png", plot = plot_h2i, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
quantity_frequency_status<-list()
quantity_frequency_headline<-list()
#Loop to calculate the average frequency of quantity for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_quantity_status<-mean(as.numeric(status_current$quantity),na.rm=TRUE)
  quantity_frequency_status[[i]]<-mean_quantity_status
  mean_quantity_headline<-mean(as.numeric(headlines_current$quantity),na.rm=TRUE)
  quantity_frequency_headline[[i]]<-mean_quantity_headline
}
#Create a dataframe for the results
quantity_frequency_df<-data.frame(Newspaper=newspapers,Quantity_Frequency_Status=unlist(quantity_frequency_status),Quantity_Frequency_Headline=unlist(quantity_frequency_headline))
#Create a new column for newspaper categories
quantity_frequency_df$Category<-ifelse(quantity_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(quantity_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
quantity_frequency_df$Category<-factor(quantity_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_quantity<-aggregate(cbind(Quantity_Frequency_Status,Quantity_Frequency_Headline)~Category,data=quantity_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_quantity<-melt(aggregated_data_quantity,id.vars="Category",variable.name="Type",value.name="Average Frequency of Quantity")
#Adjust Type factor levels to control plot order
aggregated_melted_quantity$Type<-factor(aggregated_melted_quantity$Type,levels=c("Quantity_Frequency_Headline","Quantity_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_quantity<-c("Quantity_Frequency_Headline"="#80ae9a","Quantity_Frequency_Status"="#122740")
#Create bar plot
plot_h2i_aggregated <- ggplot(aggregated_melted_quantity, aes(x = Category, y = `Average Frequency of Quantity`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Quantifiers") +
  scale_fill_manual(values = custom_colors_quantity, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2i_aggregated.png",plot=plot_h2i_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for quantity (h2i) ######
# Histogram to examine distribution
hist(total$quantity)
# Boxplot to look at relationship with predictor variables
bwplot(quantity ~ media | type, total)
# Scale quantity to be between 0 and 1 and adjust for beta regression requirements
total$quantity1 <- (total$quantity / 35) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2i <- glmmTMB(quantity1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2i)
# Run the beta regression model without interaction
mh2i_g <- glmmTMB(quantity1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2i_g)
# Compare the two models
anova_h2i <- anova(mh2i, mh2i_g)
print(anova_h2i)

# Create a table for the model mh2i_g
tab_model(mh2i_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2i.doc")
# Browse the created document
browseURL("model_results_h2i.doc")
# Convert the ANOVA results into a data frame
anova_table_h2i <- as.data.frame(anova_h2i)
anova_table_h2i$Model <- c("mh2i: adj1 ~ media * type", "mh2i_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2i <- anova_table_h2i[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2i <- read_docx()
# Create a table with flextable
anova_ft_h2i <- flextable(anova_table_h2i)
# Set the style of the table
anova_ft_h2i <- set_header_labels(anova_ft_h2i, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2i <- font(anova_ft_h2i, fontname = "Times New Roman", part = "all")
anova_ft_h2i <- fontsize(anova_ft_h2i, size = 12, part = "all")
anova_ft_h2i <- autofit(anova_ft_h2i)
# Add the table to the Word document
doc_h2i <- body_add_flextable(doc_h2i, value = anova_ft_h2i)
# Save the Word document
print(doc_h2i, target = "anova_results_h2i.docx")
# Browse the created document
browseURL("anova_results_h2i.docx")



# Visualize model predictions
library(ggeffects)
pred_h2i <- ggpredict(mh2i_g, terms = c("media", "type"))
h2i_plot_pred <- plot(pred_h2i, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2i_plot_pred.png", plot = h2i_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



#######h2ii#######
#Status messages have more numbers than titles and subtitles.(number)

# Initialize lists to collect results
number_frequency_status <- list()
number_frequency_headline <- list()
# Loop to calculate the average frequency of numbers for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of numbers in status messages
  mean_number_status <- mean(status_current$number, na.rm = TRUE)
  number_frequency_status[[i]] <- mean_number_status
  # Calculate the average frequency of numbers in headlines
  mean_number_headline <- mean(headlines_current$number, na.rm = TRUE)
  number_frequency_headline[[i]] <- mean_number_headline
}
# Create a dataframe for the results
number_frequency_df <- data.frame(
  Newspaper = newspapers,
  Number_Frequency_Status = unlist(number_frequency_status),
  Number_Frequency_Headline = unlist(number_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
number_frequency_melted <- melt(number_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Numbers")
# Display the original data frame to verify the inclusion of both types of data
print(number_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(number_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
number_frequency_melted$Newspaper <- factor(number_frequency_melted$Newspaper,
                                            levels = c("Times", "Telegraph", "Guardian",
                                                       "Sun", "Daily Mail", "Daily Express",
                                                       "Economist", "Tribune", "Prospect"))
number_frequency_melted$Type <- factor(number_frequency_melted$Type, levels = c("Number_Frequency_Headline", "Number_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Number_Frequency_Headline" = "#80ae9a", "Number_Frequency_Status" = "#122740")
# Create bar plot
plot_h2ii <- ggplot(number_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Numbers`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Numbers") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2ii.png", plot = plot_h2ii, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
number_frequency_status<-list()
number_frequency_headline<-list()
#Loop to calculate the average frequency of number for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_number_status<-mean(as.numeric(status_current$number),na.rm=TRUE)
  number_frequency_status[[i]]<-mean_number_status
  mean_number_headline<-mean(as.numeric(headlines_current$number),na.rm=TRUE)
  number_frequency_headline[[i]]<-mean_number_headline
}
#Create a dataframe for the results
number_frequency_df<-data.frame(Newspaper=newspapers,Number_Frequency_Status=unlist(number_frequency_status),Number_Frequency_Headline=unlist(number_frequency_headline))
#Create a new column for newspaper categories
number_frequency_df$Category<-ifelse(number_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(number_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
number_frequency_df$Category<-factor(number_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_number<-aggregate(cbind(Number_Frequency_Status,Number_Frequency_Headline)~Category,data=number_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_number<-melt(aggregated_data_number,id.vars="Category",variable.name="Type",value.name="Average Frequency of Number")
#Adjust Type factor levels to control plot order
aggregated_melted_number$Type<-factor(aggregated_melted_number$Type,levels=c("Number_Frequency_Headline","Number_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_number<-c("Number_Frequency_Headline"="#80ae9a","Number_Frequency_Status"="#122740")
#Create bar plot
plot_h2ii_aggregated <- ggplot(aggregated_melted_number, aes(x = Category, y = `Average Frequency of Number`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Numbers") +
  scale_fill_manual(values = custom_colors_number, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2ii_aggregated.png",plot=plot_h2ii_aggregated,dpi=300,width=20,height=16,units="cm")

###### Beta regression for number (h2ii) ######
# Histogram to examine distribution
hist(total$number)
# Boxplot to look at relationship with predictor variables
bwplot(number ~ media | type, total)
# Scale number to be between 0 and 1 and adjust for beta regression requirements
total$number1 <- (total$number / 30) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2ii <- glmmTMB(number1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2ii)
# Run the beta regression model without interaction
mh2ii_g <- glmmTMB(number1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2ii_g)
# Compare the two models
anova_h2ii <- anova(mh2ii, mh2ii_g)


# Create a table for the model mh2ii_g
tab_model(mh2ii_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2ii.doc")
# Browse the created document
browseURL("model_results_h2ii.doc")
# Convert the ANOVA results into a data frame
anova_table_h2ii <- as.data.frame(anova_h2ii)
anova_table_h2ii$Model <- c("mh2ii: adj1 ~ media * type", "mh2ii_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2ii <- anova_table_h2ii[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2ii <- read_docx()
# Create a table with flextable
anova_ft_h2ii <- flextable(anova_table_h2ii)
# Set the style of the table
anova_ft_h2ii <- set_header_labels(anova_ft_h2ii, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2ii <- font(anova_ft_h2ii, fontname = "Times New Roman", part = "all")
anova_ft_h2ii <- fontsize(anova_ft_h2ii, size = 12, part = "all")
anova_ft_h2ii <- autofit(anova_ft_h2ii)
# Add the table to the Word document
doc_h2ii <- body_add_flextable(doc_h2ii, value = anova_ft_h2ii)
# Save the Word document
print(doc_h2ii, target = "anova_results_h2ii.docx")
# Browse the created document
browseURL("anova_results_h2ii.docx")


# Visualize model predictions
library(ggeffects)
pred_h2ii <- ggpredict(mh2ii_g, terms = c("media", "type"))
h2ii_plot_pred <- plot(pred_h2ii, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2ii_plot_pred.png", plot = h2ii_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h2l#######
# h2l: Status messages contain less adverbs than headlines. (adverb)

# Initialize lists to collect results
adverb_frequency_status <- list()
adverb_frequency_headline <- list()
# Loop to calculate the average frequency of adverbs for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of adverbs in status messages
  mean_adverb_status <- mean(status_current$adverb, na.rm = TRUE)
  adverb_frequency_status[[i]] <- mean_adverb_status
  # Calculate the average frequency of adverbs in headlines
  mean_adverb_headline <- mean(headlines_current$adverb, na.rm = TRUE)
  adverb_frequency_headline[[i]] <- mean_adverb_headline
}
# Create a dataframe for the results
adverb_frequency_df <- data.frame(
  Newspaper = newspapers,
  Adverb_Frequency_Status = unlist(adverb_frequency_status),
  Adverb_Frequency_Headline = unlist(adverb_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
adverb_frequency_melted <- melt(adverb_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Adverbs")
# Display the original data frame to verify the inclusion of both types of data
print(adverb_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(adverb_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
adverb_frequency_melted$Newspaper <- factor(adverb_frequency_melted$Newspaper,
                                            levels = c("Times", "Telegraph", "Guardian",
                                                       "Sun", "Daily Mail", "Daily Express",
                                                       "Economist", "Tribune", "Prospect"))
adverb_frequency_melted$Type <- factor(adverb_frequency_melted$Type, levels = c("Adverb_Frequency_Headline", "Adverb_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Adverb_Frequency_Headline" = "#80ae9a", "Adverb_Frequency_Status" = "#122740")
# Create bar plot
plot_h2l <- ggplot(adverb_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Adverbs`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Remove title
       x = "Newspaper",
       y = "Average Frequency of Adverbs") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h2l.png", plot = plot_h2l, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
adverb_frequency_status<-list()
adverb_frequency_headline<-list()
#Loop to calculate the average frequency of adverbs for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_adverb_status<-mean(as.numeric(status_current$adverb),na.rm=TRUE)
  adverb_frequency_status[[i]]<-mean_adverb_status
  mean_adverb_headline<-mean(as.numeric(headlines_current$adverb),na.rm=TRUE)
  adverb_frequency_headline[[i]]<-mean_adverb_headline
}
#Create a dataframe for the results
adverb_frequency_df<-data.frame(Newspaper=newspapers,Adverb_Frequency_Status=unlist(adverb_frequency_status),Adverb_Frequency_Headline=unlist(adverb_frequency_headline))
#Create a new column for newspaper categories
adverb_frequency_df$Category<-ifelse(adverb_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(adverb_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
adverb_frequency_df$Category<-factor(adverb_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_adverb<-aggregate(cbind(Adverb_Frequency_Status,Adverb_Frequency_Headline)~Category,data=adverb_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_adverb<-melt(aggregated_data_adverb,id.vars="Category",variable.name="Type",value.name="Average Frequency of Adverbs")
#Adjust Type factor levels to control plot order
aggregated_melted_adverb$Type<-factor(aggregated_melted_adverb$Type,levels=c("Adverb_Frequency_Headline","Adverb_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_adverb<-c("Adverb_Frequency_Headline"="#80ae9a","Adverb_Frequency_Status"="#122740")
#Create bar plot
plot_h2l_aggregated <- ggplot(aggregated_melted_adverb, aes(x = Category, y = `Average Frequency of Adverbs`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = NULL,  # Remove title
       x = "Newspaper Category",
       y = "Average Frequency of Adverbs") +
  scale_fill_manual(values = custom_colors_adverb, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Position legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h2l_aggregated.png",plot=plot_h2l_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for adverb (h2l) ######
# Histogram to examine distribution
hist(total$adverb)
# Boxplot to look at relationship with predictor variables
bwplot(adverb ~ media | type, total)
# Scale adverb to be between 0 and 1 and adjust for beta regression requirements
total$adverb1 <- (total$adverb / 25) * 0.98 + 0.01
# Run the beta regression model with interaction
mh2l <- glmmTMB(adverb1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh2l)
# Run the beta regression model without interaction
mh2l_g <- glmmTMB(adverb1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh2l_g)
# Compare the two models
anova_h2l <- anova(mh2l, mh2l_g)



# Create a table for the model mh2l_g
tab_model(mh2l, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h2l.doc")
# Browse the created document
browseURL("model_results_h2l.doc")
# Convert the ANOVA results into a data frame
anova_table_h2l <- as.data.frame(anova_h2l)
anova_table_h2l$Model <- c("mh2l: adj1 ~ media * type", "mh2l_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h2l <- anova_table_h2l[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h2l <- read_docx()
# Create a table with flextable
anova_ft_h2l <- flextable(anova_table_h2l)
# Set the style of the table
anova_ft_h2l <- set_header_labels(anova_ft_h2l, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h2l <- font(anova_ft_h2l, fontname = "Times New Roman", part = "all")
anova_ft_h2l <- fontsize(anova_ft_h2l, size = 12, part = "all")
anova_ft_h2l <- autofit(anova_ft_h2l)
# Add the table to the Word document
doc_h2l <- body_add_flextable(doc_h2l, value = anova_ft_h2l)
# Save the Word document
print(doc_h2l, target = "anova_results_h2l.docx")
# Browse the created document
browseURL("anova_results_h2l.docx")

# Visualize model predictions
library(ggeffects)
pred_h2l <- ggpredict(mh2l_g, terms = c("media", "type"))
h2l_plot_pred <- plot(pred_h2l, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h2l_plot_pred.png", plot = h2l_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


######################Informality######################
#######H3#######
#Status messages have a higher degree of informality than titles and subtitles.

#######h3a#######
#Status messages have more quantifiers than titles and subtitles.(quantity)

# Initialize lists to collect results
quantifier_frequency_status <- list()
quantifier_frequency_headline <- list()
# Loop to calculate the average frequency of quantifiers for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of quantifiers in status messages
  mean_quantity_status <- mean(status_current$quantity, na.rm = TRUE)
  quantifier_frequency_status[[i]] <- mean_quantity_status
  # Calculate the average frequency of quantifiers in headlines
  mean_quantity_headline <- mean(headlines_current$quantity, na.rm = TRUE)
  quantifier_frequency_headline[[i]] <- mean_quantity_headline
}
# Create a dataframe for the results
quantifier_frequency_df <- data.frame(
  Newspaper = newspapers,
  Quantifier_Frequency_Status = unlist(quantifier_frequency_status),
  Quantifier_Frequency_Headline = unlist(quantifier_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
quantifier_frequency_melted <- melt(quantifier_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Quantifiers")
# Display the original data frame to verify the inclusion of both types of data
print(quantifier_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(quantifier_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
quantifier_frequency_melted$Newspaper <- factor(quantifier_frequency_melted$Newspaper,
                                                levels = c("Times", "Telegraph", "Guardian",
                                                           "Sun", "Daily Mail", "Daily Express",
                                                           "Economist", "Tribune", "Prospect"))
quantifier_frequency_melted$Type <- factor(quantifier_frequency_melted$Type, levels = c("Quantifier_Frequency_Headline", "Quantifier_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Quantifier_Frequency_Headline" = "#85C1E9", "Quantifier_Frequency_Status" = "#E59866")
# Create bar plot
plot_h3a <- ggplot(quantifier_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Quantifiers`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Frequency of Quantifiers: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Average Frequency of Quantifiers") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h3a.png", plot = plot_h3a, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
quantity_frequency_status<-list()
quantity_frequency_headline<-list()
#Loop to calculate the average frequency of quantity for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_quantity_status<-mean(as.numeric(status_current$quantity),na.rm=TRUE)
  quantity_frequency_status[[i]]<-mean_quantity_status
  mean_quantity_headline<-mean(as.numeric(headlines_current$quantity),na.rm=TRUE)
  quantity_frequency_headline[[i]]<-mean_quantity_headline
}
#Create a dataframe for the results
quantity_frequency_df<-data.frame(Newspaper=newspapers,Quantity_Frequency_Status=unlist(quantity_frequency_status),Quantity_Frequency_Headline=unlist(quantity_frequency_headline))
#Create a new column for newspaper categories
quantity_frequency_df$Category<-ifelse(quantity_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(quantity_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
quantity_frequency_df$Category<-factor(quantity_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_quantity<-aggregate(cbind(Quantity_Frequency_Status,Quantity_Frequency_Headline)~Category,data=quantity_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_quantity<-melt(aggregated_data_quantity,id.vars="Category",variable.name="Type",value.name="Average Frequency of Quantity")
#Adjust Type factor levels to control plot order
aggregated_melted_quantity$Type<-factor(aggregated_melted_quantity$Type,levels=c("Quantity_Frequency_Headline","Quantity_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_quantity<-c("Quantity_Frequency_Headline"="#85C1E9","Quantity_Frequency_Status"="#E59866")
#Create bar plot
plot_h3a_aggregated<-ggplot(aggregated_melted_quantity,aes(x=Category,y=`Average Frequency of Quantity`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of Quantity: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of Quantity")+scale_fill_manual(values=custom_colors_quantity,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h3a_aggregated.png",plot=plot_h3a_aggregated,dpi=300,width=22,height=18,units="cm")

###### Beta regression for quantity (h3a) ######
# Histogram to examine distribution
hist(total$quantity)
# Boxplot to look at relationship with predictor variables
bwplot(quantity ~ media | type, total)
# Scale quantity to be between 0 and 1 and adjust for beta regression requirements
total$quantity1 <- (total$quantity / 35) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3a <- glmmTMB(quantity1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3a)
# Run the beta regression model without interaction
mh3a_g <- glmmTMB(quantity1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3a_g)
# Compare the two models
anova(mh3a, mh3a_g)
# Visualize model predictions
library(ggeffects)
pred_h3a <- ggpredict(mh3a_g, terms = c("media", "type"))
h3a_plot_pred <- plot(pred_h3a, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3a_plot_pred.png", plot = h3a_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3aa#######
#Status messages have more numbers than titles and subtitles.(number)

# Initialize lists to collect results
number_frequency_status <- list()
number_frequency_headline <- list()
# Loop to calculate the average frequency of numbers for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of numbers in status messages
  mean_number_status <- mean(status_current$number, na.rm = TRUE)
  number_frequency_status[[i]] <- mean_number_status
  # Calculate the average frequency of numbers in headlines
  mean_number_headline <- mean(headlines_current$number, na.rm = TRUE)
  number_frequency_headline[[i]] <- mean_number_headline
}
# Create a dataframe for the results
number_frequency_df <- data.frame(
  Newspaper = newspapers,
  Number_Frequency_Status = unlist(number_frequency_status),
  Number_Frequency_Headline = unlist(number_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
number_frequency_melted <- melt(number_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Numbers")
# Display the original data frame to verify the inclusion of both types of data
print(number_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(number_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
number_frequency_melted$Newspaper <- factor(number_frequency_melted$Newspaper,
                                            levels = c("Times", "Telegraph", "Guardian",
                                                       "Sun", "Daily Mail", "Daily Express",
                                                       "Economist", "Tribune", "Prospect"))
number_frequency_melted$Type <- factor(number_frequency_melted$Type, levels = c("Number_Frequency_Headline", "Number_Frequency_Status"))
# Custom colors, ensuring headlines are first
custom_colors <- c("Number_Frequency_Headline" = "#85C1E9", "Number_Frequency_Status" = "#E59866")
# Create bar plot
plot_h3aa <- ggplot(number_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Numbers`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Frequency of Numbers: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Average Frequency of Numbers") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h3aa.png", plot = plot_h3aa, dpi = 300, width = 20, height = 16, units = "cm")

#Initialize lists to collect results
number_frequency_status<-list()
number_frequency_headline<-list()
#Loop to calculate the average frequency of number for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_number_status<-mean(as.numeric(status_current$number),na.rm=TRUE)
  number_frequency_status[[i]]<-mean_number_status
  mean_number_headline<-mean(as.numeric(headlines_current$number),na.rm=TRUE)
  number_frequency_headline[[i]]<-mean_number_headline
}
#Create a dataframe for the results
number_frequency_df<-data.frame(Newspaper=newspapers,Number_Frequency_Status=unlist(number_frequency_status),Number_Frequency_Headline=unlist(number_frequency_headline))
#Create a new column for newspaper categories
number_frequency_df$Category<-ifelse(number_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(number_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
number_frequency_df$Category<-factor(number_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_number<-aggregate(cbind(Number_Frequency_Status,Number_Frequency_Headline)~Category,data=number_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_number<-melt(aggregated_data_number,id.vars="Category",variable.name="Type",value.name="Average Frequency of Number")
#Adjust Type factor levels to control plot order
aggregated_melted_number$Type<-factor(aggregated_melted_number$Type,levels=c("Number_Frequency_Headline","Number_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_number<-c("Number_Frequency_Headline"="#85C1E9","Number_Frequency_Status"="#E59866")
#Create bar plot
plot_h3aa_aggregated<-ggplot(aggregated_melted_number,aes(x=Category,y=`Average Frequency of Number`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of Number: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of Number")+scale_fill_manual(values=custom_colors_number,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h3aa_aggregated.png",plot=plot_h3aa_aggregated,dpi=300,width=22,height=18,units="cm")


###### Beta regression for number (h3aa) ######
# Histogram to examine distribution
hist(total$number)
# Boxplot to look at relationship with predictor variables
bwplot(number ~ media | type, total)
# Scale number to be between 0 and 1 and adjust for beta regression requirements
total$number1 <- (total$number / 30) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3aa <- glmmTMB(number1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3aa)
# Run the beta regression model without interaction
mh3aa_g <- glmmTMB(number1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3aa_g)
# Compare the two models
anova(mh3aa, mh3aa_g)
# Visualize model predictions
library(ggeffects)
pred_h3aa <- ggpredict(mh3aa_g, terms = c("media", "type"))
h3aa_plot_pred <- plot(pred_h3aa, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3aa_plot_pred.png", plot = h3aa_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3b#######
#Status messages contain a higher level of emoji than titles and subtitles.

# Frequency of Emoji Usage in Different Message Types
emoji_frequency <- table(total$type, total$Emoji)
print(emoji_frequency)
# Check for Emoji Presence in Headlines
# Filter data for headlines only
headlines_data <- total[total$type == "Headline", ]
# Calculate the sum of Emoji usage in headlines
emoji_in_headlines <- sum(headlines_data$Emoji)
print(paste("Total emojis in headlines:", emoji_in_headlines))
# Verify absence of emojis in headlines
if (emoji_in_headlines > 0) {
  print("There are emojis in some headlines.")
} else {
  print("No emojis are present in headlines.")
}
# Calculate average number of emojis in status messages and headlines
emoji_status <- total[total$type == "Status", ]
emoji_headlines <- total[total$type == "Headline", ]
average_emoji_status <- ifelse(nrow(emoji_status) > 0, mean(emoji_status$Emoji, na.rm = TRUE), NA)
average_emoji_headline <- ifelse(nrow(emoji_headlines) > 0, mean(emoji_headlines$Emoji, na.rm = TRUE), NA)
# Creating a dataframe for the results
emoji_comparison_df <- data.frame(
  Type = c("Status", "Headline"),
  Average_Emoji = c(average_emoji_status, average_emoji_headline)
)
# Load ggplot2 if not already loaded
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
# Bar chart comparing average emoji usage in status messages vs. headlines
plot_h3b <- ggplot(emoji_comparison_df, aes(x = Type, y = Average_Emoji, fill = Type)) +
  geom_col(width = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Average Emoji Usage in Status Messages vs. Headlines", x = "Message Type", y = "Average Number of Emojis") +
  scale_fill_brewer(palette = "Set2") 
# Save the plot
ggsave("~/Desktop/plot/plot_h3b.png", plot = plot_h3b, dpi = 300, width = 16, height = 12, units = "cm")

# List to collect results
average_emojis <- list()
# Loop to calculate the average number of emojis per newspaper
for (i in 1:9) {
  tweets_current <- data_tweet[data_tweet$media == i, ]
  mean_emoji_current <- ifelse(nrow(tweets_current) > 0, mean(tweets_current$Emoji, na.rm = TRUE), NA)
  average_emojis[[i]] <- mean_emoji_current
}
# Create a dataframe for the results
average_emojis_df <- data.frame(
  Newspaper = newspapers,
  Average_Emoji = unlist(average_emojis)
)
# Order the newspapers according to the specified sequence
average_emojis_df$Newspaper <- factor(average_emojis_df$Newspaper, levels = newspapers)
# Load ggplot2 if not already loaded
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
# Bar chart of average emoji per newspaper
plot_h3b <- ggplot(average_emojis_df, aes(x = Newspaper, y = Average_Emoji, fill = Newspaper)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate the X-axis labels by 45 degrees
  labs(title = "Average Proportion of Emoji in Tweets by Newspaper", x = "Newspaper", y = "Average Proportion of Emoji") +
  scale_fill_brewer(palette = "Set3")
ggsave("~/Desktop/plot/h3b.png", plot = plot_h3b, dpi = 300, width = 16, height = 12, units = "cm")


#######h3c#######
#Status messages have a higher level of authenticity than titles and subtitles (Authentic).

# Initialize lists to collect results
authenticity_frequency_status <- list()
authenticity_frequency_headline <- list()
# Loop to calculate the average level of authenticity for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average level of authenticity in status messages
  mean_authentic_status <- mean(status_current$Authentic, na.rm = TRUE)
  authenticity_frequency_status[[i]] <- mean_authentic_status
  # Calculate the average level of authenticity in headlines
  mean_authentic_headline <- mean(headlines_current$Authentic, na.rm = TRUE)
  authenticity_frequency_headline[[i]] <- mean_authentic_headline
}
# Create a dataframe for the results
authenticity_frequency_df <- data.frame(
  Newspaper = newspapers,
  Authenticity_Frequency_Status = unlist(authenticity_frequency_status),
  Authenticity_Frequency_Headline = unlist(authenticity_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
authenticity_frequency_melted <- melt(authenticity_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Level of Authenticity")
# Display the original data frame to verify the inclusion of both types of data
print(authenticity_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(authenticity_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
authenticity_frequency_melted$Newspaper <- factor(authenticity_frequency_melted$Newspaper,
                                                  levels = c("Times", "Telegraph", "Guardian",
                                                             "Sun", "Daily Mail", "Daily Express",
                                                             "Economist", "Tribune", "Prospect"))
authenticity_frequency_melted$Type <- factor(authenticity_frequency_melted$Type, levels = c("Authenticity_Frequency_Headline", "Authenticity_Frequency_Status"))
# Custom colors, ensuring status messages are highlighted
custom_colors <- c("Authenticity_Frequency_Headline" = "#b5d1ae", "Authenticity_Frequency_Status" = "#326677")
# Create bar plot
plot_h3c <- ggplot(authenticity_frequency_melted, aes(x = Newspaper, y = `Average Level of Authenticity`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Removing the title
       x = "Newspaper",
       y = "Average Level of Authenticity") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Place legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h3c.png", plot = plot_h3c, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
authentic_frequency_status<-list()
authentic_frequency_headline<-list()
#Loop to calculate the average frequency of Authentic for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_authentic_status<-mean(as.numeric(status_current$Authentic),na.rm=TRUE)
  authentic_frequency_status[[i]]<-mean_authentic_status
  mean_authentic_headline<-mean(as.numeric(headlines_current$Authentic),na.rm=TRUE)
  authentic_frequency_headline[[i]]<-mean_authentic_headline
}
#Create a dataframe for the results
authentic_frequency_df<-data.frame(Newspaper=newspapers,Authentic_Frequency_Status=unlist(authentic_frequency_status),Authentic_Frequency_Headline=unlist(authentic_frequency_headline))
#Create a new column for newspaper categories
authentic_frequency_df$Category<-ifelse(authentic_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(authentic_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
authentic_frequency_df$Category<-factor(authentic_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_authentic<-aggregate(cbind(Authentic_Frequency_Status,Authentic_Frequency_Headline)~Category,data=authentic_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_authentic<-melt(aggregated_data_authentic,id.vars="Category",variable.name="Type",value.name="Average Frequency of Authentic")
#Adjust Type factor levels to control plot order
aggregated_melted_authentic$Type<-factor(aggregated_melted_authentic$Type,levels=c("Authentic_Frequency_Headline","Authentic_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_authentic<-c("Authentic_Frequency_Headline"="#b5d1ae","Authentic_Frequency_Status"="#326677")
#Create bar plot
plot_h3c_aggregated <- ggplot(aggregated_melted_authentic, aes(x = Category, y = `Average Frequency of Authentic`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Removing the title
       x = "Newspaper Category",
       y = "Average Frequency of Authenticity") +
  scale_fill_manual(values = custom_colors_authentic, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Place legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h3c_aggregated.png",plot=plot_h3c_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for Authentic (h3c) ######
# Histogram to examine distribution
hist(total$Authentic)
# Boxplot to look at relationship with predictor variables
bwplot(Authentic ~ media | type, total)
# Scale Authentic to be between 0 and 1 and adjust for beta regression requirements
total$Authentic1 <- (total$Authentic / 100) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3c <- glmmTMB(Authentic1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3c)
# Run the beta regression model without interaction
mh3c_g <- glmmTMB(Authentic1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3c_g)
# Compare the two models
anova_h3c <- anova(mh3c, mh3c_g)


# Create a table for the model mh3c_g
tab_model(mh3c_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h3c.doc")
# Browse the created document
browseURL("model_results_h3c.doc")
# Convert the ANOVA results into a data frame
anova_table_h3c <- as.data.frame(anova_h3c)
anova_table_h3c$Model <- c("mh3c: adj1 ~ media * type", "mh3c_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h3c <- anova_table_h3c[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h3c <- read_docx()
# Create a table with flextable
anova_ft_h3c <- flextable(anova_table_h3c)
# Set the style of the table
anova_ft_h3c <- set_header_labels(anova_ft_h3c, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h3c <- font(anova_ft_h3c, fontname = "Times New Roman", part = "all")
anova_ft_h3c <- fontsize(anova_ft_h3c, size = 12, part = "all")
anova_ft_h3c <- autofit(anova_ft_h3c)
# Add the table to the Word document
doc_h3c <- body_add_flextable(doc_h3c, value = anova_ft_h3c)
# Save the Word document
print(doc_h3c, target = "anova_results_h3c.docx")
# Browse the created document
browseURL("anova_results_h3c.docx")


# Visualize model predictions
library(ggeffects)
pred_h3c <- ggpredict(mh3c_g, terms = c("media", "type"))
h3c_plot_pred <- plot(pred_h3c, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3c_plot_pred.png", plot = h3c_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3d#######
#Status messages contain more question marks than titles and subtitles.(QMark)

# Initialize lists to collect results
question_mark_frequency_status <- list()
question_mark_frequency_headline <- list()
# Loop to calculate the average frequency of question marks for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of question marks in status messages
  mean_qmark_status <- mean(status_current$QMark, na.rm = TRUE)
  question_mark_frequency_status[[i]] <- mean_qmark_status
  # Calculate the average frequency of question marks in headlines
  mean_qmark_headline <- mean(headlines_current$QMark, na.rm = TRUE)
  question_mark_frequency_headline[[i]] <- mean_qmark_headline
}
# Create a dataframe for the results
question_mark_frequency_df <- data.frame(
  Newspaper = newspapers,
  Question_Mark_Frequency_Status = unlist(question_mark_frequency_status),
  Question_Mark_Frequency_Headline = unlist(question_mark_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
question_mark_frequency_melted <- melt(question_mark_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Question Marks")
# Display the original data frame to verify the inclusion of both types of data
print(question_mark_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(question_mark_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
question_mark_frequency_melted$Newspaper <- factor(question_mark_frequency_melted$Newspaper,
                                                   levels = c("Times", "Telegraph", "Guardian",
                                                              "Sun", "Daily Mail", "Daily Express",
                                                              "Economist", "Tribune", "Prospect"))
question_mark_frequency_melted$Type <- factor(question_mark_frequency_melted$Type, levels = c("Question_Mark_Frequency_Headline", "Question_Mark_Frequency_Status"))
# Custom colors, ensuring status messages are highlighted
custom_colors <- c("Question_Mark_Frequency_Headline" = "#b5d1ae", "Question_Mark_Frequency_Status" = "#326677")
# Create bar plot
plot_h3d <- ggplot(question_mark_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Question Marks`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Removing title
       x = "Newspaper",
       y = "Average Frequency of Question Marks") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Place legend at the bottom
# Save the plot
ggsave("~/Desktop/plot/h3d.png", plot = plot_h3d, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
qmark_frequency_status<-list()
qmark_frequency_headline<-list()
#Loop to calculate the average frequency of QMark for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_qmark_status<-mean(as.numeric(status_current$QMark),na.rm=TRUE)
  qmark_frequency_status[[i]]<-mean_qmark_status
  mean_qmark_headline<-mean(as.numeric(headlines_current$QMark),na.rm=TRUE)
  qmark_frequency_headline[[i]]<-mean_qmark_headline
}
#Create a dataframe for the results
qmark_frequency_df<-data.frame(Newspaper=newspapers,QMark_Frequency_Status=unlist(qmark_frequency_status),QMark_Frequency_Headline=unlist(qmark_frequency_headline))
#Create a new column for newspaper categories
qmark_frequency_df$Category<-ifelse(qmark_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(qmark_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
qmark_frequency_df$Category<-factor(qmark_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_qmark<-aggregate(cbind(QMark_Frequency_Status,QMark_Frequency_Headline)~Category,data=qmark_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_qmark<-melt(aggregated_data_qmark,id.vars="Category",variable.name="Type",value.name="Average Frequency of QMark")
#Adjust Type factor levels to control plot order
aggregated_melted_qmark$Type<-factor(aggregated_melted_qmark$Type,levels=c("QMark_Frequency_Headline","QMark_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_qmark<-c("QMark_Frequency_Headline"="#b5d1ae","QMark_Frequency_Status"="#326677")
#Create bar plot
plot_h3d_aggregated <- ggplot(aggregated_melted_qmark, aes(x = Category, y = `Average Frequency of QMark`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = NULL,  # Removing title
       x = "Newspaper Category",
       y = "Average Frequency of Question Marks") +
  scale_fill_manual(values = custom_colors_qmark, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # X-axis text
        axis.text.y = element_text(size = 12),  # Y-axis text
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")  # Place legend at the bottom
#Save the plot
ggsave("~/Desktop/plot/h3d_aggregated.png",plot=plot_h3d_aggregated,dpi=300,width=20,height=16,units="cm")


###### Beta regression for QMark (h3d) ######
# Histogram to examine distribution
hist(total$QMark)
# Boxplot to look at relationship with predictor variables
bwplot(QMark ~ media | type, total)
# Scale QMark to be between 0 and 1 and adjust for beta regression requirements
total$QMark1 <- (total$QMark / 25) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3d <- glmmTMB(QMark1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3d)
# Run the beta regression model without interaction
mh3d_g <- glmmTMB(QMark1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3d_g)
# Compare the two models
anova_h3d <- anova(mh3d, mh3d_g)

# Create a table for the model mh3d_g
tab_model(mh3d_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h3d.doc")
# Browse the created document
browseURL("model_results_h3d.doc")
# Convert the ANOVA results into a data frame
anova_table_h3d <- as.data.frame(anova_h3d)
anova_table_h3d$Model <- c("mh3d: adj1 ~ media * type", "mh3d_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h3d <- anova_table_h3d[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h3d <- read_docx()
# Create a table with flextable
anova_ft_h3d <- flextable(anova_table_h3d)
# Set the style of the table
anova_ft_h3d <- set_header_labels(anova_ft_h3d, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h3d <- font(anova_ft_h3d, fontname = "Times New Roman", part = "all")
anova_ft_h3d <- fontsize(anova_ft_h3d, size = 12, part = "all")
anova_ft_h3d <- autofit(anova_ft_h3d)
# Add the table to the Word document
doc_h3d <- body_add_flextable(doc_h3d, value = anova_ft_h3d)
# Save the Word document
print(doc_h3d, target = "anova_results_h3d.docx")
# Browse the created document
browseURL("anova_results_h3d.docx")


# Visualize model predictions
library(ggeffects)
pred_h3d <- ggpredict(mh3d_g, terms = c("media", "type"))
h3d_plot_pred <- plot(pred_h3d, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3d_plot_pred.png", plot = h3d_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3e#######
#Status messages contain more first and second-person pronouns than titles and subtitles.("i", "we", "you")

# Initialize lists to collect results for each pronoun
i_status <- list()
we_status <- list()
you_status <- list()
i_headline <- list()
we_headline <- list()
you_headline <- list()
# Loop to calculate the average usage of each pronoun for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average usage of each pronoun in status messages
  mean_i_status <- mean(as.numeric(status_current$i), na.rm = TRUE)
  mean_we_status <- mean(as.numeric(status_current$we), na.rm = TRUE)
  mean_you_status <- mean(as.numeric(status_current$you), na.rm = TRUE)
  i_status[[i]] <- mean_i_status
  we_status[[i]] <- mean_we_status
  you_status[[i]] <- mean_you_status
  # Calculate the average usage of each pronoun in headlines
  mean_i_headline <- mean(as.numeric(headlines_current$i), na.rm = TRUE)
  mean_we_headline <- mean(as.numeric(headlines_current$we), na.rm = TRUE)
  mean_you_headline <- mean(as.numeric(headlines_current$you), na.rm = TRUE)
  i_headline[[i]] <- mean_i_headline
  we_headline[[i]] <- mean_we_headline
  you_headline[[i]] <- mean_you_headline
}
# Create a dataframe for the results, combining all measures
pronoun_data <- data.frame(
  Newspaper = rep(newspapers, each = 3),
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 3),
  Type = rep(c("Pronoun I", "Pronoun We", "Pronoun You"), times = 9),
  Frequency_Status = c(unlist(i_status), unlist(we_status), unlist(you_status)),
  Frequency_Headline = c(unlist(i_headline), unlist(we_headline), unlist(you_headline))
)
# Melt the data for plotting
library(reshape2)
melted_pronoun_data <- melt(pronoun_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Define custom colors for visual distinction
custom_colors_pronouns <- c("Pronoun I" = "#122740",
                            "Pronoun We" = "#16485e",
                            "Pronoun You" = "#326677")
# Ensure that the categories are in the correct order
melted_pronoun_data$Category <- factor(melted_pronoun_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Adjust the 'Type' factor to specify the order of levels directly
melted_pronoun_data$Type <- factor(melted_pronoun_data$Type, levels = c("Pronoun I", "Pronoun We", "Pronoun You"))
# Create the plot
pronoun_plot <- ggplot(melted_pronoun_data, aes(x = Category, y = AverageValue, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~MessageType, scales = "free_y", 
             labeller = labeller(MessageType = c(Frequency_Status = "Status Message", Frequency_Headline = "Headline"))) +
  labs(title = NULL,
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors_pronouns) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))  # Standard legend order
# Save the plot
ggsave("~/Desktop/plot/comparison_pronouns_usage.png", plot = pronoun_plot, dpi = 300, width = 24, height = 20, units = "cm")



##################################new pronouns####################################

library(ggplot2)
library(reshape2)
library(dplyr)
# Assuming pronoun_data has been correctly aggregated and is stored as described
# Melt the data
melted_pronoun_data <- melt(pronoun_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Define and adjust levels for clear grouping in plot
melted_pronoun_data$MessageType <- factor(melted_pronoun_data$MessageType,
                                          levels = c("Frequency_Headline", "Frequency_Status"),
                                          labels = c("Headline", "Status Message"))
melted_pronoun_data$Category <- factor(melted_pronoun_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Create an interaction term for Fill aesthetic
melted_pronoun_data$Fill <- with(melted_pronoun_data, interaction(Type, MessageType, sep = ": "))
melted_pronoun_data$Fill <- factor(
  melted_pronoun_data$Fill,
  levels = c(
    "Pronoun I: Headline", "Pronoun I: Status Message",
    "Pronoun We: Headline", "Pronoun We: Status Message",
    "Pronoun You: Headline", "Pronoun You: Status Message"
  )
)
# Specify custom colors for visual distinction
custom_colors_pronouns <- c(
  "Pronoun I: Headline" = "#568687",   
  "Pronoun I: Status Message" = "#16485e",  
  "Pronoun We: Headline" = "#80ae9a",  
  "Pronoun We: Status Message" = "#122740", 
  "Pronoun You: Headline" = "#b5d1ae",
  "Pronoun You: Status Message" = "#326677" 
)
# Plotting
pronoun_plot1 <- ggplot(melted_pronoun_data, aes(x = Category, y = AverageValue, fill = Fill)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_y") +
  labs(title = NULL, x = "Newspaper Category", y = "Average Value") +
  scale_fill_manual(values = custom_colors_pronouns) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12)) +
  guides(fill = guide_legend(title = "Pronoun and Type", reverse = FALSE, order = TRUE))
# Display the plot
print(pronoun_plot1)
# Save the plot
ggsave("~/Desktop/plot/comparison_pronouns_usage1.png", plot = pronoun_plot1, dpi = 300, width = 24, height = 20, units = "cm")



###### Beta regression analysis setup for pronouns (i, we, you) ######
hist(total$i)
hist(total$we)
hist(total$you)
# Ensure data is appropriately scaled to fit beta regression requirements
total$i1 <- (total$i - min(total$i)) / (max(total$i) - min(total$i))
total$we1 <- (total$we - min(total$we)) / (max(total$we) - min(total$we))
total$you1 <- (total$you - min(total$you)) / (max(total$you) - min(total$you))
# Adjust scales slightly to ensure all values are strictly between 0 and 1
total$i1 <- 0.01 + 0.98 * total$i1
total$we1 <- 0.01 + 0.98 * total$we1
total$you1 <- 0.01 + 0.98 * total$you1
# Define the model for pronoun 'i'
model_i <- glmmTMB(i1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_i)
model_i_g <- glmmTMB(i1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_i_g)
anova_model_i <- anova(model_i, model_i_g)

# Create a table for the model model_i_g
tab_model(model_i_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_i.doc")
# Browse the created document
browseURL("model_results_model_i.doc")
# Convert the ANOVA results into a data frame
anova_table_model_i <- as.data.frame(anova_model_i)
anova_table_model_i$Model <- c("model_i: adj1 ~ media * type", "model_i_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_i <- anova_table_model_i[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_i <- read_docx()
# Create a table with flextable
anova_ft_model_i <- flextable(anova_table_model_i)
# Set the style of the table
anova_ft_model_i <- set_header_labels(anova_ft_model_i, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_i <- font(anova_ft_model_i, fontname = "Times New Roman", part = "all")
anova_ft_model_i <- fontsize(anova_ft_model_i, size = 12, part = "all")
anova_ft_model_i <- autofit(anova_ft_model_i)
# Add the table to the Word document
doc_model_i <- body_add_flextable(doc_model_i, value = anova_ft_model_i)
# Save the Word document
print(doc_model_i, target = "anova_results_model_i.docx")
# Browse the created document
browseURL("anova_results_model_i.docx")


# Define the model for pronoun 'we'
model_we <- glmmTMB(we1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_we)
model_we_g <- glmmTMB(we1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_we_g)
anova_model_we <- anova(model_we, model_we_g)

# Create a table for the model model_we_g
tab_model(model_we_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_we.doc")
# Browse the created document
browseURL("model_results_model_we.doc")
# Convert the ANOVA results into a data frame
anova_table_model_we <- as.data.frame(anova_model_we)
anova_table_model_we$Model <- c("model_we: adj1 ~ media * type", "model_we_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_we <- anova_table_model_we[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_we <- read_docx()
# Create a table with flextable
anova_ft_model_we <- flextable(anova_table_model_we)
# Set the style of the table
anova_ft_model_we <- set_header_labels(anova_ft_model_we, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_we <- font(anova_ft_model_we, fontname = "Times New Roman", part = "all")
anova_ft_model_we <- fontsize(anova_ft_model_we, size = 12, part = "all")
anova_ft_model_we <- autofit(anova_ft_model_we)
# Add the table to the Word document
doc_model_we <- body_add_flextable(doc_model_we, value = anova_ft_model_we)
# Save the Word document
print(doc_model_we, target = "anova_results_model_we.docx")
# Browse the created document
browseURL("anova_results_model_we.docx")


# Define the model for pronoun 'you'
model_you <- glmmTMB(you1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_you)
model_you_g <- glmmTMB(you1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_you_g)
anova_model_you <- anova(model_you, model_you_g)

# Create a table for the model model_you_g
tab_model(model_you_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_you.doc")
# Browse the created document
browseURL("model_results_model_you.doc")
# Convert the ANOVA results into a data frame
anova_table_model_you <- as.data.frame(anova_model_you)
anova_table_model_you$Model <- c("model_you: adj1 ~ media * type", "model_you_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_you <- anova_table_model_you[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_you <- read_docx()
# Create a table with flextable
anova_ft_model_you <- flextable(anova_table_model_you)
# Set the style of the table
anova_ft_model_you <- set_header_labels(anova_ft_model_you, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_you <- font(anova_ft_model_you, fontname = "Times New Roman", part = "all")
anova_ft_model_you <- fontsize(anova_ft_model_you, size = 12, part = "all")
anova_ft_model_you <- autofit(anova_ft_model_you)
# Add the table to the Word document
doc_model_you <- body_add_flextable(doc_model_you, value = anova_ft_model_you)
# Save the Word document
print(doc_model_you, target = "anova_results_model_you.docx")
# Browse the created document
browseURL("anova_results_model_you.docx")



###### Beta regression analysis setup for pronouns (shehe, they, ipron) ######
hist(total$shehe)
hist(total$they)
hist(total$ipron)
# Ensure data is appropriately scaled to fit beta regression requirements
total$shehe1 <- (total$shehe - min(total$shehe)) / (max(total$shehe) - min(total$shehe))
total$they1 <- (total$they - min(total$they)) / (max(total$they) - min(total$they))
total$ipron1 <- (total$ipron - min(total$ipron)) / (max(total$ipron) - min(total$ipron))
# Adjust scales slightly to ensure all values are strictly between 0 and 1
total$shehe1 <- 0.01 + 0.98 * total$shehe1
total$they1 <- 0.01 + 0.98 * total$they1
total$ipron1 <- 0.01 + 0.98 * total$ipron1
# Define the model for pronoun 'shehe'
model_shehe <- glmmTMB(shehe1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_shehe)
model_shehe_g <- glmmTMB(shehe1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_shehe_g)
anova_model_shehe <- anova(model_shehe, model_shehe_g)
print(anova_model_shehe)

# Create a table for the model model_shehe_g
tab_model(model_shehe, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_shehe.doc")
# Browse the created document
browseURL("model_results_model_shehe.doc")
# Convert the ANOVA results into a data frame
anova_table_model_shehe <- as.data.frame(anova_model_shehe)
anova_table_model_shehe$Model <- c("model_shehe: adj1 ~ media * type", "model_shehe_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_shehe <- anova_table_model_shehe[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_shehe <- read_docx()
# Create a table with flextable
anova_ft_model_shehe <- flextable(anova_table_model_shehe)
# Set the style of the table
anova_ft_model_shehe <- set_header_labels(anova_ft_model_shehe, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_shehe <- font(anova_ft_model_shehe, fontname = "Times New Roman", part = "all")
anova_ft_model_shehe <- fontsize(anova_ft_model_shehe, size = 12, part = "all")
anova_ft_model_shehe <- autofit(anova_ft_model_shehe)
# Add the table to the Word document
doc_model_shehe <- body_add_flextable(doc_model_shehe, value = anova_ft_model_shehe)
# Save the Word document
print(doc_model_shehe, target = "anova_results_model_shehe.docx")
# Browse the created document
browseURL("anova_results_model_shehe.docx")


# Define the model for pronoun 'they'
model_they <- glmmTMB(they1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_they)
model_they_g <- glmmTMB(they1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_they_g)
anova_model_they <- anova(model_they, model_they_g)
print(anova_model_they)

# Create a table for the model model_they_g
tab_model(model_they_g, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_they.doc")
# Browse the created document
browseURL("model_results_model_they.doc")
# Convert the ANOVA results into a data frame
anova_table_model_they <- as.data.frame(anova_model_they)
anova_table_model_they$Model <- c("model_they: adj1 ~ media * type", "model_they_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_they <- anova_table_model_they[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_they <- read_docx()
# Create a table with flextable
anova_ft_model_they <- flextable(anova_table_model_they)
# Set the style of the table
anova_ft_model_they <- set_header_labels(anova_ft_model_they, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_they <- font(anova_ft_model_they, fontname = "Times New Roman", part = "all")
anova_ft_model_they <- fontsize(anova_ft_model_they, size = 12, part = "all")
anova_ft_model_they <- autofit(anova_ft_model_they)
# Add the table to the Word document
doc_model_they <- body_add_flextable(doc_model_they, value = anova_ft_model_they)
# Save the Word document
print(doc_model_they, target = "anova_results_model_they.docx")
# Browse the created document
browseURL("anova_results_model_they.docx")


# Define the model for pronoun 'ipron'
model_ipron <- glmmTMB(ipron1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(model_ipron)
model_ipron_g <- glmmTMB(ipron1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(model_ipron_g)
anova_model_ipron <- anova(model_ipron, model_ipron_g)
print(anova_model_ipron)

# Create a table for the model model_ipron_g
tab_model(model_ipron, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_model_ipron.doc")
# Browse the created document
browseURL("model_results_model_ipron.doc")
# Convert the ANOVA results into a data frame
anova_table_model_ipron <- as.data.frame(anova_model_ipron)
anova_table_model_ipron$Model <- c("model_ipron: adj1 ~ media * type", "model_ipron_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_model_ipron <- anova_table_model_ipron[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_model_ipron <- read_docx()
# Create a table with flextable
anova_ft_model_ipron <- flextable(anova_table_model_ipron)
# Set the style of the table
anova_ft_model_ipron <- set_header_labels(anova_ft_model_ipron, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_model_ipron <- font(anova_ft_model_ipron, fontname = "Times New Roman", part = "all")
anova_ft_model_ipron <- fontsize(anova_ft_model_ipron, size = 12, part = "all")
anova_ft_model_ipron <- autofit(anova_ft_model_ipron)
# Add the table to the Word document
doc_model_ipron <- body_add_flextable(doc_model_ipron, value = anova_ft_model_ipron)
# Save the Word document
print(doc_model_ipron, target = "anova_results_model_ipron.docx")
# Browse the created document
browseURL("anova_results_model_ipron.docx")




#######h3ee#######
# H3ee: Status messages contain less third-person pronouns than titles and subtitles.("shehe", "they", "ipron")

# Initialize lists to collect results for each third-person pronoun
shehe_status <- list()
they_status <- list()
ipron_status <- list()
shehe_headline <- list()
they_headline <- list()
ipron_headline <- list()
# Loop to calculate the average usage of each third-person pronoun for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average usage of each pronoun in status messages
  mean_shehe_status <- mean(as.numeric(status_current$shehe), na.rm = TRUE)
  mean_they_status <- mean(as.numeric(status_current$they), na.rm = TRUE)
  mean_ipron_status <- mean(as.numeric(status_current$ipron), na.rm = TRUE)
  shehe_status[[i]] <- mean_shehe_status
  they_status[[i]] <- mean_they_status
  ipron_status[[i]] <- mean_ipron_status
  # Calculate the average usage of each pronoun in headlines
  mean_shehe_headline <- mean(as.numeric(headlines_current$shehe), na.rm = TRUE)
  mean_they_headline <- mean(as.numeric(headlines_current$they), na.rm = TRUE)
  mean_ipron_headline <- mean(as.numeric(headlines_current$ipron), na.rm = TRUE)
  shehe_headline[[i]] <- mean_shehe_headline
  they_headline[[i]] <- mean_they_headline
  ipron_headline[[i]] <- mean_ipron_headline
}
# Create a dataframe for the results, combining all measures
third_pronoun_data <- data.frame(
  Newspaper = rep(newspapers, each = 3),
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 3),
  Type = rep(c("Pronoun She/He", "Pronoun They", "Pronoun It"), times = 9),
  Frequency_Status = c(unlist(shehe_status), unlist(they_status), unlist(ipron_status)),
  Frequency_Headline = c(unlist(shehe_headline), unlist(they_headline), unlist(ipron_headline))
)
# Melt the data for plotting
library(reshape2)
melted_third_pronoun_data <- melt(third_pronoun_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Define custom colors for visual distinction
custom_colors_third_pronouns <- c("Pronoun She/He" = "#568687",
                                  "Pronoun They" = "#80ae9a",
                                  "Pronoun It" = "#b5d1ae")
# Ensure that the categories are in the correct order
melted_third_pronoun_data$Category <- factor(melted_third_pronoun_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Adjust the 'Type' factor to specify the order of levels directly
melted_third_pronoun_data$Type <- factor(melted_third_pronoun_data$Type, levels = c("Pronoun She/He", "Pronoun They", "Pronoun It"))
# Create the plot
third_pronoun_plot <- ggplot(melted_third_pronoun_data, aes(x = Category, y = AverageValue, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~MessageType, scales = "free_y", labeller = labeller(MessageType = c(Frequency_Status = "Status Message", Frequency_Headline = "Headline"))) +
  labs(title = NULL,
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors_third_pronouns) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
print(third_pronoun_plot)
# Save the plot
ggsave("~/Desktop/plot/comparison_third_pronouns_usage.png", plot = third_pronoun_plot, dpi = 300, width = 20, height = 16, units = "cm")




#################################third pronoun new#############################################
library(ggplot2)
library(reshape2)
library(dplyr)
# Define and adjust levels for clear grouping and plotting
melted_third_pronoun_data$MessageType <- factor(melted_third_pronoun_data$MessageType,
                                                levels = c("Frequency_Headline", "Frequency_Status"),
                                                labels = c("Headline", "Status Message"))
# Create an interaction term for Fill aesthetic for clear color coding
melted_third_pronoun_data$Fill <- with(melted_third_pronoun_data, interaction(Type, MessageType, sep = ": "))
# Define custom colors matching the exact interaction terms for visual distinction
custom_colors_third_pronouns <- c(
  "Pronoun She/He: Headline" = "#568687", 
  "Pronoun She/He: Status Message" = "#16485e",  
  "Pronoun They: Headline" = "#80ae9a",  
  "Pronoun They: Status Message" = "#122740", 
  "Pronoun It: Headline" = "#b5d1ae",   
  "Pronoun It: Status Message" = "#326677"
)
library(ggplot2)
library(reshape2)
library(dplyr)
# Ordering the fill levels to ensure they appear in the desired sequence in the legend
melted_third_pronoun_data$Fill <- with(melted_third_pronoun_data, interaction(Type, MessageType, sep = ": "))
# Reorder the 'Fill' factor to the desired sequence
melted_third_pronoun_data$Fill <- factor(
  melted_third_pronoun_data$Fill,
  levels = c(
    "Pronoun She/He: Headline", "Pronoun She/He: Status Message",
    "Pronoun They: Headline", "Pronoun They: Status Message",
    "Pronoun It: Headline", "Pronoun It: Status Message"
  )
)
# Plotting
third_pronoun_plot1 <- ggplot(melted_third_pronoun_data, aes(x = Category, y = AverageValue, fill = Fill)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_y") +
  labs(title = NULL, x = "Newspaper Category", y = "Average Value") +
  scale_fill_manual(values = custom_colors_third_pronouns) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(title = "Pronoun and Type", reverse = FALSE))
# Display the plot
print(third_pronoun_plot1)
# Save the plot
ggsave("~/Desktop/plot/comparison_third_pronouns_usage1.png", plot = third_pronoun_plot1, dpi = 300, width = 24, height = 20, units = "cm")




#######h3f#######
#Status messages contain more punctuation than titles and subtitles. (AllPunc) 

# Initialize lists to collect results
punctuation_frequency_status <- list()
punctuation_frequency_headline <- list()
# Loop to calculate the average frequency of punctuation for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of punctuation in status messages
  mean_punc_status <- mean(status_current$AllPunc, na.rm = TRUE)
  punctuation_frequency_status[[i]] <- mean_punc_status
  # Calculate the average frequency of punctuation in headlines
  mean_punc_headline <- mean(headlines_current$AllPunc, na.rm = TRUE)
  punctuation_frequency_headline[[i]] <- mean_punc_headline
}
# Create a dataframe for the results
punctuation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Punctuation_Frequency_Status = unlist(punctuation_frequency_status),
  Punctuation_Frequency_Headline = unlist(punctuation_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
punctuation_frequency_melted <- melt(punctuation_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Punctuation")
# Display the original data frame to verify the inclusion of both types of data
print(punctuation_frequency_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(punctuation_frequency_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
punctuation_frequency_melted$Newspaper <- factor(punctuation_frequency_melted$Newspaper,
                                                 levels = c("Times", "Telegraph", "Guardian",
                                                            "Sun", "Daily Mail", "Daily Express",
                                                            "Economist", "Tribune", "Prospect"))
punctuation_frequency_melted$Type <- factor(punctuation_frequency_melted$Type, levels = c("Punctuation_Frequency_Headline", "Punctuation_Frequency_Status"))
# Custom colors, ensuring status messages are highlighted
custom_colors <- c("Punctuation_Frequency_Headline" = "#b5d1ae", "Punctuation_Frequency_Status" = "#326677")
# Create bar plot
plot_h3f <- ggplot(punctuation_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Punctuation`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL,
       x = "Newspaper",
       y = "Average Frequency of Punctuation") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
print(plot_h3f)
# Save the plot
ggsave("~/Desktop/plot/h3f.png", plot = plot_h3f, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
allpunc_frequency_status<-list()
allpunc_frequency_headline<-list()
#Loop to calculate the average frequency of AllPunc for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_allpunc_status<-mean(as.numeric(status_current$AllPunc),na.rm=TRUE)
  allpunc_frequency_status[[i]]<-mean_allpunc_status
  mean_allpunc_headline<-mean(as.numeric(headlines_current$AllPunc),na.rm=TRUE)
  allpunc_frequency_headline[[i]]<-mean_allpunc_headline
}
#Create a dataframe for the results
allpunc_frequency_df<-data.frame(Newspaper=newspapers,AllPunc_Frequency_Status=unlist(allpunc_frequency_status),AllPunc_Frequency_Headline=unlist(allpunc_frequency_headline))
#Create a new column for newspaper categories
allpunc_frequency_df$Category<-ifelse(allpunc_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(allpunc_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
allpunc_frequency_df$Category<-factor(allpunc_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_allpunc<-aggregate(cbind(AllPunc_Frequency_Status,AllPunc_Frequency_Headline)~Category,data=allpunc_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_allpunc<-melt(aggregated_data_allpunc,id.vars="Category",variable.name="Type",value.name="Average Frequency of AllPunc")
#Adjust Type factor levels to control plot order
aggregated_melted_allpunc$Type<-factor(aggregated_melted_allpunc$Type,levels=c("AllPunc_Frequency_Headline","AllPunc_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_allpunc<-c("AllPunc_Frequency_Headline"="#b5d1ae","AllPunc_Frequency_Status"="#326677")
#Create bar plot
plot_h3f_aggregated<-ggplot(aggregated_melted_allpunc,aes(x=Category,y=`Average Frequency of AllPunc`,fill=Type))+
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title=NULL,x="Newspaper Category",y="Average Frequency of Punctuation")+
  scale_fill_manual(values=custom_colors_allpunc,labels=c("Headline","Status Message"))+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.text = element_text(size=12),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
#Save the plot
ggsave("~/Desktop/plot/h3f_aggregated.png",plot=plot_h3f_aggregated,dpi=300,width=22,height=18,units="cm")


###### Beta regression for AllPunc (h3f) ######
# Histogram to examine distribution
hist(total$AllPunc)
# Boxplot to look at relationship with predictor variables
bwplot(AllPunc ~ media | type, total)
# Scale AllPunc to be between 0 and 1 and adjust for beta regression requirements
total$AllPunc1 <- (total$AllPunc * 100 - min(total$AllPunc * 100) + 0.5) / (max(total$AllPunc * 100) - min(total$AllPunc * 100) + 1)
summary(total$AllPunc1)
# Run the beta regression model with interaction
mh3f <- glmmTMB(AllPunc1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3f)
# Run the beta regression model without interaction
mh3f_g <- glmmTMB(AllPunc1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3f_g)
# Compare the two models
anova_h3f <- anova(mh3f, mh3f_g)




# Load necessary libraries
library(glmmTMB)
library(broom)
library(dplyr)
library(officer)
library(flextable)
# Run the beta regression model with interaction
mh3f <- glmmTMB(AllPunc1 ~ media * type, data = total, family = beta_family(link = "logit"))
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh3f <- tidy(mh3f)
# Format the p-values and coefficients
results_mh3f <- results_mh3f %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    )
# Create a table manually
results_table <- results_mh3f %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_h3f.docx")
# Browse the created document
browseURL("model_results_h3f.docx")



# Load necessary libraries
library(glmmTMB)
library(broom)
library(dplyr)
library(knitr)
# Run the beta regression model with interaction
mh3f <- glmmTMB(AllPunc1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary_mh3f <- summary(mh3f)
# Extract estimates, standard errors, z-values, and p-values
results_mh3f <- tidy(mh3f)
# Create a table manually
results_table <- results_mh3f %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Print the table in a markdown format
kable(results_table, format = "markdown", caption = "Results of the Beta Regression Model with Interaction (mh3f)")
# Save the table to a Word document
library(officer)
library(flextable)
# Create a flextable object
ft <- flextable(results_table)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft) %>%
  body_add_par("Results of the Beta Regression Model with Interaction (mh3f)", style = "heading 1")
# Save the document
print(doc, target = "model_results_h3f.docx")
# Browse the created document
browseURL("model_results_h3f.docx")


> # Load necessary libraries
> library(glmmTMB)
> library(broom)
> library(dplyr)
> library(knitr)
> 
> # Run the beta regression model with interaction
> mh3f <- glmmTMB(AllPunc1 ~ media * type, data = total, family = beta_family(link = "logit"))
> summary_mh3f <- summary(mh3f)
> 
> # Extract estimates, standard errors, z-values, and p-values
> results_mh3f <- tidy(mh3f)
> 
> # Create a table manually
> results_table <- results_mh3f %>%
+     select(term, estimate, std.error, statistic, p.value) %>%
+     rename(
+         `Term` = term,
+         `Estimate` = estimate,
+         `Std. Error` = std.error,
+         `z value` = statistic,
+         `Pr(>|z|)` = p.value
+     )
> 
> # Print the table in a markdown format
> kable(results_table, format = "markdown", caption = "Results of the Beta Regression Model with Interaction (mh3f)")


Table: Results of the Beta Regression Model with Interaction (mh3f)

|Term                     |   Estimate| Std. Error|    z value| Pr(>&#124;z&#124;)|
|:------------------------|----------:|----------:|----------:|------------------:|
|(Intercept)              | -2.0733089|  0.0583323| -35.543070|          0.0000000|
|mediamagazines           |  0.1044086|  0.0776269|   1.345006|          0.1786234|
|mediatabloids            |  0.3428963|  0.0767337|   4.468650|          0.0000079|
|typetweet                | -0.2903875|  0.0790008|  -3.675752|          0.0002371|
|mediamagazines:typetweet |  0.4522662|  0.1100834|   4.108395|          0.0000398|
|mediatabloids:typetweet  | -0.3077841|  0.1106647|  -2.781230|          0.0054153|
> 
> # Save the table to a Word document
> library(officer)
> library(flextable)
> 
> # Create a flextable object
> ft <- flextable(results_table)
> 
> # Create a Word document and add the flextable
> doc <- read_docx() %>%
+     body_add_flextable(value = ft) %>%
+     body_add_par("Results of the Beta Regression Model with Interaction (mh3f)", style = "heading 1")
> 
> # Save the document
> print(doc, target = "model_results_h3f.docx")
> 
> # Browse the created document
> browseURL("model_results_h3f.docx")
> 


# Create a table for the model mh3f_g
tab_model(mh3f, show.se = TRUE, show.stat = TRUE, show.p = TRUE, file = "model_results_h3f.doc")
# Browse the created document
browseURL("model_results_h3f.doc")

install.packages("texreg")
install.packages("kableExtra")

library(glmmTMB)
library(broom.mixed)
library(stargazer)
library(texreg)
library(dplyr)
library(kableExtra)
# Utilizza broom.mixed per ottenere i coefficienti del modello in un formato simile a summary
tidy_mh3f <- tidy(mh3f)
table <- tidy_mh3f %>%
  kable("html", col.names = c("Predictor", "Estimate", "Std. Error", "Statistic", "p-value"),
        format.args = list(decimal.mark = ".", big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "left") %>%
  row_spec(0, bold = TRUE) %>%
  font_size(size = 12) %>%
  font("Times New Roman")
# Visualizza il risultato del tidy
print(tidy_mh3f)
# Salva i risultati in un documento HTML con texreg
writeLines(table, "model_results_h3f.html")
browseURL("model_results_h3f.html")


tidy_mh3f <- tidy(mh3f)
table <- tidy_mh3f %>%
  kable("html", col.names = c("Term", "Estimate", "Std. Error", "Statistic", "p-value"),
        format.args = list(decimal.mark = ".", big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "left") %>%
  row_spec(0, bold = TRUE)
# Salva la tabella in un documento HTML
writeLines(table, "model_results_h3f.html")
# Visualizza il documento HTML creato
browseURL("model_results_h3f.html")



# Utilizza broom.mixed per ottenere i coefficienti del modello in un formato simile a summary
tidy_mh3f <- tidy(mh3f)
# Seleziona solo le colonne desiderate
tidy_mh3f <- tidy_mh3f %>%
  select(term, estimate, std.error, statistic, p.value)
# Arrotonda i valori a due decimali
tidy_mh3f <- tidy_mh3f %>%
  mutate(across(c(estimate, std.error, statistic, p.value), ~ round(., 2)))
# Formatta il p-value in grassetto se significativo
tidy_mh3f <- tidy_mh3f %>%
  mutate(p.value = ifelse(p.value < 0.05, paste0("<strong>", p.value, "</strong>"), as.character(p.value)))
# Crea una tabella HTML con kable
table <- tidy_mh3f %>%
  kable("html", col.names = c("Term", "Estimate", "Std. Error", "Statistic", "p-value"),
        escape = FALSE, format.args = list(decimal.mark = ".", big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "left") %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" ", "Model Results" = 4)) %>%
  add_footer_row(values = "Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05", colspan = 5)
# Salva la tabella in un documento HTML
writeLines(as.character(table), "model_results_h3f.html")
# Visualizza il documento HTML creato
browseURL("model_results_h3f.html")


# Convert the ANOVA results into a data frame
anova_table_h3f <- as.data.frame(anova_h3f)
anova_table_h3f$Model <- c("mh3f: adj1 ~ media * type", "mh3f_g: adj1 ~ media + type")
# Reorder columns to have the model at the beginning
anova_table_h3f <- anova_table_h3f[, c("Model", "Df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chi Df", "Pr(>Chisq)")]
# Create a Word document
doc_h3f <- read_docx()
# Create a table with flextable
anova_ft_h3f <- flextable(anova_table_h3f)
# Set the style of the table
anova_ft_h3f <- set_header_labels(anova_ft_h3f, Model = "Model", Df = "Df", AIC = "AIC", BIC = "BIC", logLik = "logLik", deviance = "deviance", Chisq = "Chisq", `Chi Df` = "Chi Df", `Pr(>Chisq)` = "Pr(>Chisq)")
anova_ft_h3f <- font(anova_ft_h3f, fontname = "Times New Roman", part = "all")
anova_ft_h3f <- fontsize(anova_ft_h3f, size = 12, part = "all")
anova_ft_h3f <- autofit(anova_ft_h3f)
# Add the table to the Word document
doc_h3f <- body_add_flextable(doc_h3f, value = anova_ft_h3f)
# Save the Word document
print(doc_h3f, target = "anova_results_h3f.docx")
# Browse the created document
browseURL("anova_results_h3f.docx")


# Visualize model predictions
library(ggeffects)
pred_h3f <- ggpredict(mh3f_g, terms = c("media", "type"))
h3f_plot_pred <- plot(pred_h3f, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3f_plot_pred.png", plot = h3f_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3f_other#######
#Status messages contain more different punctuation than titles and subtitles.(OtherP)

# Initialize lists to collect results
punctuation_frequency_status <- list()
punctuation_frequency_headline <- list()
# Loop to calculate the average frequency of different punctuation marks for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of different punctuation marks in status messages
  freq_punc_status <- mean(as.numeric(status_current$OtherP), na.rm = TRUE)
  punctuation_frequency_status[[i]] <- freq_punc_status
  # Calculate the average frequency of different punctuation marks in headlines
  freq_punc_headline <- mean(as.numeric(headlines_current$OtherP), na.rm = TRUE)
  punctuation_frequency_headline[[i]] <- freq_punc_headline
}
# Create a dataframe for the results
punctuation_frequency_df <- data.frame(
  Newspaper = newspapers,
  Punctuation_Frequency_Status = unlist(punctuation_frequency_status),
  Punctuation_Frequency_Headline = unlist(punctuation_frequency_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
punctuation_frequency_melted <- melt(punctuation_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Punctuation Frequency")
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
punctuation_frequency_melted$Newspaper <- factor(punctuation_frequency_melted$Newspaper,
                                                 levels = c("Times", "Telegraph", "Guardian",
                                                            "Sun", "Daily Mail", "Daily Express",
                                                            "Economist", "Tribune", "Prospect"))
punctuation_frequency_melted$Type <- factor(punctuation_frequency_melted$Type, levels = c("Punctuation_Frequency_Headline", "Punctuation_Frequency_Status"))
# Custom colors, ensuring status messages are highlighted
custom_colors <- c("Punctuation_Frequency_Headline" = "#85C1E9", "Punctuation_Frequency_Status" = "#E59866")
# Create bar plot
plot_h3f_other <- ggplot(punctuation_frequency_melted, aes(x = Newspaper, y = `Average Punctuation Frequency`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Punctuation Frequency: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Average Punctuation Frequency") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h3f_other.png", plot = plot_h3f_other, dpi = 300, width = 20, height = 16, units = "cm")


#Initialize lists to collect results
otherp_frequency_status<-list()
otherp_frequency_headline<-list()
#Loop to calculate the average frequency of OtherP for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_otherp_status<-mean(as.numeric(status_current$OtherP),na.rm=TRUE)
  otherp_frequency_status[[i]]<-mean_otherp_status
  mean_otherp_headline<-mean(as.numeric(headlines_current$OtherP),na.rm=TRUE)
  otherp_frequency_headline[[i]]<-mean_otherp_headline
}
#Create a dataframe for the results
otherp_frequency_df<-data.frame(Newspaper=newspapers,OtherP_Frequency_Status=unlist(otherp_frequency_status),OtherP_Frequency_Headline=unlist(otherp_frequency_headline))
#Create a new column for newspaper categories
otherp_frequency_df$Category<-ifelse(otherp_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(otherp_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
otherp_frequency_df$Category<-factor(otherp_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_otherp<-aggregate(cbind(OtherP_Frequency_Status,OtherP_Frequency_Headline)~Category,data=otherp_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_otherp<-melt(aggregated_data_otherp,id.vars="Category",variable.name="Type",value.name="Average Frequency of OtherP")
#Adjust Type factor levels to control plot order
aggregated_melted_otherp$Type<-factor(aggregated_melted_otherp$Type,levels=c("OtherP_Frequency_Headline","OtherP_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_otherp<-c("OtherP_Frequency_Headline"="#85C1E9","OtherP_Frequency_Status"="#E59866")
#Create bar plot
plot_h3f_other_aggregated<-ggplot(aggregated_melted_otherp,aes(x=Category,y=`Average Frequency of OtherP`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of OtherP: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of OtherP")+scale_fill_manual(values=custom_colors_otherp,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h3f_other_aggregated.png",plot=plot_h3f_other_aggregated,dpi=300,width=22,height=18,units="cm")


###### Beta regression for OtherP (h3f_other) ######
# Histogram to examine distribution
hist(total$OtherP)
# Boxplot to look at relationship with predictor variables
bwplot(OtherP ~ media | type, total)
# Scale OtherP to be between 0 and 1 and adjust for beta regression requirements
total$OtherP1 <- (total$OtherP / 100) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3f_other <- glmmTMB(OtherP1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3f_other)
# Run the beta regression model without interaction
mh3f_other_g <- glmmTMB(OtherP1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3f_other_g)
# Compare the two models
anova(mh3f_other, mh3f_other_g)
# Visualize model predictions
library(ggeffects)
pred_h3f_other <- ggpredict(mh3f_other_g, terms = c("media", "type"))
h3f_other_plot_pred <- plot(pred_h3f_other, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3f_other_plot_pred.png", plot = h3f_other_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


#######h3f_apostro#######
#Status messages contain more different punctuation than titles and subtitles.(Apostro)

# Initialize lists to collect results
apostro_frequency_status <- list()
apostro_frequency_headline <- list()
# Loop to calculate the average frequency of apostrophes for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average frequency of apostrophes in status messages
  mean_apostro_status <- mean(as.numeric(status_current$Apostro), na.rm = TRUE)
  apostro_frequency_status[[i]] <- mean_apostro_status
  # Calculate the average frequency of apostrophes in headlines
  mean_apostro_headline <- mean(as.numeric(headlines_current$Apostro), na.rm = TRUE)
  apostro_frequency_headline[[i]] <- mean_apostro_headline
}
# Create a dataframe for the results
apostro_frequency_df <- data.frame(
  Newspaper = newspapers,
  Apostro_Frequency_Status = unlist(apostro_frequency_status),
  Apostro_Frequency_Headline = unlist(apostro_frequency_headline)
)
# Melt the data for plotting
apostro_frequency_melted <- melt(apostro_frequency_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Average Frequency of Apostro")
# Load ggplot2 for plotting
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
apostro_frequency_melted$Newspaper <- factor(apostro_frequency_melted$Newspaper,
                                             levels = c("Times", "Telegraph", "Guardian",
                                                        "Sun", "Daily Mail", "Daily Express",
                                                        "Economist", "Tribune", "Prospect"))
apostro_frequency_melted$Type <- factor(apostro_frequency_melted$Type, levels = c("Apostro_Frequency_Headline", "Apostro_Frequency_Status"))
# Custom colors, ensuring apostrophes are highlighted
custom_colors <- c("Apostro_Frequency_Headline" = "#85C1E9", "Apostro_Frequency_Status" = "#E59866")
# Create bar plot
plot_h3f_apostro <- ggplot(apostro_frequency_melted, aes(x = Newspaper, y = `Average Frequency of Apostro`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Frequency of Apostrophes: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Average Frequency of Apostro") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h3f_apostro.png", plot = plot_h3f_apostro, dpi = 300, width = 20, height = 16, units = "cm")



#Initialize lists to collect results
apostro_frequency_status<-list()
apostro_frequency_headline<-list()
#Loop to calculate the average frequency of Apostro for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_apostro_status<-mean(as.numeric(status_current$Apostro),na.rm=TRUE)
  apostro_frequency_status[[i]]<-mean_apostro_status
  mean_apostro_headline<-mean(as.numeric(headlines_current$Apostro),na.rm=TRUE)
  apostro_frequency_headline[[i]]<-mean_apostro_headline
}
#Create a dataframe for the results
apostro_frequency_df<-data.frame(Newspaper=newspapers,Apostro_Frequency_Status=unlist(apostro_frequency_status),Apostro_Frequency_Headline=unlist(apostro_frequency_headline))
#Create a new column for newspaper categories
apostro_frequency_df$Category<-ifelse(apostro_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(apostro_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
apostro_frequency_df$Category<-factor(apostro_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_apostro<-aggregate(cbind(Apostro_Frequency_Status,Apostro_Frequency_Headline)~Category,data=apostro_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_apostro<-melt(aggregated_data_apostro,id.vars="Category",variable.name="Type",value.name="Average Frequency of Apostro")
#Adjust Type factor levels to control plot order
aggregated_melted_apostro$Type<-factor(aggregated_melted_apostro$Type,levels=c("Apostro_Frequency_Headline","Apostro_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_apostro<-c("Apostro_Frequency_Headline"="#85C1E9","Apostro_Frequency_Status"="#E59866")
#Create bar plot
plot_h3f_apostro_aggregated<-ggplot(aggregated_melted_apostro,aes(x=Category,y=`Average Frequency of Apostro`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of Apostrophes: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of Apostro")+scale_fill_manual(values=custom_colors_apostro,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h3f_apostro_aggregated.png",plot=plot_h3f_apostro_aggregated,dpi=300,width=22,height=18,units="cm")


###### Beta regression for Apostro (h3f_apostro) ######
# Histogram to examine distribution
hist(total$Apostro)
# Boxplot to look at relationship with predictor variables
bwplot(Apostro ~ media | type, total)
# Scale Apostro to be between 0 and 1 and adjust for beta regression requirements
total$Apostro1 <- (total$Apostro / 50) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3f_apostro <- glmmTMB(Apostro1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3f_apostro)
# Run the beta regression model without interaction
mh3f_apostro_g <- glmmTMB(Apostro1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3f_apostro_g)
# Compare the two models
anova(mh3f_apostro, mh3f_apostro_g)
# Visualize model predictions
library(ggeffects)
pred_h3f_apostro <- ggpredict(mh3f_apostro_g, terms = c("media", "type"))
h3f_apostro_plot_pred <- plot(pred_h3f_apostro, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3f_apostro_plot_pred.png", plot = h3f_apostro_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



#######h3f_levels#######
#Status messages present different levels of punctuation than titles and subtitles. (AllPunc,Period,Comma,QMark,Exclam,Apostro,OtherP)

# Initialize lists to collect results for each punctuation type
allpunc_status <- list()
period_status <- list()
comma_status <- list()
qmark_status <- list()
exclam_status <- list()
apostro_status <- list()
otherp_status <- list()
allpunc_headline <- list()
period_headline <- list()
comma_headline <- list()
qmark_headline <- list()
exclam_headline <- list()
apostro_headline <- list()
otherp_headline <- list()
# Loop to calculate the average usage of each punctuation mark for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the average usage of each punctuation mark in status messages
  mean_allpunc_status <- mean(as.numeric(status_current$AllPunc), na.rm = TRUE)
  mean_period_status <- mean(as.numeric(status_current$Period), na.rm = TRUE)
  mean_comma_status <- mean(as.numeric(status_current$Comma), na.rm = TRUE)
  mean_qmark_status <- mean(as.numeric(status_current$QMark), na.rm = TRUE)
  mean_exclam_status <- mean(as.numeric(status_current$Exclam), na.rm = TRUE)
  mean_apostro_status <- mean(as.numeric(status_current$Apostro), na.rm = TRUE)
  mean_otherp_status <- mean(as.numeric(status_current$OtherP), na.rm = TRUE)
  allpunc_status[[i]] <- mean_allpunc_status
  period_status[[i]] <- mean_period_status
  comma_status[[i]] <- mean_comma_status
  qmark_status[[i]] <- mean_qmark_status
  exclam_status[[i]] <- mean_exclam_status
  apostro_status[[i]] <- mean_apostro_status
  otherp_status[[i]] <- mean_otherp_status
  # Calculate the average usage of each punctuation mark in headlines
  mean_allpunc_headline <- mean(as.numeric(headlines_current$AllPunc), na.rm = TRUE)
  mean_period_headline <- mean(as.numeric(headlines_current$Period), na.rm = TRUE)
  mean_comma_headline <- mean(as.numeric(headlines_current$Comma), na.rm = TRUE)
  mean_qmark_headline <- mean(as.numeric(headlines_current$QMark), na.rm = TRUE)
  mean_exclam_headline <- mean(as.numeric(headlines_current$Exclam), na.rm = TRUE)
  mean_apostro_headline <- mean(as.numeric(headlines_current$Apostro), na.rm = TRUE)
  mean_otherp_headline <- mean(as.numeric(headlines_current$OtherP), na.rm = TRUE)
  allpunc_headline[[i]] <- mean_allpunc_headline
  period_headline[[i]] <- mean_period_headline
  comma_headline[[i]] <- mean_comma_headline
  qmark_headline[[i]] <- mean_qmark_headline
  exclam_headline[[i]] <- mean_exclam_headline
  apostro_headline[[i]] <- mean_apostro_headline
  otherp_headline[[i]] <- mean_otherp_headline
}
# Create a dataframe for the results, combining all measures
punctuation_data <- data.frame(
  Newspaper = rep(newspapers, each = 7),
  Category = rep(ifelse(newspapers %in% c("Times", "Telegraph", "Guardian"), "Broadsheets",
                        ifelse(newspapers %in% c("Sun", "Daily Mail", "Daily Express"), "Tabloids", "Magazines")), each = 7),
  Type = rep(c("All Punctuation", "Period", "Comma", "Question Mark", "Exclamation Mark", "Apostrophe", "Other Punctuation"), times = 9),
  Frequency_Status = c(unlist(allpunc_status), unlist(period_status), unlist(comma_status), unlist(qmark_status), unlist(exclam_status), unlist(apostro_status), unlist(otherp_status)),
  Frequency_Headline = c(unlist(allpunc_headline), unlist(period_headline), unlist(comma_headline), unlist(qmark_headline), unlist(exclam_headline), unlist(apostro_headline), unlist(otherp_headline))
)
# Melt the data for plotting
library(reshape2)
melted_punctuation_data <- melt(punctuation_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Define custom colors for visual distinction
custom_colors_punctuation <- c("All Punctuation" = "#6A040F", "Period" = "#122740", "Comma" = "#16485e",
                               "Question Mark" = "#326677", "Exclamation Mark" = "#568687", "Apostrophe" = "#80ae9a", "Other Punctuation" = "#b5d1ae")
# Ensure that the categories are in the correct order
melted_punctuation_data$Category <- factor(melted_punctuation_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Adjust the 'Type' factor to specify the order of levels directly
melted_punctuation_data$Type <- factor(melted_punctuation_data$Type, levels = c("All Punctuation", "Period", "Comma", "Question Mark", "Exclamation Mark", "Apostrophe", "Other Punctuation"))
# Create the plot
punctuation_plot <- ggplot(melted_punctuation_data, aes(x = Category, y = AverageValue, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~MessageType, scales = "free_y", labeller = labeller(MessageType = c(Frequency_Status = "Status Message", Frequency_Headline = "Headline"))) +
  labs(title = NULL,
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors_punctuation) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # Centered and larger title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 12),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=12))+  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
# Save the plot
ggsave("~/Desktop/plot/comparison_punctuation_usage.png", plot = punctuation_plot, dpi = 300, width = 24, height = 20, units = "cm")


#######################New comparison punctuation#################################

library(reshape2)
library(ggplot2)
melted_punctuation_data <- melt(punctuation_data, id.vars = c("Newspaper", "Category", "Type"), variable.name = "MessageType", value.name = "AverageValue")
# Define and adjust levels for clear grouping in plot
melted_punctuation_data$MessageType <- factor(melted_punctuation_data$MessageType,
                                              levels = c("Frequency_Headline", "Frequency_Status"),
                                              labels = c("Headline", "Status Message"))
melted_punctuation_data$Category <- factor(melted_punctuation_data$Category, levels = c("Broadsheets", "Tabloids", "Magazines"))
# Create a combined column for type and message type interaction to ensure side-by-side plotting
melted_punctuation_data$Interaction <- with(melted_punctuation_data, interaction(Type, MessageType, sep = " - "))
# Create an interaction term for Fill aesthetic
melted_punctuation_data$Interaction <- factor(
  melted_punctuation_data$Interaction,
  levels = c(
    "All Punctuation - Headline", "All Punctuation - Status Message",
    "Period - Headline", "Period - Status Message",
    "Comma - Headline", "Comma - Status Message",
    "Question Mark - Headline", "Question Mark - Status Message",
    "Exclamation Mark - Headline", "Exclamation Mark - Status Message",
    "Apostrophe - Headline", "Apostrophe - Status Message",
    "Other Punctuation - Headline", "Other Punctuation - Status Message"))
# Define custom colors, possibly extended to differentiate between status and headline for each type
custom_colors_punctuation <- c(
  "All Punctuation - Headline" = "#c8e7ea",  # Soft aquamarine
  "All Punctuation - Status Message" = "#68b9bf",  # Deep aquamarine
  "Period - Headline" = "#f6d8b1",  # Soft apricot
  "Period - Status Message" = "#d68a55",  # Deep apricot
  "Comma - Headline" = "#f3aee4",  # Soft orchid
  "Comma - Status Message" = "#bd75b2",  # Deep orchid
  "Question Mark - Headline" = "#ded9f6",  # Soft lilac
  "Question Mark - Status Message" = "#8a7cbf",  # Deep lilac
  "Exclamation Mark - Headline" = "#f6c8c6",  # Soft rose blush
  "Exclamation Mark - Status Message" = "#ca7371",  # Deep rose blush
  "Apostrophe - Headline" = "#d8f2c7",  # Soft periwinkle
  "Apostrophe - Status Message" = "#8ab07e",  # Deep periwinkle
  "Other Punctuation - Headline" = "#e6d6a7",  # Soft lime green e6d6a7
  "Other Punctuation - Status Message" = "#b3a56f"  # Deep lime green b3a56f
)

# Plotting
punctuation_plot1 <- ggplot(melted_punctuation_data, aes(x = Category, y = AverageValue, fill = Interaction)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # Adjust dodge width for clear separation
  labs(title = NULL,
       x = "Newspaper Category",
       y = "Average Value") +
  scale_fill_manual(values = custom_colors_punctuation) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate and adjust X-axis labels
        axis.text.y = element_text(size = 14),  # Adjust Y-axis labels
        axis.title = element_text(size = 14),  # Axis titles
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom",
        legend.text = element_text(size=14)) +  # Move legend to the bottom
  guides(fill = guide_legend(reverse = FALSE))
# Display the plot
print(punctuation_plot1)
# Save the plot
ggsave("~/Desktop/plot/comparison_punctuation_usage1.png", plot = punctuation_plot1, dpi = 300, width = 39, height = 17, units = "cm")


#######h3g#######
#Status messages contain more conversational language than titles and subtitles (Conversation). 

# Initialize lists to collect results
conversational_language_status <- list()
conversational_language_headline <- list()
# Loop to calculate the sum of conversational language occurrences for each newspaper
for (i in 1:9) {
  # Filter status messages for the current media
  status_current <- data_tweet[data_tweet$media == i, ]
  # Filter headlines for the current media
  headlines_current <- data_title[data_title$media == i, ]
  # Calculate the sum of conversational language occurrences in status messages
  conv_lang_status <- sum(status_current$Conversation, na.rm = TRUE)
  conversational_language_status[[i]] <- conv_lang_status
  # Calculate the sum of conversational language occurrences in headlines
  conv_lang_headline <- sum(headlines_current$Conversation, na.rm = TRUE)
  conversational_language_headline[[i]] <- conv_lang_headline
}
# Create a dataframe for the results
conversational_language_df <- data.frame(
  Newspaper = newspapers,
  Conversational_Language_Status = unlist(conversational_language_status),
  Conversational_Language_Headline = unlist(conversational_language_headline)
)
# Load the 'reshape2' package for melting the data
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
# Melt the data for plotting
conversational_language_melted <- melt(conversational_language_df, id.vars = "Newspaper", variable.name = "Type", value.name = "Conversational Language Count")
# Display the original data frame to verify the inclusion of both types of data
print(conversational_language_df)
# Display the first few rows of the melted data to verify correct inclusion
print(head(conversational_language_melted, 18))
# Load required libraries
library(ggplot2)
# Setting factor levels for Newspaper and Type to ensure correct order and legend labels
conversational_language_melted$Newspaper <- factor(conversational_language_melted$Newspaper,
                                                   levels = c("Times", "Telegraph", "Guardian",
                                                              "Sun", "Daily Mail", "Daily Express",
                                                              "Economist", "Tribune", "Prospect"))
conversational_language_melted$Type <- factor(conversational_language_melted$Type, levels = c("Conversational_Language_Headline", "Conversational_Language_Status"))
# Custom colors, ensuring status messages are highlighted
custom_colors <- c("Conversational_Language_Headline" = "#85C1E9", "Conversational_Language_Status" = "#E59866")
# Create bar plot
plot_h3g <- ggplot(conversational_language_melted, aes(x = Newspaper, y = `Conversational Language Count`, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conversational Language Count: Headlines vs. Status Messages by Newspaper",
       x = "Newspaper",
       y = "Conversational Language Count") +
  scale_fill_manual(values = custom_colors, labels = c("Headline", "Status Message")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot
ggsave("~/Desktop/plot/h3g.png", plot = plot_h3g, dpi = 300, width = 20, height = 16, units = "cm")

#Initialize lists to collect results
conversation_frequency_status<-list()
conversation_frequency_headline<-list()
#Loop to calculate the average frequency of Conversation for each newspaper
for(i in 1:9){
  status_current<-data_tweet[data_tweet$media==i,]
  headlines_current<-data_title[data_title$media==i,]
  mean_conversation_status<-mean(as.numeric(status_current$Conversation),na.rm=TRUE)
  conversation_frequency_status[[i]]<-mean_conversation_status
  mean_conversation_headline<-mean(as.numeric(headlines_current$Conversation),na.rm=TRUE)
  conversation_frequency_headline[[i]]<-mean_conversation_headline
}
#Create a dataframe for the results
conversation_frequency_df<-data.frame(Newspaper=newspapers,Conversation_Frequency_Status=unlist(conversation_frequency_status),Conversation_Frequency_Headline=unlist(conversation_frequency_headline))
#Create a new column for newspaper categories
conversation_frequency_df$Category<-ifelse(conversation_frequency_df$Newspaper%in%c("Times","Telegraph","Guardian"),"Broadsheets",ifelse(conversation_frequency_df$Newspaper%in%c("Sun","Daily Mail","Daily Express"),"Tabloids","Magazines"))
#Ensure the category ordering
conversation_frequency_df$Category<-factor(conversation_frequency_df$Category,levels=c("Broadsheets","Tabloids","Magazines"))
#Aggregate data by category
aggregated_data_conversation<-aggregate(cbind(Conversation_Frequency_Status,Conversation_Frequency_Headline)~Category,data=conversation_frequency_df,FUN=mean,na.rm=TRUE)
#Melt the aggregated data for plotting
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
aggregated_melted_conversation<-melt(aggregated_data_conversation,id.vars="Category",variable.name="Type",value.name="Average Frequency of Conversation")
#Adjust Type factor levels to control plot order
aggregated_melted_conversation$Type<-factor(aggregated_melted_conversation$Type,levels=c("Conversation_Frequency_Headline","Conversation_Frequency_Status"))
#Load required libraries
library(ggplot2)
custom_colors_conversation<-c("Conversation_Frequency_Headline"="#85C1E9","Conversation_Frequency_Status"="#E59866")
#Create bar plot
plot_h3g_aggregated<-ggplot(aggregated_melted_conversation,aes(x=Category,y=`Average Frequency of Conversation`,fill=Type))+geom_bar(stat="identity",position="dodge")+labs(title="Average Frequency of Conversation: Headlines vs. Status Messages by Newspaper Category",x="Newspaper Category",y="Average Frequency of Conversation")+scale_fill_manual(values=custom_colors_conversation,labels=c("Headline","Status Message"))+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
#Save the plot
ggsave("~/Desktop/plot/h3g_aggregated.png",plot=plot_h3g_aggregated,dpi=300,width=22,height=18,units="cm")


###### Beta regression for Conversation (h3g) ######
# Histogram to examine distribution
hist(total$Conversation)
# Boxplot to look at relationship with predictor variables
bwplot(Conversation ~ media | type, total)
# Scale Conversation to be between 0 and 1 and adjust for beta regression requirements
total$Conversation1 <- (total$Conversation / 10) * 0.98 + 0.01
# Run the beta regression model with interaction
mh3g <- glmmTMB(Conversation1 ~ media * type, data = total, family = beta_family(link = "logit"))
summary(mh3g)
# Run the beta regression model without interaction
mh3g_g <- glmmTMB(Conversation1 ~ media + type, data = total, family = beta_family(link = "logit"))
summary(mh3g_g)
# Compare the two models
anova(mh3g, mh3g_g)
# Visualize model predictions
library(ggeffects)
pred_h3g <- ggpredict(mh3g_g, terms = c("media", "type"))
h3g_plot_pred <- plot(pred_h3g, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1)
# Save the plot
ggsave("~/Desktop/plot/h3g_plot_pred.png", plot = h3g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



######################Other R tests: Maybe?######################

##tone_pos with no significant p-value
# Loading necessary library
library(tidyverse)
# Example data simulation
set.seed(123)
tweets_data <- data.frame(
  tone_pos = rlnorm(100, meanlog = 0, sdlog = 1)  # Log-normal distribution, simulate some data
)
titles_data <- data.frame(
  tone_pos = rlnorm(100, meanlog = 0.1, sdlog = 1)  # Slightly different parameters
)
# Adjusting for zeros if any (adding a small constant just in case)
tweets_data$tone_pos_transformed <- log(tweets_data$tone_pos + 0.01)
titles_data$tone_pos_transformed <- log(titles_data$tone_pos + 0.01)
# Visualize the distribution of the transformed data
par(mfrow = c(1, 2))
hist(tweets_data$tone_pos_transformed, main = "Transformed Tone Pos in Tweets", xlab = "Log(Tone Pos)", col = "skyblue")
hist(titles_data$tone_pos_transformed, main = "Transformed Tone Pos in Titles", xlab = "Log(Tone Pos)", col = "salmon")
# Perform a t-test on the transformed data
t_test_results <- t.test(tweets_data$tone_pos_transformed, titles_data$tone_pos_transformed, alternative = "two.sided", var.equal = FALSE)
# Print the results of the t-test
print(t_test_results)



##emo_pos with no significant p-value
# Loading necessary library for tidyverse functions (if needed)
library(tidyverse)
# Assuming you have two data frames: tweets_data and titles_data
# Both data frames contain a column named 'emo_pos'
# Example data (please replace this with your actual data)
# Simulated smaller sample sizes
set.seed(123)
tweets_data <- data.frame(
  emo_pos = runif(30, min = 0, max = 1)  # Uniform distribution for simulation
)
titles_data <- data.frame(
  emo_pos = runif(30, min = 0, max = 1)  # Uniform distribution for simulation
)
# Perform the Mann-Whitney U Test with exact calculation
test_result <- wilcox.test(tweets_data$emo_pos, titles_data$emo_pos,
                           alternative = "two.sided", # Can be "greater" or "less" if you have a directional hypothesis
                           exact = TRUE,              # Use exact method due to small sample size
                           correct = FALSE)           # Continuity correction not needed when using exact method
# Print the results
print(test_result)
# Optional: Creating a boxplot to visualize the distributions
boxplot(tweets_data$emo_pos, titles_data$emo_pos, names = c("Tweets", "Titles"),
        main = "Boxplot of Positive Emotion in Tweets vs. Titles",
        ylab = "Positive Emotion (emo_pos)", col = c("skyblue", "salmon"))



#numbers in tweets and titles
# Loading necessary library
library(tidyverse)
# Assuming you have two data frames: tweets_data and titles_data
# Both data frames contain a column named 'number'
# Example data simulation (replace this with your actual data)
set.seed(123)
tweets_data <- data.frame(
  number = rpois(30, lambda = 3)  # Poisson distribution for simulation, assuming count data
)
titles_data <- data.frame(
  number = rpois(30, lambda = 2)  # Slightly different parameter
)
# Perform the Wilcoxon Rank Sum Test with exact calculation due to small sample size
test_result <- wilcox.test(tweets_data$number, titles_data$number,
                           alternative = "two.sided",  # Adjust if you expect one to be greater or less specifically
                           exact = TRUE,               # Use the exact method for small sample sizes
                           correct = FALSE)            # Continuity correction is not used with the exact method
# Print the results
print(test_result)
# Optional: Creating a boxplot to visualize the distributions
boxplot(tweets_data$number, titles_data$number, names = c("Tweets", "Titles"),
        main = "Boxplot of Number Usage in Tweets vs. Titles",
        ylab = "Number Usage", col = c("skyblue", "salmon"))
# Perform the Wilcoxon Rank Sum Test without specifying exact calculation
test_result <- wilcox.test(tweets_data$number, titles_data$number,
                           alternative = "two.sided",  # Adjust if you expect one to be greater or less specifically
                           correct = TRUE)             # Uses a continuity correction to handle ties
# Print the results
print(test_result)
# Optional: Creating a boxplot to visualize the distributions
boxplot(tweets_data$number, titles_data$number, names = c("Tweets", "Titles"),
        main = "Boxplot of Number Usage in Tweets vs. Titles",
        ylab = "Number Usage", col = c("skyblue", "salmon"))

# Define a function to perform a permutation test
perm_test <- function(x, y, n.perm = 1000) {
  all_data <- c(x, y)
  group_indices <- c(rep(1, length(x)), rep(2, length(y)))
  observed_diff <- abs(mean(x) - mean(y))
  count <- 0
  for (i in 1:n.perm) {
    shuffled_indices <- sample(group_indices)
    perm_x <- all_data[shuffled_indices == 1]
    perm_y <- all_data[shuffled_indices == 2]
    perm_diff <- abs(mean(perm_x) - mean(perm_y))
    if (perm_diff >= observed_diff) {
      count <- count + 1
    }
  }
  
  p_value <- count / n.perm
  return(p_value)
}
# Apply permutation test
perm_p_value <- perm_test(tweets_data$number, titles_data$number, n.perm = 10000)
print(perm_p_value)


#adverbs in tweets vs titles
# Loading necessary libraries
library(tidyverse)
# Assuming you have two data frames: tweets_data and titles_data
# Both data frames contain a column named 'adverb'
# Example data (replace this with your actual data)
set.seed(123)
tweets_data <- data.frame(
  adverb = rpois(50, lambda = 3)  # Poisson distribution for simulation, assuming count data
)
titles_data <- data.frame(
  adverb = rpois(50, lambda = 2)  # Slightly different parameter
)
# Perform the Wilcoxon Rank Sum Test without specifying exact calculation
test_result <- wilcox.test(tweets_data$adverb, titles_data$adverb,
                           alternative = "two.sided",  # Can be "greater" or "less" if you have a directional hypothesis
                           correct = TRUE)             # Uses a continuity correction to handle ties
# Print the results
print(test_result)
# Optional: Creating a boxplot to visualize the distributions
boxplot(tweets_data$adverb, titles_data$adverb, names = c("Tweets", "Titles"),
        main = "Boxplot of Adverb Usage in Tweets vs. Titles",
        ylab = "Adverb Usage", col = c("skyblue", "salmon"))


##adverb use in newspaper, tabloids, and magazines (only one p-value significant)
# Loading necessary library
library(tidyverse)
# Assuming your full data frame contains columns 'adverb' and 'media'
# Example data
set.seed(123)
data <- data.frame(
  media = rep(1:9, each = 50),
  adverb = rpois(450, lambda = sample(2:5, 450, replace = TRUE))  # Varied lambda for variety
)
# Map media numbers to their respective categories
media_categories <- c("Newspaper", "Newspaper", "Newspaper", 
                      "Tabloid", "Tabloid", "Tabloid", 
                      "Magazine", "Magazine", "Magazine")
names(media_categories) <- 1:9
# Add a category column based on media numbers
data$category <- media_categories[as.character(data$media)]
# Function to perform tests and plot within each category
analyze_category <- function(category_name) {
  # Filter data for the category
  category_data <- filter(data, category == category_name)
  # Perform Wilcoxon Rank Sum Test for all pairwise comparisons within the category
  combn(unique(category_data$media), 2, function(pair) {
    test_result <- wilcox.test(adverb ~ media,
                               data = category_data[category_data$media %in% pair, ],
                               exact = FALSE,
                               correct = TRUE)
    cat("Comparison between", pair[1], "and", pair[2], "\n")
    print(test_result)
  })
  # Creating a boxplot to visualize the distributions
  ggplot(category_data, aes(x = as.factor(media), y = adverb, fill = as.factor(media))) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set3") +
    labs(title = paste("Adverb Usage in", category_name),
         x = "Media Type",
         y = "Adverb Usage")
}
# Run analysis for each category
newspaper_plot <- analyze_category("Newspaper")
tabloid_plot <- analyze_category("Tabloid")
magazine_plot <- analyze_category("Magazine")
# Display plots
print(newspaper_plot)
print(tabloid_plot)
print(magazine_plot)

######################total PCA visualization######################
# visualisations
# Load necessary libraries
if (!require("factoextra")) {
  install.packages("factoextra")
}
library(factoextra)
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
# compute pca
pc <- prcomp(total[,7:124],scale=T)
# look at pca summary
summary(pc)
# look at loadings
round(pc$rotation,3)
# visualise percentage of total variance explained by each component
fviz_eig(pc) 
fviz_pca_ind(pc,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pc,col.ind=total$media,axes=c(3,4)) # show different axes

dim_pca <- fviz_pca_var(pc,
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
ggsave("~/Desktop/plot/dim_pca.png", plot = dim_pca, dpi = 300, width = 20, height = 16, units = "cm")


######################PCA for Emotionality######################
numeric_cols_emo <- total[, c("Tone","Affect","tone_pos","tone_neg",
                              "emotion","emo_pos","emo_neg", "Emoji", 
                              "Exclam", "adj")]
total_pca_emo <- scale(numeric_cols_emo)
pca_result_emo <- prcomp(total_pca_emo,scale=T)

summary(pca_result_emo)
biplot(pca_result_emo)
plot(pca_result_emo)
prcomp(total)
print(pca_result_emo)

fviz_eig(pca_result_emo) 
fviz_pca_ind(pca_result_emo,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_emo,col.ind=total$media,axes=c(3,4)) 
dim_pca_emo <- fviz_pca_var(pca_result_emo,
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_emo <- dim_pca_emo +
  theme(text = element_text(size = 14),  # Adjust overall text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  ggtitle("PCA Emotionality") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold"))
# Save the enhanced PCA plot
print(total$dim_pca_emo)
ggsave("~/Desktop/plot/dim_pca_emo.png", plot = dim_pca_emo, dpi = 300, width = 20, height = 16, units = "cm")



######################PCA for Emotionality: Status messages######################
numeric_cols_emo_s <- data_tweet[, c("Tone","tone_pos","tone_neg",
                              "emo_pos","emo_neg", "Emoji", 
                              "Exclam", "adj")]
total_pca_emo_s <- scale(numeric_cols_emo_s)
pca_result_emo_s <- prcomp(total_pca_emo_s,scale=T)

summary(pca_result_emo_s)
biplot(pca_result_emo_s)
plot(pca_result_emo_s)
prcomp(data_tweet)
print(pca_result_emo_s)

fviz_eig(pca_result_emo_s) 
fviz_pca_ind(pca_result_emo_s,col.ind=data_tweet$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_emo_s,col.ind=data_tweet$media,axes=c(3,4)) 
dim_pca_emo_s <- fviz_pca_var(pca_result_emo_s,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_emo_s <- dim_pca_emo_s +
  theme(text = element_text(size = 14, family = "Times New Roman"),  # Adjust overall text size
        axis.title = element_text(size = 12, family = "Times New Roman"),  # Adjust axis title size
        axis.text = element_text(size = 14, family = "Times New Roman")) +  # Adjust axis text size
  ggtitle("Fig. 2: PCA Emotionality in Status messages") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))  # Center the title
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_emo_s.png", plot = dim_pca_emo_s, dpi = 300, width = 20, height = 16, units = "cm")


######################PCA for Emotionality: Headlines######################
numeric_cols_emo_h <- data_title[, c("Tone","tone_pos","tone_neg",
                                     "emo_pos","emo_neg", 
                                     "Exclam", "adj")] #no Emoji
total_pca_emo_h <- scale(numeric_cols_emo_h)
pca_result_emo_h <- prcomp(total_pca_emo_h,scale=T)

summary(pca_result_emo_h)
biplot(pca_result_emo_h)
plot(pca_result_emo_h)
prcomp(data_title)
print(pca_result_emo_h)

fviz_eig(pca_result_emo_h) 
fviz_pca_ind(pca_result_emo_h,col.ind=data_title$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_emo_h,col.ind=data_title$media,axes=c(3,4)) 
dim_pca_emo_h <- fviz_pca_var(pca_result_emo_h,
                                 col.var = "contrib", # Color by contributions to the PC
                                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_emo_h <- dim_pca_emo_h + 
  theme(text = element_text(size = 14, family = "Times New Roman"), 
        axis.title = element_text(size = 12, family = "Times New Roman"),  
        axis.text = element_text(size = 14, family = "Times New Roman")) + 
  ggtitle("Fig. 3: PCA Emotionality in Headlines") +  
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_emo_h.png", plot = dim_pca_emo_h, dpi = 300, width = 20, height = 16, units = "cm")

loadings_emo_h <- pca_result_emo_h$rotation
print(loadings_emo_h)

loadings_emo_s <- pca_result_emo_s$rotation
print(loadings_emo_s)
######################PCA for Language complexity######################
numeric_cols_langua <- total[, c("WPS", "WC","BigWords","focusfuture","focuspresent","focuspast", "conj", "Cognition", "Analytic",
                                 "quantity", "number", "det", "allnone", "cogproc")]
total_pca_langua <- scale(numeric_cols_langua)
pca_result_langua <- prcomp(total_pca_langua,scale=T)

summary(pca_result_langua)
biplot(pca_result_langua)
plot(pca_result_langua)
prcomp(total)

fviz_eig(pca_result_langua) 
fviz_pca_ind(pca_result_langua,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_langua,col.ind=total$media,axes=c(3,4)) 
dim_pca_langua <- fviz_pca_var(pca_result_langua,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
library(ggrepel)
dim_pca_langua <- dim_pca_langua +
  theme(text = element_text(size = 14),  # Adjust overall text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  ggtitle("PCA Language complexity") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold"))
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_langua.png", plot = dim_pca_langua, dpi = 300, width = 20, height = 16, units = "cm")




######################PCA for Language complexity: Status messages######################
numeric_cols_langua_s <- data_tweet[, c("WPS", "WC","BigWords","focusfuture","focuspresent","focuspast", "conj", "Analytic",
                                        "adverb", "quantity", "number", "det", "cogproc")]
total_pca_langua_s <- scale(numeric_cols_langua_s)
pca_result_langua_s <- prcomp(total_pca_langua_s,scale=T)

summary(pca_result_langua_s)
biplot(pca_result_langua_s)
plot(pca_result_langua_s)
prcomp(data_tweet)
print(pca_result_langua_s)

fviz_eig(pca_result_langua_s) 
fviz_pca_ind(pca_result_langua_s,col.ind=data_tweet$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_langua_s,col.ind=data_tweet$media,axes=c(3,4)) 
dim_pca_langua_s <- fviz_pca_var(pca_result_langua_s,
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_langua_s <- dim_pca_langua_s +
  theme(text = element_text(size = 14, family = "Times New Roman"),
        axis.title = element_text(size = 12, family = "Times New Roman"),  
        axis.text = element_text(size = 14, family = "Times New Roman")) + 
  ggtitle("Fig. 4: PCA Linguistic Complexity in Status messages") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))  # Center the title
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_langua_s.png", plot = dim_pca_langua_s, dpi = 300, width = 20, height = 16, units = "cm")



######################PCA for Language complexity: Headlines######################
numeric_cols_langua_h <- data_title[, c("WPS", "WC","BigWords","focusfuture","focuspresent","focuspast", "conj", "Analytic",
                                        "adverb", "quantity", "number", "det", "cogproc")]
total_pca_langua_h <- scale(numeric_cols_langua_h)
pca_result_langua_h <- prcomp(total_pca_langua_h,scale=T)

summary(pca_result_langua_h)
biplot(pca_result_langua_h)
plot(pca_result_langua_h)
prcomp(data_title)
print(pca_result_langua_h)

fviz_eig(pca_result_langua_h) 
fviz_pca_ind(pca_result_langua_h,col.ind=data_title$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_langua_h,col.ind=data_title$media,axes=c(3,4)) 
dim_pca_langua_h <- fviz_pca_var(pca_result_langua_h,
                                 col.var = "contrib", # Color by contributions to the PC
                                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_langua_h <- dim_pca_langua_h +
  theme(text = element_text(size = 14, family = "Times New Roman"),
        axis.title = element_text(size = 12, family = "Times New Roman"),  
        axis.text = element_text(size = 14, family = "Times New Roman")) + 
  ggtitle("Fig. 5: PCA Linguistic Complexity in Headlines") +  
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))  
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_langua_h.png", plot = dim_pca_langua_h, dpi = 300, width = 20, height = 16, units = "cm")


loadings_langua_h <- pca_result_langua_h$rotation
print(loadings_langua_h)

loadings_langua_s <- pca_result_langua_s$rotation
print(loadings_langua_s)

######################PCA for Informality######################

numeric_cols_inf <- total[, c("Authentic","pronoun","ppron","i","we","you","shehe","they","ipron",
                              "Apostro","AllPunc", "quantity", "number",
                              "QMark", "OtherP", "Emoji", "Comma", "Period", "Exclam")]
total_pca_inf <- scale(numeric_cols_inf)
pca_result_inf <- prcomp(total_pca_inf)

summary(pca_result_inf)
biplot(pca_result_inf)
plot(pca_result_inf)
prcomp(total)

fviz_eig(pca_result_inf) 
fviz_pca_ind(pca_result_inf,col.ind=total$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_inf,col.ind=total$media,axes=c(3,4)) 

# Load necessary libraries
library(factoextra)
library(ggrepel)
# Perform PCA plot with adjusted aesthetics
dim_pca_inf <- fviz_pca_var(pca_result_inf,
                            col.var = "contrib",  # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE,  # Avoid text overlapping
                            # Increase text size and point size
                            labelsize = 6, pointsize = 6)
# Enhance the plot further
library(ggrepel)
dim_pca_inf <- dim_pca_inf +
  theme(text = element_text(size = 14),  # Adjust overall text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  ggtitle("PCA Informality") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold"))
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_inf.png", plot = dim_pca_inf, dpi = 300, width = 20, height = 16, units = "cm")


######################PCA for Informality: Status messages######################
numeric_cols_inf_s <- data_tweet[, c("Authentic","quantity", "number", "i","we","you","shehe","they","ipron",
                                     "AllPunc", "Apostro","QMark", "OtherP", "Emoji", "Comma", "Period", "Exclam")]
total_pca_inf_s <- scale(numeric_cols_inf_s)
pca_result_inf_s <- prcomp(total_pca_inf_s,scale=T)

summary(pca_result_inf_s)
biplot(pca_result_inf_s)
plot(pca_result_inf_s)
prcomp(data_tweet)
print(pca_result_inf_s)

fviz_eig(pca_result_inf_s) 
fviz_pca_ind(pca_result_inf_s,col.ind=data_tweet$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_inf_s,col.ind=data_tweet$media,axes=c(3,4)) 
dim_pca_inf_s <- fviz_pca_var(pca_result_inf_s,
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_inf_s <- dim_pca_inf_s +
  theme(text = element_text(size = 14, family = "Times New Roman"),  # Adjust overall text size
        axis.title = element_text(size = 12, family = "Times New Roman"),  # Adjust axis title size
        axis.text = element_text(size = 14, family = "Times New Roman")) +  # Adjust axis text size
  ggtitle("Fig. 6: PCA Informality in Status messages") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))  # Center the title
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_inf_s.png", plot = dim_pca_inf_s, dpi = 300, width = 20, height = 16, units = "cm")


######################PCA for Informality: Headlines######################
zero_value_columns <- apply(numeric_cols_inf_h, 2, function(x) all(x == 0))
# Print names of columns where all values are 0
zero_value_column_names <- names(numeric_cols_inf_h)[zero_value_columns]
# Output the column names
print(zero_value_column_names)

numeric_cols_inf_h <- data_title[, c("Authentic","quantity", "number", "i","we","you","shehe","they","ipron",
                                     "AllPunc", "Apostro","QMark", "OtherP", "Comma", "Period", "Exclam")] #no Emoji
total_pca_inf_h <- scale(numeric_cols_inf_h)
pca_result_inf_h <- prcomp(total_pca_inf_h,scale=T)

summary(pca_result_inf_h)
biplot(pca_result_inf_h)
plot(pca_result_inf_h)
prcomp(data_title)
print(pca_result_inf_h)

fviz_eig(pca_result_inf_h) 
fviz_pca_ind(pca_result_inf_h,col.ind=data_title$media,add.elipses=T) # visualise the pc scores of individual data ROWS on the first two axes
fviz_pca_ind(pca_result_inf_h,col.ind=data_title$media,axes=c(3,4)) 
dim_pca_inf_h <- fviz_pca_var(pca_result_inf_h,
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Enhance the plot further
if (!require("ggrepel")) {
  install.packages("ggrepel")
}
library(ggrepel)
dim_pca_inf_h <- dim_pca_inf_h +
  theme(text = element_text(size = 14, family = "Times New Roman"),  # Adjust overall text size
        axis.title = element_text(size = 12, family = "Times New Roman"),  # Adjust axis title size
        axis.text = element_text(size = 14, family = "Times New Roman")) +  # Adjust axis text size
  ggtitle("Fig. 7: PCA Informality in Headlines") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold", family = "Times New Roman", hjust = 0.5))  # Center the title
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/dim_pca_inf_h.png", plot = dim_pca_inf_h, dpi = 300, width = 20, height = 16, units = "cm")



loadings_inf_h <- pca_result_inf_h$rotation
print(loadings_inf_h)

loadings_inf_s <- pca_result_inf_s$rotation
print(loadings_inf_s)

######################Total PCA for Emotionality, Language complexity and Informality######################

numeric_cols_total <- total[, c("Tone", "Affect", "tone_pos", "tone_neg", "emotion", "emo_pos", "emo_neg", "Emoji", 
                                       "Exclam", "adj", "WPS", "WC", "BigWords", "focusfuture", "focuspresent", "focuspast", 
                                       "conj", "Cognition", "Analytic", "quantity", "number", "det", "allnone", "cogproc", 
                                       "Authentic", "pronoun", "ppron", "i", "we", "you", "shehe", "they", "ipron", "Apostro", 
                                       "AllPunc", "QMark", "OtherP", "Comma", "Period")]
# Scale the combined data
total_pca_scaled <- scale(numeric_cols_total)
# Perform PCA
total_pca <- prcomp(total_pca_scaled, scale. = TRUE)
# Summary of PCA
print(summary(total_pca))
# Visualize eigenvalues
fviz_eig(total_pca)
# Visualize the PCA scores on the first two principal components with ellipses
fviz_pca_ind(total_pca,
             col.ind = total$media,  # Color by media type
             addEllipses = TRUE,
             palette = "jco")
# Visualize contributions of variables to the first two principal components
dim_total_pca <- fviz_pca_var(total_pca,
                              col.var = "contrib",  # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE)  # Avoid text overlapping
# Enhance the plot further
dim_total_pca <- dim_total_pca +
  theme(text = element_text(size = 14),  # Adjust overall text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  ggtitle("Total PCA Analysis") +  # Add a title and adjust its size
  theme(plot.title = element_text(size = 16, face = "bold"))
# Save the enhanced PCA plot
ggsave("~/Desktop/plot/total_pca.png", plot = dim_total_pca, dpi = 300, width = 20, height = 16, units = "cm")


######################Total PCA for Status messages######################

numeric_cols_total_s <- data_tweet[, c("Tone", "tone_pos", "tone_neg", "emotion", "emo_pos", "emo_neg", "Emoji", 
                                "adj", "WPS", "WC", "BigWords", "focusfuture", "focuspresent", "focuspast", 
                                "conj", "Cognition", "Analytic", "quantity", "number", "det", "allnone", "cogproc", 
                                "Authentic", "i", "we", "you", "shehe", "they", "ipron", "Apostro", 
                                "AllPunc", "QMark", "Exclam", "OtherP")]
# Scale the combined data
total_pca_scaled_s <- scale(numeric_cols_total_s)
# Perform PCA
total_pca_s <- prcomp(total_pca_scaled_s, scale. = TRUE)
# Summary of PCA
print(summary(total_pca_s))
# Visualize eigenvalues
fviz_eig(total_pca_s)
# Visualize the PCA scores on the first two principal components with ellipses
fviz_pca_ind(total_pca_s,
             col.ind = data_tweet$media,  # Color by media type
             addEllipses = TRUE,
             palette = "jco")
library(ggrepel)
# Enhanced PCA plot with adjusted settings to manage overlaps
dim_total_pca_s <- fviz_pca_var(total_pca_s,
                                col.var = "contrib",  # Color by contributions to the PC
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE,  # Use repel to avoid overlaps
                                max.overlaps = 10,  # Allow more overlaps if necessary
                                labelsize = 3,  # Reduce label size
                                pointsize = 2)  # Reduce point size
dim_total_pca_s <- dim_total_pca_s +
  theme(text = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 15)) +
  ggtitle("Total PCA Status messages") +
  theme(plot.title = element_text(size = 18, face = "bold"))
# Save the adjusted plot
ggsave("~/Desktop/plot/total_pca_status.png", plot = dim_total_pca_s, dpi = 300, width = 20, height = 16, units = "cm")


######################Total PCA for Headlines######################

numeric_cols_total_h <- data_title[, c("Tone", "tone_pos", "tone_neg", "emotion", "emo_pos", "emo_neg", "Emoji", 
                                        "adj", "WPS", "WC", "BigWords", "focusfuture", "focuspresent", "focuspast", 
                                        "conj", "Cognition", "Analytic", "quantity", "number", "det", "allnone", "cogproc", 
                                        "Authentic", "i", "we", "you", "shehe", "they", "ipron", "Apostro", 
                                        "AllPunc", "QMark", "Exclam", "OtherP")]
# Check for zero variance in the unscaled data
is_constant <- apply(numeric_cols_total_h, 2, function(col) length(unique(col)) == 1)

# Print columns that have constant values
if (any(is_constant)) {
  print(names(numeric_cols_total_h)[is_constant])
} else {
  cat("No constant columns found\n")
}
# Removing constant columns from the data
numeric_cols_total_h <- numeric_cols_total_h[, !is_constant]
# Proceed to scale if there are still columns left
if (ncol(numeric_cols_total_h) > 0) {
  total_pca_scaled_h <- scale(numeric_cols_total_h)
  # Step 2: Perform PCA on the Cleaned and Scaled Data
  total_pca_h <- prcomp(total_pca_scaled_h, scale. = TRUE)
  print(summary(total_pca_h))
} else {
  cat("No columns available for PCA after removing constant columns.\n")
}
# Scale the combined data
total_pca_scaled_h <- scale(numeric_cols_total_h)
# Perform PCA
total_pca_h <- prcomp(total_pca_scaled_h, scale. = TRUE)
# Summary of PCA
print(summary(total_pca_h))
# Visualize eigenvalues
fviz_eig(total_pca_h)
# Visualize PCA indices
fviz_pca_ind(total_pca_h,
             col.ind = total$media,  # Color by media type
             addEllipses = TRUE,
             palette = "jco")
library(ggrepel)
# Enhanced PCA plot with adjusted settings to manage overlaps
dim_total_pca_h <- fviz_pca_var(total_pca_h,
                                col.var = "contrib",  # Color by contributions to the PC
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE,  # Use repel to avoid overlaps
                                max.overlaps = 10,  # Allow more overlaps if necessary
                                labelsize = 3,  # Reduce label size
                                pointsize = 2)  # Reduce point size
dim_total_pca_h <- dim_total_pca_h +
  theme(text = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 15)) +
  ggtitle("Total PCA Headlines") +
  theme(plot.title = element_text(size = 18, face = "bold"))
# Save the adjusted plot
ggsave("~/Desktop/plot/total_pca_headline.png", plot = dim_total_pca_h, dpi = 300, width = 20, height = 16, units = "cm")




######################Other test, but not very useful######################
tot_var_inf <- c("Authentic","pronoun","ppron","i","we","you","shehe","they","ipron",
                 "Apostro","AllPunc", "quantity", "number",
                 "QMark", "OtherP")
library("psych")
psych::alpha(total[,tot_var_inf], check.keys=TRUE)
environment(alpha)
tot_var_inf <- c("Authentic","pronoun","ppron","i","we","you","shehe","they","ipron",
                 "Apostro","AllPunc", "quantity", "number",
                 "QMark", "OtherP")
f = factanal(total[,tot_var_inf],factors=5)
print(f,sort=T,cutoff=.3,digits=2)


######################Correlations######################
cor(data_title$emotion,data_title$emo_neg)

######################Wilcoxon test######################

# Load necessary libraries
library("readxl")
# Read the data
data_tweet <- read_xlsx('~/Desktop/Tabelle Database/LIWC-22 Results - Data_Tweet - LIWC Analysis.xlsx')
data_title <- read_xlsx('~/Desktop/Tabelle Database/LIWC-22 Results - Data_Title - LIWC Analysis.xlsx')
# Combine the data
total <- rbind(data_tweet, data_title)
# Define media types
total$media <- ifelse(total$media < 4, "broadsheets", 
                      ifelse(total$media < 7, "tabloids", "magazines"))
# Define tweet or title
total$type <- c(rep("tweet", length = nrow(data_tweet)),
                rep("title", length = nrow(data_title)))
# Define newspaper
total$newspaper <- ifelse(total$media == 1, "Times",
                          ifelse(total$media == 2, "Telegraph",
                                 ifelse(total$media == 3, "Guardian",
                                        ifelse(total$media == 4, "Sun",
                                               ifelse(total$media == 5, "Daily_Mail",
                                                      ifelse(total$media == 6, "Daily_Express",
                                                             ifelse(total$media == 7, "Economist",
                                                                    ifelse(total$media == 8, "Tribune", "Prospect"))))))))
# Convert newspaper to a factor
total$newspaper <- as.factor(total$newspaper)
# Define newspapers
newspapers <- c("Times", "Telegraph", "Guardian", "Sun", "Daily Mail", 
                "Daily Express", "Economist", "Tribune", "Prospect")
# Calculate means of emotion-related variables for tweets and titles
tweet_means <- colMeans(subset(total, type == "tweet")[c("tone_pos", "emo_pos")])
title_means <- colMeans(subset(total, type == "title")[c("tone_pos", "emo_pos")])
# Compare means using t-tests
t_test_tone_pos <- t.test(subset(total, type == "tweet")$tone_pos, 
                          subset(total, type == "title")$tone_pos)
t_test_emo_pos <- t.test(subset(total, type == "tweet")$emo_pos, 
                         subset(total, type == "title")$emo_pos)
# Print results
print("Mean Tone Positivity:")
print(tweet_means["tone_pos"])
print(title_means["tone_pos"])
print("Mean Emotion Positivity:")
print(tweet_means["emo_pos"])
print(title_means["emo_pos"])
print("T-Test Results for Tone Positivity:")
print(t_test_tone_pos)
print("T-Test Results for Emotion Positivity:")
print(t_test_emo_pos)


# Calculate means of emotion-related variables for tweets and titles
tweet_means <- colMeans(subset(total, type == "tweet")[c("tone_pos", "emo_pos")])
title_means <- colMeans(subset(total, type == "title")[c("tone_pos", "emo_pos")])
# Perform Wilcoxon rank-sum test
wilcox_test_tone_pos <- wilcox.test(subset(total, type == "tweet")$tone_pos, 
                                    subset(total, type == "title")$tone_pos)
wilcox_test_emo_pos <- wilcox.test(subset(total, type == "tweet")$emo_pos, 
                                   subset(total, type == "title")$emo_pos)
# Print results
print("Mean Tone Positivity:")
print(tweet_means["tone_pos"])
print(title_means["tone_pos"])
print("Mean Emotion Positivity:")
print(tweet_means["emo_pos"])
print(title_means["emo_pos"])
print("Wilcoxon Rank-Sum Test Results for Tone Positivity:")
print(wilcox_test_tone_pos) #there is a statistically significant difference in tone positivity between tweets and titles
print("Wilcoxon Rank-Sum Test Results for Emotion Positivity:")
print(wilcox_test_emo_pos)

######################Statistical Analyses, already did them in the hypothesis######################

# look at distribution of the outcome variable 
hist(total$Analytic) # not normally distributed, values bound between 0 and 100 -> beta regression may be a good choice
# could there be a relationship with predictor variables of interest
bwplot(Analytic ~ media | type, total)
### run the model
###---------------
total$Analytic <- total$Analytic/100 # the beta model wants values between 0-1 instead of 0-100, maybe to it in one go at the top?
# run the beta regression model
#m1 <- glmmTMB (Analytic ~ media * relevel(type,ref="tweet"), total, family = beta_family(link = "logit")) #does not seem to work
m1 <- glmmTMB(Analytic ~ media * type, data = total, 
              family = beta_family(link = "logit"))
# model summary
summary(m1)
### model selection (potentially)
###---------------
# interactions don't seem relevant: check!
#m1b <- glmmTMB (Analytic ~ media + type, total, family = beta_family(link = "logit"))
m1b <- glmmTMB(Analytic ~ media + type, data = total, 
               family = beta_family(link = "logit"))
summary(m1b)
# compare the two models
anova(m1, m1b) # m1 seems just about better based on p value, but AIC difference is only 2, so i would pick simpler model
### visualise model predictions
###---------------
library(ggeffects)
pred <- ggpredict(m1, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1,jitter=0.05,dot.size = 1) # ignore warning messages
plot(pred)
#media+type




######################TUTTE le tabelle con i coefficienti rifatte######################

summary(mh1a_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh1a_g <- tidy(mh1a_g)
# Format the p-values and coefficients
results_mh1a_g <- results_mh1a_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh1a_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_h1a_g.docx")
# Browse the created document
browseURL("model_results_h1a_g.docx")

summary(mtone_pos_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mtone_pos_g <- tidy(mtone_pos_g)
# Format the p-values and coefficients
results_mtone_pos_g <- results_mtone_pos_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mtone_pos_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mtone_pos_g.docx")
# Browse the created document
browseURL("model_results_mtone_pos_g.docx")


summary(mtone_neg_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mtone_neg_g <- tidy(mtone_neg_g)
# Format the p-values and coefficients
results_mtone_neg_g <- results_mtone_neg_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mtone_neg_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mtone_neg_g.docx")
# Browse the created document
browseURL("model_results_mtone_neg_g.docx")


summary(memo_pos_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_memo_pos_g <- tidy(memo_pos_g)
# Format the p-values and coefficients
results_memo_pos_g <- results_memo_pos_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_memo_pos_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_memo_pos_g.docx")
# Browse the created document
browseURL("model_results_memo_pos_g.docx")


summary(memo_neg_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_memo_neg_g <- tidy(memo_neg_g)
# Format the p-values and coefficients
results_memo_neg_g <- results_memo_neg_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_memo_neg_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_memo_neg_g.docx")
# Browse the created document
browseURL("model_results_memo_neg_g.docx")

summary(mh1c_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh1c_g <- tidy(mh1c_g)
# Format the p-values and coefficients
results_mh1c_g <- results_mh1c_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh1c_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh1c_g.docx")
# Browse the created document
browseURL("model_results_mh1c_g.docx")


summary(mh1d_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh1d_g <- tidy(mh1d_g)
# Format the p-values and coefficients
results_mh1d_g <- results_mh1d_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh1d_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh1d_g.docx")
# Browse the created document
browseURL("model_results_mh1d_g.docx")


summary(mh2a)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2a <- tidy(mh2a)
# Format the p-values and coefficients
results_mh2a <- results_mh2a %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2a %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2a.docx")
# Browse the created document
browseURL("model_results_mh2a.docx")


summary(mh2aa)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2aa <- tidy(mh2aa)
# Format the p-values and coefficients
results_mh2aa <- results_mh2aa %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2aa %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2aa.docx")
# Browse the created document
browseURL("model_results_mh2aa.docx")

summary(mh2b_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2b_g <- tidy(mh2b_g)
# Format the p-values and coefficients
results_mh2b_g <- results_mh2b_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2b_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2b_g.docx")
# Browse the created document
browseURL("model_results_mh2b_g.docx")


summary(mh2f_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2f_g <- tidy(mh2f_g)
# Format the p-values and coefficients
results_mh2f_g <- results_mh2f_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2f_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2f_g.docx")
# Browse the created document
browseURL("model_results_mh2f_g.docx")



summary(mh2e)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2e <- tidy(mh2e)
# Format the p-values and coefficients
results_mh2e <- results_mh2e %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2e %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2e.docx")
# Browse the created document
browseURL("model_results_mh2e.docx")

summary(mh2c_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2c_g <- tidy(mh2c_g)
# Format the p-values and coefficients
results_mh2c_g <- results_mh2c_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2c_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2c_g.docx")
# Browse the created document
browseURL("model_results_mh2c_g.docx")

summary(mh2c_past)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2c_past <- tidy(mh2c_past)
# Format the p-values and coefficients
results_mh2c_past <- results_mh2c_past %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2c_past %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2c_past.docx")
# Browse the created document
browseURL("model_results_mh2c_past.docx")


summary(mh2c_future)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2c_present <- tidy(mh2c_future)
# Format the p-values and coefficients
results_mh2c_present <- results_mh2c_present %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2c_present %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2c_present.docx")
# Browse the created document
browseURL("model_results_mh2c_present.docx")



summary(mh2d)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2d <- tidy(mh2d)
# Format the p-values and coefficients
results_mh2d <- results_mh2d %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2d %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2d.docx")
# Browse the created document
browseURL("model_results_mh2d.docx")


summary(mh2l)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2l <- tidy(mh2l)
# Format the p-values and coefficients
results_mh2l <- results_mh2l %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2l %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2l.docx")
# Browse the created document
browseURL("model_results_mh2l.docx")



summary(mh2g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2g <- tidy(mh2g)
# Format the p-values and coefficients
results_mh2g <- results_mh2g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2g.docx")
# Browse the created document
browseURL("model_results_mh2g.docx")


summary(mh2i_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2i_g <- tidy(mh2i_g)
# Format the p-values and coefficients
results_mh2i_g <- results_mh2i_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2i_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2i_g.docx")
# Browse the created document
browseURL("model_results_mh2i_g.docx")


summary(mh2ii_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh2ii_g <- tidy(mh2ii_g)
# Format the p-values and coefficients
results_mh2ii_g <- results_mh2ii_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh2ii_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh2ii_g.docx")
# Browse the created document
browseURL("model_results_mh2ii_g.docx")


summary(mh3c_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh3c_g <- tidy(mh3c_g)
# Format the p-values and coefficients
results_mh3c_g <- results_mh3c_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh3c_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh3c_g.docx")
# Browse the created document
browseURL("model_results_mh3c_g.docx")

summary(model_i_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_i_g <- tidy(model_i_g)
# Format the p-values and coefficients
results_model_i_g <- results_model_i_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_i_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_i_g.docx")
# Browse the created document
browseURL("model_results_model_i_g.docx")



summary(model_we_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_we_g <- tidy(model_we_g)
# Format the p-values and coefficients
results_model_we_g <- results_model_we_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_we_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_we_g.docx")
# Browse the created document
browseURL("model_results_model_we_g.docx")


summary(model_you_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_you_g <- tidy(model_you_g)
# Format the p-values and coefficients
results_model_you_g <- results_model_you_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_you_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_you_g.docx")
# Browse the created document
browseURL("model_results_model_you_g.docx")



summary(model_shehe)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_shehe <- tidy(model_shehe)
# Format the p-values and coefficients
results_model_shehe <- results_model_shehe %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_shehe %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_shehe.docx")
# Browse the created document
browseURL("model_results_model_shehe.docx")


summary(model_they_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_they_g <- tidy(model_they_g)
# Format the p-values and coefficients
results_model_they_g <- results_model_they_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_they_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_they_g.docx")
# Browse the created document
browseURL("model_results_model_they_g.docx")


summary(model_ipron)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_model_ipron <- tidy(model_ipron)
# Format the p-values and coefficients
results_model_ipron <- results_model_ipron %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_model_ipron %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_model_ipron.docx")
# Browse the created document
browseURL("model_results_model_ipron.docx")


summary(mh3f)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh3f <- tidy(mh3f)
# Format the p-values and coefficients
results_mh3f <- results_mh3f %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh3f %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh3f.docx")
# Browse the created document
browseURL("model_results_mh3f.docx")


summary(mh3d_g)
# Extract estimates, standard errors, z-values, and p-values using tidy
results_mh3d_g <- tidy(mh3d_g)
# Format the p-values and coefficients
results_mh3d_g <- results_mh3d_g %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2)
  )
# Create a table manually
results_table <- results_mh3d_g %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    `Term` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `Pr(>|z|)` = p.value
  )
# Create a flextable object with the formatted results
ft <- flextable(results_table) %>%
  colformat_num(j = c("Estimate", "Std. Error", "z value"), digits = 2)
# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft)
# Save the document
print(doc, target = "model_results_mh3d_g.docx")
# Browse the created document
browseURL("model_results_mh3d_g.docx")



##################################model predictions##############################################
library(ggeffects)
pred <- ggpredict(mh1c_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh1c_g_plot_pred <- plot(pred)
print(mh1c_g_plot_pred)
ggsave("~/Desktop/plot/mh1c_g_plot_pred.png", plot = mh1c_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh1d_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh1d_g_plot_pred <- plot(pred)
print(mh1d_g_plot_pred)
ggsave("~/Desktop/plot/mh1d_g_plot_pred.png", plot = mh1d_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh2a, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2a_plot_pred <- plot(pred)
print(mh2a_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2a_plot_pred.png", plot = mh2a_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh2aa, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2aa_plot_pred <- plot(pred)
print(mh2aa_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2aa_plot_pred.png", plot = mh2aa_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh2b_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2b_g_plot_pred <- plot(pred)
print(mh2b_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2b_g_plot_pred.png", plot = mh2b_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh2f_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2f_g_plot_pred <- plot(pred)
print(mh2f_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2f_g_plot_pred.png", plot = mh2f_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh2e, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2e_plot_pred <- plot(pred)
print(mh2e_plot_pred)
ggsave("~/Desktop/plot/mh2e_plot_pred.png", plot = mh2e_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh2c_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2c_g_plot_pred <- plot(pred)
print(mh2c_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2c_g_plot_pred.png", plot = mh2c_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")

library(ggeffects)
pred <- ggpredict(mh2d, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2d_plot_pred <- plot(pred)
print(mh2d_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2d_plot_pred.png", plot = mh2d_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(mh2l, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2l_plot_pred <- plot(pred)
print(mh2l_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh2l_plot_pred.png", plot = mh2l_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh2g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2g_plot_pred <- plot(pred)
print(mh2g_plot_pred)
ggsave("~/Desktop/plot/mh2g_plot_pred.png", plot = mh2g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh2ii_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2ii_g_plot_pred <- plot(pred)
print(mh2ii_g_plot_pred)
ggsave("~/Desktop/plot/mh2ii_g_plot_pred.png", plot = mh2ii_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh2i_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh2i_g_plot_pred <- plot(pred)
print(mh2i_g_plot_pred)
ggsave("~/Desktop/plot/mh2i_g_plot_pred.png", plot = mh2i_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh3c_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh3c_g_plot_pred <- plot(pred)
print(mh3c_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh3c_g_plot_pred.png", plot = mh3c_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh3d_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh3d_g_plot_pred <- plot(pred)
print(mh3d_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh3d_g_plot_pred.png", plot = mh3d_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh3f_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh3f_g_plot_pred <- plot(pred)
print(mh3f_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh3f_g_plot_pred.png", plot = mh3f_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(mh3f, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
mh3f_plot_pred <- plot(pred)
print(mh3f_plot_pred)
#media+type
ggsave("~/Desktop/plot/mh3f_plot_pred.png", plot = mh3f_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(model_i_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_i_g_plot_pred <- plot(pred)
print(model_i_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_i_g_plot_pred.png", plot = model_i_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(model_we_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_we_g_plot_pred <- plot(pred)
print(model_we_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_we_g_plot_pred.png", plot = model_we_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(model_you_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_you_g_plot_pred <- plot(pred)
print(model_you_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_you_g_plot_pred.png", plot = model_you_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(model_shehe, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_shehe_plot_pred <- plot(pred)
print(model_shehe_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_shehe_plot_pred.png", plot = model_shehe_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



library(ggeffects)
pred <- ggpredict(model_they_g, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_they_g_plot_pred <- plot(pred)
print(model_they_g_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_they_g_plot_pred.png", plot = model_they_g_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")


library(ggeffects)
pred <- ggpredict(model_ipron, terms = c("media", "type")) # calculate model predictions
pred
plot(pred, add.data=T, dot.alpha = 0.1, jitter=0.05, dot.size = 1) # ignore warning messages
model_ipron_plot_pred <- plot(pred)
print(model_ipron_plot_pred)
#media+type
ggsave("~/Desktop/plot/model_ipron_plot_pred.png", plot = model_ipron_plot_pred, dpi = 300, width = 22, height = 18, units = "cm")



