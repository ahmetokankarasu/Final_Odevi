# library(devtools)
# remotes::install_github("OpenIntroStat/openintro")
# https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv

rm(list=ls()); cat("\14")
#####################################################################################################
# Gerekli kütüphanelerin aktif edilmesi
library(here)
library(readxl)
library(tidyr)
library(glmnet)
library(readxl)
library(ggplot2)
library(markdown)
library("PerformanceAnalytics")
library(dplyr)
library(skimr)
library(summarytools)
library(xtable)
library(summarytools)
st_options(lang = "tr")

# remotes::install_github("datalorax/equatiomatic")

# excel_sheets(path = "../data/Data.xlsx")




#####################################################################################################
# Exceldeki verilerin R içerisine aktarılması
Col_Name <- c("Reference area", "Source", "Sex", "Age", "Time",
              "Total", "Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")

Data_LF <- read_excel("../data/Data.xlsx", sheet = "Labour force",
                      col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                      )[ , Col_Name]

Data_WP <- read_excel("../data/Data.xlsx", sheet = "Working-age population",
                      col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                      )[ , Col_Name]

Data_E <- read_excel("../data/Data.xlsx", sheet = "Employment",
                     col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                    )[ , Col_Name]

Data_DJ <- read_excel("../data/Data.xlsx", sheet = "Discouraged jobseekers",
                      col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                    )[ , Col_Name]

Data_UeR <- read_excel("../data/Data.xlsx", sheet = "Unemployment rate",
                       col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                      )[ , Col_Name]

Data_Ue <- read_excel("../data/Data.xlsx", sheet = "Unemployment",
                      col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text")
                      )[ , Col_Name]


colnames(Data_LF)
# [ , c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level")]


#####################################################################################################
# Verilerin gather fonksiyonu ile eğitim seviyelerine göre bilgilerin tek tek kolonlara dönüştürülmesi. Ayrıca Farklı tabloları bilgileriyle birleştirmek amacıyla unique bir id oluşturulmasu
Data_LF <- Data_LF %>%
  gather("Edu Level", "Labour force", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
rename("Total LF" = Total)
Data_LF$Merge_ID <- apply(Data_LF[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))

Data_WP <- Data_WP %>%
  gather("Edu Level", "Working-age population", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
  rename("Total WP" = Total)
Data_WP$Merge_ID <- apply(Data_WP[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))


Data_E <- Data_E %>%
  gather("Edu Level", "Employment", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
  rename("Total E" = Total)
Data_E$Merge_ID <- apply(Data_E[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))


Data_DJ <- Data_DJ %>%
  gather("Edu Level", "Discouraged jobseekers", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
  rename("Total DJ" = Total)
Data_DJ$Merge_ID <- apply(Data_DJ[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))


Data_UeR <- Data_UeR %>%
  gather("Edu Level", "Unemployment rate", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
  rename("Total UeR" = Total)
Data_UeR$Merge_ID <- apply(Data_UeR[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))


Data_Ue <- Data_Ue %>%
  gather("Edu Level", "Unemployment", c("Less than basic", "Basic", "Intermediate", "Advanced", "Level not stated")) %>%
  rename("Total Ue" = Total)
Data_Ue$Merge_ID <- apply(Data_Ue[ , c("Reference area", "Sex", "Age", "Time", "Edu Level")], 1, function(x) paste0(x, collapse =""))

#####################################################################################################
# Anahtar ile farklı tabloların tek tablo haline getirilmesi
Data_Final <- merge(Data_LF, Data_WP[ , !(colnames(Data_WP) %in% c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level"))], by="Merge_ID")

Data_Final <- merge(Data_Final, Data_E[ , !(colnames(Data_WP) %in% c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level"))], by="Merge_ID")
Data_Final <- merge(Data_Final, Data_DJ[ , !(colnames(Data_WP) %in% c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level"))], by="Merge_ID")
Data_Final <- merge(Data_Final, Data_UeR[ , !(colnames(Data_WP) %in% c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level"))], by="Merge_ID")
Data_Final <- merge(Data_Final, Data_Ue[ , !(colnames(Data_WP) %in% c("Reference area", "Source", "Sex", "Age", "Time", "Edu Level"))], by="Merge_ID")





#####################################################################################################
# Kolon isimlerinin düzeltilmesi ve bazı kolonların sonveriden çıkartılması
Data_Final <- Data_Final %>%
  janitor::clean_names()

k <- grep(pattern = "total", x= colnames(Data_Final))
Data_Final  <- Data_Final[ , !(colnames(Data_Final) %in%  c("Merge_ID", "Source", colnames(Data_Final)[k]) ) ]


#####################################################################################################
# veri içerisindei eksik değerli (NA) ve bazı satırların filtelenmesi
Countries <- sort(table(Data_Final$reference_area),  decreasing = TRUE)
Countries <- Countries[Countries>=350]
print(Countries)


Data_Final  <- Data_Final  %>%
  filter(sex != "Total") %>%
  filter( (time >= 2014 ) & (time <= 2022 ) ) %>%
  filter( reference_area %in% names(Countries) ) %>%
  na.omit() %>%
  mutate( across(labour_force:unemployment, as.numeric) )


rm(Data_DJ, Data_E,  Data_LF, Data_Ue, Data_UeR, Data_WP, Countries, k)


#####################################################################################################
# kategorik ve numeric değerli kolonların dönüşümlerinin yapılması. Aynı zamanda numeric olan kolonların log10 tabanına çevrilmesi

Data_Final$reference_area <- as.factor(Data_Final$reference_area)
Data_Final$sex <- as.factor(Data_Final$sex)
Data_Final$age <- as.factor(Data_Final$age)
Data_Final$time <- as.factor(Data_Final$time)
Data_Final$edu_level <- as.factor(Data_Final$edu_level)


Col_Name <- c("labour_force", "working_age_population", "employment", "discouraged_jobseekers", "unemployment")
Data_Final[ , Col_Name] <- log10((Data_Final[ , Col_Name] *1000)+10)

#####################################################################################################
skimr::skim_without_charts(Data_Final)
#####################################################################################################
glimpse(Data_Final)
#####################################################################################################
summary(Data_Final)
#####################################################################################################



#####################################################################################################

# Add notched box plot
ggplot(Data_Final, aes(x=unemployment, y=reference_area)) +
  geom_boxplot(notch = TRUE)
  # +geom_jitter(aes(colour =sex) , position=position_jitter(0.2))


Data_Final  %>% group_by(reference_area)  %>%
  summarise(Avg=mean(unemployment_rate), Std=sd(unemployment_rate))




ggplot(Data_Final, aes(x=unemployment, y=sex, fill =time)) +
  geom_boxplot(notch = TRUE)


Data_Final  %>% group_by(sex, time)  %>%
  summarise(Avg=mean(unemployment_rate), Std=sd(unemployment_rate))



ggplot(Data_Final, aes(x=unemployment, y=sex, fill =edu_level)) +
  geom_boxplot(notch = TRUE)




library(PerformanceAnalytics)

chart.Correlation(Data_Final[ , c("labour_force", "working_age_population", "employment", "discouraged_jobseekers", "unemployment_rate", "unemployment")], histogram = TRUE, method = "pearson")


descr(Data_Final, style = 'rmarkdown')

#####################################################################################################
# +  unemployment_rate unemployment

Formula <- as.formula(unemployment_rate ~ reference_area + sex + age + time + edu_level + labour_force + working_age_population + employment + discouraged_jobseekers)
score_mod <- lm(Formula, data = Data_Final)

summary(score_mod)


generics::tidy(score_mod)

equatiomatic::extract_eq(score_mod, use_coefs = TRUE)

#####################################################################################################

#####################################################################################################



#####################################################################################################




#####################################################################################################






#####################################################################################################






#####################################################################################################
ggplot(survey) +
  aes(x = handedness) +
  geom_histogram()


survey %>%
  ggplot() +
  aes(x = handedness, y = handspan) +
  geom_point() +
  hrbrthemes::theme_ipsum_rc()

#####################################################################################################
**_bağzı
kelimeler_**
  ([Mülkiye!](http://www.politics.ankara.edu.tr/))
![Benjamin Bannekat](../../images/bannekat.png)



• Süt
• Yumurta
• Ekmek
• Zey

‘‘‘{r}
head(pressure)
summary(pressure)
‘‘‘

$$
  \mathbb{E}[Y] = \beta_0 + \beta_1x
$$



#####################################################################################################
Data <- separate(Data, col=Time, into=c('Month',  'Year'), sep=' ')

Data_DR <- Data %>%spread(Product, Value)



Data_DR_Group <- Data_DR %>% group_by(Country) %>%
  summarise(across(colnames(Data_DR)[4:dim(Data_DR)[2]], sum))


Data_DR_Group <- Data_DR %>% group_by(Country) %>%
  summarise(across(colnames(Data_DR)[4:dim(Data_DR)[2]], sum))





table(Data_Ue$Age)


table(Data_LF$Age)


