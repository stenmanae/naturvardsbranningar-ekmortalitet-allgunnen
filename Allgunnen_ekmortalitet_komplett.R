
rm(list=ls()) 

library (survival) # survival analysis
library (survminer) # survival analysis
library (ggfortify) # GGplot2 installeras automatiskt (fun.SeasonPlot m.m.)
library (readxl)   # data input through excel
library (RODBC)     # ensures connection to an Access db. 
library (dplyr)     # chaining 
library (tidyverse) # # organisera data 
library (nlme) # GAM 
library (mgcv) # GAM 
library (emmeans)   # post-hoc tukey 

setwd ("C:/Users/stenm/OneDrive - student.lnu.se/24Exjobb/naturvardsbranningar/R")

# Access
my.db.name <- "OakB_database_v2.accdb"

# .. setting up a connection
# .. https://stackoverflow.com/questions/17115632/microsoftodbc-driver-manager-data-source-name-not-found-and-no-default-drive 
# .. edit stuff in ODBC data source administrator (needed?)
my.con <- RODBC::odbcConnectAccess2007(my.db.name)

# .. just check
RODBC::sqlTables(my.con)

# .. fetch a table
data.raw <- RODBC::sqlFetch(my.con, "Treesurvival")
head (data.raw) #looks good 

# .. immediately closing all connections after we got our data
RODBC::odbcCloseAll() 

# getting rid of rare tree species 
data.raw <- subset (data.raw, SpeciesID == "oak" | SpeciesID == "pine" |
                      SpeciesID == "spruce") #| SpeciesID == "birch")

# re-coding status
data.raw$status <- dplyr::recode (data.raw$status, "live" = 1, "dead" = 0, "halfdead" = 1)

## data.in <- fun.recoding.event.survival(data.raw)
## data.in <- subset (data.in, SpeciesID == 1 | SpeciesID == 2 | SpeciesID == 3, SpeciesID == 4)

data.in <- data.raw


### +++++++++++++++++++++++++++++++++++ undersökning av data ++++++++++++++++++++++++++++

#kollar NA 
na_counts_base <- colSums(is.na(data.in))
print(na_counts_base)

#tar bort ett stickprov som saknar firescrouch värde. Detta tillåter följande max, min, mean beräkningarna: 
data.in <-data.in%>%
  drop_na(fireScrouch)

#endast för ek: 
data.oak <-data.in%>%
  filter(SpeciesID == "oak")

#endast för ek Norrskog: 
data.oak.norr <-data.oak%>%
  filter(BurnedAreaID == "MainSite2022")

#endast för ek Sjömunnen:  
data.oak.sjo <-data.oak%>%
  filter(FireYear == 2018) #använder detta filter då filter genom ID inte funkade 


#Sammanlagt 
attach(data.oak)
max(fireScrouch) #600 (6 m) 
min(fireScrouch) #0 (0 m) 
mean(fireScrouch) #112.42 (1.12 m) 
sum(status == "0") #360 döda 
sum(status == "1") #130 levande  
sum(sproutUp == "1") #105 med stamskott 
sum(sproutDown == "1") #244 med rotskott 
sum(sproutDown == "1"| data.oak$sproutUp == "1") #302 träd med skott 
sum(sproutDown == "1"& data.oak$sproutUp == "1") #47 träd med bådeoch 
mean(dbh) #9.38


#Norrskog
attach(data.oak.norr)
max(fireScrouch) #600 (6 m) 
min(fireScrouch) #10 (0.1 m) 
mean(fireScrouch) #136.2 (1.36 m) 
sum(status == "0") #262 döda 
sum(status == "1") #60 levande  
sum(sproutUp == "1") #41 med stamskott 
sum(sproutDown == "1") #171 med rotskott 
sum(sproutDown == "1"| sproutUp == "1") #195 träd med skott 
mean(dbh) #8,469 

#sjömunnen
attach(data.oak.sjo)
max(fireScrouch) #200 (2 m) 
min(fireScrouch) #0 (0 m) 
mean(fireScrouch) #66,77 (0,67 m) 
sum(status == "0") #98 döda 
sum(status == "1") #70 levande  
sum(sproutDown == "1"| sproutUp == "1") #107 träd med skott 
mean(dbh) #11.12

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# lägger ihop sandvik och norrskog/sjömunnen 

#lägger in lokalnamn  
data.in <- data.in %>%
  mutate(lokal = ifelse(FireYear== "2022", "Norrskog", "Sjömunnen"))

#läser in excelfil med sandviksdata 
sandvik2023<- read_excel("ek_mortalitet_sandvik_20240907.xlsx", sheet = "allspecies") 
sandvik2023<-sandvik2023%>% #change data type 
  mutate(
    across(c(FireYear,status,sproutUp,sproutDown,fireScrouch), as.integer))%>%
  mutate(lokal=c("Sandvik")) #lägger till lokalnamn i denna df också

#Kombinerar  
data.full<-bind_rows(data.in, sandvik2023)

#tar bort NA 
data.full<-data.full %>%
  drop_na(status) #sju togs bort 

#kontroller hur många ekar för de olika lokalerna 
data.full%>%
  filter(SpeciesID=="oak")%>%
  count(lokal) 

#kollar antalet träd 
sum(data.full$SpeciesID == "spruce") #130 
sum(data.full$SpeciesID == "pine")#331 
sum(data.full$SpeciesID == "oak") #688 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Kaplan-Mayer icke-parametrisk skattning 

#vad är skillnaden mellan alla lokalerna för ek? 
data.oak <- subset (data.full, SpeciesID == "oak")
oak.survival.dia <- survfit(Surv(dbh, status) ~ lokal, data = data.oak)
oak.survival.dia
summary(oak.survival.dia)

# LOG RANK TEST (paket survival) för ek på lokalerna 
survdiff<- survdiff (Surv(dbh, status) ~ lokal, data = data.oak)
survdiff #signifikant skillnad 0.004

# graf ek 
ggsurvplot(oak.survival.dia, 
           pval = TRUE, 
           palette = "uchicago",
           legend = c("right"), 
           legend.labs = c("Norrskog","Sandvik","Sjömunnen"), 
           conf.int = TRUE, 
           surv.median.line = "hv", #median
           xlab = "Dbh, cm",
           ylab = "Sannolikhet för mortalitet",
           title = "Kaplan-Meier skattning: Quercus mortalitet")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# KM för alla trädslag (källa sjömunnen och norrskog data.in)

# recoding 
#data.in<- data.in %>% 
#mutate(status = ifelse(status == 1, 0, 1)) #recode om det behövs? 

# MODEL FIT 
fit <- survfit(Surv(dbh, status) ~ SpeciesID, data = data.in)  
fit 
summary(fit)

# graf 
ggsurvplot(fit, 
           pval = TRUE, 
           legend = c("right"), 
           legend.labs = c("Q. robur", "P. sylvestris", "P. abies"), 
           conf.int = TRUE, 
           surv.median.line = "hv", #median 
           xlab = "Dbh, cm",
           ylab = "Sannolikhet för mortalitet",
           title = "Kaplan-Meier skattning: mortalitet")

# LOG RANK TEST (paket survival)
survdiff<- survdiff (Surv(dbh, status) ~ SpeciesID, data = data.in)
survdiff #signifikant skillnad 
summary(survdiff)

###### testa bara ek och tall??
ek.gran <- subset (data.in, SpeciesID == "oak" | SpeciesID == "spruce") 

survdiff<- survdiff (Surv(dbh, status) ~ SpeciesID, data = ek.gran)
survdiff #SIGNIFIKANT 
summary(survdiff)

# MODEL FIT #KONTROLLERA MED LOGLOGISTIC POST HOC TEST 
fit <- survfit(Surv(dbh, status) ~ SpeciesID, data =  ek.gran)  
fit 
summary(fit)

ggsurvplot(fit, 
           pval = TRUE, 
           legend = c("right"), 
           legend.labs = c("Q. robur", "P. abies"), 
           conf.int = TRUE, 
           surv.median.line = "hv", #median 
           xlab = "Dbh, cm",
           ylab = "Sannolikhet för mortalitet",
           title = "Kaplan-Meier skattning: mortalitet")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# JÄMFÖR  modelleringar av överlevnad med olika sannolikhetsfördelningar - paramteriska beskrivande tester 

#Weibull: a well-recognized statistical technique for exploring the relationship between the survival of a patient, a parametric distribution and several explanatory variables.
#In contrast to a "regular" survivalship analysis, here we use diameter as a proxy of
# mortality probability and an inversed event definition. That is, event is survival after fire,
# and not mortality event. 

set.seed(123)
tree.survival.wei <- survreg (Surv(dbh, status) ~ SpeciesID, data=data.in,
                              dist = "weibull")
summary (tree.survival.wei)

# exponential model
tree.survival.exp <- survreg (Surv(dbh, status) ~ SpeciesID, data=data.in,
                              dist = "exponential")
summary (tree.survival.exp)

# log-logistic model
tree.survival.loglog <- survreg (Surv(dbh, status) ~ SpeciesID, data=data.in,
                                 dist = "loglogistic")
summary (tree.survival.loglog)

# normal
tree.survival.lognorm <- survreg (Surv(dbh, status) ~ SpeciesID, data=data.in,
                                  dist = "lognormal")
summary (tree.survival.lognorm)

library(MASS)
library(multcomp)
AIC(tree.survival.wei,tree.survival.exp, tree.survival.loglog,tree.survival.lognorm) #lognorm bäst 
BIC(tree.survival.wei,tree.survival.exp, tree.survival.loglog,tree.survival.lognorm) #lognorm bäst 

# ++++++++++++++++++++++++++++++++++++++++++++ post hoc comparisons +++++++++++++++

summary(glht(tree.survival.lognorm, linfct = mcp(SpeciesID= "Tukey")))

# ++++++++++++++++++++++++++++ visualisering överlevnadsanalys och KM +++++++++++++++++++++++++++++++++++++++ 

# tips på hur man kombinerar geom_lines baserat på skattade sannoliketsfördelningar för varje grupp med KM kurvorna. 
# https://stackoverflow.com/questions/9151591/how-to-plot-the-survival-curve-generated-by-survreg-package-survival-of-r

# sammma kod för KM fit som ovan
fit <- survfit(Surv(dbh, status) ~ SpeciesID, data = data.in) 

# sammma kod förlognormal modell som ovan
tree.survival.lognorm <- survreg (Surv(dbh, status) ~SpeciesID, data=data.in,
                                  dist = "lognormal")

# spara varje enskild skattning för varje trädslag 
pred.species1 = predict(tree.survival.lognorm , newdata=list(SpeciesID="pine"),type="quantile",p=seq(.01,.99,by=.01))
pred.species2 = predict(tree.survival.lognorm , newdata=list(SpeciesID="spruce"),type="quantile",p=seq(.01,.99,by=.01))
pred.species3 = predict(tree.survival.lognorm , newdata=list(SpeciesID="oak"),type="quantile",p=seq(.01,.99,by=.01))

df = data.frame(y=seq(.99,.01,by=-.01), Quercus=pred.species3,Pinus=pred.species1, Picea=pred.species2)
df_long = gather(df, key= "SpeciesID", value="dbh", -y)

# skapa och spara ny KM graf (enklare version)
p<-ggsurvplot(fit, data = data.in, 
              legend = c("right"), 
              legend.labs = c("Q. robur", "P. sylvestris", "P. abies"),
              xlab = "Dbh, cm",
              ylab = "Sannolikhet för mortalitet",
              title = "Kaplan-Meier skattning: mortalitet med lognormalfördelning")

p$plot = p$plot + 
  geom_line(data = df_long, aes(x = dbh, y = y, group = SpeciesID), linetype = "dashed") +
  theme_classic()

print(p) 

# ++++++++++++++++++++++++++ GLM - regressionsanalys ++++++++++++++++++++++++++

#GLM binomial logit (logistic regression) 

####### GLM för data.in (utan sandvik)
data.in$Trädslag <-as.factor(data.in$SpeciesID) #faktor 
levels(data.in$Trädslag) <- list(Quercus = "oak", Pinus = "pine", Picea = "spruce")

# recode för att det behövs - inte längre en survivalship kurva men en regressionsmodel 
data.in<- data.in %>% 
  mutate(status = ifelse(status == 1, 0, 1)) 

# regressionsmodel, binärt utfall, binomial logit link funktion 
glm_modell <- glm(status ~ dbh + Trädslag, family = binomial, data = data.in)
summary(glm_modell) # statistiskt signifikant 

# konfiensintervaller 
confint(glm_modell) 

# beräkna koefficienter för att räkna ut odds 
coef(glm_modell) 
exp(coef(glm_modell)) 

# Odds med konfidensintervaller kombinerat 
exp(cbind(OR = coef(glm_modell), confint(glm_model)))# For a one unit increase in dbh, the odds of being being dead (mortality) (versus being alive) decrease by a factor of 0.7. 

# expand grid 
ny.df <- expand.grid(dbh = seq(min(data.in$dbh, na.rm = TRUE), max(data.in$dbh, na.rm = TRUE), length.out = 100),
                     Trädslag = levels(data.in$Trädslag))

# gör en prediction och spara som ny variabel 
ny.df$sannolikhet_mor <- predict(glm_modell, ny.df, type = "response")

# Visualisera 
ggplot(ny.df, aes(x = dbh, y = sannolikhet_mor, color= Trädslag)) +
  geom_line() +
  labs(x = "Dbh, cm", y = "Sannolikhet för mortalitet") +
  ggtitle("Regressionsanalys: mortalitet och dbh")+ 
  theme_classic()

# kontrollera redisualer / diagnostik.  
plot(residuals(glm_modell)) # inget tecken på heteroscedacity men tecken på inte så slupmässig spridning... det finns en viss struktur i residualdatat. 
# OBS denna metod är svår att tolka. Bättre med DHARMa
# https://stats.stackexchange.com/questions/267461/how-do-i-get-the-residuals-for-a-glm-with-a-binary-response-variable-using-r


#install.packages("DHARMa")
library(DHARMa)

#kontrollera residualer 
res = simulateResiduals(glm_modell)
plot(res) # inga problem med residualerna 


# ++++++++++++++++++++++++++ GAM  ++++++++++++++++++++++++++

# Testa linjär regression fast med GAM binomial logit (logistic regression) med spline 
# fortfarande endast för sjömunnen och norskog 

library (mgcv) 

# bara med dbh och trädslag för att jämföra med GLM 
gam_modell_1 <- gam(status ~ s(dbh) + Trädslag, family = binomial, method= "REML", data = data.in)
summary(gam_modell_1)  

# expand grid 
ny.df <- expand.grid(dbh = seq(min(data.in$dbh, na.rm = TRUE), max(data.in$dbh, na.rm = TRUE), length.out = 100), #EXPANDERA 100 Gånger 
                     Trädslag = levels(data.in$Trädslag)) 

# gör en prediction och spara som ny variabel 
ny.df$sannolikhet_mor <- predict(gam_modell_1, ny.df, type = "response") #modell_1 för att jämföra med GLM 

# Visualisera 
ggplot(ny.df, aes(x = dbh, y = sannolikhet_mor, color= Trädslag)) +
  geom_line() +
  labs(x = "Dbh, cm", y = "Sannolikhet för mortalitet") +
  ggtitle("GAM regressionsanalys: mortalitet och dbh")+ 
  theme_classic()

res = simulateResiduals(gam_modell_1)
plot(res) # inga problem med residualerna 

AIC(gam_modell_1, glm_modell) #gam bättre modell 
BIC(gam_modell_1, glm_modell) #gam bättre modell

# ++++++++++++++++++++++++ Kontroll för att se om dbh som spline skilljer sig mellan lokalerna  ++++++++++++++++++++

# skapa enskilda modeller för varje lokal 
gam_modell_dbh_sjo<-gam(status ~ s(dbh, k=20), family = binomial, method= "REML", data = data.oak.sjo) #bara sjömunnen
gam_modell_dbh_norr<-gam(status ~ s(dbh,k=20), family = binomial, method= "REML", data = data.oak.norr) #bara norrskog 
gam_modell_dbh_sand<-gam(status ~ s(dbh,k=20), family = binomial, method= "REML", data = sandvik2023) #bara sandvik 
summary(gam_modell_dbh_sjo) #mest linjär men ändå anpassat till smoothing s() eftersom edg är ändå högre en 1 
summary(gam_modell_dbh_norr)
summary(gam_modell_dbh_sand)

# kontroller residualerna 
res = simulateResiduals(gam_modell_dbh_sjo)
plot(res) # inga problem med residualerna 
res = simulateResiduals(gam_modell_dbh_norr)
plot(res) # OBS !!!!!           problem med residualerna. Försök att logtransformera. 
res = simulateResiduals(gam_modell_dbh_sand)
plot(res) # inga problem med residualerna 

# gör normalitetstest och logtransformation för norrskog  
shapiro.test(data.oak.norr$dbh)
hist(log(data.oak.norr$dbh))
shapiro.test(log(data.oak.norr$dbh)) #icke sig 
data.oak.norr$dbh<-log(data.oak.norr$dbh)

# dbh logtransformerat ny modell  
gam_modell_dbh_norr2<-gam(status ~ s(dbh,k=20), family = binomial, method= "REML", data = data.oak.norr) #bara norrskog 

# kontrollera residualer för den nya modellen 
res = simulateResiduals(gam_modell_dbh_norr2)
plot(res) # fortfarande problem OBS!!! 

# graf med residualer och partiella effekter 
layout(matrix(1:4, ncol = 2))
plot(gam_modell_dbh_sjo, resid=T,pch = 1, all.terms = TRUE) 
abline(h = 0, lty = 'dashed')
plot(gam_modell_dbh_norr,resid=T,pch = 1, all.terms = TRUE)
abline(h = 0, lty = 'dashed')
plot(gam_modell_dbh_sand,resid=T,pch = 1, all.terms = TRUE)
abline(h = 0, lty = 'dashed')

#++++++++++++++++++++++POST HOC analys EMMEANS ++++++++++++++++++++++

#https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

#sjömunnen 
emmeans(gam_modell_dbh_sjo, ~dbh, type="response") #visar för 11.1....kanske för att det är medelvärdet? 
summary(data.oak.sjo$dbh) #mean is 11.12 
emmeans(gam_modell_dbh_sjo, ~dbh, at = list(dbh = c(5, 10, 15, 20, 25)), type="response") #respons är bra för att det visar orginalskalan...för att det är en logit-link funktion.  

#norrskog 
emmeans(gam_modell_dbh_norr, ~dbh, at = list(dbh = c(5, 10, 15, 20, 25)), type="response") 

#sandvik 
emmeans(gam_modell_dbh_sand, ~dbh, at = list(dbh = c(5, 10, 15, 20)), type="response") 



#++++++++++++++++++++++++++++++++++++ SANDVIK  ++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++ SANDVIK +++++++++++++++++++++++++++++++++++++++

# *ID* Individuella ekträd - provyta 1 m radie från stammen
# *dbh* diameter vid brösthöjd
# *F_status och E_status* status levande 1 / död 0 före och efter bränning 
# *kronhojd* avstånd från marken i meter (m) till trädkronan (första gren)
# *F_humus och E_humus* djup på organiska humuslager i varje väderriktning, väst, syd, öst, nord (cm)
# *F_stubbskott, E_stubbskott* skott på marknivå 
# *F_stamskott, E_stamskott* skott på stammen 
# *Flamma_max, flamma_min* högsta punkten där elden bränt stammen, samt lägsta punkten
# *barrforna* barrförna
# *lovforna* - lövförna (främst eklöv)
# *friskmossa* - husmossa, väggmossa m.m?
# *blabaris* - blåbärsris
# *grenar* - död klena grenar på marken

# +++++++++++++++++++++++++++++++++++++++++ Förberedelser +++++++++++++++++++

#install.packages('datasets')
#install.packages('caTools')
#install.packages('party')
#install.packages('dplyr')
#install.packages('missForest')
#install.packages("rpart.plot")
#install.packages("rpart")
#install.packages('rsample')
#install.packages("caret") 
#install.packages("ROSE")

library (readxl)     # import från excel 
library (ggfortify) # överlevnadsanalys
library (car)     # kanske inte behövs   
library (missForest) # för imputation 
library (rpart.plot) # decision tree 
library (rpart)  # decision tree 
library (rsample) #decision tree 
library (caret) #confusion matrix 
library (ROSE) # resampling 
library (randomForest)

# +++++++++++++++++++++++++++++++++++++++++ Importera data +++++++++++++++++++

#huvudata 
sandvik<- read_excel("ek_mortalitet_sandvik_20240907.xlsx", na = "NA") 

#omgivning 
sandvik_om <- read_excel("ek_mortalitet_sandvik_20240907.xlsx", 3, na = "NA") 
# +++++++++++++++++++++++++++++++++++++++++ Korrigeringar format +++++++++++++++++++

attach(sandvik)

# gör om till faktor 
sandvik <- sandvik %>%
  mutate(
    across(c(barrforna, lovforna, friskmossa, grenar, blabarsris,lingonris, F_stubbskott, E_stamskott,E_stubbskott, F_stamskott,E_status, F_status), as.factor)
  )

# Fixa omgivningsdata 
sandvik_omgivning_sum<-sandvik_om%>% 
  dplyr::select(ID, diameter, status)%>% #konflikt med MASS så måste skriva dplyr 
  na.omit (diameter) %>% #tar bort de enstaka NA för att det kommer fyllas in automatiskt i nästa steg 
  complete(ID, status = c(0, 1)) %>%  # Autoifyllning för att när det inte fanns träd inom provytan så noterades ID inte ner, men detta behövs representeras som O. 
  replace_na(list(diameter = 0))%>% #ersätter NA med 0 
  mutate(grundyta_omgiv = 0.0001*pi*(diameter/2)^2)%>% #0.0001 för m2 
  group_by(ID, status)%>% 
  summarize(grundyta_omgiv = sum(grundyta_omgiv, na.rm = TRUE))%>% 
  pivot_wider( # så att allt hamnnar i samma rad 
    names_from = status, 
    values_from = grundyta_omgiv, 
    names_prefix = "status_"
  ) %>% 
  rename( #unika namn 
    dod_omgiv = status_0,
    levande_omgiv = status_1
  ) %>% 
  dplyr::select(ID, dod_omgiv, levande_omgiv) #tar bort statuskolumn.


# Omgivningsdata, endast för Gran så att gran kan plockas ut och analyseras enskilt
id_sekvens <- paste0("slo-sv", sprintf("%04d", 01:205)) #För gran så skapar jag först en lista för att fyll in glapp i datat för gran (för att få ett jämt antal värden)

sandvik_omgivning_gran<-sandvik_om%>% 
  dplyr::select(ID, diameter, tradslag, status)%>% 
  filter(tradslag == 3)%>%  #gran = 3 
  filter(!is.na(diameter)) %>% #tar bort enstaka NA för att det kommer fyllas in automatiskt vid nästa steg 
  complete(ID = id_sekvens, status = c(0, 1)) %>%  # Autoifyllning för att det saknas data för många provytor
  replace_na(list(diameter = 0))%>% #ersätter NA med 0 
  mutate(grundyta_gran = 0.0001*pi*(diameter/2)^2)%>% #0.0001 för m2 
  group_by(ID, status)%>% 
  summarize(grundyta_gran = sum(grundyta_gran, na.rm = TRUE))%>% 
  pivot_wider(
    names_from = status, 
    values_from = grundyta_gran, 
    names_prefix = "status_"
  ) %>% 
  rename(
    dod_gran = status_0,
    levande_gran = status_1
  ) %>% 
  dplyr::select(ID, dod_gran, levande_gran)


##### Vill kombinera all omgivningsdata (dod_omgiv, levande_omgiv, dod_gran och levande_gran) med sandvik_rent. Imputera även NA.   

# NA är ett problem 
sum(is.na(sandvik)) #75 NA värden

# lägger ihop värden från alla DF och tar bort träd som saknas från tabellen. 
sandvik_merge<-merge(sandvik, sandvik_omgivning_sum) %>%
  merge(sandvik_omgivning_gran)%>%
  drop_na(E_status) #Raderar alla träd som inte gick att hitta. Förlorar 7 observationer. 

names(which(colSums(is.na(sandvik_merge)) > 0)) # kollarvilka kolumner som fortfarande innehåller NA

# +++++++++++++++++++++++++++++++++++++++++ Imputation ++++++++++++++++++++++++++++++++

# Imputation med missforest för restrerande variablerna som saknas 

# sandvik_imput <- missForest(sandvik_merge)$ximp #funkar inte då ID finns med, så måste ta bort ID först och sen köra imputering

#nytt försök. Tar bort ID tillfälligt 
sandvik_imput_no_id <- sandvik_merge %>% 
  dplyr::select(-ID)  

#imputation 
sandvik_imput<- missForest(sandvik_imput_no_id)$ximp 

#lägger tillbaks ID 
sandvik_imput <- sandvik_imput%>% 
  mutate(ID = sandvik_merge$ID) %>%
  relocate(ID) #Så att ID intar första position igen 


sum(is.na(sandvik_imput)) #0 NA värden

sandvik_rent<-sandvik_imput%>% #lägger till nya kolumner: genomsnittliga värden för humuslager och sotningshöjd och kombinerar stubb och stamskott till "skott", delar upp i diametersklasser %>% 
  rowwise() %>% 
  mutate(
    humus_v_skillnad = F_humus_v - E_humus_v,
    humus_s_skillnad = F_humus_s - E_humus_s,
    humus_o_skillnad = F_humus_o - E_humus_o,
    humus_n_skillnad = F_humus_n - E_humus_n,
    humus_snitt_skillnad = mean(c(humus_v_skillnad,humus_s_skillnad,humus_o_skillnad,humus_n_skillnad)),
    F_humus_snitt = mean(c(F_humus_v,F_humus_s,F_humus_o,F_humus_n)),
    E_humus_snitt = mean(c(E_humus_v,E_humus_s,E_humus_o,E_humus_n)), 
    flamma_snitt = mean(c(flamma_max,flamma_min)),
    skott = as.factor(case_when(E_stubbskott == 1| E_stamskott == 1~ 1, TRUE ~ 0)),
    diameterklass = cut(dbh, breaks=c(0, 5, 10, 15,20,25,30,35, 40, 45))
  )


# +++++++++++++++++++++++++++++++++++++++++ Kontroller ++++++++++

attach(sandvik_rent)

# histogram

hist(flamma_max) #inte normalfördelat ut 
hist(log(flamma_max)) #logaristmisk transformation 
shapiro.test(log(flamma_max)) #är normalfördelat - inte statistisk signifikant 

hist(humus_snitt_skillnad)#ser normalfördelat ut 
shapiro.test(humus_snitt_skillnad)#är normalfördelat - inte statistisk signifikant

#frågan är om omgiv data behöver a) normaliseras b) ändra skala? 
histogram(log(sandvik_rent$dod_omgiv))
shapiro.test(log(sandvik_rent$dod_omgiv+0.00001))
histogram(log(sandvik_rent$levande_omgiv))
histogram(log(sandvik_rent$levande_gran)) #Borde fixa 
histogram(log(sandvik_rent$dod_gran)) # Borde fixa 

#lägger till log transformerad variabel 
sandvik_rent<-sandvik_rent%>% 
  mutate(dod_omgiv_log = log(dod_omgiv+0.00001))%>% 
  mutate(levande_omgiv_log = log(levande_omgiv+0.00001))%>% 
  mutate(levande_gran_log = log(levande_gran+0.00001))%>% 
  mutate(dod_gran_log = log(dod_gran+0.00001))%>%
  mutate(flamma_max_log=log(flamma_max))


## Colinearity - ignorera för tillfället 

# +++++++++++++++++++++++++++++++++++++++++ Beskrivande statistik +++++++++++++++++++


fct_count(E_status, sort = FALSE, prop = FALSE) #184 döda träd, 14 levande 
fct_count(skott, sort = FALSE, prop = FALSE) #110 träd med skottskjuting, 88 utan
fct_count(E_stamskott, sort = FALSE, prop = FALSE) #15 träd med stamskott, 183 utan
fct_count(E_stubbskott, sort = FALSE, prop = FALSE) #102 träd med stubbmskott, 96 utan
fct_count(friskmossa, sort = FALSE, prop = FALSE) #147 ytor med friskmossa, 51 utan
fct_count(blabarsris, sort = FALSE, prop = FALSE) #110 ytor med blåbärsris, 87 utan

max(flamma_max) #6.1
min(flamma_max)#0.16 
mean(flamma_max)#1.2669m 
mean(dbh) #7.29cm 

#Pearsons correlations tester 
cor.test(dbh, flamma_max) 
cor.test(kronhojd, flamma_max) 

#fördelning av sotningshöjd per diameterklass 
ggplot(sandvik_rent, aes(diameterklass, flamma_max))+ 
  geom_boxplot()+
  labs(x="Diameterklass (cm)", y="Sotningshöjd (m)", title="Boxplot: Sotningshöjd i förhållande till diameterklass")+
  theme_classic()#+
#theme(axis.text.x = element_text(color = "black", vjust=0.5),axis.text.y = element_text(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black"))


#Histogram fördelningen av dbh  
hist((dbh), breaks = 50)

ggplot(sandvik_rent, aes(x=dbh)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme_classic()+ 
  labs(x="Dbh, cm", y="Densitet", title="Histogram med täthetsfunktion: Sandvik dbh")


ggplot(sandvik_rent, aes(x=humus_snitt_skillnad)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme_classic()+ 
  labs(x="Humusförbränning, cm", y="Densitet", title="Histogram med täthetsfunktion: Sandvik humusförbränning")

# Kronbrand 
sandvik_rent<- sandvik_rent %>% 
  mutate(kronbrand = as.factor(ifelse(flamma_max< kronhojd, 0, 1))) #ränkar ut hur många träd hade kronbrand
sandvik_rent%>% 
  group_by (kronbrand)%>%
  count() #Räknar ut hur många kronbrandsträd det kanske var på sandvik. #162 ej kronbrand, 36 kronbrand. 

#Basic plots TEST 
plot(flamma_max, type = 'p',col = "darkred", pch=8)
plot(dod_omgiv,type = 'b',col = "darkgreen", pch=1)
plot(levande_omgiv,type = 'b',col = "darkgreen", pch=1)


########## Spridningsdiagrammer  2D 

# kronbrand 
ggplot(sandvik_rent, aes(kronhojd, flamma_max, color = E_status))+
  geom_point()+
  geom_abline(intercept = , slope =, color = "grey")+ 
  theme_classic()+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color="grey", size = 1))+
  labs(x="Kronhöjd, m", y="Sotningshöjd, m", color= "Status", title="Spridningsdiagram: Kronbrand", abline = "kronbrand")


# sotningshöjd på levande och döda träd
ggplot(sandvik_rent, aes(dbh, flamma_max, color = E_status))+
  geom_point()+
  #stat_ellipse(type = "t") + #möjliga variabler "t", "norm", "euclid"
  theme_classic()+ 
  theme(legend.position = c(0.15, .95),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color="grey", size = 1))+ 
  labs(x="Dbh, cm", y="Sotningshöjd, m", color= "Status", title="Spridningsdiagram: sotningshöjd")

# sotningshojd och humusförbränning på levande och döda träd 
ggplot(sandvik_rent, aes(E_humus_snitt, F_humus_snitt,  color = E_status))+ 
  geom_point()+
  #stat_ellipse(type = "t") +  #möjliga variabler "t", "norm", "euclid"
  theme_classic()+
  labs(x="Humusdjup efter bränning, cm", y="Humusdjup innan bränning, cm", color= "Status", title="Spridningsdiagram: Humusförbränning")
# överlevnad (intakt krona) verkar bildlE_humus_snitt# överlevnad (intakt krona) verkar bildligt kopplat till lägre humusförbränning och lägre sotningshöjden 

# sotningshojd och humusförbränning på levande (skott) och döda träd 
ggplot(sandvik_rent, aes(humus_snitt_skillnad,flamma_max, color = skott))+ 
  geom_point()+
  stat_ellipse(type = "t")  + #möjliga variabler "t", "norm", "euclid"
  theme_classic()


########## Spridningsdiagrammer  3D 

# Häftig 3D plot 
# install.packages("plotly")
library(plotly)
plot_ly(sandvik_rent, x = ~dbh, y =~kronhojd , z = ~flamma_max, color = ~E_status, type = "scatter3d", mode = "markers" ) %>%
  add_trace(
    x = c(0,max(sandvik_rent$dbh)),  #
    y = c(min(0), max(sandvik_rent$kronhojd)),  
    z = c(min(0), max(sandvik_rent$flamma_max)),  
    type = "scatter3d",
    mode = "lines",
    line = list(color = 'grey', width = 1),
    inherit = FALSE)%>%
  layout(
    scene = list(
    xaxis = list(autorange = "reversed"),
    yaxis = list(title = "Kronhöjd"),
    zaxis = list(title = "Sotningshöjd")))


#### facet_grid #use: (saved graph)+ facet_grid(variabel ~ variabel)

#### boxplot

# boxplot för sothöjd och status 
ggplot(sandvik_rent, aes(E_status, flamma_max))+ 
  geom_boxplot()+
  labs(x="Status (död/levande)", y="Sotningshöjd", title="Sotningshöjd (högsta punkt) på döda och levande träd")+
  theme_classic()

# boxplot för Sotningshöjd (lägsta punkt) och status 
ggplot(sandvik_rent, aes(E_status, flamma_min))+ 
  geom_boxplot()+
  labs(x="Status (död/levande)", y="Sotningshöjd minimum höjd", title="Sotningshöjd (lägsta punkt) på döda och levande träd")+
  theme_classic()

# humusförbränning och skott 
ggplot(sandvik_rent, aes(skott, humus_snitt_skillnad))+ 
  geom_boxplot()+
  labs(x="Skott", y="Humus förbränning", title="Humus förbränning och skottbildning")+
  theme_classic()

# +++++++++++++++++ Beskrivande statistik jämförelsen mellan lokaler +++++++++++++++++++

#data och procent hämtat från analyser från norrskog och sjömunnen 

lokaler<- data.frame(
  lokal = c("Sjömunnen", "Norrskog", "Sandvik"),
  dbh = c(11.12, 8.47, 7.29), 
  sotningshojd = c(0.67, 1.35, 1.27), 
  mortalitet = c(58.3, 81.1, 92.9), 
  skott = c(63.8, 60.6, 55.6)
)

lokaler

# Calculate radius and area (since area is proportional to the square of radius)
lokaler$radius <- lokaler$dbh / 2
lokaler$yta <- pi * (lokaler$radius^2)



#GILLAR INTE DENNA 
ggplot(lokaler, aes(x = lokal, y = 0, size = yta)) +
  geom_bar(aes(y = mortalitet), stat = "identity", fill = "grey", alpha = 0.7, width = 0.4) +
  geom_point(shape = 1, color = "#654321") +    
  scale_size_area(max_size = 50) + #storlek 
  scale_y_continuous(
    name = "Mortalitet (%) ",  
    sec.axis = sec_axis(~ . / 65, name = "Sotningshöjd (m)"))+
  geom_point(aes(y = sotningshojd * 65), shape = 8, color = "red", size = 3) + 
  geom_text(aes(label = c("11,12 cm dbh", "8,47 cm dbh", "7,29 cm dbh")), vjust = 0, hjust = .48, size = 3.2) +theme_classic() +
  ggtitle("Mortalitet, dbh och sotningshöjd")+
  xlab("") +
  ylab("") +
  theme(axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"),  
        legend.position = "none") 

lokaler_piv <- lokaler %>%
  pivot_longer(cols = c(mortalitet, skott), names_to = "type", values_to = "value")


#NYTT FÖRSÖK MED ENDAST FLAMHÖJD - gillar inte heller denna 
ggplot(lokaler, aes(x = lokal, y = 0, size = yta)) +
  geom_point(shape = 1, color = "#654321", position = ) +   
  scale_y_continuous(
    breaks = seq(0.6, 2, by = .1), 
    labels = seq(0.6, 2, by = .1))+  
  scale_size_area(max_size = 50) + #storlek 
  geom_point(aes(y = sotningshojd), shape = 8, color = "red", size = 3) + 
  geom_text(aes(label = c("11,12 cm dbh", "8,47 cm dbh", "7,29 cm dbh")), vjust = 0, hjust = .48, size = 3.2) +theme_classic() +
  ggtitle("Dbh och sotningshöjd")+
  xlab("") +
  ylab("                 Sotningshöjd (m)") +
  theme(axis.text.y.left = element_text(color = "red"),
        axis.title.y.left = element_text(color = "red"),
        legend.position = "none") 


# Jämförelse av mortalitet och skotskjutning - gillar inte heller denna 
lokaler_piv <- lokaler %>%
  pivot_longer(cols = c(mortalitet, skott), names_to = "type", values_to = "value")

ggplot(lokaler_piv, aes(x = lokal, y = value, fill = type))+
  scale_y_continuous(
    expand = c(0,0), #eller expand = expansion(mult = c(0, .1)
    limits= c(0,100),
    breaks = seq(0, 100, by = 20), 
    labels = seq(0, 100, by = 20))+ 
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.5) +
  ggtitle("Mortalitet och Skottskjutning") +
  xlab("") +
  ylab("Procent (%)") +
  scale_fill_manual(values = c("mortalitet" = "grey", "skott" = "lightgreen"), 
                    labels = c("Krondöd(%)", "Skottskjutning (%)"),name = "") + 
  theme_classic()


# +++++++++++++++++++++++++++++++++++++++++ Statistisk analys +++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++ BESLUTSTRÄD SKOTT  +++++++++++++++++++

# Målet är att 1) förutse mortalitet 2) förutse skottskjutningsförmågan 

#Första försöket ## https://www.geeksforgeeks.org/decision-tree-in-r-programming/

## NYTT FÖRSÖK 1 ## https://www.datacamp.com/tutorial/decision-trees-R
#funkar inte heller - behöver något för binärt utfall 
# install.packages('ISLR')
# library(ISLR)
# library(tidyr)
# install.packages('parsnip')
# library(parsnip)
# tree_fit <- tree_spec %>%
# fit(flamma_max ~ ., data = train_data)
# predictions <- tree_fit %>%
# predict(test_data) %>%
# pull(.pred)
# metrics <- metric_set(rmse, rsq)
# model_performance <- test_data %>%
# mutate(predictions = predictions) %>%
# metrics(truth = medv, estimate = predictions

## NYTT FÖRSÖK 2 - RPART ## https://fderyckel.github.io/machinelearningwithr/trees-and-classification.html

## skott
sandvik_skott <-sandvik_rent%>%
  select(skott, flamma_max_log, humus_snitt_skillnad, friskmossa, grenar, blabarsris, lovforna, barrforna, lingonris, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, E_humus_snitt, F_humus_snitt, dbh, kronhojd)

set.seed(123)
data_split <- initial_split(sandvik_skott, prop = 0.8)
traningsset <- training(data_split)
testset <- testing(data_split)

tree <- rpart(skott ~ ., data = traningsset, cp=0.0001)
rpart.plot(tree)

prognos <- predict(tree, newdata = testset, type = "class")
conf_matrix <- confusionMatrix(prognos, testset$skott)
print(conf_matrix) #Ingen bra modell - presterar sämre än NIR (no info rate)


############# Slutsats: helt ok men är mycket bättre om det kan vara ett aggregat av betydelseträd

# +++++++++++++++++++++++++++++++++++++++++ RANDOM FOREST SKOTT +++++++++++++++++++

###### skott modell_1 = dålig accuracy 

#välj variabler 
skott <- sandvik_rent%>%
  select(skott, flamma_max, humus_snitt_skillnad, friskmossa, grenar, blabarsris, lovforna, barrforna, lingonris, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, E_humus_snitt, F_humus_snitt, dbh, kronhojd)

#dela upp i tränings och dataset 
set.seed(123)
data_split <- initial_split(skott, prop = 0.8)
traningsset_skott <- training(data_split)
testset_skott <- testing(data_split)

#bygg modellen 
modell_1<- randomForest(skott ~ ., data = traningsset_skott, ntree = 1000, mtry = 4) #(vanligvist square root of predictors 17 = 4)

# prognos 
RF_skott_prog <- predict(modell_1, newdata = testset_skott)

# kontroll med förvirringsmatris 
confusionMatrix(RF_skott_prog , testset_skott$skott) 

# betydelsegraf 
randomForest::varImpPlot(modell_1, 
                         sort=TRUE, 
                         main="Variable Importance Plot") 

###### skott modell_2

#välj variabler 
skott <-sandvik_rent%>%
  select(skott, flamma_max, humus_snitt_skillnad, friskmossa, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, E_humus_snitt, F_humus_snitt, dbh, kronhojd)

#dela upp i tränings och dataset 
set.seed(123)
data_split <- initial_split(skott, prop = 0.8)
traningsset_skott <- training(data_split)
testset_skott <- testing(data_split)

#bygg modellen 
modell_2<- randomForest(skott ~ ., data = traningsset_skott, ntree = 1000, mtry = 3) #(vanligvist square root of predictors 10 = 3)

# prognos 
RF_skott_prog <- predict(modell_2, newdata = testset_skott)

# kontroll med förvirringsmatris 
confusionMatrix(RF_skott_prog , testset_skott$skott) 

# betydelsegraf 
randomForest::varImpPlot(modell_2, 
                         sort=TRUE, 
                         main="Variable Importance Plot") 


# kontrollerar korrelation
cor <-sandvik_rent%>%
  select(flamma_max, humus_snitt_skillnad, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, E_humus_snitt, F_humus_snitt, dbh, kronhojd)

print(cor(cor)) 
#Högt korrelerade variabler är flamma max och dbh (0.63), samt humus_snitt_skillnad och F_humus_snitt (0.78). Väljer att ta bort  F_humus_snitt och flamma_max. 


###### skott modell_3

# tar bort korrelerade värden och andra värden som inte har så stor betydelse i betydelsegrafen. 
skott <-sandvik_rent%>%
  select(skott, friskmossa, flamma_max, humus_snitt_skillnad, dod_omgiv_log, levande_omgiv_log, levande_gran_log, E_humus_snitt)

#dela upp i tränings och dataset 
set.seed(123)
data_split <- initial_split(skott, prop = 0.8)
traningsset_skott <- training(data_split)
testset_skott <- testing(data_split)

# bygg modellen 
modell_3<- randomForest(skott ~ ., data = traningsset_skott, ntree = 501, mtry = 3) #(vanligvist square root of predictors 8 = 3)

# prognos 
RF_skott_prog <- predict(modell_3, newdata = testset_skott)

# kontroll med förvirringsmatris 
confusionMatrix(RF_skott_prog , testset_skott$skott) 

# betydelsegraf 
randomForest::varImpPlot(modell_3, 
                         sort=TRUE, 
                         main="Variable Importance Plot") 


# +++++++++++++++++++++++++++++++++++++++++ "BESLUTSTRÄD MORTALITET(E_STATUS)" +++++++++++++++++++

#val av variabler 
sandvik_status <-sandvik_rent%>%
  select(E_status, flamma_max, humus_snitt_skillnad, friskmossa, grenar, blabarsris, lovforna, barrforna, lingonris, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, F_humus_snitt, dbh, kronhojd)


#skapandet av testset och träningsset 
set.seed(123)
data_split_status <- initial_split(sandvik_status, prop = 0.8)
traningsset_status <- training(data_split_status)
testset_status <- testing(data_split_status)

# enskild betydelsegraf 
tree_status <- rpart(E_status~ ., data = traningsset_status, cp=0.0001)
rpart.plot(tree_status)

#proognos 
prognos <- predict(tree_status, newdata = testset_status, type = "class")

#förvirringsmatris 
conf_matrix <- confusionMatrix(prognos, testset_status$E_status)
print(conf_matrix) #Alldeles för hög prediktability - problem då så många träd dog. Kappa 0, ingen neg pred val. Lösning: resampling. "Oversampling the minority class/ undersampling the majority class can help balance the dataset" 


###### fixa databalansen för för få mer träning på utfallet för överlevnad. Får endast vara för träningsdatat 
balanced_data <- ROSE(E_status ~ ., data = traningsset_status, seed=123)$data
status_balanced <- rpart(E_status ~ ., data = balanced_data, cp = 0.0001) #använder balanced istället  

table(balanced_data$E_status) #84 (0) och 74 (1)  #bra 

#betydelsegraf 
rpart.plot(status_balanced)

#prognos 
prognos <- predict(status_balanced, newdata = testset_status, type = "class")
conf_matrix <- confusionMatrix(prognos, testset_status$E_status) 
print(conf_matrix) #ok men inte fantastiskt??? 

## cross validation behövs 
kontroll <- trainControl(method = "cv", number = 10) # Träna modellen med 10 olika värden på cp (komplexitet)
CV_tree <- train(E_status ~ ., data = balanced_data, method = "rpart", 
                 trControl = kontroll, 
                 tuneLength =10)  # antalet cp-värden 
print(CV_tree$bestTune) # cp 0.04954955 #sen blev det 0? Kanske fel någonstans. O can sometimes indicate overfitting, where the decision tree becomes too complex.
plot(CV_tree) #visar att en komplex graf är rätt 

CV_optimised<-rpart(E_status ~ ., data = balanced_data, cp = 0)
rpart.plot(CV_optimised)
prognos <- predict(CV_optimised, newdata = testset_status, positive = "class")
conf_matrix <- confusionMatrix(prognos, testset_status$E_status)
print(conf_matrix) # något konstigt då cp blev 0. Dessutom är positive class:0 när det borde vara tvärt om. positivt kappavärde. Mcnemar test p-value: 0.2482, ingen skillnad mellan hur modellen hanterar de två klasserna. 0.925  


# +++++++++++++++++++++++++++++++++++++++++ "RANDOM FOREST MORTALITET(E_STATUS)" +++++++++++++++++++

# RANDOM FOREST 

####### MORTALITET 

# Kom ihåg att de korrelerade värden var: flamma max och dbh (0.63), samt humus_snitt_skillnad och F_humus_snitt (0.78)

library(caret)
library(ROSE)

#korsvalidering 
control <- trainControl(method = "cv", 
                        number = 5,              # Antal "folds" (delar upp in bins) Fyra tråningset som testas mot ett. 
                        classProbs = TRUE,       # För att beräkna sannolikheter
                        summaryFunction = twoClassSummary, # binärt - används för att utvärdera klassificeringen
                        savePredictions = TRUE) 

# val av variabler 
status<-sandvik_rent%>%
  select(E_status, dbh, flamma_max, humus_snitt_skillnad,levande_gran_log,levande_omgiv_log, kronhojd, lingonris)

levels(status$E_status) <- c("dod", "levande")# Det blev problem med korsvalidering så jag kodar om till synthetically valid names 
set.seed(123) 

#Dela upp data med partition så det blir representativt 
traning_index <- createDataPartition(status$E_status, p = .8, list = FALSE, times = 1)
traning <- status[traning_index,]
test <- status[-traning_index,]

#balancerar datat med ROSE ENDAST FÖR TRÄNINGDATASET OBS 
balanced<- ROSE(E_status ~ ., data = traning, seed=123)$data #genererar fler värden 
table(balanced$E_status) # bra 

#skapar modell utifrån den balanserade träningsdatat 
modell <- train(E_status ~ ., 
                data = balanced, 
                method = "rf", 
                trControl = control, 
                tuneGrid = expand.grid(mtry = 2:5))

#variable importance (caret)  
imp <- varImp(modell, scale = FALSE) 
imp # mellan0-100 

#visa bara top 5 
plot(imp, top = 5)

############### Utvärdering 

#prognos 
prognos <- predict(modell, newdata = traning)

#förvirringsmatris 
confusionMatrix(prognos, traning$E_status,positive ="levande") 
#inte super bra alls 

# ++++++++++++++++++++ gör ändringar ++++++++++++++++++ 

status<-sandvik_rent%>%
  select(E_status, dbh, flamma_max, humus_snitt_skillnad, F_humus_snitt, E_humus_snitt, dod_gran_log, dod_omgiv_log, levande_gran_log,levande_omgiv_log, kronhojd)

levels(status$E_status) <- c("dod", "levande")# Det blev problem med korsvalidering så jag kodar om till synthetically valid names 
set.seed(123) 

traning_index <- createDataPartition(status$E_status, p = .8, list = FALSE, times = 1)
traning <- status[traning_index,]
test <- status[-traning_index,]

#balancerar datat med ROSE 
balanced<- ROSE(E_status ~ ., data = traning, seed=123)$data #genererar fler värden 
table(balanced$E_status)

#skapar modell utifrån den balanserade träningsdatat 
modell <- train(E_status ~ ., 
                data = balanced, 
                method = "rf", 
                trControl = control, 
                tuneGrid = expand.grid(mtry = 2:5))

#variable importance (caret)  
imp <- varImp(modell, scale = FALSE) 
imp 
plot(imp, top = 5)

# utvärdering 
prognos <- predict(modell, newdata = traning)
confusionMatrix(prognos, traning$E_status,positive ="levande") 

# ++++++++++++++++++++ gör ändringar igen ++++++++++++++++++ 

status<-sandvik_rent%>%
  select(E_status, dbh, flamma_max, humus_snitt_skillnad, E_humus_snitt, dod_gran_log, dod_omgiv_log, levande_gran_log, kronhojd)

levels(status$E_status) <- c("dod", "levande")# Det blev problem med korsvalidering så jag kodar om till synthetically valid names 
set.seed(123) 

#Dela upp data med partition 
traning_index <- createDataPartition(status$E_status, p = .8, list = FALSE, times = 1)
traning <- status[traning_index,]
test <- status[-traning_index,]

#balancerar datat med ROSE 
balanced<- ROSE(E_status ~ ., data = traning, seed=123)$data #genererar fler värden 
table(balanced$E_status)

#skapar modell utifrån den balanserade träningsdatat 
modell <- train(E_status ~ ., 
                data = balanced, 
                method = "rf", 
                trControl = control, 
                tuneGrid = expand.grid(mtry = 2:5))

#variable importance (caret)  
imp <- varImp(modell, scale = FALSE) 
imp 
plot(imp, top = 5)

# utvärdering 
prognos <- predict(modell, newdata = traning)
confusionMatrix(prognos, traning$E_status,positive ="levande") 
#ser bättre ut - NIR och acurracy jämställda 

# +++++++++++++++++++++ Utvärdering +++++++++++++

# En sista utvärdering med TESTdata 
prognos <- predict(modell, newdata = test)
confusionMatrix(prognos, test$E_status,positive ="levande") #accuracy är 1 vilket det inte var tidigare.. 

# graf 
imp <- as.data.frame(imp$importance)

# Omvandla data till ett lämplig format för ggplot
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
#imp <- imp %>% 
#arrange(desc(Overall)) #ranking efter betydelse 


ggplot(imp, aes(x = reorder(varnames, Overall), y = Overall)) +
  #geom_hline(yintercept = seq(0, max(imp$Overall), by = 1), linetype = "dotted", color = "lightgrey")+
  geom_point(size = 3, shape = 16 ,color = "darkblue") +  
  #scale_color_discrete(name="Variabelgrupp") +
  ylab("Betydelse (0-100)") +
  xlab("Variabel") +
  ggtitle("Betydelsegraf för Random Forest Modell") +  
  coord_flip() +  # Vänd axlarna
  scale_y_continuous(expand=c(0, 0), limits=c(0, 25))+ 
  theme_bw() +  
  theme(legend.position = "none")


#+++++++++++++++ visa ett enda beslutsträd från aggregationen i Random Forest? ++++++++++++


#pausa tills vidare.... 
modell <- train(E_status ~ ., data = balanced, method = "rpart")
rpart.plot(modell$finalModel, 
           main = "Beslutsträd för Sandvik",
           type = 3,            # Type of plot
           extra = 101,         # Display extra information at the nodes
           fallen.leaves = F, # Put leaves at the bottom of the plot
           shadow.col = "gray",  # Add shadows for better visualization
           box.palette = "Blues", # Use a color palette for the nodes
           cex = 0.8)           # Adjust the size of the text)


#+++++++++++++++++++++++++++++++++++ RANDOM FOREST stubbskott +++++++++++++++++++++++++++++++++++++++++

## STUBBSKOTT 
status<-sandvik_rent%>%
  select(E_stubbskott,flamma_min, flamma_max, humus_snitt_skillnad, friskmossa, grenar, blabarsris, lovforna, barrforna, lingonris, levande_omgiv_log, dod_omgiv_log, dod_gran_log, levande_gran_log, E_humus_snitt, F_humus_snitt, dbh, kronhojd)

levels(status$E_stubbskott) <- c("inga", "skott")# Det blev problem med korsvalidering så jag kodar om till synthetically valid names 
set.seed(123) # kommer inte ihåg om det behövs? 

#Dela upp data med partition # tänker göra det med ett validerings set då det blivit konstigt med träningsdata....
set.seed(123)
traning_index <- createDataPartition(status$E_stubbskott, p = .6, list = FALSE) 
traning <- status[traning_index,]
validering <- status[-traning_index,]

set.seed(123)
validering_index <- createDataPartition(validering$E_stubbskott, p=0.5, list=FALSE) #INNEBÄR ATT DET BLIR 20% VALIDATION OCH 20% TEST 
val <- validering[validering_index,]
test <- validering[-validering_index,]

# to choose different measures of performance
control <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

#spara modell 
modell <- train(E_stubbskott ~ ., 
                data = traning, 
                method = "rf", 
                trControl = control, 
                tuneGrid = expand.grid(mtry = 6),
                metric = "ROC")

#variable importance (caret)  
imp <- varImp(modell, scale = FALSE) 
imp # importance scores to be between 0 and 100
plot(imp)

# Utvärdering 
prognos <- predict(modell, newdata = val)
confusionMatrix(prognos, val$E_stubbskott,positive ="skott")


######### model 2 ######### 

status<-sandvik_rent%>%
  select(E_stubbskott, friskmossa, flamma_min, flamma_max, dod_omgiv_log, dod_gran_log, E_humus_snitt, F_humus_snitt, dbh)

levels(status$E_stubbskott) <- c("inga", "skott")# Det blev problem med korsvalidering så jag kodar om till synthetically valid names 


#Dela upp data med partition # tänker göra det med ett validerings set då det blivit konstigt med träningsdata....
set.seed(123) 
traning_index <- createDataPartition(status$E_stubbskott, p = .6, list = FALSE) 
traning <- status[traning_index,]
validering <- status[-traning_index,]

set.seed(123) 
validering_index <- createDataPartition(validering$E_stubbskott, p=0.5, list=FALSE) #INNEBÄR ATT DET BLIR 20% VALIDATION OCH 20% TEST 
val <- validering[validering_index,] 
test <- validering[-validering_index,] 


# to choose different measures of performance
control <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

# spara modell 
modell <- train(E_stubbskott ~ ., 
                data = traning, 
                method = "rf", 
                trControl = control, 
                tuneGrid = expand.grid(mtry = 4),
                metric = "ROC")

#variable importance (caret)  
imp <- varImp(modell, scale = FALSE) 
imp # importance scores to be between 0 and 100


# Utvärdering 
prognos <- predict(modell, newdata = val)
confusionMatrix(prognos, val$E_stubbskott,positive ="skott")

#++++++++++++++++++++++Slutgiltiga Utvärdering med TESTDATA, skott  ++++++++++++++++++++

# Slutgiltiga Utvärdering med TESTDATA 
prognos <- predict(modell, newdata = test)
confusionMatrix(prognos, test$E_stubbskott,positive ="skott") 

### graf 
imp <- as.data.frame(imp$importance)

# Omvandla datarammet till en lämplig form för ggplot
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
#imp <- imp %>% 
#arrange(desc(Overall)) #ranking efter betydelse 

###########
ggplot(imp, aes(x = reorder(varnames, Overall), y = Overall)) +
  #geom_hline(yintercept = seq(0, max(25), by = 1), linetype = "dotted", color = "lightgrey")+
  geom_point(size = 3, shape = 16 ,color = "darkgreen") +  
  scale_color_discrete(name="Variabelgrupp") +
  ylab("Betydelse (0-100)") +
  xlab("") +
  ggtitle("Betydelsegraf för Random Forest Modell") +  
  coord_flip() +  # Vänd axlarna
  scale_y_continuous(expand=c(0, 0), limits=c(0, 25))+ 
  theme_bw() +  
  theme(legend.position = "none")


# +++++++++++++++++++++++++++++++++++++++++ "Generalised Additive model" +++++++++++++++++++

# OBS tänk på NA värden. KLART! 

## https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gam.html

## modelleringsmetod - börjar med många variabler och tar bort 
## andvänd random forest klassificeringsresultaten som inspiration 
# notera att s(variabel, by = variabel) är lika med variabel:variable. 
# http://r.qcbs.ca/workshop08/workshop08-en/workshop08-en.html#30

#https://statisticseasily.com/logistic-regression-using-r/ GLM, beskrivning, antaganden och validering 

################ GAM eller GLM? 

# visa att det är bättre med icke linjär modell för dbh 
linear_model <- gam(E_status ~ dbh) #vanlig linjär modell med gam()
linear_model <- glm(E_status ~ dbh) #vanlig linjär modell med glm
nested_gam_model <- gam(E_status ~ s(dbh)) # med smoothing 
anova(linear_model, nested_gam_model, test = "Chisq") #definitivt bättre med icke linjär bättre modellering

#+++++++++++++++++++++++++++++++++++
# GAM 
#+++++++++++++++++++++++++++++++++++

sandvik_rent<- sandvik_rent %>% 
  mutate(E_status = ifelse(E_status == 1, 0, 1)) #recode för att jag tror att det visar probabilitet för success = 1 = levande. Men vi vill visa sannolikhet för success = 0 mortalitet. 

gam_model1<- gam(E_status ~ s(dbh)+ humus_snitt_skillnad + flamma_max + levande_gran_log+ levande_omgiv_log +dod_omgiv_log +levande_gran, family = binomial, method= "REML", data = sandvik_rent) 
summary(gam_model1)
plot(gam_model1, all.terms = TRUE, pages = 1) #visar alla variabler #visar smoothes #tydligt att endast DBH har en icke-linjär relation till E_status 
abline(h = 0, lty = 'dashed')

## plot prep 
p_obj <- plot(gam_model1, residuals = TRUE)
p_obj <- p_obj[[1]] # just one smooth so select the first component
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
data_df <- as.data.frame(p_obj[c("raw", "p.resid")])

## plot ## KOM IHÅG ATT JAG RECODADE 
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL),
           sides = "b") +
  geom_point(data = data_df, mapping = aes(x = raw, y = p.resid)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_line() +
  labs(x = p_obj$xlab, y = p_obj$ylab)+
  theme_classic()

# lägg till abline(h = 0, lty = 'dashed') ovan? 

#++++++++++++++++++ färbättra modellen+++++++++++++++++++

set.seed(123)
gam_modell1<- gam(E_status ~ s(dbh) + humus_snitt_skillnad + flamma_max +levande_gran_log + kronhojd + E_humus_snitt, family = binomial, method= "REML", data = sandvik_rent) 
summary(gam_modell1)  

gam_modell2 <- gam(E_status ~ s(dbh) + humus_snitt_skillnad + flamma_max +levande_gran_log + kronhojd, family = binomial, method= "REML", data = sandvik_rent)
summary(gam_modell2)  

gam_modell3<- gam(E_status ~ s(dbh) + humus_snitt_skillnad + flamma_max, family = binomial, method= "REML", data = sandvik_rent) 
summary(gam_modell3)

gam_modell3_K3<-gam(E_status ~   s(dbh, k=3) + humus_snitt_skillnad + flamma_max, family = binomial, method= "REML", data = sandvik_rent) 
summary(gam_modell3_K3) 

gam_modell3_K5<-gam(E_status ~   s(dbh, k=3) + s(humus_snitt_skillnad) + flamma_max, family = binomial, method= "REML", data = sandvik_rent) 
summary(gam_modell3_K5) #bättre 

gam_modell3_te<-gam(E_status ~ te(humus_snitt_skillnad,dbh)+ flamma_max, family = binomial, method= "REML", data = sandvik_rent)  # te är two way interaction
summary(gam_modell3_te) #intressant? 

# Jämför AIC och BIC värden för två modeller (lägre är bättre)
AIC(gam_modell1,gam_modell2) # 
AIC(gam_modell2,gam_modell3) # 
AIC(gam_modell3,gam_modell3_K3) # 
AIC(gam_modell3_K3,gam_modell3_K5) # MODELL3_K3 bäst AIC värde (straffar komplexitet)
AIC(gam_modell3_K3,gam_modell3_te) #model 3_k3 fortfarande bättre 

BIC(gam_modell1,gam_modell2) # 
BIC(gam_modell2,gam_modell3) # 
BIC(gam_modell3,gam_modell3_K3) # 
BIC(gam_modell3_K3,gam_modell3_K5) # MODELL3_K3 bäst BIC värde (straffar komplexitet)
BIC(gam_modell3_K3,gam_modell3_te) #model 3_k3 fortfarande bättre 

set.seed(123)
plot(gam_modell3_K3, resid = T, pch = 1, all.terms = TRUE, page = 1)
abline(h = 0, lty = 'dashed')

library(mgcViz)

#+++++++++++++++visualisering.... Behövs inte längre 
b <- getViz(gam_modell3_K3)
print(plot(b, allTerms = T), pages = 1) 

b <- getViz(gam_modell3_te)
print(plot(b, allTerms = T), pages = 1) 

anova(gam_modell3,gam_modell2, test = "Chisq") # försök att minimera residualer sum of squares

#++++++++++++++++++++++++++++++ Fråga: Är humusförbränning eller sotningshöjd en bättre förklarande variabel? #################

#jämför två tester ANOVA analaysis of deviance table 
gam_modell_humus<-gam(E_status ~  s(humus_snitt_skillnad), family = binomial, method= "REML", data = sandvik_rent) #bara humus 
gam_modell_flamma<-gam(E_status ~  flamma_max, family = binomial, method= "REML", data = sandvik_rent) # bara sotningsöjd 
plot(gam_modell_humus, resid = T, pch = 1, all.terms = TRUE, page = 1)

BIC(gam_modell_humus,gam_modell_flamma) 
AIC(gam_modell_humus,gam_modell_flamma) 

summary(gam_modell_humus) #bättre 
summary(gam_modell_flamma)
anova(gam_modell_humus,gam_modell_flamma)

#++++++++++++++++++++++++++ skott ++++++++++++++++++++++++++ 

gam_skott1<- gam(E_stubbskott ~ humus_snitt_skillnad + flamma_max +levande_gran_log + kronhojd + E_humus_snitt + levande_omgiv_log + lovforna + friskmossa + flamma_min, family = binomial, data = sandvik_rent,method= "REML") 
summary(gam_skott1) ## 7% förklarande R-sq ## Dåligt men friskmossa intressant 

gam_skott2<- gam(E_stubbskott ~ friskmossa +lovforna + flamma_min, family = binomial, data = sandvik_rent) #tog bort blabarris, lingonris, barrforna 
summary(gam_skott2) ## 

AIC(gam_skott1,gam_skott2) # 1 har något lägre AIC änn alla andra 

anova(gam_skott1,gam_skott2, test = "Chisq") # försök att minimera residualer sum of squares.. här har modell2 fler. 

plot(gam_skott2, resid = T, pch = 1, all.terms = TRUE, page = 1)

# +++++++++++++++++++++++++++++++++++++++++ Anteckningar +++++++++++++++++++

# Bonferroni adjustment of the p-value to assess significance. new p-value is 0.05/2 = 0.025 (new value actually should be 0.05/4 =0.0125 bc four regression test 






