#Import dataset
data <- read.csv2("Desktop/SEB_uzduotis/bank (1)/bank-full.csv")
data
#1)DATA MANIPULATION TASK

#Random subsample of data set
#Random sample - atsitiktinai parenkami atributai (stulpeliai) duomenyse
sample(data, size = 10)
#Random subset - atsitiktinai parenkamos eilutės visame duomenų rinkinyje (visi atributai paliekami)
#atsitiktinai parinktos 100 eilučių
random <- data[sample(nrow(data), 100), ]
random
#atsitiktinai parinktos 250 eilučių
random2 <- data[sample(nrow(data), 250), ]
random2
#Trumpesnis kodas atsitiktinai parinkti duomenis (atsitiktinai parenkamos 300 eilučių)
library(dplyr)
sample_n(data,300)
#Vienodas atsitikinis duomenų rinkinys kiekvieną kartą
set.seed(123)
sample_n(data,10)

#filter desired rows using simple and more complex conditions
#Sąlyga, jog individas būtų vedęs
df1 <- subset(data, marital == "married")   
df1
#Sąlyga, jog individas būtų pensijoje, tačiau norimas rezultatas individo amžius ir darbo statusas 
df2 <- subset(data, job == "retired", select = c(age, job))
df2
#Parenkami amžiaus ir išsilavinimo stulpeliai ir individai, kurių amžius mažiau nei 25-eri
data %>% select(age, education) %>% filter(age < 25)
#išrenkami individai, su kuriais kontaktuota nuo 20 iki 25 sekundžių
data %>% select(duration) %>% filter(between(duration, 20, 25))
#Išfiltruojamos visos eilutės ir parenkamos tos, kuriose individas užima techniko arba vadybininko poziciją
data %>% select(everything()) %>% filter(job %in% c("management", "technician")) 
#Individai, kurie turi asmeninę paskolą ir buvo kontaktuoti gegužę arba birželį
filter(data, (month == "may" | month == "jun") & loan == "yes")

#drop unnecessary variables, rename some variables;
#Panaikinamas "previous" stulpelis - kiti atributai pirminėje apžvalgoje atrodo svarbesni
data2 <- select(data, -previous)
data2
#Kintamųjų pervadinimas 
#Pakeičiamas stulpelio pavadinimas esamam kintamajam
data2 <- data2 %>% rename("deposit" = "y")
#Pakeičiami visi pavadinimai stulpelių (šiuo atveju į didžiąsias raides)
data2 <- data2 %>% rename_with(toupper)
data2

#calculate summarizing statistics (for full sample and by categorical variables as well)
#Naudodamiesi summary funkcija galima sužinoti kiekvieno kintamojo minimalią/maksimalią reikšmę, kvantilius, mediana, vidurkį. 
#Skaičiuoju vidurkį metinio balanso kiekvienai darbo grupei
summary(data)
#Skaičiuoju vidurkį metinio balanso kiekvienai darbo grupei ir amžiaus vidurkį
data %>% group_by(job) %>% summarise(
    count = n(),
    mean_balance = mean(balance), 
    mean_age = mean(age))
#Proporcija - procentinė dalis individų pagal darbo sritį
data_job <- data %>% group_by(job) %>% summarise(count = n())
proporcija <- prop.table(data_job$count)
data_job %>% mutate(proporcija = proporcija*100)
#Kryžminio klasifikavimo lentelė, kurioje pateikiamas individų skaičius pagal vedybinį statusą ir išsilavinimą
table(data$marital, data$education)

#create new variables using simple transformation and custom functions
#Naujas kintamasis - individo ID
data$ID <- seq.int(nrow(data))
#Duration kintamasis pakeičiamas iš sekundžių į minutes
data <- mutate(data, duration2 = duration/60)
#sukuriamas naujas kintamasis, individas priskiriamas vienai iš 3 grupių pagal amžių
data <- data %>% mutate(age_group = dplyr::case_when(
      age <= 25            ~ "Youth",
      age > 25 & age <= 55 ~ "Middle age",
      age > 55 ~ "The elderly"))

#order data set by several variables
#nurodomi kintamieji pagal kuriuos norima surūšiuoti duomenis
data_order <- data[with(data, order(age, job)),] 
#nurodomas tik stulpelio numeris
data_order2 <- data[order(data[,1], data[,2] ),]


#2)DATA VISUALISATION TASK
library(ggplot2)
#Grafikas Nr.1 
#Atspindi klientų pasirinkusių/nepasirinkusių indėlį skaičių skirtingose darbo grupėse
data <- data %>% rename("deposit" = "y")
legend_title <- "Deposit"
graph <-ggplot(data, aes(fill = deposit, x=job)) + 
  geom_bar(position="dodge") + ylab("Number of Clients") + xlab("Type of Job") +
  scale_fill_manual(legend_title, values=c("chartreuse3","coral1"), labels = c("Subscribed", "Not subscribed")) +
  ggtitle("Term Deposit Subscription Distribution among Types of Jobs") 
graph 

#Grafikas Nr. 2
#Pagal grafiką galime matyti nepaisant skirtingo komunikacijos būdo ilgesnis 
#kontaktavimo laikas lemia teigiamą žmonių apsisprendimą dėl indelio. Kadangi kintamųjų yra pakankamai didelis
#skaičius ir visų nematome aiškiai, tai tik pirminė įžvalga pagal grafiką. Tiksliau ryšys tarp kontaktavimo laiko ir tikimybės pasirinkti indelį
#bus įverintas naudojant tikimybių modelį. Taip pat, pagal grafiką aiškiai matome, jog kontaktas telefonu vidutiniškai trunka
#trumpiausiai. 
ggplot(data, aes(y = factor(contact, labels = c("unknown", "telephone","cellular")), 
           x = duration2, color = deposit)) +
  geom_jitter(size = 1, alpha = 0.5) + 
  labs(title = "Contact Duration Distribution", 
       subtitle = "Classified by communication type and deposit subscription",x = "Duration (in minutes)",
       y = "Contact Communication Type") +
  theme_minimal() +
  theme(legend.position = "right") + scale_color_manual(legend_title, values=c("chartreuse3","coral1"), labels = c("Yes", "No"))

#Grafikas Nr.3
#Pirmiausia, remiantis grafiku galime pastebėti, kad apkritai didesnė dalis tiriamųjų (individų) turi būsto paskolą. 
#Taip pat galima pastebėti išsilavinimo lygio pasiskirtymą tarp skirtingų darbo pozicijų, pavyzdžiui, dauguma
#vadybininko poziciją užimančių individų turi aukštąjį išsilavinimą, tuo tarpu, likusiose pozicijose
#pasiskirsto žmonės, įgyję vidurinį išssilavinimą. Didžioji dalis žmonių, turintys vidurinį išsilavinimą
#priklauso darbininkų klasei. Taip pat, pastebima, jog didžioji dalis darbininkų klasės darbuotojų kaip ir administracijos, ir paslaugų
#yra pasiėmę būsto paskolą. Priešingai, didžioji dalis namų šeimininkių, bedarbių ir pensijinio amžiaus žmonių šiuo metu neturi būsto paskolos. 
abc <- data %>% group_by(job, education, housing) %>% count()
abc$housing <- factor(abc$housing,levels = c("yes", "no"))
library(ggalluvial)
update_geom_defaults("text", list(size = 3))
graph3 <- ggplot(abc, aes(axis1 = job, axis2 = housing, y = n)) +
  geom_alluvium(aes(fill = education)) + geom_stratum() + geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("job", "housing"),expand = c(0.1, 0.1)) +
  labs(title = "House Loan Distribution", subtitle = "Classified by Type of Job, Education",y = "Frequency") +
  theme_minimal()
graph3

#3)MODELLING TASK
data$housing <- ifelse(data$housing == "yes", 1, 0)
data$deposit <- ifelse(data$deposit == "yes", 1, 0)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]

#Kuriamas logit modelis. Pasirinkti kintamieji: amžius, darbo tipas, išsilavinimas, būsto paskola, vidutinis metinis balansas, kontaktavimo laikas.
model <- glm(deposit ~ age + job + education + housing + balance + duration,
              family=binomial(link = "logit"), data = train)
summary(model)
#Amžius ir nežinomas ar nedirbančiojo asmens statusas neturi statistiškai reikšmingos įtakos
#indėlio pasirinkimui. Visi likusieji kintamieji yra statistiškai reikšmingi. 
#Keleta interpretacijų:
#Kaip ir buvo tikėtasi, kontaktavimo laikas turi teigiamą įtaką tikimybei.
#Jei individas turi būsto paskolą, tai neigiamai veikia tikimybę, jog tas pats individas pasirinks indėlį. 
#Pavyzdys: kokia  indėlio pasiimo tikimybė individui, kuris yra 35 metų, dirba paslaugų sektoriuje, turi vidurinį išsilavinimą, 
#neturi būsto paskolos, jo metinis balansas lygus 1000 eur ir kontaktavimo laikas 495 sekundės?
a <- c(1,35,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1000,495)
logitsum <- sum(model$coefficients*a)
tikimybe <- exp(logitsum)/(1+exp(logitsum))
#Tikimybė, jog nurodytas individas pasiims indėlį lygi 0.23

library(caret)
probabilities <- predict(model ,newdata=test, type="response")
predicted <- ifelse(probabilities > 0.5, 1, 0)
mean(predicted == test$deposit)
#Klasifikavimo prognozavimo tikslumas (classification prediction accuracy) yra apie 89 %. Modelis geras.
#Neteisingo klasifikavimo klaidų lygis (misclassification error rate) yra 11 %.
#goodness of fit
library(performance)
performance_hosmer(model, n_bins = 10)

