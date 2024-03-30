#             1. IMPORTING THE DATABASE 
#load the dataset using rio
library(datasets)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#CSV datase import
# rio_csv = promenliva koja go pretstavuva dataset-ot za Software Professional Salary
rio_csv <- import("D:/Seminarska_BS/Salary_Dataset_with_Extra_Features.csv")
#gi pokazuva prvite 6 reda od dataset-ot
head(rio_csv)
#celiot dataset
View(rio_csv)

########################################################
#                  I. PRV DEL A
#       1) TABLE WITH FREQUENCIES for ecah category
#           za KVANTITATIVNI PODATOCI (Rating, Salary)
#         1. RATING promenlivata

## 1.tabela na cestoti za rating (Rating)

#gi smestuvame vrednostite od Rating vo nova promeliva
rating = rio_csv$Rating

# sort vo rastecki redosled
sort(rating)

#1.Rang na vrednostite za Rating
range(rating)

#2.tocki na prekin 
#interval od najbliskite half-integers
breaks1 = seq(1.0, 5.5, by=0.5)
breaks1

#3. intervalite se zatvoreni od levo, a otvoreni od desno
rating.cut = cut(rating, breaks1, right = FALSE)

#4. presmetka na frekfencijata za rating vo sekoj podinterval 
rating.freq = table(rating.cut)

#proverka na frekfencii
rating.freq

#proverka vo kolonski prikaz
cbind(rating.freq)

## 2.tabela na relativnite cestoti za ratingot na komapnijata (RATING)
rating.relfreq = rating.freq / nrow(rio_csv)
#proveruvame rezultati 
rating.relfreq
#zaokruzeni decimali
old = options(digits = 1)
rating.relfreq
#vrati vo stara verzija 
options(old)

#priaz vo TABELA ZA FREQ i RELFREQ
old = options(digits = 1)
cbind(rating.freq, rating.relfreq)

## 3. tabela za kumulativnite cestoti na RATING
rating.cumfreq = cumsum(rating.freq)
rating.cumfreq

#spojuvame vo tabelata TABELA ZA FREQ RELFREQ i CUMFREQ
cbind(rating.freq, rating.relfreq, rating.cumfreq)

## 3. presmetka na SREDNI TOCKI NA SEKOJ OD INTERVALITE

# gi opredeluvame srednite tocki
mid = c()
for(i in 1: length(breaks1)-1)
{
  mid = c(mid, (breaks1[i] + breaks1[i+1])/2)
}

## 4. Histogram x- rating y- frequency

h1 = hist(rating, right = FALSE, breaks = breaks1, 
          col = "grey", 
          main = "Рејтинг на компаниите од 1 до 5", 
          ylab = "честоти")

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink")

h1 = hist(rating, right = FALSE, breaks = breaks1, 
          col = colors, 
          main = "Полигон на честоти на Рејтинг на компаниите од 1 до 5", 
          ylab = "честоти")

## 5. poligon na cestoti
lines(mid,rating.freq)

##




##################################################################
##                   2. SALARY 

# tabela na cestoti za platata na vrabotenite (Salary) pretstavena vo evra (€)

#pristapuvam do podatocite za salary
salary = rio_csv$Salary

#Za kvantitativnata promelivata Salary ja klasificiram spored promelivata Status Employment
#odnosno za sekoj tip na Statust Employment ja razgleuvam frekfencijata za posoodvetni rezultati


#                               ////  pravam Pita grafik za frekfencijata na status employment /////

employment_status = rio_csv$`Employment Status`
employment_status_freq = table(employment_status)

#frekfencija za sekoj tip na vraboteni so employmnet status
employment_status_freq

#formiram data frame
employment_status_c <- c("Contractor", "FullTime", "Intern", "Trainee")
count_freq <- c(548, 20083, 2106, 33)
data_frame <- data.frame(employment_status_c, count_freq)

#Pie chart 
colors = c("lightblue", "red", "green", "purple") 

pie_data <- data_frame$count_freq
percentage_labels <- paste(employment_status_c, " ", round((pie_data / sum(pie_data)) * 100, 1), "%", sep = "")

pie(pie_data, labels = percentage_labels, main = "Фрекфенција на застапеност на видот на вработени", col = colors)

#                                  /////////////////////////////////////////////////////////////////


#             1. Frekfencija na plata na sekoj tip na vraboten : 
# Contractor, Full Time, Intern, Trainee

#prikaz na graficite po koloni i redici
par(mfrow = c(1, 2))

#                  * 1 Contractor employees *

salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]

salary.contractor

#sortirana niza od salary contractor
sort(salary.contractor)

#range
range(salary.contractor)

#golemina na podatocite za salary contractor
length(salary.contractor)

#1)
#tocki na prekin
breaks.contractor <- seq(132, 80332, by = 8020)
contarctor.cuts <- table(cut(salary.contractor, breaks = breaks.contractor, right = FALSE))

table_data <- data.frame(
  Interval = sprintf("[%d, %d)", breaks.contractor[-length(breaks.contractor)], breaks.contractor[-1]),
  Frequency = contarctor.cuts
)

print(table_data)


hist(salary.contractor,
     main = "Хистограм на платите на Contract тип на вработени во Софтверска компанија",
     breaks = 10,
     xlab = "Плата на Contractor тип на вработени во евра (€) во Софтверска компанија",
     ylab = "Фрекфенција",
     col = "lightblue")

# gi opredeluvame srednite tocki
mid = c()
for(i in 1: length(breaks.contractor)-1)
{
  mid = c(mid, (breaks.contractor[i] + breaks.contractor[i+1])/2)
}
#proverka na srednite tochki
mid

#poligon na cestoti
hist(salary.contractor,
     main = "Полигон на честоти на платите на Contract тип на вработени во Софтверска компанија",
     breaks = 10,
     xlab = "Плата на Contractor тип на вработени во евра (€) во Софтверска компанија",
     ylab = "Фрекфенција",
     col = "lightblue")

#gi povrzuvam srednite tochki
lines(mid,contarctor.cuts)


#                  * 2 FullTime employees *

full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]

#sortirana niza od salary contractor
sort(full_time_salaries)

#range
range(full_time_salaries)

#golemina na podatocite za salary contractor
length(full_time_salaries)

#1)
#tocki na prekin
breaks.fulltime <- seq(90, 120000, by = 11991)
full_time_salaries_cuts <- table(cut(full_time_salaries, breaks = breaks.fulltime , right = FALSE))

table_data <- data.frame(
  Interval = sprintf("[%d, %d)", breaks.fulltime[-length(breaks.fulltime)], breaks.fulltime[-1]),
  Frequency = full_time_salaries_cuts
)

print(table_data)

hist(rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"],
  main = "Хистограм за плата на вработените од тип FullTime во (€)",
  breaks = 10,
  xlab = "Плата на вработените од тип FullTime во (€)",
  ylab = "Фрекфенција",
  col = "red")

# gi opredeluvame srednite tocki
mid = c()
for(i in 1: length(breaks.fulltime)-1)
{
  mid = c(mid, (breaks.fulltime[i] + breaks.fulltime[i+1])/2)
}

mid

#poligon na cestoti
hist(rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"],
     main = "Полигон на честоти за плата на вработените од тип FullTime во (€)",
     breaks = 10,
     xlab = "Плата на вработените од тип FullTime во (€)",
     ylab = "Фрекфенција",
     col = "red")
#poligon na cestoti
lines(mid,full_time_salaries_cuts)

#                  * 3 Intern employees *

#histogram za Intern Employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
range(intern_salaries)
length(intern_salaries)

sort(intern_salaries)

breaks.intern <- seq(20, 77020, by = 7000)
intern_salaries_cuts <- table(cut(intern_salaries, breaks = breaks.intern , right = FALSE))

table_data <- data.frame(
  Interval = sprintf("[%d, %d)", breaks.intern[-length(breaks.intern)], breaks.intern[-1]),
  Frequency = intern_salaries_cuts)

#tabela na cestoti za intern salary
print(table_data)


hist(rio_csv$Salary [rio_csv$`Employment Status`== "Intern"],
     main = "Хистограм за плата на Intern тип на работници во Софтверска компанија",
     xlab = "Плата на Intern тип на работници во Евра (€) во Софтверска компанија",
     ylab = "Фрекфенција",
     col = "green")

# gi opredeluvame srednite tocki
mid = c()
for(i in 1: length(breaks.intern)-1)
{
  mid = c(mid, (breaks.intern[i] + breaks.intern[i+1])/2)
}

mid

## poligon na cestoti
hist(rio_csv$Salary [rio_csv$`Employment Status`== "Intern"],
     main = "Полигон на честоти за плата на Intern работници во Софтверска компанија",
     xlab = "Плата на Intern работници во Евра (€) во Софтверска компанија",
     ylab = "Фрекфенција",
     col = "green")
#poligon na cestoti
lines(mid,intern_salaries_cuts)


#                  * 4 Trainee employees *

#histogram za Trainee Employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
length(trainee_salaries)
range(trainee_salaries)
sort(trainee_salaries)

breaks.trainee <- seq(1056, 13056, by = 1000)
trainee_salaries_cuts <- table(cut(trainee_salaries, breaks = breaks.trainee , right = FALSE))

table_data <- data.frame(
  Interval = sprintf("[%d, %d)", breaks.trainee[-length(breaks.trainee)], breaks.trainee[-1]),
  Frequency = trainee_salaries_cuts
)

print(table_data)

#gi setiram marginite na graficite
par(mar = c(5, 4, 4, 2)) 

# histogram za trainee employee salaries
hist(trainee_salaries, 
     breaks = breaks.trainee, 
     main = "Хистограм на платите на вработените кои се Trainee во Софтверска компанија", 
     xlab = "Плата на Тrainee тип на работници во Евра (€) во Софтверска комапнија", 
     ylab = "Фрекфенција",
     col = "purple")

#poligon na cestoti
# gi opredeluvame srednite tocki
mid = c()
for(i in 1: length(breaks.trainee)-1)
{
  mid = c(mid, (breaks.trainee[i] + breaks.trainee[i+1])/2)
}

mid

hist(trainee_salaries, right = FALSE,
     breaks = breaks.trainee, 
     main = "Полигон на платите на вработените кои се Trainee во Софтверска компанија", 
     xlab = "Плата на Тrainee тип на работници во Евра (€) во Софтверска комапнија", 
     ylab = "Фрекфенција",
     col = "purple")

## 5. poligon na cestoti
lines(mid,trainee_salaries_cuts)



######################
#       2) STEM-AND-LEAF plot za vrabotenite koi se Trainee i Contractor bidejki se vo pomal broj i se polesni za prikaz

# 1. STEM-AND-LEAF za TRAINEE

# za TRAINEE- prikaz na steblo list dijagram za platite na vrabotenite koi se trainee
#gi sortiram platite 
trainee_salaries_sorted = sort(trainee_salaries)
trainee_salaries_sorted

# gi dvojam iljadarkite(stems) i listovite(leaves)
stems <- trainee_salaries_sorted %/% 1000  # Extract thousands
leaves <- trainee_salaries_sorted %% 1000  # Extract remainder

#stem-and-leaf plot
stem_data <- data.frame(stems, leaves)
unique_stems <- unique(stem_data$stems)

for (stem in unique_stems) {
  cat(stem, " | ", paste(stem_data$leaves[stem_data$stems == stem], collapse = " "), "\n")
}

# 2. STEM-AND-LEAF za CONTRACTORS

# za CONTRACTOR- prikaz na steblo list dijagram za platite na vrabotenite koi se contractors
#gi sortiram platite 
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
contractor_salaries_sorted = sort(salary.contractor)
contractor_salaries_sorted

# gi dvojam iljadarkite(stems) i listovite(leaves)
stems <- contractor_salaries_sorted %/% 1000  # Extract thousands
leaves <- contractor_salaries_sorted %% 1000  # Extract remainder

#stem-and-leaf plot
stem_data <- data.frame(stems, leaves)
unique_stems <- unique(stem_data$stems)

for (stem in unique_stems) {
  cat(stem, " | ", paste(stem_data$leaves[stem_data$stems == stem], collapse = " "), "\n")
}

######################
#                     3) GRAFIK NA RASEJUVANJE
# za odnosot pomegju ratingot na Softverskata kompanija i platite na razlicnite tipovi 
#rabotnici

par(mfrow = c(1, 1))
# ##                 ** 1. Rating i Contactor employee salary

salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]

#rejting samo za vrabotenite Contractor
rating.contractor = rio_csv$Rating [rio_csv$`Employment Status`== "Contractor"]
rating.contractor

#prikaz vo tabela
cbind(salary.contractor, rating.contractor)

#scatter plot
plot(salary.contractor, rating.contractor,
     xlab = "Плата на вработениот од тип Contractor во евра",
     ylab = "Рејтинг на Софтверската компанија",
     main = "График на расејување на Рејтинг на Софтверската компанија  и Плата на вработениот од тип Contractor во евра",
     col = "blue")

#linija na trend
abline(lm(rating.contractor ~ salary.contractor))

## ##                     ** 2. Rating i FullTime employee salary

full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]

rating.fulltime = rio_csv$Rating [rio_csv$`Employment Status` == "FullTime"]
rating.fulltime

#prikaz vo tabela
cbind(full_time_salaries, rating.fulltime)

#scatter plot
plot(full_time_salaries, rating.fulltime,
     xlab = "Плата на вработениот од тип FullTime во евра",
     ylab = "Рејтинг на Софтверската компанија",
     main = "График на расејување на Рејтинг на Софтверската компанија  и Плата на вработениот од тип FullTime во евра",
     col = "red")

#linija na trend
abline(lm(rating.fulltime ~ full_time_salaries))

#### ##                    ** 3. Rating i Intern employee salary

intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
rating.intern = rio_csv$Rating [rio_csv$`Employment Status`== "Intern"]

rating.intern
cbind(intern_salaries, rating.intern)

#scatter plot
plot(intern_salaries, rating.intern,
     xlab = "Плата на вработениот од тип Intern во евра",
     ylab = "Рејтинг на Софтверската компанија",
     main = "График на расејување на Рејтинг на Софтверската компанија  и Плата на вработениот од тип Intern во евра",
     col = "green")

#linija na trend
abline(lm(rating.intern ~ intern_salaries))

#### ##                    ** 4. Rating i Trainee employee salary

trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
rating.trainee = rio_csv$Rating [rio_csv$`Employment Status`== "Trainee"]

cbind(trainee_salaries, rating.trainee)

#scatter plot
plot(trainee_salaries, rating.trainee,
     ylab = "Рејтинг на Софтверската компанија во која работи вработениот од тип Trainee",
     xlab = "Плата на вработениот од тип Trainee во евра",
     main = "График на расејување на Рејтинг на Софтверската компанија  и Плата на вработениот од тип Trainee во евра",
     col = "purple")

abline(lm(rating.trainee ~ trainee_salaries))

######################
#                     4) MODA, MEDIJANA i PROSEK

##                          * PROSEK *
#PROSEK ZA RATING
mean(rating)
#PROSEK za plata na  COUNTRACTOR employees
mean(salary.contractor)
#PROSEK za plata na FullTime employees
mean(full_time_salaries)
#PROSEK za plata na Intern employees
mean(intern_salaries)
#PROSEK za plata na Trainee employees
mean(trainee_salaries)

##                          * MEDIJANA *
#MEDIJANA ZA RATING
median(rating)
#MEDIJANA za plata na  COUNTRACTOR employees
median(salary.contractor)
#MEDIJANA za plata na FullTime employees
median(full_time_salaries)
#MEDIJANA za plata na Intern employees
median(intern_salaries)
#MEDIJANA za plata na Trainee employees
median(trainee_salaries)

##                          * MODA *
#   1 - MODA za RATING
#frekfencija na sekoj rating posebno NE VO INTERVALI!
rating.freq2 = table(rating)
rating.freq2
cbind(rating.freq2)

#ja barame najzastapenata rating ocena
most_frequent_rating <- as.numeric(names(rating.freq2)[which.max(rating.freq2)])
print(most_frequent_rating)

#   2 - MODA za plata na CONTRACTOR employees
#frekfencija za sekoja plata
contractor_salary_freq = table(salary.contractor)
cbind(contractor_salary_freq)

#najzastapenata plata za Contractor
most_frequent_contractor_salary <- as.numeric(names(contractor_salary_freq)[which.max(contractor_salary_freq)])
print(most_frequent_contractor_salary)

#   3 - MODA za plata na FullTime employees
#frekfencija za plata na FulTime employees
fulltime_salaries_freq = table(full_time_salaries)
cbind(fulltime_salaries_freq)

##najzastapenata plata za FullTime
most_frequent_fulltime_salary <- as.numeric(names(fulltime_salaries_freq)[which.max(fulltime_salaries_freq)])
print(most_frequent_fulltime_salary)

#   4 - MODA za plata na Intern employees
intern_salaries_freq = table(intern_salaries)
cbind(intern_salaries_freq)

#najzastapena plata za Intern employee
most_frequent_intern_salary <- as.numeric(names(intern_salaries_freq)[which.max(intern_salaries_freq)]) 
print(most_frequent_intern_salary)

#   5 - MODA za plata na Trainee employees
trainee_salaries_freq = table(trainee_salaries)
cbind(trainee_salaries_freq)

#najzastapenata plata za Trainee employees
most_frequent_trainee_salary <- as.numeric(names(trainee_salaries_freq)[which.max(trainee_salaries_freq)])
print(most_frequent_trainee_salary)

######################
#                     5) KVARTILI, OPSEG i INTERKVARTILEN RASPON

##                          * KVARTILI *

#   1 - za Rating
rating = rio_csv$Rating
quantile(rating)

#   2 - za plata na CONTRACTOR employees
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
quantile(salary.contractor)

#   3 - za plata na FullTime employees
# display numbers without scientific notation
options(scipen = 999)
full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]
quantile(full_time_salaries)

#   4 - za plata na Intern employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
quantile(intern_salaries)

#   5 - za plata na Trainee employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
quantile(trainee_salaries)

###
##                          * OPSEG *
#   1 - za Rating
rating = rio_csv$Rating
max(rating) - min(rating)

#   2 - za plata na CONTRACTOR employees
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
max(salary.contractor) - min(salary.contractor)

#   3 - za plata na FullTime employees
# display numbers without scientific notation
options(scipen = 999)
full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]
max(full_time_salaries) - min(full_time_salaries)

#   4 - za plata na Intern employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
max(intern_salaries) - min(intern_salaries)

#   5 - za plata na Trainee employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
max(trainee_salaries) - min(trainee_salaries)

###
##                  * INTERKVARTILEN RASPON *

#   1 - za Rating
rating = rio_csv$Rating
IQR(rating)

#   2 - za plata na CONTRACTOR employees
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
IQR(salary.contractor)

#   3 - za plata na FullTime employees
# display numbers without scientific notation
options(scipen = 999)
full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]
IQR(full_time_salaries)

#   4 - za plata na Intern employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
IQR(intern_salaries)

#   5 - za plata na Trainee employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
IQR(trainee_salaries)

######################
#             6) DISPERZIJA(var()) i STANDARDNA DEVIJACIJA(sd())

#   1 - za Rating
rating = rio_csv$Rating
var(rating)
sd(rating)

#   2 - za plata na CONTRACTOR employees
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
var(salary.contractor)
sd(salary.contractor)

#   3 - za plata na FullTime employees
# display numbers without scientific notation
options(scipen = 999)
full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]
var(full_time_salaries)
sd(full_time_salaries)

#   4 - za plata na Intern employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
var(intern_salaries)
sd(intern_salaries)

#   5 - za plata na Trainee employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
var(trainee_salaries)
sd(trainee_salaries)

######################
#             7) KOEFICIENT NA KORELACIJA ///TODO

#Za rating i Employment Type 
#   1 - za plata na CONTRACTOR employees
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]

#rejting samo za vrabotenite Contractor
rating.contractor = rio_csv$Rating [rio_csv$`Employment Status`== "Contractor"]
rating.contractor

#prikaz vo tabela
cbind(salary.contractor, rating.contractor)

#koeficient na korelacija
cor(salary.contractor, rating.contractor)


#   2 - za plata na FullTime employees
full_time_salaries <- rio_csv$Salary[rio_csv$`Employment Status` == "FullTime"]
rating.fulltime = rio_csv$Rating [rio_csv$`Employment Status` == "FullTime"]
rating.fulltime
#prikaz vo tabela
cbind(full_time_salaries, rating.fulltime)
#koef na korel
cor(full_time_salaries, rating.fulltime)

#   3 - za plata na Intern employees
intern_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Intern"]
rating.intern = rio_csv$Rating [rio_csv$`Employment Status`== "Intern"]
rating.intern
cbind(intern_salaries, rating.intern)
#koef na korel
cor(intern_salaries, rating.intern)


#   4 - za plata na Trainee employees
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
rating.trainee = rio_csv$Rating [rio_csv$`Employment Status`== "Trainee"]

cbind(trainee_salaries, rating.trainee)

#koef na korel
cor(trainee_salaries, rating.trainee)


###################################################################
#                  II. VTOR DEL B

#           1. INTERVAL NA DOVERBA za CONTRACTOR employee salary

########         ** za CONTRACTOR employee salary ** ########

salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
salary.contractor

#sortirana niza od salary contractor
sort(salary.contractor)

#range
range(salary.contractor)

#golemina na primerokot n>=30 
length(salary.contractor)

# 1) za CONTRACTOR employee salary prosek
mean(salary.contractor) 

# 2) Interval na doverba za: 
#   - ne pozata disperzija na populacijata
#   - golem obem na primerok n>=30 
# To tezi kon normalna raspredelba i rabotime so Z
#  X(crta) +- zzα/2/2 * (S/sqrt(n))

#    2.1 marginata na greshka i procenka na intervalot so nivo na doverba od 99%
n = length(salary.contractor)

# presmetka na S (standardna devijacija na primerokot)
S = sd(salary.contractor)
print(S)

#standardna procenka na greshka
SE = S/sqrt(n); SE

#margina na greshka (greshka na primerokot)
alpha <- 0.01  # 1 - α = 0.99, pa α = 0.05
alpha_2 = alpha / 2
#so 3 decimiali
print(alpha_2) # 0.005

z_alpha_2 <- qnorm(1 - alpha_2) #qnorm(0.975) = 2.58
print(z_alpha_2) # dobivam 2.58
E = qnorm(1 - alpha_2) * SE; E #dobivam 772

#sega go dodavam na X(crta) (prosekot) i go baram intervalot na doverba 

# prosek na primerokot
xbar = mean(salary.contractor)

#intervalot na doverba 
xbar + c(-E, E)


########         ** za Trainee employee salary ** ########

trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
sort(trainee_salaries)

range(trainee_salaries)

#golemina na primerokot n>=30 
length(trainee_salaries)

# 1) za CONTRACTOR employee salary prosek
mean(trainee_salaries) 

# 2) Interval na doverba za: 
#   - ne pozata disperzija na populacijata
#   - golem obem na primerok n>=30 
#  X(crta) +- zα/2 * (S/sqrt(n))

#    2.1 marginata na greshka i procenka na intervalot so nivo na doverba od 95%
n = length(trainee_salaries)
print(n)

# presmetka na S (standardna devijacija na primerokot)
S = sd(trainee_salaries)
print(S)

#margina na greshka (greshka na primerokot)
SE = S/sqrt(n); SE

#margina na greshka (greshka na primerokot)
alpha <- 0.05  # 1 - α = 0.95, pa α = 0.05
alpha_2 = alpha / 2
#so 3 decimiali
print(alpha_2) # 0.025

z_alpha_2 <- qnorm(1 - alpha_2) #qnorm(0.975) = 1.96
print(z_alpha_2) # dobivam 1.96
E = qnorm(0.975) * SE; E #dobivam 587.4

#sega go dodavam na X(crta) (prosekot) i go baram intervalot na doverba 
# prosek na primerokot
xbar = mean(trainee_salaries)
#intervalot na doverba 
xbar + c(-E, E)

#---------------------------------------------------------------------#
#           2. Testiranje hipotezi

#
########         ** za CONTRACTOR employee salary ** ########

salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]
salary.contractor

#sortirana niza od salary contractor
sort(salary.contractor)

#range
range(salary.contractor)

#golemina na primerokot n>=30 
length(salary.contractor) # n = 548

#---------------------------------------------------------------------#
###** Od slucnajno izbrani 548 vraboteni so status Contractor vo Softwerska kompanija se dobieni: 
###*prosecna plata od mi=6901 evra i strandardna devijacija S=7016.
###* Go testirame tvrdenjeto dali prosecnata plata vrabotenite so status Contractor e 12 000 evra godishno.
###*So nivo na zacajnost alpha=0.01 !
###*

#gi inicijaliziram vrednostite
x_crta_contractor_salary = mean(salary.contractor)
x_crta_contractor_salary

S_contractor_salary = sd(salary.contractor)
S_contractor_salary

n_salary_contractor = length(salary.contractor)
n_salary_contractor

alpha_contractor_salary <- 0.01
alpha_contractor_salary

mi0 <- 12000

# 1) obelezhjeto X - procechna plata na Comtractor employee
# 2) postavuvanje na hipotezite:
# H0: mi = mi0
# Ha: mi  ≠ mi0

# H0: mi0 = 12 000
# Ha: mi0 ≠ 12 000 (ne e ednakvo)

# 3) избор на Test statistikata
# n >= 30 (golem primerok)
# S = 7016
# x_crta(prosek na primerokot) = 6901

# Z0 = ((x_crta - mi0) / S) * sqrt(548) = -0.727 * 23.409 = -17.018
options(digits = 3) # zaokruzuva na 3 decimali
dropka = ((x_crta_contractor_salary - mi0) / S_contractor_salary) ; dropka
koren = sqrt(n_salary_contractor); koren

Z0 = dropka * koren; Z0  # Z0 = -17.018

# 4) Od tablicata gledame 
# z_alfa_polovina = z_0.01/2 = z_0.005 
z_alfa_polovina = alpha_contractor_salary / 2; z_alfa_polovina #0.005
# Ф(z_0.005) = 1- 0.005 = 0.995
fi_z_alfa_polovina =  1 - 0.005; fi_z_alfa_polovina

#vrednost na Z_alfa_polovina ja barame
vrednost_z_alfa_polovina = qnorm(fi_z_alfa_polovina); vrednost_z_alfa_polovina
# Ф(0.995) = 2.58
# z_alfa_polovina = 2.58

#5) kriticen domen, oblast na otfrlanje na nultata hipotza
# bidejki nasiot test e dvostran ova e oblikot na kriticniot domen
# C = (-∞, -z_alfa_polovina) U (z_alfa_polovina, +∞)
# vo nasiot slucaj: 
# C = (-∞, -2.58) U (2.58, +∞)

#6) Zaklucok:
# Vrednost na test statistikata Z0 = -17.018 pripagja vo kriticniot domen (Z0 ∈ C)
# Sto znaci deka nulatat hipoteza H0 se OTFRLA, odnosno taa e netocna !
# Pretpostavkata deka prosecnata plata e 12 000 evra godishno za Contractor employees e pogreshna!!


#---------------------------------------------------------------------#

#           3. Test za Raspredelba dali podatocite imaat normalna raspredelba 

# 1) podatoci za koi se pretpostavuva deka imaat normalna distribucija.
salary.contractor = rio_csv$Salary [rio_csv$`Employment Status`== "Contractor"]

#sortirana niza od salary contractor
sort(salary.contractor)

# 2) Od podatocite ja procenuvame srednata vrednost na primerokot (m) i varijansata na primerokot (s^2)
mu_hat <- mean(salary.contractor); mu_hat
sigma2_hat <- var(salary.contractor); sigma2_hat

# 3) Izvrshuvame test za норм расп koristejḱi ja statistikata na hi-kvadrat sporeduvajki ja nabljuduvanata frekventna distribucija sochočekuvanata distribucija na frekvencija pod pretpostavka za normalna distribucija.
options(scipen = 999)
observed_freq <- hist(salary.contractor, plot = FALSE)$counts
expected_freq <- dnorm(seq(min(salary.contractor), max(salary.contractor), length.out = length(observed_freq)), mean = mu_hat, sd = sqrt(sigma2_hat)) * length(salary.contractor); expected_freq
chi_squared_stat <- sum((observed_freq - expected_freq)^2 / expected_freq); chi_squared_stat

# 4) Gi presmetuvame stepenite na sloboda (df) za hi-kvadrat test
df <- length(observed_freq) - 2 - 1; df

# presmetuvame p-vrednost
p_value <- 1 - pchisq(chi_squared_stat, df); p_value

# printanje na rezultatite
cat("Chi-Squared Statistic:", chi_squared_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# sporeduvame so izbranoto nivo na značajnost (alfa) za da odredime dali da ja otfrlime nultata hipoteza
alpha <- 0.01 
if (p_value < alpha) {
  cat("Odbivanje na nultata hipoteza: Podatocite ne sledat normalna distribucija.\n")
} else {
  cat("Prifakjanje na nultata hipoteza: Podatocite sledat normalna distribucija.\n")
}

#---------------------------------------------------------------------#

#           4. Test za Hipotezi na nezavisnost za Contractor salary


data <- data.frame(rio_csv$`Employment Status`, rio_csv$`Job Roles`)

#tabela na kontignecija
contingency_table <- table(rio_csv$`Employment Status`, rio_csv$`Job Roles`)
contingency_table

# Test za nezavisnost chi kvadraten test
chi_squared_test_result <- chisq.test(contingency_table)

alpha <- 0.01

# pecatenje na rezultatot
print(chi_squared_test_result)

#odluka dali da ja prifatime ili otfrlime nultata hipoteza
if (chi_squared_test_result$p.value < alpha) {
  cat("Otfrlanje na nultata hipoteza: Postoi znacajna povrzanost pomegju Employment Status i Job Role promenlivite, obelezhjata.\n")
} else {
  cat("Prifakjanje na nultata hipoteza: Nema znacajna povrzanost pomegju Employment Status i Job Role promenlivite, obelezhjata.\n")
}


#...................................................................#

#           5. Regresiona Analiza


#proverka na podatoci za rating na Trainee
trainee_salaries = rio_csv$Salary [rio_csv$`Employment Status`== "Trainee"]
rating.trainee = rio_csv$Rating [rio_csv$`Employment Status`== "Trainee"]

cbind(trainee_salaries, rating.trainee)

#scatter plot
plot(trainee_salaries, rating.trainee,
     ylab = "Рејтинг на Софтверската компанија во која работи вработениот од тип Trainee",
     xlab = "Плата на вработениот од тип Trainee во евра",
     main = "График на расејување на Рејтинг на Софтверската компанија  и Плата на вработениот од тип Trainee во евра",
     col = "purple")

abline(lm(rating.trainee ~ trainee_salaries))

#koef na korel
cor(trainee_salaries, rating.trainee)
#negativna asocijacija, slaba linearna zavisnost imaat


#proseci
mean_X <- mean(trainee_salaries); mean_X
mean_Y <- mean(rating.trainee); mean_Y

# SSx and SSy
options(scipen = 999)
SSx <- sum((trainee_salaries - mean_X)^2); SSx #225069797
SSy <- sum((rating.trainee - mean_Y)^2); SSy #13.1
SSxy <- sum((trainee_salaries - mean_X) * (rating.trainee - mean_Y)); SSxy

# Print the results
cat("SSx:", SSx, "\n")
cat("SSy:", SSy, "\n")
cat("SSxy:", SSxy, "\n")

#prava na regresija vo oblik y = beta_nula + beta_1 * x
beta_1 = SSxy / SSx; beta_1 #-0.0000203
beta_0 = mean_Y - (SSxy / SSx) * mean_X; beta_0 #3.93

#pravata na regresija
# y = 3.93 -0.0000203x

#interpretacija na rezultatite
#SST
SST <- sum((rating.trainee - mean_Y)^2); SST #13.1
cat("SST:", SST, "\n")
#SSE
model <- lm(rating.trainee ~ trainee_salaries)
SSE <- sum(residuals(model)^2)
cat("SSE:", SSE, "\n")
#SSR
SSR <- sum((predict(model) - mean_Y)^2)
cat("SSR:", SSR, "\n")

#koeficient na determiniranost
R_2 = SSR / SST; R_2 #0.00708
#slaba e jachinata na sovpagjanj na pravata na regresija so podatocite.

##CLEAR 

#clear enviroment
rm(list = ls())
#clear console 
cat("\014")

