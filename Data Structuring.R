#---- LVM Case Study ------
# --- Group 7 --------------
# Magnus Acker      (394456)
# Daniel Carriola   (425699) 
# Gino S. Coletti P (425904)
# Ondej Zaruba     (434084)



#Start structuring the data

#Load the data into the working environment
load("lvm.RData")

#Exploring data and looking at its initial structure, no NA's identified

MyLVM = lvm
str(lvm)
summary(lvm)

#----Variables identified in the dataset----

# Categorical Variables:
#1. beruf_stgl_schl : Employemnt Status
#2. fam_stand_schl  : Family Status
#3. anrede_schl     : Gender
#4. kund_typ_schl   : Customer Type
#5. kgs5            : Location ID
#6. kd_id           : Customer ID
#7. portal_flg      : Portal Flag

# Metric Variables:
#1. alter           : Age

#- No. of Contracts per Type of Insurance
#2. pg_k_anz        : Vehicle
#3. pg_sach_anz     : Property
#4. pg_ah_anz       : Indemnity
#5. pg_au_anz       : Accident
#6. pg_rs_anz       : Legal
#7. pg_kv_anz       : Health
#8. pg_lv_anz       : Life
#9. pg_fdl_anz      : Financial Services

#10. gesamtbeitrag  : Total Premium
#11. vetrag_anz     : Total No. of Contracts

#Give names to the columns to facilitate data reading
colnames(MyLVM) <- c("Employment.Status","Family.Status", "Gender","Customer.Type", 
                   "Age", "Cust.Location", "Vehicle", "Property", "Indemnity",
                   "Accident", "Legal", "Health", "Life", "Financial.Serv",
                   "Total.Premium", "Total.No.Contracts", "Cust.ID", "Portal.Flag")


#Asssign the correct values/labels to the categorical variables, putting them into factors
#to later run analysis.

MyLVM$Gender <- factor(MyLVM$Gender,
                     levels=c(1:2),
                     labels= c("Male", "Female"))


MyLVM$Employment.Status <- factor(MyLVM$Employment.Status,
                                  levels= c(0:15),
                                  labels = c("Unknown", "Employee", "Managing Employee",
                                  "Worker", "Officials", "Clerk", "NA", "Freelancer",
                                  "Self-employed", "Trainee", "Military/civil service",
                                  "Retiree", "Unemployed", "Student", "Pol/Feu Lebenszeit",
                                  "Beamter"))
# *6 is labeled as "NA" as there are no observations for it in the variable

MyLVM$Family.Status <- factor(MyLVM$Family.Status,
                                levels= c(0:10),
                                labels = c("Unknown", "Single", "Married", "Divorced",
                                           "Widowed", "Alone", "Civil Partnership", 
                                           "Stand-alone", "Not Stand-alone", "Dead", 
                                           "Long-term Relat"))


MyLVM$Customer.Type <- factor(MyLVM$Customer.Type,
                                levels= c(0:2),
                                labels = c("Private", "Agriculture", "Commercial"))

MyLVM$Portal.Flag <- factor(MyLVM$Portal.Flag,
                              levels= c(0:1),
                              labels = c("Non.User", "User"))


#There are some small categories according to Employment Status and Family Status
#it might make sense to group similar categories to increase balance between groups
summary(MyLVM$Employment.Status)

#Order this later nicer
#Unknown           Employee      Managing Employee                 Worker 
#4871               11072                    742                   2525 
#Officials          Clerk                     NA             Freelancer 
#123                 321                      0                    112 
#Self-employed     Trainee Military/civil service                Retiree 
#1564                 343                      5                   1505 
#Unemployed           Student     Pol/Feu Lebenszeit                Beamter 
#694                    475                     94                    554 

summary(MyLVM$Family.Status)

#Unknown            Single           Married          Divorced           Widowed             Alone 
#2258              5021             13145               736               525               384 
#Civil Partnership    Stand-alone   Not Stand-alone  Dead      Long-term Relat 
#2230               243               274                26               158 


#Mix occupation factors: 
# 'public service' -Officials, Clerk, Military, Police, Beamter
# 'self-employment' -self-employed, freelance
levels(MyLVM$Employment.Status) <- c("Unknown", "Employee", "Managing Employee",
                                     "Worker", "Public Service", "Public Service", "NA", "Self-employed",
                                     "Self-employed", "Trainee", "Public Service",
                                     "Retiree", "Unemployed", "Student", "Public Service",
                                     "Public Service")

# Family status: single -stand-alone, single, alone
levels(MyLVM$Family.Status) <- c("Unknown", "Single", "Married", "Divorced",
                                 "Widowed", "Single", "Civil Partnership", 
                                 "Single", "Not-alone", "Dead", 
                                 "Long-term Relat")

summary(MyLVM$Employment.Status)
summary(MyLVM$Family.Status)
summary(MyLVM$Portal.Flag)



## Geographical analysis


map = readRDS("./DEU_adm1.rds") # Map of Germany

regions = read.csv2("./Bundeslaender CSV.csv", sep = ",", encoding = "utf-8", header = TRUE)
regions$ID = str_pad(regions$ID, 2, pad = "0")

landkreise = read.csv2("./Landkreise-Bevoelkerung CSV.csv", sep = ",", header = TRUE)
names(landkreise)[names(landkreise) == "Schl??sselnummer"] = "ID" # Change name of the column for easier access
landkreise$ID = str_pad(landkreise$ID, 5, pad = "0")
landkreise$Insgesamt = as.numeric(str_replace_all(landkreise$Insgesamt, fixed(" "), ""))

MyLVM$Cust.Location = str_pad(MyLVM$Cust.Location, 5, pad = "0")
MyLVM$Region.Code = substr(MyLVM$Cust.Location, 1, 2)
MyLVM = merge(MyLVM, regions, by.x = "Region.Code", by.y = "ID")
MyLVM = MyLVM[, !(names(MyLVM) %in% c("Region.Code", "Hauptstadt"))] # Remove info not needed
names(MyLVM)[names(MyLVM) == "Bundesland"] = "Region" # Change name of the column for easier access

# Total number of customers
total_customers = nrow(MyLVM)

total_customers_region = lapply (1:nrow(regions), function (x) {
  which(substr(MyLVM$Cust.Location, 1, 2) %in% regions[x,1])
})

customers_region = sapply(1:nrow(regions), function (x) {
  length(total_customers_region[[x]])
})

customers_region_percentage = sapply(1:nrow(regions), function (x) {
  customers_region[x]/total_customers
})

premium_region = sapply(1:nrow(regions), function (x) {
  sum(MyLVM$Total.Premium[total_customers_region[[x]]])
})

customer_premium_region = premium_region/customers_region

region_information = cbind(as.data.frame(cbind(customer_premium_region, customers_region_percentage,
                                               "region_id" = regions$ID,
                                               "region" = as.character(regions$Bundesland))))
region_information$customer_premium_region = as.numeric(as.character(customer_premium_region))
region_information$customers_region_percentage = as.numeric(as.character(customers_region_percentage))


# Percentage of lvm customers per region
col_no = cut(as.numeric(region_information$customers_region_percentage[as.numeric(map@data$CCA_1)]),
             c(seq(0, max(region_information$customers_region_percentage), by = 0.05), 1))
map$col_no = col_no
map_customers_region_percentage = spplot(map, "col_no", col = grey(.4), col.regions = brewer.pal(8, "Greens"),
                                         colorkey = list(labels = list( 
                                           labels = c("<0%", "0-5%", "5-10%", "10-15%", "15-20%", "25-30%", "30-35%", ">35%"),????????????????
                                           width = 2, cex = 1.5)),
                                         main = list("Distr. of LVM Customers per Region", cex = 2))

# Total premium of lvm customer per region
col_no = cut(as.numeric(region_information$customer_premium_region[as.numeric(map@data$CCA_1)]),
             c(seq(500, max(region_information$customer_premium_region), by = 100), 
               max(region_information$customer_premium_region)))
map$col_no = col_no
map_customer_premium_region = spplot(map, "col_no", col = grey(.4), col.regions = brewer.pal(6, "Greens"), 
                                     colorkey = list(labels = list( 
                                       labels = c("<???600", "???600-???700", "???700-???800", "???800-???900", "???900-???1000", ">???1000"),????????????????
                                       width = 2, cex = 2)),
                                     main = list("Total Premium/Customer per Region", cex = 2))



#Start checking relationhips among variables

#Separete for males and females to later make comparisons between groups
MaleLVM <- MyLVM[MyLVM$Gender == "Male", ]
FemLVM <- MyLVM[MyLVM$Gender == "Female",]

#Creating a subset by age groups
Age020 <- MyLVM[MyLVM$Age > 0.00 & MyLVM$Age <= 20,]
Age2030 <- MyLVM[MyLVM$Age > 20 & MyLVM$Age <= 30,]
Age3040 <- MyLVM[MyLVM$Age > 30 & MyLVM$Age <= 40,]
Age4050 <- MyLVM[MyLVM$Age > 40 & MyLVM$Age <= 50,]
Age5060 <- MyLVM[MyLVM$Age > 50 & MyLVM$Age <= 60,]
Age6070 <- MyLVM[MyLVM$Age > 60 & MyLVM$Age <= 70,]
Age7080 <- MyLVM[MyLVM$Age > 70 & MyLVM$Age <= 80,]

#1. Graph and calculate freq. distribution of the variables

#93% of the client base is from private...
#Cust. Type
levels(MyLVM$Customer.Type)
hist.data.perc(MyLVM$Customer.Type, "Cust. Type")

hist.data.perc(MyLVM$Gender[MyLVM$Customer.Type =="Private"], "Private Cust")
hist.data.perc(MyLVM$Gender[MyLVM$Customer.Type =="Agriculture"], "Agric. Cust")
hist.data.perc(MyLVM$Gender[MyLVM$Customer.Type =="Commercial"], "Commer. Cust")

hist.data.perc(MaleLVM$Customer.Type, "Male Cust. Type")
hist.data.perc(FemLVM$Customer.Type, "Female Cust. Type")

#Emply Stat.
levels(MyLVM$Employment.Status)
hist.data.perc(MyLVM$Employment.Status, "Empl. Stat")
hist.data.perc(MaleLVM$Employment.Status, "Male Empl. Stat")
hist.data.perc(FemLVM$Employment.Status, "Female Empl. Stat")

#Fam. Status
hist.data.perc(MyLVM$Family.Status, "Fam. Stat")
hist.data.perc(MaleLVM$Family.Status, "Male Fam. Stat")
hist.data.perc(FemLVM$Family.Status, "Female Fam. Stat")

#59% of male and 41% of female clients --> opportun. to capture more females, Germany is 50/50
hist.data.perc(MyLVM$Gender, "Gender")


#98% of their clients do not use the online portal...
levels(MyLVM$Portal.Flag)
hist.data.perc(MyLVM$Portal.Flag, "Portal Users")

hist.data.perc(MyLVM$Gender[MyLVM$Portal.Flag =="Non.User"], "Portal Non-Users")
hist.data.perc(MyLVM$Gender[MyLVM$Portal.Flag =="User"], "Portal Users")


#Users per age group
Users20 <- sum(Age020$Portal.Flag == "User")
Users2030<- sum(Age2030$Portal.Flag == "User")
Users3040<- sum(Age3040$Portal.Flag == "User")
Users4050<- sum(Age4050$Portal.Flag == "User")
Users5060<- sum(Age5060$Portal.Flag == "User")
Users6070<- sum(Age6070$Portal.Flag == "User")
Users7080<- sum(Age7080$Portal.Flag == "User")



#both groups are the same 98% non-users and 2% users
hist.data.perc(MaleLVM$Portal.Flag, "Male Portal Users")
hist.data.perc(FemLVM$Portal.Flag, "Fem Portal Users")

#There are 2 observations (clients) with less than 5 years --> poss outliers?
hist.data.perc(MyLVM$Age, "Age")


#Total Premium
hist.data.perc(MyLVM$Total.Premium, "Total Premium") + xlim(0, 25000)


#Premium per Type of Customer
hist.data.perc(MyLVM$Total.Premium[MyLVM$Customer.Type =="Private"], "Premium Private")
hist.data.perc(MyLVM$Total.Premium[MyLVM$Customer.Type =="Agriculture"], "Pr. Agriculture")
hist.data.perc(MyLVM$Total.Premium[MyLVM$Customer.Type =="Commercial"], "Pr. Commercial")

#Sum the priemum for each cust. type

Premium.Priv <- sum(MyLVM$Total.Premium[MyLVM$Customer.Type =="Private"])
Premium.Agr <- sum(MyLVM$Total.Premium[MyLVM$Customer.Type =="Agriculture"])
Premium.Comm <- sum(MyLVM$Total.Premium[MyLVM$Customer.Type =="Commercial"])

meanPriv <- mean(MyLVM$Total.Premium[MyLVM$Customer.Type =="Private"])
meanAgr <- mean(MyLVM$Total.Premium[MyLVM$Customer.Type =="Agriculture"])
meanComm <- mean(MyLVM$Total.Premium[MyLVM$Customer.Type =="Commercial"])


#Premium per gender
Premium.Male <- sum(MaleLVM$Total.Premium)
Premium.Female <-sum(FemLVM$Total.Premium)

#Premium per Portal User
Prem.NonUs <- sum(MyLVM$Total.Premium[MyLVM$Portal.Flag == "Non.User"])
Prem.User <- sum(MyLVM$Total.Premium[MyLVM$Portal.Flag == "User"])
                 




#No. of Contracts.

#Premium per age group

Prem.020 <-sum(MyLVM$Total.Premium[MyLVM$Age > 0.00 & MyLVM$Age <= 20])
Prem.2030 <-sum(MyLVM$Total.Premium[MyLVM$Age > 20 & MyLVM$Age <= 30])
Prem.3040 <-sum(MyLVM$Total.Premium[MyLVM$Age > 30 & MyLVM$Age <= 40])
Prem.4050 <-sum(MyLVM$Total.Premium[MyLVM$Age > 40 & MyLVM$Age <= 50])
Prem.5060 <-sum(MyLVM$Total.Premium[MyLVM$Age > 50 & MyLVM$Age <= 60])
Prem.6070 <-sum(MyLVM$Total.Premium[MyLVM$Age > 60 & MyLVM$Age <= 70])
Prem.7080 <-sum(MyLVM$Total.Premium[MyLVM$Age > 70 & MyLVM$Age <= 80])

#Pie Charts:
par(mfrow=c(1,1))
##Concl: 12% of the premium comes from the biggest customers "commercial" --> 3% of their clients
piecharts(c(Premium.Agr,Premium.Comm,Premium.Priv), c("Agriculture","Commercial","Private"),
          "Tot. Premium by Cust.Type")
##Aprox 70% of prem. comes from 'male' clients --> explained, because agric and comm are mainly male
piecharts(c(Premium.Male, Premium.Female), c("Male","Female"),
          "Tot. Premium by Gender")
##60% of the prem. comes from age group 40-60 --> try to cap. young ppl w/ higher inc?
piecharts(c(Prem.020,Prem.2030,Prem.3040,Prem.4050,Prem.5060,Prem.6070,Prem.7080), 
          c("0-20 years","20-30 years", "30-40 years", "40-50 years", "50-60 years",
                            "60-70 years", "70-80 years", "80-more years"),
          "Tot. Premium by Age")
#Interesting that the largest group of users are in the age of 50-60y
piecharts(c(Users20,Users2030,Users3040,Users4050,Users5060,Users6070,Users7080),
          c("0-20 years","20-30 years", "30-40 years", "40-50 years", "50-60 years",
            "60-70 years", "70-80 years", "80-more years"), "Port. Users by Age")
#Non.Users are 96% 
piecharts(c(Prem.NonUs,Prem.User), c("Non.User", "User"), "Tot. Prem. by Portal Use")

#No. of Contracts
hist.norm(MyLVM$Total.No.Contracts, "No. of Contracts")
hist.data(MaleLVM$Total.No.Contracts, "Male No. of Contracts")
hist.data(MyLVM$Total.No.Contracts, "Female No. of Contracts")

#for the regions/states missing (also a function which identifies states)


#Scatterplots and Correlations
#For correlations analysis first, focus on the metric variables to find relationships
#Analysis of categorical var. should be done through ANOVA and its variations to find
# significant results.

names(MyLVM)
LVM.correlations <- data.frame("Age"= MyLVM$Age,"Tot.No.Contr"= MyLVM$Total.No.Contracts, 
                               "Vehicle" = MyLVM$Vehicle,"Property"=  MyLVM$Property, 
                               "Imdemnity"= MyLVM$Indemnity,"Accident"=  MyLVM$Accident,
                               "Legal"= MyLVM$Legal, "Health"= MyLVM$Health, "Life"= MyLVM$Life,
                               "Financ.Serv"= MyLVM$Financial.Serv,"Tot.Prem"= MyLVM$Total.Premium)

#Scatterplots -->nothing much can be told from them, and it takes a lot time 
#to compute
pairs(LVM.correlations, panel = function (x, y, ...) {
      points(x, y, ...)
      abline(lm(y ~ x), col="red")},
      pch = 19,cex.labels=2,cex.axis=2)


#Use of heatmaps for better visualization of correlations.
heatmap(lvm,"pearson","Pearson Correlation")

heatmap(LVM.correlations,"pearson","Pearson Correlation")
heatmap(LVM.correlations, "spearman", "Spearman Correlation")

### ATTENTION: Kendall takes too much time to compute!!
# heatmap(LVM.correlations,"kendall","Kendall Correlation")

##Start with normality and outlier detection

#considering only the metric variables

#Visual Inspection through QQ-plots and Histograms 
#Checking for normality:
ag <- hist.norm(MyLVM$Age, "Age") 
prem<- hist.norm(MyLVM$Total.Premium, "Total Premium") 
cont <- hist.norm(MyLVM$Total.No.Contracts, "No. of Contracts")
veh<- hist.norm(MyLVM$Vehicle, "Vehicle") 
prop <- hist.norm(MyLVM$Property, "Property") 
indem <- hist.norm(MyLVM$Indemnity,"Indemnity")
acc<-hist.norm(MyLVM$Accident,"Accident")
leg <-hist.norm(MyLVM$Legal,"Legal")
heal<-hist.norm(MyLVM$Health,"Health")
lif<-hist.norm(MyLVM$Life,"Life")
fin<-hist.norm(MyLVM$Financial.Serv,"Financ.")

#Histograms with norm. distr. curve
multiplot(ag,prem,cont,veh,prop,indem,acc,leg,heal,lif,fin, cols=4)
#qqplots from MVN package 
par(mfrow=c(1,1))
uniPlot(LVM.correlations, type="qqplot", col="chartreuse3")

#Performing statistical tests

#Performing univariate tests:
#SW-Test --> sample is too large (doesn't work) 
uniNorm(LVM.correlations, type="SW", desc=T)
##--> draw a random sample of max 5,000 size --> also not normal distr.
test <- LVM.correlations[sample(nrow(LVM.correlations),5000),]
uniNorm(test, type="SW", desc=T)

#Anderson-Darling Test --> none of the variables are Norm.distr.
uniNorm(LVM.correlations, type="AD", desc=T)

#Kolmogorv-Smirnov Test --> none of the variables are Norm.distr
uniNorm(LVM.correlations, type="Lillie", desc=T)

#CVM Test --> p-values too small -> none of them are norm. distr.
uniNorm(LVM.correlations, type="CVM", desc=T)

#Based on statistical tests --> we cannot assume a univ. normal distrib.


#Check for Multivariate
# 2. Check for MVN with Mardia, Henze-Zirkler and Roysten tests with package "MVN"
library(MVN)
# At this point we check for Multivariate Normality. However, as the necessary
# condition is violated (e.g. Univariate Normal distribution),
# postive results are not possible. Further, as non of the variables is approx. normally
# distributed concentrating on a subset of the data makes no sense.

# Mardia test on a random subset of 5000 oberservation due to high data volume.
# MArdia tests on SKewness and kurotosis, thus only on the necessary condition

MyLVMtest<-LVM.correlations[sample(nrow(MyLVM),5000),c(1:11)]
Mardia_Test<-mardiaTest(MyLVMtest,qqplot=TRUE)
Mardia_Test # Not multivariate normal. Seen from p-values and from Q-QPlot

# Hinze-Zirkler Test
Henze_Test<- hzTest(MyLVMtest,qqplot=TRUE)
Henze_Test # Not multivariate normal. Seen from p-values and from Q-QPlot

# Royston Test with n = 2000
Royston_Test<- roystonTest(MyLVMtest[1:2000,], qqplot=TRUE)
Royston_Test # Not multivariate normal. Seen from p-values and from Q-QPlot

mvOutlier(LVM.correlations[,c(1,11)],qqplot=TRUE)
stroutlier

#Doing box-cox transformation to our metric variables

##Log transformation --> did not help either --> some kinds of contracts have
# 0s, son then it's not possible to apply it correctly.
LVM.correlations.ln <- log(LVM.correlations)


#SW-Test --> sample is too large (doesn't work) 
uniNorm(LVM.correlations.ln, type="SW", desc=T)
##--> draw a random sample of max 5,000 size --> also not normal distr.
test2 <- LVM.correlations.ln[sample(nrow(LVM.neu),5000),]
uniNorm(test2, type="SW", desc=T)

#Anderson-Darling Test --> none of the variables are Norm.distr.
uniNorm(LVM.correlations.ln, type="AD", desc=T)

#Kolmogorv-Smirnov Test --> none of the variables are Norm.distr
uniNorm(LVM.correlations.ln, type="Lillie", desc=T)

#CVM Test --> p-values too small -> none of them are norm. distr.
uniNorm(LVM.correlations.ln, type="CVM", desc=T)

##Box-Cox Transformation -->not possible
##Conc. Data is not Normally and we move forward.

layout(matrix(1:12, ncol=4,nrow=3))
sapply(colnames(test), function(x){
       lambda <- boxcoxnc(test$Total.Premium,method='sw',
                          plotit=FALSE,lam=seq(-4,4,0.01))$result[[1]]
       tr <- (test$Total.Premium[[x]] ^{lambda}-1)/lambda
       hist(tr,main=x,xlab=round(shapiro.test(tr)$p.value,2),
            col='cyan',cex.main=2,cex.lab=2,ylab='')
       })


lambda <- boxcoxnc(test$Accident,method='sw',
                   plotit=FALSE,lam=seq(-4,4,0.01))$result[[1]]
tr <- ((test$Accident)^{lambda} -1)/lambda
hist(tr,main=x,xlab=round(shapiro.test(tr)$p.value,2),
     col='cyan',cex.main=2,cex.lab=2,ylab='')


##Outlier detection

#1. Do boxplots of metric variable for identifying potential outliers

#Outliers Age: 2 observations which are of age less than 5years who
#do not have any meaningful info attached to it (e.g. 0??? Premium, 1 contract, private cust) 
OutAge<- ggplot(data=MyLVM, aes(x= "",y=Age)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("") +
  ylab("Age")

summary(MyLVM[MyLVM$Age < 5,])

#Outliers Tot. Premium: 75% of the clients have a premium between 217???-1010???, it is oberservable
#observations above this range for high paying clients --> not necessarily measurement error.
OutPrem <- ggplot(data=MyLVM, aes(x= "",y=Total.Premium)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("") +
  ylab("Tot. Premium")

summary(MyLVM[MyLVM$Total.Premium >= 1010,])

#Outliers Tot. No. Of Contracts: all observations have at least 1 contract, and most of the clients
#hold between 1-4 contracts that are mostly vehicle, property, or indemnity insurances.
#Basically private customers, whereas in the case of higher no. of contracts e.g.>15, we
#see mainly commercial&agriculture clients.

OutTCont <- ggplot(data=MyLVM, aes(x= "",y=Total.No.Contracts)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("") +
  ylab("Tot. No. Of Contracts")

summary(MyLVM[MyLVM$Total.No.Contracts <= 4,])
summary(MyLVM[MyLVM$Total.No.Contracts>= 15,])

#Outliers For Each kind of Contract
ContrTyp = cbind.data.frame(MyLVM$Vehicle, MyLVM$Property, MyLVM$Indemnity, MyLVM$Accident, MyLVM$Legal, 
                            MyLVM$Health, MyLVM$Life, MyLVM$Financial.Serv)
ContrTypM = melt(ContrTyp)

OutTyCon <- ggplot(data=ContrTypM, aes(x=variable ,y=value)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete(labels=c("Vehic.","Prop.","Indem.","Accid.",
                            "Legal","Health","Life","Financ")) + xlab("Kind of Contract") +
  ylab("No. Of Contracts")

#Plotting Outliers from Met. Var.
multiplot(OutAge, OutPrem, OutTCont, OutTyCon, cols=2)

#2.Looking into more details of the categorical variables w/ metric var.

#Age-Gender
Out.AgeGen <- ggplot(data=MyLVM, aes(x= Gender ,y=Age)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Gender") +
  ylab("Age")

#Age-Employment Status
OutEmpl <- ggplot(data=MyLVM, aes(x= Employment.Status,y=Age)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Employ Stat") +
  ylab("Age")

#Age-Family Status
OutFam <- ggplot(data=MyLVM, aes(x= Family.Status,y=Age)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Family Stat") +
  ylab("Age")

#Age-Portal Use
OutPort<- ggplot(data=MyLVM, aes(x= Portal.Flag,y=Age)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Portal Use") +
  ylab("Age")
#Premium-Cust. type
OutPrCT <- ggplot(data=MyLVM, aes(x= Customer.Type,y=Total.Premium)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Cust. Type") +
  ylab("Tot. Premium")

#by cust type
OutCT <- ggplot(data=MyLVM, aes(x=Customer.Type,y=Total.No.Contracts)) +
  geom_boxplot(fill = "grey80", colour = "chartreuse3") +
  scale_x_discrete() + xlab("Cust. Type") +
  ylab("Tot. No. Of Contracts")

multiplot(Out.AgeGen, OutEmpl, OutFam,OutPort, OutPrCT, OutCT, cols=2)

##PCA
#Only with metric variables: Total Prem., Age, and Tot. No. of Contracts (the
#single )

# Underlying covariance matrix 
#not being positive-semidefinite ("covariance matrix is not non-negative definite")


PCA <- princomp(LVM.correlations, cor=TRUE, scores=TRUE)

#This implies the presence of negative eigenvalues in the cov/corr matrices

#Identifying which eigenv. are negative: it is that of the 11th variable. Yet,
# the PCA is not running if we exclude it.
which(eigen(cor(LVM.correlations))$values<0)

#Therefore, what can cause this is a very small variance of a variable,
#so we sort them from lower to greater and try eliminating one by one until
#the PCA runs:
sort(apply(LVM.correlations, 2, var), decreasing=FALSE)
match(names(sort(apply(LVM.correlations, 2, var), decreasing=FALSE)), 
      names(LVM.correlations))

#This leads to excluding Financ. Serv (var=0.06) which allows to run the PCA
PCA = princomp(LVM.correlations[,-c(10)], cor=TRUE, scores=TRUE)
summary(PCA, loadings = TRUE)

#Based on the 80% of var. criterion, we could chose 6 PCs as they cover 84% 
#of the variance explained among the metric variables. 

#However, looking at the scree plot and the 'kink' criterion one could argue to 
#consider between 2-3 PCs, which would explain at most 58%
plot(PCA$sdev, type="b", main="Scree diagram PCA on cor", xlab="Component number", ylab="Component variance")

#Concl: Stay with 6 PCs.

biplot(PCA, main="Biplot PCA on correlat")
#show loadings of components
round((summary(PCA, loadings = T)$loadings[,1:6]),3)

#Looking at the scatterplots from the 6PCs
pairs(PCA$scores[,1:6], m="Scatterplot on PCs")
cor(PCA)