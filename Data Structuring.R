#---- LVM Case Study ------
# --- Group 7 --------------
# Magnus Acker      (394456)
# Daniel Carriola   (425699) 
# Gino S. Coletti P (425904)
# Ondej Zaruba     (434084)


# Disable scientific notation
options(scipen = 999)


#Start structuring the data

#Load the data into the working environment
load("lvm.RData")

#Exploring data and looking at its initial structure

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

#Classify according to region
#Load csv with city


#Start checking relationhips among variables

#1. Graph distribution of the categorical variables
plot(factor(MyLVM$Employment.Status))
plot(factor(MyLVM$Family.Status))
plot(factor(MyLVM$Gender))
plot(factor(MyLVM$Customer.Type))
plot(factor(MyLVM$Portal.Flag))
#for the regions missing









