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

#Give names to the columns  
colnames(MyLVM) <- c("Employment Status","Family Status", "Gender","Customer Type", 
                   "Age", "Cust. Location", "Vehicle", "Property", "Indemnity",
                   "Accident", "Legal", "Health", "Life", "Financial Serv.",
                   "Total No. Contracts", "Total Premium", "Cust. ID", "Portal Flag")


#Asssign the correct values/labels to the categorical variables, putting them into factors
#to later run analysis.

MyLVM$Gender <- factor(MyLVM$Gender,
                     levels=c(1:2),
                     labels= c("Male", "Female"))

#6 is labeled as "NA" as there is no value for it in the 
MyLVM$`Employment Status` <- factor(MyLVM$`Employment Status`,
                                  levels= c(0:15),
                                  labels = c("Unknown", "Employee", "Managing Employee",
                                  "Worker", "Officials", "Clerk", "NA", "Freelancer",
                                  "Self-employed", "Trainee", "Military/civil service",
                                  "Retiree", "Unemployed", "Student", "Pol/Feu Lebenszeit",
                                  "Beamter"))

MyLVM$`Family Status` <- factor(MyLVM$`Family Status`,
                                levels= c(0:10),
                                labels = c("Unknown", "Single", "Married", "Divorced",
                                           "Widowed", "Alone", "Civil Partnership", 
                                           "Stand-alone", "Not Stand-alone", "Dead", 
                                           "Long-term Relat"))


MyLVM$`Customer Type` <- factor(MyLVM$`Customer Type`,
                                levels= c(0:2),
                                labels = c("Private", "Agriculture", "Commercial"))

#There are some small categories according to Employment Status and Family Status
#it might make sense to group similar categories to increase balance between groups
summary(MyLVM$`Employment Status`)

#Order this later nicer
#Unknown           Employee      Managing Employee                 Worker 
#4871               11072                    742                   2525 
#Officials          Clerk                     NA             Freelancer 
#123                 321                      0                    112 
#Self-employed     Trainee Military/civil service                Retiree 
#1564                 343                      5                   1505 
#Unemployed           Student     Pol/Feu Lebenszeit                Beamter 
#694                    475                     94                    554 

summary(MyLVM$`Family Status`)

#Unknown            Single           Married          Divorced           Widowed             Alone 
#2258              5021             13145               736               525               384 
#Civil Partnership    Stand-alone   Not Stand-alone  Dead      Long-term Relat 
#2230               243               274                26               158 
