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

#Exploring data and looking at its stracture
str(lvm)

#Give names to the columns  
colnames(lvm) <- c("Employment Status","Family Status", "Gender","Customer Type", 
                   "Age", "Cust. Location", "Vehicle", "Property", "Indemnity",
                   "Accident", "Legal", "Health", "Life", "Financial Serv.",
                   "Total No. Contracts", "Total Premium", "Cust. ID")
factor(lvm$`Employment Status`)

summary(lvm$`Employment Status`)
#Asssign the correct values/labels to the categorical variables

Labels.FamStat <- list("Unknown" = 0, "Single"= 1, "Married" = 2, "Divorced" = 3,
                       "Widowed" = 4, "Alone" = 5, "Civil Partnership" = 6, 
                       "Stand-alone"= 7, "Not Stand-alone"=8, "Dead" = 9, 
                       "Long-term Relat" = 10)

Labels.CustType <- list("Private"= 0, "Agriculture" = 1, "Commercial" =2)

Labels.CustType <- list("Private"= 0, "Agriculture"= 1, "Commercial" = 2)

#6 is ommited as there is no 6 on the LVM data set
Labels.Employ <- list("Unknown" = 0, "Employee" = 1, "Managing Employee" = 2,
                       "Worker" = 3, "Officials" = 4, "Clerk"=5, "Freelancer" = 7,
                       "Self-employed" = 8, "Trainee" = 9, "Military/civil service" = 10,
                      "Retiree" = 11, "Unemployed" = 12, "Student" = 13, "Pol/Feu Lebenszeit" = 14,
                      "Clerk" = 15)
