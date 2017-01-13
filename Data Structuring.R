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

#Asssign the correct values/labels to the categorical variables

Labels.FamStat <- list("Unknown" = 0, "Single"= 1, "Married" = 2, "Divorced" = 3,
                       "Widowed" = 4, "Alone" = 5, "Civil Partnership" = 6, 
                       ""
  
  0 = ???unknown???, 1 = ???Single???, 2= ???Married???, 3 = ???Divorced???, 4 = ???Widowed???, 5 = ???allein stehend / getrennt lebend???,
6 = ???Civil partnership???, 7 = ???allein stehend???, 8 = ???nicht allein stehend???,  9 = ???dead???, 10 = ???Lebenspartnerschaft???


