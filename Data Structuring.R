#---- LVM Case Study ------
# --- Group 7 --------------
# Magnus Acker      (394456)
# Daniel Carriola   (425699) 
# Gino S. Coletti P (425904)
# Ond??ej Z??ruba     (434084)



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







