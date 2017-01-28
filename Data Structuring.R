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


#Ggplot theme setup
theme_set(theme_bw()) # I like bw acually
# present_theme <- theme_set(theme_update(axis.text = element_text(size = 15, colour = "black"), axis.title = element_text(size = 18, colour = "black", face = "bold"),
#                                         axis.text.x = element_text(angle = 0, hjust = 1), axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
#                                         legend.title = element_blank(), legend.text = element_text(size = 40)))

#theme_set(present_theme)

#Start checking relationhips among variables

#1. Graph distribution of the categorical variables
# to be improved later - would me nice to have exact numbers next to the bars
# and maybe precentages on the bars 

ggplot(MyLVM,aes(x=reorder(Employment.Status,Employment.Status,
                     function(x)-length(x))))+ geom_bar() +
  labs(x="Employment status",y=element_blank()) + coord_flip()

ggplot(MyLVM,aes(x=reorder(Family.Status,Family.Status,
                           function(x)-length(x))))+ geom_bar() +
  labs(x="Family status",y=element_blank()) + coord_flip()

ggplot(MyLVM,aes(x=reorder(Gender,Gender,
                           function(x)-length(x))))+ geom_bar() +
  labs(x="Gender",y=element_blank())

ggplot(MyLVM,aes(x=reorder(Customer.Type,Customer.Type,
                           function(x)-length(x))))+ geom_bar() +
  labs(x="Customer Type",y=element_blank())

ggplot(MyLVM,aes(x=reorder(Portal.Flag,Portal.Flag,
                           function(x)-length(x))))+ geom_bar() +
  labs(x="Portal",y=element_blank())

# Histogram overlaid with kernel density curve for Age
ggplot(MyLVM, aes(x=Age)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

library(scales) # for coord_cartesian

summary(MyLVM$Total.No.Contracts) #max is 39 but only very new ppl have so many, maybe add actual numbers on top of the bars?
ggplot(MyLVM,aes(x=reorder(Total.No.Contracts,Total.No.Contracts,
                           function(x)-length(x))))+ geom_bar() +
  labs(x="Contracts",y=element_blank()) + coord_cartesian(xlim = c(1, 15)) #zoom to 1 to 15
# can someone add exponentionaly decreasing funcion on top of that - for comparison, looks almost decreasing exponenitonaly

# just a histogram of groupped residential codes to get a gist
ggplot(MyLVM, aes(Cust.Location)) + 
  geom_histogram(binwidth = 1000) +
  labs(x="District of Residence", y=element_blank())



#for the regions missing
#better do map








