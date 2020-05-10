
#Reading file from wokring directory
crime_data = read.csv("garda_stations.csv")

#Displaying the first 5 rows
head(crime_data,5)

#displays the structure of the data
str(crime_data)
#printing the dimensions of data 
dim(crime_data)

#The dataset by default has no null values 
sum(is.na(crime_data))

sum(crime_data == -1)

#stringr library used for string manipulation
library(stringr)
#storing the colnames so that the names can be changed
col_names <- colnames(crime_data)
col_names

#Renaming the column names for better readability 
modified_col_names <- gsub("Attempts.or.threats.to.murder..assaults..harassments.and.related.offences.","MAH_", col_names)
modified_col_names <- gsub("Dangerous.or.negligent.acts.", "DN_", modified_col_names)
modified_col_names <- gsub("Kidnapping.and.related.offences.", "kidnap_", modified_col_names)
modified_col_names <- gsub("Robbery..extortion.and.hijacking.offences.", "REHi_", modified_col_names)
modified_col_names <- gsub("Burglary.and.related.offences.", "Burglary_" ,modified_col_names)
modified_col_names <- gsub("Theft.and.related.offences.", "Theft_", modified_col_names)
modified_col_names <- gsub("Fraud..deception.and.related.offences.", "Fraud_", modified_col_names)
modified_col_names <- gsub("Controlled.drug.offences.", "Drug_", modified_col_names)
modified_col_names <- gsub("Weapons.and.Explosives.Offences.", "Weapon_", modified_col_names)
modified_col_names <- gsub("Damage.to.property.and.to.the.environment.", "PDE_", modified_col_names)
modified_col_names <- gsub("Public.order.and.other.social.code.offences.", "PoSo_", modified_col_names)
modified_col_names <- gsub("Offences.against.government..justice.procedures.and.organisation.of.crime.", "AgainstGvt_",modified_col_names)

#The changed column names are stored in this variable
modified_col_names

#Assigning the new names to the data
colnames(crime_data) <- modified_col_names

#The cleaned data is saved to csv so that we dont have to recode or restructure repeatedly 
write.csv(crime_data, "CleanedCrimeData.csv")


#There are 183 columns consisting number of crimes in each year with respect to crime categories 
#we sum up all the similar categorie crimes using rowSums
crime_data$Total_MAH <- rowSums(crime_data[,6:18])
crime_data$Total_DN <- rowSums(crime_data[,19:32])
crime_data$Total_kidnap <- rowSums(crime_data[,33:46])
crime_data$Total_REHi <- rowSums(crime_data[,47:60])
crime_data$Total_Burglary <- rowSums(crime_data[,61:74])
crime_data$Total_Theft <- rowSums(crime_data[,75:88])
crime_data$Total_Fraud <- rowSums(crime_data[,89:102])
crime_data$Total_Drug <- rowSums(crime_data[,103:116])
crime_data$Total_weapon <- rowSums(crime_data[,117:130])
crime_data$Total_PDE <- rowSums(crime_data[,131:144])
crime_data$Total_poSo <- rowSums(crime_data[,145:158])
crime_data$Total_AgainstGVT <- rowSums(crime_data[,159:172])
crime_data$Total_crime <- rowSums(crime_data[,6:184])

str(crime_data)
dim(crime_data)

#rounded the decimal part using round function
crime_data$crime_rate <- round((crime_data$Total_crime/sum(crime_data$Total_crime))*100, digits = 3)


#Below code is done to select only required columns in the data
modified_col_names_total <- colnames(crime_data)
variables_list <- !logical(length(modified_col_names_total))
variables_list

?setNames

#setNames can be used to set the names and match the required name  
variables_list <- setNames(variables_list, modified_col_names_total)
variables_list

#Selecting the cOlnames that contain word Total
Total_variables <- sapply(modified_col_names_total, function(x) grepl("Total", x))
Total_variables

#We are setting the columns to true which are required for analysis  
Total_variables["id"] <- TRUE
Total_variables["Station"] <- TRUE
Total_variables["Divisions"] <- TRUE
Total_variables["crime_rate"] <- TRUE


#selecting the columns that are required for analysis 
simple_crime_data <- crime_data[,Total_variables]
#This is the cleaned and simplified data used for analysis 
head(simple_crime_data)

#structire of the data after cleaning 
str(simple_crime_data)

#Lets check how these variables are correlated 

#Taking only numerical variables as corrplot cannot handle factors
numerical_list <- sapply(simple_crime_data, is.numeric)
numerical_list

#We dont need ID for correlation plot
numerical_list["id"] <- FALSE 

#All the numerical variables are stored in crime_variable_list
crime_variable_list <- simple_crime_data[numerical_list]
crime_variable_list

#displays the statistical summary of the values 
sapply(crime_variable_list, summary)

#finding the correlation of the variables 
library(corrplot)
corrplot(corr = cor(crime_variable_list), tl.col = "Black", tl.cex = 0.9)
cor(crime_variable_list)

#I wanted to add region for each county by using another dataset.
district_levels <- read.csv("IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv")

str(district_levels)

#The dataset i used for matching the counties and getting regions doesnt have word division in it. so i removed word division from simple_crime_data
simple_crime_data$Divisions <- str_remove_all(simple_crime_data$Divisions," division")

#making the strings to lower so that i dont get any mismatch
simple_crime_data$Divisions <- str_to_lower(simple_crime_data$Divisions)
district_levels$GARDA.DIVISION <- str_to_lower(district_levels$GARDA.DIVISION)

#Matching the columns Division to get Region columns
simple_crime_data$Region <- district_levels$REGION[match(simple_crime_data$Divisions, district_levels$GARDA.DIVISION)]

#If you observe the data you can see NA are reflected this might because of any space in the string 
sum(is.na(simple_crime_data$Region))

#we have to remove it.
simple_crime_data$Divisions <- gsub(" ","", simple_crime_data$Divisions)

#Now apply match again
simple_crime_data$Region <- district_levels$REGION[match(simple_crime_data$Divisions, district_levels$GARDA.DIVISION)]

#now the regions are added
head(simple_crime_data$Region,10)

#you can see still there are na values. we must handle them
sum(is.na(simple_crime_data$Region))

#Looking the patterns in missing data. we can see that in Region variable
library(mice)
md.pattern(simple_crime_data)
library(VIM)
missing_values <- aggr(simple_crime_data, prop= FALSE, numbers = TRUE)
summary(missing_values)

table(simple_crime_data$Divisions)
table(district_levels$GARDA.DIVISION)

#Handling missing values
#when i cross checked the district dataset the few names are not correct so i made few changes.

simple_crime_data$Divisions <- gsub("corkcity", "cork city", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("corknorth", "cork north", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("corkwest", "cork west", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.eastern", "d.m.r. eastern", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.northcentral", "d.m.r. north central", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.northern", "d.m.r. northern", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.northern", "d.m.r. northern", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.southcentral", "d.m.r. south central", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.southern", "d.m.r. southern", simple_crime_data$Divisions)
simple_crime_data$Divisions <- gsub("d.m.r.western", "d.m.r. western", simple_crime_data$Divisions)

#Now again match the divisions to get region
simple_crime_data$Region <- district_levels$REGION[match(simple_crime_data$Divisions, district_levels$GARDA.DIVISION)]

#There are no null values now
sum(is.na(simple_crime_data$Region))

#changing the position of the region column
simple_crime_data <- simple_crime_data[, c(1,2,3,18,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

str(simple_crime_data)

#The structure of simple_crime_data has charc for Divisons variable. we must change it to factor as it is category not a string
simple_crime_data$Divisions <- as.factor(simple_crime_data$Divisions)

#The structured is changed 
str(simple_crime_data)

write.csv(simple_crime_data,"simpleCrimeData.csv")


#infering visualizations for better understanding 
#dplyr provides functions like groupby,filter,mutate etc
install.packages("dplyr")
install.packages("ggplot")
library("dplyr")
library("ggplot2")
#grouping the region and deriving their total crime in that particular region
simple_crime_data%>%
  group_by(Region)%>%
  summarise(Total_crimes = sum(Total_crime))%>%
  ggplot(aes(x = Region, y = sort(Total_crimes, decreasing = TRUE), fill = Region)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Regions",
    y = "Crime_Frequency",
    title = paste(
      "Crime_Frequency_by_Region"
    )
  )

simple_crime_data%>%
  group_by(Divisions)%>%
  #grouping the region and deriving their total crime in that particular region
  summarise(Total_crimes = sum(Total_crime))%>%
  ggplot(aes(x = Divisions, y = sort(Total_crimes, decreasing = TRUE), fill = Divisions)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "County",
    y = "Crime_Frequency",
    title = paste(
      "Crime_Frequency_by_Divisions"
    )
  )
#More number of crimes where due to theft 
barplot(colSums(simple_crime_data[,5:16]))

#----------------------------PCA--------------------
#PCA

#Selecting only numerical variables to get principal components 
numerical_pca_list <- crime_variable_list[, 1:13]

#using prcomp to get principal components of numerical_pca_list
pca <- prcomp(numerical_pca_list, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)

pca

#used to get the eigenvalues 
library("factoextra")
library("ggplot2")
eig_values <- get_eigenvalue(pca)
eig_values


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 90))
?fviz_eig

#Gives the descriptions for each component in PCA
pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
#this explain how correlated our pca variables are
corrplot(pca_for_variables$cos2, is.corr = FALSE)

#plotting the PC's showing their variance  
fviz_pca_var(pca, col.var = "black")

?cor
fviz_cos2(pca, choice = "var", axes = 1:2)


fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE) # Avoid text overlapping


#biplot allows you to visualize how the samples relate to one another in our PCA (which samples are similar and which are different) 
#and will simultaneously reveal how each variable contributes to each principal component. 
library("ggbiplot")
ggbiplot(pca)
ggbiplot(pca, labels=rownames(simple_crime_data$Divisions))


#install.packages("factoextra")
library(factoextra)

fviz_pca_ind(pca,
             axes = c(1, 2), col.var = "contrib",
             geom.ind = "point", # show points only
             col.ind = simple_crime_data$Region, # colour by groups
             #palette = c("Red", "Green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Most Crimes by region"
)


fviz_pca_biplot(pca, 
                col.ind = simple_crime_data$Region, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Most crimes by region")


#_---------------------------HYpothesis test-----------------------------

library(ggplot2)

?hist

#The below few lines are used to check the distribution of variables 
#using histogram and q-q plot
hist(simple_crime_data$Total_MAH)
qqnorm(simple_crime_data$Total_MAH, main = "Q-Q plot for temp")
qqline(simple_crime_data$Total_MAH, col =3, lwd = 2)

hist(simple_crime_data$Total_weapon)
qqnorm(simple_crime_data$Total_weapon, main = "Q-Q plot for temp")
qqline(simple_crime_data$Total_weapon, col =3, lwd = 2)

#Shapiro test tells if sample is randomly distributed or not
normality_test <- shapiro.test(simple_crime_data$Total_MAH)
#p value is less 0.05 so its not normally distributed 
normality_test$p.value

normality_test <- shapiro.test(simple_crime_data$Total_weapon)
normality_test$p.value

#checking distribution for every variable 
with(simple_crime_data, sapply(simple_crime_data[,5:17], shapiro.test))

#using the spearman correlation test because the data is not normally distributed
#and we are checking the relationship between murders and weapon type crime categories 
cor.test(simple_crime_data$Total_MAH, simple_crime_data$Total_weapon, method = "spearman")



# Power test for sample size determination
# Loading pwr library
library(pwr)
# giving the test name and conventional effect size 
size <- cohen.ES(test = "t", size = "medium")
size
# Computing the power of test with effective size as from conventional effective size 
samplesize <- pwr.r.test(r=size$effect.size , n=NULL, sig.level = 0.01, power = 0.8)
samplesize
# Plotting the power of test
plot(samplesize)






