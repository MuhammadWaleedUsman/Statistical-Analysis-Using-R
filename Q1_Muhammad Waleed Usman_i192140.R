# Script by Muhammad Waleed Usman (i19-2140)


# Intsall these dependencies if not installed
install.packages('corrplot') # corrplot
install.packages('Hmisc') # Hmisc
install.packages('readxl') # readxl
install.packages('dplyr') # dplyr
install.packages('ggplot2') # ggplot2


# Libraries included in the project
library(readxl)
library(corrplot)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(plyr)


# Flattern Matrix function which flatterns the data into a flattern data to check the highest value by using the FLattern Matrix
flattenMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


# Probability function which has two inputs as a columns data as event a and b 
Probabilities <- function(A, B,title) {
  X <- table(t(A))
  Y <- table(t(B))
  twoway <- table(t(A),t(B))
  barplot(twoway, legend = T, beside = T,main = title)
  prob_intake <- prop.table(X)
  prob_type <- prop.table(Y)
  prop.table(twoway,1)
  return(prop.table(twoway,1))
}


# Reading the excel file provided
admission <- read_excel(file.choose(), skip = 1)


# Question 1. Do the students performing good in Matric/O-level exam have a better chance of getting admission in NUCES?


# Making of list of Matriculation marks
Matriculation_Obtained_marks =admission$Obtained...16
Matriculation_Total_marks =admission$Total...15

# Calculating the percentages of matriculation marks
Matriculation_Percentages = c(as.numeric((Matriculation_Obtained_marks /Matriculation_Total_marks)*100))
Matriculation_Grades = data.frame(Grades=character(),stringsAsFactors=FALSE)


#Evaluating the Grades of matriculation marks
for (marks in Matriculation_Percentages){
  if (!is.na(marks)){
    if (marks>=80) {Matriculation_Grades <- rbind(Matriculation_Grades,c("A Grade"),stringsAsFactors=FALSE)}
    else if (marks>=70) {Matriculation_Grades <- rbind(Matriculation_Grades,c("B Grade"),stringsAsFactors=FALSE)}
    else {Matriculation_Grades <- rbind(Matriculation_Grades,c("C Grade"),stringsAsFactors=FALSE)}
  }
  else if (is.na(marks)){
    Matriculation_Grades <- rbind(Matriculation_Grades,c(NA),stringsAsFactors=FALSE)
  }
}


# Admission Offers from the NU and Nts to the Student in NUCES
Nts_Admission_Offer= data.frame(ntsOffer= admission$Discipline...33, stringsAsFactors = FALSE)
NU_Admission_Offer= data.frame(nuoffer= admission$Discipline...35, stringsAsFactors = FALSE)
Combine_offer_Nts_NU = Nts_Admission_Offer


#Taking union of the two offers 
Combine_offer_Nts_NU[is.na(Combine_offer_Nts_NU)] <- NU_Admission_Offer[is.na(Combine_offer_Nts_NU)]
# Making categories of the selected and the notselected candedates
Combine_offer_Nts_NU[!is.na(Combine_offer_Nts_NU)] <- "Selected"
Combine_offer_Nts_NU[is.na(Combine_offer_Nts_NU)] <- "Not Selected"

#Dataframe of Combine_offer_Nts_NU
Dataframe_Combine_offer_Nts_NU = data.frame(Nts=Nts_Admission_Offer,nu=NU_Admission_Offer,combineoffer=Combine_offer_Nts_NU)
#View(Dataframe_Combine_offer_Nts_NU)

Probability_table_for_Question1 = Probabilities(Matriculation_Grades,Combine_offer_Nts_NU,"Barplot for Grades in Matric/O-level and Selection in Nuces")
print(Probability_table_for_Question1)

