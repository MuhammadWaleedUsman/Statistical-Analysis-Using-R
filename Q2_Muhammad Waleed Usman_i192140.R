# Question 2. Do the students performing good in FSC/A-level exam have a better chance of getting admission in NUCES?

# Making of list of Fsc marks
fsc_Obtained_marks =admission$Obtained...20
fsc_Total_marks =admission$Total...19

# Calculating the percentages of Fsc marks
fsc_Percentages = c(as.numeric((fsc_Obtained_marks /fsc_Total_marks)*100))
fsc_Grades = data.frame(Grades=character(),stringsAsFactors=FALSE)


#Evaluating the Grades of Fsc marks
for (marks in fsc_Percentages){
  if (!is.na(marks)){
    if (marks>=80) {fsc_Grades <- rbind(fsc_Grades,c("A Grade"),stringsAsFactors=FALSE)}
    else if (marks>=70) {fsc_Grades <- rbind(fsc_Grades,c("B Grade"),stringsAsFactors=FALSE)}
    else {fsc_Grades <- rbind(fsc_Grades,c("C Grade"),stringsAsFactors=FALSE)}
  }
  else if (is.na(marks)){
    fsc_Grades <- rbind(fsc_Grades,c(NA),stringsAsFactors=FALSE)
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

Probability_table_for_Question1 = Probabilities(fsc_Grades,Combine_offer_Nts_NU,"Barplot for Grades in Fsc/A-level and Selection in Nuces")
print(Probability_table_for_Question1)

