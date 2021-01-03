


# Question 4. Students who have passed the NTS exam instead of the NU test have a better chance of joining NUCES?

# Finding the robability of the students who have selected in the NUCES by NTS test
probNts = sum(!is.na(Nts_Admission_Offer))/(sum(!is.na(Nts_Admission_Offer))+sum(!is.na(NU_Admission_Offer)))

# Finding the robability of the students who have selected in the NUCES by NU test
probNU = sum(!is.na(NU_Admission_Offer))/(sum(!is.na(Nts_Admission_Offer))+sum(!is.na(NU_Admission_Offer)))


# COmbine Matrix of the NTS and NU probabilities
Probalities = c(Probility_Nts=probNts,Probility_NU=probNU)
print(Probalities)

# Plotting the probailities in a barplot
barplot(Probalities)
