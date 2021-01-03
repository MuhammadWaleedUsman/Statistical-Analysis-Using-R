

# Question 3. Do the students who could not get the first preference of their choice of discipline failed to join NUCES?
Discipline_Preference = data.frame(pref= admission$Ist...9, stringsAsFactors = FALSE)
Admission_Status= data.frame(pref= admission$Discipline...37, stringsAsFactors = FALSE)
Compare_Admission_And_Discipline_pref = data.frame(ifelse(Discipline_Preference == Admission_Status,"Got Choice","Failed to get Choice"),stringsAsFactors = FALSE)
Numerate_compare <- table(Compare_Admission_And_Discipline_pref)
#numbertable[as.character("Got Choice")]
#numbertable[as.character("Failed to get Choice")]

# Finding the probaility of the preference
Probability_of_Preference = c(Numerate_compare[as.character("Failed to get Choice")]/(Numerate_compare[as.character("Got Choice")]+Numerate_compare[as.character("Failed to get Choice")]),Numerate_compare[as.character("Got Choice")]/(Numerate_compare[as.character("Got Choice")]+Numerate_compare[as.character("Failed to get Choice")]))
barplot(Probability_of_Preference)
print(Probability_of_Preference)
