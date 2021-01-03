# Question 6. Is there any correlation between the Matric/O-level/F.Sc./A-Level and the NU test score? (students with the NTS test score can be ignored here)


# Making of list of Matriculation marks
Matriculation_Obtained_marks =admission$Obtained...16
Matriculation_Total_marks =admission$Total...15

# Making of list of Fsc marks
fsc_Obtained_marks =admission$Obtained...20
fsc_Total_marks =admission$Total...19
NUTest_BBA = admission$BBA
NUtest_BS = admission$BS...24

# Calculating the percentages of matriculation marks
Matriculation_Percentages = c(as.numeric((Matriculation_Obtained_marks /Matriculation_Total_marks)*100))

# Calculating the percentages of Fsc marks
fsc_Percentages = c(as.numeric((fsc_Obtained_marks /fsc_Total_marks)*100))


# Correlation Between the matric percentages and the NUtest BBA marks 
cor.test(Matriculation_Percentages,NUTest_BBA)

# Correlation Between the matric percentages and the NUtest BS marks 
cor.test(Matriculation_Percentages,NUtest_BS)

# Correlation Between the FSC percentages and the NUtest BBA marks 
cor.test(fsc_Percentages,NUTest_BBA)

# Correlation Between the FSC percentages and the NUtest BS marks 
cor.test(fsc_Percentages,NUtest_BS)



