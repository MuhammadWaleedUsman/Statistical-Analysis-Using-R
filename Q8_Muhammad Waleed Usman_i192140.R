

# Question 8. Which region’s Intermediate board performed best in the NU test? You can categorize the boards into five regions (4 provinces + Federal). Is there any correlation between the Board and the performance in the NU test?


# Boards and marks of the candidates
Board_Fsc = admission$Board...17
NU_BS_Test_Marks = admission$BS...24
NU_BBA_Test_Marks= admission$BBA

# PRovince wise dataframe with NUTEST BS and BBA
prov <- data.frame(Board=Board_Fsc, BS=NU_BS_Test_Marks,BBA= NU_BBA_Test_Marks)
pro <- data.frame(Province= revalue( prov$Board,
                                     c("Karachi"="sindh","Sindh Technical"="sindh", "Sialkot"="punjab", "London"="Others", "Armed Forces"="Others" ,"Kohat"="kpk"  ,"Bannu"= "kpk", "Malakand Div" = "kpk","Sawat"="kpk","D.I.Khan"="kpk", "D.G.Khan"="punjab","Mardan"="kpk"  ,"Quetta"="balochistan", "Punjab Technical"="punjab","Sargodha"="punjab" ,"Peshawar"="kpk", "Mirpur"="kpk", "Larkana"="sindh","Faislabad"="punjab", "Agha Khan"="sindh", "Abbotabad"="kpk" , "Hyderabad"="sindh","Sukkur" = "sindh",  "Cambridge"= "Federal" , "Mirpur Khas" = "sindh" ,"Sahiwal"="punjab",  "Gujranwala"="punjab", "Rawalpindi"="punjab", "Lahore"= "punjab", "Bahawalpur"="punjab", "Multan"="punjab")), B=prov$BS, C=prov$BBA)


# Replacing the nulls with the 0 in the dataframe
pro$B <- replace(pro$B, is.na(pro$B),0)
pro$C <- replace(pro$C, is.na(pro$C),0)
pro$B <- data.frame(a=ifelse(pro$B >=45, "A Grade",  ifelse(pro$B >35,"B Grade",ifelse(pro$B >20, "C Grade", ifelse(pro$B>10,"D Grade","F Grade")))),stringsAsFactors = FALSE)
pro$C <- data.frame(a=ifelse(pro$C >=45, "A Grade",  ifelse(pro$C >35,"B Grade",ifelse(pro$C >20, "C Grade", ifelse(pro$C>10,"D Grade","F Grade")))),stringsAsFactors = FALSE)

# Calculating the Two way probability tables
probability = Probabilities(pro$Province,pro$C,"Intermediate board perfomance in the NU test Province wise")
print(probability)
