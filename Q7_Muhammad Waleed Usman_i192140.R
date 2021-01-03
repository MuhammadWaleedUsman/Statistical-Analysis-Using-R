

# Question 7. Which Intermediate Boards were successful in getting admission in NU?

# Board of the itermediate candiates
Board_Fsc = data.frame(Board=admission$Board...17 , Offer = Combine_offer_Nts_NU )
Board_Fsc_Filtered <- data.frame(Offer=ifelse(((is.na(Board_Fsc$ntsOffer) & is.na(Board_Fsc$Board) )| is.na(Board_Fsc$ntsOffer) | is.na(Board_Fsc$Board)) , NA ,"selected"), Board = Board_Fsc$Board)

# Removing the NA values from the boards
Removing_NA <- data.frame(D=ifelse( (!is.na(Board_Fsc_Filtered$Board) ) & (!is.na(Board_Fsc_Filtered$Offer)) , as.character(Board_Fsc_Filtered$Board),NA))
Romoving_NA <- na.omit(Removing_NA)

# Aggregate Table for the intermediate boards according to the grades of the students in NU test
Aggregate_Table_Intermediate = data.frame(aggregate(numeric(nrow(Romoving_NA)), Romoving_NA[c("D")], length))
Aggregate_Table_Intermediate_sorted <- Aggregate_Table_Intermediate[with(Aggregate_Table_Intermediate, order(x,decreasing = TRUE)),]
View(Aggregate_Table_Intermediate_sorted)
