
#import data(change file name)
SBC_Programs <- read_csv("Downloads/SBC Programs Inventory 2 - Masterlist Reformat (5).csv", 
                         +     na = "NA")
View(SBC_Programs)

#function that returns the format Kumu needs for the "TO" column
RPT <- function(type, num, start){
  if (num==0){
    return("")
  }
  else { 
    str1 = ""
    for (i in start:(num+start)) {
      if (i == start) {
        new_str = paste(type, as.character(i)) 
        str1 = paste(str1, new_str)
      }
      if (i != start) {
        new_str = paste(" | ", type, as.character(i)) 
        str1 = paste(str1, new_str)
      }
    }
    return (str1)
  }
}

#make unknown numbers of people 0s (they just won't show up in the diagram)
A1 <- SBC_Programs
A1$EI1_number[A1$EI1_number==""] <- 0
A1$EI2_number[A1$EI2_number==""] <- 0
A1$EI3_number[A1$EI3_number==""] <- 0
A1$EI4_number[A1$EI4_number==""] <- 0
A1$EI5_number[A1$EI5_number==""] <- 0
A1$EI6_number[A1$EI6_number==""] <- 0
A1$EI7_number[A1$EI7_number==""] <- 0
A1$EI8_number[A1$EI8_number==""] <- 0
A1$EI9_number[A1$EI9_number==""] <- 0

#emtpy dataframe
A2 <- data.frame(matrix(NA, nrow = 0, ncol = (11)))
colnames(A2) <- c("FROM", "TO", colnames(A1)[1:9])
right.names <- c("FROM", "TO", colnames(A1[1:9]))

#fill A2 dataframe with the Kumu formatted version of the original sheet
for (i in 1:nrow(A1)){
  if (A1[i, "EI_type1"] != "na") {
    v1.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI_type1[i]), "EI1_number"]))
    to <- as.character(RPT(A1[i, "EI_type1"], as.integer(A1[i,"EI1_number"]), (v1.start+1)))
    v1 <- c("FROM" = "SBC", "TO" = to, A1[i,1:9])
    A2 <- rbind(A2, v1)
  }
  if (A1[i, "EI_type2"] != "na") {
    v2.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI_type2[i]), "EI1_number"]))
    to2 <- as.character(RPT(A1[i, "EI_type2"], as.integer(A1[i,"EI2_number"]), (v2.start+1)))
    v2 <- c("SBC", to2, c(A1[i,1:5]), A1[i,10:13])
    names(v2) <- right.names
    A2 <- rbind(A2, v2)
  }
  if (A1[i, "EI3_type"] != "na") {
    v3.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI3_type[i]), "EI1_number"]))
    to3 <- as.character(RPT(A1[i, "EI3_type"], as.integer(A1[i,"EI3_number"]), (v3.start+1)))
    v3 <- c("SBC", to3, c(A1[i,1:5]), A1[i,14:17])
    names(v3) <- right.names
    A2 <- rbind(A2, v3)
  }
  if (A1[i, "EI4_type"] != "na") {
    v4.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI4_type[i]), "EI1_number"]))
    to4 <- as.character(RPT(A1[i, "EI4_type"], as.integer(A1[i,"EI4_number"]), (v4.start+1)))
    v4 <- c("SBC", to4, c(A1[i,1:5]), A1[i,18:21])
    names(v4) <- right.names
    A2 <- rbind(A2, v4)
  }
  if (A1[i, "EI5_type"] != "na") {
    v5.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI5_type[i]), "EI1_number"]))
    to5 <- as.character(RPT(A1[i, "EI5_type"], as.integer(A1[i,"EI5_number"]), (v5.start+1)))
    v5 <- c("SBC", to5, c(A1[i,1:5]), A1[i,22:25])
    names(v5) <- right.names
    A2 <- rbind(A2, v5)
  }
  if (A1[i, "EI6_type"] != "na") {
    v6.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI6_type[i]), "EI1_number"]))
    to6 <- as.character(RPT(A1[i, "EI6_type"], as.integer(A1[i,"EI6_number"]), (v6.start+1)))
    v6 <- c("SBC", to6, c(A1[i,1:5]), A1[i,26:29])
    names(v6) <- right.names
    A2 <- rbind(A2, v6)
  }
  if (A1[i, "EI7_type"] != "na") {
    v7.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI7_type[i]), "EI1_number"]))
    to7 <- as.character(RPT(A1[i, "EI7_type"], as.integer(A1[i,"EI7_number"]), (v7.start+1)))
    v7 <- c("SBC", to7, c(A1[i,1:5]), A1[i,30:33])
    names(v7) <- right.names
    A2 <- rbind(A2, v7)
  }
  if (A1[i, "EI8_type"] != "na") {
    v8.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI8_type[i]), "EI1_number"]))
    to8 <- as.character(RPT(A1[i, "EI8_type"], as.integer(A1[i,"EI8_number"]), (v8.start+1)))
    v8 <- c("SBC", to8, c(A1[i,1:5]), A1[i,34:37])
    names(v8) <- right.names
    A2 <- rbind(A2, v8)
  }
  if (A1[i, "EI9_type"] != "na") {
    v9.start <- sum(as.integer(A2[which(A2$EI_type1 == A1$EI9_type[i]), "EI1_number"]))
    to9 <- as.character(RPT(A1[i, "EI9_type"], as.integer(A1[i,"EI9_number"]), (v9.start+1)))
    v9 <- c("SBC", to9, c(A1[i,1:5]), A1[i,34:37])
    names(v9) <- right.names
    A2 <- rbind(A2, v9)
  }
}

#data cleaning/reformat for tables#
A2[A2$EI1_interaction_type == "Donor | Event Presenter", "EI1_interaction_type"] <- "Donor"
#delete resourcED survey participants
A2 <- A2[!(A2$EI1_interaction_type =="Surveyee"),] 
#fix capitalization
A2[A2$EI1_interaction_type == "Event attendee", "EI1_interaction_type"] <- "Event Attendee"
#remove rows with missing information
A2 <- A2[!(A2$TO==""),]
#Fete
A2[A2$EI_type1 == "Donor", "EI_type1"] <- "Public"

#export dataset
write.csv(A2, "A24.csv")

##table with number of each type of engagement##
types.engagement <- unique(A2$EI1_interaction_type)
A3 <- data.frame(matrix(NA, nrow = 0, ncol = (2)))
A2$EI1_number <- as.integer(A2$EI1_number)
for (i in 1:length(types.engagement)){
  a <- sum(A2[A2$EI1_interaction_type == types.engagement[i],"EI1_number"])
  A3[i,2] <- a
  A3[i,1] <- types.engagement[i]
}

##table with number of each type of person##
types.person <- unique(A2$EI_type1)
A4 <- data.frame(matrix(NA, nrow = 0, ncol = (2)))
for (i in 1:length(types.person)){
  b <- sum(A2[A2$EI_type1 == types.person[i],"EI1_number"])
  A4[i,2] <- b
  A4[i,1] <- types.person[i]
}

partners <- A2[A2$EI1_interaction_type == "Partner",]
customers <- sum(A2[A2$EI1_interaction_type == "Customer","EI1_number"])

##table with cross listed numbers of type of people and type of engagement##
A5 <-data.frame(matrix(NA, nrow = 0, ncol = length(types.engagement)))
colnames(A5) <- types.engagement
rownames(A5) <- types.person
for (i in 1:length(types.person)){
  B1 <- A2[A2$EI_type1 == types.person[i],]
  row <- c()
  for (n in 1:length(types.engagement)){
    number <- sum(B1[B1$EI1_interaction_type == types.engagement[n],"EI1_number"])
    row <- c(row, number)
  }
  A5[i,]<-row
} 
