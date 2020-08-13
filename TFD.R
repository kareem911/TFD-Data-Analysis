

library(ggplot2)

#normalizing function
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

#removing outliers function
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


BastlerinManual$Type <- as.factor(BastlerinManual$Type)  #BastlerinManual is the dataframe name
CocacolaManual$Type <- as.factor(CocacolaManual$Type)    #CocacolaManual is the dataframe name
FriscoManual$Type <- as.factor(FriscoManual$Type)        #FriscoManual is the dataframe name
KoaliumManual$Type <- as.factor(KoaliumManual$Type)      #KoaliumManual is the dataframe name
PaulManual$Type <- as.factor(PaulManual$Type)            #PaulManual is the dataframe name

CocacolaManual$Type <- factor(CocacolaManual$Type , levels=c("HFLV", "LFLV", "HFSV", "LFSV"))                  # Arranging levels in all dataframes for easier comparison to graphs
FriscoManual$Type <- factor(FriscoManual$Type , levels=c("HFLV", "LFLV", "HFSV", "LFSV"))
BastlerinManual$Type <- factor(BastlerinManual$Type , levels=c("HFLV", "LFLV", "HFSV", "LFSV"))
PaulManual$Type <- factor(PaulManual$Type , levels=c("HFLV", "LFLV", "HFSV", "LFSV"))
KoaliumManual$Type <- factor(KoaliumManual$Type , levels=c("HFLV", "LFLV", "HFSV", "LFSV"))




CocacolaManual = CocacolaManual[CocacolaManual$Cocacola!= 0, ]                                                 # Removing zero values that occured due to technical problems or otherwise
PaulManual = PaulManual[PaulManual$Paul!= 0, ]
FriscoManual = FriscoManual[FriscoManual$Frisco!= 0, ]
BastlerinManual = BastlerinManual[BastlerinManual$Bastlerin!= 0, ]
KoaliumManual = KoaliumManual[KoaliumManual$Koalium!= 0, ]

BastlerinManual$TFD_clean<-remove_outliers(BastlerinManual$Bastlerin)                                          # Calling remove outilers function to remove outliers values
BastlerinManual$TFD_normclean<-normalize(BastlerinManual$TFD_clean)                                            # Normalizing to remove the speed factor
ggplot(BastlerinManual, aes(, y=TFD_normclean, fill=Type)) +                                                   # Plotting a boxplot too visualize data
  geom_boxplot()

CocacolaManual$TFD_clean<-remove_outliers(CocacolaManual$Cocacola) 
CocacolaManual$TFD_normclean<-normalize(CocacolaManual$TFD_clean)
ggplot(CocacolaManual, aes(, y=TFD_normclean, fill=Type)) + 
  geom_boxplot()



FriscoManual$TFD_clean<-remove_outliers(FriscoManual$Frisco)
FriscoManual$TFD_normclean<-normalize(FriscoManual$TFD_clean)
ggplot(FriscoManual, aes(, y=TFD_normclean, fill=Type)) + 
  geom_boxplot()

KoaliumManual$TFD_clean<-remove_outliers(KoaliumManual$Koalium)
KoaliumManual$TFD_normclean<-normalize(KoaliumManual$TFD_clean)
ggplot(KoaliumManual, aes(, y=TFD_normclean, fill=Type)) + 
  geom_boxplot()


  
PaulManual$TFD_clean<-remove_outliers(PaulManual$Paul)
PaulManual$TFD_normclean<-normalize(PaulManual$TFD_clean)
ggplot(PaulManual, aes(, y=TFD_normclean, fill=Type)) + 
  geom_boxplot()

 --------------------------------------------
  
PaulManual$Freq <- NA                                                                                                      # Adding column holding either high frequency or low frequrency regardless the vowel type
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("HF", "HF", "LF", "LF")
PaulManual$Freq <- values[match(PaulManual$Type, index)]
ggplot(PaulManual, aes(, y=TFD_normclean, fill=Freq)) + 
  geom_boxplot()


BastlerinManual$Freq <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("HF", "HF", "LF", "LF")
BastlerinManual$Freq <- values[match(BastlerinManual$Type, index)]
ggplot(BastlerinManual, aes(, y=TFD_normclean, fill=Freq)) + 
  geom_boxplot()

CocacolaManual$Freq <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("HF", "HF", "LF", "LF")
CocacolaManual$Freq <- values[match(CocacolaManual$Type, index)]
ggplot(CocacolaManual, aes(, y=TFD_normclean, fill=Freq)) + 
  geom_boxplot()

FriscoManual$Freq <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("HF", "HF", "LF", "LF")
FriscoManual$Freq <- values[match(FriscoManual$Type, index)]
ggplot(FriscoManual, aes(, y=TFD_normclean, fill=Freq)) + 
  geom_boxplot()

KoaliumManual$Freq <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("HF", "HF", "LF", "LF")
KoaliumManual$Freq <- values[match(KoaliumManual$Type, index)]
ggplot(KoaliumManual, aes(, y=TFD_normclean, fill=Freq)) + 
  geom_boxplot()

----------------------------------------------
  
  PaulManual$Vowel <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")                                                                                                   # Adding column holding either long vowel or short vowel regardless the frequency
values <- c("LV", "SV", "LV", "SV")
PaulManual$Vowel <- values[match(PaulManual$Type, index)]
ggplot(PaulManual, aes(, y=TFD_normclean, fill=Vowel)) + 
  geom_boxplot()


BastlerinManual$Vowel <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("LV", "SV", "LV", "SV")
BastlerinManual$Vowel <- values[match(BastlerinManual$Type, index)]
ggplot(BastlerinManual, aes(, y=TFD_normclean, fill=Vowel)) + 
  geom_boxplot()

CocacolaManual$Vowel <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("LV", "SV", "LV", "SV")
CocacolaManual$Vowel <- values[match(CocacolaManual$Type, index)]
ggplot(CocacolaManual, aes(, y=TFD_normclean, fill=Vowel)) + 
  geom_boxplot()

FriscoManual$Vowel <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("LV", "SV", "LV", "SV")
FriscoManual$Vowel <- values[match(FriscoManual$Type, index)]
ggplot(FriscoManual, aes(, y=TFD_normclean, fill=Vowel)) + 
  geom_boxplot()

KoaliumManual$Vowel <- NA
index <- c("HFLV", "HFSV", "LFLV", "LFSV")
values <- c("LV", "SV", "LV", "SV")
KoaliumManual$Vowel <- values[match(KoaliumManual$Type, index)]
ggplot(KoaliumManual, aes(, y=TFD_normclean, fill=Vowel)) + 
  geom_boxplot()

