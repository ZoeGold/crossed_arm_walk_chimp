### R script for Goldsborough et al. Crossed arm walk study ###

require(ggplot2)
require(dplyr)
require(reshape2)

### FREQUENCY OF CAW #### 
setwd("~/Research/Projects/Burgers Zoo (2017-2020)/Crossed arm walk (2020)/crossed_arm_walk")

CAW <- read.csv("Instances CAW.csv", row.names = NULL)
# Table 1: Frequency of CAW per ID
ftable(CAW$ID1)

# comparing CAW frequency indoor with outdoor
CAWInOut <- as.data.frame(as.matrix(ftable(CAW$ID1, CAW$in_out)))
CAWInOut$in_rate <- CAWInOut$`in`/52.52 # average amount of hours observed indoor
CAWInOut$out_rate <- CAWInOut$`out`/79.69 # average amount of hours observed outdoor
t.test(CAWInOut$in_rate, CAWInOut$out_rate, paired = TRUE)

# Average rate of CAW with Period 1 split into three portions of equal length (Nov-Jan, Feb-Mar, Apr-Jun)
# Counts obtained from CAW data set
# Hours observed per individual extracted manually from full dataset
CAW_Graph <- read.csv("Data for CAW Frequency Graph.csv")

CAW_Graph$rate <- as.numeric(CAW_Graph$rate)
CAW_Graph$period <- factor(CAW_Graph$period, labels = c("Nov-Jan", "Feb-Mar", "Apr-Jun"))
#too busy, remove individuals that showed it less than 2 times
CAW_GraphG <- subset(CAW_Graph, CAW_Graph$Individual == "Moni" | CAW_Graph$Individual == "Moniek" | CAW_Graph$Individual == "Morami"| CAW_Graph$Individual == "Roosje" |
                             CAW_Graph$Individual == "Tushi") 

#png("CAW Frequency Graph.png", width = 7, height = 6, units = 'in', res = 300)
#pdf(file = "CAW Frequency Graph.pdf", width = 7, height = 6)
ggplot(data = CAW_GraphG, aes(x = period, y = rate, group = Individual, linetype = Individual, shape = Individual)) + geom_line() + geom_point(size = 3) + scale_shape_manual(values = 0:10) +
  theme_bw() + labs(y = "Rate of crossed-arm walk (per hour)",x = "Period 1") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(face = "italic", size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title =  element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"))
#dev.off()

#### SOCIAL INTEGRATION ####
### Period 1
## Grooming
Groom_P1 <- read.csv("Grooming Globals Period 1.csv")
GroomGiven_P1 <- Groom_P1 %>% count(ID1)
GroomReceived_P1 <- Groom_P1 %>% count(ID2)

inds <- data.frame(ID = (sort(unique(Groom_P1$ID1)))) 
inds$HrsObs_P1<- c(118.6333333,	118.5333333,	118.6333333,	118.6333333,	118.5833333,	118.6333333,	116.6333333,	118.5333333,	118.6333333,	118.6333333,	118.6333333,	118.5333333,	118.6333333,	118.6333333,	115.6333333)
inds$ScanPoints_P1 <- c(805,	804,	805,	805,	805,	805,	792,	805,	805,	805,	805,	804,	805,	805,	785)
# scan points and hours observed manually calculated from full dataset by subtracting time marked not visible from total observation times
inds$GroomGiven_P1 <- GroomGiven_P1$n/inds$HrsObs_P1
inds$GroomReceived_P1 <- GroomReceived_P1$n/inds$HrsObs_P1

## Proximity
Prox_P1 <- read.csv("Proximity Scans Period 1.csv")
Prox_P1_1 <- Prox_P1 %>% count(ID1) 
Prox_P1_2 <- Prox_P1 %>% count(ID2)
inds$Prox_P1 <- (Prox_P1_1$n + Prox_P1_2$n)/inds$ScanPoints_P1

## Calculate CSI
inds$CSI_P1 <- (((inds$GroomGiven_P1/(median(inds$GroomGiven_P1)))+(inds$GroomReceived_P1/(median(inds$GroomReceived_P1)))+
                   (inds$Prox_P1/(median(inds$Prox_P1))))/3)

### Period 2
inds$HrsObs_P2 <- 32 # everyone always visible 
inds$ScanPoints_P2 <- 32*5 #5 scans per observation hour

## Grooming
Data_P2 <- read.csv("Proximity & Grooming Period 2.csv")
Data_P2$Dyad <- ifelse(Data_P2$ID2 < Data_P2$ID1, paste(Data_P2$ID2, Data_P2$ID1, sep = "-"), paste(Data_P2$ID1, Data_P2$ID2, sep = "-"))
Groom_P2 <- subset(Data_P2, Data_P2$behavior == "groom")
# to make dichotomous per hour
Groom_P2D <- Groom_P2[!duplicated(Groom_P2[c("hour", "Dyad")]),]

GroomGiven_P2 <- Groom_P2D %>% count(ID1)
GroomReceived_P2 <- Groom_P2D %>% count(ID2)
inds$GroomGiven_P2 <- GroomGiven_P2$n/inds$HrsObs_P2
inds$GroomReceived_P2 <- GroomReceived_P2$n/inds$HrsObs_P2

## Proximity
Prox_P2 <- subset(Data_P2, Data_P2$behavior == "prox")
Prox_P2_1 <- Prox_P2 %>% count(ID1)
Prox_P2_2 <- Prox_P2 %>% count(ID2)
inds$Prox_P2 <- (Prox_P2_1$n + Prox_P2_2$n)/inds$ScanPoints_P2

## Calculate CSI
inds$CSI_P2 <- (((inds$GroomGiven_P2/(median(inds$GroomGiven_P2)))+(inds$GroomReceived_P2/(median(inds$GroomReceived_P2)))+
                    (inds$Prox_P2/(median(inds$Prox_P2))))/3)

inds$CSI_change <- inds$CSI_P2- inds$CSI_P1

CSI <- inds[,c("ID", "CSI_P1", "CSI_P2")]
CSI_bp <- melt(CSI, id.vars = "ID")
CSI_bp$type <- c(rep("Period 1",15), rep("Period 2", 15))

CSI_MonEr <- subset(CSI_bp, CSI_bp$ID == "Moni" | CSI_bp$ID == "Erika")

#png("CSI Comparison Boxplot.png", width = 7, height = 6, units = 'in', res = 300)
#pdf(file = "CSI Comparison Boxplot.pdf", width = 7, height = 6)
ggplot()+  geom_boxplot(data = CSI_bp, aes(y = value, x = type), width = 0.5) + geom_point(data = CSI_MonEr, aes(y = value, x =type , shape = ID), size = 3) + 
  geom_line(data = CSI_MonEr, aes(y = value, x = type, group = ID), linetype = "dashed")  + geom_hline(yintercept = 1, linetype = "dotted") +
  theme_bw() + labs(x = "",y = "CSI score") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.title.x = element_text(colour= "black", size = 12),
        axis.title.y = element_text(colour= "black", size = 12),
        legend.text = element_text(colour= "black", size = 12),
        legend.title = element_text(colour = "black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))
#dev.off()

#### Preferred partners
# subset with all individuals aside from Moni
M_inds <- inds[-9, c("ID", "HrsObs_P1", "HrsObs_P2")]
## Period 1
M_Give_P1 <- subset(Groom_P1, Groom_P1$ID1 == "Moni")
M_GiveGr_P1 <- M_Give_P1 %>% count(ID2)
M_GiveGr_P1 <- M_GiveGr_P1 %>% rename(GG_P1 = n)
M_inds <- left_join(M_inds, M_GiveGr_P1, by = c("ID" = "ID2"))
M_inds[is.na(M_inds)] <- 0
M_inds$Giverate_P1 <- M_inds$GG_P1/M_inds$HrsObs_P1

M_Receive_P1 <- subset(Groom_P1, Groom_P1$ID2 == "Moni")
M_ReceiveGr_P1 <- M_Receive_P1 %>% count(ID1)
M_ReceiveGr_P1 <- M_ReceiveGr_P1 %>% rename(RG_P1 = n)
M_inds <- left_join(M_inds, M_ReceiveGr_P1, by = c("ID" = "ID1"))
M_inds[is.na(M_inds)] <- 0
M_inds$Receiverate_P1 <- M_inds$RG_P1/M_inds$HrsObs_P1

## Period 2
M_Give_P2 <- subset(Groom_P2D, Groom_P2D$ID1 == "Moni")
M_GiveGr_P2 <- M_Give_P2 %>% count(ID2)
M_GiveGr_P2 <- M_GiveGr_P2 %>% rename(GG_P2 = n)
M_inds <- left_join(M_inds, M_GiveGr_P2, by = c("ID" = "ID2"))
M_inds[is.na(M_inds)] <- 0
M_inds$Giverate_P2 <- M_inds$GG_P2/M_inds$HrsObs_P2

M_Receive_P2 <- subset(Groom_P2D, Groom_P2D$ID2 == "Moni")
M_ReceiveGr_P2 <- M_Receive_P2 %>% count(ID1)
M_ReceiveGr_P2 <- M_ReceiveGr_P2 %>% rename(RG_P2 = n)
M_inds <- left_join(M_inds, M_ReceiveGr_P2, by = c("ID" = "ID1"))
M_inds[is.na(M_inds)] <- 0
M_inds$Receiverate_P2 <- M_inds$RG_P2/M_inds$HrsObs_P2