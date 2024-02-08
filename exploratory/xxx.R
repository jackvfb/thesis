library(tidyverse)
library(PAMpal)
library(seewave)

data <- data.frame(db = c("C:/Users/jackv/Documents/thesis-data/TrainingData/db/CalCURSeas_Harbor - Copy.sqlite3",
                          "C:/Users/jackv/Documents/thesis-data/TrainingData/db/OPPS_008_Harbor - Copy.sqlite3",
                          "C:/Users/jackv/Documents/thesis-data/TrainingData/db/OPPS_010_Harbor - Copy.sqlite3",
                          "C:/Users/jackv/Documents/thesis-data/TrainingData/db/PASCAL_Dalls_TowedArray - Copy.sqlite3"),
                   UID = c(2812000015, 308000031, 308000030, 2000156),
                   species = c("pp", "pp", "pp", "pd"))


pps <- PAMpalSettings(db=data$db,
                      binaries="C:/Users/jackv/Documents/thesis-data/TrainingData/binaries",
                      sr_hz = 384000,
                      filterfrom_khz = 100,
                      filterto_khz = 160,
                      winLen_sec = 0.0025)

study <- processPgDetections(pps, mode="db", id="template clicks")

templates <- sapply(data$UID, \(x) getBinaryData(study, x))


#pick the UID of a click at random
#uid <- sample(getClickData(ev)$UID, 1)
# [1] "3641000096" is a good one
uid <- 3641000096


#get the binary data needed to plot the waveforms of the clicks in the event
b <- getBinaryData(myStudy, UID = uid)
b <- b[[1]]
#plot channel 2 wave
plot(b$wave[,2], type = 'l', xlab = "Time", ylab = "Amplitude")

#get the matrix needed to plot the click spectra in the event
p <- calculateAverageSpectra(myStudy,
                             evNum = id(ev),
                             channel = 2,
                             filterfrom_khz = 110,
                             filterto_khz = 150,
                             plot = FALSE)

#get index of the column for uid
i <- match(uid, p$UID)

#Make spectrum
df <- data.frame(value = p$allSpec[,i], freq = p$freq)
ggplot(df, aes(freq/1000, value))+
  geom_line() +
  scale_x_continuous(breaks = seq(0, 200, by = 50)) +
  labs(x = "Frequency (kHz)",
       y = "Normalized Magnitude (dB)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
