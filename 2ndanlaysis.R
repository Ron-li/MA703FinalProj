library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)

setwd("/Users/rongli/Documents/BU/MA703/FinalProject/data")

info <- read.table("metadata_primaryschool.txt")

# see file names
#unzip("copresence-LyonSchool.zip", list = TRUE)

# unzip
unz <- unzip("copresence-LyonSchool.zip", "copresence-LyonSchool.edges")

# quick look : looks like edge list
#readLines(unz, n=10)
dat <- read.table(unz, skip=0, sep=" ")
rm(unz)

# EDA
####################### time range ###########################

head(dat)
str(dat)
colnames(dat) <- c("P1", "P2", "time")
summary(dat$time) # day1: 9:30:40 -- day2: 18:12:40

# dat %<>% distinct()
# dat2 <- dat
# dat2$id <- paste0(pmin(dat2$P1, dat2$P2), pmax(dat2$P1, dat2$P2))
# dat2 <- dat2[, c(4, 3)]
# dat2 %<>% distinct()
# rm(dat2)
# have verified no replicates in data

# load as a graph
# library(igraph)
# g <- graph_from_data_frame(dat)
# g
# plot(g, layout=layout_in_circle)
# too large to draw a graph

# let time minus 1 hour
dat$time <- dat$time - 1*60*60
dat$day <- ifelse(dat$time/60/60 < 24, 1, 2)

summary(dat$time) # day1: 8:30:40 -- day2: 17:12:40

summary(dat[dat$day == 1, "time"]) 
# day1 range from 8:30:40am to 17:17:20pm
summary(dat[dat$day == 2, "time"] - 24*60*60)
# day2 range from 8:33:20am to 17:12:40pm

######################### nodes info ###############################

## characteristics of vertices
colnames(info) <- c("id", "class", "gender")
info %>% group_by(class) %>% summarise(c = sum(id > 0))

day1 <- dat[dat$day == 1, ]
day2 <- dat[dat$day == 2, ]

info[info$id %in% unique(c(day1$P1, day1$P2)), ] %>% 
  group_by(class) %>% 
  summarise(count = sum(id > 0))
info[info$id %in% unique(c(day2$P1, day2$P2)), ] %>% 
  group_by(class) %>% 
  summarise(count = sum(id > 0))

########################## Contact Events ############################

dat2 <- dat
dat2$id <- paste0(pmin(dat2$P1, dat2$P2), pmax(dat2$P1, dat2$P2)) %>% as.numeric()
dat2 <- dat2[order(dat2$id, dat2$time), -4]

dat2$lag_time <- ifelse(dat2$time - lag(dat2$time, 1) == 20, 0, 1)
dat2$lag_time[1] <- 1
dat2[dat2$lag_time == 1, "lag_time"] <- 1:nrow(dat2[dat2$lag_time == 1,])
dat2$lag_time2 <- c(dat2[2:nrow(dat2), "lag_time"], 1)
dat2$period <- 1

df <- data.frame(lag_time = rle(dat2$lag_time)$values, 
                 lag_time2 = rle(dat2$lag_time2)$values, 
                 period = rle(dat2$lag_time)$lengths + rle(dat2$lag_time2)$lengths)
dat2[dat2$lag_time > 0 & dat2$lag_time2 == 0, "period"] <- df[df$lag_time > 0 & df$lag_time2 == 0, "period"]
dat2 <- dat2[dat2$lag_time > 0, ]
cont <- dat2[, -c(5, 6)] # 1963083 contacts in total
rm(df)
rm(dat2)
#save(cont, file = "contact.Rdata")

# calculate contacts in day1 and day2 separately is not good
# total contacts split into 2 days
cont1 <- cont[cont$time < 24*60*60, ]
nrow(cont1)# day1: 1005113 contacts
cont2 <- cont[cont$time > 24*60*60, ]
nrow(cont2)# day2: 957970 contacts
nrow(cont1) * 2 / length(unique(c(cont1$P1, cont1$P2))) # day1 average contacts: 8482
nrow(cont2) * 2 / length(unique(c(cont2$P1, cont2$P2))) # day2 average contacts: 7950


##################### duration of contacts ############################
summary(cont$period)
summary(cont$period) * 20
var(cont$period*20)
# average duration for each contact: 67.19seconds, max duration is 5940s = 99mins

nrow(cont[cont$period == 1, ]) / nrow(cont)
# 54.58% of the contacts lasts 20 seconds.
nrow(cont[cont$period <= 3, ]) / nrow(cont)
# 79.89% of the contacts lasts less than or equal to 1 min.
nrow(cont[cont$period > 15, ]) / nrow(cont)
# 3.75% of the contacts lasts more than 5 mins.

# the following is to find the distribution of contact durations:
duration <- cont %>% group_by(period) %>% summarise(sum = sum(period > 0))
duration$period <- 20 * duration$period
duration$frequency <- duration$sum / nrow(cont)

# The cumulative time for each pair
pairdur <- cont %>% group_by(id) %>% summarise(sum = sum(period) * 20)
pairdur <- pairdur %>% group_by(sum) %>% summarise(frequency = sum(id > 0))
colnames(pairdur) <- c("period", "sum")
pairdur$frequency <- pairdur$sum / sum(pairdur$sum)

# Log-log plot of the distribution of the contact durations and of 
# the cumulated duration of all the contacts two individuals i and 
# j have over two days
library(scales)
gg <- ggplot() + 
  geom_point(duration, mapping = aes(x = period, y = frequency)) + 
  geom_point(pairdur, mapping = aes(x = period, y = frequency), col = 'blue', shape = 0) + 
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) + 
  labs(title = "Log-log plot of the distibution of the contact durations") + 
  xlab("Duration(s)") + 
  ylab("Distribution")
gg + geom_vline(xintercept = 60, col = "red", linetype = "dashed") + 
  geom_vline(xintercept = 10*60, col = 'red', linetype = "dashed") + 
  geom_vline(xintercept = 60*60, col = 'red', linetype = "dashed") + 
  annotate(geom="text", x=80, y=0.00004, label="1 min",
           color="red") + 
  annotate(geom="text", x=580, y=0.00004, label="10 min",
           color="red") + 
  annotate(geom="text", x=60*60, y=0.00004, label="1 hour",
           color="red")
rm(gg)
rm(duration)
rm(pairdur)

# aggregated networks: two daily static network
network1 <- cont1 %>% group_by(id) %>% summarise(weight = sum(period)*20)
network1$P1 <- str_sub(network1$id, 1, 4) %>% as.numeric()
network1$P2 <- str_sub(network1$id, 5, 8) %>% as.numeric()
network1 <- network1[, c(3, 4, 2)]

network2 <- cont2 %>% group_by(id) %>% summarise(weight = sum(period)*20)
network2$P1 <- str_sub(network2$id, 1, 4) %>% as.numeric()
network2$P2 <- str_sub(network2$id, 5, 8) %>% as.numeric()
network2 <- network2[, c(3, 4, 2)]

# 1. mean degree of a node: degree is considered a centrality measure
# Centrality: related to the 'importance' of vertices
# local connectivity: degree distribution
# 2. average of local clustering coefficients

library(igraph)
timels <- unique(dat$time)
n_indi <- c()
average <- c()
clst_ave <- c()
for (i in 1:length(timels)){
  print(i)
  datnull <- dat[dat$time == timels[i], ]
  gnull <- graph_from_data_frame(datnull)
  n_indi[i] <- gorder(gnull)
  average[i] <- mean(degree(gnull))
  clst_ave[i] <- transitivity(gnull, type = "average")
}
rm(datnull, gnull)
df <- data.frame(time = timels, n = n_indi, degree = average)
write.csv(df, "degree.csv")

mean(df$degree)
mean(clst_ave)

g1 <- graph_from_data_frame(network1)
g2 <- graph_from_data_frame(network2)

mean(c(degree(g1), degree(g2)))
hist(c(degree(g1), degree(g2)))

# mean local clustering
transitivity(g1, type = "average")
transitivity(g2, type = "average")

# mean shortest path
mean_distance(g1)
mean_distance(g2)

#for each individual, calculate the Pearson correlation coefficients between:
# (i) the time spent for contact each day
# (ii) degree
for (i in 1:nrow(info)){
  print(i)
  datnull <- day1 %>% filter(P1 == info$id[i] | P2 == info$id[i])
  info$degree1[i] <- length(unique(c(datnull$P1, datnull$P2))) - 1
  info$durat1[i] <- length(unique(datnull$time)) * 20
  
  datnull <- day2 %>% filter(P1 == info$id[i] | P2 == info$id[i])
  info$degree2[i] <- length(unique(c(datnull$P1, datnull$P2))) - 1
  info$durat2[i] <- length(unique(datnull$time)) * 20
}
rm(datnull)

cor(info$durat1, info$durat2)
cor(info$degree1, info$degree2)

############################### Find R0 ###############################
# replicate time
dat$id <- paste0(pmin(dat$P1, dat$P2), pmax(dat$P1, dat$P2))
datnull <- dat
datnull$time <- datnull$time + 2*24*60*60
datrep <- rbind(dat[,-4], datnull[,-4])

# time list
timels <- unique(datrep$time)

# compute R0 for simulations
R0 <- c()
for (i in 1:500){
  print(i)
  filename <- paste0("./Simulation7/test", i, ".RData")
  load(filename)
  
  seed <- which(output[[1]]$status == "I")
  a <- lapply(output, function(x) x[, "status"]) %>% unlist()
  a <- matrix(a, nrow = 242)
  a <- cbind(info$id, a) %>% as.data.frame()
  
  b <- apply(a, MARGIN = 2, FUN = function(x) sum(x == "R"))
  t1 <- which(b >= 1)[1]
  
  a$firstE <- apply(a, MARGIN = 1, FUN = function(x) which(x == "E")[1])
  b <- a %>% filter(firstE <= t1) %>% select(V1, firstE)
  if (nrow(b) > 0) {
    b$V1 %<>% as.numeric()
    b$id <- paste0(pmin(b$V1, info[seed, "id"]), pmax(b$V1, info[seed, "id"]))
    b$firstE <- timels[b$firstE - 2]
    
    for (j in 1:nrow(b)){
      datnull <- datrep %>% filter(id == b$id[j], time == b$firstE[j])
      b$related[j] <- ifelse(nrow(datnull) == 1, 1, 0)
    }
    R0[i] <- sum(b$related)
  }
}

# save(R0, file = "./Simulation8/R0.RData")

######################### S7: correct some R0 #######################
# for simulation7
load("./Simulation7/R0.RData")
which(is.na(R0) == TRUE)

for (i in which(is.na(R0) == TRUE)){
  filename <- paste0("./Simulation7/test", i, ".RData")
  load(filename)
  
  datnull <- output[[6248]] %>% filter(status == "R")
  print(nrow(datnull))
}
# R0[243] needs to be corrected

load("./Simulation7/test243.RData")
seed <- which(output[[1]]$status == "I")
a <- lapply(output, function(x) x[, "status"]) %>% unlist()
a <- matrix(a, nrow = 242)
a <- cbind(info$id, a) %>% as.data.frame()

b <- apply(a, MARGIN = 2, FUN = function(x) sum(x == "R"))

a$firstE <- apply(a, MARGIN = 1, FUN = function(x) which(x == "E")[1])
b <- a %>% filter(is.na(firstE) == F) %>% select(V1, firstE)

b$V1 %<>% as.numeric()
b$id <- paste0(pmin(b$V1, info[seed, "id"]), pmax(b$V1, info[seed, "id"]))
b$firstE <- timels[b$firstE - 2]

for (j in 1:nrow(b)){
  print(j)
  datnull <- datrep %>% filter(id == b$id[j], time == b$firstE[j])
  b$related[j] <- ifelse(nrow(datnull) == 1, 1, 0)
}
print(sum(b$related))

R0[243] <- 13
R0[is.na(R0) == TRUE] <- 0

save(R0, file = "./Simulation7/R0.RData")

######################### S8: correct some R0 #######################
# for simulation8
load("./Simulation8/R0.RData")
which(is.na(R0) == TRUE)

for (i in which(is.na(R0) == TRUE)){
  filename <- paste0("./Simulation8/test", i, ".RData")
  load(filename)
  
  datnull <- output[[6248]] %>% filter(status == "R")
  print(nrow(datnull))
}
# no need for change

R0[is.na(R0) == TRUE] <- 0
save(R0, file = "./Simulation8/R0.RData")

######################## Analyze R0 ################################
load("./Simulation7/R0.RData")
mean(R0) # 19.51
sd(R0) # 9.54
hist(R0)
hist(R0, col = 'skyblue3', breaks = 20)

library(ggthemes)
ggplot() + 
  geom_histogram(aes(x = R0, y = ..density..), 
                 bins = 20, fill = "gray", colour = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(R0), 
                            sd = sd(R0)),
                colour = "salmon", size = 1.25) +
  theme_tufte() +
  theme(axis.line = element_line(size = 1),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  xlab("R0") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0, 50, 5))

load("./Simulation8/R0.RData")
mean(R0) # 24.61
sd(R0) # 12.35
hist(R0)
hist(R0, col = 'skyblue3', breaks = 30)

ggplot() + 
  geom_histogram(aes(x = R0, y = ..density..), 
                 bins = 25, fill = "gray", colour = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(R0), 
                            sd = sd(R0)),
                colour = "salmon", size = 1.25) +
  theme_tufte() +
  theme(axis.line = element_line(size = 1),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  xlab("R0") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0, 60, 5))

################### Analyze the final cases ########################
# see distribution of final cases of simulation7
load("./Simulation7/finalcase7.RData")
finalcase7 <- finalcase7[1:1000, ]
I7 <- finalcase7 %>% group_by(242-S) %>% summarise(frequency = sum(S > -1))
I7$frequency <- I7$frequency / 1000

load("./Simulation8/finalcase8.RData")
finalcase8 <- finalcase8[1:1000, ]
I8 <- finalcase8 %>% group_by(242-S) %>% summarise(frequency = sum(S > -1))
I8$frequency <- I8$frequency / 1000

gg <- ggplot() + 
  geom_point(I7, mapping = aes(x = `242 - S`, y = frequency)) + 
  geom_point(I8, mapping = aes(x = `242 - S`, y = frequency), col = 'red', shape = 0) + 
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) + 
  labs(title = "Distribution of the final number of cases") + 
  xlab("Number of final cases") + 
  ylab("Distribution")
gg

################# find cases over time #################

timenew <- c(30640, seq(9, 17, 0.5) * 60*60, 62240, 
             117200, seq(33, 41, 0.5) * 60 * 60, 148360)
timenew <- c(timenew, timenew + 48*60*60)
timenew <- which(timels %in% timenew)

cases <- data.frame()
for (i in 1:1000){
  print(i)
  filename <- paste0("./Simulation8/test", i, ".RData")
  load(filename)
  
  a <- lapply(output, function(x) x[, "status"]) %>% unlist()
  a <- matrix(a, nrow = 242)
  a <- cbind(info$id, a) %>% as.data.frame()
  
  b <- apply(a, MARGIN = 2, FUN = function(x) sum(x != "S"))
  b <- b[-1]
  
  cases[1:6248, i] <- b
}
matplot(cases, type = "b", pch=12, cex = 0.01, col = "lightgrey")
lines(x = rep(1565, 200), y = seq(1, 200, 1))
lines(x = rep(3124, 200), y = seq(1, 200, 1))
lines(x = rep(3124+1565, 200), y = seq(1, 200, 1))
save(cases, file = "./Simulation8/cases.RData")
