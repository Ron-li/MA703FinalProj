library(dplyr)
library(magrittr)
library(ggplot2)

setwd("/Users/rongli/Documents/BU/MA703/FinalProject/data")

info <- read.table("metadata_primaryschool.txt")

# see file names
unzip("copresence-LyonSchool.zip", list = TRUE)


# unzip
unz <- unzip("copresence-LyonSchool.zip", "copresence-LyonSchool.edges")

# quick look : looks like edge list
readLines(unz, n=10)
dat <- read.table(unz, skip=0, sep=" ")

# EDA
head(dat)
str(dat)
colnames(dat) <- c("P1", "P2", "time")
summary(dat$time)

dat %<>% distinct()
dat2 <- dat
dat2$id <- paste0(pmin(dat2$P1, dat2$P2), pmax(dat2$P1, dat2$P2))
dat2 <- dat2[, c(4, 3)]
dat2 %<>% distinct()
rm(dat2)
# have verified no replicates in data

# load as a graph
# library(igraph)
# g <- graph_from_data_frame(dat)
# g
# plot(g, layout=layout_in_circle)
# too large to draw a graph

dat$time <- dat$time - 1*60*60
dat$day <- ifelse(dat$time/60/60 < 24, 1, 2)

summary(dat$time)
30640/60/60
148360/60/60 - 24

summary(dat[dat$day == 1, "time"]) 
# day1 range from 8:30:40am to 17:17:20pm
summary(dat[dat$day == 2, "time"] - 24*60*60)
# day2 range from 8:33:20am to 17:12:40pm

######################### Summary ###############################

## characteristics of vertices
colnames(info) <- c("id", "class", "gender")
info %>% group_by(class) %>% summarise(c = sum(id > 1))
info[info$id %in% unique(c(dat[dat$day == 1, "P1"], dat[dat$day == 1, "P2"])), ]%>% 
  group_by(class) %>% summarise(c = sum(id > 1))
info[info$id %in% unique(c(dat[dat$day == 2, "P1"], dat[dat$day == 2, "P2"])), ]%>% 
  group_by(class) %>% summarise(c = sum(id > 1))

########################## calculate contact ########################

dat2 <- dat
dat2$id <- paste0(pmin(dat2$P1, dat2$P2), pmax(dat2$P1, dat2$P2)) %>% as.numeric()
dat2 <- dat2[order(dat2$id, dat2$time), c(5, 1:3)]

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
cont <- dat2[, -c(5, 6)]
rm(df)
# save(cont, file = "contact.Rdata")

#####################################################################

# Summary
cont1 <- cont[cont$time < 24*60*60, ]
cont2 <- cont[cont$time > 24*60*60, ]
nrow(cont)
nrow(cont1) # day1 contacts
nrow(cont2) # day2 contacts
nrow(cont1) * 2 / length(unique(c(cont1$P1, cont1$P2))) # day1 average contacts
nrow(cont2) * 2 / length(unique(c(cont2$P1, cont2$P2))) # day1 average contacts

length(unique(cont1$id)) * 2 / length(unique(c(cont1$P1, cont1$P2)))
length(unique(cont2$id)) * 2 / length(unique(c(cont2$P1, cont2$P2)))

duration <- cont %>% group_by(period) %>% summarise(sum = sum(period > 0))
duration$period <- 20 * duration$period
duration$frequency <- duration$sum / nrow(cont)

durat1 <- cont1 %>% group_by(id) %>% summarise(period = sum(period) * 20)
sum(durat1$period) / nrow(durat1) / 60
durat1 <- durat1 %>% group_by(period) %>% summarise(frequency = sum(period > 0))
durat1$frequency <- durat1$frequency / sum(durat1$frequency)

durat2 <- cont2 %>% group_by(id) %>% summarise(period = sum(period) * 20)
sum(durat2$period) / nrow(durat2) / 60
durat2 <- durat2 %>% group_by(period) %>% summarise(frequency = sum(period > 0))
durat2$frequency <- durat2$frequency / sum(durat2$frequency)

library(scales)
gg <- ggplot() + 
  geom_point(duration, mapping = aes(x = period, y = frequency)) + 
  geom_point(durat1, mapping = aes(x = period, y = frequency), col = 'blue') + 
  geom_point(durat2, mapping = aes(x = period, y = frequency), col = 'purple') +
  scale_x_continuous(trans = log10_trans(),
                        breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_continuous(trans = log10_trans(),
                        breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x))) + 
  labs(title = "Log-log plot of the distibution of the contact durations") + 
  xlab("Duration(s)") + 
  ylab("Frequency")
gg + geom_vline(xintercept = 60, col = "red", linetype = "dashed") + 
  geom_vline(xintercept = 10*60, col = 'red', linetype = "dashed") + 
  geom_vline(xintercept = 60*60, col = 'red', linetype = "dashed") + 
  annotate(geom="text", x=80, y=0.00004, label="1 min",
           color="red") + 
  annotate(geom="text", x=580, y=0.00004, label="10 min",
           color="red") + 
  annotate(geom="text", x=60*60, y=0.00004, label="1 hour",
           color="red")

# duration
sum(duration[duration$period < 60, "frequency"])
sum(duration[duration$period > 5*60, "frequency"])

# accumulated duration in day1
sum(durat1[durat1$period > 10*60, "frequency"])
sum(durat1[durat1$period < 1*60, "frequency"])
sum(durat1[durat1$period > 60*60, "frequency"])


############## try to do the SEIR model #####################
# replicate time
datnull <- dat
datnull$time <- datnull$time + 2*24*60*60
datrep <- rbind(dat[,-4], datnull[,-4])

# time list
timels <- unique(datrep$time)
diff <- (timels - lag(timels))/20
diff[1] <- 1

library(extraDistr)
`%notin%` <- Negate(`%in%`)

########## Simulation1: beta = 15 * 10^(-5), but we use beta20 #############
beta <- 15 * (10^-5)
beta20 <- 1 - (1 - beta)^20
sigma <- 1 / (2*24*60*60/3) # 2 day
gamma <- 1 / (4*24*60*60/3) # 4 days

for (j in 1:372){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta20) == 0, "S", "E")
  }
  filename <- paste0("./Simulation1/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:372){
  print(j)
  filename <- paste0("./Simulation1/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase1 <- data.frame(S = S, E = E, I = I, R = R)
finalcase1
hist(242 - S)
save(finalcase1, file = "./Simulation1/finalcase1.RData")
load("./Simulation1/finalcase1.RData")
###########################################################################

########## Simulation2: beta = 3 * 10^(-4), but we use beta20 #############
beta <- 3 * (10^-4)
beta20 <- 1 - (1 - beta)^20
sigma <- 1 / (1*24*60*60/3) # 1 day
gamma <- 1 / (2*24*60*60/3) # 2 days

for (j in 1:346){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta20) == 0, "S", "E")
  }
  filename <- paste0("./Simulation2/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:346){
  print(j)
  filename <- paste0("./Simulation2/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase2 <- data.frame(S = S, E = E, I = I, R = R)
finalcase2
hist(242 - S)
save(finalcase2, file = "./Simulation2/finalcase2.RData")
load("./Simulation2/finalcase2.RData")
###########################################################################

################### Simulation3: beta = 15 * 10^(-5) ######################
beta <- 15 * (10^-5)
sigma <- 1 / (2*24*60*60/3) # 2 day
gamma <- 1 / (4*24*60*60/3) # 4 days

for (j in 1:406){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation3/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:406){
  print(j)
  filename <- paste0("./Simulation3/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase3 <- data.frame(S = S, E = E, I = I, R = R)
finalcase3
hist(242 - S)
save(finalcase3, file = "./Simulation3/finalcase3.RData")
load("./Simulation3/finalcase3.RData")
###########################################################################


##################### Simulation4: beta = 3 * 10^(-4) ######################
beta <- 3 * (10^-4)
sigma <- 1 / (1*24*60*60/3) # 1 day
gamma <- 1 / (2*24*60*60/3) # 2 days

for (j in 1:437){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation4/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:437){
  print(j)
  filename <- paste0("./Simulation4/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase4 <- data.frame(S = S, E = E, I = I, R = R)
finalcase4
hist(242 - S)
save(finalcase4, file = "./Simulation4/finalcase4.RData")
load("./Simulation4/finalcase4.RData")
###########################################################################

################## Simulation5: beta = 15*4 * 10^(-5) #####################
beta <- 15*4 * (10^-5)
sigma <- 1 / (2*24*60*60/3) # 2 day
gamma <- 1 / (4*24*60*60/3) # 4 days

for (j in 1:469){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation5/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:469){
  print(j)
  filename <- paste0("./Simulation5/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase5 <- data.frame(S = S, E = E, I = I, R = R)
finalcase5
hist(242 - S)
save(finalcase5, file = "./Simulation5/finalcase5.RData")
###########################################################################

##################### Simulation6: beta = 3*4 * 10^(-4) ######################
beta <- 3*4 * (10^-4)
sigma <- 1 / (1*24*60*60/3) # 1 day
gamma <- 1 / (2*24*60*60/3) # 2 days

for (j in 1:460){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation6/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:460){
  print(j)
  filename <- paste0("./Simulation6/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase6 <- data.frame(S = S, E = E, I = I, R = R)
finalcase6
hist(242 - S)
save(finalcase6, file = "./Simulation6/finalcase6.RData")
###########################################################################

################## Simulation7: beta = 15*4 * 10^(-5) #####################
beta <- 15*4 * (10^-5)
sigma <- 1 / (2*24*60*60/20) # 2 day
gamma <- 1 / (4*24*60*60/20) # 4 days

for (j in 113:150){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation7/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:1065){
  print(j)
  filename <- paste0("./Simulation7/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase7 <- data.frame(S = S, E = E, I = I, R = R)
finalcase7
hist(242 - S)
save(finalcase7, file = "./Simulation7/finalcase7.RData")
###########################################################################


################## Simulation8: beta = 3*4 * 10^(-4) #####################
beta <- 3 * 4 * (10^-4)
sigma <- 1 / (1*24*60*60/20) # 1 day
gamma <- 1 / (2*24*60*60/20) # 2 days

for (j in 94:150){
  output <- replicate(n=length(timels), expr = data.frame(id = info$id, 
                                                          status = "S"), 
                      simplify = F)
  p0 <- sample(1:nrow(info), 1)
  output[[1]]$status[p0] <- "I"
  
  for (i in 1:(length(timels) - 1)){
    print(i)
    ## let t+1 replicate the previous status
    output[[i+1]] <- output[[i]] 
    
    ## we are at time t, we have lists of four groups
    ttime <- timels[i] 
    S <- output[[i]] %>% filter(status == "S") %>% select(id) %>% unlist()
    E <- output[[i]] %>% filter(status == "E") %>% select(id) %>% unlist()
    I <- output[[i]] %>% filter(status == "I") %>% select(id) %>% unlist()
    
    ## for "E" group, some people become "I"
    output[[i+1]][output[[i+1]]$id %in% E, "status"] <- ifelse(rbern(length(E), 1 - (1 - sigma)^diff[i+1]) == 0, "E", "I")
    
    ## for "I" group, some people become "R"
    output[[i+1]][output[[i+1]]$id %in% I, "status"] <- ifelse(rbern(length(I), 1 - (1 - gamma)^diff[i+1]) == 0, "I", "R")
    
    ## for "S" group, some people become "E"
    IScont <- datrep %>% filter(time == ttime) %>% 
      filter((P1 %in% I & P2 %in% S) | (P1 %in% S & P2 %in% I))
    Sls <- unique(c(IScont$P1, IScont$P2))
    Sls <- Sls[Sls %notin% I]
    output[[i+1]][output[[i+1]]$id %in% Sls, "status"] <- ifelse(rbern(length(Sls), beta) == 0, "S", "E")
  }
  filename <- paste0("./Simulation8/test", j, ".RData")
  save(output, file = filename)
}

# see distribution of final cases
S <- c()
E <- c()
I <- c()
R <- c()

for (j in 1:1101){
  print(j)
  filename <- paste0("./Simulation8/test", j, ".RData")
  load(filename)
  data <- output[[6248]]
  S[j] <- nrow(data[data$status == "S", ])
  E[j] <- nrow(data[data$status == "E", ])
  I[j] <- nrow(data[data$status == "I", ])
  R[j] <- nrow(data[data$status == "R", ])
}
finalcase8 <- data.frame(S = S, E = E, I = I, R = R)
finalcase8
hist(242 - S)
save(finalcase8, file = "./Simulation8/finalcase8.RData")
###########################################################################

## minimize the file size

for (i in 1033:1101) {
  print(i)
  filename <- paste0("./Simulation8/test", i, ".RData")
  load(filename)
  save(output, file = filename)
}





