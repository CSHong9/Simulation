library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)

#replication
#set.seed(111)

## parameters
n.actors <- 100 
issues <- 4 
time <- 500
n.rep <- 1000

### array for summarizing outcomes
rep.num.issue  <- array (data=0, dim=c(time-1, issues, n.rep)) #only in 499 rounds issues are selected
rep.hhi <- array (data=0, dim=c(1, n.rep))
rep.interest.1 <- array (data=0, dim=c(n.actors, time, n.rep)) 
rep.interest.2 <- array (data=0, dim=c(n.actors, time, n.rep)) 
rep.interest.3 <- array (data=0, dim=c(n.actors, time, n.rep)) 
rep.interest.4 <- array (data=0, dim=c(n.actors, time, n.rep)) 

############ simulation start #################
for (r in 1:n.rep){

### array for issues interests
##3 dimensional array: actors, issues, time  
issues.interest <- array (data=0, dim=c(n.actors, issues, time)) 

### array for distances among actors by their interests
##3 dimensional array: actors matrix (2 dim), time 
euclidean.distance <- array (data=0, dim=c(n.actors, n.actors, time)) 
norm.euclidean.distance <- array (data=0, dim=c(n.actors, n.actors, time)) 
soc.distance.prob <- array (data=0, dim=c(n.actors, n.actors, time)) 

############ initial conditions ################

sigma1<-(100/3) 
issues.interest [,1,1] <- rnorm (n.actors, 0, sigma1) 
issues.interest [,2,1] <- rnorm (n.actors, 0, sigma1) 
issues.interest [,3,1] <- rnorm (n.actors, 0, sigma1) 
issues.interest [,4,1] <- rnorm (n.actors, 0, sigma1) 

for (i in 1:issues){ 
  for (pi in 1:n.actors){ 
    if (issues.interest [pi,i,1]>100) 
      issues.interest [pi,i,1]<-100 
    if (issues.interest [pi,i,1]< (-100)) 
      issues.interest [pi,i,1]<- (-100) 
    if (issues.interest [pi,i,1]< 1 & issues.interest [pi,i,1]>0) 
      issues.interest [pi,i,1]<- 1 
    if (issues.interest [pi,i,1]< 0 &issues.interest [pi,i,1]>-1) 
      issues.interest [pi,i,1]<- (-1) 
  } 
} 

##intitialize euclidean distance 
squared.differences<- c(0,0,0,0) 
for (pi in 1:n.actors){ 
  for (pj in 1:n.actors){ 
    if (!pi==pj){ 
      for (i in 1:issues){ 
        squared.differences[i] <- (issues.interest[pi,i,1]-issues.interest[pj,i,1])^2 
      } 
      euclidean.distance [pi,pj,1] <- sqrt(sum(squared.differences)) 
    } 
  } 
} 

max.eucl.dist <- max (euclidean.distance [,,1]) 
for (pi in 1:n.actors){ 
  for (pj in 1:n.actors){ 
    norm.euclidean.distance [pi,pj,1] <- euclidean.distance [pi,pj,1]/max.eucl.dist 
  } 
} 

## here we use (1 - lamda) (probability of interaction)
soc.distance.prob[,,] <- 1 - mean(norm.euclidean.distance[,,1]) 
for (t in 1:time){ 
  diag (soc.distance.prob[,,t])<-0 
} 

############ Iteration Flow ################

### array for partner selection
contacted.people <- array (data=0, dim=c(n.actors, n.actors, time)) 

### array for chosen issues
issue.chosen <- array (data=0, dim=c(n.actors, issues, time)) 

### infl is a scaling factor (.1) that restricts the range of attitude change given an interaction.
infl<-0.1 

timered <- time-1 
#timered <- time (would casue error since issues.interest[pi, i, t + 1] : subscript out of bounds)
for (t in 1:timered){
  
  #######################################
  ###Selection of interaction partners###
  #######################################

  ### randomly select people actors would like to contact. the number is based on their issues interests
  ### the interaction between actors a and b is defined as the outcome of a random draw from a Bernoulli distribution with
  ### probability equal to 1 minus the perceived ideological distance between a and b. 
  ### 0.005 is a scaling factor that limits the number of interactions to a reasonable range. (p.791)
  
  for (pi in 1:n.actors){ 
    issues.mean <- mean(abs(issues.interest[pi,,t])) 
    pool <- 1:n.actors 
    contacts  <- sample(pool,.005*round(issues.mean)^2, replace=T) 
    for (pj in contacts){	 
      contacted.people[pi,pj,t]<-rbinom(1, 1, prob=soc.distance.prob[pi,pj,t])	 
    } 
  }
  ## check the distribution of the number of contacts
  #summary(rowSums(contacted.people[,,1]))
  
  ########################################
  ###Process of interpersonal influence###
  ########################################
  
  ### Select the issue for discussion
  for (pi in 1:n.actors) { 
    for (pj in 1:n.actors) { 
      if (contacted.people [pi,pj,t]==1){ 
        ##count the number of issues in which actors are in agreement 
        count <- 0 
        for (issue in 1:issues){ 
          if (sign(issues.interest[pi,issue,t]) == sign(issues.interest[pj,issue,t])) 
            count <- count+1 
        } 
        
        ##select the issue on which actors confront, by comparing all the issues and selecting the one in which their joint interest (disregarding the sign) is higher. 
        sum <- c(0,0,0,0) 
        for (issue in 1:issues){ 
          sum [issue] <- abs(issues.interest[pi,issue,t])+abs(issues.interest[pj,issue,t]) 
        } 
        vec.i <- order (sum, decreasing=T) #gives the vector that order the order of sum values 
        i<-vec.i[1] 
        
        
        #information on choosed issues	(for record)		 
        issue.chosen [pi,i,t] <- issue.chosen[pi,i,t]+1 
        issue.chosen [pj,i,t] <- issue.chosen[pj,i,t]+1 
  
  ### Compute the change for each actor based on their interest on the issue (condition 5, p.793)   
        #change in people's opinion
        d.pi<-(abs(issues.interest[pi,i,t]-issues.interest[pj,i,t])/abs(issues.interest[pi,i,t]))*infl 
        d.pj<-(abs(issues.interest[pi,i,t]-issues.interest[pj,i,t])/abs(issues.interest[pj,i,t]))*infl 
  
  ### Determine the direction of change according to the sign of the issue
  ### in this stage, only changes are stored)
        if (issues.interest [pi,i,t]>0 & issues.interest [pj,i,t]>0){ 
          issues.interest[pi,i,t+1]<- issues.interest [pi,i,t+1]+d.pi 
          issues.interest[pj,i,t+1]<- issues.interest [pj,i,t+1]+d.pj 
        } 
        
        if (issues.interest [pi,i,t]<0 & issues.interest [pj,i,t]<0){ 
          issues.interest[pi,i,t+1]<- issues.interest [pi,i,t+1]-d.pi 
          issues.interest[pj,i,t+1]<- issues.interest [pj,i,t+1]-d.pj 
        } 
        
        d.pi<-abs((issues.interest[pi,i,t]+issues.interest[pj,i,t])/abs(issues.interest[pi,i,t]))*infl 
        d.pj<-abs((issues.interest[pi,i,t]+issues.interest[pj,i,t])/abs(issues.interest[pj,i,t]))*infl 
        
        if (issues.interest [pi,i,t]>0 & issues.interest [pj,i,t]<0){ 
          if (count>1) { 
          ### compromise
            issues.interest[pi,i,t+1] <- issues.interest[pi,i,t+1]-d.pi 
            issues.interest[pj,i,t+1] <- issues.interest [pj,i,t+1]+d.pj 
          } 
          else { 
          ### conflict
            issues.interest[pi,i,t+1] <- issues.interest[pi,i,t+1]+d.pi 
            issues.interest[pj,i,t+1] <- issues.interest [pj,i,t+1]-d.pj 
          } 
        } 
        if (issues.interest [pi,i,t]<0 & issues.interest [pj,i,t]>0){ 
          if (count>1) { 
          ### compromise
            issues.interest[pi,i,t+1] <- issues.interest[pi,i,t+1]+d.pi 
            issues.interest[pj,i,t+1] <- issues.interest [pj,i,t+1]-d.pj 
          } 
          else { 
          ### conflict
            issues.interest[pi,i,t+1] <- issues.interest[pi,i,t+1]-d.pi 
            issues.interest[pj,i,t+1] <- issues.interest [pj,i,t+1]+d.pj 
          } 
        } 
      
  ### Update actors’ perceived ideological distance with the current/actual distance
        #update soc.distance prob for people who get in touch 
        squared.differences <- c(0,0,0,0) 
        if (!pi==pj){ 
          for (i in 1:issues){ 
            squared.differences[i] <- (issues.interest[pi,i,t] - issues.interest[pj,i,t])^2
          } 
          euclidean.distance [pi,pj,t:time] <- sqrt(sum(squared.differences)) 
          euclidean.distance [pj,pi,t:time] <- sqrt(sum(squared.differences)) 
          
        } 
        max.eucl.dist <- max(euclidean.distance [,,t]) 
        norm.euclidean.distance [pi,pj,t:time] <- euclidean.distance [pi,pj,t]/max.eucl.dist 
        norm.euclidean.distance [pj,pi,t:time] <- euclidean.distance [pj,pi,t]/max.eucl.dist 
        soc.distance.prob[pi,pj,t:time] <- 1 - norm.euclidean.distance[pi,pj,t] 
        soc.distance.prob[pj,pi,t:time] <- 1 - norm.euclidean.distance[pj,pi,t] 
      } 
    }	 
  }
  ### update all issues interest (as the influence process only change interests in salient issues)
  for (i in 1:issues) {	 
    issues.interest[,i,t+1] <- issues.interest[,i,t+1] + issues.interest[,i,t] 
  } 
  
  for (i in 1:issues){ 
    for (pi in 1:n.actors){ 
      if (issues.interest [pi,i,t+1] > 100) 
        issues.interest [pi,i,t+1] <- 100 
      if (issues.interest [pi,i,t+1]< (-100)) 
        issues.interest [pi,i,t+1] <- (-100) 
      if (issues.interest [pi,i,t+1] < 1 & issues.interest [pi,i,t+1] > 0) 
        issues.interest [pi,i,t+1] <- 1 
      if (issues.interest [pi,i,t+1] < 0 & issues.interest [pi,i,t+1] > -1) 
        issues.interest [pi,i,t+1] <- (-1) 
    } 
  } 
  
#  print (t) 
}

### data.frame for figure 1. (p.795)
issue.df <- as.data.frame.table(issue.chosen[,,1:499])
colnames(issue.df) <- c("actors","issue","round", "Freq")
issue.df$actors <- as.numeric(issue.df$actors)
issue.df$round <- as.numeric(issue.df$round)
issue.df$issue <- as.factor(issue.df$issue)

issue.plot <- issue.df %>%
  group_by(round, issue) %>%
  summarize(Freq = sum(Freq, na.rm = TRUE)) %>%
  filter(!is.na(issue))

#print(ggplot(data=issue.plot) +
#  geom_line(data=issue.plot , aes(x=round, y=Freq, color=issue)) +
#  ylim(0, 1000) + theme(axis.title.x=element_blank(),
#                       axis.text.x=element_blank(),
#                       axis.ticks.x=element_blank()) +
#  scale_color_grey())

### compute a Herfindahl and Hirschman index of concentration (hereafter, the HH index):
vec <- colSums(issue.chosen[,,499])
HHI <- sum((vec/sum(vec))^2)

# figure 1
rep.hhi[,r] <- HHI
rep.num.issue[,,r] <- as.matrix(xtabs(Freq ~., issue.plot))

# figure 2
rep.interest.1[,,r] <- issues.interest[,1,]
rep.interest.2[,,r] <- issues.interest[,2,]
rep.interest.3[,,r] <- issues.interest[,3,]
rep.interest.4[,,r] <- issues.interest[,4,]

print (r)

}

# figure 2 - histograme
#hist(rep.interest.1[,1,1])
#hist(rep.interest.1[,100,1])
#hist(rep.interest.1[,200,1])
#hist(rep.interest.1[,350,1])
#hist(rep.interest.1[,500,1])

save.image("pilotpolarization.RData")

as.data.frame.table(rep.hhi) %>%
  ggplot(., aes(Freq)) + 
  geom_histogram(binwidth = .01)
