#Created 30Jun2022
#KathKold
#Purpose: simulation of consumer purchase data and run mixed model 
#Specifically mean monetary proportion of purchased sugary drinks (meanq)
# and mean hba1c in subsequent quarter (mvq2)

require(devtools)
install_github('kkholst/lava',dep=TRUE)
library(Publish)
library(lava)
#install.packages("Rgraphviz")
#library(Rgraphviz)
packageVersion("Publish")
packageVersion("lava")
packageVersion("visNetwork")
packageVersion("igraph")
packageVersion("Rgraphviz")
lava.options(layout="fdp")
lava.options(plot.engine="Rgraphviz")
library(data.table)
#install.packages("lme4")
library(lme4)

d <- lvm()

distribution(d,~sex) <- binomial.lvm(p=0.66) #sex
distribution(d,~inc) <- normal.lvm(mean=296352,sd=123838) #income 
distribution(d,~meanq) <- normal.lvm(mean=3.6,sd=3.8) #moneyary percentage on sugar drinks, perhaps consider log transformation?
distribution(d,~mvq2) <- normal.lvm(mean=54.8,sd=14.3) #hba1c (mmol/mol)

d <- categorical(d,~medgroup,K=3,p=c(0.533,0.241),
                 labels=c("insulin","other","metformin")) #mediationgroup
d <- categorical(d,~edu,K=5,p=c(0.047,0.454,0.226,0.173),
                 labels=c("1","2","3","4","5"))           #education
d <- categorical(d,~fstruc,K=5,p=c(0.022,0.265,0.221,0.425),
                 labels=c("s_adult","s_old","adult_k",">1old",">1adult")) #family structure 

regression(d,mvq2~meanq) <- 0.1279
regression(d,mvq2~sex) <- 2.9
regression(d,mvq2~medgroup[1]+medgroup[2]) <- c(-1,-6) #hmmmm.... look into: should be -3.9 and -11.8, but it does not show

set.seed(19)
dt <- sim(d,1337)

#lm(mvq2~medgroup,data=dt)


# Add unique person identifier ---------------------------------------------------------------
#551 unique persons 
#223:1, 142:2, 61:3, 52:4, 35:5, 18:6, 8:7, 8:8, 4:9 
#should be read: 223 individuals with only one measurement and so on.. 

setDT(dt)
dt[order(sex,fstruc,edu,inc,medgroup),id:=c(1:223,rep(224:365,each=2),rep(366:426,each=3),rep(427:478,each=4),rep(479:513,each=5),rep(514:531,each=6),rep(532:539,each=7),rep(540:547,each=8),rep(548:551,each=9))]


#Try to run mixed model and compare coef ----------------------------------------------------

summary(lmer(mvq2~(1+meanq|id)+edu+fstruc+sex,data=dt))
summary(lmer(mvq2~(meanq||id)+edu+fstruc+sex,data=dt))
summary(lmer(mvq2~meanq+(1|id)+edu+fstruc+sex,data=dt)) #estimate not too far?





#hmm: should we use another approach to simulate data?? -----------------------------------------

# https://stats.stackexchange.com/questions/187981/how-to-simulate-data-to-demonstrate-mixed-effects-with-r-lme4
#how to account for correlated struture.. 






