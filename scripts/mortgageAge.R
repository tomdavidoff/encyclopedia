# mortgageAge.R
# plot mortgage status by age among household heads in IPUMS 2000 5% and 2020-2024 5 Year ACS
# Tom Davidoff 
# 07/09/26


library(data.table)
library(ggplot2)


d0 <- fread("~/DropboxExternal/dataRaw/ipums/mortgageOwnerHeads2000.csv",skip=12)
d0[,source:="2000 Census"]
print(head(d0))

d24 <- fread("~/DropboxExternal/dataRaw/ipums/mortgageOwnerHeads2024.csv",skip=12)
d24[,source:="2020-2024 ACS"]
print(head(d24))

dT <- rbind(d0,d24)
dT[,age:=as.numeric(substring(V2,1,2))]
dT[,mortgageStatus:=V3-1] # V3 is 1=No mortgage, 2=Mortgage, so subtract 1 to get 0=No mortgage, 1=Mortgage

# typical econ journal plot -- ticks on vertical axis and horizontal. No weird shading. yes ticks with value labels lik
ggplot(dT[,.(mortgageStatus,age,source)]) +
  geom_point(aes(x=age,y=mortgageStatus,color=source)) +
  scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1)) +
  scale_x_continuous(breaks=seq(20,100,10)) +
  labs(x="Age",y="Share with mortgage debt",color="Data Source") +
  theme_classic()
ggsave("text/mortgageStatusByAge.png",width=8,height=6)
