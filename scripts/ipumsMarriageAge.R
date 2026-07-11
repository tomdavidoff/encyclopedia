# ipumsMarriageAge.R
# Marriage by age IPUMS
# Tom Davidoff 
# 07/10/26

library(data.table)
library(ggplot2)


d0 <- fread("~/DropboxExternal/dataRaw/ipums/ageMarriage2000.csv",skip=12)
d24 <- fread("~/DropboxExternal/dataRaw/ipums/ageMarriage2024.csv",skip=12)
print(head(d0))
print(head(d24))

d0[,sample := 2000]
d24[,sample := 2024]
dT <- rbind(d0,d24)
dT[,age:=as.numeric(substring(V2,1,3))]
dT[,fractionMarried:=V3]
print(head(dT))

ggplot(dT[,.(age,fractionMarried,sample)],aes(x=age,y=fractionMarried,color=as.factor(sample))) + geom_line() + 
  scale_color_manual(values=c("blue","red")) + 
  labs(title="Fraction Married by Age",x="Age",y="Fraction Married",color="Year") +
  theme_minimal()
ggsave("text/ageMarriageIPUMS.png")

