# ipumsAge.R
# Marriage and owner-head/spouse rates by age, 2000 vs 2020-2024
# Tom Davidoff
# 07/10/26

library(data.table)
library(ggplot2)

df <- fread("~/DropboxExternal/dataRaw/ipums/usa_00056.csv")
df[,period:=ifelse(YEAR==2000,"2000","2020-2024")]

pdt <- rbind(df[,.(value=weighted.mean(MARST==1,PERWT),measure="Married"),by=.(AGE,period)],
             df[,.(value=weighted.mean(RELATE %in% c(1,2) & OWNERSHP==1,PERWT),measure="Own"),by=.(AGE,period)])

ggplot(pdt[AGE %between% c(25,90)],aes(x=AGE,y=value,color=measure,linetype=period)) +
  geom_line() +
  scale_color_manual(values=c("Married"="blue","Own"="red")) +
  labs(x="Age",y="Fraction",color=NULL,linetype=NULL) +
  theme_classic()
ggsave("text/ageMarriageOwnership.png",width=7,height=5)

