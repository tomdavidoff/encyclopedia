# ipums20002024.R
# R to analyze Census stuff on ownership by income
# Do metro FE change importance

library(data.table)
library(ggplot2)
library(fixest)

dE <- fread("~/DropboxExternal/dataRaw/ipums/usa_00055.csv")
print(head(dE))
CUTQUANT <- .1
dE <- dE[INCSS>0 & RENT>0 & MET2013>0 & INCSS!=99999 & (HHTYPE==4 | HHTYPE==6)]
print(summary(dE))
print(quantile(dE[YEAR==2000,INCSS]))
print(quantile(dE[YEAR==2000,INCSS],.1))
print(quantile(dE[YEAR!=2000,INCSS],.1))
print(quantile(dE[YEAR!=2000,RENT],.1))
print(summary(feols(log(RENT) ~ log(INCSS) | MET2013,data=dE[YEAR==2000 & RENT>quantile(RENT,CUTQUANT) & INCSS>quantile(INCSS,CUTQUANT)])))
print(summary(feols(log(RENT) ~ log(INCSS) | MET2013 + MULTYEAR,data=dE[YEAR!=2000 & RENT>quantile(RENT,CUTQUANT) & INCSS>quantile(INCSS,CUTQUANT)])))
print(summary(feols(log(RENT) ~ log(INCSS) ,data=dE[YEAR==2000 & RENT>quantile(RENT,CUTQUANT) & INCSS>quantile(INCSS,CUTQUANT)])))
print(summary(feols(log(RENT) ~ log(INCSS) ,data=dE[YEAR!=2000 & RENT>quantile(RENT,CUTQUANT) & INCSS>quantile(INCSS,CUTQUANT)])))

# Davis - FOM
dShare <- data.table(rentLevel=numeric(),rentShare=numeric(),N=numeric())
for (m in unique(dE[,MET2013])) {
  print(m)
  dm <- dE[YEAR!=2000 & MET2013==m, .(rentLevel=median(RENT), rentShare=12*median(RENT/INCSS), N=.N)]
  dShare <- rbind(dShare, dm)
}
dShare <- dShare[!is.na(rentLevel) & !is.na(rentShare)]
print(summary(dShare))
print(cor(dShare))


q("no")

df <- fread("~/DropboxExternal/dataRaw/ipums20002024Tenure.csv")
head(df)
print(names(df))
print(table(df$OWNERSHP))
print(table(df$RELATE))
print(table(df$MARST))

df[,own:=ifelse(OWNERSHP==1,1,0)]

print(df[,mean(HHINCOME),by=own])

print(summary(feols(own ~ log(HHINCOME)  | SAMPLE + YEAR + AGE + MARST,data=df[YEAR==2000])))
print(summary(feols(own ~ log(HHINCOME)  | SAMPLE +  AGE + MARST,data=df[YEAR!=2000])))

print(summary(feols(own ~ log(HHINCOME)  | MET2013 + SAMPLE + YEAR + AGE + MARST,data=df[YEAR==2000])))
print(summary(feols(own ~ log(HHINCOME)  | MET2013 + SAMPLE +  AGE + MARST,data=df[YEAR!=2000])))

# line plot in age for 2000 and then not 2010. Note sample is just household heads -- relate==1

# do married now
pdt <- df[RELATE==1, .(own=weighted.mean(own, HHWT)),
          by=.(AGE, period=ifelse(YEAR==2000, "2000", "2020-2024"))]

ggplot(pdt[AGE %between% c(18,90)], aes(x=AGE, y=own, color=period)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values=c("2000"="black", "2020-2024"="red")) +
  labs(title="Homeownership by Age, 2000 vs 2020-2024",
       y="Homeownership Rate", x="Age", color=NULL)

ggsave("text/homeownershipAge2010.png", width=7, height=5)

mkpanel <- function(d) d[RELATE==1,
  .(own=weighted.mean(own, HHWT)),
  by=.(AGE, period=ifelse(YEAR==2000, "2000", "2020-2024"))]

panels <- list(
  allMetros     = df[MET2013>0],
  sanFrancisco  = df[MET2013==41860],
  houston       = df[MET2013==26420],
  boston	= df[MET2013==14460],
  chicago       = df[MET2013==16980])

titles <- c(allMetros="All Metros", sanFrancisco="San Francisco",
            houston="Houston", boston = "Boston", chicago="Chicago")

for (nm in names(panels)) {
  pdt <- mkpanel(panels[[nm]])
  p <- ggplot(pdt[AGE %between% c(25,90)], aes(x=AGE, y=own, color=period)) +
    geom_line(linewidth=0.8) +
    scale_color_manual(values=c("2000"="black", "2020-2024"="red")) +
    labs(title=paste0("Homeownership by Age: ", titles[nm]),
         y="Homeownership Rate", x="Age", color=NULL)
  ggsave(paste0("text/homeownershipAge_", nm, ".png"), p, width=7, height=5)
}

# MARITAL STATUS
pdt <- df[RELATE==1 & YEAR!=2000 & MARST!=2,
  .(own=weighted.mean(own, HHWT)),
  by=.(AGE, marst=ifelse(MARST==1, "Married", "Not married"))]

ggplot(pdt[AGE %between% c(25,90)], aes(x=AGE, y=own, color=marst)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values=c("Married"="black", "Not married"="red")) +
  labs(title="Homeownership by Age and Marital Status, 2020-2024",
       y="Homeownership Rate", x="Age", color=NULL)

ggsave("text/homeownershipAgeMarst.png", width=8, height=5)

 #[1] "YEAR"      "MULTYEAR"  "SAMPLE"    "SERIAL"    "CBSERIAL"  "HHWT"     [7] "CLUSTER"   "MET2013"   "STRATA"    "GQ"        "OWNERSHP"  "OWNERSHPD" [13] "HHINCOME"  "PERNUM"    "PERWT"     "RELATE"    "RELATED"   "AGE"      [19] "MARST"    
