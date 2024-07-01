setwd('E:\\ASSIGNMENT\\Data')
getwd()

install.packages('AER')
library(AER)

data("Affairs")
head(Affairs)
unique(Affairs$affairs)
table(Affairs$affairs)

## from Table 22.4 in Greene (2003)
fm.tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating ,
  data = Affairs)
fm.tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  right = 4, data = Affairs)

summary(fm.tobit)
summary(fm.tobit2)

# Fit a Tobit Model to real data - NSSO68
unique(df$state_1)
df = read.csv('NSSO68.csv', header=TRUE)
dput(names(df))
df_pun = df[df$state_1== 'Pun',]
vars <- c("Sector", "hhdsz", "Religion", "Social_Group", "MPCE_URP", "Sex", "Age", "Marital_Status", "Education", "chicken_q", "chicken_v")

df_pun_p = df_pun[vars]
names(df_pun_p)

df_pun_p$price = df_pun_p$chicken_v / df_pun_p$chicken_q
names(df_pun_p)

summary(df_pun_p)

head(table(df_pun_p$chicken_q))
dim(df_pun_p)

# Fitting a Multiple Linear regression Model
fit = lm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price , data=df_pun_p)
summary(fit)

# Fitting a Tobit Model to the data
install.packages('GGally')
install.packages('VGAM')
install.packages('ggplot2')
exp(-1.104e+00)
sd(df_pun_p$chicken_q)
require(ggplot2)
require(GGally)
require(VGAM)

ggpairs(df_pun_p[, c("chicken_q", "MPCE_URP", "price")])

m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_pun_p)
summary(m)

exp(-1.032e+00)
sd(df_pun_p$chicken_q)
df_pun_p$price[is.na(df_pun_p$price)] <- 0

m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_pun_p)
summary(m)
