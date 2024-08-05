data = read.csv("Pokemon.csv")

# replacing name duplications
data$Name = sub(".*(Mega)", "Mega", data$Name)

str(data)
head(data)

# legendary pokemons
data[data$Legendary=="True",]

# legendary dragon pokemon
data[data$Legendary=="True" & (data$Type1=="Dragon" | data$Type2=="Dragon"),]

# pokemon with max stats
sapply(data[5:11], max, na.rm = TRUE)

data[which.max(data$Total),]
data[which.max(data$HP),]
data[which.max(data$Attack),]
data[which.max(data$Defense),]
data[which.max(data$Sp.Atk),]
data[which.max(data$Sp.Def),]
data[which.max(data$Speed),]

# sort by total (the sum of all attributes)
sorted_by_total = data[order(-data$Total),]

# all the unique pokemon types
unique(data$Type1)
length(unique(data$Type1))

# summary of stats
summary(data[5:11])

# attack distribution of pokemon + mean
library(ggplot2)

ggplot(data, aes(x=Attack)) +
    geom_histogram(color="black", fill="white", binwidth = 10) -> attack_plot
attack_plot + geom_vline(aes(xintercept=mean(Attack)),
    color="red", linetype="dashed", size=1)

# attack distribution over all types
ggplot(data, aes(x=Type1, y=Attack)) + geom_boxplot()

# fire vs water pokemon
fire_pokemon = data[data$Type1=='Fire' | data$Type2=='Fire',]
fire_pokemon = head(fire_pokemon, n=50)
water_pokemon = data[data$Type1=='Water' | data$Type2=='Water',]
water_pokemon = head(water_pokemon, n=50)

fire_vs_water = data.frame(x=fire_pokemon$Attack, y=water_pokemon$Defense)
water_vs_fire = data.frame(x=water_pokemon$Attack, y=fire_pokemon$Defense)

ggplot(data = fire_vs_water, aes(x=x,y=y)) +
    geom_point(size=2, col="red", shape=15) +
    geom_point(data=water_vs_fire, size=2, col="blue", shape=16) +
    labs(x="Attack", y="Defense")

# boxplot of stats
atk = data$Attack
def = data$Defense
spd = data$Speed
hp = data$HP

boxplot(hp, atk, def, spd,
    main = "Attributes Boxplot",
    at = c(1,2,3,4),
    names = c("hp", "atk", "def", "spd"),
    las = 2
)


gen1 = data[data$Generation==1,]
gen1_atk = gen1[order(-gen1$Attack),]
gen1_atk_top50 = head(gen1_atk, n=50)$Attack

gen2 = data[data$Generation==2,]
gen2_atk = gen2[order(-gen2$Attack),]
gen2_atk_top50 = head(gen2_atk, n=50)$Attack

df_gen1 = data.frame(y=gen1_atk_top50)
ggplot(df_gen1, aes(sample=y)) + stat_qq() + stat_qq_line() -> p1

df_gen2 = data.frame(y=gen2_atk_top50)
ggplot(df_gen2, aes(sample=y)) + stat_qq() + stat_qq_line() -> p2

require(gridExtra)
grid.arrange(p1, p2, ncol=2)

shapiro.test(df_gen1$y)
shapiro.test(df_gen2$y)
# based on the shapiro test p much less than 0.05 significance level
# so it's not normally distributed, we use Wilcox test

wilcox.test(gen1_atk_top50, gen2_atk_top50, paired = TRUE)

# again p much less than 0.05, we can conclude that
# the median of the attack attribute from gen 1
# is significantly different to that of gen 2

data[data$Type1=="Water" | data$Type2=="Water",] -> data_water  # nrow 126
data[data$Type1=="Fire" | data$Type2=="Fire",]   -> data_fire   # nrow 64
data[data$Type1=="Grass" | data$Type2=="Grass",] -> data_grass  # nrow 95

# n=800 total pokemon
# p_w = 126*100/800 = 15.75%
# p_f = 64*100/800  = 8%
# p_e = 95*100/800  = 11.875%

top = head(data[order(-data$Total),], n=150)

# observations
top[top$Type1=="Water" | top$Type2=="Water",] -> top_water  # nrow 20
top[top$Type1=="Fire" | top$Type2=="Fire",]   -> top_fire   # nrow 21
top[top$Type1=="Grass" | top$Type2=="Grass",] -> top_grass  # nrow 9

prop.test(x = 20, n = 150, p = 0.1575, correct = FALSE)
prop.test(x = 21, n = 150, p = 0.08,   correct = FALSE)
prop.test(x = 9,  n = 150, p = 0.1187, correct = FALSE)

# p-value-grass    = 0.0262
# p-value-fire     = 0.0067
# p-value-water    = 0.4165

# on a 95% percent confidence interval we have a significant
# difference on all 3 types but because the p-value of fire
# is the smallest, we could also assume that when offered
# a starter pokemon, fire is the best choice


library(ggcorrplot)
attributes_dtf = data[,5:11]
corr = round(cor(attributes_dtf), 1)
head(corr)
names(attributes_dtf)

ggcorrplot(corr, hc.order = TRUE, lab = TRUE)
cor(attributes_dtf$Total, attributes_dtf$Attack)
# linear regression with one feature - attack

# 75% of our data is used for training, 25% for test
sample_size = floor(0.75 * nrow(attributes_dtf))

set.seed(123)
train_ind = sample(seq_len(nrow(attributes_dtf)), size=sample_size)

train = attributes_dtf[train_ind, ]
test = attributes_dtf[-train_ind, ]

model = lm(Total ~ Attack, data=train)

summary(model)

plot(attributes_dtf$Attack, attributes_dtf$Total, main = "Simple Linear Regression",
     xlab = "Attack", ylab = "Total",
     pch = 19, frame = FALSE)
abline(model, col = "blue")

prediction = predict(model, test)
prediction

actual_predict_dtf = data.frame(cbind(actuals=test$Total, predicteds=prediction))
cor(actual_predict_dtf)

head(actual_predict_dtf)


# multiple linear regression
# Sp.Atk 0.7
# Speed 0.6
# Defense 0.6
# Sp.Def 0.7
# HP 0.6
# Attack 0.7

model = lm(Total ~ Sp.Atk + Sp.Def + Attack + Defense, data=train)
summary(model)
# this offers a pretty good prediction model with R^2=0.91

prediction = predict(model, test)

actual_predict_dtf = data.frame(cbind(actuals=test$Total, predicteds=prediction))
cor(actual_predict_dtf)

head(actual_predict_dtf, n=10)

# used to create latex tables out of dataframes
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
