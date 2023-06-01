# 对鲍鱼数据集重新编码
g=c("M","F","F","I","M","M","F")
g
which(g=="M")
ifelse(g=="M",1,ifelse(g=="F",2,3))

grps=list()
for (gen in c("M","F","I")) grps[[gen]]=which(g==gen)
grps

# 另一种方法
lapply(c("M","F","I"),function(gender) which(g==gender))

# 另一种方法
# split(x,f)中x是向量或数据框,f是因子或因子的列表,
# 按照f把x划分为组,并返回分组的列表
split(1:7,g)

aba <- read.csv("abalone.data",header=F,as.is=T) # stringsAsFactors = FALSE
#数据框重命名
names(aba)=c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
head(aba)
grps <- list()
for (gen in c("M","F")) grps[[gen]] <- which(aba[,1]==gen)
abam <- aba[grps$M,]
abaf <- aba[grps$F,]
plot(abam$Length,abam$Diameter)
plot(abaf$Length,abaf$Diameter,pch="x",new=FALSE)

#对不同性别组分别做直径对长度的回归分析
by(aba,aba$Sex,function(m) lm(m[,2]~m[,3]))

# 排除幼鱼数据
abamf <- aba[aba$Sex != "I",]
head(abamf)
abamf$Sex <- factor(abamf$Sex,levels = c("M","F"))
loall <- sapply(abamf[,-1],function(clmn) {glm(abamf$Sex~clmn,family=binomial)$coef})
loall
