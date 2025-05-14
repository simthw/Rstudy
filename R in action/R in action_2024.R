## 不明白或有问题的搜索Q

ls() #列出所有对象 
rm(list = ls()) #移除当前工作空间所有对象
part3#----------------------
#PART3
mydatatxt <- "
age gender weight
25 m 166
20 f 120
40 m 170"
mydata <- read.table(header = TRUE,text = mydatatxt)
pdf("11.pdf")
with(mtcars,{
  plot(wt,mpg)
  abline(lm(mpg~wt))
  title("Regression of mpg on wt")
}) 
dev.off()

n <- 10
mycolors <- rainbow(n)
pie(rep(1,n),labels = mycolors,col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1,n),labels=mygrays,col=mygrays)
plot(dose,drugA,type="b",col="red",lty=2,pch=2,lwd=2,
     main="Clinical Trials for DrugA",sub="This is hypothetical data",xlab="Dosage",
     ylab="Drug Response",xlim=c(0,60),ylim=c(0,70))
opar <- par(no.readonly = TRUE) #复制一份当前的图形参数设置
par() #修改当前图形参数
par(opar) #还原了原始的图形参数设置
plot(optionname=value) #指定的图形参数修改仅对当前绘制的图形本身有效
#pch=21-25,这5个符号可以指定符号边框颜色(col=)和填充颜色(bg=)
#坐标轴修改
#颜色，可通过颜色下标（数字），名称，十六进制颜色值或rgb()（红-绿-蓝三色值）,
# hsv()（色相-饱和度-亮度值）表示
#自定义坐标轴
x <- c(1:10)
y <- x
z <- 10/x
dev.new()
opar <- par(no.readonly=TRUE)
par(mar=c(5,4,4,8)+0.1)
plot(x,y,type="b",pch=21,col="red",yaxt="n",lty=3,ann=FALSE)
lines(x,z,type="b",pch=22,col="blue",lty=2)
axis(2,at=y,labels=y,col.axis="red",las=2)
axis(4,at=z,labels=round(z,digits=2),col.axis="blue",las=2,cex.axis=0.7,
     tck=-0.01)
mtext("y=1/x",side=4,line=3,cex.lab=1,las=2,col="blue")
title("An Example of Creative Axes",xlab="X values",ylab="Y=X")
par(opar)
#code3-3添加图例等
dose <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)
opar <- par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
plot(dose,drugA,type="b",pch=15,lty=1,col="red",ylim=c(0,60),
     main="DrugA vs DrugB",xlab="Drug Dosage",ylab="Drug Respose")
lines(dose,drugB,type="b",pch=17,lty=2,col="blue")
abline(h=30,lwd=1.5,lty=2,col="gray") #添加参考线
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio=0.5) #次要刻度线
legend("topleft",title="drug type",inset=0.05,legend=c("A","B"),lty=c(1,2),
       pch=c(15,17),col=c("red","blue"),cex=0.5) #添加图例
par(opar)
#3.4.5添加文本标注
with(mtcars,{
  plot(wt,mpg,main="Mileage vs. Car Weight",
       xlab="Weight",ylab="Mileage",pch=18,col="blue")
  text(wt,mpg,row.names(mtcars),cex=0.6,pos=4,col="red") #文本标注
})
#字体族修改
par <- par(no.readonly=TRUE)
par(cex=1.5,pin=c(4.5,3))
plot(1:7,1:7,type="n")
text(3,3,"Example of default text",cex=0.5)
text(4,4,family="mono","Example of mono-spaced text",cex=0.5)
text(5,5,family="serif","Example of serif text",cex=0.5)
par(opar)
#3.5par(mfrow)函数和par(mfcol)函数，2行2列图形分布
with(mtcars,{
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(2,2))
  plot(wt,mpg,main="scatterplot of wt vs. mpg")
  plot(wt,disp,main="scatterplot of wt vs.disp")
  hist(wt,main="histogram of wt")
  boxplot(wt,main="boxplot of wt")
})
par(opar)
#3行1列图形分布
with(mtcars,{
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(3,1))
  hist(wt)
  hist(mpg)
  hist(disp)
})
par(opar) 
#layout(matrix)函数,matrix指定要组合的多个图形所在的位置
with(mtcars,{
  layout(matrix(c(1,1,2,3),nrow = 2,ncol = 2,byrow = TRUE)) 
  #c(n1,n2,n3,n4)图形位置，默认按列填充
  #c(1,1,2,3)1幅图被置于第一行，另2幅图被置于第2行
  hist(wt)
  hist(mpg)
  hist(disp)
})
#修改图形布局的宽和高
with(mtcars,{
  layout(matrix(c(1,1,2,3),nrow = 2,ncol = 2,
  byrow = TRUE),widths=c(3,1),heights=c(1,2)) 
  hist(wt)
  hist(mpg)
  hist(disp)
})
#code3-4,fig()控制图形布局
opar <- par(no.readonly=TRUE)
par(fig=c(0,0.8,0,0.8)) 
#绘图区域左下角坐标(0,0)，右上角坐标(1,1),
# 参数fig()的取值是一个形如c(x1,x2,y1,y2)的数值向量
plot(mtcars$wt,mtcars$mpg,xlab="miles per gallon",
     ylab="car weight")
par(fig=c(0,0.8,0.55,1),new=TRUE)
boxplot(mtcars$wt,horizontal=TRUE,axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg,axes=FALSE)
mtext("enhanceg scatterplot",side=3,outer=TRUE,line=-3)
par(opar)

part4#----------------------
###PART4
# code4-1
manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <- c("US","US","UK","UK","UK")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2)
q5 <- c(5,5,2,NA,1)
leadership <- data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,
                         row.names=manager,stringsAsFactors=FALSE)
#连续性年龄变量重编码为类别型变量
#仅在[condition]值为TRUE时才执行赋值，99岁编码为缺失值
leadership$age[leadership$age == 99] <- NA 
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                  leadership$age <= 75] <- "Middle aged"
leadership$agecat[leadership$age < 55] <- "Young"
#写法2，同上述代码，within()允许修改数据框
leadership <- within(leadership,{
  agecat <- NA #创建了空白数据集agecat
  agecat[age>75] <- "Elder"
  agecat[age>=55&age<=75] <- "Middle aged"
  agecat[age<55] <- "Young"
})

# code4-2,创建新变量方法1
mydata <- data.frame(x1 = c(2,2,6,4),x2 = c(3,4,2,8))
mydata$sum <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
# code4-2,创建新变量方法2
attach(mydata)
mydata$sum <- x1 + x1
mydata$meanx <- (x1 + x2)/2
detach(mydata)
# code4-2,创建新变量方法3，transform()！
mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)
#4-4变量重命名
fix(leadership) #交互式修改，方法1
# 修改变量名，方法2
library(reshape)
leadership <- rename(leadership,
                     c(manager="managerID",date="testDate"))
# names()修改变量名，方法3
names(leadership)[1] <- "managerID"
names(leadership)[6:10] <- paste0("item",c(1:5))
y <- c(1,2,3,NA)
is.na(y) #检测是否存在缺失值
is.na(leadership[,6:10])

#4.5 缺失值
x <- c(1,2,NA,3)
y <- x[1]+x[2]+x[3]+x[4]
z <- sum(x)
z <- sum(x,na.rm=TRUE) #na.rm=TRUE移除缺失值
#code4-4,na.omit()移除含缺失值的行
leadership
newdata <- na.omit(leadership) #所有包含缺失数据的行都被删除
newdata

#4.6 将作为字符型数据输入的日期转为日期值
mydates <- as.Date(c("2007-06-22","2004-02-13")) #默认日期格式yyyy-mm-dd
#使用mm/dd/yyyy格式读取数据并转换显示格式
strdates <- c("01/05/1965","08/16/1975") 
mydate1 <- as.Date(strdates,"%m/%d/%Y")
# mydate2 <- as.Date(strdates) 不添加日期格式%m/%d/%Y输出结果异常
leadership$date <- as.Date(leadership$date,"%m/%d/%y") #例子，从字符串变量转为对应日期，##年份输出有问题

#返回系统日期
Sys.Date() #返回日期，默认格式
date() #返回系统的日期和时间,
format(Sys.Date(),format="%d-%m-%Y") #输出指定格式的日期值
format(Sys.Date(),format="%B")
format(Sys.Date(),format="%A")
#计算时间间隔
startdate <- as.Date("2004-02-13")
enddate <- as.Date("2011-01-22")
days <- enddate-startdate
#计算时间间隔
difftime(enddate,startdate,units="days")
#给定日期判断周几
date1 <- as.Date("2022-7-21")
weekdays(date1) #判定1
format(date1,format="%A")  #判定2
date2 <- as.Date("1956-10-12")
format(date2,format="%A")
date3 <- as.Date("1993-02-04")
format(date3,format="%A")
weekdays(date3)
#逆转换，日期变量转换为字符型变量
strDates <- as.character(strdates)
#code4-5数据类型判断和转换
a <- c(1,2,3)
is.numeric(a)
is.vector(a)
a <- as.character(a)
is.numeric(a)
is.vector(a)
is.character(a)

#数据排序
leadership
sort(leadership$age)
df1 <- leadership[order(leadership$age),] #对比sort和order函数排序的区别！！
df1
attach(leadership)
df2 <- leadership[order(gender,age),]
df2
detach(leadership)
with(leadership,{
  dd3 <- leadership[order(gender,-age),]
  dd3
})

#横向合并，添加列
total <- merge(dataframeA,dataframeB,by="ID") #通过共有变量进行联结
total <- cbind(A,B) #列合并，必须有相同的行数，且有相同的顺序
#行合并，添加观测， 必须有相同的列变量，但顺序不必一定相同
total <- rbind(dataframeA,dataframeB)
#选入变量
newdata <- leadership[,c(6:10)] #从数据框选择了第6至10列变量，并保存在数据框newdata中
myvars <- c("q1","q2","q3","q4","q5")
newdata <- leadership[myvars] #等价行216
myvars <- paste("q",1:5,sep="")
newdata <- leadership[myvars] #等价行216

#剔除变量
#names()生成包含所有变量名的字符型标量，%in%匹配q3和q4，匹配上为TRUE值
myvars1 <- names(leadership) %in% c("q3","q4") 
#上述逻辑值反转，选择逻辑值为TRUE的列，q3和q4被剔除
newdata <- leadership[!myvars1]
#剔除变量另一种表达
dd5 <- leadership[c(-8,-9)] 

#code4-6选入观测
#示例1
dd6 <- leadership[1:3,]
#示例2
dd7 <- leadership[which(leadership$gender=="M"&
                          leadership$age>30),] #函数which给出了值为true元素的下标
#例子2attach()表达，等价行227-228
attach(leadership)
dd7 <- leadership[which(gender=="M"&age>30),]
detach(leadership)
#选取指定日期之间的观测值
leadership$date <- as.Date(leadership$date,"%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
dd8 <- leadership[which(leadership$date>=startdate&
                          leadership$date<=enddate),] 

#subset()函数选择变量和观测
dd9 <- subset(leadership,age>=35|age<=24,select=c(q1,q2,q3,q4)) 
#选择了age大于35或小于24的观测，保留了q1倒q4标量
dd10 <- subset(leadership,gender="M"& age > 25,select=gender:q4) 
#选择25岁以上男性，保留变量gender到q4
#从数据集或数据框中随机抽样
sa1 <- leadership[sample(1:nrow(leadership),3,replace=FALSE),] 

part5#-----------------------------------------
###PART5 统计/概率/字符处理函数
# code5-1,均值和标准差
x <- c(1,2,3,4,5,6,7,8)
mean(x) #平均值
sd(x) #标准差
#按照定义计算
n <- length(x)
meanx <- sum(x)/n
css <- sum((x-meanx)^2)
sdx <- sqrt(css/(n-1)) 
meanx
sdx

### 任意均值和标准差的标准化
newdata <- scale(mydata) * SD + M #SD=1,M=0即转为均值为0，
# 标准差为1的标准正态分布

###正态分布函数
x <- pretty(c(-3,3),30)
y <- dnorm(x)
plot(x,y,type="l",xlab="normal deviate",ylab="density",yaxs="i")
pnorm(1.96)
qnorm(0.9,mean=500,sd=100)
rnorm(50,mean=50,sd=10)

###code5-2,伪随机数
runif(5) #生成服从正态分布的数
runif(5)
set.seed(1234) #通过设定种子，重现结果
runif(5)

#code5-3,生成服从多元正态分布数据
library(MASS)
options(digits=3) 
set.seed(1234)
mean <- c(230.7,146.7,3.6)
sigma <- matrix(c(15360.8,6721.2,-47.1,
                  6721.2,4700.9,-16.5,
                  -47.1,-16.5,0.3),nrow=3,ncol=3)
#生成500个随机观测
p5_df1 <- mvrnorm(500,mean,sigma) 
#矩阵转化为数据框
p5_df1 <- as.data.frame(p5_df1) 
#为变量指定名称
names(p5_df1) <- c("y","x1","x2") 
dim(p5_df1)
head(p5_df1,n=10)  
tail(p5_df1,n=10)  

# 字符处理
x <- c("ab","cde","fghij")
length(x) 
nchar(x[3]) #计算x中的字符数量
x <- "abcdef"
substr(x,2,4) #"bcd"，提取x的2到4位
substr(x,2,4) <- "22222" #替换x的2到4位
x #"a222ef"

#在x中搜索grep(pattern,x,fixed=TRUE),TRUE的话pattern为正则表达式，
# FALSE则pattern为字符串，见下面两个例子
# grep(pattern, x, ignore. case=FALSE,fixed=FALSE)
grep("A",c("A","b","c"),fixed=TRUE) #"1",例1
grep("A",c("b","A","c"),fixed=TRUE) #"2",例2

#在x中搜索pattern，并以文本replacement将其替换，
# sub(pattern, replacement, x,ignore.case=FALSE, fixed=FALSE)
#'\s'用来查找空白的正则表达式，'\\s'，'\'为转义符，只使用'\s'报错
sub("\\s",".","Hello Welcome to Macao",fixed = FALSE) 
# 当pattern为文本字符串时，fixed=true
sub("to",".","Hello Welcome to Macao",fixed = TRUE)

# strsplit(x, split, fixed=FALSE)，在split处分割字符向量x中的元素
y <- strsplit("abc","",fixed=TRUE) #为什么会输出列表？
unlist(y)[2]
sapply(y,"[",2) #看不懂？

# 复制粘贴
paste("x",1:3,sep="")
paste("x",1:3,sep="M")  
paste("Today is",Sys.Date())
paste("Today is",date())  
toupper("abc")  
tolower("ABC")
indices <- seq(1,10,2) #生成一个序列
y <- rep(1:3,2) #rep(x, n),将x重复n次
t <- cut(1:25,5,ordered_result = TRUE) #cut()连续型变量被分割为有n个水平的因子
x <- pretty(1:1000,50) #选取n+1个等间距的取整值，将连续型变量分割为n个区间
# cat(... , file ="myfile",append =FALSE)，连接...中的对象，并将其输出
firstname <- c("Jane")
cat("Hello",firstname,"\n")
# cat例子2
name <- "Bob"
#连接前两项，后面内容为转义字符，\n表示新行，\t为制表符，\'为单引号，\b为退格
#第二行缩进一个空格，因此使用退格转义字符"\b"
cat("Hello",name,"\b.\n","Isn\'t R","\t","Great?\n") 
cat("Hello",name,"\b.\n","Isn\'t R","\n","Great?\n")

#code5-4
a <- 5
sqrt(a)
b <- c(1.243,5.654,2.99)
round(b)
c <- matrix(runif(12),nrow=3)
log(c)
mean(c)
#code5-5
dd12 <- matrix(rnorm(30),nrow=6)
round(dd12,digits = 2) #两位小数
#MARGIN=1表示行，MARGIN=2表示列,apply()可将函数应用到某个维度上
apply(dd12,1,mean) #6行的均值
apply(dd12,2,mean) #5列的均值
#截尾均值，基于中间60%的数值，最高和最低的20%值被忽略
apply(dd12,2,mean,trim=0.2) 

#code5-6
options(digits=2) #digits(),小数点后两位
student <- c("John Davis","Angela Williams","Bullwinkle Moose","David Jones",
             "Janice Markhammer","Cheryl Cushing","Reuven Ytzrhak","Greg Knox",
             "Joel England","Mary Rayburn")
math <- c(502,600,412,358,495,512,410,625,573,522)
science <- c(95,99,80,82,75,85,80,95,89,86)
english <- c(25,22,18,15,20,28,15,30,27,18)
roster <- data.frame(student,math,science,english,stringsAsFactors=FALSE)
z <- scale(roster[,c(2:4)]) #标准化数据
score <- apply(z,1,mean) #各行均值
roster <- cbind(roster,score) 
y <- quantile(score,c(0.8,0.6,0.4,0.2)) #综合得分score的百分位数
roster$grade[score>=y[1]] <- "A" #百分位数排名重编码为新的类别型变量grade
roster$grade[score<y[1]&score>=y[2]] <- "B" 
roster$grade[score<y[2]&score>=y[3]] <- "c"
roster$grade[score<y[3]&score>=y[4]] <- "D"
roster$grade[score<y[4]] <- "F"
roster
name <- strsplit(roster$student," ") #将姓名拆分为姓氏和名字，返回一个列表
name
firstname <- sapply(name,"[",1) #提取列表name第1个元素成分
firstname
lastname <- sapply(name,"[",2) #"[" 是提取某个对象一部分的函数
lastname
roster <- cbind(firstname,lastname,roster[,-1]) #不需要student列变量，
roster <- roster[order(firstname,lastname),] #按firstname和lastname排列
roster #完成任务目标

#5.4,控制
# for循环重复地执行一个语句，直到某个变量的值不再包含在序列中为止
for(i in 1:10)
print("hello")
# while循环重复地执行一个语句，直到条件不为真为止
i <- 10
while(i>0){print("hello");i <- i - 1} #循环

grade
if(is.character(grade))  
grade <- as.factor(grade)
if(!is.factor(grade)) 
grage <- as.factor(grade) else print ("grade already is a factor")

ifelse(score>0.5,"passed","failed")
outcome <- ifelse(score>0.5,"passed","failed")
# switch示例
feelings <- c("sad","afraid")
for(i in feelings)
  print(
    switch(i,
           happy="I am glad you are hanppy",
           afraid="there is nothing to fear",
           sad="cheer up",
           angry="calm down now"
          )
       )
#code5-8,自编函数，计算参数统计量均值/标准差，和非参数统计量中位数/绝对中位差
mystats <- function(x,parametric=TRUE,print=FALSE){
  if(parametric){
    center <- mean(x);spread <- sd(x)
  } else{
    center <- median(x);spread <- mad(x)
  }
  if(print&parametric){
    cat("mean=",center,"\n","sd=",spread,"\n")
  } else if (print & !parametric){
    cat("median=",center,"\n","mad=",spread,"\n")
  }
  result <- list(center=center,spread=spread)
  return(result)
} 
set.seed(1234)
x <- rnorm(500)
y <- mystats(x)
y <- mystats(x,parametric=FALSE,print=TRUE)

#输出当天日期
mydate <- function(type="long") { 
  switch(type,
   long=format(Sys.Date(),"%A %B %d %Y"),
   short=format(Sys.Date(),"%m-%d-%Y"),
   cat(type, "is not a recognized type\n")
        )
}
mydate("long")
mydate("short")
mydate()
mydate("medium")

#code5-9，t()转置数据集
cars <- mtcars[1:5,1:4]
cars
t(cars)
#code5-10整合数据
options(digits=3)
attach(mtcars)
aggdatac <- aggregate(mtcars,by=list(cyl,gear),FUN = mean,na.rm=TRUE) #by的变量必须在列表中
aggdatac #group.1表示汽缸数cyl，group.2表示档位数gear
#同行410-412
with(mtcars,{
  data1 <- aggregate(mtcars,by=list(cyl,gear),FUN=mean,na.rm=TRUE)
  data1
})
#5.6.3
library("reshape2")
ID <- c(1,1,2,2)
Time <- c(1,2,1,2)
X1 <- c(5,3,6,2)
X2 <- c(6,5,1,4)
da1 <- data.frame(ID,Time,X1,X2)
da2 <- melt(da1,id=(c("ID","Time"))) #融合数据，每个测量变量独占一行，行中带有唯一确定这个测量值所需的标识符变量
da2
dcast(da2,ID~variable,mean) #仔细体会！
dcast(da2,Time~variable,mean)
dcast(da2,ID~Time,mean)
dcast(da2,ID+Time~variable)
dcast(da2,ID+variable~Time)
dcast(da2,ID~variable+Time)

part6#----------------------------------------
#PART6
#code6-1,条形图
library("vcd")
counts <- table(Arthritis$Improved) #读取数据集,table()提取各单元计数
counts
dev.new()
barplot(counts,
        main="simple barplot",
        xlab="improved",ylab="frequency")
dev.new()
barplot(counts,
        main="horizontal barplot",
        xlab="frequency",ylab="improved",
        horiz=TRUE) #水平条形图
#类别型变量是因子或有序型因子时，可直接创建条形图，无需表格化
dev.new()
plot(Arthritis$Improved,main="simple barplot",xlab="improved",ylab="frequency")
dev.new()
plot(Arthritis$Improved,horiz=TRUE,main="horizontal barplot",xlab="improved",ylab="frequency")
dev.off()
counts <- table(Arthritis$Improved,Arthritis$Treatment)
#堆砌/分组条形图
library("vcd")
counts <- table(Arthritis$Improved,Arthritis$Treatment)
counts
#code6-2
barplot(counts,  #堆砌
        main="stacked barplot",
        xlab="treatment",ylab="frequency",
        col=c("red","yellow","green"),
        legend=rownames(counts),
        args.legend=list(x="topright",inset=-0.1,cex=0.7))
barplot(counts,
        main="grouped barplot",
        xlab="treatment",ylab="frequency",
        col=c("red","yellow","green"),
        legend=rownames(counts),beside=TRUE, #分组
        args.legend = c(x=8,y=35,cex=0.8))
#code6-3
states <- data.frame(state.region,state.x77)
means <- aggregate(states$Illiteracy,by=list(state.region),FUN=mean)
means <- means[order(means$x),] #均值排序
barplot(means$x,names.arg=means$Group.1) #meanx是包含各条形高度的向量，names.arg=means$Group.1显示x轴标签
title("mean illiteracy rate")
opar <- par(no.readonly=TRUE) #条形图调整
par(mar=c(5,8,4,2))
par(las=2)
counts <- table(Arthritis$Improved)
barplot(counts,
        main="treatment outcome",
        horiz=TRUE,
        cex.names=0.8,
        names.arg=c("no improvement","some improvement","marked improvement"))
par(opar)
#6.1.5 棘状图，每个条形的高度为1
library(vcd)
attach(Arthritis)
counts <- table(Treatment,Improved)
spine(counts,main="spinogram example") #棘状图绘制
detach(Arthritis)
#code6-5饼图
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
slices <- c(10,12,4,16,8)
labels <- c("us","uk","australia","germany","france")
pie(slices,labels=labels,main="simple piechart")
pct <- round(slices/sum(slices)*100)
labels2 <- paste(labels," ",pct,"%",sep="")
pie(slices,labels=labels2,col=rainbow(length(labels2)),
    main="piechar with percentages") #标签转为比例值，rainbow()设置颜色
library(plotrix)
pie3D(slices,labels=labels,explode=0.1,main="3D piechart") #3维饼图
mytable <- table(state.region)
labels3 <- paste(names(mytable),"\n",mytable,sep="")
pie(mytable,labels=labels3,main="piechart from a table\n (with sample sizes)")
par(opar) 
#扇形图
library(plotrix)
opar <- par(no.readonly=TRUE)
par(mar=c(5,5,10,2))
slices <- c(10,12,4,16,8)
lbls <- c("us","uk","austrlia","germany","france")
fan.plot(slices,labels=lbls,main="fan plot") #各扇形图相互叠加，半径不同，重要的是扇形宽度
par(opar)
#code6-6直方图
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
hist(mtcars$mpg)
hist(mtcars$mpg,
     breaks=12, #控制组的数量
     col="red",
     xlab="mile per gallon",
     main="colored histogram with 12 bins")
hist(mtcars$mpg,
     freq=FALSE, #根据概率密度绘制
     breaks=12,
     col="red",
     xlab="miles per gallon",
     main="histogram, rug plot, density curve")
rug(jitter(mtcars$mpg)) #Q：轴须图
lines(density(mtcars$mpg),col="blue",lwd=2) #添加一条密度曲线
x <- mtcars$mpg
h <- hist(x,
          breaks=12,
          col="red",
          xlab="miles per gallon",
          main="histogram with norm curve and box")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x)) #Q
yfit <- yfit*diff(h$mids[1:2])*length(x) #Q 
lines(xfit,yfit,col="blue",lwd=2)
box()
par(opar)
#code6-7核密度图
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
d <- density(mtcars$mpg)
plot(d)
d <- density(mtcars$mpg) 
plot(d,main="kernel density of miles per gallon") #添加标题
polygon(d,col="red",border="blue") #曲线蓝色，填充红色
rug(mtcars$mpg,col="brown") #棕色轴须
par(opar)
#code6-8核密度图比较
# install.packages("sm")
library(sm)
opar <- par(no.readonly = TRUE)
par(lwd=2)
with(mtcars,{
  cyl.f <- factor(cyl,levels=c(4,6,8),labels=c("4 cylinder","6 cylinder","8 cylinder")) 
  sm.density.compare(mpg,cyl,xlab="miles per gallon") 
  title(main="mpg distribution by car cylinders")
  colfill <- c(2:(1+length(levels(cyl.f)))) #添加颜色向量
  legend("topright",levels(cyl.f),fill=colfill) #添加图例
  })
par(opar)
#箱线图
boxplot(mtcars$mpg,main="box plot",ylab="miles per gallon")
boxplot.stats(mtcars$mpg)
boxplot(mpg~cyl,data=mtcars,
        main="car mileage datas",
        xlab="number of cylinders",
        ylab="miles per gallon")

boxplot(mpg~cyl,data=mtcars,
        varwidth=TRUE, #箱线图宽度与样本大小成正比
        notch=TRUE, #含凹槽
        col="red", #红色填充
        main="car mileage datas",
        xlab="number of cylinders",
        ylab="miles per gallon")
#code6-9两个交叉因子的箱线图
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels=c(4,6,8),
                       labels=c("4","6","8")) #汽缸数量因子
mtcars$am.f <- factor(mtcars$am,
                      levels=c(0,1),
                      labels=c("auto","standard")) #变速箱类型因子
boxplot(mpg~am.f*cyl.f,
        data=mtcars,
        varwidth=TRUE,
        col=c("gold","darkgreen"),
        main="mpg distribution by auto type",
        xlab="auto type",ylab="miles per gallon")
#code6-10小提琴图
# install.packages("vioplot")
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1,x2,x3,
        names=c("4 cyl","6 cyl","8 cyl"),
        col="gold")
title("villin plots of miles per gallon",ylab="miles per gallon",xlab="number of cylinders")
#点图
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=1,
         main="gas mileage for car models",
         xlab="miles per gallon")
#code6-11,点图
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="black",
         color=x$color,
         pch=19,
         main="gas mileage for car models\ngrouped by cylinder",
         xlab="miles per gallon")

part7#--------------------------------------
###PART7
#7-1描述性统计分析
#给整体的数据计算描述统计量
myvars <- c("mpg","hp","wt")
head(mtcars[myvars])
summary(mtcars[myvars])
write.table(mtcars[myvars],"myvars.csv", row.names=TRUE,col.names=TRUE,sep=",")
write.table(mtcars,"mtcars.csv", row.names=TRUE,col.names=TRUE,sep=",")
#code7-2,sapply()可计算任意选择的描述性统计量,mystats函数计算了若干描述性统计量
mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n #偏度,正偏态右偏，负偏态左偏
  kurt <- sum((x-m)^4/s^4)/n-3 #峰度，大于0尖峰，小于0平峰
  return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt))
}
options(digits=3)
myvars <- c("mpg","hp","wt")
sapply(mtcars[myvars],mystats)
#code7-3通过Hmisc包describe()计算描述性统计量
myvars <- c("mpg","hp","wt")
library(Hmisc)
describe(mtcars[myvars]) #返回变量、观测的数量、缺失值和唯一值(未重复）的数目、平均值、分位数以及五个最大值和最小值
#code7-4astecs包中的stat.desc()函数计算描述性统计量
myvars <- c("mpg","hp","wt")
library(pastecs)
options(digits=3)
stat.desc(mtcars[myvars],norm=TRUE)
#code7-5psych包中的describe()计算描述性统计量
#Q:psych在Hmisc之后被载入，describe()函数被屏蔽（masked），
# 如果想使用Hmisc包中的版本，可以键入Hmisc::describe(mt)
myvars <- c("mpg","hp","wt")
library(psych)
describe(mtcars[myvars])
#描述分组数据统计量
#code7-6,aggregate()分组获取描述性统计量
myvars <- c("mpg","hp","wt")
aggregate(mtcars[myvars],by=list(am=mtcars$am),mean) #am是mtcars中的一列
#by=list(mtcars$am)，则am列将被标注为Group.1而不是am
aggregate(mtcars[myvars],by=list(am=mtcars$am),sd)
#aggregate()每次调用中使用单返回值函数，平均值/标准差

#code7-8,doBy包summaryBy()分组描述统计量
#install.packages("doBy")
library(doBy)
dstats <- function(x)(c(mean=mean(x),sd=sd(x))) 
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=dstats) #使用自编函数dstats
mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n 
  kurt <- sum((x-m)^4/s^4)/n-3 
  return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt))
}
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=mystats) #使用自编函数mystats
#code7-9,psych包describeBy()分组描述统计量
library(psych)
describeBy(mtcars[myvars],list(mtcars$am))
##reshape包分组描述统计量
library(reshape)
dstats <- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
dfm <- melt(mtcars,measure.vars=c("mpg","hp","wt"),id.vars=c("am","cyl"))
cast(dfm,am+cyl+variable~.,dstats) #variable为字面意思表示重组后数据框中的变量

#7.2频数表和列联表
library(vcd)
head(Arthritis)
write.table(Arthritis,"Arthritis.csv",sep=",")
tab1 <- with(Arthritis,table(Improved));tab1 #一维列联表,table()函数
prop.table(tab1) #比例形式
prop.table(tab1)*100 #百分比形式

#二维列联表
tab2 <- table(Arthritis$Treatment,Arthritis$Improved);tab2 
tab3 <- xtabs(~Treatment+Improved,data=Arthritis);tab3 #行变量和列变量名称均显示
# 行和与行比例
margin.table(tab3,1) #1指treatment
prop.table(tab3,1) 
# 列和与列比例
margin.table(tab3,2) #2指improved
prop.table(tab3,2) 
prop.table(tab3) #各单元格所占比例，单元格数目除以总数比例

#addmargins()函数计算行与列和
addmargins(tab3) #边际和，添加了各行和与各列和
addmargins(prop.table(tab3)) #比例求和
# addmargins 下标对应的有点不明白？
addmargins(tab3,1) #improved 列和Q
addmargins(tab3,2) #treatment 行和Q 和前面的不太一致
addmargins((prop.table(tab3,1)),2) #？
addmargins((prop.table(tab3,2)),1) #？

###code7-10CrossTable生成二维列联表
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved) 

#code7-11三维列联表
# Treatment需要通过下标1来引用，Sex通过下标2来引用，Improve通过下标3来引用
tab4 <- xtabs(~Treatment+Improved+Sex,data=Arthritis);tab4
#ftable显示多维列联表各单元格频数
ftable(tab4) 
margin.table(tab4,1) #第一个元素Treatment
margin.table(tab4,2) #第二个元素Sex
margin.table(tab4,3) #第三个元素Improved
margin.table(tab4,c(1,3)) #1treatmen和3improved的边际频数
prop.table(tab4,c(1,2)) #1treatment和sex的改善情况
ftable(prop.table(tab4)) 
ftable(prop.table(tab4,c(1,2))) #第1、2变量比例
ftable(addmargins(prop.table(tab4,c(1,2)),3)) #第1、2变量比例，为第3个下标添加边际和

ftable(prop.table(tab4,c(1,3))) #第1、3变量比例
ftable(addmargins(prop.table(tab4,c(1,3)),2)) #第1、3变量比例，为第2个下标添加边际和

ftable(prop.table(tab4,c(2,3))) #第2、3变量比例
ftable(addmargins(prop.table(tab4,c(2,3)),1)) #第2、3变量比例，为第1个下标添加边际和

#code7-12卡方独立性检验
###显著性检验评估了是否存在充分的证据以拒绝原假设H0
# 类别型结果变量组间比较
library(vcd)
tab3 <- xtabs(~Treatment+Improved,data=Arthritis);tab3 
chisq.test(tab3) #检验接受的治疗和改善水平关系，H0：接受/不接受治疗情况相同
# p<0.05,拒绝H0，说明治疗和改善水平存在关系
tab5 <- xtabs(~Improved+Sex,data=Arthritis);tab5 
chisq.test(tab5)#检验性别和改善水平关系，H0：男/女改善水平一致
# p>0.05,无法拒绝H0，说明性别和改善水平无关

#Fisher检验
tab3 <- xtabs(~Treatment+Improved,data=Arthritis);tab3 
fisher.test(tab3) #检验treatment和improved关系，H0：接受/不接受治疗情况相同
# p值小于0.05，拒绝假设

#Cochran—Mantel—Haenszel卡方检验，分层卡方检验
tab4 <- xtabs(~Treatment+Improved+Sex,data=Arthritis);tab4
mantelhaen.test(tab4) #检验treatment与得到的improved在性别的每一水平下的关系
# H0:treatment和improved在性别的每一水平下相等，p<0.05,拒绝H0

#code7-13相关性度量
library(vcd)
tab3 <- xtabs(~Treatment+Improved,data=Arthritis);tab3 
assocstats(tab3) #计算phi系数，列联系数和cramer's V系数，较大值相关性较强

##列联表转换为扁平格式，将已有列联表转换为行为观测值列为变量的格式
#code7-15,可以将任意R中的表格转为一个扁平格式的数据框
#code7-16,保存期刊列联表数据为扁平格式
treatment <- rep(c("Placebo","Treated"),times=3);treatment
improved <- rep(c("None","Some","Marked"),each=2);improved
Freq <- c(29,13,7,17,7,21);Freq
mytable <- as.data.frame(cbind(treatment,improved,Freq));mytable
table2flat <- function(mytable){  
  df <- as.data.frame(mytable)
  rows <- dim(df)[1]
  cols <- dim(df)[2]
  x <- NULL
  for(i in 1:rows){
    for (j in 1:df$Freq[i]) {
      row <- df[i,c(1:(cols-1))]
      x <- rbind(x,row)
    }
  }
  row.names(x) <- c(1:dim(x)[1])
  return(x)
} #自定义函数
df_table <- table2flat(mytable);df_table
write.csv(df_table,"df_table.csv",row.names = TRUE) 

#code7-14 相关系数和协方差
# Pearson积差相关系数衡量了两个定量变量之间的线性相关程度。Spearman等级相关系数
# 则衡量分级定序变量之间的相关程度。Kendall’s Tau相关系数也是一种非参数的
# 等级相关度量
states <- state.x77[,1:6];states 
dim(state.x77[,1:6]) #50行6列
write.csv(state.x77,"states.csv",row.names = TRUE)
cor(states) #相关系数，默认pearson相关系数
cov(states) #协方差
cor(states,method = "kendall") #kendall相关系数
cor(states,method="spearman") #spearson相关系数
###计算非方形矩阵的相关性
x <- states[,c("Population","Income","Illiteracy","HS Grad")];x
y <- states[,c("Life Exp","Murder")];y
cor(x,y)

#偏相关
# 偏相关是指在控制一个或多个定量变量时，另外两个定量变量之间的相互关系
#install.packages("ggm")
library("ggm")
states <- state.x77[,1:6];states 
#示例：控制了收入、文盲率和高中毕业率，人口和谋杀率的相关系数
pcor(c(1,5,2,3,6),cov(states)) #前两个population/murder为要计算相关系数的
# 变量下标，其余2/3/6为要排除影响变量的下标

#对变量相关系数进行统计显著性检验
states <- as.data.frame(states) #原states一维？is.atomic=true
cor.test(states$Illiteracy,states$Murder)
cor.test(states[,3],states[,5]) #原假设:相关系数为0

# 计算矩阵相关系数并进行显著性检验
library(psych)
corr.test(states,use="complete") #pairwise/complete对缺失值执行成对/行删除
#检验在控制一个或多个额外变量时两个变量之间的条件独立性
pcor.test(pcor(c(1,5,2,3,6),cov(states)),c(2,3,6),50)

###连续性结果变量的组间比较
#检验两个总体均值相等的假设，针对两组独立样本的t检验
# 假设两组数据是独立的，并且是从正态总体中抽得
library(MASS)
# t.test假定方差不平等
t.test(Prob~So,data=UScrime) #原假设：南方各州group1和
# 非南方各州group2拥有相同监禁概率,p<0.001,拒绝原假设,监禁概率有统计差异
write.csv(UScrime,"UScrime.csv",row.names=TRUE) 

#非独立样本t检验，本例为年轻和年长平均失业率比较，两组数据不独立
library(MASS)
sapply(UScrime[c("U1","U2")],function(x)(c(mean=mean(x),sd=sd(x)))) #均值
with(UScrime,t.test(U1,U2,paired=TRUE)) #H0：年长和年轻男性平均失业率相同
#p<0.01,拒绝原假设

#非参数检验，Wilcoxon秩和检验
with(UScrime,by(Prob,So,median)) #So分组的Prob均值，两组独立样本
wilcox.test(Prob~So,data=UScrime) 
#H0：南方So1和非南方各州So0监禁率相同的假设，p<0.05,拒绝假设，
# 结果同参数检验相同，参见上面独立样本t检验

#非独立样本非参数检验，Wilcoxon秩和检验
sapply(UScrime[c("U1","U2")],median) #两组非独立样本数据
with(UScrime,wilcox.test(U1,U2,paired=TRUE)) #paired可以不添加
#p<0.01,拒绝原假设，结论与非独立样本t检验相同

#比较四个地区（东北部、南部、中北部和西部）的文盲率
states <- state.x77[,1:6];states 
states_1 <- as.data.frame(cbind(state.region,states));states_1
write.csv(states_1,"states_1.csv")
#若无法满足ANOVA，且各组独立使用kruskal检验
kruskal.test(Illiteracy~state.region, data=states_1) 
# p<0.05,拒绝原假设

##非参数多组比较
source("wmc.txt")
states <- data.frame(state.region,state.x77);states 
wmc(Illiteracy~state.region,data=states,method="holm")
#多次比较各地区差异，west/South;North Central/South;Northeast/South统计差异大

part8#---------------------------------------
###PART8
#code8-1,一元/多项式回归
 #拟合线性模型的基本函数lm(formula,data),formula模型形式，data拟合模型的数据
#formula Y~X1+X2+...+Xk
# 左边为响应变量，右边为各个预测变量，预测变量之间用加号分隔
write.csv(women,"women.csv")
dim(women)
fit1 <- lm(weight~height,data=women);fit1 #显示截距和斜率
summary(fit1) #回归系数t检验，H0:β1=0，拒绝回归系数3.45显著不为0，
# 表明身高增加1寸，体重预期增加3.45磅
#r2表明模型解释99.1%方差，残差1.53可视为模型用身高预测体重的平均误差 
confint(fit1) #置信区间
fitted(fit1) #预测值
residuals(fit1) #残差值，等同women$weight - fitted(fit1)
plot(women$height,women$weight,xlab = "height (in inches)",ylab = "weight(in pounds)")
abline(fit1)

#添加二项式提高回归的预测精度，code8-2
fit2 <- lm(weight~height+I(height^2),data=women) 
#I()算术的角度来解释括号中的元素
summary(fit2)
plot(women$height,women$weight,xlab="height(in inches",ylab = "weight(in lbs)")
lines(women$height,fitted(fit2))

#添加一个三项式
fit3 <- lm(weight~height+I(height^2)+I(height^3),data=women)
summary(fit3)
plot(women$height,women$weight,xlab="height(in inches",ylab = "weight(in lbs)")
lines(women$height,fitted(fit3))

#scatterplot()绘制二元关系图
library(car)
#身高与体重的散点图，直线为线性拟合，虚线为曲线平滑拟合，边界为箱线图
scatterplot(weight~height,data=women, 
            smooth=TRUE,lty=2,
            pch=19,main="women age 30-39",
            xlab="height",ylab="weight")

#code8-5有交互项的多元线性回归,effect()展示交互的结果
fit5 <- lm(mpg~hp+wt+hp:wt,data=mtcars)
summary(fit5)
library(effects) 
plot(effect("hp:wt",fit5,xlevels=list(wt=c(2.2,3.2,4.2))),multiline=TRUE)

#===
#统计假设
###评价模型拟合情况
#正态性：自变量固定，因变量呈正态分布，则残差值呈均值为0的正态分布，若满足
#正态性假设，QQ图落在呈45度角的直线上；独立性：因变量是否相互独立需从来源判断；
#线性：自变量因变量线性相关，则残差值和拟合值没有系统关联，residuals vs fitted;
#同方差性：scale-location图中，水平线周围点随机分布；residuals vs leverage图
#鉴别离群点/高杠杆值点和强影响点
fit1 <- lm(weight~height,data=women)
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(fit1)
par(opar) #要先定义opar

#添加二项式后评价模型拟合情况
fit2 <- lm(weight~height+I(height^2),data=women)
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(fit2) #residuals vs fitted效果变好
par(opar)

#根据上述结果删除点13和15后拟合模型的评价图,点13qq图，15强影响点
fit_omit <- lm(weight~height+I(height^2),data=women[-c(13,15),])
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(fit_omit)
par(opar)

#多元线性回归，多个预测变量，如探究一个州的犯罪率和其他因素的关系
df1 <- as.data.frame(state.x77[,c("Murder","Population",
                                  "Illiteracy","Income","Frost")]);df1
cor(df1) #多元回归分析第一步先检查变量之间的相关性，
# cor()函数计算两个变量之间的相关系数
###区别变量间相关系数/回归系数
library(car)
# 散点图矩阵，添加平滑和线性拟合曲线
scatterplotMatrix(df1,smooth=TRUE,lty=2,main="scatter plot matrix")

#code8-4多元线性回归
df1 <- as.data.frame(state.x77[,c("Murder","Population",
                                  "Illiteracy","Income","Frost")]);df1
fit4 <- lm(Murder~Population + Illiteracy + Income + Frost,data=df1);fit4
summary(fit4)

#states多元回归的置信区间
confint(fit4)

#states的多元回归评价
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(fit4) #满足统计假设
par(opar)

# car包方法统计假设检验
library(car)
#8.3.2正态性假设检验
# n-p-1个自由度的t分布下的学生化残差（studentized residual，也称学生化删除残差
# 或折叠化残差）图形，其中n是样本大小，p是回归参数的数目（包括截距项）
qqPlot(fit4,labels=row.names(states),id=list(method="identify"),simulate=TRUE,
       main="Q-Q plot") #identify交互式选择，simulate=true，置信区间为95%

#对上一行结果中选择的nevada真实值和预测值比较
states["Nevada",] #提取行观测值nevada
fitted(fit4)["Nevada"] #预测值3.9%
residuals(fit4)["Nevada"] 
rstudent(fit4)["Nevada"]

#code8-6 绘制学生化残差图的函数，直方图，并添加正态曲线、核密度曲线和轴虚图
# 自定义residplot函数查看离群点，结合qqplot更直观
residplot <- function(fit,nbreaks=10){ #数据集fit4
  z <- rstudent(fit)
  hist(z,breaks = nbreaks,freq = FALSE,
  xlab = "studentized residual",
  main="distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),
        add=TRUE,col="blue",lwd=2)
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("normal curve","kernel density curve"),
         lty=1:2,col = c("blue","red"),cex=0.7)
}
residplot(fit4)

#检验因变量是否相互独立
durbinWatsonTest(fit4) # 检验时间序列数据的相关性，每个数据与之后一个数据比较，
# 适用于时间独立的数据,p=0.256说明误差项(因变量)相互独立，无自相关性

#自变量因变量是否线性关系
crPlots(fit4)

#code8-7 检验同方差性
ncvTest(fit4) #零假设为误差方差不变，p>0.05，接受H0
spreadLevelPlot(fit4) #水平随机分布，残差同方差性

###code8-8 线性模型假设的综合检验
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit4)
summary(gvmodel) #模型假设提供了单独的综合检验（通过/不通过）

###code8-9 检验多重共线性
vif(fit4)
sqrt(vif(fit4))>2  #值大于2表明存在多重共线性问题，逻辑判断

#8.4异常观测值：离群点，高杠杆值点和强影响点
#离群点，q-q图置信区间外或标准化残差值绝对值大于2.求最大标准化残差绝对值
# Bonferroni调整后的p值
outlierTest(fit4) #Nevada被判定为离群点

###高杠杆值观测点，即是与其他预测变量有关的离群点
# 通过帽子统计量判断高杠杆值，帽子均值p/n，p是模型估计的参数数目，n是样本量，
# 若观测点帽子值大于均值2至3倍，被认定为高杠杆值点
hat.plot <- function(fit4){
  p <- length(coefficients(fit4))
  n <- length(fitted(fit4))
  plot(hatvalues(fit4),main="index plot of hat values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit4),names(hatvalues(fit4)))
}
hat.plot(fit4)

#cook's S值鉴别强影响点，值大于4/（n-k-1)时表明是强影响点，
# n样本量大小，k是预测变量大小
cutoff <- 4/(nrow(states)-length(fit4$coefficients)-2)
plot(fit4,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red") #若设定D=1为标准，则无强影响点
#判断Alaska、Hawaii和Nevada是强影响点，但不知道如何影响

#变量添加图
avPlots(fit4,ask=FALSE,id=TRUE)#图中的直线表示相应预测变量的
# 实际回归系数。你可以想象删除某些强影响点后直线的改变，以此来估计它的影响效果

###将离群点、杠杆值和强影响点整合到一幅图
influencePlot(fit4,id=list(method="noteworthy"),main="influence plot",
              sub="circle size is proportional to Cook's distance")
#学生残差-2到2，离群点，Nevada/Rhode Island
#横轴hatvalue大于0.2或0.3，高杠杆值，Alaska/California
#圈大小cookD判断强影响点，Alaska/Nevada 

####模型不符合正态假设的处理
#删除对应的数据点
#变量变换
#添加或删除变量
#改用其他回归方法

# 对响应变量变换，powerTransform()通过最大似然估计来正态化响应变量
#code8-10，Box-Cox正态变换
library(car)
states <- data.frame(state.region,state.x77);states 
summary(powerTransform(states$Murder))
#可用0.5次幂转换因变量
# 但是H0：λ=0，p<0.05,拒绝；λ=1，p=0.145,无法拒绝，因此无需变换

#car包中的boxTidwell()通过获得预测变量幂数的最大似然估计来改善线性关系
boxTidwell(Murder~Population+Illiteracy,data=states) 
#变换自变量（人口和文盲率）来预测谋杀率模型
# population λ=0.87和illiteracy 1.36合适，但是各自检验p表明无需变换

#code8-11,比较嵌套模型的拟合优度，嵌套指模型的一些项完全包含在另一个模型中
# 在states的多元回归模型中，我们发现Income和Frost的回归系数不显著，此时你可以
# 检验不含这两个变量的模型与包含这两项的模型预测效果是否一样好
fit_com <- lm(Murder~Population+Illiteracy+Income+Frost,data=states)
fit_part <- lm(Murder~Population+Illiteracy,data=states)
anova(fit_com,fit_part) #检验不显著
# fit_part嵌套在fit_com中，检验表明income和frost可删除，无需添加到模型中
#AIC赤池信息准则考虑了模型的统计拟合度和用来拟合的参数数目，
# AIC值越小的模型要优先选择
AIC(fit_com,fit_part) #没有Income和Frost的fit_part更佳

#code8-13,向前向后逐步回归，逐步回归法评判模型,逐渐添加或减小变量
library(MASS)
stepAIC(fit_com,direction="backward")
# 通过aic值选择模型，但不能保证模型是最佳模型

#全子集回归
# code8-14，states数据进行全子集回归
library(leaps)
leaps_vars <- regsubsets(Murder~Population+Illiteracy+Income+Frost,
                         data=states,nbest=4) #nbest值：n个不同子集大小
#nbest=2先展示两个单预测变量模型，然后展示两个双预测变量模型，依次类推
plot(leaps_vars,scale="adjr2") #adjr2调整r平方值
# adjr2值含义是预测变量解释响应变量的程度
#结果表明含population和illiteracy双预测变量模型最佳

#基于Mallows Cp统计量模型评价
library(car)
subsets(leaps_vars,statistic = "cp",
        main="cp plot for all subsets regression") #手动添加图例
abline(1,1,lty=2,col="red") #添加截距和斜率均为1的直线
#越好的模型离截距项和斜率为1的直线越近，结果有4个较优模型，
#含Population和Illiteracy的双变量模型;含Population、Illiteracy和Frost的三变量模
#型,或Population、Illiteracy和Income的三变量模型（它们在图形上重叠了，不易分辨
# ）;含Population、Illiteracy、Income和Frost的四变量模型

#交叉验证，部分数据作为训练样本，部分作为保留样本，在训练样本上获取回归方程，在
# 保留样本上做预测
# code8-15，k重交叉验证，样本被分为k个子样本，轮流将k-1个子样本组合作为训练集，
# 另一个作为保留集，这样会获得k 个预测方程，记录k 个保留样本的预测表现结果，
# 然后求其平均值
#install.packages("bootstrap")
library(bootstrap)
shrinkage <- function(fit,k=10){ #10重交叉验证
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x,y,theta.fit,theta.predict,ngroup=k)
  r2 <- cor(y,fit$fitted.values)^2
  r2cv <- cor(y,results$cv.fit)^2
  cat("Original R-square =",r2,"\n")
  cat(k,"Fold Cross-Validated R-square =",r2cv,"\n")
  cat("Change =",r2-r2cv,"\n")
} #创建了包含自变量和因变量的矩阵，可获得初始R平方和交叉验证的R平方
df1 <- as.data.frame(state.x77[,c("Murder","Population",
                                  "Illiteracy","Income","Frost")]);df1
fit4 <- lm(Murder~Population + Illiteracy + Income + Frost,data=df1);fit4
shrinkage(fit4) #交叉验证的R平方减少0.12，初始样本回归r2过于乐观
fit6 <- lm(Murder~Population+Illiteracy,data=df1);fit6
shrinkage(fit6)#比初始只减少0.04，双预测变量模型更合适，r平方减少越少预测越精确

#预测变量相对重要性,方法1
#将数据标准化为均值为0，标准差为1的数据集，scale()返回矩阵，lm()函数要求的数据框
states_z <- as.data.frame(scale(df1));states_z 
fit_z <- lm(Murder~Population+Illiteracy+Income+Frost,states_z) 
coef(fit_z) #其他预测变量不变，预测变量一个标准差的变化导致响应变量标准差变化
#当其他因素不变时，文盲率一个标准差的变化将增加0.68个标准差的谋杀率

###code8-17,方法2，计算预测变量的相对权重
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar,2:nvar]
  rxy <- R[2:nvar,1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare)*100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=",round(rsquare,digits = 3)),
          ...)
  return(import)
}
relweights(fit4,col="lightgrey") 
#可以看到各个预测变量对模型方差的解释程度，Illiteracy解释了
# 59%的R平方，Frost解释了20.79%

part9#-----------------------------------------
# PART9,方差分析
rm(list = ls())
library("car")
#install.packages("gplots")
library("gplots")
# install.packages("HH")
library("HH")
# install.packages("rrcov")
# install.packages("robustbase")
# install.packages("DEoptimR")
# install.packages("pcaPP")
library("rrcov")
# install.packages("mvoutlier")
library("mvoutlier")

#code9-1，单因素方差分析，比较分类因子定义的两个或多个组别中的因变量均值
library("multcomp")
attach(cholesterol)
str(cholesterol)
write.csv(cholesterol,"cholesterol.csv",row.names=TRUE,)
table(cholesterol[,"trt"]) #查看治疗方法
# aggregate(response,by=list(trt = cholesterol$trt),FUN=mean)
# aggregate(response,by=list(trt = cholesterol$trt),FUN=sd)
# 查看各疗法均值和sd
library(reshape)
dstats <- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
dfm <- melt(cholesterol,measure.vars="response",id.vars="trt")
cast(dfm,trt+variable~.,dstats) #variable为字面意思表示重组后数据框中的变量
aov_1 <- aov(response ~ trt) #检验5种疗法组间差异
summary(aov_1) #H0均值相等，F检验显著，拒绝H0，
# 绘制带有置信区间的组均值图形
library(gplots)
plotmeans(response~trt,xlab="treatment",ylab="response",
          main="mean plot\nwith 95% CI") 

# 对各组均值差异成对检验
library("multcomp")
TukeyHSD(aov_1) #成对组间比较，p<0.05,差异显著
# 组间成对比较图形
opar <- par(no.readonly=TRUE)
par(las=2) #旋转轴标签
par(mar=c(5,8,4,2)) #增大左边界的面积
plot(TukeyHSD(aov_1)) #置信区间包含0的说明差异不显著
par(opar)

#成对组间比较，箱线图形式
library("multcomp")
opar <- par(no.readonly=TRUE)
par(mar=c(5,4,6,2))
tuk <- glht(aov_1,linfct=mcp(trt="Tukey"))
plot(cld(tuk,level=0.05),col="lightgrey") #有相同字母的组说明组间差异不显著
par(opar)

# 单因素方差分析中假设条件：因变量服从正态分布，各组方差相等
#正态性检验，Q-Q图
library("multcomp")
library("car")
attach(cholesterol)
qqPlot(lm(response~trt,data=cholesterol),simulate=TRUE,main="Q-Q plot",
       labels=FALSE) #数据落在95%置信区间范围内，满足正态性假设
#方差齐性检验，Bartlett检验
bartlett.test(response~trt,data=cholesterol) #H0五组方法效果方差相等
#p值0.97，无统计显著不同
#离群点检验
outlierTest(aov_1) #离群点检测，没有离群点

# 单因素协方差分析，code9-3，协变量为时间
data(litter,package="multcomp")
write.csv(litter,"litter.csv",row.names=TRUE)
attach(litter)
table(dose) #不同剂量幼崽数
# 不同剂量幼崽重量均值
library(reshape)
dstats <- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
dfm <- melt(litter,measure.vars="weight",id.vars="dose")
cast(dfm,dose+variable~.,dstats) #variable为字面意思表示重组后数据框中的变量
# 因变量weight，dose自变量，gesttime协变量
weight_aov <- aov(weight~gesttime+dose) 
summary(weight_aov)
#F检验p<0.05,1)gesttime与weight相关;2)控制gesttime，dose与weight相关

# 去除协变量gesttime后的组均值
library(effects)
effect("dose",weight_aov)

# code9-4,假定未用药条件与其他三种条件的比较检验,不明白自定义contrast这行Q
library("multcomp")
contrast <- rbind("no drug vs. drug"=c(3,-1,-1,-1))#第一组和其他三组均值进行比较
summary(glht(weight_aov,linfct=mcp(dose=contrast))) 
#p<0.05,表明未用药组比其他组出生体重高

# ANCOVA前提和ANOVA相同，需要正态性和同方差性假设，此外还需要假定回归斜率相同
#正态性检验，Q-Q图
library("multcomp")
library("car")
attach(litter)
qqPlot(lm(weight ~ gesttime + dose,data=cholesterol),simulate=TRUE,
       main="Q-Q plot",labels=FALSE) #正态分布
#方差齐性检验，Bartlett检验
bartlett.test(weight ~ gesttime,data=litter) #同方差性，无统计显著不同
#离群点检验
outlierTest(weight_aov) #没有离群点

#code9-5,检验回归斜率同质性，不明白，假定四种剂量处理通过gesttime预测
# weight的回归效率相同Q
library("multcomp")
aov_2 <- aov(weight~gesttime * dose,data = litter) #包含gesttime和dose交互项
summary(aov_2) #F检验交互效应不显著，支持了斜率相等的假设

# HH包中的ancova()绘制因变量、协变量和因子之间的关系图
library("HH")
ancova(weight~gesttime+dose,data=litter) #四种剂量处理gesttime和weight关系图,
# 回归线相互平行，截距项不同
ancova(weight~gesttime*dose,data=litter) #0剂量违背同斜率Q

### code9-6,双因素方差分析
attach(ToothGrowth)
write.csv(ToothGrowth,"ToothGrowth.csv",row.names=TRUE)
table(supp,dose)
aggregate(len,by=list(supp = ToothGrowth$supp,dose = ToothGrowth$dose),FUN=mean)
aggregate(len,by=list(supp = ToothGrowth$supp,dose = ToothGrowth$dose),FUN=sd)
aov_tooth <- aov(len ~ supp * dose)
summary(aov_tooth) #主效应和交互效应都很显著

#可视化双因素方差分析的交互效应,方法1
interaction.plot(dose,supp,len,type="b",col=c("red","blue"),pch=c(16,18),
                 main="interaction between dose and supplement type")
#可视化双因素方差分析的交互效应，方法2，添加置信区间/均值/样本大小
library("gplots")
plotmeans(len~interaction(supp,dose,sep=" "),
          connect=list(c(1,3,5),c(2,4,6)), ##连线，按照组间
          col=c("red","darkgreen"),
          main="interaction plot with 95% CIs",
          xlab="treatment and dose combination")
###方法3，对任意顺序的因子设计的主效应和交互效应可视化
ibrary("HH")
interaction2wt(len ~ supp * dose)

# 在某浓度二氧化碳的环境中，对寒带植物与非寒带植物的光合作用率进行了比较，
# 重复测量方差分析（长数据格式），组内+组间因子，本例type组间因子，conc组内因子
attach(CO2);CO2
head(CO2)
write.csv(CO2,"CO2.csv",row.names=TRUE,)
CO2_chil <- subset(CO2,Treatment=="chilled") #提取treat为chilled的数据集
# type组间因子，主效应
aov_chil <- aov(uptake~conc*Type+Error(Plant/(conc),CO2_chil))#error 不明白Q
summary(aov_chil)
opar <- par(no.readonly = TRUE)
par(las=2,mar=c(10,4,4,2))
# conc-uptake图
with(CO2_chil,interaction.plot(conc,Type,uptake,type="b",col=c("red","blue"),
    pch=c(16,18),main="interaction plot for plant type and concentration"))
# uptake~Type*conc，type和conc不能换顺序，看起来画图和aov不一样
boxplot(uptake~Type*conc,data=CO2_chil,col=c("gold","green"),
        main="Chilled Quebec and Mississippi Plants",
        ylab="CO2 uptake rate (umol/m^2 sec)")
par(opar)

# 研究美国谷物中的卡路里、脂肪和糖含量是否会因为储存架位置的不同而发生变化
library("MASS")
attach(UScereal) #因变量卡路里、脂肪和糖含量，自变量货架三水平1、2、3
write.csv(UScereal,"UScereal.csv",row.names=TRUE)
usce_1 <- cbind(calories,fat,sugars) #3个因变量组合
write.csv(usce_1,"usce_1.csv",row.names = TRUE)
aggregate(usce_1,by=list(shelf = UScereal$shelf),FUN=mean)
#组间差异多元检验
usce_aov1 <- manova(usce_1~shelf) #结果表明三个组营养成分测量值显著不同
summary(usce_aov1)
# 多元检验显著，进而对每一个变量做单因素方差分析
summary.aov(usce_aov1) #每种营养成分测量值都不同

##对单元素组间方差分析，先要将自变量转为因子数据
shelf <- as.factor(UScereal[,"shelf"]) #shelf转为factor数据
plot(TukeyHSD(aov(calories ~ shelf),data = UScereal)) #可视化数据

#单因素多元方差分析两个前提：多元正态性和方差-协方差矩阵同质性
# 多元正态性：因变量组成的向量服从一个多元正态分布，QQ图
center <- colMeans(usce_1) #不是很明白Q
n <- nrow(usce_1)
p <- ncol(usce_1)
cov <- cov(usce_1)
d <- mahalanobis(usce_1,center,cov)
dev.new() 
coord <- qqplot(qchisq(ppoints(n),df=p),
         d,main="qq plot assessing multivariate normality",
         ylab="mahalanobies D2")
abline(a=0,b=1)
identify(coord$x,coord$y,labels=row.names(UScereal))
dev.off()
#删除点64Wheaties Honey Gold和65Wheaties后多元正态检验
usce_2 <- cbind(calories,fat,sugars) #删除不符合正态分布点
n <- nrow(usce_2)
p <- ncol(usce_2)
cov <- cov(usce_2)
d <- mahalanobis(usce_2,center,cov)
dev.new() 
coord <- qqplot(qchisq(ppoints(n),df=p),
                d,main="qq plot assessing multivariate normality",
                ylab="mahalanobies D2")
abline(a=0,b=1)
identify(coord$x,coord$y,labels=row.names(UScereal)) #结果和未删除时差不多Q
dev.off()

# 检验各组的协方差齐性，box's M，可以不看这个检验
box_m(usce_1,UScereal$shelf) #p值显著，
box_m(usce_2,UScereal$shelf) #报错Q

# 检验多元离群点
library(mvoutlier)
outliers <- aq.plot(usce_1) #31/32离群

# 若多元正态性胡总和方差-协方差均值假设不满足，或者多元离群点较多
# 稳健单因素MANOVA或非参数MANOVA
library("rrcov")
Wilks.test(usce_1,shelf,method="mcd") #各shelf谷物营养成分含量不同
Wilks.test(usce_2,shelf,method="mcd") #报错Q

# Q 使用回归思路做anova
library("multcomp")
attach(cholesterol)
levels(cholesterol$trt) #trt因子数据
aov_chole <- aov(response~trt) #aov()拟合模型
summary(aov_chole)

#不明白Q线性模型lm()处理因子，使用一系列与因子水平相对应的数值型对照变量
# 代替因子若因子有k个水平，则创建k-1个对照变量，因子的第一个水平变成了参考组，
# 随后的变量都以它为标准
contrasts(cholesterol$trt) #编码过程
lm_chole <- lm(response~trt,data=cholesterol) 
summary(lm_chole)

part10#-----------------------------------------
###PART10 Q
# 功效分析针对的是假设检验
# 功效分析可以帮助在给定置信度的情况下，判断检测到给定效应值时所需的样本量
# 也可以帮助你在给定置信度水平情况下，计算在某样本量内能检测到给定效应值的概率
#四个量：样本大小、显著性水平(发现效应不发生的概率)、功效(真实效应发生的概率)
# 和效应值(在备择或研究假设下效应的量)Q
library("pwr")
# t检验
# 假定将使用双尾独立样本t检验来比较两种情况下驾驶员的反应时间均值。
# 如果你根据过去的经验知道反应时间有1.25 s的标准偏差，并认定反应时间1 s的差值是
# 巨大的差异，那么在这个研究中，可设定要检测的效应值为d=1/1.25=0.8或者更大。
# 另外，如果差异存在，你希望有90%的把握检测到它，由于随机变异性的存在，你也希望
# 有95%的把握不会误报差异显著
pwr.t.test(d=0.8,sig.level=0.05,power=0.9,type="two.sample",
           alternative="two.sided") #n=34

# 假定在比较这两种情况时，你想检测到总体均值0.5个标准偏差的差异，并且将误报差
# 异的几率限制在1%内。此外，你能获得的受试者只有40人。那么在该研究中，你能检测
# 到这么大总体均值差异的概率是多少
pwr.t.test(n=20,d=0.5,sig.level=0.01,type="two.sample",
           alternative="two.sided") #power=0.14

# 样本大小不同t检验,n1和n2是两组的样本大小
# pwr.t2n.test(n1=,n2=,d=,sig.level=,power=,alternative= )

# 对五个组做单因素方差分析，要达到0.8的功效，效应值为0.25，
# 并选择0.05的显著性水平，计算各组需要的样本大小
# 对于单因素方差分析，效应值可通过f来衡量
pwr.anova.test(k=5,f=0.25,sig.level = 0.05,power=0.8) #n=5*39

# 对相关性分析进行功效分析
# r是效应值（通过线性相关系数衡量）
# 研究抑郁与孤独的关系。你的零假设和研究假设为：H0:ρ ≤ 0.25 和 H1:ρ > 0.25
# 其中，ρ是两个心理变量的总体相关性大小(r)。你设定显著性水平为0.05，而且如果H0是
# 错误的，你想有90%的信心拒绝H0，那么研究需要多少观测
pwr.r.test(r=0.25,sig.level = 0.05,power = 0.9,alternative = "greater") #n=133

# 线性模型（比如多元回归）功效分析
# u和v分别是分子自由度和分母自由度，f2是效应值
# 现假设你想研究老板的领导风格对员工满意度的影响，是否超过薪水和工作小费对员工
# 满意度的影响。领导风格可用四个变量来评估，薪水和小费与三个变量有关。过去的
# 经验表明，薪水和小费能够解释约30%的员工满意度的方差。而从现实出发，领导
# 风格至少能解释35%的方差。假定显著性水平为0.05，那么在90%的置信度情况下，
# 你需要多少受试者才能得到这样的方差贡献率呢？
# 此处，sig.level = 0.05，power = 0.90，u = 3（总预测变量数减去集合B中的预测
# 变量数），效应值为f2 = (0.35 - 0.30)/(1 - 0.35) = 0.076 9
pwr.f2.test(u=3,f2=0.0769,sig.level = 0.05,power=0.9) 
# 在多元回归中，分母的自由度等于N - k - 1，N是总观测数，k是预测变量数。
# 本例中，N - 7- 1 = 185，即需要样本大小N = 185 + 7 + 1 = 193

#比例检验功效分析，h是效应值，n是各组相同的样本量
# 假定你对某流行药物能缓解60%使用者的症状感到怀疑。而一种更贵的新药如果能
# 缓解65%使用者的症状，就会被投放到市场中。此时，在研究中你需要多少受试者?假设
# 你想有90%的把握得出新药更有效的结论，并且希望有95%的把握不会误得结论
pwr.2p.test(h=ES.h(0.65,0.6),sig.level = 0.05,power = 0.9,
            alternative = "greater") #n=1605

# 评估卡方检验的功效、效应值和所需的样本大小
# w是效应值，N是总样本大小，df是自由度
# 举一个简单的例子，假设你想研究人种与工作晋升的关系。你预期样本中70%是白种人，
# 10%是美国黑人，20%西班牙裔人。而且，你认为相比30%的美国黑人和50%的西班牙裔人，
# 60%的白种人更容易晋升。你预期总人数的42%是晋升的白种人（0.42 = 0.70 × 0.60），
# 总人数的7%是未晋升的美国黑人（0.07 = 0.10 × 0.70）。让我们取0.05的显著性水平
# 和0.90的预期功效水平。双因素列联表的自由度为(r-1)(c-1)，r是行数，c是列数
# 计算假设的效应值
w <- ES.w2(matrix(c(0.42,0.28,0.03,0.07,0.1,0.1),byrow = TRUE,nrow = 3));w
pwr.chisq.test(w=0.19,df=2,sig.level = 0.05,power = 0.9) #n=351

# 单因素ANOVA中检测显著效应所需的样本大小
nes <- length(seq(0.1,0.5,0.01));nes
samsize <- NULL
for (i in 1:nes) {
  result <- pwr.anova.test(k = 5,f = es[i],sig.level = 0.05,power = 0.9)
  samsize[i] <- ceiling(result$n)
}
plot(samsize,es,type = "l",col = "red",
     ylab = "Effect size",
     xlab = "Sample size (per cell)",
     main = "One way ANOVA with power=0.9 and alpha=0.05")

# 检验各种效应值下的相关性所需的样本量曲线
r <- seq(0.1,0.5,0.01);r
p <- seq(0.4,0.9,0.1);p
nr <- length(seq(0.1,0.5,0.01));nr
np <- length(seq(0.4,0.9,0.1));np
samsize <- array(numeric(nr * np),dim = c(nr,np))
for (i in 1:np) {
  for (j in 1:nr) {
  result <- pwr.r.test(n = NULL,r = r[j],sig.level = 0.05,power = p[i],
                       alternative = "two.sided")
  samsize[j,i] <- ceiling(result$n)
  }
}
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange,yrange,type = "n",xlab = "correlation coefficient (r)",
    ylab = "sample size(n)")
for (i in 1:np) {
  lines(r,samsize[,i],type = "l",lwd = 2,col = colors[i])
}
abline(v = 0,h = seq(0,yrange[2],50),lty = 2,col = "grey89")
abline(h = 0,v = seq(xrange[1],xrange[2],0.02),lty = 2,col = "gray89")
title("sample size estimation for correlation studies\n sig=0.05(two-tailed)")
legend("topright",title = "power", as.character(p),fill = colors)

part11#------------------------------------------
###PART11
attach(mtcars)
plot(wt,mpg,
     main="basic scatter plot of mpg vs. weight",
     xlab="car weight (lbs/1000)",
     ylab="miles per gallon",pch=19)
abline(lm(mpg~wt),col="red",lwd=2,lty=1) #添加拟合的线性直线
lines(lowess(wt,mpg),col="blue",lwd=2,lty=2) #添加平滑曲线
# Q添加拟合曲线的散点图
library("car")
scatterplot(mpg~wt|cyl,data=mtcars,lwd=2,span=0.75,
            main="scatter plot of mpg vs. weight by #cylinders",
            xlab="weight of car (lbs/1000)",
            ylab="miles per gallon",
            legend.plot=TRUE,
            labels=row.names(mtcars),
            id.method="identify",
            boxplotts="xy")
#散点图矩阵几种示例
library("car")
pairs(~mpg+disp+drat+wt,data=mtcars,
      main="basic scatter plot matrix") #主对角线上下的六幅图相同
pairs(~mpg+disp+drat+wt,data=mtcars,
      upper.panel=NULL,
      main="basic scatter plot matrix") #只显示主对角线一侧的图
scatterplotMatrix(~mpg+disp+drat+wt,data=mtcars,
                  spread=FALSE,lty.smooth=2,
                  main="scatter plot matrix via car package") #添加线性和平滑(loess)曲线，lty.smooth=2设定平滑曲线使用虚线
#根据变量相关性重新排序并添加颜色
library("car")
library("gclus")  
mydata <- mtcars[c(1,3,5,6)]
mydata.corr <- abs(cor(mydata)) #计算相关关系的绝对值
mycolors <- dmat.color(mydata.corr) #获取绘图颜色
myorder <- order.single(mydata.corr) #对变量排序
cpairs(mydata,myorder,panel.colors=mycolors,
       gap=0.5,mian="variables ordered and colored by correlations")
# 
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n,mean = 0,sd=0.5),ncol=2)
c2 <- matrix(rnorm(n,mean = 3,sd=2),ncol=2)
mydata <- rbind(c1,c2)
str(mydata)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x","y")
with(mydata,plot(x,y,pch=19,main="scatter plot with 10000 observations")) #10000个观测值，无法分辨x和y
#核密度估计生成用颜色密度来表示点分布
with(mydata,smoothScatter(x,y,
                 main="scatterplot colored by smoothed densities")) 
# 利用六边形单元格来表示
library("hexbin")
with(mydata,{
  bin <- hexbin(x,y,xbins=50)
  plot(bin,main="hexagonal binning with 10000 observations")
})
# 通过颜色展示点的密度，效果较好
# install.packages("IDPmisc")
library("IDPmisc")
with(mydata,
     iplot(x,y,main="image scatterplot with color indicating density"))
# 三维散点图
library("scatterplot3d")
attach(mtcars)
scatterplot3d(wt,disp,mpg,
              main="basic 3d scatterplot") #简单3d图
#添加了垂直线和阴影
scatterplot3d(wt,disp,mpg,
              pch=16,highlight.3d = TRUE,
              type = "h",
              main="3D scatterplt with vertical lines") 
# 添加了垂直线、阴影和回归平面
s3d <- scatterplot3d(wt,disp,mpg,
              pch=16,highlight.3d = TRUE,
              type = "h",
              main="3D scatterplt with vertical lines") #添加了垂直线和阴影
fit <- lm(mpg~wt+disp)
s3d$plane3d(fit)
detach(mtcars)
# 旋转三维图
library("rgl")
attach(mtcars)
plot3d(wt,disp,mpg,col="red",size=5)
detach(mtcars)
# 旋转三维图
library("car")
# library("rgl")
# library("mgcv")
with(mtcars,
     scatter3d(wt,disp,mpg))
# 气泡图，x代表车重，y代表每加仑英里数，旗袍大小代表发动机排量
attach(mtcars)
r <- sqrt(disp/pi)
symbols(wt,mpg,circle=r,inches=0.30, #inches比例因子，控制圆圈大小，默认值1
        fg="white",bg="lightblue",
        main="bubble plot with point size proportional to displacement",
        ylab="miles per gallon",
        xlab="weight of car (lbs/1000)")
text(wt,mpg,rownames(mtcars),cex=0.6)
detach(mtcars)
# 创建散点图和折线图
write.table(Orange,"Orange.csv",row.names=TRUE,sep=",")
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
t1 <- subset(Orange,Tree==1)
plot(t1$age,t1$circumference, #散点图
     xlab="age (days)",
     ylab="circumference (mm)",
     main="orange tree 1 growth")
plot(t1$age,t1$circumference, #折线图
     xlab="age (days)",
     ylab="circumference (mm)",
     main="orange tree 1 growth",
     type="b")
par(opar)
# 多条折线图
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange,yrange,
     type="n",
     xlab="age (days)",
     ylab="circumference (mm)")
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18,18+ntrees,1)
for(i in 1:ntrees){
  tree <- subset(Orange,Tree==i)
  lines(tree$age,tree$circumference,
        type="b",lwd=2,lty=linetype[i],
        col=colors[i],pch=plotchar[i])
}
#2022-12-2
# 变量相关图
options(digits=2)
cor(mtcars)
# install.packages("corrgram")
library("corrgram")
corrgram(mtcars,order=TRUE,lower.panel=panel.shade, #order=true,使用PCA对变量重新排序
         upper.panel=panel.pie,text.panel=panel.txt,
         main="correlogram of mtcars intercorrelations") #蓝色正相关，红色负相关，饼图正相关顺时针填充，负相关逆时针填充
# 自定义饼图颜色
col.corrgram <- function(ncol){
                colorRampPalette(c("darkgoldenrod4","burlywood1",
                                   "darkkhaki","darkgreen"))(ncol)
                              }
corrgram(mtcars,order=TRUE,lower.panel=panel.shade, 
         upper.panel=panel.pie,text.panel=panel.txt,
         main="a correlogram of different color") 
# 多组类别型变量，马赛克图
ftable(Titanic)
library("vcd")
dev.new()
mosaic(Titanic,shade=TRUE,legend=TRUE) #形式1
mosaic(~Class+Sex+Age+Survived,data=Titanic,shade=TRUE,legend=TRUE) #形式2

part12#----------------------------------------------
###PART12 置换检验和自助法
rm(list = ls())
# install.packages("lmPern")
library("coin")
library("lmPerm") #方差分析和回归分析的置换检验,2022-12-5

# t检验和单因素置换检验结果比较
library("coin")
score <- c(40,57,45,55,58,57,64,55,62,65)
treatment <- factor(c(rep("A",5),rep("B",5)))
df_scr <- data.frame(treatment,score)
t.test(score~treatment,data=df_scr,var.equal=TRUE) #t检验显著差异
oneway_test(score~treatment,data=df_scr,distribution="exact")#置换检验无显著差异

# 检验美国南部监禁概率与非南部间的差异
# coin包规定所有的类别型变量都必须以因子形式编码
library("MASS")
df_UScrime <- transform(UScrime,So=factor(So)) ##transform
wilcox_test(Prob~So,data=df_UScrime,distribution="exact") 
#p<0.05,结果表明监禁在南部可能更多

# part9单因素方差分析来评价五种药物疗法对降低胆固醇的效果,此处K样本置换检验
library("multcomp")
set.seed(1234)
oneway_test(response~trt,data=cholesterol,
            distribution=approximate(nresample=9999)) #各组显著不同

# 判断类别型变量的独立性
# 评价关节炎的治疗（treatment）与效果（improvement）间的关系。治疗有两个水平
# （安慰剂、治疗），效果有三个水平（无、部分、显著），变量Improved以
# 有序因子形式编码。
library("coin")
library("vcd")
df_Arthritis <- transform(Arthritis,
             Improved=as.factor(as.numeric(Improved))) 
##有序因子变成分类因子，有序因子则是线性与线性趋势检验，而不是卡方检验
set.seed(1234)
chisq_test(Treatment~Improved,data=df_Arthritis,
           distribution=approximate(nresample=9999))

# 检验美国文盲率与谋杀率间的相关性，两数值变量的独立性置换检验
# state.x77是一个矩阵，在coin包中，必须将其转化为一个数据框
df_states <- as.data.frame(state.x77) 
set.seed(1234)
spearman_test(Illiteracy~Murder,data=df_states,
              distribution=approximate(nresample=9999)) #独立性假设未被满足

# 两配对组的置换检验可使用wilcoxsign_test()函数；多于两组时使用friedman_
# test()函数
# 检验两个年龄段间的失业率是否相等
library("MASS")
wilcoxsign_test(U1~U2,data=UScrime,distribution="exact") #失业率不同

# lmPerm包可做线性模型的置换检验
# 5名女性的身高和体重间的关系
library("lmPerm")
set.seed(1234)
lm_weight <- lmp(weight~height,data=women,perm="Prob")
summary(lm_weight)

# 多项式回归的置换检验
lm_weight2 <- lmp(weight~height+I(height^2),data=women,perm="Prob")
summary(lm_weight2)

# 通过美国50个州的人口数、文盲率、收入水平和结霜天数预测犯罪率
# 多元回归置换检验
library("lmPerm")
set.seed(1234)
df_states <- as.data.frame(state.x77)
lm_murder <- lmp(Murder~Population+Illiteracy+Income+Frost,
           data=df_states,perm="Prob")
summary(lm_murder) 
#置换检验中与正态理论中显著性不同时，表明数据可能违反了正态性假设或者存在离群点

# 2022-12-7
# 各种疗法对降低胆固醇的影响，，单因素方差分析的置换检验
library("lmPerm")
library("multcomp")
set.seed(1234)
aov_res <- aovp(response~trt,data=cholesterol,perm="Prob")
summary(aov_res) #各疗法的效果不全相同

# 控制妊娠期时间相同，观测四种药物剂量对鼠崽体重的影响
# 单因素协方差分析置换检验
library("lmPerm")
set.seed(1234)
aov_weight2 <- aovp(weight~gesttime+dose,data=litter,perm="Prob")
summary(aov_weight2) #影响不相同

#维生素C对豚鼠牙齿生长的影响，该实验两个因子是剂量（三水平）和喂食方式（两水平）
# 双因素方差分析置换检验
library("lmPerm")
set.seed(1234)
aov_len <- aovp(len~supp*dose,data=ToothGrowth,perm="Prob")
summary(aov_len) #0.05水平统计显著，0.01水平只有主效应显著

# 自助法Q
#多元回归根据车重和发动机排量来预测汽车每加仑行驶的英里数（统计量）
#获得标准的回归统计量和95%的R平方值的置信区间
rsq <- function(formula,data,indices){
       d <- data[indices,]
       fit <- lm(formula,data=d)
       return(summary(fit)$r.square) #函数返回回归的R平方值
} 
library("boot")
set.seed(1234)
boot_res <- boot(data=mtcars,statistic=rsq,
                R=1000,formula=mpg~wt+disp) #自助抽样1000次
print(boot_res)
plot(boot_res) #结果自助的R平方值不呈正态分布Q
boot.ci(boot_res,type=c("perc","bca")) #获得95%置信区间
# 由于两种方法0都在置信区间外，零假设H0：R平方值=0都被拒绝

# 自助法估计三个回归系数（截距、车重和发动机排量）95%置信区间
bs <- function(formula,data,indices){
      d <- data[indices,]
      fit <- lm(formula,data=d)
      return(coef(fit))
} #创建返回回归系数向量的函数
library("boot")
set.seed(1234)
boot_res2 <- boot(data=mtcars,statistic = bs,
                R=1000,formula=mpg~wt+disp)
print(boot_res2)
plot(boot_res2,index=1) #对多个统计量自助抽样时，索引参数，本例1指截距项
plot(boot_res2,index=2) #2车重
plot(boot_res2,index=3) #3发动机排量
boot.ci(boot_res2,type="bca",index=1)
boot.ci(boot_res2,type="bca",index=2)#获得车重95%的置信区间
boot.ci(boot_res2,type="bca",index=3)
# 2022-12-31

part13#----------------------------------------------
###PART13，二项式回归和泊松回归
# 2023-1-3，非正态因变量，类别型或计数型
# 以AER包中的数据框Affairs为例，我们将探究婚外情的数据
# install.packages("AER")
library("AER")
data(Affairs,package = "AER")
write.csv(Affairs,"Affairs.csv",row.names = TRUE)
table(Affairs$affairs) #affairs是数值型数据
# 这种转化无法定义是否有婚外情，是或否
#Affairs <- transform(Affairs,affairs = factor(affairs))
# 转为，1或0二值型
Affairs$ynaffair[Affairs$affairs>0] <- 1 #转化为factor，y/n
Affairs$ynaffair[Affairs$affairs==0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,levels=c(0,1),labels=c("no","yes"))
table(Affairs$ynaffair)
glm_ynaffair <- glm(ynaffair~gender+age+yearsmarried+children+religiousness+
                education+occupation+rating,data=Affairs,family=binomial())
summary(glm_ynaffair)
# 去除检验不显著的这些变量
glm_reduynaffair <- glm(ynaffair~age+yearsmarried+religiousness+rating,
                       data=Affairs, family=binomial())
summary(glm_reduynaffair)
# 模型嵌套glm_reduynaffair是glm_ynaffair的一个子集，使用anova比较
anova(glm_reduynaffair,glm_ynaffair,test="Chisq") #不显著，4和9各变量效果一样
# 模型参数解释
coef(glm_reduynaffair) #响应变量是Y=1的对数优势比（log）
exp(coef(glm_reduynaffair))#结果指数化，婚龄增加一年，婚外情的优势比将乘以1.106
exp(confint(glm_reduynaffair))

### 评价婚姻评分对婚外情概率的影响，首先，创建一个虚拟数据集，设定
# 年龄、婚龄和宗教信仰为它们的均值，婚姻评分的范围为1～5
df_pre <- data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age),yearsmarried=
         mean(Affairs$yearsmarried),religiousness=mean(Affairs$religiousness))
df_pre$prob <- predict(glm_reduynaffair,newdata=df_pre,
                       type = "response");df_pre #添加prob至数据集
# 评价年龄对婚外情概率的影响，假定其他因素不变
df_pre2 <- data.frame(rating=mean(Affairs$rating),age=seq(17,57,10),
                      yearsmarried=mean(Affairs$yearsmarried),
                      religiousness=mean(Affairs$religiousness))
df_pre2$prob <- predict(glm_reduynaffair,newdata=df_pre2,
                        type = "response");df_pre2
# 检测过度离势，值比1大表明方差大于期望方差
glm_reduynaffair <- glm(ynaffair~age+yearsmarried+religiousness+rating,
                        data=Affairs, family=binomial())
glm_reduynaffair2 <- glm(ynaffair~age+yearsmarried+religiousness+rating,
                         data=Affairs,family=quasibinomial())
pchisq(summary(glm_reduynaffair2)$dispersion*glm_reduynaffair$df.residual,
               glm_reduynaffair$df.residual,lower=F) #H0:值为1，p=0.34

###泊松回归
# 分析计数型数据,药物治疗是否能减少癫痫发病数
# install.packages("robust")
library("robust")
data(breslow.dat,package="robust") #关注6/7/8/10列数据
write.csv(breslow.dat,"breslow.csv",row.names = TRUE)
opar <- par(no.readonly=TRUE)
# 查看sumY和Trt-sumY关系
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY,breaks=20,xlab="Seizure Count",main="distribution of Seizures")
boxplot(sumY~Trt,xlab="treatment",main="group comparisions")
par(opar)

#泊松回归拟合 
poi_sumY <- glm(sumY~Base+Age+Trt,data=breslow.dat,family=poisson())
summary(poi_sumY)
# 解释模型参数
coef(poi_sumY)
exp(coef(poi_sumY))
# 保持其他变量不变，年龄增加一岁，期望的癫痫发病数将乘以1.023

# 泊松回归过度离势判断，值大于1表明存在过度离势
# install.packages("qcc")
library("qcc")
qcc.overdispersion.test(breslow.dat$sumY,type="poisson")
# 显著性检验的p值小于0.05，表明确实存在过度离势

#类泊松方法family=quasipoisson()
poi_sumY2 <- glm(sumY~Base+Age+Trt,data=breslow.dat,family=quasipoisson())
summary(poi_sumY2)
# 结果解读：标准误越大将会导致Trt（和Age）的p值越大于0.05，当考虑过度离势，并控
# 制基础癫痫数和年龄时，并没有充足的证据表明药物治疗相对于使用安慰剂能显著
# 降低癫痫发病次数。值得思考！

part14#------------------------------------------------
###PART14，主成分和因子分析

# code14-1 美国法官评分的主成分分析
library("psych")
fa.parallel(USJudgeRatings[,-1],fa="pc",n.iter=100,
            show.legend = FALSE,main="screen plot with parallel analysis")
# 展示了基于观测特征值的碎石检验（由线段和x符号组成）、根据100个随机数据矩阵
# 推导出来的特征值均值（虚线），pc=1

#提取主成分
pca_1 <- principal(USJudgeRatings[,-1],nfactors=1);pca_1
# PC1栏包含了成分载荷，指观测变量与主成分的相关系数；h2栏指成分公因子方差：
# 主成分对每个变量的方差解释度；u2栏指成分唯一性：方差无法被主成分解释的比例
# 第一主成分解释了11个变量92%的方差

# code14-2 两个主成分示例
library("psych")
attach(Harman23.cor);Harman23.cor
fa.parallel(Harman23.cor$cov,n.obs=305,fa="pc",n.iter=100,
            show.legend = FALSE,main="screen plot with parallel analysis")#pc=2
pca_2 <- principal(Harman23.cor$cov,nfactors = 2,rotate="none");pca_2

# 主成分旋转
# code14-3 14-2示例方差极大旋转的pca
rpca_1 <- principal(Harman23.cor$cov,nfactors = 2,rotate="varimax");rpca_1

# code14-4 从原始数据中获取成分得分,这个意义在哪Q
pca_1 <- principal(USJudgeRatings[,-1],nfactors=1,scores = TRUE)
head(pca_1$scores)

# 律师与法官的接触频数与法官评分间的相关系数
cor(USJudgeRatings$CONT,pca_1$scores) #无关联

# 获得pca主成分得分的系数，当主成分分析基于相关系数矩阵时，原始数据便不可用了，
# 也不可能获取每个观测的主成分得分，但是你可以得到用来计算主成分得分的系数
rpca_2 <- principal(Harman23.cor$cov,nfactors = 2,rotate="varimax");rpca_2
round(unclass(rpca_2$weights),2) #不明白Q 等同rpca_2[["weights"]]

#探索性因子分析法 
options(digits = 2)
covariances <- ability.cov$cov;covariances #6个变量的协方差矩阵
correlations <- cov2cor(covariances);correlations #转为相关系数矩阵

# 判断需提取的公共因子数
fa.parallel(correlations,n.obs=112,fa="both",n.iter=100,
            main="screen plots with parallel analysis") #pc=1或2,fa=2

# code14-6 未旋转的主轴迭代因子法fm=pa
fa_1 <- fa(correlations,nfactors=2,rotate="none",fm="pa");fa_1 #因子载荷解释Q
 
# code14-7 正交旋转提取因子,强制两个因子不相关,rotate="varimax"
fa.varimax <- fa(correlations,nfactors=2,rotate="varimax",fm="pa");fa.varimax 
# 对于正交旋转，因子分析的重点在于因子结构矩阵（变量与因子的相关系数）

# code14-8 斜交旋转提取因子,允许两个因子相关,rotate="promax"
fa.promax <- fa(correlations,nfactors=2,rotate="promax",fm="pa");fa.promax
# 对于斜交旋转，因子分析会考虑三个矩阵：因子结构矩阵、因子模式矩阵和因子关联
# 矩阵。因子模式矩阵即标准化的回归系数矩阵(PA1和PA2栏中的值组成了因子模式矩阵。
# 它们是标准化的回归系数，而不是相关系数)。它列出了因子预测变量的权重。因子关联
# 矩阵即因子相关系数矩阵。因子关联矩阵显示两个因子的相关系数为0.57，相关性很大
##因子结构矩阵（或称因子载荷阵）没有被列出来，但你可以使用公式F = P*Phi很轻松
# 地得到它，其中F是因子载荷阵，P为因子模式矩阵，Phi为因子关联矩阵
#计算因子载荷阵Q
fsm <- function(oblique){
      if(class(oblique)[2]=="fa"&is.null(oblique$Phi)){
        warning("object doesn't look like oblique EFA")
      } else {
        P <- unclass(oblique$loading)
        F <- P %*% oblique$Phi
        colnames(F) <- c("PA1","PA2")
        return(F)
      }
}
fsm(fa.promax) #变量与因子间的相关系数

# 绘制正交或者斜交结果的图形
factor.plot(fa.promax,labels=rownames(fa.promax$loadings)) #斜交图形
factor.plot(fa.varimax,labels=rownames(fa.varimax$loadings)) #正交图形
fa.diagram(fa.promax,simple = FALSE) ###最终的EFA分析图

# 对Harman74.cor中的24个心理学测验进行因子分析
library("psych")
fa.24tests <- fa(Harman74.cor$cov,nfactors=4,rotate="promax")
dev.new()
fa.diagram(fa.24tests,simple=FALSE)
dev.off()

part15 
-------------------------------------------------
### PART15,2023-1-5
# install.packages(c("VIM","mice"))
library("VIM")
library("mice")
write.table(sleep,"sleep.csv",sep=",")
#加载数据集
data(sleep,package = "VIM")
#识别矩阵或数据框中没有缺失值的行,若每行都包含完整的实例，
# 则返回TRUE的逻辑向量；若每行有一个或多个缺失值，则返回FALSE。
sleep[complete.cases(sleep),] #42个观测为完整数据
# 列出有一个或多个缺失值的行
sleep[!complete.cases(sleep),] #20个含一个或多个缺失值

# 获取关于缺失数据的有用信息
sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream)) 
mean(!complete.cases(sleep)) #32%观测值包含一个或多个缺失值

# 数据集较大时，以矩阵或数据框形式展示缺失值
library("mice")
data(sleep,package = "VIM")
md.pattern(sleep)

# 绘制每个变量组合的缺失值数
library("VIM")
aggr(sleep,prop=FALSE,numbers=TRUE) #代码有问题

# 生成展示每个观测值数据的图形
matrixplot(sleep)

# 生成散点图，并在图形边界展示变量的缺失值信息，
# 以做梦时长与哺乳动物妊娠期时长的关系为例
marginplot(sleep[c("Gest","Dream")],pch=20,col=c("darkgray","red","blue"))

# 使用指示变量替代数据集中的数据，1表示缺失，0表示存在
x <- as.data.frame(abs(is.na(sleep)))
head(sleep,n=5)
head(x,n=5)
# 提取含缺失值的变量
y <- x[which(apply(x,2,sum)>0)]
y
# 列出指示变量之间的相关系数
cor(y)
# 含缺失值变量与其他可观测变量间的关系
cor(sleep,y,use="pairwise.complete.obs")

# 存储没有缺失值的数据框或矩阵形式的观测值
newdata <- mydata[complete.cases(mydata),]
# na.omit函数，
newdata <- na.omit(mydata)

# 示例：睡眠研究中变量间的关系
options(digits=1)
# 完整数据的相关系数
cor(na.omit(sleep))
# 行删除后对的线性回归
fit <- lm(Dream~Span+Gest,data=na.omit(sleep))
summary(fit)

# 多重插补（MI）是一种基于重复模拟的处理缺失值的方法
# 它将从一个包含缺失值的数据集中生成一组完整的数据集
library("mice")
data(sleep,package = "VIM")
# mice()首先从一个包含缺失数据的数据框开始，然后返回一个包含多个完整数据集的对象
#每个完整数据集都是通过对原始数据框中的缺失数据进行插补而生成的
# imp是一个包含m个插补数据集的列表对象,同时还含有完成插补过程的信息,默认地,m为5
imp <- mice(sleep,seed = 1234)
# with()函数可依次对每个完整数据集应用统计模型（如线性模型或广义线性模型）
fit <- with(imp,lm(Dream ~ Span + Gest))
# 最后，pool()函数将这些单独的分析结果整合为一组结果
pooled <- pool(fit)
summary(pooled) #span的p值约为0.08；gest的p值小于0.05，与dream显著负相关
# 提取imp对象的子成分，观测实际的插补值
imp$imp$Dream
# 利用complete()函数可以观察m个插补数据集中的任意一个
dataset3 <- complete(imp,action=3) #action可取m中任意一个数
dataset3

part16#-------------------------------------------------
###PART16
library("lattice")
write.table(singer,"singer.csv",sep=",")
histogram(~height|voice.part,data=singer,main="distribution of heights by voice pitch",
          xlab="height (inches)")
# lattice绘图示例
library("lattice")
attach(mtcars)
gear <- factor(gear,levels = c(3,4,5),
               labels = c("3 gears","4 gears","5 gears"))
cyl <- factor(cyl,levels = c(4,6,8),
              labels = c("4 cylinders","6 cylinders","8 cylinders"))
densityplot(~mpg,main="density plot",
            xlab="miles per gallon")
densityplot(~mpg|cyl,
            main="density plot by number of cylinders",
            xlab="miles per gallon")
bwplot(cyl~mpg|gear,
       main="box plots by cylinders and gears",
       xlab="miles per gallon",ylab="cylinders")
xyplot(mpg~wt|cyl*gear,
       main="scatter plots by cylinders and gears",
       xlab="car weight",ylab="miles per gallon")
cloud(mpg~wt*qsec|cyl,
      main="3D scatter plots by cylinders")
dotplot(cyl~mpg|gear,
        main="dot plots by number of gears and cylinders",
        xlab="miles per gallon")
splom(mtcars[c(1,3,4,5,6)],main="scatter plot matrix for mtcars data")
detach(mtcars)
# 存储生成的图形
library("lattice")
mygraph <- densityplot(~height|voice.part,data=singer)
# 修改图形
update(mygraph,col="red",pch=16,cex=0.8,jitter=0.05,lwd=2)
# 条件变量，将连续型变量转为瓦块变量
displacement <- equal.count(mtcars$disp,number=3,overlap=0)
# 修改面板的布局（三列和一行）和宽高比，方便对三组进行比较
xyplot(mpg~wt|displacement,data=mtcars,
       main="miles per gallon vs. weight by engine displacement",
       xlab="weight",ylab="miles per gallon",
       layout=c(3,1),aspect=1.5)
# code16-2 自定义面板函数
library("lattice")
displacement <- equal.count(mtcars$disp,number=3,overlap=0)
mypanel <- function(x,y){
  panel.xyplot(x,y,pch=19)
  panel.rug(x,y)
  panel.grid(h=-1,v=-1)
  panel.lmline(x,y,col="red",lwd=1,lty=2)
}
xyplot(mpg~wt|displacement,data=mtcars,
       main="miles per gallon vs. weight by engine displacement",
       xlab="weight",ylab="miles per gallon",
       layout=c(3,1),aspect=1.5,panel=mypanel)
# code16-3 自定义面板函数和额外选项
library("lattice")
mtcars$transmission <- factor(mtcars$am,levels=c(0,1),
                              labels=c("automatic","manual"))
panel.smoother <- function(x,y){
  panel.xyplot(x,y,pch=19)
  panel.loess(x,y)
  panel.grid(h=-1,v=-1)
  panel.abline(h=mean(y),lwd=2,lty=2,col="green")
}
xyplot(mpg~disp|transmission,data=mtcars,
       scales = list(cex=0.8,col="red"),
       main="miles per gallon vs. weight by engine displacement",
       sub="dotted lines are group means",aspect=1,
       xlab="displacement",ylab="miles per gallon",
       panel=panel.smoother)
# 分组变量核密度图
library("lattice")
mtcars$transmission <- factor(mtcars$am,levels=c(0,1),
                              labels=c("automatic","manual"))
densityplot(~mpg,data=mtcars,
            group=transmission,
            main="MPG distribution by transmission type",
            xlab="miles per gallon",
            auto.key=TRUE)













