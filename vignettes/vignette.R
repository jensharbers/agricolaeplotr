## ----setup, eval = FALSE------------------------------------------------------
#  install.packages("agricolaeplotr")

## ----load_package-------------------------------------------------------------
library("agricolaeplotr")

library("ggplot2")
library("agricolae")

## -----------------------------------------------------------------------------
library(agricolae) # origin of the needed design object
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')

head(outdesign$book,10)

plot_design.factorial_crd(outdesign,ncols=7,nrows=3, width = 1, height = 1)


## ---- echo=TRUE, results='asis'-----------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_y = TRUE)

## ---- echo=TRUE, results='asis'-----------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_x = TRUE)

## ---- results='asis'----------------------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_x = TRUE,reverse_y = TRUE)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE,space_width = 1,space_height = 1)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE,space_width = 0.7,space_height = 0.8)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B")

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(129984)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B")

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(129866478)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_poster()

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(12986)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')

plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_pres()

## ---- echo = TRUE, results='asis'---------------------------------------------
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_pres() + scale_fill_viridis_d()

## -----------------------------------------------------------------------------
set.seed(23488833)
trt <-c(3,2)
outdesign<-design.ab(trt, serie=2, design="lsd",seed = 454555)
length_table <- dim(outdesign$book)[1] # length of the table

outdesign$book$yield <- sample(c(5:12,c(NA,NA,NA)), size = length_table, replace = TRUE)
plot_design.factorial_lsd(outdesign,factor_name = "yield") + scale_fill_viridis_c()


## -----------------------------------------------------------------------------
set.seed(23488833)
trt <-c(3,2)
outdesign<-design.ab(trt, serie=2, design="lsd",seed = 454555)
length_table <- dim(outdesign$book)[1] # length of the table

yield <- sample(c(5:20,c(NA,NA,NA)), size = length_table, replace = TRUE)
df <- cbind(plots=outdesign$book$plots,yield)
head(df,10)
outdesign$book <- merge(outdesign$book,df, by.x = "plots", by.y = "plots")
plot_design.factorial_lsd(outdesign,factor_name = "yield") + scale_fill_viridis_c()


## -----------------------------------------------------------------------------

set.seed(1298664)
plots <- as.factor(1:(8*6))
block <- as.factor((rep(1:6,each=8)))
A <- as.vector(replicate(8,sample(rep(1:2,times=3),6,replace=FALSE)))
outcome <- runif(48,20,100)
experiment <- cbind(plots,block,A,outcome)
experiment <- as.data.frame(experiment)
head(experiment)

experiment_design <- list()
experiment_design$parameters$design<- "factorial"
experiment_design$parameters$applied <-  "rcbd"

experiment_design$book <- experiment
head(experiment_design)
plot_design.factorial_rcbd(experiment_design,factor_name = "A")
plot_design.factorial_rcbd(experiment_design,factor_name = "outcome")


## -----------------------------------------------------------------------------
set.seed(1298664)
t1<-c('a','b','c','d','e',"f","g","h")
t2<-c("u",'v','w','x','y',"z")
outdesign2 <- design.split(trt1=t1, trt2=t2, r=r,serie = 2,
                           seed = 0, kinds = 'Super-Duper',
                           randomization=TRUE,first=TRUE,design = 'lsd')

plot_split_lsd(outdesign2,factor_name_1 = "t1",factor_name_2 = "t2",width = 2,height = 2, subplots = FALSE,labels = "plots")


plot_split_lsd(outdesign2,width = 2,height = 2, subplots = TRUE, labels = "splots", factor_name_1 = "t1", factor_name_2 = "t2")


## -----------------------------------------------------------------------------
set.seed(1298664)
t1<-c('a','b','c','d','e','f','g')
t2<-c('v','w','x','y','z')
r <- 4
outdesign2 <- design.split(trt1=t1, trt2=t2, r=r,
serie = 2, seed = 0, kinds = 'Super-Duper',
randomization=TRUE,first=TRUE,design = 'crd')
plot_split_crd(outdesign2,ncols = 5,nrows=6, subplots = FALSE,
               factor_name_1 = "t1",factor_name_2 = "t2")
plot_split_crd(outdesign2,ncols = 5,nrows=6, subplots = TRUE, labels="splots",factor_name_1 = "t1",factor_name_2 = "t2")


## -----------------------------------------------------------------------------
set.seed(1298664)
T1<-c('a','b','c','d','e',"f","g")
T2<-c("we",'v','w','x','y','z',"d")
r = 3
outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,serie = 2,
 seed = 0, kinds = 'Super-Duper',randomization=TRUE,
 first=TRUE,design = 'rcbd')

plot_split_rcbd(outdesign2,width = 5,height = 5,subplots = FALSE, 
                factor_name_1 = "T1",factor_name_2 = "T2")

plot_split_rcbd(outdesign2,width = 5,height = 5,labels = "splots",
                factor_name_1 = "T1",factor_name_2 = "T2")



