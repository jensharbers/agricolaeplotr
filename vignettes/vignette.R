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



## -----------------------------------------------------------------------------
require(FielDHub)
SpatpREP1 <- partially_replicated(nrows = 25,
                                  ncols = 18,
                                  repGens = c(280,50,10,1,1),
                                  repUnits = c(1,2,3,20,20),
                                  planter = "cartesian",
                                  plotNumber = 101,
                                  seed = 77)

p <- plot_fieldhub(SpatpREP1,
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = TRUE,
reverse_x = TRUE)

## -----------------------------------------------------------------------------
NAME <- paste("G", 1:492, sep = "")
repGens = c(108, 384);repUnits = c(2,1)
REPS <- rep(repUnits, repGens)
treatment_list <- data.frame(list(ENTRY = 1:492, 
                                   NAME = NAME, 
                                   REPS = REPS))

SpatpREP2 <- partially_replicated(nrows = 30,
                                   ncols = 20,
                                   planter = "serpentine",
                                   plotNumber = 101,
                                   seed = 41,
                                   data = treatment_list)

SpatpREP2$infoDesign

plot_fieldhub(SpatpREP2,
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = TRUE,
reverse_x = TRUE)


## -----------------------------------------------------------------------------
spatd <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, 
                              checks = 4, 
                              plotNumber = 101, 
                              kindExpt = "SUDC", 
                              planter = "serpentine", 
                              seed = 1987,
                              exptName = "20WRY1", 
                              locationNames = "MINOT")

plot_fieldhub(spatd,
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = TRUE,
reverse_x = TRUE)



## -----------------------------------------------------------------------------
OptimAd1 <- optimized_arrangement(nrows = 20, ncols = 20, lines = 362, 
                                  amountChecks = 38, 
                                  checks = 1:5,
                                  planter = "cartesian", 
                                  plotNumber = 101,
                                  seed = 14,
                                  exptName = "20RW1",
                                  locationNames = "CASSELTON")
OptimAd1$infoDesign

plot_fieldhub(OptimAd1,
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)



## -----------------------------------------------------------------------------


rectangularLattice1 <- rectangular_lattice(t = 20, k = 4, r = 6, l = 1, 
                                           plotNumber = 101,
                                           locationNames = "FARGO", 
                                           seed = 126)
rectangularLattice1$infoDesign


plot_fieldhub(rectangularLattice1,
x = "REP",
y = "ENTRY",
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)


## -----------------------------------------------------------------------------


squareLattice1 <- square_lattice(t = 64, k = 8, r = 5, l = 2, 
                                 plotNumber = c(1001, 2001),
                                 locationNames = c("FARGO", "MINOT"), 
                                 seed = 1986)
squareLattice1$infoDesign


plot_fieldhub(squareLattice1,
x = "REP",
y = "ENTRY",
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)


## -----------------------------------------------------------------------------
squareLattice1 <- square_lattice(t = 64, k = 8, r = 5, l = 2, 
                                 plotNumber = c(1001, 2001),
                                 locationNames = c("FARGO", "MINOT"), 
                                 seed = 1986)
squareLattice1$infoDesign



plot_fieldhub(squareLattice1,
x = "REP",
y = "ENTRY",
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)


## -----------------------------------------------------------------------------
ARCBD1 <- RCBD_augmented(lines = 50, checks = 3, b = 6, l = 1, 
                         planter = "cartesian", 
                         plotNumber = c(1,1001),
                         seed = 23, 
                         locationNames = c("FARGO"))
ARCBD1$infoDesign


plot_fieldhub(ARCBD1,
x = "ROW",
y = "COLUMN",
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)


## -----------------------------------------------------------------------------

ibd1 <- incomplete_blocks(t = 12,
                          k = 4,
                          r = 2,
                          seed = 1984)
ibd1$infoDesign

plot_fieldhub(ibd1,
x = "ENTRY",
y = "REP",
labels = "PLOT",
factor_name = "PLOT",
width = 12,
height = 10,
reverse_y = FALSE,
reverse_x = FALSE)


