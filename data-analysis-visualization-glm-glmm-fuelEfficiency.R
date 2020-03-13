## ----setup, include=FALSE, message=FALSE, echo=TRUE--------------------------------------------------------------------------
library(Pmisc)
library(tidyverse)
library(lme4)
knitr::opts_chunk$set(echo = TRUE)


## ---- data loading-----------------------------------------------------------------------------------------------------------
cUrl =  'https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip'
cFile = file.path(tempdir(), basename(cUrl))
download.file(cUrl, cFile)
cFile2 = unzip(cFile, exdir=tempdir())
x = read.table(cFile2, sep=',', header=TRUE, stringsAsFactors=FALSE)


## ----data cleaning-----------------------------------------------------------------------------------------------------------
# have a look
head(x)
# Restrict to non-electric vehicles, since electic vehicles are hard to compare with.
xSub = x[grep("Electricity|CNG", x$fuelType, invert=TRUE), ]
# Create a decade variable, centerd on 2000
xSub$decade = (xSub$year - 2000)/10
# Create a table of the car makes, in order form most to least cars in the dataset
makeTable = sort(table(xSub$make), decreasing=TRUE)
# Use the table above, to make a factor that returns unique car makes.
xSub$makeFac = factor(xSub$make, levels=names(makeTable))
# Make a factor for the number of cylinders, (return unique values for cylinders),
# and make 4-cylinders vehicle the reference group.
xSub$cylFac = relevel(factor(xSub$cylinders), '4')
# Check this worked
levels(xSub$cylFac)
# Get rid of vehicles with missing cylinder number
xSub = xSub[!is.na(xSub$cylFac), ]
# Make the transmission variable nicer, use grepl function to search the pattern:
xSub$transmission = factor(
  grepl("Manual", xSub$trany), levels=c(FALSE,TRUE),
	labels = c('Automatic', 'Manual'))
# Look at the classes of vehicle
table(xSub$VClass)
# Make a variable indicating if the vehicle had four-wheel drive
xSub$FWD = grepl("([[:punct:]]|[[:space:]])+4(WD|wd)",  xSub$VClass)
# Make a new, simpler vehicle type variable
xSub$type = gsub("Vans.*", "Vans",  xSub$VClass)
xSub$type = gsub("Vehicles", "Vehicle", xSub$type)
xSub$type = gsub("Standard[[:space:]]+", "", xSub$type)
xSub$type = factor(gsub("([[:punct:]]|[[:space:]])+(2|4)(WD|wd)", "",  xSub$type))


## ----final dataset-----------------------------------------------------------------------------------------------------------
# final dataset
car_final <- xSub %>% 
  select(comb08, decade, transmission, makeFac, cylFac, type, FWD)
car_final


## ----linear model------------------------------------------------------------------------------------------------------------
lm = lm(comb08 ~., data = car_final)
# summary(lm)


## ----diagram_fuelEffByMake---------------------------------------------------------------------------------------------------
# Using tidyverse
car_final %>% 
  mutate(grand_mean = mean(comb08)) %>% 
  group_by(makeFac) %>% 
  mutate(n_make = n()) %>% 
  filter(n_make >= 1000 | n_make %in% (5:15)) %>% 
  mutate(mean = mean(comb08)) %>% 
  ggplot(aes(x = makeFac, y = comb08)) +
  geom_boxplot() +
  geom_point(aes(y=mean), color = "red") +
  geom_hline(aes(yintercept = grand_mean), color = "blue") +
  coord_flip()


## ----lmm model---------------------------------------------------------------------------------------------------------------
lmm_make <- lme4::lmer(comb08 ~ cylFac + 
                  decade + transmission + 
                  (1|makeFac),
                data=car_final)
summary(lmm_make)


## ----------------------------------------------------------------------------------------------------------------------------
my_random_effects <- lme4::ranef(lmm_make, condVar=TRUE)

ranef_df <- as.data.frame(my_random_effects)
# ggplot
ranef_df %>% 
  ggplot(aes(x = grp, y = condval, ymin = condval - 2*condsd, ymax = condval + 2*condsd)) +
  geom_point() +
  geom_errorbar() +
  coord_flip()


## ----diagram_automanufacturers-----------------------------------------------------------------------------------------------
x = data.frame(
	make = rownames(my_random_effects$makeFac),
	est = my_random_effects$makeFac[[1]],
	se = drop(attributes(my_random_effects$makeFac)$postVar),
	stringsAsFactors = FALSE
	)
x$lower = x$est - 2*x$se
x$upper = x$est + 2*x$se
x = x[x$se < 2, ]
x = x[order(x$est), ]

x$index = rank(x$est)
x$accurate = rank(x$se) < 40

x$col= rep_len(RColorBrewer::brewer.pal(8, 'Set2'), nrow(x))
x$colTrans = paste0(x$col, '40')
x$colLine = x$col
x[!x$accurate,'colLine'] = x[!x$accurate,'colTrans']

x$cex = -log(x$se) 
x$cex = x$cex - min(x$cex)
x$cex = 3*x$cex / max(x$cex)


x$textpos = rep_len(c(4,2), nrow(x))
x[!x$accurate & x$est > 0, 'textpos'] = 4
x[!x$accurate & x$est < 0, 'textpos'] = 2

x$textloc = x$est


x$textCex = c(0.5, 0.9)[1+x$accurate]


par(mar=c(4,0,0,0), bty='n')
plot(x$est, x$index, yaxt='n', xlim = range(x$est),
	#xlim = range(x[,c('lower','upper')]),
	xlab='mpg', ylab='', pch=15, col=x$colTrans , cex=x$cex)

x[!x$accurate & x$est > 0, 'textloc'] = par('usr')[1]
x[!x$accurate & x$est < 0, 'textloc'] = par('usr')[2]

abline(v=0, col='grey')
segments(x$lower, x$index, x$upper, x$index, pch=15, col=x$colLine)
text(
	x$textloc, 
	x$index, x$make,
	pos = x$textpos,
	col=x$col,
	cex=x$textCex, offset=1)


## ----lmm_add_variables-------------------------------------------------------------------------------------------------------
lmm_make_2 = lme4::lmer(comb08~cylFac+type+FWD+decade+transmission+
                          (1|makeFac),
                        data = car_final)
summary(lmm_make_2)


## ----LRT---------------------------------------------------------------------------------------------------------------------
lmtest::lrtest(lmm_make_2, lmm_make)


## ----LMM_randomSlope---------------------------------------------------------------------------------------------------------
lmm_make_3 = lme4::lmer(comb08 ~ cylFac+type+FWD+decade+transmission+
                          (1+decade|makeFac),
                        data=car_final)
summary(lmm_make_3)


## ----LRT2--------------------------------------------------------------------------------------------------------------------
lmtest::lrtest(lmm_make_2, lmm_make_3)


## ----reduce sample size------------------------------------------------------------------------------------------------------
# restrict the data to just car make with more than 800 vehicles
car_800 <- car_final %>%
  mutate(grand_mean = mean(comb08)) %>%
  group_by(makeFac) %>%
  mutate(n_make = n()) %>%
  filter(n_make >= 800)
# re-fit model with new dataset
lmm_make_reduced1 = lme4::lmer(comb08~cylFac+type+FWD+decade+transmission+(1|makeFac), data = car_800)
# re-fit model with new dataset random slope
lmm_make_reduced2 = lme4::lmer(comb08~cylFac+type+FWD+decade+transmission+(1+decade|makeFac), data = car_800)
# redo LRT
lmtest::lrtest(lmm_make_reduced1, lmm_make_reduced2)


## ----diagram_sloperandom-----------------------------------------------------------------------------------------------------
car_800 %>%
  ggplot(aes(x=decade, y=comb08))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm")+
  facet_wrap(~makeFac)


## ----histogram---------------------------------------------------------------------------------------------------------------
# hist of response
hist(car_final$comb08)


## ----ggplot response---------------------------------------------------------------------------------------------------------
# ggplot
car_final %>%
  ggplot(aes(x=comb08))+
  geom_histogram(fill="darkgrey", color="black", bins=60)


## ----GLMM--------------------------------------------------------------------------------------------------------------------
glmm_make = lme4::glmer(comb08 ~ cylFac+transmission+decade+(1|makeFac),
                        family=Gamma(link=log),
                        data=xSub[xSub$year < 2000, ])

lme4::VarCorr(glmm_make)

lattice::dotplot(lme4::ranef(glmm_make), condVar = TRUE)


## ----plot_1------------------------------------------------------------------------------------------------------------------
Pmisc::ranefPlot(glmm_make, grpvar = "makeFac",  level = 0.5, maxNames = 12)

