## ----setup,include=FALSE-------------------------------------------------
library('tidyverse') # Load library always required
#library('magrittr')
#library('knitr')
opts_chunk$set(
#dev = 'pdf',
#dev='tikz',
fig.align='center', 
size='footnotesize', 
#results='asis',
out.width='.65\\linewidth',
fig.path='figure/', 	
fig.keep='high', 	
fig.show='asis', 
error=F,  
fig.align='center', 
fig.asp=.65, 
#cache=TRUE,	
warning=FALSE,
message=FALSE,
comment=NA,
error=FALSE,  
#margin=TRUE,
prompt=TRUE
)
knit_hooks$set(margin=function(before,options,envir){ 
  if(before) 
    par(mar=c(4,4,0.01,0.01)) 
  else NULL 
}
)

## ------------------------------------------------------------------------
a<-c(1, 2, 5, 3, 4) # numeric vector
b<-c("one","two","three") # character vector
c<-c(TRUE,TRUE,FALSE) # logical vector
a
b[2]

## ------------------------------------------------------------------------
y<-matrix(0:9, nrow=2, ncol=5);y
y[,1]; y[2,]

## ------------------------------------------------------------------------
cells<-c(1,26,24,68) 
rnames<-c("R1","R2") 
cnames<-c("C1","C2")
mymatrix<-matrix(cells,nrow=2,ncol=2,byrow=TRUE,
          dimnames=list(rnames, cnames))
mymatrix

## ------------------------------------------------------------------------
dim1<-c("A1","A2")
dim2<-c("B1","B2","B3") 
dim3<-c("C1","C2","C3","C4")
z<-array(1:24,c(2,3,4),dimnames=list(dim1,dim2,dim3))

## ------------------------------------------------------------------------
z

## ------------------------------------------------------------------------
patientID<-c(1, 2, 3, 4) 
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor") 
patientdata<-data.frame(patientID,age,diabetes,status) 
patientdata

## ------------------------------------------------------------------------
str(patientdata)      
summary(patientdata)       

## ------------------------------------------------------------------------
head(patientdata) 

## ------------------------------------------------------------------------
patientdata$age #variable age from patientdata
patientdata[1:2] 
patientdata[c("diabetes", "status")] 

## ------------------------------------------------------------------------
diabetes<-c("Type1","Type2","Type1","Type1") 
diabetes
diabetes<-factor(diabetes)
diabetes
levels(diabetes) 
class(diabetes)

## ------------------------------------------------------------------------
status<-c("Poor","Improved","Excellent","Poor") 
status1<-factor(status,order=TRUE) 
status1
status2<-factor(status,order=TRUE,levels=c("Poor","Improved","Excellent"))
status2
status3<-ordered(status)
status3

## ------------------------------------------------------------------------
g<-"My First List" 
h<-c(25, 26, 18, 39) 
j<-matrix(1:10,nrow=2) 
k<-c("one", "two", "three")
mylist<-list(title=g,ages=h,j,k)
mylist
mylist[[2]] 
mylist[["ages"]]

## ------------------------------------------------------------------------
head(mpg)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
       geom_point(mapping = aes(x = displ, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
 geom_point(mapping = aes(x = displ, y = hwy, color = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
 geom_point(mapping = aes(x = displ, y = hwy, shape = factor(cyl)))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
 geom_point(mapping = aes(x = displ, y = hwy, size = cyl)) 

## ------------------------------------------------------------------------
ggplot(data = mpg) +      
   geom_point(mapping = aes(x = displ, y = hwy, 
           size = cyl, alpha = 0.5))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, 
    size = cyl, color = cyl, alpha = 0.5))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl, nrow = 2)

## ------------------------------------------------------------------------
ggplot(data = mpg) +       
  geom_histogram(aes(x = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) +       
  geom_density(aes(x = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) +       
  geom_boxplot(aes(x = class, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) +      
  geom_bar(aes(x = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) +    
  geom_bar(aes(x = class, color = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) +   
   geom_bar(aes(x = class, fill = class))

## ------------------------------------------------------------------------
b <- ggplot(mpg, aes(x = displ, y = hwy))
b + geom_point() 

## ------------------------------------------------------------------------
b + geom_point() + geom_smooth(method = lm)

## ------------------------------------------------------------------------
b + geom_point() + geom_smooth()

## ------------------------------------------------------------------------
b + geom_point(mapping = aes(color = class)) + geom_smooth()

## ------------------------------------------------------------------------
 b + geom_point(mapping = aes(color = class)) + 
 geom_smooth(data = filter(mpg, class == "subcompact"), 
 se = FALSE)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
stat_summary( mapping = aes(x = displ, y = hwy), 
    fun.ymin = min, fun.ymax = max, fun.y = median )

## ------------------------------------------------------------------------
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
geom_bar( mapping = aes(x = cut, fill = clarity),
position = "fill" )

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
geom_bar( mapping = aes(x = cut, fill = clarity),
position = "dodge" )

## ----echo = FALSE--------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

## ----echo = FALSE--------------------------------------------------------
ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

## ------------------------------------------------------------------------
library(nycflights13)
flights

## ------------------------------------------------------------------------
filter(flights, month == 1, day == 1)

## ------------------------------------------------------------------------
(dec25 <- filter(flights, month == 12, day == 25))

## ------------------------------------------------------------------------
df <- tibble(x = c(1, NA, 3)) 
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

## ------------------------------------------------------------------------
arrange(flights, year, month, day)

## ------------------------------------------------------------------------
arrange(flights, desc(dep_delay))

## ------------------------------------------------------------------------
# Select columns by name 
select(flights, year, month, day)

## ------------------------------------------------------------------------
select(flights, year:day)

## ------------------------------------------------------------------------
select(flights, -(year:day))

## ------------------------------------------------------------------------
flights_sml <- select(flights, year:day, ends_with("delay"),
               distance, air_time)
mutate(flights_sml, gain = arr_delay - dep_delay, 
       speed = distance / air_time * 60)

## ------------------------------------------------------------------------
mutate(flights_sml, 
gain = arr_delay - dep_delay, 
hours = air_time / 60, 
gain_per_hour = gain / hours )

## ------------------------------------------------------------------------
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
summarize(flights, delay.sd = sd(dep_delay, na.rm = TRUE))

## ------------------------------------------------------------------------
by_day <- group_by(flights, year, month, day) 
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

## ------------------------------------------------------------------------
flights %>% 
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

## ------------------------------------------------------------------------
flights %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay, na.rm = TRUE))

## ------------------------------------------------------------------------
not_cancelled <- flights %>% 
	filter(!is.na(dep_delay), !is.na(arr_delay))

## ------------------------------------------------------------------------
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

## ------------------------------------------------------------------------
not_cancelled %>% 
	group_by(year, month, day) %>% 
	summarize(n_early = sum(dep_time < 500))

## ------------------------------------------------------------------------
not_cancelled %>% 
	group_by(year, month, day) %>% 
	summarize(hour_perc = mean(arr_delay > 60))

## ------------------------------------------------------------------------
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

## ------------------------------------------------------------------------
daily <- group_by(flights, year, month, day)
(per_month <- summarize(per_day, flights = sum(flights)))

## ------------------------------------------------------------------------
# Find the worst members of each group
flights_sml %>% 
	group_by(year, month, day) %>% 
	filter(rank(desc(arr_delay)) < 10)

## ------------------------------------------------------------------------
popular_dests <- flights %>% 
	group_by(dest) %>% 
	filter(n() > 365) 
popular_dests

## ------------------------------------------------------------------------
popular_dests %>% 
	filter(arr_delay > 0) %>% 
	mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
	select(year:day, dest, arr_delay, prop_delay)

## ------------------------------------------------------------------------
tibble( 
x = 1:5, 
y = 1, 
z = x ^ 2 + y
)

## ------------------------------------------------------------------------
tribble( 
~x, ~y, ~z, 
#--|--|---- 
"a", 2, 3.6,
"b", 1, 8.5 
)

## ------------------------------------------------------------------------
df <- tibble( x = runif(5), y = rnorm(5) ) 
# Extract by name 
df$x 
df[["x"]] 
# Extract by position 
df[[1]]

## ------------------------------------------------------------------------
table1

## ------------------------------------------------------------------------
table2

## ------------------------------------------------------------------------
table3

## ------------------------------------------------------------------------
table4a  # cases

## ------------------------------------------------------------------------
table4b # population

## ------------------------------------------------------------------------
table1 %>%    
	mutate(rate = cases / population * 10000)

## ------------------------------------------------------------------------
table1 %>%    
count(year, wt = cases)

## ------------------------------------------------------------------------
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

## ------------------------------------------------------------------------
table4a

## ------------------------------------------------------------------------
table4a %>% 
gather(`1999`, `2000`, key = "year", value = "cases")

## ------------------------------------------------------------------------
table4b %>% 
gather(`1999`, `2000`, key = "year", value = "population")

## ------------------------------------------------------------------------
tidy4a <- table4a %>% 
gather(`1999`, `2000`, key = "year", value = "cases") 
tidy4b <- table4b %>% 
gather(`1999`, `2000`, key = "year", value = "population") 
left_join(tidy4a, tidy4b)

## ------------------------------------------------------------------------
table2

## ------------------------------------------------------------------------
spread(table2, key = type, value = count)

## ------------------------------------------------------------------------
table3

## ------------------------------------------------------------------------
table3 %>%
separate(rate, into = c("cases", "population"))

## ------------------------------------------------------------------------
table3 %>%
separate(rate, into = c("cases", "population"), sep = "/")

## ------------------------------------------------------------------------
table3 %>% 
separate( rate, into = c("cases", "population"), 
         convert = TRUE )

## ------------------------------------------------------------------------
table5

## ------------------------------------------------------------------------
table5 %>% 
unite(new, century, year)

## ------------------------------------------------------------------------
table5 %>% 
unite(new, century, year, sep = "")

## ------------------------------------------------------------------------
stocks <- tibble( 
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016), 
  qtr  = c(   1,    2,    3,    4,    2,    3,    4), 
  return=c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66) 
)

## ------------------------------------------------------------------------
stocks %>% 
complete(year, qtr)

## ------------------------------------------------------------------------
stocks %>%
spread(year, return)

## ------------------------------------------------------------------------
treatment <- tribble(
~ person, ~ treatment, ~response, 
"Derrick Whitmore", 1, 7, 
	            NA, 2, 10, 
                NA, 3, 9,
"Katherine Burke", 1, 4 
)

## ------------------------------------------------------------------------
treatment %>% 
fill(person)

## ----message=FALSE-------------------------------------------------------
mean(mpg$hwy)
median(mpg$hwy)
quantile(mpg$hwy,probs=c(0.25,0.5,0.75))

## ------------------------------------------------------------------------
not_cancelled %>% 
 group_by(year, month, day) %>% 
 summarize( 
 # average delay: 
 avg_delay1 = mean(arr_delay), 
 # average positive delay: 
 avg_delay2 = mean(arr_delay[arr_delay > 0]) 
)

## ----message=FALSE-------------------------------------------------------
IQR(mpg$hwy)
var(mpg$hwy)
sd(mpg$hwy)
mad(mpg$hwy)

## ------------------------------------------------------------------------
not_cancelled %>% 
	group_by(dest) %>% 
	summarize(
		distance_sd = sd(distance),
		distance_var = var(distance)
	) %>% 
	arrange(desc(distance_sd))

## ----message=FALSE-------------------------------------------------------
library(moments)
skewness(mpg$hwy,na.rm = TRUE)
kurtosis(mpg$hwy,na.rm = TRUE)

## ------------------------------------------------------------------------
library(moments)
mpg %>%
  summarize(
  mean = mean(hwy),
  sd = sd(hwy),
  skew = skewness(hwy, na.rm = FALSE),
  kurt = kurtosis(hwy, na.rm = FALSE)
)

## ------------------------------------------------------------------------
dec_stats<-function(x,na.omit=FALSE){ 
  if(na.omit) 
     x<-x[!is.na(x)] 
  m<-mean(x) 
  n<-length(x) 
  s<-sd(x) 
  skew<-sum((x-m)^3/s^3)/n 
  kurt<-sum((x-m)^4/s^4)/n - 3 
return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt)) 
}
round(dec_stats(mpg$hwy),3)

