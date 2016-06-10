require(utils)
blank <- expand.grid(age = c("A","B","C","D","E","F"),
            gen = c("Female","Male"),
            eth = c("Maori", "European or other","Pacific"  ),
            quin= c("01", "02", "03", "04", "05"),
            year= c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"),
            dhb = c("Auckland" ,"Bay of Plenty","Canterbury", "Capital & Coast",    "Counties Manukau",   "Hawke's Bay", "Hutt Valley",      
                    "Lakes", "MidCentral", "Nelson Marlborough", "Northland", "Otago", "South Canterbury", "Southland", 
                    "Tairawhiti", "Taranaki", "Waikato", "Wairarapa", "Waitemata", "West Coast", "Whanganui"),
            deaths = 0, pop = 0)

c01 <- read.csv("R2001.csv"); c06 <- read.csv("R2006.csv"); c13 <- read.csv("R2013.csv")
lb <- read.csv("lbimp.csv"); lb$year <- factor(lb$year); lb$dep <- factor(lb$dep); lb$age <- as.factor("lb"); levels(lb$eth) <- c("Asian", "European","Maori", "MELAA", "Pacific" , "Total" ,"Unknown")
c01$y2001 <- 2001; c06$y2006 <- 2006; c13$y2013 <- 2013
levels(c01$dhb);  levels(c06$dhb);  levels(c13$dhb); levels(lb$dhb)
levels(c01$dhb) <- c("Area outside District Health Board", "Auckland", "Bay of Plenty", "Canterbury", 
                     "Capital & Coast", "Counties Manukau" , "Hawke's Bay" , "Hutt Valley" , "Lakes" ,
                     "MidCentral" , "Nelson Marlborough" , "New Zealand" , "Northland" , "Otago" ,
                     "South Canterbury" , "Southland" , "Tairawhiti" , "Taranaki" ,
                     "Waikato" , "Wairarapa"  ,"Waitemata" , "West Coast" , "Whanganui" )
levels(c06$dhb) <- levels(c01$dhb); levels(c13$dhb) <- levels(c01$dhb)
## but remove AODHB, Unknown, Total and NZ
lb  <-  lb[lb$dhb  != "Area outside District Health Board",];lb  <-  lb[lb$dhb  != "Unknown",];lb  <-  lb[lb$dhb  != "New Zealand",]
c01 <- c01[c01$dhb != "Area outside District Health Board",];c01 <- c01[c01$dhb != "New Zealand",]
c06 <- c06[c06$dhb != "Area outside District Health Board",];c06 <- c06[c06$dhb != "New Zealand",]
c13 <- c13[c13$dhb != "Area outside District Health Board",];c13 <- c13[c13$dhb != "New Zealand",]
#lb <- droplevels(lb); c01 <- droplevels(c01); c06 <- droplevels(c06); c13 <- droplevels(c13)
# eth doesnt
levels(c01$eth);  levels(c06$eth);  levels(c13$eth); levels(lb$eth)
#lb  "Asian"    "European" "Maori"    "MELAA"    "Pacific"  "Total"    "Unknown" 
#2001"Asian"    "European" "Maori"    "Other"    "Pacific"  "Total"   
#2006"Asian"    "European" "Maori"    "MELAA"    "Other"    "Pacific"  "Total"   
#2013"Asian"    "European" "Maori"    "MELAA"    "Other"    "Pacific"  "Total"  
# combine eth as stated above
levels(lb$eth)  <- list("Maori" = "Maori", "European or other" = c("European", "Asian", "MELAA"), "Pacific" = "Pacific", "Total" = "Total", "Unknown" = "Unknown")
levels(c01$eth) <- list("Maori" = "Maori", "European or other" = c("European", "Asian", "Other"), "Pacific" = "Pacific", "Total" = "Total")
levels(c06$eth) <- list("Maori" = "Maori", "European or other" = c("European", "Asian", "Other", "MELAA"), "Pacific" = "Pacific", "Total" = "Total")
levels(c13$eth) <- list("Maori" = "Maori", "European or other" = c("European", "Asian", "Other", "MELAA"), "Pacific" = "Pacific", "Total" = "Total")
levels(c01$eth);  levels(c06$eth);  levels(c13$eth); levels(lb$eth)
lb  <-   lb[lb$eth  != "Total",];lb  <-  lb[lb$eth  != "Unknown",]
c01 <-  c01[c01$eth  != "Total",]
c06 <-  c06[c06$eth  != "Total",]
c13 <-  c13[c13$eth  != "Total",]
lb <- droplevels(lb); c01 <- droplevels(c01); c06 <- droplevels(c06); c13 <- droplevels(c13)
# 3 levels
levels(c01$eth);  levels(c06$eth);  levels(c13$eth); levels(lb$eth)
#lb   "Maori"             "European or other" "Pacific"           
#2001 "Maori"             "European or other" "Pacific"                    
#2006 "Maori"             "European or other" "Pacific"               
#2013 "Maori"             "European or other" "Pacific"       
# now eth does!
#
c01$dep <- factor(c01$dep); c06$dep <- factor(c06$dep); c13$dep <- factor(c13$dep)
levels(c01$dep);  levels(c06$dep);  levels(c13$dep); levels(lb$dep)
# dep doesnt
#2001 ".." "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
#2006 ".." "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
#2013 ".." "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
#lb   "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "99"
## make 10 level dep into 5 level quin
levels(c01$dep) <- list("01" = c("1","2"), "02" = c("3", "4"), "03" = c("5","6"), "04" = c("7","8"), "05" = c("9","10"), "Unk" = "..")
levels(c06$dep) <- list("01" = c("1","2"), "02" = c("3", "4"), "03" = c("5","6"), "04" = c("7","8"), "05" = c("9","10"), "Unk" = "..")
levels(c13$dep) <- list("01" = c("1","2"), "02" = c("3", "4"), "03" = c("5","6"), "04" = c("7","8"), "05" = c("9","10"), "Unk" = "..")
levels(lb$dep)  <- list("01" = c("1","2"), "02" = c("3", "4"), "03" = c("5","6"), "04" = c("7","8"), "05" = c("9","10"), "Unk" = "99")
# drop Unk
c01 <- c01[c01$dep != "Unk",];c06 <- c06[c06$dep != "Unk",];c13 <- c13[c13$dep != "Unk",];lb <- lb[lb$dep != "Unk",]
levels(c01$dep);  levels(c06$dep);  levels(c13$dep); levels(lb$dep)
#lb <- droplevels(lb); c01 <- droplevels(c01); c06 <- droplevels(c06); c13 <- droplevels(c13)
## tidy all levels
# aggregate identical units 
c01 <- aggregate(pop  ~ age + y2001 + dhb + eth + gen + dep, data = c01, sum)
c06 <- aggregate(pop  ~ age + y2006 + dhb + eth + gen + dep, data = c06, sum)
c13 <- aggregate(pop  ~ age + y2013 + dhb + eth + gen + dep, data = c13, sum)
lb  <- aggregate(lpop  ~ age + year + dhb + eth + gen + dep, data = lb, sum)
names(c01) <- c("age" , "y2001" , "dhb" , "eth" , "gen" , "quin"  ,"pop01")
names(c06) <- c("age" , "y2006" , "dhb" , "eth" , "gen" , "quin"  ,"pop06")
names(c13) <- c("age" , "y2013" , "dhb" , "eth" , "gen" , "quin"  ,"pop13")
names(lb)  <- c("age" , "year" , "dhb" , "eth" , "gen" , "quin"  ,"pop")
lb <- droplevels(lb); c01 <- droplevels(c01); c06 <- droplevels(c06); c13 <- droplevels(c13)
## split lb years
lbyears<-split(lb,lb$year); rm(lb)
## name each object
names(lbyears) <- c("2002", "2003", "2004", "2005", "2006" , "2007" , "2008" , "2009" , "2010" , "2011" , "2012" , "2013", "2014")
lb02 <- as.data.frame(lbyears[1]);colnames(lb02)  <- c("age", "y2002", "dhb", "eth", "gen", "quin", "lpop02" )
lb03 <- as.data.frame(lbyears[2]);colnames(lb03)  <- c("age", "y2003", "dhb", "eth", "gen", "quin", "lpop03" )
lb04 <- as.data.frame(lbyears[3]);colnames(lb04)  <- c("age", "y2004", "dhb", "eth", "gen", "quin", "lpop04" )
lb05 <- as.data.frame(lbyears[4]);colnames(lb05)  <- c("age", "y2005", "dhb", "eth", "gen", "quin", "lpop05" )
lb06 <- as.data.frame(lbyears[5]);colnames(lb06)  <- c("age", "y2006", "dhb", "eth", "gen", "quin", "lpop06" )
lb07 <- as.data.frame(lbyears[6]);colnames(lb07)  <- c("age", "y2007", "dhb", "eth", "gen", "quin", "lpop07" )
lb08 <- as.data.frame(lbyears[7]);colnames(lb08)  <- c("age", "y2008", "dhb", "eth", "gen", "quin", "lpop08" )
lb09 <- as.data.frame(lbyears[8]);colnames(lb09)  <- c("age", "y2009", "dhb", "eth", "gen", "quin", "lpop09" )
lb10 <- as.data.frame(lbyears[9]);colnames(lb10)  <- c("age", "y2010", "dhb", "eth", "gen", "quin", "lpop10" )
lb11 <- as.data.frame(lbyears[10]);colnames(lb11) <- c("age", "y2011", "dhb", "eth", "gen", "quin", "lpop11" )
lb12 <- as.data.frame(lbyears[11]);colnames(lb12) <- c("age", "y2012", "dhb", "eth", "gen", "quin", "lpop12" )
lb13 <- as.data.frame(lbyears[12]);colnames(lb13) <- c("age", "y2013", "dhb", "eth", "gen", "quin", "lpop13" )
lb14 <- as.data.frame(lbyears[13]);colnames(lb14) <- c("age", "y2014", "dhb", "eth", "gen", "quin", "lpop14" )
rm(lbyears)
## work with AB and lb only now. jimmy around owing to lack of 2001 lbs
c01AB <- c01[c01$age == "AB",]; c01AB <- c01AB[c(2,3,4,5,6,7)]
lb06  <- lb06[c(2,3,4,5,6,7)]
c06AB <- c06[c06$age == "AB",]; c06AB <- c06AB[c(2,3,4,5,6,7)]
lb13  <- lb13[c(2,3,4,5,6,7)]
c13AB <- c13[c13$age == "AB",]; c13AB <- c13AB[c(2,3,4,5,6,7)]
d06 <- merge(c06AB, lb06, c("dhb", "eth", "gen", "quin", "y2006"), all =T)
d06[is.na(d06)] <- 0
d06$Apop06 <- d06$pop06 - d06$lpop06
d06$Apop06[d06$pop06 == 0] <- 0
d13 <- merge(c13AB, lb13, c("dhb", "eth", "gen", "quin", "y2013"), all =T)
d13[is.na(d13)] <- 0
d13$Apop13 <- d13$pop13 - d13$lpop13
d13$Apop13[d13$pop13 == 0] <- 0
rm(c01AB,c06AB,c13AB)
## tweak to make one obj for each year with A and B
a06 <- d06[c(1,2,3,4,5,7)]; b06 <- d06[c(1,2,3,4,5,8)]; rm(d06)
a06$age <- "A"; b06$age <- "B"
colnames(a06) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
colnames(b06) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
AB06 <- rbind(a06,b06)
a13 <- d13[c(1,2,3,4,5,7)]; b13 <- d13[c(1,2,3,4,5,8)]; rm(d13)
a13$age <- "A"; b13$age <- "B"
colnames(a13) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
colnames(b13) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
AB13 <- rbind(a13,b13)
## make 2002 set for extrap
names(c01) <- c("age" , "y2001" , "dhb" , "eth" , "gen" , "quin"  ,"pop01")
names(c06) <- c("age" , "y2006" , "dhb" , "eth" , "gen" , "quin"  ,"pop06")
a <- merge(c01, c06, c("dhb", "eth", "gen", "quin", "age"), all =T)
b <- merge(a, c13, c("dhb", "eth", "gen", "quin", "age"), all =T); rm(a)
b$pop02 <- b$pop01 + (b$pop06 - b$pop01)/5; b$y2002 <- 2002
c02 <- b[c(5,13,1,2,3,4,12)]
lb02  <- lb02[c(2,3,4,5,6,7)]
c02AB <- c02[c02$age == "AB",]; c02AB <- c02AB[c(2,3,4,5,6,7)]
d02 <- merge(c02AB, lb02, c("dhb", "eth", "gen", "quin", "y2002"), all =T)
d02[is.na(d02)] <- 0
x <- which(d02$pop < d02$lpop)
d02$Apop02 <- d02$pop02 - d02$lpop02
d02$Apop02[x] <- d02$lpop[x]
a02 <- d02[c(1,2,3,4,5,7)]; b02 <- d02[c(1,2,3,4,5,8)]
a02$age <- "A"; b02$age <- "B"
colnames(a02) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
colnames(b02) <- c("dhb", "eth", "gen", "quin", "year", "pop", "age")
AB02 <- rbind(a02,b02)
rm(lb02,b,d02,c02AB)
a <- merge(AB02, AB06, c("dhb", "eth", "gen", "quin", "age"), all =T)
b <- merge(a, AB13, c("dhb", "eth", "gen", "quin", "age"), all =T); rm(a)
colnames(b) <- c( "dhb" , "eth" , "gen" , "quin" , "age", "y2002", "pop02", "y2006", "pop06", "y2013", "pop13")
c <- c02[c02$age != "AB",]; c <- c[c(3,4,5,6,1,2,7)]
d <- c06[c06$age != "AB",]; c <- c[c(3,4,5,6,1,2,7)]
e <- c13[c13$age != "AB",]; c <- c[c(3,4,5,6,1,2,7)]
f <- merge(c,d,c("dhb", "eth", "gen", "quin", "age"), all = T)
g <- merge(f,e,c("dhb", "eth", "gen", "quin", "age"), all = T)
h <- rbind(b,g); h$age <- factor(h$age); rm(b,c,d,e,f,g)
denom1 <- h[h$age != "A",]
x <- h[h$age == "A",]
denom1$y2003 <- 2003; denom1$pop03 <- round(denom1$pop02 + (denom1$pop06 - denom1$pop02)/4,0)
denom1$y2004 <- 2004; denom1$pop04 <- round(denom1$pop03 + (denom1$pop06 - denom1$pop02)/4,0)
denom1$y2005 <- 2005; denom1$pop05 <- round(denom1$pop04 + (denom1$pop06 - denom1$pop02)/4,0)
denom1$y2007 <- 2007; denom1$pop07 <- round(denom1$pop06 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2008 <- 2008; denom1$pop08 <- round(denom1$pop07 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2009 <- 2009; denom1$pop09 <- round(denom1$pop08 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2010 <- 2010; denom1$pop10 <- round(denom1$pop09 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2011 <- 2011; denom1$pop11 <- round(denom1$pop10 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2012 <- 2012; denom1$pop12 <- round(denom1$pop11 + (denom1$pop13 - denom1$pop06)/7,0) 
denom1$y2014 <- 2014; denom1$pop14 <- round(denom1$pop13 + (denom1$pop13 - denom1$pop06)/7,0) 
## make lbs
lb03$age <- "A"; colnames(lb03)[7] <- "pop03"
y <- merge(x,lb03, c("dhb","eth","gen","quin", "age"), all = T)
lb04$age <- "A"; colnames(lb04)[7] <- "pop04"
x <- merge(y,lb04, c("dhb","eth","gen","quin", "age"), all = T)
lb05$age <- "A"; colnames(lb05)[7] <- "pop05"
y <- merge(x,lb05, c("dhb","eth","gen","quin", "age"), all = T)
lb07$age <- "A"; colnames(lb07)[7] <- "pop07"
x <- merge(y,lb07, c("dhb","eth","gen","quin", "age"), all = T)
lb08$age <- "A"; colnames(lb08)[7] <- "pop08"
y <- merge(x,lb08, c("dhb","eth","gen","quin", "age"), all = T)
lb09$age <- "A"; colnames(lb09)[7] <- "pop09"
x <- merge(y,lb09, c("dhb","eth","gen","quin", "age"), all = T)
lb10$age <- "A"; colnames(lb10)[7] <- "pop10"
y <- merge(x,lb10, c("dhb","eth","gen","quin", "age"), all = T)
lb11$age <- "A"; colnames(lb11)[7] <- "pop11"
x <- merge(y,lb11, c("dhb","eth","gen","quin", "age"), all = T)
lb12$age <- "A"; colnames(lb12)[7] <- "pop12"
y <- merge(x,lb12, c("dhb","eth","gen","quin", "age"), all = T)
lb14$age <- "A"; colnames(lb14)[7] <- "pop14"
x <- merge(y,lb14, c("dhb","eth","gen","quin", "age"), all = T)
denom1 <- rbind(x, denom1)
rm(lb03,lb04,lb05,lb06,lb07,lb08,lb09,lb10,lb11,lb12,lb13,lb14,x,y,h,a02,a06,a13,AB02,AB06,AB13,b02,b06,b13,c01,c02,c06,c13)
f02 <- denom1[c(1,2,3,4,5,6,7)]  ; colnames(f02)[c(6,7)] <- c("year","pop")
f03 <- denom1[c(1,2,3,4,5,12,13)]; colnames(f03)[c(6,7)] <- c("year","pop")
f04 <- denom1[c(1,2,3,4,5,14,15)]; colnames(f04)[c(6,7)] <- c("year","pop")
f05 <- denom1[c(1,2,3,4,5,16,17)]; colnames(f05)[c(6,7)] <- c("year","pop")
f06 <- denom1[c(1,2,3,4,5,8,9)]  ; colnames(f06)[c(6,7)] <- c("year","pop")
f07 <- denom1[c(1,2,3,4,5,18,19)]; colnames(f07)[c(6,7)] <- c("year","pop")
f08 <- denom1[c(1,2,3,4,5,20,21)]; colnames(f08)[c(6,7)] <- c("year","pop")
f09 <- denom1[c(1,2,3,4,5,22,23)]; colnames(f09)[c(6,7)] <- c("year","pop")
f10 <- denom1[c(1,2,3,4,5,24,25)]; colnames(f10)[c(6,7)] <- c("year","pop")
f11 <- denom1[c(1,2,3,4,5,26,27)]; colnames(f11)[c(6,7)] <- c("year","pop")
f12 <- denom1[c(1,2,3,4,5,28,29)]; colnames(f12)[c(6,7)] <- c("year","pop")
f13 <- denom1[c(1,2,3,4,5,10,11)]; colnames(f13)[c(6,7)] <- c("year","pop")
f14 <- denom1[c(1,2,3,4,5,30,31)]; colnames(f14)[c(6,7)] <- c("year","pop")
denom <- rbind(f02,f03,f04,f05,f06,f07,f08,f09,f10,f11,f12,f13,f14); rm(denom1,f02,f03,f04,f05,f06,f07,f08,f09,f10,f11,f12,f13,f14)
denom$year <- factor(denom$year)
## END ##
##
# Generate numerator file
##
nums <- read.csv("Mort12Oct2015.csv")
nums <- nums[nums$AgeGroup != "Z",]
nums <- nums[nums$AgeGroup != "",]
nums <- nums[nums$DHBResidence != "Overseas",]
nums <- nums[nums$NzhisDep06 != "NA",]
row.has.na <- apply(nums, 1, function(x){any(is.na(x))}); sum(row.has.na);nums <- nums[!row.has.na,]
nums$NzhisDep06 <- as.factor(nums$NzhisDep06)
nums <- droplevels(nums)
nums <- nums[,c(2,3,4,5,6,7,10,12,13,14)]
names(nums) <- c( "icd" , "gen" , "agey" , "year" , "age" , "dhb", "eth", "quin", "cat", "cau")
levels(nums$dhb) <- c("Auckland", "Bay of Plenty", "Canterbury", 
                      "Capital & Coast", "Counties Manukau" , "Hawke's Bay" ,
                      "Hutt Valley" , "Lakes" , "MidCentral" ,
                      "Nelson Marlborough" , "Northland",
                      "Otago" , "South Canterbury" , "Southland" ,
                      "Tairawhiti" , "Taranaki" ,"Waikato" , 
                      "Wairarapa"  ,"Waitemata" , "West Coast" , "Whanganui" )
levels(nums$eth) <- list("Maori" = "Maori", "European or other" = c("European", "Asian", "MELAA", "Other"), "Pacific" = "Pacific Peoples")
row.has.na <- apply(nums, 1, function(x){any(is.na(x))}); sum(row.has.na);nums <- nums[!row.has.na,]
levels(nums$quin) <- list("01" = c("1","2"), "02" = c("3", "4"), "03" = c("5","6"), "04" = c("7","8"), "05" = c("9","10"))
levels(nums$gen) <- c("Female","Male")
nums$year <- factor(nums$year)
nums <- droplevels(nums)
##
# Merge Denoms and Nums
##
nums <- nums[,c(2,4,5,6,7,8)]
nums$deaths <- 1
temp <- aggregate(deaths ~ gen + year + age + dhb + eth + quin, data = nums,FUN = sum)
names(temp) <- names(nums)
temp <- temp[c(4,5,1,6,3,2,7)]
temp <- merge(temp, denom, c("dhb", "eth", "gen", "quin", "age", "year"), all =T)
temp$deaths[is.na(temp$deaths)] <- 0
temp$pop[is.na(temp$pop)] <- 0
temp$pop[temp$pop < 1] <-0
x <- which(temp$pop < temp$deaths)
temp$deaths[x] <- 0
x <- temp[temp$pop < temp$deaths,]
x
mort <- temp
write.csv(mort, "mort.csv")
temp <- rbind(blank,mort)
mort <- aggregate(cbind(deaths,pop) ~ gen + age + eth + quin + dhb + year, data = temp, FUN = sum)
rm(denom,nums,temp,x,row.has.na, blank)