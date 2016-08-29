## set wd to dropbox folder
setwd("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis")

## import csv
card_data <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/card_data_analysis_csv_7july2015.csv")

## libraries used
library(multcomp)

## see the headings and attach names
names(card_data)
attach(card_data)

## refactor type and song
card_data$Type = as.factor(card_data$Type)
levels(card_data$Type) = c("Cactus.Wren","Texas","Bill.Williams","Portal")
print(levels(card_data$Type))

card_data$Song = as.factor(card_data$Song)
levels(card_data$Song) = c("Cactus.Wren",
                           "Texas.1","Texas.2","Texas.4","Texas.5","Texas.10","Texas.11","Texas.12",
                           "Bill.Williams.1","Bill.Williams.2","Bill.Williams.4","Bill.Williams.5","Bill.Williams.6",
                           "Portal.4","Portal.14","Portal.21","Portal.23","Portal.24")
print(levels(card_data$Song))

mean(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]) #21.07463
mean(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]) #18.86567
mean(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]) #20.83582
mean(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]) #10.71642

close_resp_mean = c(mean(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]),
                    mean(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]),
                    mean(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]),
                    mean(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]))
songtypes = c("Cactus.Wren","Texas","Bill.Williams","Portal")

sum(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]==24) #50
sum(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]==24) #45
sum(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]==24) #53
sum(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]==24) #23

sd(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]) #5.638916
sd(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]) #8.077082
sd(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]) #6.725341
sd(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]) #10.20666


## summary of the data
summary(card_data)
  ## note: Date = 42140 = May 16 2015, Time = 0.25 = 06:00:00 AM

## looking at basic relationships
plot(card_data$Type,card_data$Time) ## not dif
plot(card_data$Type,card_data$Date) ## not dif
plot(card_data$Type,card_data$Order) ## texas dif, but not sig
## t.test(card_data$Order[card_data$Type=="Texas"],card_data$Order[card_data$Type=="Cactus.Wren"])

## plot(card_data$Close.Sum.As.Number,card_data$Time)
plot(card_data$Time[
  card_data$Close.Sum.As.Number!=24],
  card_data$Close.Sum.As.Number[card_data$Close.Sum.As.Number!=24])
abline(card_data$Time[
  card_data$Close.Sum.As.Number!=24],
  card_data$Close.Sum.As.Number[card_data$Close.Sum.As.Number!=24])

plot(card_data$Close.Pre.As.Number[
  card_data$Close.Sum.As.Number!=24],
     card_data$Close.Resp.As.Number[
       card_data$Close.Sum.As.Number!=24])
abline(card_data$Close.Pre.As.Number[
  card_data$Close.Sum.As.Number!=24],
       card_data$Close.Resp.As.Number[
         card_data$Close.Sum.As.Number!=24])

plot(card_data$Close.Pre.As.Number,
     card_data$Close.Resp.As.Number)
abline(card_data$Close.Pre.As.Number,
       card_data$Close.Resp.As.Number)


## start making the glm
card_closest_glm1 <- glm(Close.Resp.As.Number ~ Point + 
                             Order + Date + Type + Time + Close.Pre.As.Number + Close.Sum.As.Number)
summary(card_closest_glm1)

  ## need to take into account the non independence

## make a second glm with all terms 
  ##(not point, not locality, not the not-as-number, removed PB or Post only, removed minuses, removed div.2, removed not pres/abs, removed sums, removed 0 to 4)
card_closest_glm_all = glm(Close.Resp.As.Number ~ Order+ Song+ Time+ Date+ Type+
                             Count.24.Plus.Pre+
                             Chips.Pre+ Chips.Pres.Abs.Resp+
                             Songs.Less.24.Pre+ Songs.Less.24.Resp+
                             Songs.24.Plus.Pre+ Songs.24.Plus.Resp+
                             Pre.0.to.1+ Pre.2.to.4+ Pre.8.to.16+ Pre.1.to.2+Pre.4.to.8+Pre.16.to.24+
                             Resp.0.to.1+Resp.2.to.4+Resp.8.to.16+Resp.24.Plus+ Resp.1.to.2+Resp.4.to.8+Resp.16.to.24,
                             Close.Pre.As.Number+
                             Flyby.Pre+Flyby.Resp+
                           data=card_data)
summary(card_closest_glm_all)
alias(card_closest_glm_all)


## start making the glmm
install.packages("lme4")
library(lme4)

## list of univariate glms

## closest distance response 
## given date, not sig
card_close_date_glm =
  glm(Close.Resp.As.Number ~ 
        Date, data=card_data)
par(mfrow=c(2,2))
plot(card_close_date_glm)
summary(card_close_date_glm)

## given time, not sig
card_close_time_glm = 
  glm(Close.Resp.As.Number ~ 
        Time, data=card_data)
par(mfrow=c(2,2))
plot(card_close_time_glm)
summary(card_close_time_glm)

## given type, PT sig negative, rest not - THIS IS WITHOUT CHANGE IN FACTORS
## with change in factors, BW sig negative, rest not
card_close_type_glm = 
  glm(Close.Resp.As.Number ~ 
        Type, data=card_data)
par(mfrow=c(2,2))
plot(card_close_type_glm)
summary(card_close_type_glm)

## given song, all PT sig, BW6 sig, BW4,TX5 almost sig, rest not - THIS IS BEFORE REFACTOR
## after refactor, TX5,TX11,BW1,BW2,BW4 very sig, TX12 sig, TX2,PT24 almost sig
card_close_song_glm = 
  glm(Close.Resp.As.Number ~ 
        Song, data=card_data)
par(mfrow=c(2,2))
plot(card_close_song_glm)
summary(card_close_song_glm)

## given point, not sig (but is this meaningful?)
card_close_point_glm = 
  glm(Close.Resp.As.Number ~ 
        Point, data=card_data)
par(mfrow=c(2,2))
plot(card_close_point_glm)
summary(card_close_point_glm)

## given order, not sig
card_close_order_glm = 
  glm(Close.Resp.As.Number ~ 
        Order, data=card_data)
par(mfrow=c(2,2))
plot(card_close_order_glm)
summary(card_close_order_glm)

## given pre closest - sig, positive
card_close_pre_resp_glm = 
  glm(Close.Resp.As.Number ~ 
        Close.Pre.As.Number, data=card_data)
par(mfrow=c(2,2))
plot(card_close_pre_resp_glm)
summary(card_close_pre_resp_glm)

## given resp flyby - very sig, negative
card_close_fb_resp_glm = 
  glm(Close.Resp.As.Number ~ 
        Flyby.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_close_fb_resp_glm)
summary(card_close_fb_resp_glm)

## given pre flyby - sig, negative
card_close_fb_pre_glm = 
  glm(Close.Resp.As.Number ~ 
        Flyby.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_close_fb_pre_glm)
summary(card_close_fb_pre_glm)

## given resp chips, very sig, negative
card_close_chips_resp_glm = 
  glm(Close.Resp.As.Number ~ 
        Chips.Pres.Abs.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_close_chips_resp_glm)
summary(card_close_chips_resp_glm)

## given pre chips - not sig
Chips.Pre.1 = Chips.Pre
card_close_chips_pre_glm = 
  glm(Close.Resp.As.Number ~ 
        Chips.Pre.1, data=card_data)
par(mfrow=c(2,2))
plot(card_close_chips_pre_glm)
summary(card_close_chips_pre_glm)

## given plus resp songs, sig, negative
card_close_plus_resp_glm = 
  glm(Close.Resp.As.Number ~ 
        Songs.24.Plus.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_close_plus_resp_glm)
summary(card_close_plus_resp_glm)

## given plus pre songs, sig, negative
## pre and resp songs are correlated tho
card_close_plus_pre_glm = 
  glm(Close.Resp.As.Number ~ 
        Songs.24.Plus.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_close_plus_pre_glm)
summary(card_close_plus_pre_glm)

## given less resp songs, very sig, negative
card_close_less_resp_glm = 
  glm(Close.Resp.As.Number ~ 
        Songs.Less.24.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_close_less_resp_glm)
summary(card_close_less_resp_glm)

## given less pre songs, sig, negative
## but pre and resp less songs are correlated
card_close_less_pre_glm = 
  glm(Close.Resp.As.Number ~ 
        Songs.Less.24.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_close_less_pre_glm)
summary(card_close_less_pre_glm)



## flyby resp
## given flyby pre, not sig
card_flyby_pre_resp_glm = 
  glm(Flyby.Resp ~ 
        Flyby.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_pre_resp_glm)
summary(card_flyby_pre_resp_glm)

## given date, not sig
card_flyby_date_glm = 
  glm(Flyby.Resp ~ 
        Date, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_date_glm)
summary(card_flyby_date_glm)

## given time, not sig
card_flyby_time_glm = 
  glm(Flyby.Resp ~ 
        Time, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_time_glm)
summary(card_flyby_time_glm)

## given type, PT sig positive, rest not BEFORE REFACTOR
## after refactor, BW sig pos, rest not
card_flyby_type_glm = 
  glm(Flyby.Resp ~ 
        Type, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_type_glm)
summary(card_flyby_type_glm)

## given song, all PT sig pos, BW6 sig pos BEFORE REFACTOR
## after refactor, TX11,BW1,BW2,BW4 very sig, TX5,TX12 sig
card_flyby_song_glm = 
  glm(Flyby.Resp ~ 
        Song, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_song_glm)
summary(card_flyby_song_glm)

## given point, not sig (but is this meaningful?)
card_flyby_point_glm = 
  glm(Flyby.Resp ~ 
        Point, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_point_glm)
summary(card_flyby_point_glm)

## given order, not sig
card_flyby_order_glm = 
  glm(Flyby.Resp ~ 
        Order, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_order_glm)
summary(card_flyby_order_glm)

## given pre closest -not sig
card_flyby_pre_close_glm = 
  glm(Flyby.Resp ~ 
        Close.Pre.As.Number, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_pre_close_glm)
summary(card_flyby_pre_close_glm)

## given resp chips, very sig pos
Chips.Resp.1 = card_data$Chips.Pres.Abs.Resp
card_flyby_chips_resp_glm = 
  glm(Flyby.Resp ~ 
        Chips.Resp.1, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_chips_resp_glm)
summary(card_flyby_chips_resp_glm)

## given pre chips - not sig
Chips.Pre.1 = card_data$Chips.Pre
card_flyby_chips_pre_glm = 
  glm(Flyby.Resp ~ 
        Chips.Pre.1, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_chips_pre_glm)
summary(card_flyby_chips_pre_glm)

## given plus resp songs, not sig, negative
card_flyby_plus_resp_glm = 
  glm(Flyby.Resp ~ 
        Songs.24.Plus.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_plus_resp_glm)
summary(card_flyby_plus_resp_glm)

## given plus pre songs, sig, negative
## pre and resp songs are correlated tho
card_flyby_plus_pre_glm = 
  glm(Flyby.Resp ~ 
        Songs.24.Plus.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_plus_pre_glm)
summary(card_flyby_plus_pre_glm)

## given less resp songs, very sig, negative
card_flyby_less_resp_glm = 
  glm(Flyby.Resp ~ 
        Songs.Less.24.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_less_resp_glm)
summary(card_flyby_less_resp_glm)

## given less pre songs, sig, negative
## but pre and resp less songs are correlated
card_flyby_less_pre_glm = 
  glm(Flyby.Resp ~ 
        Songs.Less.24.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_flyby_less_pre_glm)
summary(card_flyby_less_pre_glm)



## plus songs resp
## given plus songs pre, very sig, positive
card_plus_pre_resp_glm = 
  glm(Songs.24.Plus.Resp ~ 
      Songs.24.Plus.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_pre_resp_glm)
summary(card_plus_pre_resp_glm)

## given date, not sig
card_plus_resp_date_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Date, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_date_glm)
summary(card_plus_resp_date_glm)

## given time, mod sig, positive
card_plus_resp_time_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Time, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_time_glm)
summary(card_plus_resp_time_glm)

## given type
## with refactor, cactus wren almost sig pos, without texas almost sig pos
card_plus_resp_type_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Type, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_type_glm)
summary(card_plus_resp_type_glm)

## given song
## no refactor, cactus almost sig pos, after refact TX10 almost sig pos
card_plus_resp_song_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Song, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_song_glm)
summary(card_plus_resp_song_glm)

## given point, mod sig neg
card_plus_resp_point_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Point, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_point_glm)
summary(card_plus_resp_point_glm)

## given order, mod sig pos
card_plus_resp_order_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Order, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_resp_order_glm)
summary(card_plus_resp_order_glm)

## given closest dist pre, sig neg
card_plus_close_pre_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Close.Pre.As.Number, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_close_pre_glm)
summary(card_plus_close_pre_glm)

## given flyby pre, sig pos
card_plus_flyby_pre_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Flyby.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_flyby_pre_glm)
summary(card_plus_flyby_pre_glm)



## given chips resp, not sig
Chips.Resp.1 = card_data$Chips.Pres.Abs.Resp
card_plus_chips_resp_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Chips.Resp.1, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_chips_resp_glm)
summary(card_plus_chips_resp_glm)

## given chips pre, not sig
Chips.Pre.1 = card_data$Chips.Pre
card_plus_chips_pre_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Chips.Pre.1, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_chips_pre_glm)
summary(card_plus_chips_pre_glm)

## given less resp, very sig, pos
card_plus_less_resp_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Songs.Less.24.Resp, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_less_resp_glm)
summary(card_plus_less_resp_glm)

## given less pre, sig pos
card_plus_less_pre_glm = 
  glm(Songs.24.Plus.Resp ~ 
        Songs.Less.24.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_plus_less_pre_glm)
summary(card_plus_less_pre_glm)



## less songs resp
## given less pre songs, mod sig, positive
card_less_pre_resp_glm = 
  glm(Songs.Less.24.Resp ~ 
        Songs.Less.24.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_less_pre_resp_glm)
summary(card_less_pre_resp_glm)

## given plus pre, mod sig pos
card_less_plus_pre_glm = 
  glm(Songs.Less.24.Resp ~ 
        Songs.24.Plus.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_less_plus_pre_glm)
summary(card_less_plus_pre_glm)

## given date, not sig
card_less_resp_date_glm = 
  glm(Songs.Less.24.Resp ~ 
        Date, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_date_glm)
summary(card_less_resp_date_glm)

## given time, not sig
card_less_resp_time_glm = 
  glm(Songs.Less.24.Resp ~ 
      Time, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_time_glm)
summary(card_less_resp_time_glm)

## given type, BW highly sig pos
card_less_resp_type_glm = 
  glm(Songs.Less.24.Resp ~ 
        Type, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_type_glm)
summary(card_less_resp_type_glm)

## given song, BW1 high sig pos, BW2/BW4/TX11 mod sig pos, PT24 almost sig pos
card_less_resp_song_glm = 
  glm(Songs.Less.24.Resp ~ 
        Song, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_song_glm)
summary(card_less_resp_song_glm)

## given point, almost sig neg
card_less_resp_point_glm = 
  glm(Songs.Less.24.Resp ~ 
        Point, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_point_glm)
summary(card_less_resp_point_glm)

## given order, not sig
card_less_resp_order_glm = 
  glm(Songs.Less.24.Resp ~ 
        Order, data=card_data)
par(mfrow=c(2,2))
plot(card_less_resp_order_glm)
summary(card_less_resp_order_glm)

## given closest dist pre, not sig
card_less_close_pre_glm = 
  glm(Songs.Less.24.Resp ~ 
        Close.Pre.As.Number, data=card_data)
par(mfrow=c(2,2))
plot(card_less_close_pre_glm)
summary(card_less_close_pre_glm)

## given flyby pre, sig pos
card_less_flyby_pre_glm = 
  glm(Songs.Less.24.Resp ~ 
        Flyby.Pre, data=card_data)
par(mfrow=c(2,2))
plot(card_less_flyby_pre_glm)
summary(card_less_flyby_pre_glm)

## given chips resp, very sig, pos
card_less_chips_resp_glm = 
  glm(Songs.Less.24.Resp ~ 
        Chips.Resp.1, data=card_data)
par(mfrow=c(2,2))
plot(card_less_chips_resp_glm)
summary(card_less_chips_resp_glm)

## given chips pre, not sig
card_less_chips_pre_glm = 
  glm(Songs.Less.24.Resp ~ 
        Chips.Pre.1, data=card_data)
par(mfrow=c(2,2))
plot(card_less_chips_pre_glm)
summary(card_less_chips_pre_glm)

## chips resp
## given chips pre, very sig pos
card_chips_resp_pre_glm = 
  glm(Chips.Resp.1 ~ 
        Chips.Pre.1, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_pre_glm)
summary(card_chips_resp_pre_glm)

## given date, sig neg
card_chips_resp_date_glm = 
  glm(Chips.Resp.1 ~ 
        Date, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_date_glm)
summary(card_chips_resp_date_glm)

## given time, not sig
card_chips_resp_time_glm = 
  glm(Chips.Resp.1 ~ 
        Time, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_time_glm)
summary(card_chips_resp_time_glm)

## given type, BW sig pos
card_chips_resp_type_glm = 
  glm(Chips.Resp.1 ~ 
        Type, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_type_glm)
summary(card_chips_resp_type_glm)

## given song, very sig BW1 pos, mod sig BW2/BW4 pos, sig TX11,TX12 pos
## almost sig TX1/TX5/PT23 pos
card_chips_resp_song_glm = 
  glm(Chips.Resp.1 ~ 
       Song, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_song_glm)
summary(card_chips_resp_song_glm)

## given point, not sig
card_chips_resp_point_glm = 
  glm(Chips.Resp.1 ~ 
        Point, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_point_glm)
summary(card_chips_resp_point_glm)

## given order, not sig
card_chips_resp_order_glm = 
  glm(Chips.Resp.1 ~ 
        Order, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_resp_order_glm)
summary(card_chips_resp_order_glm)

## given closest dist pre, almost sig neg
card_chips_close_pre_glm = 
  glm(Chips.Resp.1 ~ 
       Close.Pre.As.Number, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_close_pre_glm)
summary(card_chips_close_pre_glm)

## given flyby pre, sig pos
card_chips_flyby_pre_glm = 
  glm(Chips.Resp.1 ~ 
       Flyby.Pre, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_flyby_pre_glm)
summary(card_chips_flyby_pre_glm)

## given plus pre, not sig
card_chips_plus_pre_glm = 
  glm(Chips.Resp.1 ~ 
        Songs.24.Plus.Pre, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_plus_pre_glm)
summary(card_chips_plus_pre_glm)

## given less pre, sig pos
card_chips_less_pre_glm = 
  glm(Chips.Resp.1 ~ 
        Songs.Less.24.Pre, data=card_data,
      family=binomial)
par(mfrow=c(2,2))
plot(card_chips_less_pre_glm)
summary(card_chips_less_pre_glm)

## doing the "pre" things for date, order, time, song, point etc
card_prechips_date_glm = 
  glm(Chips.Pre.1 ~ 
        Date, data=card_data,
      family=binomial)
summary(card_prechips_date_glm)
## prechips date, very sig, neg

card_prechips_order_glm = 
  glm(Chips.Pre.1 ~ 
        Order, data=card_data,
      family=binomial)
summary(card_prechips_order_glm)
## prechips order, not sig

card_prechips_point_glm = 
  glm(Chips.Pre.1 ~ 
        Point, data=card_data,
      family=binomial)
summary(card_prechips_point_glm)
## prechips point, not sig

card_prechips_time_glm = 
  glm(Chips.Pre.1 ~ 
        Time, data=card_data,
      family=binomial)
summary(card_prechips_time_glm)
## prechips time, not sig

card_prechips_type_glm = 
  glm(Chips.Pre.1 ~ 
        Type, data=card_data,
      family=binomial)
summary(card_prechips_type_glm)
## prechips type, not sig

card_prechips_song_glm = 
  glm(Chips.Pre.1 ~ 
        Song, data=card_data,
      family=binomial)
summary(card_prechips_song_glm)
## prechips song, not sig

card_lesspre_song_glm = 
  glm(Songs.Less.24.Pre ~ 
        Song, data=card_data)
summary(card_lesspre_song_glm)
## less pre songs not sig

card_lesspre_point_glm = 
  glm(Songs.Less.24.Pre ~ 
        Point, data=card_data)
summary(card_lesspre_point_glm)
## less pre point not sig

card_lesspre_order_glm = 
  glm(Songs.Less.24.Pre ~ 
        Order, data=card_data)
summary(card_lesspre_order_glm)
## less pre order not sig

card_lesspre_type_glm = 
  glm(Songs.Less.24.Pre ~ 
        Type, data=card_data)
summary(card_lesspre_type_glm)
## less pre type not sig

card_lesspre_time_glm = 
  glm(Songs.Less.24.Pre ~ 
        Time, data=card_data)
summary(card_lesspre_time_glm)
## less pre time not sig

card_lesspre_date_glm = 
  glm(Songs.Less.24.Pre ~ 
        Date, data=card_data)
summary(card_lesspre_date_glm)
## less pre date very sig neg

card_pluspre_song_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Song, data=card_data)
summary(card_pluspre_song_glm)
## pluspre song texas 4 sig neg, portal 21 almost sig, rest not sig

card_pluspre_point_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Point, data=card_data)
summary(card_pluspre_point_glm)
## pluspre point mod sig neg

card_pluspre_order_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Order, data=card_data)
summary(card_pluspre_order_glm)
## pluspre order not sig

card_pluspre_type_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Type, data=card_data)
summary(card_pluspre_type_glm)
## pluspre type not sig

card_pluspre_time_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Time, data=card_data)
summary(card_pluspre_time_glm)
## pluspre time sig pos

card_pluspre_date_glm = 
  glm(Songs.24.Plus.Pre ~ 
        Date, data=card_data)
summary(card_pluspre_date_glm)
## pluspre date not sig

card_flybypre_song_glm = 
  glm(Flyby.Pre ~ 
        Song, data=card_data)
summary(card_flybypre_song_glm)
## flybypre song not sig

card_flybypre_point_glm = 
  glm(Flyby.Pre ~ 
        Point, data=card_data)
summary(card_flybypre_point_glm)
## flybypre point not sig

card_flybypre_order_glm = 
  glm(Flyby.Pre ~ 
        Order, data=card_data)
summary(card_flybypre_order_glm)
## flybypre order not sig

card_flybypre_type_glm = 
  glm(Flyby.Pre ~ 
        Type, data=card_data)
summary(card_flybypre_type_glm)
## flybypre type texas sig pos rest not

card_flybypre_time_glm = 
  glm(Flyby.Pre ~ 
        Time, data=card_data)
summary(card_flybypre_time_glm)
## flybypre time not sig

card_flybypre_date_glm = 
  glm(Flyby.Pre ~ 
        Date, data=card_data)
summary(card_flybypre_date_glm)
## flybypre date not sig

## Close.Pre.As.Number, Point, Order, Date, Song, Type, Time
card_closedistpre_song_glm = 
  glm(Close.Pre.As.Number ~ 
        Song, data=card_data)
summary(card_closedistpre_song_glm)
## closedistpre song not sig

card_closedistpre_point_glm = 
  glm(Close.Pre.As.Number ~ 
        Point, data=card_data)
summary(card_closedistpre_point_glm)
## closedistpre point not sig

card_closedistpre_order_glm = 
  glm(Close.Pre.As.Number ~ 
        Order, data=card_data)
summary(card_closedistpre_order_glm)
## closedistpre order not sig

card_closedistpre_type_glm = 
  glm(Close.Pre.As.Number ~ 
        Type, data=card_data)
summary(card_closedistpre_type_glm)
## closedistpre type texas sig neg

card_closedistpre_time_glm = 
  glm(Close.Pre.As.Number ~ 
        Time, data=card_data)
summary(card_closedistpre_time_glm)
## closedistpre time not sig

card_closedistpre_date_glm = 
  glm(Close.Pre.As.Number ~ 
        Date, data=card_data)
summary(card_closedistpre_date_glm)
## closedistpre date not sig

## and finally doing the pres with each other

card_pre_chips_dist_glm = 
  glm(Chips.Pre.1 ~ 
        Close.Pre.As.Number, data=card_data,
      family=binomial)
summary(card_pre_chips_dist_glm)
## chips pre and close pre, very sig neg

card_pre_chips_fb_glm = 
  glm(Chips.Pre.1 ~ 
        Flyby.Pre, data=card_data,
      family=binomial)
summary(card_pre_chips_fb_glm)
## chips pre and fb pre, very sig pos

card_pre_chips_less_glm = 
  glm(Chips.Pre.1 ~ 
        Songs.Less.24.Pre, data=card_data,
      family=binomial)
summary(card_pre_chips_less_glm)
## chips pre and less pre, mod sig pos

card_pre_chips_plus_glm = 
  glm(Chips.Pre.1 ~ 
        Songs.24.Plus.Pre, data=card_data,
      family=binomial)
summary(card_pre_chips_plus_glm)
## chips pre and plus pre, not sig

card_pre_dist_fb_glm = 
  glm(Close.Pre.As.Number ~ 
        Flyby.Pre, data=card_data)
summary(card_pre_dist_fb_glm)
## dist pre and fb pre, very sig neg

card_pre_dist_less_glm = 
  glm(Close.Pre.As.Number ~ 
        Songs.Less.24.Pre, data=card_data)
summary(card_pre_dist_less_glm)
## dist pre and less pre, very sig neg

card_pre_dist_plus_glm = 
  glm(Close.Pre.As.Number ~ 
        Songs.24.Plus.Pre, data=card_data)
summary(card_pre_dist_plus_glm)
## dist pre and plus pre, not sig

card_pre_fb_less_glm = 
  glm(Flyby.Pre ~ 
        Songs.Less.24.Pre, data=card_data)
summary(card_pre_fb_less_glm)
## pre fb and less, not sig

card_pre_fb_plus_glm = 
  glm(Flyby.Pre ~ 
        Songs.24.Plus.Pre, data=card_data)
summary(card_pre_fb_plus_glm)
## pre fb and plus, not sig

card_pre_less_plus_glm = 
  glm(Songs.Less.24.Pre ~ 
        Songs.24.Plus.Pre, data=card_data)
summary(card_pre_less_plus_glm)
## pre less and plus, mod sig pos

## and the non-biological variables
card_order_date_glm = glm(Order ~ Date, data=card_data)
summary(card_order_date_glm) ## sig pos

card_order_point_glm = glm(Order ~ Point, data=card_data)
summary(card_order_point_glm) ## not sig

card_order_type_glm = glm(Order ~ Type, data=card_data)
summary(card_order_type_glm) ## not sig

card_order_time_glm = glm(Order ~ Time, data=card_data)
summary(card_order_time_glm) ## not sig

card_order_song_glm = glm(Order ~ Song, data=card_data)
summary(card_order_song_glm) ## not sig

card_date_point_glm = glm(Date ~ Point, data=card_data)
summary(card_date_point_glm) ## very sig pos

card_date_type_glm = glm(Date ~ Type, data=card_data)
summary(card_date_type_glm) ## not sig

card_date_time_glm = glm(Date ~ Time, data=card_data)
summary(card_date_time_glm) ## mod sig neg

card_date_song_glm = glm(Date ~ Song, data=card_data)
summary(card_date_song_glm) ## tx2 almost sig neg, bw1 sig neg, bw5 almost sig neg, bw6 mod sig neg

card_point_type_glm = glm(Point ~ Type, data=card_data)
summary(card_point_type_glm) ## not sig

card_point_time_glm = glm(Point ~ Time, data=card_data)
summary(card_point_time_glm) ## very sig neg

card_point_song_glm = glm(Point ~ Song, data=card_data)
summary(card_point_song_glm) ## bw1 almost sig neg
 
card_time_type_glm = glm(Time ~ Type, data=card_data)
summary(card_time_type_glm) ## not sig

card_time_song_glm = glm(Time ~ Song, data=card_data)
summary(card_time_song_glm) ## bw1 sig pos





## getting the AICs of models
