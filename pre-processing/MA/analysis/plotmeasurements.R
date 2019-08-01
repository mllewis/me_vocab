# Calculations for deriving SD from plot measurements
# SD = SE * sqrt(n)
# SD = sqrt(var)
# avg_month <- 365.2425 / 12.0

###mvervis 1995
n1 = 9
n2 = 13
m1 = 3.22/4
m2 = .62/4
sd1 = .83/4
s23 = .65/4

M = ((m1 * n1) + (m2 * n2))/(n1 + n2)
SD = ((sd1 * n1) + (sd2 * n2))/(n1 + n2)

M
SD

p1 = 173
c1 = 286
p2 = 31
c2 = 175

p_vocab = ((p1 * n1) + (p2 * n2))/(n1 + n2)
c_vocab = ((c1 * n1) + (c2 * n2))/(n1 + n2)

p_vocab
c_vocab



### kalashnikova1996a
# data from page 87 (for noth exps)
n = 46
w = (.968-.874)/2
w = 1.96*s/sqrt(n)
s = (sqrt(n) * w)/1.96 
s

####kalashnikova2016
# SD_dif = sqrt((var/n) + var/n)) # convert to SEs
# SD_dif = sqrt(se1^2 + se2^2)

n = 27
sd1 = .27
sd2 = .08
# SD_dif = sqrt((sd1^2/n) + sd2^2/n)) 

### Namy 2001
t = 2.92
n = 16
x1 = .61
x2 = .5

#t <- (x1 - x2) / (s/sqrt(n))
s <- ((x1 -x2) * sqrt(n)) / t
s

### Davidson 1997
# calculate sd based on greek 3-4yo condition
t = 1.46
n = 16
x1 = .6
x2 = .5

#t <- (x1 - x2) / (s/sqrt(n))
s <- (x1 -x2) * sqrt(n)/ t
s 


### Preisler 2005
# 32/39
# there are 4 possible ways 7 trials could have been incorrect
v1 = c(c(1,1,1,1,1,1,1), rep(2, 12), 1) # 1 because one trial missing
v2 = c(c(1,1,1,1,1,0), rep(2, 13), 1)
v3 = c(c(1,1,1,0,0), rep(2, 14), 1)
v4 = c(c(1,0,0,0), rep(2, 15), 1)

estimated_sd <- mean(c(sd(v1), sd(v2), sd(v3), sd(v4)))/2
mean = 32/39
mean
estimated_sd

### Romski 1996
# table 2, column1
d <- c(0, .25, 0, .25, 0, .5, .5, .75, 1, 1, 1, 1) #
sd(d)

### Wall et al 2015
# table 1
m3s = c(rep(0,8), rep(.5, 11), rep(1, 7))
mean(m3s)
sd(m3s)

m4s = c(rep(0,9), rep(.5, 10), rep(1,11))
mean(m4s)
sd(m4s)

####houston-price2010 (Figure 1; UK)
# SD_dif = sqrt((var/n) + var/n)) # convert to SEs
# SD_dif = sqrt(se1^2 + se2^2)

# monolinguals
h = 2.93
H = 35
y1 = .6
y2 = 1.33
y1se = .13
y2se = .19

y1t = (y1 * H)/h
y2t = (y2 * H)/h

dift = y2t - y1t 

y1set = (y1se * H)/h
y2set = (y2se * H)/h

sd_dift = sqrt(y1set^2 + y1set^2)

dift/100
sd_dift/100


# bilinguals
y1 = .58
y2 = .49
y1se = .15
y2se = .23

y1t = (y1 * H)/h
y2t = (y2 * H)/h

dift = y2t - y1t 

y1set = (y1se * H)/h
y2set = (y2se * H)/h

sd_dift = sqrt(y1set^2 + y1set^2)

dift/100
sd_dift/100

### horst2010
n = 12
h = 3.17
H = 1

Y2 = 2.91
Y2se = .16

Y3 = 2.3
Y3se = .19
Y4 = 2.77
Y4se = .19

y2t = Y2/h
y2est = (Y2se/h) * sqrt(n)
y2t
y2est

y3t = Y3/h
y3est = (Y3se/h) * sqrt(n)
y3t
y3est

y4t = Y4/h
y4est = (Y4se/h) * sqrt(n)
y4t
y4est



####graham2010 (v2)
y=3.05
n=22

#no cues
yse = .16
sd = sqrt(n) *(yse/y)
#*or*
round((sqrt(n) *((yse/y)*6))/6,2) # all 6 trials
round(1.85/y,2)

#consistent 
yse= .12
round(sqrt(n) *(yse/y),2)
round(2.7/y,2)

####grassmann2010
#2s
y=2.29
n=12

#baseline 2
yse=.11
(yse/y)*sqrt(n)

#non-ostensive 2
yse=.25
(yse/y)*sqrt(n)

#ostensive 2
yse=.06
(yse/y)*sqrt(n)

#4s
y=3.76
n=12

#baseline 4
yse=.2
(yse/y)*sqrt(n)

#non-ostensive 4
yse=.44
(yse/y)*sqrt(n)

#ostensive 4
yse=.1
(yse/y)*sqrt(n)

####mather2009
yten= .74

#young/no repeats
n = 35
yse=.23
se = yse/yten
se*sqrt(n)

#old/no repeats
n=32
yse=.26
se = yse/yten
se*sqrt(n)

#young/ repeats
n = 35
yse=.26
se = yse/yten
se*sqrt(n)

#old/ repeats
n=32
yse=.23
se = yse/yten
se*sqrt(n)

####mather2009V2
y_ten = .61
y_one = y_ten/10

#young/no repeats
n = 35
yse = .19

se = yse/y_one 
(se*sqrt(n))/100

.03*sqrt(n)

y_m = .04
(100 - (50 + .04/y_one))/100

#old/no repeats
n=32
yse=.26
se = yse/yten
se*sqrt(n)

#young/ repeats
n = 35
yse=.26
se = yse/yten
se*sqrt(n)

#old/ repeats
n=32
yse=.23
se = yse/yten
se*sqrt(n)


####horst2008
y=2.16
n=16

#ostensive
yse= .085
(yse/y) *sqrt(n)

#follow in
yse=.11
(yse/y) *sqrt(n)
