# Calculations for deriving SD from plot measurements
# SD = SE * sqrt(n)
# SD = sqrt(var)

####houston-price2010 (Figure 1; UK)
# SD_dif = sqrt((var/n) + var/n)) # convert to SEs
# SD_dif = sqrt(se1^2 + se2^2)

# monolinguals
y1 = 
y2 = 
y1se = 
y2se = 


# bilinguals
y1 = 
y2 =
y1se = 
y2se = 

####graham2010
y=4.61
n=22

#no gaze
yse = .25
sd = sqrt(n) *(yse/y)
#*or*
(sqrt(n) *((yse/y)*6))/6 # all 6 trials

#consistent 
yse= .15
sqrt(n) *(yse/y)

#inconsistent
yse = .23
sqrt(n) *(yse/y)

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

####horst2008
y=2.16
n=16

#ostensive
yse= .085
(yse/y) *sqrt(n)

#follow in
yse=.11
(yse/y) *sqrt(n)
