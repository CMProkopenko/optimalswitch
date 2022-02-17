gc()
rm(list = ls())

####Prey Switehing Model####
###originally created MAY 2018, version from Jan 2022
###Authors C. Prokopenko, T Avgar

################################################
#####Terminology notes:
#####through revision some parameter names have changes
###prey x in  code is prey i  (vulnerable) in Manuscript, prey y in code is prey j  (costly) in Mnauscript
###chase (c) in code is attack (a) in MS, handling (h) in code is consume (b) in MS

###################################################

###SCENARIOS


#   null  
#   search rate  
#   chase time, attack time
#   chase energy, attack energy
#   probability of kill
#   handle time, consume time
#   handle energy, consume energy
#   Energy content

################################################
# g_func is used to create gain rate functions for all scenarios
#
# Paramaters:
#   pk    :  probability of kill
#   tc    : chase time, attack time
#   ec    : chase energy, attack energy
#   th    : handling time, consume time
#   eh    : handling energy, consume energy
#   E     : energy content in prey

#### energetic gain RATE of prey

g_func <- function(pk,tc,ec,th,eh,E){
  function(){
    (-ec*tc+pk*(E-eh*th))/(tc+pk*th)
  }
}

# GA_func is used to create Alternate Gain functions for all scenarios
#
# Paramaters:
#   pk    : probability of kill
#   es    : search energy
#   s     : search rate
#   tc    : chase time, attack time
#   ec    : chase energy, attack energy
#   th    : handling time, consume time
#   eh    : handling energy, consume energy
#   E     : Energy content of prey
#   N     : prey density

#### alternative gain RATE from rejecting encountered prey and continuing to seareh

ga_func <- function(es,s,pk,tc,ec,th,eh,N,E){
  function(){
    ((-es*((N*s)^-1)-(ec*tc))+(pk*(E-(eh*th))))/(((N*s)^-1)+tc+(pk*th))
  }
}

################################################
# fN_func is used to create Functional Response functions for all scenarios
#
# Paramaters:
### Probabilities
#   pkv   : pkx or pky
#   pkx   : probability of killing prey x
#   pky   : probability of killing prey y
#   pev   : always 1 or deterministic, probabilistic - pex or pey
#   pex   : 1, deterministic, probabilistic
#   pey   : 1, deterministic, probabilistic
### Time
#   sv: sx or sy
#   tcx
#   tcy
#   tcx
#   tcy
### Energy
#   esx
#   esy
#   ecx
#   ecy
#   ehx
#   ehy
### Density
#   Nv: Nx or Ny
#   Nx: density of prey x 
#   Ny: density of prey y


fN_func <- function(pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny){
  function(){
    (sv*pev*pkv*Nv)/(1+((Nx*sx*pex*((pkx*thx)+tcx))+(Ny*sy*pey*(pky*thy+tcy))))
  }
}

###fNG_func is the energetic functional response

fNG_func = function(pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey){
  function(){
    ((Nx*sx*pex*(-ecx*tcx+pkx*(Ex-ehx*thx)))+(Ny*sy*pey*(-ecy*tcy+pky*(Ey-ehy*thy)))-es)/(1+(Nx*sx*pex*(tcx+pkx*thx))+(Ny*sy*pey*(tcy+pky*thy)))
  }
}

####Parameter values for all scenarios

#DENSITY PREY Y
#Ny = .1
#Ny = 10
Ny = rep(seq(0.1,10,0.1),200) #rep 1 for single prey appendix and switching figure 3a, 200 for multiprey
#Ny = Ny*2

##DENSITY PREY X
#Nx =  rep(seq(0,10,.1),1)
#Nx = 0 ##single prey results
#Nx = 10
#Nx = .1
Nx = sort(Ny)
#Nx = 10-Ny ###figure 3a, switching plot
#Nx = Ny  ##figure 3 b, switching plot

###COSTS FOR X (prey i in ms)
es <- 1
#esx <-
ecx <-1 #.5 #1
ehx <- 1

sx <- 1
tcx <- 1 #.5 #1
thx <- 1

Ex <- 1000  ### change to 500, 1000, 2000

pkx <- .5
                                
####DISSIMILARITY

###values for in text dissimilarity values
ds  <- .5/sx
dec <- 125.625/ecx     #125.625/ecx
deh <- 250.25/ehx     #250.25/ehx
dtc <- 1.7455/tcx   #1.7455/tcx
dth <- 2.49103/thx   #2.49103/thx
dE <-  750.75/Ex      #750.75/Ex
dpk <- 0.35036/pkx   #0.35036/pkx

# d <- sample(rep(seq(1,1000,10),2))
# 
# ds <- 1/d
# dec <- d
# deh <- d
# dtc <- d
# dth <- d
# dE <- d
# dpk <- 1/d


#different parameter values depending on the scenario

################################################
### null

##COSTS FOR Y (prey j in ms)
#esy <-
ecy <- ecx
ehy <- ehx

sy  <- sx
tcy <- tcx
thy <- thx

Ey <- Ex

pkx_null <- pkx 
pky_null <- pkx

#### gain rate
#pk,tc,ec,th,eh,E
gxf_null  <- g_func(pkx_null,tcx,ecx,thx,ehx,Ex)
gyf_null  <- g_func(pky_null,tcy,ecy,thy,ehy,Ey)


#es,s,pk,tc,ec,th,eh,N,E
gaxf_null <- ga_func(es,sx,pkx_null,tcx,ecx,thx,ehx,Nx,Ex)
gayf_null <- ga_func(es,sy,pky_null,tcy,ecy,thy,ehy,Ny,Ey)

gx_null  <- gxf_null()
gy_null  <- gyf_null()
gax_null <- gaxf_null()
gay_null <- gayf_null()

#probability of engaging
#naive
#pex_null = 1
#pey_null = 1

#probabilistic
if (all(Nx==0)) {
  pex_null = 0
  pey_null = 1
} else {
  pex_null = ifelse(gay_null<=gx_null,1,(gx_null/gay_null))
  pey_null = ifelse(gax_null<=gy_null,1,(gy_null/gax_null))
}

#deterministic
#if (Nx==0) {
#  pex_null = 0
# pey_null = 1
#} else {
#  pex_null =ifelse(gay_null<=gx_null,1,0)
#  pey_null =ifelse(gax_null<=gy_null,1,0)
#}


#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_null <- fN_func(pex_null,pex_null,pey_null,pkx_null,pkx_null,pky_null,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_null <- fN_func(pey_null,pex_null,pey_null,pky_null,pkx_null,pky_null,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_null <- fNG_func(pex_null,pey_null,pkx_null,pky_null,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)
  
#### variables

fx_null <- fNxf_null()
fy_null  <- fNyf_null()
fe_null <- fNGf_null()


### search rate
################################################

###COSTS FOR Y
###search rate dissimilar
sy <- sx*ds
#esy <- esx
ecy <- ecx
tcy <- tcx
ehy <- ehx
thy <- thx

Ey <- Ex

pkx_s <- pkx
pky_s <- pkx 

#### gain rate
#pk,tc,ec,th,eh,E
gxf_s  <- g_func(pkx_s,tcx,ecx,thx,ehx,Ex)
gyf_s  <- g_func(pky_s,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_s <- ga_func(es,sx,pkx_s,tcx,ecx,thx,ehx,Nx,Ex)
gayf_s <- ga_func(es,sy,pky_s,tcy,ecy,thy,ehy,Ny,Ey)

gx_s  <- gxf_s()
gy_s  <- gyf_s()
gax_s <- gaxf_s()
gay_s <- gayf_s()

#probability of engaging
#naive
#pex_s = 1
#pey_s = 1

#probabilistic
if (all(Nx==0)) {
 pex_s = 0
  pey_s = 1
} else {
  pex_s =ifelse(gay_s<=gx_s,1,(gx_s/gay_s))
  pey_s =ifelse(gax_s<=gy_s,1,(gy_s/gax_s))
}

#deterministic
#if (Nx==0) {
#  pex_s = 0
#  pey_s = 1
#} else {
#  pex_s =ifelse(gay_s<=gx_s,1,0)
#  pey_s =ifelse(gax_s<=gy_s,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_s <- fN_func(pex_s,pex_s,pey_s,pkx_s,pkx_s,pky_s,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_s <- fN_func(pey_s,pex_s,pey_s,pky_s,pkx_s,pky_s,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_s <- fNG_func(pex_s,pey_s,pkx_s,pky_s,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

#### variables

fx_s <- fNxf_s()
fy_s  <- fNyf_s()
fe_s <- fNGf_s()

### search energy
################################################
# 
# ###COSTS FOR Y
# ###search rate dissimilar
# sy <- sx
# #esy <- esx
# ecy <- ecx
# tcy <- tcx
# ehy <- ehx
# thy <- thx
# 
# Ey <- Ex
# 
# pkx_es <- pkx
# pky_es <- pkx 
# 
# #### gain rate
# #pk,tc,ec,th,eh,E
# gxf_es  <- g_func(pkx_es,tcx,ecx,thx,ehx,Ex)
# gyf_es  <- g_func(pky_es,tcy,ecy,thy,ehy,Ey)
# 
# #es,s,pk,tc,ec,th,eh,N,E
# gaxf_es <- ga_func(es,sx,pkx_es,tcx,ecx,thx,ehx,Nx,Ex)
# gayf_es <- ga_func(es,sy,pky_es,tcy,ecy,thy,ehy,Ny,Ey)
# 
# gx_es  <- gxf_es()
# gy_es  <- gyf_es()
# gax_es <- gaxf_es()
# gay_es <- gayf_es()
# 
# #probability of engaging
# #naive
# #pex_es = 1
# #pey_es = 1
# 
# #probabilistic
# if (all(Nx==0)) {
#   pex_es = 0
#   pey_es = 1
# } else {
#   pex_es =ifelse(gay_es<=gx_es,1,(gx_es/gay_es))
#   pey_es =ifelse(gax_es<=gy_es,1,(gy_es/gax_es))
# }
# 
# #deterministic
# #if (Nx==0) {
# #  pex_es = 0
# #  pey_es = 1
# #} else {
# #  pex_es =ifelse(gay_es<=gx_es,1,0)
# #  pey_es =ifelse(gax_es<=gy_es,1,0)
# #}
# 
# #### functional response
# # pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
# fNxf_es <- fN_func(pex_es,pex_es,pey_es,pkx_es,pkx_es,pky_es,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
# fNyf_es <- fN_func(pey_es,pex_es,pey_es,pky_es,pkx_es,pky_es,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# # pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
# fNGf_es <- fNG_func(pex_es,pey_es,pkx_es,pky_es,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)
# 
# #### variables
# fx_es <- fNxf_es()
# fy_es  <- fNyf_es()
# fe_es <- fNGf_es()



###chase energy
################################################

###COSTS FOR Y
sy <- sx
#esy <- esx
### chase energy dissimilar
ecy <- ecx*dec
tcy <- tcx
ehy <- ehx
thy <- thx

Ey <- Ex

pkx_ec <- pkx
pky_ec <- pkx

#### gain rate
#pk,tc,ec,th,eh,E
gxf_ec  <- g_func(pkx_ec,tcx,ecx,thx,ehx,Ex)
gyf_ec  <- g_func(pky_ec,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_ec <- ga_func(es,sx,pkx_ec,tcx,ecx,thx,ehx,Nx,Ex)
gayf_ec <- ga_func(es,sy,pky_ec,tcy,ecy,thy,ehy,Ny,Ey)

gx_ec  <- gxf_ec()
gy_ec  <- gyf_ec()
gax_ec <- gaxf_ec()
gay_ec <- gayf_ec()

#probability of engaging
#naive
#pex_ec = 1
#pey_ec = 1

#probabilistic
if (all(Nx==0)) {
  pex_ec = 0
  pey_ec = 1
} else {
  pex_ec =ifelse(gay_ec<=gx_ec,1,(gx_ec/gay_ec))
  pey_ec =ifelse(gax_ec<=gy_ec,1,(gy_ec/gax_ec))
}

#deterministic
#if (Nx==0) {
#  pex_ec = 0
#  pey_ec = 1
#} else {
#  pex_ec =ifelse(gay_ec<=gx_ec,1,0)
#  pey_ec =ifelse(gax_ec<=gy_ec,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_ec <- fN_func(pex_ec,pex_ec,pey_ec,pkx_ec,pkx_ec,pky_ec,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_ec <- fN_func(pey_ec,pex_ec,pey_ec,pky_ec,pkx_ec,pky_ec,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_ec <- fNG_func(pex_ec,pey_ec,pkx_ec,pky_ec,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

#### variables
fx_ec <- fNxf_ec()
fy_ec  <- fNyf_ec()
fe_ec <- fNGf_ec()

###chase time
################################################

###COSTS FOR Y
sy <- sx
#esy <-
ecy <- ecx
###chase time dissimilar
tcy <- tcx*dtc
ehy <- ehx
thy <- thx

Ey <- Ex

pkx_tc <- pkx 
pky_tc <- pkx 

#### gain rate
#pk,tc,ec,th,eh,E
gxf_tc  <- g_func(pkx_tc,tcx,ecx,thx,ehx,Ex)
gyf_tc  <- g_func(pky_tc,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_tc <- ga_func(es,sx,pkx_tc,tcx,ecx,thx,ehx,Nx,Ex)
gayf_tc <- ga_func(es,sy,pky_tc,tcy,ecy,thy,ehy,Ny,Ey)

gx_tc  <- gxf_tc()
gy_tc  <- gyf_tc()
gax_tc <- gaxf_tc()
gay_tc <- gayf_tc()

#probability of engaging
#naive
#pex_tc = 1
#pey_tc = 1

#probabilistic
if (all(Nx==0))  {
  pex_tc  = 0
  pey_tc  = 1
} else {
  pex_tc  =ifelse(gay_tc<=gx_tc,1,(gx_tc/gay_tc))
  pey_tc  =ifelse(gax_tc<=gy_tc,1,(gy_tc/gax_tc))
}

#deterministic
#if (Nx==0) {
# pex_tc = 0
#  pey_tc = 1
#} else {
#  pex_tc =ifelse(gay_tc<=gx_tc,1,0)
#  pey_tc =ifelse(gax_tc<=gy_tc,1,0)
#}


#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_tc <- fN_func(pex_tc,pex_tc,pey_tc,pkx_tc,pkx_tc,pky_tc,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_tc <- fN_func(pey_tc,pex_tc,pey_tc,pky_tc,pkx_tc,pky_tc,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_tc <- fNG_func(pex_tc,pey_tc,pkx_tc,pky_tc,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

#### variables
fx_tc <- fNxf_tc()
fy_tc <- fNyf_tc()
fe_tc <- fNGf_tc()

###probability of kill
################################################

###COSTS FOR Y
sy <- sx
#esy <-
ecy <- ecx
tcy <- tcx
ehy <- ehx
thy <- thx

Ey <- Ex

pkx_pk <- pkx
###probability of kill is dissimilar
pky_pk <- pkx*dpk

#### gain rate
#pk,tc,ec,th,eh,E
gxf_pk  <- g_func(pkx_pk,tcx,ecx,thx,ehx,Ex)
gyf_pk  <- g_func(pky_pk,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_pk <- ga_func(es,sx,pkx_pk,tcx,ecx,thx,ehx,Nx,Ex)
gayf_pk <- ga_func(es,sy,pky_pk,tcy,ecy,thy,ehy,Ny,Ey)

gx_pk  <- gxf_pk()
gy_pk  <- gyf_pk()
gax_pk <- gaxf_pk()
gay_pk <- gayf_pk()
#probability of engaging
#naive
#pex_pk = 1
#pey_pk = 1

#probabilistic
if (all(Nx==0))  {
  pex_pk= 0
  pey_pk = 1
} else {
  pex_pk =ifelse(gay_pk<=gx_pk,1,(gx_pk/gay_pk))
  pey_pk =ifelse(gax_pk<=gy_pk,1,(gy_pk/gax_pk))
}

#deterministic
#if (Nx==0) {
#  pex_pk = 0
#  pey_pk = 1
#} else {
#  pex_pk =ifelse(gay_pk<=gx_pk,1,0)
#  pey_pk =ifelse(gax_pk<=gy_pk,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_pk <- fN_func(pex_pk,pex_pk,pey_pk,pkx_pk,pkx_pk,pky_pk,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_pk <- fN_func(pey_pk,pex_pk,pey_pk,pky_pk,pkx_pk,pky_pk,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_pk <- fNG_func(pex_pk,pey_pk,pkx_pk,pky_pk,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

fx_pk <- fNxf_pk()
fy_pk  <- fNyf_pk()
fe_pk <- fNGf_pk()

###handle energy
################################################

###COSTS FOR Y
sy <- sx
#esy <-
ecy <- ecx
tcy <- tcx
# handle energy dissimilar
ehy <- ehx*deh
thy <- thx

Ey <- Ex

pkx_eh <- pkx
pky_eh <- pkx

#### gain rate
#pk,tc,ec,th,eh,E
gxf_eh  <- g_func(pkx_eh,tcx,ecx,thx,ehx,Ex)
gyf_eh  <- g_func(pky_eh,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_eh <- ga_func(es,sx,pkx_eh,tcx,ecx,thx,ehx,Nx,Ex)
gayf_eh <- ga_func(es,sy,pky_eh,tcy,ecy,thy,ehy,Ny,Ey)

gx_eh  <- gxf_eh()
gy_eh  <- gyf_eh()
gax_eh <- gaxf_eh()
gay_eh <- gayf_eh()

#probability of engaging
#naive
#pex_eh = 1
#pey_eh = 1

#probabilistic
if (all(Nx==0))  {
  pex_eh= 0
  pey_eh = 1
} else {
  pex_eh =ifelse(gay_eh<=gx_eh,1,(gx_eh/gay_eh))
  pey_eh =ifelse(gax_eh<=gy_eh,1,(gy_eh/gax_eh))
}

#deterministic
#if (Nx==0) {
#  pex_eh = 0
#  pey_eh = 1
#} else {
#  pex_eh =ifelse(gay_eh<=gx_eh,1,0)
#  pey_eh =ifelse(gax_eh<=gy_eh,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_eh <- fN_func(pex_eh,pex_eh,pey_eh,pkx_eh,pkx_eh,pky_eh,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_eh <- fN_func(pey_eh,pex_eh,pey_eh,pky_eh,pkx_eh,pky_eh,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_eh <- fNG_func(pex_eh,pey_eh,pkx_eh,pky_eh,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

#### variables
fx_eh <- fNxf_eh()
fy_eh  <- fNyf_eh()
fe_eh <- fNGf_eh()

###handle time
################################################

###COSTS FOR Y
sy <- sx
#esy <-
ecy <- ecx
tcy <- tcx
ehy <- ehx
# handle time is dissimilar
thy <- thx*dth

Ey <- Ex

pkx_th <- pkx
pky_th <- pkx

#### gain rate
#pk,tc,ec,th,eh,E
gxf_th  <- g_func(pkx_th,tcx,ecx,thx,ehx,Ex)
gyf_th  <- g_func(pky_th,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_th <- ga_func(es,sx,pkx_th,tcx,ecx,thx,ehx,Nx,Ex)
gayf_th <- ga_func(es,sy,pky_th,tcy,ecy,thy,ehy,Ny,Ey)

gx_th  <- gxf_th()
gy_th  <- gyf_th()
gax_th <- gaxf_th()
gay_th <- gayf_th()
#probability of engaging
#naive
#pex_th = 1
#pey_th = 1

#probabilistic
if (all(Nx==0))  {
  pex_th= 0
  pey_th = 1
} else {
  pex_th =ifelse(gay_th<=gx_th,1,(gx_th/gay_th))
  pey_th =ifelse(gax_th<=gy_th,1,(gy_th/gax_th))
}

#deterministic
#if (Nx==0) {
#  pex_th = 0
#  pey_th = 1
#} else {
#  pex_th =ifelse(gay_th<=gx_th,1,0)
#  pey_th =ifelse(gax_th<=gy_th,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_th <- fN_func(pex_th,pex_th,pey_th,pkx_th,pkx_th,pky_th,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_th <- fN_func(pey_th,pex_th,pey_th,pky_th,pkx_th,pky_th,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_th <- fNG_func(pex_th,pey_th,pkx_th,pky_th,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

fx_th <- fNxf_th()
fy_th  <- fNyf_th()
fe_th <- fNGf_th()

###Energetic content
################################################

###COSTS FOR Y
sy <- sx
#esy <-
ecy <- ecx
tcy <- tcx
ehy <- ehx
thy <- thx

###energy is dissimilar
Ey <- Ex/dE

pkx_E <- pkx
pky_E <- pkx

#### gain rate
#pk,tc,ec,th,eh,E
gxf_E  <- g_func(pkx_E,tcx,ecx,thx,ehx,Ex)
gyf_E  <- g_func(pky_E,tcy,ecy,thy,ehy,Ey)

#es,s,pk,tc,ec,th,eh,N,E
gaxf_E <- ga_func(es,sx,pkx_E,tcx,ecx,thx,ehx,Nx,Ex)
gayf_E <- ga_func(es,sy,pky_E,tcy,ecy,thy,ehy,Ny,Ey)

gx_E  <- gxf_E()
gy_E  <- gyf_E()
gax_E <- gaxf_E()
gay_E <- gayf_E()
#probability of engaging
#naive
#pex_E = 1
#pey_E = 1

#probabilistic
if (all(Nx==0))  {
  pex_E= 0
  pey_E = 1
} else {
  pex_E =ifelse(gay_E<=gx_E,1,(gx_E/gay_E))
  pey_E =ifelse(gax_E<=gy_E,1,(gy_E/gax_E))
}

#deterministic
#if (Nx==0) {
#  pex_E = 0
#  pey_E = 1
#} else {
#  pex_E =ifelse(gay_E<=gx_E,1,0)
#  pey_E =ifelse(gax_E<=gy_E,1,0)
#}

#### functional response
# pev,pex,pey,pkv,pkx,pky,sv,sx,sy,tcx,tcy,thx,thy,Nv,Nx,Ny
fNxf_E <- fN_func(pex_E,pex_E,pey_E,pkx_E,pkx_E,pky_E,sx,sx,sy,tcx,tcy,thx,thy,Nx,Nx,Ny)
fNyf_E <- fN_func(pey_E,pex_E,pey_E,pky_E,pkx_E,pky_E,sy,sx,sy,tcx,tcy,thx,thy,Ny,Nx,Ny)
# pex,pey,pkx,pky,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey
fNGf_E <- fNG_func(pex_E,pey_E,pkx_E,pky_E,es,ecx,ecy,ehx,ehy,sx,sy,tcx,tcy,thx,thy,Nx,Ny,Ex,Ey)

fx_E <- fNxf_E()
fy_E  <- fNyf_E()
fe_E <- fNGf_E()


################################################
#### dataframe
FRDF<- data.frame(Nx = Nx,
                  Ny = Ny,
                  ds = ds,
                  #des = ,
                  dec = dec,
                  deh = deh,
                  dtc = dtc,
                  dth = dth,
                  dE = dE,
                  dpk = dpk,
                  gx_null = gx_null,
                  gax_null = gax_null,
                  gy_null = gy_null,
                  gay_null = gay_null,
                  fx_null = fx_null,
                  fy_null = fy_null,
                  pry_null = fy_null/Ny,
                  #gx_es = gx_es,
                  #gax_es = gax_es,
                  #gy_es = gy_es,
                  #gay_es = gay_es,
                  #fx_es = fx_es,
                  #fy_es = fy_es,
                  #pry_es = fy_es/Ny,
                  gx_ec = gx_ec,
                  gax_ec = gax_ec,
                  gy_ec = gy_ec,
                  gay_ec = gay_ec,
                  fx_ec = fx_ec,
                  fy_ec = fy_ec, 
                  pry_ec = fy_ec/Ny,
                  gx_eh = gx_eh,
                  gax_eh = gax_eh,
                  gy_eh = gy_eh,
                  gay_eh = gay_eh,
                  fx_eh = fx_eh,
                  fy_eh = fy_eh, 
                  pry_eh = fy_eh/Ny,
                  gx_s = gx_s,
                  gax_s = gax_s,
                  gy_s = gy_s,
                  gay_s = gay_s,
                  fx_s = fx_s,
                  fy_s = fy_s,
                  pry_s = fy_s/Ny,
                  gx_tc = gx_tc,
                  gax_tc = gax_tc,
                  gy_tc = gy_tc,
                  gay_tc = gay_tc,
                  fx_tc = fx_tc,
                  fy_tc = fy_tc,
                  pry_tc = fy_tc/Ny,
                  gx_th = gx_th,
                  gax_th = gax_th,
                  gy_th = gy_th,
                  gay_th = gay_th,
                  fx_th = fx_th,
                  fy_th = fy_th,
                  pry_th = fy_th/Ny,
                  gx_pk = gx_pk,
                  gax_pk = gax_pk,
                  gy_pk = gy_pk,
                  gay_pk = gay_pk,
                  fx_pk = fx_pk,
                  fy_pk = fy_pk,
                  pry_pk = fy_pk/Ny,
                  gx_E = gx_E,
                  gax_E = gax_E,
                  gy_E = gy_E,
                  gay_E = gay_E,
                  fx_E = fx_E,
                  fy_E = fy_E,
                  pry_E = fy_E/Ny
                  )



