library('xlsx')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")


##############################################################################################
#ARG <- subset(ARG,Season == "2021")
arg_totalrounds <-  (length(arg_teams) - 1 )*2
arg_totalmatches <- (length(arg_teams)*(length(arg_teams) - 1))
arg_eachround <- arg_totalmatches / arg_totalrounds
arg_matchesplayed <-  nrow(ARG)

ARG_rounds <- ARG

if(arg_matchesplayed %% arg_eachround == 0)
{
  arg_currentround <- arg_matchesplayed / arg_eachround
  arg_matchday <- c()
  arg_matchday <- rep(1:arg_currentround, each = arg_eachround)
}else if(arg_matchesplayed %% arg_eachround != 0)

{

  arg_modulus <- arg_matchesplayed %% arg_eachround
  arg_currentround <- (arg_matchesplayed - arg_modulus) / arg_eachround
  arg_matchday <- c()
  arg_matchday_vec1 <- c()
  arg_matchday_vec2 <- c()
  arg_matchday_vec1 <- rep(1:arg_currentround, each = arg_eachround)
  arg_matchday_vec2[1:arg_modulus] <- c(arg_currentround + 1)
  arg_matchday <- append(arg_matchday_vec1,arg_matchday_vec2)
}
ARG_rounds <- cbind(ARG_rounds,arg_matchday)
tail(ARG_rounds)
#####################################################################################################
#####################################################################################################
##############################################################################################
AUT <- subset(AUT,Season == "2021/2022")
aut_totalrounds <-  (length(aut_teams) - 1 )*2
aut_totalmatches <- (length(aut_teams)*(length(aut_teams) - 1))
aut_eachround <- aut_totalmatches / aut_totalrounds

aut_matchesplayed <-  nrow(AUT)
AUT_rounds
AUT_rounds <- AUT

if(aut_matchesplayed %% aut_eachround == 0)
{
  aut_currentround <- aut_matchesplayed / aut_eachround
  aut_matchday <- c()
  aut_matchday <- rep(1:aut_currentround, each = aut_eachround)
}else if(aut_matchesplayed %% aut_eachround != 0)

{





  aut_modulus <- aut_matchesplayed %% aut_eachround
  aut_currentround <- (aut_matchesplayed - aut_modulus) / aut_eachround
  aut_matchday <- c()
  aut_matchday_vec1 <- c()
  aut_matchday_vec2 <- c()
  aut_matchday_vec1 <- rep(1:aut_currentround, each = aut_eachround)
  aut_matchday_vec2[1:aut_modulus] <- c(aut_currentround + 1)
  aut_matchday <- append(aut_matchday_vec1,aut_matchday_vec2)
}
AUT_rounds <- cbind(AUT_rounds,aut_matchday)
#####################################################################################################
##############################################################################################
BRA <- subset(BRA,Season == "2021")
bra_totalrounds <-  (length(bra_teams) - 1 )*2
bra_totalmatches <- (length(bra_teams)*(length(bra_teams) - 1))
bra_eachround <- bra_totalmatches / bra_totalrounds

bra_matchesplayed <-  nrow(BRA)

BRA_rounds <- BRA

if(bra_matchesplayed %% bra_eachround == 0)
{
  bra_currentround <- bra_matchesplayed / bra_eachround
  bra_matchday <- c()
  bra_matchday <- rep(1:bra_currentround, each = bra_eachround)
}else if(bra_matchesplayed %% bra_eachround != 0)

{

  bra_modulus <- bra_matchesplayed %% bra_eachround
  bra_currentround <- (bra_matchesplayed - bra_modulus) / bra_eachround
  bra_matchday <- c()
  bra_matchday_vec1 <- c()
  bra_matchday_vec2 <- c()
  bra_matchday_vec1 <- rep(1:bra_currentround, each = bra_eachround)
  bra_matchday_vec2[1:bra_modulus] <- c(bra_currentround + 1)
  bra_matchday <- append(bra_matchday_vec1,bra_matchday_vec2)
}
BRA_rounds <- cbind(BRA_rounds,bra_matchday)
#####################################################################################################
##############################################################################################
CHN <- subset(CHN,Season == "2021")
chn_totalrounds <-  (length(chn_teams) - 1 )*2
chn_totalmatches <- (length(chn_teams)*(length(chn_teams) - 1))
chn_eachround <- chn_totalmatches / chn_totalrounds

chn_matchesplayed <-  nrow(CHN)

CHN_rounds <- CHN

if(chn_matchesplayed %% chn_eachround == 0)
{
  chn_currentround <- chn_matchesplayed / chn_eachround
  chn_matchday <- c()
  chn_matchday <- rep(1:chn_currentround, each = chn_eachround)
}else if(chn_matchesplayed %% chn_eachround != 0)

{

  chn_modulus <- chn_matchesplayed %% chn_eachround
  chn_currentround <- (chn_matchesplayed - chn_modulus) / chn_eachround
  chn_matchday <- c()
  chn_matchday_vec1 <- c()
  chn_matchday_vec2 <- c()
  chn_matchday_vec1 <- rep(1:chn_currentround, each = chn_eachround)
  chn_matchday_vec2[1:chn_modulus] <- c(chn_currentround + 1)
  chn_matchday <- append(chn_matchday_vec1,chn_matchday_vec2)
}
CHN_rounds <- cbind(CHN_rounds,chn_matchday)
#####################################################################################################
##############################################################################################
DNK <- subset(DNK,Season == "2021/2022")
dnk_totalrounds <-  (length(dnk_teams) - 1 )*2
dnk_totalmatches <- (length(dnk_teams)*(length(dnk_teams) - 1))
dnk_eachround <- dnk_totalmatches / dnk_totalrounds

dnk_matchesplayed <-  nrow(DNK)

DNK_rounds <- DNK

if(dnk_matchesplayed %% dnk_eachround == 0)
{
  dnk_currentround <- dnk_matchesplayed / dnk_eachround
  dnk_matchday <- c()
  dnk_matchday <- rep(1:dnk_currentround, each = dnk_eachround)
}else if(dnk_matchesplayed %% dnk_eachround != 0)

{

  dnk_modulus <- dnk_matchesplayed %% dnk_eachround
  dnk_currentround <- (dnk_matchesplayed - dnk_modulus) / dnk_eachround
  dnk_matchday <- c()
  dnk_matchday_vec1 <- c()
  dnk_matchday_vec2 <- c()
  dnk_matchday_vec1 <- rep(1:dnk_currentround, each = dnk_eachround)
  dnk_matchday_vec2[1:dnk_modulus] <- c(dnk_currentround + 1)
  dnk_matchday <- append(dnk_matchday_vec1,dnk_matchday_vec2)
}
DNK_rounds <- cbind(DNK_rounds,dnk_matchday)
#####################################################################################################
##############################################################################################
FIN <- subset(FIN,Season == "2021")
fin_totalrounds <-  (length(fin_teams) - 1 )*2
fin_totalmatches <- (length(fin_teams)*(length(fin_teams) - 1))
fin_eachround <- fin_totalmatches / fin_totalrounds

fin_matchesplayed <-  nrow(FIN)

FIN_rounds <- FIN

if(fin_matchesplayed %% fin_eachround == 0)
{
  fin_currentround <- fin_matchesplayed / fin_eachround
  fin_matchday <- c()
  fin_matchday <- rep(1:fin_currentround, each = fin_eachround)
}else if(fin_matchesplayed %% fin_eachround != 0)

{

  fin_modulus <- fin_matchesplayed %% fin_eachround
  fin_currentround <- (fin_matchesplayed - fin_modulus) / fin_eachround
  fin_matchday <- c()
  fin_matchday_vec1 <- c()
  fin_matchday_vec2 <- c()
  fin_matchday_vec1 <- rep(1:fin_currentround, each = fin_eachround)
  fin_matchday_vec2[1:fin_modulus] <- c(fin_currentround + 1)
  fin_matchday <- append(fin_matchday_vec1,fin_matchday_vec2)
}
FIN_rounds <- cbind(FIN_rounds,fin_matchday)
#####################################################################################################
##############################################################################################
IRL <- subset(IRL,Season == "2021")
irl_totalrounds <-  (length(irl_teams) - 1 )*2
irl_totalmatches <- (length(irl_teams)*(length(irl_teams) - 1))
irl_eachround <- irl_totalmatches / irl_totalrounds

irl_matchesplayed <-  nrow(IRL)

IRL_rounds <- IRL

if(irl_matchesplayed %% irl_eachround == 0)
{
  irl_currentround <- irl_matchesplayed / irl_eachround
  irl_matchday <- c()
  irl_matchday <- rep(1:irl_currentround, each = irl_eachround)
}else if(irl_matchesplayed %% irl_eachround != 0)

{

  irl_modulus <- irl_matchesplayed %% irl_eachround
  irl_currentround <- (irl_matchesplayed - irl_modulus) / irl_eachround
  irl_matchday <- c()
  irl_matchday_vec1 <- c()
  irl_matchday_vec2 <- c()
  irl_matchday_vec1 <- rep(1:irl_currentround, each = irl_eachround)
  irl_matchday_vec2[1:irl_modulus] <- c(irl_currentround + 1)
  irl_matchday <- append(irl_matchday_vec1,irl_matchday_vec2)
}
IRL_rounds <- cbind(IRL_rounds,irl_matchday)
#####################################################################################################
##############################################################################################
JPN <- subset(JPN,Season == "2021")
jpn_totalrounds <-  (length(jpn_teams) - 1 )*2
jpn_totalmatches <- (length(jpn_teams)*(length(jpn_teams) - 1))
jpn_eachround <- jpn_totalmatches / jpn_totalrounds

jpn_matchesplayed <-  nrow(JPN)

JPN_rounds <- JPN

if(jpn_matchesplayed %% jpn_eachround == 0)
{
  jpn_currentround <- jpn_matchesplayed / jpn_eachround
  jpn_matchday <- c()
  jpn_matchday <- rep(1:jpn_currentround, each = jpn_eachround)
}else if(jpn_matchesplayed %% jpn_eachround != 0)

{

  jpn_modulus <- jpn_matchesplayed %% jpn_eachround
  jpn_currentround <- (jpn_matchesplayed - jpn_modulus) / jpn_eachround
  jpn_matchday <- c()
  jpn_matchday_vjpn1 <- c()
  jpn_matchday_vjpn2 <- c()
  jpn_matchday_vjpn1 <- rep(1:jpn_currentround, each = jpn_eachround)
  jpn_matchday_vjpn2[1:jpn_modulus] <- c(jpn_currentround + 1)
  jpn_matchday <- append(jpn_matchday_vjpn1,jpn_matchday_vjpn2)
}
JPN_rounds <- cbind(JPN_rounds,jpn_matchday)
#####################################################################################################
##############################################################################################
MEX <- subset(MEX,Season == "2020/2021")
mex_totalrounds <-  (length(mex_teams) - 1 )*2
mex_totalmatches <- (length(mex_teams)*(length(mex_teams) - 1))
mex_eachround <- mex_totalmatches / mex_totalrounds

mex_matchesplayed <-  nrow(MEX)

MEX_rounds <- MEX

if(mex_matchesplayed %% mex_eachround == 0)
{
  mex_currentround <- mex_matchesplayed / mex_eachround
  mex_matchday <- c()
  mex_matchday <- rep(1:mex_currentround, each = mex_eachround)
}else if(mex_matchesplayed %% mex_eachround != 0)

{

  mex_modulus <- mex_matchesplayed %% mex_eachround
  mex_currentround <- (mex_matchesplayed - mex_modulus) / mex_eachround
  mex_matchday <- c()
  mex_matchday_vec1 <- c()
  mex_matchday_vec2 <- c()
  mex_matchday_vec1 <- rep(1:mex_currentround, each = mex_eachround)
  mex_matchday_vec2[1:mex_modulus] <- c(mex_currentround + 1)
  mex_matchday <- append(mex_matchday_vec1,mex_matchday_vec2)
}
MEX_rounds <- cbind(MEX_rounds,mex_matchday)
#####################################################################################################
##############################################################################################
MLS <- subset(MLS,Season == "2021")
mls_totalrounds <-  (length(mls_teams) - 1 )*2
mls_totalmatches <- (length(mls_teams)*(length(mls_teams) - 1))
mls_eachround <- mls_totalmatches / mls_totalrounds

mls_matchesplayed <-  nrow(MLS)

MLS_rounds <- MLS

if(mls_matchesplayed %% mls_eachround == 0)
{
  mls_currentround <- mls_matchesplayed / mls_eachround
  mls_matchday <- c()
  mls_matchday <- rep(1:mls_currentround, each = mls_eachround)
}else if(mls_matchesplayed %% mls_eachround != 0)

{

  mls_modulus <- mls_matchesplayed %% mls_eachround
  mls_currentround <- (mls_matchesplayed - mls_modulus) / mls_eachround
  mls_matchday <- c()
  mls_matchday_vec1 <- c()
  mls_matchday_vec2 <- c()
  mls_matchday_vec1 <- rep(1:mls_currentround, each = mls_eachround)
  mls_matchday_vec2[1:mls_modulus] <- c(mls_currentround + 1)
  mls_matchday <- append(mls_matchday_vec1,mls_matchday_vec2)
}
MLS_rounds <- cbind(MLS_rounds,mls_matchday)
#####################################################################################################
##############################################################################################
NOR <- subset(NOR,Season == "2021")
nor_totalrounds <-  (length(nor_teams) - 1 )*2
nor_totalmatches <- (length(nor_teams)*(length(nor_teams) - 1))
nor_eachround <- nor_totalmatches / nor_totalrounds

nor_matchesplayed <-  nrow(NOR)

NOR_rounds <- NOR

if(nor_matchesplayed %% nor_eachround == 0)
{
  nor_currentround <- nor_matchesplayed / nor_eachround
  nor_matchday <- c()
  nor_matchday <- rep(1:nor_currentround, each = nor_eachround)
}else if(nor_matchesplayed %% nor_eachround != 0)

{

  nor_modulus <- nor_matchesplayed %% nor_eachround
  nor_currentround <- (nor_matchesplayed - nor_modulus) / nor_eachround
  nor_matchday <- c()
  nor_matchday_vec1 <- c()
  nor_matchday_vec2 <- c()
  nor_matchday_vec1 <- rep(1:nor_currentround, each = nor_eachround)
  nor_matchday_vec2[1:nor_modulus] <- c(nor_currentround + 1)
  nor_matchday <- append(nor_matchday_vec1,nor_matchday_vec2)
}
NOR_rounds <- cbind(NOR_rounds,nor_matchday)
#####################################################################################################
##############################################################################################
POL <- subset(POL,Season == "2021/2022")
pol_totalrounds <-  (length(pol_teams) - 1 )*2
pol_totalmatches <- (length(pol_teams)*(length(pol_teams) - 1))
pol_eachround <- pol_totalmatches / pol_totalrounds

pol_matchesplayed <-  nrow(POL)

POL_rounds <- POL

if(pol_matchesplayed %% pol_eachround == 0)
{
  pol_currentround <- pol_matchesplayed / pol_eachround
  pol_matchday <- c()
  pol_matchday <- rep(1:pol_currentround, each = pol_eachround)
}else if(pol_matchesplayed %% pol_eachround != 0)

{

  pol_modulus <- pol_matchesplayed %% pol_eachround
  pol_currentround <- (pol_matchesplayed - pol_modulus) / pol_eachround
  pol_matchday <- c()
  pol_matchday_vec1 <- c()
  pol_matchday_vec2 <- c()
  pol_matchday_vec1 <- rep(1:pol_currentround, each = pol_eachround)
  pol_matchday_vec2[1:pol_modulus] <- c(pol_currentround + 1)
  pol_matchday <- append(pol_matchday_vec1,pol_matchday_vec2)
}
POL_rounds <- cbind(POL_rounds,pol_matchday)
#####################################################################################################
##############################################################################################
ROU <- subset(ROU,Season == "2021/2022")
rou_totalrounds <-  (length(rou_teams) - 1 )*2
rou_totalmatches <- (length(rou_teams)*(length(rou_teams) - 1))
rou_eachround <- rou_totalmatches / rou_totalrounds

rou_matchesplayed <-  nrow(ROU)

ROU_rounds <- ROU

if(rou_matchesplayed %% rou_eachround == 0)
{
  rou_currentround <- rou_matchesplayed / rou_eachround
  rou_matchday <- c()
  rou_matchday <- rep(1:rou_currentround, each = rou_eachround)
}else if(rou_matchesplayed %% rou_eachround != 0)

{

  rou_modulus <- rou_matchesplayed %% rou_eachround
  rou_currentround <- (rou_matchesplayed - rou_modulus) / rou_eachround
  rou_matchday <- c()
  rou_matchday_vec1 <- c()
  rou_matchday_vec2 <- c()
  rou_matchday_vec1 <- rep(1:rou_currentround, each = rou_eachround)
  rou_matchday_vec2[1:rou_modulus] <- c(rou_currentround + 1)
  rou_matchday <- append(rou_matchday_vec1,rou_matchday_vec2)
}
ROU_rounds <- cbind(ROU_rounds,rou_matchday)
##########################################################
##############################################################################################
RUS <- subset(RUS,Season == "2021/2022")
rus_totalrounds <-  (length(rus_teams) - 1 )*2
rus_totalmatches <- (length(rus_teams)*(length(rus_teams) - 1))
rus_eachround <- rus_totalmatches / rus_totalrounds

rus_matchesplayed <-  nrow(RUS)

RUS_rounds <- RUS

if(rus_matchesplayed %% rus_eachround == 0)
{
  rus_currentround <- rus_matchesplayed / rus_eachround
  rus_matchday <- c()
  rus_matchday <- rep(1:rus_currentround, each = rus_eachround)
}else if(rus_matchesplayed %% rus_eachround != 0)

{

  rus_modulus <- rus_matchesplayed %% rus_eachround
  rus_currentround <- (rus_matchesplayed - rus_modulus) / rus_eachround
  rus_matchday <- c()
  rus_matchday_vec1 <- c()
  rus_matchday_vec2 <- c()
  rus_matchday_vec1 <- rep(1:rus_currentround, each = rus_eachround)
  rus_matchday_vec2[1:rus_modulus] <- c(rus_currentround + 1)
  rus_matchday <- append(rus_matchday_vec1,rus_matchday_vec2)
}
RUS_rounds <- cbind(RUS_rounds,rus_matchday)
#####################################################################################################
##############################################################################################
SWE <- subset(SWE,Season == "2021")
swe_totalrounds <-  (length(swe_teams) - 1 )*2
swe_totalmatches <- (length(swe_teams)*(length(swe_teams) - 1))
swe_eachround <- swe_totalmatches / swe_totalrounds

swe_matchesplayed <-  nrow(SWE)

SWE_rounds <- SWE

if(swe_matchesplayed %% swe_eachround == 0)
{
  swe_currentround <- swe_matchesplayed / swe_eachround
  swe_matchday <- c()
  swe_matchday <- rep(1:swe_currentround, each = swe_eachround)
}else if(swe_matchesplayed %% swe_eachround != 0)

{

  swe_modulus <- swe_matchesplayed %% swe_eachround
  swe_currentround <- (swe_matchesplayed - swe_modulus) / swe_eachround
  swe_matchday <- c()
  swe_matchday_vec1 <- c()
  swe_matchday_vec2 <- c()
  swe_matchday_vec1 <- rep(1:swe_currentround, each = swe_eachround)
  swe_matchday_vec2[1:swe_modulus] <- c(swe_currentround + 1)
  swe_matchday <- append(swe_matchday_vec1,swe_matchday_vec2)
}
SWE_rounds <- cbind(SWE_rounds,swe_matchday)
#####################################################################################################
##############################################################################################
SWZ <- subset(SWZ,Season == "2021/2022")
swz_totalrounds <-  (length(swz_teams) - 1 )*2
swz_totalmatches <- (length(swz_teams)*(length(swz_teams) - 1))
swz_eachround <- swz_totalmatches / swz_totalrounds

swz_matchesplayed <-  nrow(SWZ)

SWZ_rounds <- SWZ

if(swz_matchesplayed %% swz_eachround == 0)
{
  swz_currentround <- swz_matchesplayed / swz_eachround
  swz_matchday <- c()
  swz_matchday <- rep(1:swz_currentround, each = swz_eachround)
}else if(swz_matchesplayed %% swz_eachround != 0)

{

  swz_modulus <- swz_matchesplayed %% swz_eachround
  swz_currentround <- (swz_matchesplayed - swz_modulus) / swz_eachround
  swz_matchday <- c()
  swz_matchday_vec1 <- c()
  swz_matchday_vec2 <- c()
  swz_matchday_vec1 <- rep(1:swz_currentround, each = swz_eachround)
  swz_matchday_vec2[1:swz_modulus] <- c(swz_currentround + 1)
  swz_matchday <- append(swz_matchday_vec1,swz_matchday_vec2)
}
SWZ_rounds <- cbind(SWZ_rounds,swz_matchday)
#####################################################################################################
##############################################################################################






