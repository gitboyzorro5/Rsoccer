library('xlsx')
library('lubridate')
#############################################################
myfixturesnl <- readxl::read_excel('../FDAS/myodds_20212022_newleagues.xlsx', sheet = '3way')
myfixturesnl$Date <- dmy(myfixturesnl$Date)
myfixturesnl <- myfixturesnl[myfixturesnl$Date >= '2022-08-12',]

####################################################################
myfixturesnlaut <- subset(myfixturesnl,Div == "Admiral Bundesliga")
####################################################################
final_doublefixture_aut <- c()
final_first_column_aut <- c()
final_second_column_aut <- c()

for(dbfixture_aut in 1:nrow(myfixturesnlaut))
{
  test_fixture_aut <- myfixturesnlaut[dbfixture_aut,]
  test_fixture_inv_aut <- test_fixture_aut[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_aut <- test_fixture_aut[rep(seq_len(nrow(test_fixture_aut)), each = 5),]
  final_test_fixture_inv_aut <- test_fixture_inv_aut[rep(seq_len(nrow(test_fixture_inv_aut)), each = 5),]


  final_test_fixture_aut <- final_test_fixture_aut[,c(24,25)]
  final_test_fixture_inv_aut <- final_test_fixture_inv_aut[,c(24,25)]

  final_test_fixture_aut <- as.data.frame(final_test_fixture_aut)
  final_test_fixture_inv_aut <- as.data.frame(final_test_fixture_inv_aut)

  final_first_column_aut <- c(final_test_fixture_aut[,c(1)],final_test_fixture_inv_aut[,c(1)])
  final_second_column_aut <- c(final_test_fixture_aut[,c(2)],final_test_fixture_inv_aut[,c(2)])

  mid_doublefixture_aut <- cbind(final_first_column_aut,final_second_column_aut)

  final_doublefixture_aut <- rbind(final_doublefixture_aut,mid_doublefixture_aut)


}
unlink('finaldoublefixtureaut.csv')
write.csv(final_doublefixture_aut,"finaldoublefixtureaut.csv")
############################################################################################################################
###################################################################
myfixturesnlarg <- subset(myfixturesnl,Div == "Liga Profesional")
####################################################################
final_doublefixture_arg <- c()
final_first_column_arg <- c()
final_second_column_arg <- c()

for(dbfixture_arg in 1:nrow(myfixturesnlarg))
{
  test_fixture_arg <- myfixturesnlarg[dbfixture_arg,]
  test_fixture_inv_arg <- test_fixture_arg[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_arg <- test_fixture_arg[rep(seq_len(nrow(test_fixture_arg)), each = 4),]
  final_test_fixture_inv_arg <- test_fixture_inv_arg[rep(seq_len(nrow(test_fixture_inv_arg)), each = 4),]


  final_test_fixture_arg <- final_test_fixture_arg[,c(24,25)]
  final_test_fixture_inv_arg <- final_test_fixture_inv_arg[,c(24,25)]

  final_test_fixture_arg <- as.data.frame(final_test_fixture_arg)
  final_test_fixture_inv_arg <- as.data.frame(final_test_fixture_inv_arg)

  final_first_column_arg <- c(final_test_fixture_arg[,c(1)],final_test_fixture_inv_arg[,c(1)])
  final_second_column_arg <- c(final_test_fixture_arg[,c(2)],final_test_fixture_inv_arg[,c(2)])

  mid_doublefixture_arg <- cbind(final_first_column_arg,final_second_column_arg)

  final_doublefixture_arg <- rbind(final_doublefixture_arg,mid_doublefixture_arg)


}
unlink('finaldoublefixturearg.csv')
write.csv(final_doublefixture_arg,"finaldoublefixturearg.csv")
##################################################################################################################
myfixturesnlbra <- subset(myfixturesnl,Div == "Serie A")
####################################################################
final_doublefixture_bra <- c()
final_first_column_bra <- c()
final_second_column_bra <- c()

for(dbfixture_bra in 1:nrow(myfixturesnlbra))
{
  test_fixture_bra <- myfixturesnlbra[dbfixture_bra,]
  test_fixture_inv_bra <- test_fixture_bra[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_bra <- test_fixture_bra[rep(seq_len(nrow(test_fixture_bra)), each = 4),]
  final_test_fixture_inv_bra <- test_fixture_inv_bra[rep(seq_len(nrow(test_fixture_inv_bra)), each = 4),]


  final_test_fixture_bra <- final_test_fixture_bra[,c(24,25)]
  final_test_fixture_inv_bra <- final_test_fixture_inv_bra[,c(24,25)]

  final_test_fixture_bra <- as.data.frame(final_test_fixture_bra)
  final_test_fixture_inv_bra <- as.data.frame(final_test_fixture_inv_bra)

  final_first_column_bra <- c(final_test_fixture_bra[,c(1)],final_test_fixture_inv_bra[,c(1)])
  final_second_column_bra <- c(final_test_fixture_bra[,c(2)],final_test_fixture_inv_bra[,c(2)])

  mid_doublefixture_bra <- cbind(final_first_column_bra,final_second_column_bra)

  final_doublefixture_bra <- rbind(final_doublefixture_bra,mid_doublefixture_bra)


}
unlink('finaldoublefixturebra.csv')
write.csv(final_doublefixture_bra,"finaldoublefixturebra.csv")
############################################################################################################################
myfixturesnlchn <- subset(myfixturesnl,Div == "Super League")
####################################################################
final_doublefixture_chn <- c()
final_first_column_chn <- c()
final_second_column_chn <- c()

for(dbfixture_chn in 1:nrow(myfixturesnlchn))
{
  test_fixture_chn <- myfixturesnlchn[dbfixture_chn,]
  test_fixture_inv_chn <- test_fixture_chn[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_chn <- test_fixture_chn[rep(seq_len(nrow(test_fixture_chn)), each = 4),]
  final_test_fixture_inv_chn <- test_fixture_inv_chn[rep(seq_len(nrow(test_fixture_inv_chn)), each = 4),]


  final_test_fixture_chn <- final_test_fixture_chn[,c(24,25)]
  final_test_fixture_inv_chn <- final_test_fixture_inv_chn[,c(24,25)]

  final_test_fixture_chn <- as.data.frame(final_test_fixture_chn)
  final_test_fixture_inv_chn <- as.data.frame(final_test_fixture_inv_chn)

  final_first_column_chn <- c(final_test_fixture_chn[,c(1)],final_test_fixture_inv_chn[,c(1)])
  final_second_column_chn <- c(final_test_fixture_chn[,c(2)],final_test_fixture_inv_chn[,c(2)])

  mid_doublefixture_chn <- cbind(final_first_column_chn,final_second_column_chn)

  final_doublefixture_chn <- rbind(final_doublefixture_chn,mid_doublefixture_chn)


}
unlink('finaldoublefixturechn.csv')
write.csv(final_doublefixture_chn,"finaldoublefixturechn.csv")
############################################################################################################################
myfixturesnldnk <- subset(myfixturesnl,Div == "Superliga")
####################################################################
final_doublefixture_dnk <- c()
final_first_column_dnk <- c()
final_second_column_dnk <- c()

for(dbfixture_dnk in 1:nrow(myfixturesnldnk))
{
  test_fixture_dnk <- myfixturesnldnk[dbfixture_dnk,]
  test_fixture_inv_dnk <- test_fixture_dnk[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_dnk <- test_fixture_dnk[rep(seq_len(nrow(test_fixture_dnk)), each = 5),]
  final_test_fixture_inv_dnk <- test_fixture_inv_dnk[rep(seq_len(nrow(test_fixture_inv_dnk)), each = 5),]


  final_test_fixture_dnk <- final_test_fixture_dnk[,c(24,25)]
  final_test_fixture_inv_dnk <- final_test_fixture_inv_dnk[,c(24,25)]

  final_test_fixture_dnk <- as.data.frame(final_test_fixture_dnk)
  final_test_fixture_inv_dnk <- as.data.frame(final_test_fixture_inv_dnk)

  final_first_column_dnk <- c(final_test_fixture_dnk[,c(1)],final_test_fixture_inv_dnk[,c(1)])
  final_second_column_dnk <- c(final_test_fixture_dnk[,c(2)],final_test_fixture_inv_dnk[,c(2)])

  mid_doublefixture_dnk <- cbind(final_first_column_dnk,final_second_column_dnk)

  final_doublefixture_dnk <- rbind(final_doublefixture_dnk,mid_doublefixture_dnk)


}
unlink('finaldoublefixturednk.csv')
write.csv(final_doublefixture_dnk,"finaldoublefixturednk.csv")
############################################################################################################################
myfixturesnlfin <- subset(myfixturesnl,Div == "Veikkausliiga")
####################################################################
final_doublefixture_fin <- c()
final_first_column_fin <- c()
final_second_column_fin <- c()

for(dbfixture_fin in 1:nrow(myfixturesnlfin))
{
  test_fixture_fin <- myfixturesnlfin[dbfixture_fin,]
  test_fixture_inv_fin <- test_fixture_fin[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_fin <- test_fixture_fin[rep(seq_len(nrow(test_fixture_fin)), each = 5),]
  final_test_fixture_inv_fin <- test_fixture_inv_fin[rep(seq_len(nrow(test_fixture_inv_fin)), each = 5),]


  final_test_fixture_fin <- final_test_fixture_fin[,c(24,25)]
  final_test_fixture_inv_fin <- final_test_fixture_inv_fin[,c(24,25)]

  final_test_fixture_fin <- as.data.frame(final_test_fixture_fin)
  final_test_fixture_inv_fin <- as.data.frame(final_test_fixture_inv_fin)

  final_first_column_fin <- c(final_test_fixture_fin[,c(1)],final_test_fixture_inv_fin[,c(1)])
  final_second_column_fin <- c(final_test_fixture_fin[,c(2)],final_test_fixture_inv_fin[,c(2)])

  mid_doublefixture_fin <- cbind(final_first_column_fin,final_second_column_fin)

  final_doublefixture_fin <- rbind(final_doublefixture_fin,mid_doublefixture_fin)


}
unlink('finaldoublefixturefin.csv')
write.csv(final_doublefixture_fin,"finaldoublefixturefin.csv")
############################################################################################################################
myfixturesnlirl <- subset(myfixturesnl,Div == "Premier Division")
####################################################################
final_doublefixture_irl <- c()
final_first_column_irl <- c()
final_second_column_irl <- c()

for(dbfixture_irl in 1:nrow(myfixturesnlirl))
{
  test_fixture_irl <- myfixturesnlirl[dbfixture_irl,]
  test_fixture_inv_irl <- test_fixture_irl[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_irl <- test_fixture_irl[rep(seq_len(nrow(test_fixture_irl)), each = 5),]
  final_test_fixture_inv_irl <- test_fixture_inv_irl[rep(seq_len(nrow(test_fixture_inv_irl)), each = 5),]


  final_test_fixture_irl <- final_test_fixture_irl[,c(24,25)]
  final_test_fixture_inv_irl <- final_test_fixture_inv_irl[,c(24,25)]

  final_test_fixture_irl <- as.data.frame(final_test_fixture_irl)
  final_test_fixture_inv_irl <- as.data.frame(final_test_fixture_inv_irl)

  final_first_column_irl <- c(final_test_fixture_irl[,c(1)],final_test_fixture_inv_irl[,c(1)])
  final_second_column_irl <- c(final_test_fixture_irl[,c(2)],final_test_fixture_inv_irl[,c(2)])

  mid_doublefixture_irl <- cbind(final_first_column_irl,final_second_column_irl)

  final_doublefixture_irl <- rbind(final_doublefixture_irl,mid_doublefixture_irl)


}
unlink('finaldoublefixtureirl.csv')
write.csv(final_doublefixture_irl,"finaldoublefixtureirl.csv")
############################################################################################################################
myfixturesnljpn <- subset(myfixturesnl,Div == "J1 League")
####################################################################
final_doublefixture_jpn <- c()
final_first_column_jpn <- c()
final_second_column_jpn <- c()

for(dbfixture_jpn in 1:nrow(myfixturesnljpn))
{
  test_fixture_jpn <- myfixturesnljpn[dbfixture_jpn,]
  test_fixture_inv_jpn <- test_fixture_jpn[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_jpn <- test_fixture_jpn[rep(seq_len(nrow(test_fixture_jpn)), each = 4),]
  final_test_fixture_inv_jpn <- test_fixture_inv_jpn[rep(seq_len(nrow(test_fixture_inv_jpn)), each = 4),]


  final_test_fixture_jpn <- final_test_fixture_jpn[,c(24,25)]
  final_test_fixture_inv_jpn <- final_test_fixture_inv_jpn[,c(24,25)]

  final_test_fixture_jpn <- as.data.frame(final_test_fixture_jpn)
  final_test_fixture_inv_jpn <- as.data.frame(final_test_fixture_inv_jpn)

  final_first_column_jpn <- c(final_test_fixture_jpn[,c(1)],final_test_fixture_inv_jpn[,c(1)])
  final_second_column_jpn <- c(final_test_fixture_jpn[,c(2)],final_test_fixture_inv_jpn[,c(2)])

  mid_doublefixture_jpn <- cbind(final_first_column_jpn,final_second_column_jpn)

  final_doublefixture_jpn <- rbind(final_doublefixture_jpn,mid_doublefixture_jpn)


}
unlink('finaldoublefixturejpn.csv')
write.csv(final_doublefixture_jpn,"finaldoublefixturejpn.csv")
############################################################################################################################
myfixturesnlmex <- subset(myfixturesnl,Div == "Liga MX")
####################################################################
final_doublefixture_mex <- c()
final_first_column_mex <- c()
final_second_column_mex <- c()

for(dbfixture_mex in 1:nrow(myfixturesnlmex))
{
  test_fixture_mex <- myfixturesnlmex[dbfixture_mex,]
  test_fixture_inv_mex <- test_fixture_mex[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_mex <- test_fixture_mex[rep(seq_len(nrow(test_fixture_mex)), each = 4),]
  final_test_fixture_inv_mex <- test_fixture_inv_mex[rep(seq_len(nrow(test_fixture_inv_mex)), each = 4),]


  final_test_fixture_mex <- final_test_fixture_mex[,c(24,25)]
  final_test_fixture_inv_mex <- final_test_fixture_inv_mex[,c(24,25)]

  final_test_fixture_mex <- as.data.frame(final_test_fixture_mex)
  final_test_fixture_inv_mex <- as.data.frame(final_test_fixture_inv_mex)

  final_first_column_mex <- c(final_test_fixture_mex[,c(1)],final_test_fixture_inv_mex[,c(1)])
  final_second_column_mex <- c(final_test_fixture_mex[,c(2)],final_test_fixture_inv_mex[,c(2)])

  mid_doublefixture_mex <- cbind(final_first_column_mex,final_second_column_mex)

  final_doublefixture_mex <- rbind(final_doublefixture_mex,mid_doublefixture_mex)


}
unlink('finaldoublefixturemex.csv')
write.csv(final_doublefixture_mex,"finaldoublefixturemex.csv")
############################################################################################################################
myfixturesnlmls <- subset(myfixturesnl,Div == "MLS")
####################################################################
final_doublefixture_mls <- c()
final_first_column_mls <- c()
final_second_column_mls <- c()

for(dbfixture_mls in 1:nrow(myfixturesnlmls))
{
  test_fixture_mls <- myfixturesnlmls[dbfixture_mls,]
  test_fixture_inv_mls <- test_fixture_mls[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_mls <- test_fixture_mls[rep(seq_len(nrow(test_fixture_mls)), each = 6),]
  final_test_fixture_inv_mls <- test_fixture_inv_mls[rep(seq_len(nrow(test_fixture_inv_mls)), each = 6),]


  final_test_fixture_mls <- final_test_fixture_mls[,c(24,25)]
  final_test_fixture_inv_mls <- final_test_fixture_inv_mls[,c(24,25)]

  final_test_fixture_mls <- as.data.frame(final_test_fixture_mls)
  final_test_fixture_inv_mls <- as.data.frame(final_test_fixture_inv_mls)

  final_first_column_mls <- c(final_test_fixture_mls[,c(1)],final_test_fixture_inv_mls[,c(1)])
  final_second_column_mls <- c(final_test_fixture_mls[,c(2)],final_test_fixture_inv_mls[,c(2)])

  mid_doublefixture_mls <- cbind(final_first_column_mls,final_second_column_mls)

  final_doublefixture_mls <- rbind(final_doublefixture_mls,mid_doublefixture_mls)


}
unlink('finaldoublefixturemls.csv')
write.csv(final_doublefixture_mls,"finaldoublefixturemls.csv")
############################################################################################################################
myfixturesnlnor <- subset(myfixturesnl,Div == "Eliteserien")
####################################################################
final_doublefixture_nor <- c()
final_first_column_nor <- c()
final_second_column_nor <- c()

for(dbfixture_nor in 1:nrow(myfixturesnlnor))
{
  test_fixture_nor <- myfixturesnlnor[dbfixture_nor,]
  test_fixture_inv_nor <- test_fixture_nor[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_nor <- test_fixture_nor[rep(seq_len(nrow(test_fixture_nor)), each = 6),]
  final_test_fixture_inv_nor <- test_fixture_inv_nor[rep(seq_len(nrow(test_fixture_inv_nor)), each = 6),]


  final_test_fixture_nor <- final_test_fixture_nor[,c(24,25)]
  final_test_fixture_inv_nor <- final_test_fixture_inv_nor[,c(24,25)]

  final_test_fixture_nor <- as.data.frame(final_test_fixture_nor)
  final_test_fixture_inv_nor <- as.data.frame(final_test_fixture_inv_nor)

  final_first_column_nor <- c(final_test_fixture_nor[,c(1)],final_test_fixture_inv_nor[,c(1)])
  final_second_column_nor <- c(final_test_fixture_nor[,c(2)],final_test_fixture_inv_nor[,c(2)])

  mid_doublefixture_nor <- cbind(final_first_column_nor,final_second_column_nor)

  final_doublefixture_nor <- rbind(final_doublefixture_nor,mid_doublefixture_nor)


}
unlink('finaldoublefixturenor.csv')
write.csv(final_doublefixture_nor,"finaldoublefixturenor.csv")
############################################################################################################################
myfixturesnlpol <- subset(myfixturesnl,Div == "Ekstraklasa")
####################################################################
final_doublefixture_pol <- c()
final_first_column_pol <- c()
final_second_column_pol <- c()

for(dbfixture_pol in 1:nrow(myfixturesnlpol))
{
  test_fixture_pol <- myfixturesnlpol[dbfixture_pol,]
  test_fixture_inv_pol <- test_fixture_pol[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_pol <- test_fixture_pol[rep(seq_len(nrow(test_fixture_pol)), each = 5),]
  final_test_fixture_inv_pol <- test_fixture_inv_pol[rep(seq_len(nrow(test_fixture_inv_pol)), each = 5),]


  final_test_fixture_pol <- final_test_fixture_pol[,c(24,25)]
  final_test_fixture_inv_pol <- final_test_fixture_inv_pol[,c(24,25)]

  final_test_fixture_pol <- as.data.frame(final_test_fixture_pol)
  final_test_fixture_inv_pol <- as.data.frame(final_test_fixture_inv_pol)

  final_first_column_pol <- c(final_test_fixture_pol[,c(1)],final_test_fixture_inv_pol[,c(1)])
  final_second_column_pol <- c(final_test_fixture_pol[,c(2)],final_test_fixture_inv_pol[,c(2)])

  mid_doublefixture_pol <- cbind(final_first_column_pol,final_second_column_pol)

  final_doublefixture_pol <- rbind(final_doublefixture_pol,mid_doublefixture_pol)


}
unlink('finaldoublefixturepol.csv')
write.csv(final_doublefixture_pol,"finaldoublefixturepol.csv")
############################################################################################################################
############################################################################################################################
myfixturesnlrou <- subset(myfixturesnl,Div == "Liga 1")
####################################################################
final_doublefixture_rou <- c()
final_first_column_rou <- c()
final_second_column_rou <- c()

for(dbfixture_rou in 1:nrow(myfixturesnlrou))
{
  test_fixture_rou <- myfixturesnlrou[dbfixture_rou,]
  test_fixture_inv_rou <- test_fixture_rou[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_rou <- test_fixture_rou[rep(seq_len(nrow(test_fixture_rou)), each = 6),]
  final_test_fixture_inv_rou <- test_fixture_inv_rou[rep(seq_len(nrow(test_fixture_inv_rou)), each = 6),]


  final_test_fixture_rou <- final_test_fixture_rou[,c(24,25)]
  final_test_fixture_inv_rou <- final_test_fixture_inv_rou[,c(24,25)]

  final_test_fixture_rou <- as.data.frame(final_test_fixture_rou)
  final_test_fixture_inv_rou <- as.data.frame(final_test_fixture_inv_rou)

  final_first_column_rou <- c(final_test_fixture_rou[,c(1)],final_test_fixture_inv_rou[,c(1)])
  final_second_column_rou <- c(final_test_fixture_rou[,c(2)],final_test_fixture_inv_rou[,c(2)])

  mid_doublefixture_rou <- cbind(final_first_column_rou,final_second_column_rou)

  final_doublefixture_rou <- rbind(final_doublefixture_rou,mid_doublefixture_rou)


}
unlink('finaldoublefixturerou.csv')
write.csv(final_doublefixture_rou,"finaldoublefixturerou.csv")
###########################################################################################################################
myfixturesnlrus <- subset(myfixturesnl,Div == "Premier League")
####################################################################
final_doublefixture_rus <- c()
final_first_column_rus <- c()
final_second_column_rus <- c()

for(dbfixture_rus in 1:nrow(myfixturesnlrus))
{
  test_fixture_rus <- myfixturesnlrus[dbfixture_rus,]
  test_fixture_inv_rus <- test_fixture_rus[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_rus <- test_fixture_rus[rep(seq_len(nrow(test_fixture_rus)), each = 5),]
  final_test_fixture_inv_rus <- test_fixture_inv_rus[rep(seq_len(nrow(test_fixture_inv_rus)), each = 5),]


  final_test_fixture_rus <- final_test_fixture_rus[,c(24,25)]
  final_test_fixture_inv_rus <- final_test_fixture_inv_rus[,c(24,25)]

  final_test_fixture_rus <- as.data.frame(final_test_fixture_rus)
  final_test_fixture_inv_rus <- as.data.frame(final_test_fixture_inv_rus)

  final_first_column_rus <- c(final_test_fixture_rus[,c(1)],final_test_fixture_inv_rus[,c(1)])
  final_second_column_rus <- c(final_test_fixture_rus[,c(2)],final_test_fixture_inv_rus[,c(2)])

  mid_doublefixture_rus <- cbind(final_first_column_rus,final_second_column_rus)

  final_doublefixture_rus <- rbind(final_doublefixture_rus,mid_doublefixture_rus)


}
unlink('finaldoublefixturerus.csv')
write.csv(final_doublefixture_rus,"finaldoublefixturerus.csv")
############################################################################################################################
myfixturesnlswe <- subset(myfixturesnl,Div == "Allsvenskan")
####################################################################
final_doublefixture_swe <- c()
final_first_column_swe <- c()
final_second_column_swe <- c()

for(dbfixture_swe in 1:nrow(myfixturesnlswe))
{
  test_fixture_swe <- myfixturesnlswe[dbfixture_swe,]
  test_fixture_inv_swe <- test_fixture_swe[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_swe <- test_fixture_swe[rep(seq_len(nrow(test_fixture_swe)), each = 4),]
  final_test_fixture_inv_swe <- test_fixture_inv_swe[rep(seq_len(nrow(test_fixture_inv_swe)), each = 4),]


  final_test_fixture_swe <- final_test_fixture_swe[,c(24,25)]
  final_test_fixture_inv_swe <- final_test_fixture_inv_swe[,c(24,25)]

  final_test_fixture_swe <- as.data.frame(final_test_fixture_swe)
  final_test_fixture_inv_swe <- as.data.frame(final_test_fixture_inv_swe)

  final_first_column_swe <- c(final_test_fixture_swe[,c(1)],final_test_fixture_inv_swe[,c(1)])
  final_second_column_swe <- c(final_test_fixture_swe[,c(2)],final_test_fixture_inv_swe[,c(2)])

  mid_doublefixture_swe <- cbind(final_first_column_swe,final_second_column_swe)

  final_doublefixture_swe <- rbind(final_doublefixture_swe,mid_doublefixture_swe)


}
unlink('finaldoublefixtureswe.csv')
write.csv(final_doublefixture_swe,"finaldoublefixtureswe.csv")
############################################################################################################################
myfixturesnlswz <- subset(myfixturesnl,Div == "Swiss")
####################################################################
final_doublefixture_swz <- c()
final_first_column_swz <- c()
final_second_column_swz <- c()

for(dbfixture_swz in 1:nrow(myfixturesnlswz))
{
  test_fixture_swz <- myfixturesnlswz[dbfixture_swz,]
  test_fixture_inv_swz <- test_fixture_swz[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_swz <- test_fixture_swz[rep(seq_len(nrow(test_fixture_swz)), each = 5),]
  final_test_fixture_inv_swz <- test_fixture_inv_swz[rep(seq_len(nrow(test_fixture_inv_swz)), each = 5),]


  final_test_fixture_swz <- final_test_fixture_swz[,c(24,25)]
  final_test_fixture_inv_swz <- final_test_fixture_inv_swz[,c(24,25)]

  final_test_fixture_swz <- as.data.frame(final_test_fixture_swz)
  final_test_fixture_inv_swz <- as.data.frame(final_test_fixture_inv_swz)

  final_first_column_swz <- c(final_test_fixture_swz[,c(1)],final_test_fixture_inv_swz[,c(1)])
  final_second_column_swz <- c(final_test_fixture_swz[,c(2)],final_test_fixture_inv_swz[,c(2)])

  mid_doublefixture_swz <- cbind(final_first_column_swz,final_second_column_swz)

  final_doublefixture_swz <- rbind(final_doublefixture_swz,mid_doublefixture_swz)


}
unlink('finaldoublefixtureswz.csv')
write.csv(final_doublefixture_swz,"finaldoublefixtureswz.csv")
############################################################################################################################






















