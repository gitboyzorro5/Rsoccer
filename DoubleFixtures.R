
library('xlsx')
library('lubridate')
#############################################################
myfixturesdiv <- readxl::read_excel('../FDAS/myodds_20222023.xlsx', sheet = '3way')
myfixturesdiv$Date <- dmy(myfixturesdiv$Date)
myfixturesdiv <- myfixturesdiv[myfixturesdiv$Date >= '2022-08-05',]

####################################################################
myfixturesdivb1 <- subset(myfixturesdiv,Div == "B1")
####################################################################
final_doublefixture_b1div <- c()
final_first_column_b1div <- c()
final_second_column_b1div <- c()

for(dbfixture_b1div in 1:nrow(myfixturesdivb1))
{
  test_fixture_b1div <- myfixturesdivb1[dbfixture_b1div,]
  test_fixture_inv_b1div <- test_fixture_b1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_b1div <- test_fixture_b1div[rep(seq_len(nrow(test_fixture_b1div)), each = 4),]
  final_test_fixture_inv_b1div <- test_fixture_inv_b1div[rep(seq_len(nrow(test_fixture_inv_b1div)), each = 4),]


  final_test_fixture_b1div <- final_test_fixture_b1div[,c(24,25)]
  final_test_fixture_inv_b1div <- final_test_fixture_inv_b1div[,c(24,25)]

  final_test_fixture_b1div <- as.data.frame(final_test_fixture_b1div)
  final_test_fixture_inv_b1div <- as.data.frame(final_test_fixture_inv_b1div)

  final_first_column_b1div <- c(final_test_fixture_b1div[,c(1)],final_test_fixture_inv_b1div[,c(1)])
  final_second_column_b1div <- c(final_test_fixture_b1div[,c(2)],final_test_fixture_inv_b1div[,c(2)])

  mid_doublefixture_b1div <- cbind(final_first_column_b1div,final_second_column_b1div)

  final_doublefixture_b1div <- rbind(final_doublefixture_b1div,mid_doublefixture_b1div)


}
unlink('finaldoublefixtureb1div.csv')
write.csv(final_doublefixture_b1div,"finaldoublefixtureb1div.csv")
############################################################################################################################
####################################################################
myfixturesdivd1 <- subset(myfixturesdiv,Div == "D1")
####################################################################
final_doublefixture_d1div <- c()
final_first_column_d1div <- c()
final_second_column_d1div <- c()

for(dbfixture_d1div in 1:nrow(myfixturesdivd1))
{
  test_fixture_d1div <- myfixturesdivd1[dbfixture_d1div,]
  test_fixture_inv_d1div <- test_fixture_d1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_d1div <- test_fixture_d1div[rep(seq_len(nrow(test_fixture_d1div)), each = 4),]
  final_test_fixture_inv_d1div <- test_fixture_inv_d1div[rep(seq_len(nrow(test_fixture_inv_d1div)), each = 4),]


  final_test_fixture_d1div <- final_test_fixture_d1div[,c(24,25)]
  final_test_fixture_inv_d1div <- final_test_fixture_inv_d1div[,c(24,25)]

  final_test_fixture_d1div <- as.data.frame(final_test_fixture_d1div)
  final_test_fixture_inv_d1div <- as.data.frame(final_test_fixture_inv_d1div)

  final_first_column_d1div <- c(final_test_fixture_d1div[,c(1)],final_test_fixture_inv_d1div[,c(1)])
  final_second_column_d1div <- c(final_test_fixture_d1div[,c(2)],final_test_fixture_inv_d1div[,c(2)])

  mid_doublefixture_d1div <- cbind(final_first_column_d1div,final_second_column_d1div)

  final_doublefixture_d1div <- rbind(final_doublefixture_d1div,mid_doublefixture_d1div)


}
unlink('finaldoublefixtured1div.csv')
write.csv(final_doublefixture_d1div,"finaldoublefixtured1div.csv")
############################################################################################################################
####################################################################
myfixturesdivd2 <- subset(myfixturesdiv,Div == "D2")
####################################################################
final_doublefixture_d2div <- c()
final_first_column_d2div <- c()
final_second_column_d2div <- c()

for(dbfixture_d2div in 1:nrow(myfixturesdivd2))
{
  test_fixture_d2div <- myfixturesdivd2[dbfixture_d2div,]
  test_fixture_inv_d2div <- test_fixture_d2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_d2div <- test_fixture_d2div[rep(seq_len(nrow(test_fixture_d2div)), each = 4),]
  final_test_fixture_inv_d2div <- test_fixture_inv_d2div[rep(seq_len(nrow(test_fixture_inv_d2div)), each = 4),]


  final_test_fixture_d2div <- final_test_fixture_d2div[,c(24,25)]
  final_test_fixture_inv_d2div <- final_test_fixture_inv_d2div[,c(24,25)]

  final_test_fixture_d2div <- as.data.frame(final_test_fixture_d2div)
  final_test_fixture_inv_d2div <- as.data.frame(final_test_fixture_inv_d2div)

  final_first_column_d2div <- c(final_test_fixture_d2div[,c(1)],final_test_fixture_inv_d2div[,c(1)])
  final_second_column_d2div <- c(final_test_fixture_d2div[,c(2)],final_test_fixture_inv_d2div[,c(2)])

  mid_doublefixture_d2div <- cbind(final_first_column_d2div,final_second_column_d2div)

  final_doublefixture_d2div <- rbind(final_doublefixture_d2div,mid_doublefixture_d2div)


}
unlink('finaldoublefixtured2div.csv')
write.csv(final_doublefixture_d2div,"finaldoublefixtured2div.csv")
############################################################################################################################
####################################################################
myfixturesdive0 <- subset(myfixturesdiv,Div == "E0")
####################################################################
final_doublefixture_e0div <- c()
final_first_column_e0div <- c()
final_second_column_e0div <- c()

for(dbfixture_e0div in 1:nrow(myfixturesdive0))
{
  test_fixture_e0div <- myfixturesdive0[dbfixture_e0div,]
  test_fixture_inv_e0div <- test_fixture_e0div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_e0div <- test_fixture_e0div[rep(seq_len(nrow(test_fixture_e0div)), each = 4),]
  final_test_fixture_inv_e0div <- test_fixture_inv_e0div[rep(seq_len(nrow(test_fixture_inv_e0div)), each = 4),]


  final_test_fixture_e0div <- final_test_fixture_e0div[,c(24,25)]
  final_test_fixture_inv_e0div <- final_test_fixture_inv_e0div[,c(24,25)]

  final_test_fixture_e0div <- as.data.frame(final_test_fixture_e0div)
  final_test_fixture_inv_e0div <- as.data.frame(final_test_fixture_inv_e0div)

  final_first_column_e0div <- c(final_test_fixture_e0div[,c(1)],final_test_fixture_inv_e0div[,c(1)])
  final_second_column_e0div <- c(final_test_fixture_e0div[,c(2)],final_test_fixture_inv_e0div[,c(2)])

  mid_doublefixture_e0div <- cbind(final_first_column_e0div,final_second_column_e0div)

  final_doublefixture_e0div <- rbind(final_doublefixture_e0div,mid_doublefixture_e0div)


}
unlink('finaldoublefixturee0div.csv')
write.csv(final_doublefixture_e0div,"finaldoublefixturee0div.csv")
############################################################################################################################
####################################################################
myfixturesdive1 <- subset(myfixturesdiv,Div == "E1")
####################################################################
final_doublefixture_e1div <- c()
final_first_column_e1div <- c()
final_second_column_e1div <- c()

for(dbfixture_e1div in 1:nrow(myfixturesdive1))
{
  test_fixture_e1div <- myfixturesdive1[dbfixture_e1div,]
  test_fixture_inv_e1div <- test_fixture_e1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_e1div <- test_fixture_e1div[rep(seq_len(nrow(test_fixture_e1div)), each = 4),]
  final_test_fixture_inv_e1div <- test_fixture_inv_e1div[rep(seq_len(nrow(test_fixture_inv_e1div)), each = 4),]


  final_test_fixture_e1div <- final_test_fixture_e1div[,c(24,25)]
  final_test_fixture_inv_e1div <- final_test_fixture_inv_e1div[,c(24,25)]

  final_test_fixture_e1div <- as.data.frame(final_test_fixture_e1div)
  final_test_fixture_inv_e1div <- as.data.frame(final_test_fixture_inv_e1div)

  final_first_column_e1div <- c(final_test_fixture_e1div[,c(1)],final_test_fixture_inv_e1div[,c(1)])
  final_second_column_e1div <- c(final_test_fixture_e1div[,c(2)],final_test_fixture_inv_e1div[,c(2)])

  mid_doublefixture_e1div <- cbind(final_first_column_e1div,final_second_column_e1div)

  final_doublefixture_e1div <- rbind(final_doublefixture_e1div,mid_doublefixture_e1div)


}
unlink('finaldoublefixturee1div.csv')
write.csv(final_doublefixture_e1div,"finaldoublefixturee1div.csv")
############################################################################################################################
####################################################################
myfixturesdive2 <- subset(myfixturesdiv,Div == "E2")
####################################################################
final_doublefixture_e2div <- c()
final_first_column_e2div <- c()
final_second_column_e2div <- c()

for(dbfixture_e2div in 1:nrow(myfixturesdive2))
{
  test_fixture_e2div <- myfixturesdive2[dbfixture_e2div,]
  test_fixture_inv_e2div <- test_fixture_e2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_e2div <- test_fixture_e2div[rep(seq_len(nrow(test_fixture_e2div)), each = 4),]
  final_test_fixture_inv_e2div <- test_fixture_inv_e2div[rep(seq_len(nrow(test_fixture_inv_e2div)), each = 4),]


  final_test_fixture_e2div <- final_test_fixture_e2div[,c(24,25)]
  final_test_fixture_inv_e2div <- final_test_fixture_inv_e2div[,c(24,25)]

  final_test_fixture_e2div <- as.data.frame(final_test_fixture_e2div)
  final_test_fixture_inv_e2div <- as.data.frame(final_test_fixture_inv_e2div)

  final_first_column_e2div <- c(final_test_fixture_e2div[,c(1)],final_test_fixture_inv_e2div[,c(1)])
  final_second_column_e2div <- c(final_test_fixture_e2div[,c(2)],final_test_fixture_inv_e2div[,c(2)])

  mid_doublefixture_e2div <- cbind(final_first_column_e2div,final_second_column_e2div)

  final_doublefixture_e2div <- rbind(final_doublefixture_e2div,mid_doublefixture_e2div)


}
unlink('finaldoublefixturee2div.csv')
write.csv(final_doublefixture_e2div,"finaldoublefixturee2div.csv")
############################################################################################################################
####################################################################
myfixturesdive3 <- subset(myfixturesdiv,Div == "E3")
####################################################################
final_doublefixture_e3div <- c()
final_first_column_e3div <- c()
final_second_column_e3div <- c()

for(dbfixture_e3div in 1:nrow(myfixturesdive3))
{
  test_fixture_e3div <- myfixturesdive3[dbfixture_e3div,]
  test_fixture_inv_e3div <- test_fixture_e3div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_e3div <- test_fixture_e3div[rep(seq_len(nrow(test_fixture_e3div)), each = 4),]
  final_test_fixture_inv_e3div <- test_fixture_inv_e3div[rep(seq_len(nrow(test_fixture_inv_e3div)), each = 4),]


  final_test_fixture_e3div <- final_test_fixture_e3div[,c(24,25)]
  final_test_fixture_inv_e3div <- final_test_fixture_inv_e3div[,c(24,25)]

  final_test_fixture_e3div <- as.data.frame(final_test_fixture_e3div)
  final_test_fixture_inv_e3div <- as.data.frame(final_test_fixture_inv_e3div)

  final_first_column_e3div <- c(final_test_fixture_e3div[,c(1)],final_test_fixture_inv_e3div[,c(1)])
  final_second_column_e3div <- c(final_test_fixture_e3div[,c(2)],final_test_fixture_inv_e3div[,c(2)])

  mid_doublefixture_e3div <- cbind(final_first_column_e3div,final_second_column_e3div)

  final_doublefixture_e3div <- rbind(final_doublefixture_e3div,mid_doublefixture_e3div)


}
unlink('finaldoublefixturee3div.csv')
write.csv(final_doublefixture_e3div,"finaldoublefixturee3div.csv")
############################################################################################################################
####################################################################
myfixturesdivec <- subset(myfixturesdiv,Div == "EC")
####################################################################
final_doublefixture_ecdiv <- c()
final_first_column_ecdiv <- c()
final_second_column_ecdiv <- c()

for(dbfixture_ecdiv in 1:nrow(myfixturesdivec))
{
  test_fixture_ecdiv <- myfixturesdivec[dbfixture_ecdiv,]
  test_fixture_inv_ecdiv <- test_fixture_ecdiv[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_ecdiv <- test_fixture_ecdiv[rep(seq_len(nrow(test_fixture_ecdiv)), each = 4),]
  final_test_fixture_inv_ecdiv <- test_fixture_inv_ecdiv[rep(seq_len(nrow(test_fixture_inv_ecdiv)), each = 4),]


  final_test_fixture_ecdiv <- final_test_fixture_ecdiv[,c(24,25)]
  final_test_fixture_inv_ecdiv <- final_test_fixture_inv_ecdiv[,c(24,25)]

  final_test_fixture_ecdiv <- as.data.frame(final_test_fixture_ecdiv)
  final_test_fixture_inv_ecdiv <- as.data.frame(final_test_fixture_inv_ecdiv)

  final_first_column_ecdiv <- c(final_test_fixture_ecdiv[,c(1)],final_test_fixture_inv_ecdiv[,c(1)])
  final_second_column_ecdiv <- c(final_test_fixture_ecdiv[,c(2)],final_test_fixture_inv_ecdiv[,c(2)])

  mid_doublefixture_ecdiv <- cbind(final_first_column_ecdiv,final_second_column_ecdiv)

  final_doublefixture_ecdiv <- rbind(final_doublefixture_ecdiv,mid_doublefixture_ecdiv)


}
unlink('finaldoublefixtureecdiv.csv')
write.csv(final_doublefixture_ecdiv,"finaldoublefixtureecdiv.csv")
############################################################################################################################
####################################################################
myfixturesdivf1 <- subset(myfixturesdiv,Div == "F1")
####################################################################
final_doublefixture_f1div <- c()
final_first_column_f1div <- c()
final_second_column_f1div <- c()

for(dbfixture_f1div in 1:nrow(myfixturesdivf1))
{
  test_fixture_f1div <- myfixturesdivf1[dbfixture_f1div,]
  test_fixture_inv_f1div <- test_fixture_f1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_f1div <- test_fixture_f1div[rep(seq_len(nrow(test_fixture_f1div)), each = 4),]
  final_test_fixture_inv_f1div <- test_fixture_inv_f1div[rep(seq_len(nrow(test_fixture_inv_f1div)), each = 4),]


  final_test_fixture_f1div <- final_test_fixture_f1div[,c(24,25)]
  final_test_fixture_inv_f1div <- final_test_fixture_inv_f1div[,c(24,25)]

  final_test_fixture_f1div <- as.data.frame(final_test_fixture_f1div)
  final_test_fixture_inv_f1div <- as.data.frame(final_test_fixture_inv_f1div)

  final_first_column_f1div <- c(final_test_fixture_f1div[,c(1)],final_test_fixture_inv_f1div[,c(1)])
  final_second_column_f1div <- c(final_test_fixture_f1div[,c(2)],final_test_fixture_inv_f1div[,c(2)])

  mid_doublefixture_f1div <- cbind(final_first_column_f1div,final_second_column_f1div)

  final_doublefixture_f1div <- rbind(final_doublefixture_f1div,mid_doublefixture_f1div)


}
unlink('finaldoublefixturef1div.csv')
write.csv(final_doublefixture_f1div,"finaldoublefixturef1div.csv")
############################################################################################################################
####################################################################
myfixturesdivf2 <- subset(myfixturesdiv,Div == "F2")
####################################################################
final_doublefixture_f2div <- c()
final_first_column_f2div <- c()
final_second_column_f2div <- c()

for(dbfixture_f2div in 1:nrow(myfixturesdivf2))
{
  test_fixture_f2div <- myfixturesdivf2[dbfixture_f2div,]
  test_fixture_inv_f2div <- test_fixture_f2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_f2div <- test_fixture_f2div[rep(seq_len(nrow(test_fixture_f2div)), each = 4),]
  final_test_fixture_inv_f2div <- test_fixture_inv_f2div[rep(seq_len(nrow(test_fixture_inv_f2div)), each = 4),]


  final_test_fixture_f2div <- final_test_fixture_f2div[,c(24,25)]
  final_test_fixture_inv_f2div <- final_test_fixture_inv_f2div[,c(24,25)]

  final_test_fixture_f2div <- as.data.frame(final_test_fixture_f2div)
  final_test_fixture_inv_f2div <- as.data.frame(final_test_fixture_inv_f2div)

  final_first_column_f2div <- c(final_test_fixture_f2div[,c(1)],final_test_fixture_inv_f2div[,c(1)])
  final_second_column_f2div <- c(final_test_fixture_f2div[,c(2)],final_test_fixture_inv_f2div[,c(2)])

  mid_doublefixture_f2div <- cbind(final_first_column_f2div,final_second_column_f2div)

  final_doublefixture_f2div <- rbind(final_doublefixture_f2div,mid_doublefixture_f2div)


}
unlink('finaldoublefixturef2div.csv')
write.csv(final_doublefixture_f2div,"finaldoublefixturef2div.csv")
############################################################################################################################
####################################################################
myfixturesdivg1 <- subset(myfixturesdiv,Div == "G1")
####################################################################
final_doublefixture_g1div <- c()
final_first_column_g1div <- c()
final_second_column_g1div <- c()

for(dbfixture_g1div in 1:nrow(myfixturesdivg1))
{
  test_fixture_g1div <- myfixturesdivg1[dbfixture_g1div,]
  test_fixture_inv_g1div <- test_fixture_g1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_g1div <- test_fixture_g1div[rep(seq_len(nrow(test_fixture_g1div)), each = 4),]
  final_test_fixture_inv_g1div <- test_fixture_inv_g1div[rep(seq_len(nrow(test_fixture_inv_g1div)), each = 4),]


  final_test_fixture_g1div <- final_test_fixture_g1div[,c(24,25)]
  final_test_fixture_inv_g1div <- final_test_fixture_inv_g1div[,c(24,25)]

  final_test_fixture_g1div <- as.data.frame(final_test_fixture_g1div)
  final_test_fixture_inv_g1div <- as.data.frame(final_test_fixture_inv_g1div)

  final_first_column_g1div <- c(final_test_fixture_g1div[,c(1)],final_test_fixture_inv_g1div[,c(1)])
  final_second_column_g1div <- c(final_test_fixture_g1div[,c(2)],final_test_fixture_inv_g1div[,c(2)])

  mid_doublefixture_g1div <- cbind(final_first_column_g1div,final_second_column_g1div)

  final_doublefixture_g1div <- rbind(final_doublefixture_g1div,mid_doublefixture_g1div)


}
unlink('finaldoublefixtureg1div.csv')
write.csv(final_doublefixture_g1div,"finaldoublefixtureg1div.csv")
############################################################################################################################
####################################################################
myfixturesdivi1 <- subset(myfixturesdiv,Div == "I1")
####################################################################
final_doublefixture_i1div <- c()
final_first_column_i1div <- c()
final_second_column_i1div <- c()

for(dbfixture_i1div in 1:nrow(myfixturesdivi1))
{
  test_fixture_i1div <- myfixturesdivi1[dbfixture_i1div,]
  test_fixture_inv_i1div <- test_fixture_i1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_i1div <- test_fixture_i1div[rep(seq_len(nrow(test_fixture_i1div)), each = 4),]
  final_test_fixture_inv_i1div <- test_fixture_inv_i1div[rep(seq_len(nrow(test_fixture_inv_i1div)), each = 4),]


  final_test_fixture_i1div <- final_test_fixture_i1div[,c(24,25)]
  final_test_fixture_inv_i1div <- final_test_fixture_inv_i1div[,c(24,25)]

  final_test_fixture_i1div <- as.data.frame(final_test_fixture_i1div)
  final_test_fixture_inv_i1div <- as.data.frame(final_test_fixture_inv_i1div)

  final_first_column_i1div <- c(final_test_fixture_i1div[,c(1)],final_test_fixture_inv_i1div[,c(1)])
  final_second_column_i1div <- c(final_test_fixture_i1div[,c(2)],final_test_fixture_inv_i1div[,c(2)])

  mid_doublefixture_i1div <- cbind(final_first_column_i1div,final_second_column_i1div)

  final_doublefixture_i1div <- rbind(final_doublefixture_i1div,mid_doublefixture_i1div)


}
unlink('finaldoublefixturei1div.csv')
write.csv(final_doublefixture_i1div,"finaldoublefixturei1div.csv")
############################################################################################################################
####################################################################
myfixturesdivi2 <- subset(myfixturesdiv,Div == "I2")
####################################################################
final_doublefixture_i2div <- c()
final_first_column_i2div <- c()
final_second_column_i2div <- c()

for(dbfixture_i2div in 1:nrow(myfixturesdivi2))
{
  test_fixture_i2div <- myfixturesdivi2[dbfixture_i2div,]
  test_fixture_inv_i2div <- test_fixture_i2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_i2div <- test_fixture_i2div[rep(seq_len(nrow(test_fixture_i2div)), each = 4),]
  final_test_fixture_inv_i2div <- test_fixture_inv_i2div[rep(seq_len(nrow(test_fixture_inv_i2div)), each = 4),]


  final_test_fixture_i2div <- final_test_fixture_i2div[,c(24,25)]
  final_test_fixture_inv_i2div <- final_test_fixture_inv_i2div[,c(24,25)]

  final_test_fixture_i2div <- as.data.frame(final_test_fixture_i2div)
  final_test_fixture_inv_i2div <- as.data.frame(final_test_fixture_inv_i2div)

  final_first_column_i2div <- c(final_test_fixture_i2div[,c(1)],final_test_fixture_inv_i2div[,c(1)])
  final_second_column_i2div <- c(final_test_fixture_i2div[,c(2)],final_test_fixture_inv_i2div[,c(2)])

  mid_doublefixture_i2div <- cbind(final_first_column_i2div,final_second_column_i2div)

  final_doublefixture_i2div <- rbind(final_doublefixture_i2div,mid_doublefixture_i2div)


}
unlink('finaldoublefixturei2div.csv')
write.csv(final_doublefixture_i2div,"finaldoublefixturei2div.csv")
############################################################################################################################
####################################################################
myfixturesdivn1 <- subset(myfixturesdiv,Div == "N1")
####################################################################
final_doublefixture_n1div <- c()
final_first_column_n1div <- c()
final_second_column_n1div <- c()

for(dbfixture_n1div in 1:nrow(myfixturesdivn1))
{
  test_fixture_n1div <- myfixturesdivn1[dbfixture_n1div,]
  test_fixture_inv_n1div <- test_fixture_n1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_n1div <- test_fixture_n1div[rep(seq_len(nrow(test_fixture_n1div)), each = 4),]
  final_test_fixture_inv_n1div <- test_fixture_inv_n1div[rep(seq_len(nrow(test_fixture_inv_n1div)), each = 4),]


  final_test_fixture_n1div <- final_test_fixture_n1div[,c(24,25)]
  final_test_fixture_inv_n1div <- final_test_fixture_inv_n1div[,c(24,25)]

  final_test_fixture_n1div <- as.data.frame(final_test_fixture_n1div)
  final_test_fixture_inv_n1div <- as.data.frame(final_test_fixture_inv_n1div)

  final_first_column_n1div <- c(final_test_fixture_n1div[,c(1)],final_test_fixture_inv_n1div[,c(1)])
  final_second_column_n1div <- c(final_test_fixture_n1div[,c(2)],final_test_fixture_inv_n1div[,c(2)])

  mid_doublefixture_n1div <- cbind(final_first_column_n1div,final_second_column_n1div)

  final_doublefixture_n1div <- rbind(final_doublefixture_n1div,mid_doublefixture_n1div)


}
unlink('finaldoublefixturen1div.csv')
write.csv(final_doublefixture_n1div,"finaldoublefixturen1div.csv")
############################################################################################################################
####################################################################
myfixturesdivp1 <- subset(myfixturesdiv,Div == "P1")
####################################################################
final_doublefixture_p1div <- c()
final_first_column_p1div <- c()
final_second_column_p1div <- c()

for(dbfixture_p1div in 1:nrow(myfixturesdivp1))
{
  test_fixture_p1div <- myfixturesdivp1[dbfixture_p1div,]
  test_fixture_inv_p1div <- test_fixture_p1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_p1div <- test_fixture_p1div[rep(seq_len(nrow(test_fixture_p1div)), each = 4),]
  final_test_fixture_inv_p1div <- test_fixture_inv_p1div[rep(seq_len(nrow(test_fixture_inv_p1div)), each = 4),]


  final_test_fixture_p1div <- final_test_fixture_p1div[,c(24,25)]
  final_test_fixture_inv_p1div <- final_test_fixture_inv_p1div[,c(24,25)]

  final_test_fixture_p1div <- as.data.frame(final_test_fixture_p1div)
  final_test_fixture_inv_p1div <- as.data.frame(final_test_fixture_inv_p1div)

  final_first_column_p1div <- c(final_test_fixture_p1div[,c(1)],final_test_fixture_inv_p1div[,c(1)])
  final_second_column_p1div <- c(final_test_fixture_p1div[,c(2)],final_test_fixture_inv_p1div[,c(2)])

  mid_doublefixture_p1div <- cbind(final_first_column_p1div,final_second_column_p1div)

  final_doublefixture_p1div <- rbind(final_doublefixture_p1div,mid_doublefixture_p1div)


}
unlink('finaldoublefixturep1div.csv')
write.csv(final_doublefixture_p1div,"finaldoublefixturep1div.csv")
############################################################################################################################
####################################################################
myfixturesdivsc0 <- subset(myfixturesdiv,Div == "SC0")
####################################################################
final_doublefixture_sc0div <- c()
final_first_column_sc0div <- c()
final_second_column_sc0div <- c()

for(dbfixture_sc0div in 1:nrow(myfixturesdivsc0))
{
  test_fixture_sc0div <- myfixturesdivsc0[dbfixture_sc0div,]
  test_fixture_inv_sc0div <- test_fixture_sc0div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sc0div <- test_fixture_sc0div[rep(seq_len(nrow(test_fixture_sc0div)), each = 4),]
  final_test_fixture_inv_sc0div <- test_fixture_inv_sc0div[rep(seq_len(nrow(test_fixture_inv_sc0div)), each = 4),]


  final_test_fixture_sc0div <- final_test_fixture_sc0div[,c(24,25)]
  final_test_fixture_inv_sc0div <- final_test_fixture_inv_sc0div[,c(24,25)]

  final_test_fixture_sc0div <- as.data.frame(final_test_fixture_sc0div)
  final_test_fixture_inv_sc0div <- as.data.frame(final_test_fixture_inv_sc0div)

  final_first_column_sc0div <- c(final_test_fixture_sc0div[,c(1)],final_test_fixture_inv_sc0div[,c(1)])
  final_second_column_sc0div <- c(final_test_fixture_sc0div[,c(2)],final_test_fixture_inv_sc0div[,c(2)])

  mid_doublefixture_sc0div <- cbind(final_first_column_sc0div,final_second_column_sc0div)

  final_doublefixture_sc0div <- rbind(final_doublefixture_sc0div,mid_doublefixture_sc0div)


}
unlink('finaldoublefixturesc0div.csv')
write.csv(final_doublefixture_sc0div,"finaldoublefixturesc0div.csv")
############################################################################################################################
####################################################################
myfixturesdivsc1 <- subset(myfixturesdiv,Div == "SC1")
####################################################################
final_doublefixture_sc1div <- c()
final_first_column_sc1div <- c()
final_second_column_sc1div <- c()

for(dbfixture_sc1div in 1:nrow(myfixturesdivsc1))
{
  test_fixture_sc1div <- myfixturesdivsc1[dbfixture_sc1div,]
  test_fixture_inv_sc1div <- test_fixture_sc1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sc1div <- test_fixture_sc1div[rep(seq_len(nrow(test_fixture_sc1div)), each = 4),]
  final_test_fixture_inv_sc1div <- test_fixture_inv_sc1div[rep(seq_len(nrow(test_fixture_inv_sc1div)), each = 4),]


  final_test_fixture_sc1div <- final_test_fixture_sc1div[,c(24,25)]
  final_test_fixture_inv_sc1div <- final_test_fixture_inv_sc1div[,c(24,25)]

  final_test_fixture_sc1div <- as.data.frame(final_test_fixture_sc1div)
  final_test_fixture_inv_sc1div <- as.data.frame(final_test_fixture_inv_sc1div)

  final_first_column_sc1div <- c(final_test_fixture_sc1div[,c(1)],final_test_fixture_inv_sc1div[,c(1)])
  final_second_column_sc1div <- c(final_test_fixture_sc1div[,c(2)],final_test_fixture_inv_sc1div[,c(2)])

  mid_doublefixture_sc1div <- cbind(final_first_column_sc1div,final_second_column_sc1div)

  final_doublefixture_sc1div <- rbind(final_doublefixture_sc1div,mid_doublefixture_sc1div)


}
unlink('finaldoublefixturesc1div.csv')
write.csv(final_doublefixture_sc1div,"finaldoublefixturesc1div.csv")
############################################################################################################################
####################################################################
myfixturesdivsc2 <- subset(myfixturesdiv,Div == "SC2")
####################################################################
final_doublefixture_sc2div <- c()
final_first_column_sc2div <- c()
final_second_column_sc2div <- c()

for(dbfixture_sc2div in 1:nrow(myfixturesdivsc2))
{
  test_fixture_sc2div <- myfixturesdivsc2[dbfixture_sc2div,]
  test_fixture_inv_sc2div <- test_fixture_sc2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sc2div <- test_fixture_sc2div[rep(seq_len(nrow(test_fixture_sc2div)), each = 4),]
  final_test_fixture_inv_sc2div <- test_fixture_inv_sc2div[rep(seq_len(nrow(test_fixture_inv_sc2div)), each = 4),]


  final_test_fixture_sc2div <- final_test_fixture_sc2div[,c(24,25)]
  final_test_fixture_inv_sc2div <- final_test_fixture_inv_sc2div[,c(24,25)]

  final_test_fixture_sc2div <- as.data.frame(final_test_fixture_sc2div)
  final_test_fixture_inv_sc2div <- as.data.frame(final_test_fixture_inv_sc2div)

  final_first_column_sc2div <- c(final_test_fixture_sc2div[,c(1)],final_test_fixture_inv_sc2div[,c(1)])
  final_second_column_sc2div <- c(final_test_fixture_sc2div[,c(2)],final_test_fixture_inv_sc2div[,c(2)])

  mid_doublefixture_sc2div <- cbind(final_first_column_sc2div,final_second_column_sc2div)

  final_doublefixture_sc2div <- rbind(final_doublefixture_sc2div,mid_doublefixture_sc2div)


}
unlink('finaldoublefixturesc2div.csv')
write.csv(final_doublefixture_sc2div,"finaldoublefixturesc2div.csv")
############################################################################################################################
####################################################################
myfixturesdivsc3 <- subset(myfixturesdiv,Div == "SC3")
####################################################################
final_doublefixture_sc3div <- c()
final_first_column_sc3div <- c()
final_second_column_sc3div <- c()

for(dbfixture_sc3div in 1:nrow(myfixturesdivsc3))
{
  test_fixture_sc3div <- myfixturesdivsc3[dbfixture_sc3div,]
  test_fixture_inv_sc3div <- test_fixture_sc3div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sc3div <- test_fixture_sc3div[rep(seq_len(nrow(test_fixture_sc3div)), each = 4),]
  final_test_fixture_inv_sc3div <- test_fixture_inv_sc3div[rep(seq_len(nrow(test_fixture_inv_sc3div)), each = 4),]


  final_test_fixture_sc3div <- final_test_fixture_sc3div[,c(24,25)]
  final_test_fixture_inv_sc3div <- final_test_fixture_inv_sc3div[,c(24,25)]

  final_test_fixture_sc3div <- as.data.frame(final_test_fixture_sc3div)
  final_test_fixture_inv_sc3div <- as.data.frame(final_test_fixture_inv_sc3div)

  final_first_column_sc3div <- c(final_test_fixture_sc3div[,c(1)],final_test_fixture_inv_sc3div[,c(1)])
  final_second_column_sc3div <- c(final_test_fixture_sc3div[,c(2)],final_test_fixture_inv_sc3div[,c(2)])

  mid_doublefixture_sc3div <- cbind(final_first_column_sc3div,final_second_column_sc3div)

  final_doublefixture_sc3div <- rbind(final_doublefixture_sc3div,mid_doublefixture_sc3div)


}
unlink('finaldoublefixturesc3div.csv')
write.csv(final_doublefixture_sc3div,"finaldoublefixturesc3div.csv")
############################################################################################################################
####################################################################
myfixturesdivsp1 <- subset(myfixturesdiv,Div == "SP1")
####################################################################
final_doublefixture_sp1div <- c()
final_first_column_sp1div <- c()
final_second_column_sp1div <- c()

for(dbfixture_sp1div in 1:nrow(myfixturesdivsp1))
{
  test_fixture_sp1div <- myfixturesdivsp1[dbfixture_sp1div,]
  test_fixture_inv_sp1div <- test_fixture_sp1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sp1div <- test_fixture_sp1div[rep(seq_len(nrow(test_fixture_sp1div)), each = 4),]
  final_test_fixture_inv_sp1div <- test_fixture_inv_sp1div[rep(seq_len(nrow(test_fixture_inv_sp1div)), each = 4),]


  final_test_fixture_sp1div <- final_test_fixture_sp1div[,c(24,25)]
  final_test_fixture_inv_sp1div <- final_test_fixture_inv_sp1div[,c(24,25)]

  final_test_fixture_sp1div <- as.data.frame(final_test_fixture_sp1div)
  final_test_fixture_inv_sp1div <- as.data.frame(final_test_fixture_inv_sp1div)

  final_first_column_sp1div <- c(final_test_fixture_sp1div[,c(1)],final_test_fixture_inv_sp1div[,c(1)])
  final_second_column_sp1div <- c(final_test_fixture_sp1div[,c(2)],final_test_fixture_inv_sp1div[,c(2)])

  mid_doublefixture_sp1div <- cbind(final_first_column_sp1div,final_second_column_sp1div)

  final_doublefixture_sp1div <- rbind(final_doublefixture_sp1div,mid_doublefixture_sp1div)


}
unlink('finaldoublefixturesp1div.csv')
write.csv(final_doublefixture_sp1div,"finaldoublefixturesp1div.csv")
############################################################################################################################
####################################################################
myfixturesdivsp2 <- subset(myfixturesdiv,Div == "SP2")
####################################################################
final_doublefixture_sp2div <- c()
final_first_column_sp2div <- c()
final_second_column_sp2div <- c()

for(dbfixture_sp2div in 1:nrow(myfixturesdivsp2))
{
  test_fixture_sp2div <- myfixturesdivsp2[dbfixture_sp2div,]
  test_fixture_inv_sp2div <- test_fixture_sp2div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_sp2div <- test_fixture_sp2div[rep(seq_len(nrow(test_fixture_sp2div)), each = 4),]
  final_test_fixture_inv_sp2div <- test_fixture_inv_sp2div[rep(seq_len(nrow(test_fixture_inv_sp2div)), each = 4),]


  final_test_fixture_sp2div <- final_test_fixture_sp2div[,c(24,25)]
  final_test_fixture_inv_sp2div <- final_test_fixture_inv_sp2div[,c(24,25)]

  final_test_fixture_sp2div <- as.data.frame(final_test_fixture_sp2div)
  final_test_fixture_inv_sp2div <- as.data.frame(final_test_fixture_inv_sp2div)

  final_first_column_sp2div <- c(final_test_fixture_sp2div[,c(1)],final_test_fixture_inv_sp2div[,c(1)])
  final_second_column_sp2div <- c(final_test_fixture_sp2div[,c(2)],final_test_fixture_inv_sp2div[,c(2)])

  mid_doublefixture_sp2div <- cbind(final_first_column_sp2div,final_second_column_sp2div)

  final_doublefixture_sp2div <- rbind(final_doublefixture_sp2div,mid_doublefixture_sp2div)


}
unlink('finaldoublefixturesp2div.csv')
write.csv(final_doublefixture_sp2div,"finaldoublefixturesp2div.csv")
############################################################################################################################
####################################################################
myfixturesdivt1 <- subset(myfixturesdiv,Div == "T1")
####################################################################
final_doublefixture_t1div <- c()
final_first_column_t1div <- c()
final_second_column_t1div <- c()

for(dbfixture_t1div in 1:nrow(myfixturesdivt1))
{
  test_fixture_t1div <- myfixturesdivt1[dbfixture_t1div,]
  test_fixture_inv_t1div <- test_fixture_t1div[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,24,26,27,28,29,30,31,32,33)]

  final_test_fixture_t1div <- test_fixture_t1div[rep(seq_len(nrow(test_fixture_t1div)), each = 4),]
  final_test_fixture_inv_t1div <- test_fixture_inv_t1div[rep(seq_len(nrow(test_fixture_inv_t1div)), each = 4),]


  final_test_fixture_t1div <- final_test_fixture_t1div[,c(24,25)]
  final_test_fixture_inv_t1div <- final_test_fixture_inv_t1div[,c(24,25)]

  final_test_fixture_t1div <- as.data.frame(final_test_fixture_t1div)
  final_test_fixture_inv_t1div <- as.data.frame(final_test_fixture_inv_t1div)

  final_first_column_t1div <- c(final_test_fixture_t1div[,c(1)],final_test_fixture_inv_t1div[,c(1)])
  final_second_column_t1div <- c(final_test_fixture_t1div[,c(2)],final_test_fixture_inv_t1div[,c(2)])

  mid_doublefixture_t1div <- cbind(final_first_column_t1div,final_second_column_t1div)

  final_doublefixture_t1div <- rbind(final_doublefixture_t1div,mid_doublefixture_t1div)


}
unlink('finaldoublefixturet1div.csv')
write.csv(final_doublefixture_t1div,"finaldoublefixturet1div.csv")
############################################################################################################################












































