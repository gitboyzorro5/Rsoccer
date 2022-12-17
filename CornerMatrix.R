#create Goals Scored form since start of season
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
library('xlsx')
#tgv2 <- with(B1, tapply(TG, list(HomeTeam, AwayTeam), FUN = mean))
#with(sorted_B1, tapply(TG, list(HomeTeam, Date), FUN = mean))
#awayteam_tg <- with(sorted_B1, tapply(TG, list(AwayTeam, Date), FUN = mean))

#create home and away matrices
b1_corners_h <- tapply(B1$HCO, B1[c("HomeTeam", "Date")],mean)
b1_corners_a <- tapply(B1$ACO, B1[c("AwayTeam", "Date")],mean)
d1_corners_h <- tapply(D1$HCO, D1[c("HomeTeam", "Date")],mean)
d1_corners_a <- tapply(D1$ACO, D1[c("AwayTeam", "Date")],mean)
d2_corners_h <- tapply(D2$HCO, D2[c("HomeTeam", "Date")],mean)
d2_corners_a <- tapply(D2$ACO, D2[c("AwayTeam", "Date")],mean)
e0_corners_h <- tapply(E0$HCO, E0[c("HomeTeam", "Date")],mean)
e0_corners_a <- tapply(E0$ACO, E0[c("AwayTeam", "Date")],mean)
e1_corners_h <- tapply(E1$HCO, E1[c("HomeTeam", "Date")],mean)
e1_corners_a <- tapply(E1$ACO, E1[c("AwayTeam", "Date")],mean)
e2_corners_h <- tapply(E2$HCO, E2[c("HomeTeam", "Date")],mean)
e2_corners_a <- tapply(E2$ACO, E2[c("AwayTeam", "Date")],mean)
e3_corners_h <- tapply(E3$HCO, E3[c("HomeTeam", "Date")],mean)
e3_corners_a <- tapply(E3$ACO, E3[c("AwayTeam", "Date")],mean)
ec_corners_h <- tapply(EC$HCO, EC[c("HomeTeam", "Date")],mean)
ec_corners_a <- tapply(EC$ACO, EC[c("AwayTeam", "Date")],mean)
f1_corners_h <- tapply(F1$HCO, F1[c("HomeTeam", "Date")],mean)
f1_corners_a <- tapply(F1$ACO, F1[c("AwayTeam", "Date")],mean)
f2_corners_h <- tapply(F2$HCO, F2[c("HomeTeam", "Date")],mean)
f2_corners_a <- tapply(F2$ACO, F2[c("AwayTeam", "Date")],mean)
g1_corners_h <- tapply(G1$HCO, G1[c("HomeTeam", "Date")],mean)
g1_corners_a <- tapply(G1$ACO, G1[c("AwayTeam", "Date")],mean)
i1_corners_h <- tapply(I1$HCO, I1[c("HomeTeam", "Date")],mean)
i1_corners_a <- tapply(I1$ACO, I1[c("AwayTeam", "Date")],mean)
i2_corners_h <- tapply(I2$HCO, I2[c("HomeTeam", "Date")],mean)
i2_corners_a <- tapply(I2$ACO, I2[c("AwayTeam", "Date")],mean)
n1_corners_h <- tapply(N1$HCO, N1[c("HomeTeam", "Date")],mean)
n1_corners_a <- tapply(N1$ACO, N1[c("AwayTeam", "Date")],mean)
p1_corners_h <- tapply(P1$HCO, P1[c("HomeTeam", "Date")],mean)
p1_corners_a <- tapply(P1$ACO, P1[c("AwayTeam", "Date")],mean)
sc0_corners_h <- tapply(SC0$HCO, SC0[c("HomeTeam", "Date")],mean)
sc0_corners_a <- tapply(SC0$ACO, SC0[c("AwayTeam", "Date")],mean)
sc1_corners_h <- tapply(SC1$HCO, SC1[c("HomeTeam", "Date")],mean)
sc1_corners_a <- tapply(SC1$ACO, SC1[c("AwayTeam", "Date")],mean)
sc2_corners_h <- tapply(SC2$HCO, SC2[c("HomeTeam", "Date")],mean)
sc2_corners_a <- tapply(SC2$ACO, SC2[c("AwayTeam", "Date")],mean)
sc3_corners_h <- tapply(SC3$HCO, SC3[c("HomeTeam", "Date")],mean)
sc3_corners_a <- tapply(SC3$ACO, SC3[c("AwayTeam", "Date")],mean)
sp1_corners_h <- tapply(SP1$HCO, SP1[c("HomeTeam", "Date")],mean)
sp1_corners_a <- tapply(SP1$ACO, SP1[c("AwayTeam", "Date")],mean)
sp2_corners_h <- tapply(SP2$HCO, SP2[c("HomeTeam", "Date")],mean)
sp2_corners_a <- tapply(SP2$ACO, SP2[c("AwayTeam", "Date")],mean)
t1_corners_h <- tapply(T1$HCO, T1[c("HomeTeam", "Date")],mean)
t1_corners_a <- tapply(T1$ACO, T1[c("AwayTeam", "Date")],mean)
#remove na values

b1_corners_h[is.na(b1_corners_h)] <- ""
b1_corners_a[is.na(b1_corners_a)] <- ""
d1_corners_h[is.na(d1_corners_h)] <- ""
d1_corners_a[is.na(d1_corners_a)] <- ""
d2_corners_h[is.na(d2_corners_h)] <- ""
d2_corners_a[is.na(d2_corners_a)] <- ""
e0_corners_h[is.na(e0_corners_h)] <- ""
e0_corners_a[is.na(e0_corners_a)] <- ""
e1_corners_h[is.na(e1_corners_h)] <- ""
e1_corners_a[is.na(e1_corners_a)] <- ""
e2_corners_h[is.na(e2_corners_h)] <- ""
e2_corners_a[is.na(e2_corners_a)] <- ""
e3_corners_h[is.na(e3_corners_h)] <- ""
e3_corners_a[is.na(e3_corners_a)] <- ""
ec_corners_h[is.na(ec_corners_h)] <- ""
ec_corners_a[is.na(ec_corners_a)] <- ""
f1_corners_h[is.na(f1_corners_h)] <- ""
f1_corners_a[is.na(f1_corners_a)] <- ""
f2_corners_h[is.na(f2_corners_h)] <- ""
f2_corners_a[is.na(f2_corners_a)] <- ""
g1_corners_h[is.na(g1_corners_h)] <- ""
g1_corners_a[is.na(g1_corners_a)] <- ""
i1_corners_h[is.na(i1_corners_h)] <- ""
i1_corners_a[is.na(i1_corners_a)] <- ""
i2_corners_h[is.na(i2_corners_h)] <- ""
i2_corners_a[is.na(i2_corners_a)] <- ""
n1_corners_h[is.na(n1_corners_h)] <- ""
n1_corners_a[is.na(n1_corners_a)] <- ""
p1_corners_h[is.na(p1_corners_h)] <- ""
p1_corners_a[is.na(p1_corners_a)] <- ""
sc0_corners_h[is.na(sc0_corners_h)] <- ""
sc0_corners_a[is.na(sc0_corners_a)] <- ""
sc1_corners_h[is.na(sc1_corners_h)] <- ""
sc1_corners_a[is.na(sc1_corners_a)] <- ""
sc2_corners_h[is.na(sc2_corners_h)] <- ""
sc2_corners_a[is.na(sc2_corners_a)] <- ""
sc3_corners_h[is.na(sc3_corners_h)] <- ""
sc3_corners_a[is.na(sc3_corners_a)] <- ""
sp1_corners_h[is.na(sp1_corners_h)] <- ""
sp1_corners_a[is.na(sp1_corners_a)] <- ""
sp2_corners_h[is.na(sp2_corners_h)] <- ""
sp2_corners_a[is.na(sp2_corners_a)] <- ""
t1_corners_h[is.na(t1_corners_h)] <- ""
t1_corners_a[is.na(t1_corners_a)] <- ""
#combine the matrices
#B1
for(b1_rowhco in 1:nrow(b1_corners_h)) {
  for(b1_colhco in 1:ncol(b1_corners_h)) {

    # print(my_matrix[row, col])
    for(b1_rowaco in 1:nrow(b1_corners_a)) {
      for(b1_colaco in 1:ncol(b1_corners_a)) {
        ifelse(!b1_corners_a[b1_rowaco,b1_colaco]=="",b1_corners_h[b1_rowaco,b1_colaco] <- b1_corners_a[b1_rowaco,b1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#D1
for(d1_rowhco in 1:nrow(d1_corners_h)) {
  for(d1_colhco in 1:ncol(d1_corners_h)) {

    # print(my_matrix[row, col])
    for(d1_rowaco in 1:nrow(d1_corners_a)) {
      for(d1_colaco in 1:ncol(d1_corners_a)) {
        ifelse(!d1_corners_a[d1_rowaco,d1_colaco]=="",d1_corners_h[d1_rowaco,d1_colaco] <- d1_corners_a[d1_rowaco,d1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#D2
for(d2_rowhco in 1:nrow(d2_corners_h)) {
  for(d2_colhco in 1:ncol(d2_corners_h)) {

    # print(my_matrix[row, col])
    for(d2_rowaco in 1:nrow(d2_corners_a)) {
      for(d2_colaco in 1:ncol(d2_corners_a)) {
        ifelse(!d2_corners_a[d2_rowaco,d2_colaco]=="",d2_corners_h[d2_rowaco,d2_colaco] <- d2_corners_a[d2_rowaco,d2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#E0
for(e0_rowhco in 1:nrow(e0_corners_h)) {
  for(e0_colhco in 1:ncol(e0_corners_h)) {

    # print(my_matrix[row, col])
    for(e0_rowaco in 1:nrow(e0_corners_a)) {
      for(e0_colaco in 1:ncol(e0_corners_a)) {
        ifelse(!e0_corners_a[e0_rowaco,e0_colaco]=="",e0_corners_h[e0_rowaco,e0_colaco] <- e0_corners_a[e0_rowaco,e0_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#E1
for(e1_rowhco in 1:nrow(e1_corners_h)) {
  for(e1_colhco in 1:ncol(e1_corners_h)) {

    # print(my_matrix[row, col])
    for(e1_rowaco in 1:nrow(e1_corners_a)) {
      for(e1_colaco in 1:ncol(e1_corners_a)) {
        ifelse(!e1_corners_a[e1_rowaco,e1_colaco]=="",e1_corners_h[e1_rowaco,e1_colaco] <- e1_corners_a[e1_rowaco,e1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#E2
for(e2_rowhco in 1:nrow(e2_corners_h)) {
  for(e2_colhco in 1:ncol(e2_corners_h)) {

    # print(my_matrix[row, col])
    for(e2_rowaco in 1:nrow(e2_corners_a)) {
      for(e2_colaco in 1:ncol(e2_corners_a)) {
        ifelse(!e2_corners_a[e2_rowaco,e2_colaco]=="",e2_corners_h[e2_rowaco,e2_colaco] <- e2_corners_a[e2_rowaco,e2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#E3
for(e3_rowhco in 1:nrow(e3_corners_h)) {
  for(e3_colhco in 1:ncol(e3_corners_h)) {

    # print(my_matrix[row, col])
    for(e3_rowaco in 1:nrow(e3_corners_a)) {
      for(e3_colaco in 1:ncol(e3_corners_a)) {
        ifelse(!e3_corners_a[e3_rowaco,e3_colaco]=="",e3_corners_h[e3_rowaco,e3_colaco] <- e3_corners_a[e3_rowaco,e3_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#EC
for(ec_rowhco in 1:nrow(ec_corners_h)) {
  for(ec_colhco in 1:ncol(ec_corners_h)) {

    # print(my_matrix[row, col])
    for(ec_rowaco in 1:nrow(ec_corners_a)) {
      for(ec_colaco in 1:ncol(ec_corners_a)) {
        ifelse(!ec_corners_a[ec_rowaco,ec_colaco]=="",ec_corners_h[ec_rowaco,ec_colaco] <- ec_corners_a[ec_rowaco,ec_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#F1
for(f1_rowhco in 1:nrow(f1_corners_h)) {
  for(f1_colhco in 1:ncol(f1_corners_h)) {

    # print(my_matrix[row, col])
    for(f1_rowaco in 1:nrow(f1_corners_a)) {
      for(f1_colaco in 1:ncol(f1_corners_a)) {
        ifelse(!f1_corners_a[f1_rowaco,f1_colaco]=="",f1_corners_h[f1_rowaco,f1_colaco] <- f1_corners_a[f1_rowaco,f1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#F2
for(f2_rowhco in 1:nrow(f2_corners_h)) {
  for(f2_colhco in 1:ncol(f2_corners_h)) {

    # print(my_matrix[row, col])
    for(f2_rowaco in 1:nrow(f2_corners_a)) {
      for(f2_colaco in 1:ncol(f2_corners_a)) {
        ifelse(!f2_corners_a[f2_rowaco,f2_colaco]=="",f2_corners_h[f2_rowaco,f2_colaco] <- f2_corners_a[f2_rowaco,f2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#G1
for(g1_rowhco in 1:nrow(g1_corners_h)) {
  for(g1_colhco in 1:ncol(g1_corners_h)) {

    # print(my_matrix[row, col])
    for(g1_rowaco in 1:nrow(g1_corners_a)) {
      for(g1_colaco in 1:ncol(g1_corners_a)) {
        ifelse(!g1_corners_a[g1_rowaco,g1_colaco]=="",g1_corners_h[g1_rowaco,g1_colaco] <- g1_corners_a[g1_rowaco,g1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#I1
for(i1_rowhco in 1:nrow(i1_corners_h)) {
  for(i1_colhco in 1:ncol(i1_corners_h)) {

    # print(my_matrix[row, col])
    for(i1_rowaco in 1:nrow(i1_corners_a)) {
      for(i1_colaco in 1:ncol(i1_corners_a)) {
        ifelse(!i1_corners_a[i1_rowaco,i1_colaco]=="",i1_corners_h[i1_rowaco,i1_colaco] <- i1_corners_a[i1_rowaco,i1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#I2
for(i2_rowhco in 1:nrow(i2_corners_h)) {
  for(i2_colhco in 1:ncol(i2_corners_h)) {

    # print(my_matrix[row, col])
    for(i2_rowaco in 1:nrow(i2_corners_a)) {
      for(i2_colaco in 1:ncol(i2_corners_a)) {
        ifelse(!i2_corners_a[i2_rowaco,i2_colaco]=="",i2_corners_h[i2_rowaco,i2_colaco] <- i2_corners_a[i2_rowaco,i2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#N1
for(n1_rowhco in 1:nrow(n1_corners_h)) {
  for(n1_colhco in 1:ncol(n1_corners_h)) {

    # print(my_matrix[row, col])
    for(n1_rowaco in 1:nrow(n1_corners_a)) {
      for(n1_colaco in 1:ncol(n1_corners_a)) {
        ifelse(!n1_corners_a[n1_rowaco,n1_colaco]=="",n1_corners_h[n1_rowaco,n1_colaco] <- n1_corners_a[n1_rowaco,n1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#P1
for(p1_rowhco in 1:nrow(p1_corners_h)) {
  for(p1_colhco in 1:ncol(p1_corners_h)) {

    # print(my_matrix[row, col])
    for(p1_rowaco in 1:nrow(p1_corners_a)) {
      for(p1_colaco in 1:ncol(p1_corners_a)) {
        ifelse(!p1_corners_a[p1_rowaco,p1_colaco]=="",p1_corners_h[p1_rowaco,p1_colaco] <- p1_corners_a[p1_rowaco,p1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SC0
for(sc0_rowhco in 1:nrow(sc0_corners_h)) {
  for(sc0_colhco in 1:ncol(sc0_corners_h)) {

    # print(my_matrix[row, col])
    for(sc0_rowaco in 1:nrow(sc0_corners_a)) {
      for(sc0_colaco in 1:ncol(sc0_corners_a)) {
        ifelse(!sc0_corners_a[sc0_rowaco,sc0_colaco]=="",sc0_corners_h[sc0_rowaco,sc0_colaco] <- sc0_corners_a[sc0_rowaco,sc0_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SC1
for(sc1_rowhco in 1:nrow(sc1_corners_h)) {
  for(sc1_colhco in 1:ncol(sc1_corners_h)) {

    # print(my_matrix[row, col])
    for(sc1_rowaco in 1:nrow(sc1_corners_a)) {
      for(sc1_colaco in 1:ncol(sc1_corners_a)) {
        ifelse(!sc1_corners_a[sc1_rowaco,sc1_colaco]=="",sc1_corners_h[sc1_rowaco,sc1_colaco] <- sc1_corners_a[sc1_rowaco,sc1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SC2
for(sc2_rowhco in 1:nrow(sc2_corners_h)) {
  for(sc2_colhco in 1:ncol(sc2_corners_h)) {

    # print(my_matrix[row, col])
    for(sc2_rowaco in 1:nrow(sc2_corners_a)) {
      for(sc2_colaco in 1:ncol(sc2_corners_a)) {
        ifelse(!sc2_corners_a[sc2_rowaco,sc2_colaco]=="",sc2_corners_h[sc2_rowaco,sc2_colaco] <- sc2_corners_a[sc2_rowaco,sc2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SC3
for(sc3_rowhco in 1:nrow(sc3_corners_h)) {
  for(sc3_colhco in 1:ncol(sc3_corners_h)) {

    # print(my_matrix[row, col])
    for(sc3_rowaco in 1:nrow(sc3_corners_a)) {
      for(sc3_colaco in 1:ncol(sc3_corners_a)) {
        ifelse(!sc3_corners_a[sc3_rowaco,sc3_colaco]=="",sc3_corners_h[sc3_rowaco,sc3_colaco] <- sc3_corners_a[sc3_rowaco,sc3_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SP1
for(sp1_rowhco in 1:nrow(sp1_corners_h)) {
  for(sp1_colhco in 1:ncol(sp1_corners_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowaco in 1:nrow(sp1_corners_a)) {
      for(sp1_colaco in 1:ncol(sp1_corners_a)) {
        ifelse(!sp1_corners_a[sp1_rowaco,sp1_colaco]=="",sp1_corners_h[sp1_rowaco,sp1_colaco] <- sp1_corners_a[sp1_rowaco,sp1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#SP2
for(sp2_rowhco in 1:nrow(sp2_corners_h)) {
  for(sp2_colhco in 1:ncol(sp2_corners_h)) {

    # print(my_matrix[row, col])
    for(sp2_rowaco in 1:nrow(sp2_corners_a)) {
      for(sp2_colaco in 1:ncol(sp2_corners_a)) {
        ifelse(!sp2_corners_a[sp2_rowaco,sp2_colaco]=="",sp2_corners_h[sp2_rowaco,sp2_colaco] <- sp2_corners_a[sp2_rowaco,sp2_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}
#T1
for(t1_rowhco in 1:nrow(t1_corners_h)) {
  for(t1_colhco in 1:ncol(t1_corners_h)) {

    # print(my_matrix[row, col])
    for(t1_rowaco in 1:nrow(t1_corners_a)) {
      for(t1_colaco in 1:ncol(t1_corners_a)) {
        ifelse(!t1_corners_a[t1_rowaco,t1_colaco]=="",t1_corners_h[t1_rowaco,t1_colaco] <- t1_corners_a[t1_rowaco,t1_colaco],next)
        #print(my_matrix[row, col])
      }
    }

  }
}


#
# #write out the data to excel
# write.xlsx(b1_corners_h,'GSmatrix.xlsx',sheetName = "B1")
# write.xlsx(d1_corners_h,'GSmatrix.xlsx',sheetName = "D1", append = TRUE)
# write.xlsx(d2_corners_h,'GSmatrix.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_corners_h,'GSmatrix.xlsx',sheetName = "E0", append = TRUE)
 #write.xlsx(e1_corners_h,'cornermatrix.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_corners_h,'GSmatrix.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_corners_h,'GSmatrix.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_corners_h,'GSmatrix.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_corners_h,'GSmatrix.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_corners_h,'GSmatrix.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_corners_h,'GSmatrix.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_corners_h,'GSmatrix.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_corners_h,'GSmatrix.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_corners_h,'GSmatrix.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_corners_h,'GSmatrix.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_corners_h,'GSmatrix.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_corners_h,'GSmatrix.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_corners_h,'GSmatrix.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_corners_h,'GSmatrix.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_corners_h,'GSmatrix.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_corners_h,'GSmatrix.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_corners_h,'GSmatrix.xlsx',sheetName = "T1", append = TRUE)

