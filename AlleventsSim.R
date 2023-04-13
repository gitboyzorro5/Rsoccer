#
# #################################################
# ###
# poisson_round <- 12
# D1_eventsim <- c()
# D1_alleventsimprep <- list()
# while (poisson_round <= 18){
#   ROUND_20222023 <- poisson_round
#
#   #########################################################
#   # B1 <- B1_rounds[B1_rounds$b1_matchday <= ROUND_20222023,]
#   # B1$FTHG <- as.numeric(B1$FTHG)
#   # B1$FTAG <- as.numeric(B1$FTAG)
#   # B1$TG <- as.numeric(B1$TG)
#
#   D1 <- D1_rounds[D1_rounds$d1_matchday <= ROUND_20222023,]
#   D1$FTHG <- as.numeric(D1$FTHG)
#   D1$FTAG <- as.numeric(D1$FTAG)
#   D1$TG <- as.numeric(D1$TG)
#   D1$HY <- as.numeric(D1$HY)
#   D1$AY <- as.numeric(D1$AY)
#   D1$HCO <- as.numeric(D1$HCO)
#   D1$ACO <- as.numeric(D1$ACO)
#   D1$HF <- as.numeric(D1$HF)
#   D1$AF <- as.numeric(D1$AF)
#   D1$HST <- as.numeric(D1$HST)
#   D1$AST <- as.numeric(D1$AST)
#   # D2 <- D2_rounds[D2_rounds$d2_matchday <= ROUND_20222023,]
#   # D2$FTHG <- as.numeric(D2$FTHG)
#   # D2$FTAG <- as.numeric(D2$FTAG)
#   # D2$TG <- as.numeric(D2$TG)
#   # E0 <- E0_rounds[E0_rounds$e0_matchday <= ROUND_20222023,]
#   # E0$FTHG <- as.numeric(E0$FTHG)
#   # E0$FTAG <- as.numeric(E0$FTAG)
#   # E0$TG <- as.numeric(E0$TG)
#   # E1 <- E1_rounds[E1_rounds$e1_matchday <= ROUND_20222023,]
#   # E1$FTHG <- as.numeric(E1$FTHG)
#   # E1$FTAG <- as.numeric(E1$FTAG)
#   # E1$TG <- as.numeric(E1$TG)
#   # E2 <- E2_rounds[E2_rounds$e2_matchday <= ROUND_20222023,]
#   # E2$FTHG <- as.numeric(E2$FTHG)
#   # E2$FTAG <- as.numeric(E2$FTAG)
#   # E2$TG <- as.numeric(E2$TG)
#   # E3 <- E3_rounds[E3_rounds$e3_matchday <= ROUND_20222023,]
#   # E3$FTHG <- as.numeric(E3$FTHG)
#   # E3$FTAG <- as.numeric(E3$FTAG)
#   # E3$TG <- as.numeric(E3$TG)
#   # EC <- EC_rounds[EC_rounds$ec_matchday <= ROUND_20222023,]
#   # EC$FTHG <- as.numeric(EC$FTHG)
#   # EC$FTAG <- as.numeric(EC$FTAG)
#   #
#   # EC$TG <- as.numeric(EC$TG)
#   # F1 <- F1_rounds[F1_rounds$f1_matchday <= ROUND_20222023,]
#   # F1$FTHG <- as.numeric(F1$FTHG)
#   # F1$FTAG <- as.numeric(F1$FTAG)
#   # F1$TG <- as.numeric(F1$TG)
#   # F2 <- F2_rounds[F2_rounds$f2_matchday <= ROUND_20222023,]
#   # F2$FTHG <- as.numeric(F2$FTHG)
#   # F2$FTAG <- as.numeric(F2$FTAG)
#   # F2$TG <- as.numeric(F2$TG)
#   # G1 <- G1_rounds[G1_rounds$g1_matchday <= ROUND_20222023,]
#   # G1$FTHG <- as.numeric(G1$FTHG)
#   # G1$FTAG <- as.numeric(G1$FTAG)
#   # # G1$TG <- as.numeric(G1$TG)
#   # I1 <- I1_rounds[I1_rounds$i1_matchday <= ROUND_20222023,]
#   # I1$FTHG <- as.numeric(I1$FTHG)
#   # I1$FTAG <- as.numeric(I1$FTAG)
#   # I1$TG <- as.numeric(I1$TG)
#   # I2 <- I2_rounds[I2_rounds$i2_matchday <= ROUND_20222023,]
#   # I2$FTHG <- as.numeric(I2$FTHG)
#   # I2$FTAG <- as.numeric(I2$FTAG)
#   # I2$TG <- as.numeric(I2$TG)
#   # N1 <- N1_rounds[N1_rounds$n1_matchday <= ROUND_20222023,]
#   # N1$FTHG <- as.numeric(N1$FTHG)
#   # N1$FTAG <- as.numeric(N1$FTAG)
#   # N1$TG <- as.numeric(N1$TG)
#   # P1 <- P1_rounds[P1_rounds$p1_matchday <= ROUND_20222023,]
#   # P1$FTHG <- as.numeric(P1$FTHG)
#   # P1$FTAG <- as.numeric(P1$FTAG)
#   # P1$TG <- as.numeric(P1$TG)
#   # SP1 <- SP1_rounds[SP1_rounds$sp1_matchday <= ROUND_20222023,]
#   # SP1$FTHG <- as.numeric(SP1$FTHG)
#   # SP1$FTAG <- as.numeric(SP1$FTAG)
#   # SP1$TG <- as.numeric(SP1$TG)
#   # SP2 <- SP2_rounds[SP2_rounds$sp2_matchday <= ROUND_20222023,]
#   # SP2$FTHG <- as.numeric(SP2$FTHG)
#   # SP2$FTAG <- as.numeric(SP2$FTAG)
#   # SP2$TG <- as.numeric(SP2$TG)
#   # SC0 <- SC0_rounds[SC0_rounds$sc0_matchday <= ROUND_20222023,]
#   # SC0$FTHG <- as.numeric(SC0$FTHG)
#   # SC0$FTAG <- as.numeric(SC0$FTAG)
#   # SC0$TG <- as.numeric(SC0$TG)
#   # SC1 <- SC1_rounds[SC1_rounds$sc1_matchday <= ROUND_20222023,]
#   # SC1$FTHG <- as.numeric(SC1$FTHG)
#   # SC1$FTAG <- as.numeric(SC1$FTAG)
#   # SC1$TG <- as.numeric(SC1$TG)
#   # SC2 <- SC2_rounds[SC2_rounds$sc2_matchday <= ROUND_20222023,]
#   # SC2$FTHG <- as.numeric(SC2$FTHG)
#   # SC2$FTAG <- as.numeric(SC2$FTAG)
#   # SC2$TG <- as.numeric(SC2$TG)
#   # SC3 <- SC3_rounds[SC3_rounds$sc3_matchday <= ROUND_20222023,]
#   # SC3$FTHG <- as.numeric(SC3$FTHG)
#   # SC3$FTAG <- as.numeric(SC3$FTAG)
#   # SC3$TG <- as.numeric(SC3$TG)
#   # T1 <- T1_rounds[T1_rounds$t1_matchday <= ROUND_20222023,]
#   # T1$FTHG <- as.numeric(T1$FTHG)
#   # T1$FTAG <- as.numeric(T1$FTAG)
#   # T1$TG <- as.numeric(T1$TG)
#
#   #############################################################
#   source("goaltotalsv2.R")
#   source("CornerTotalsV2.R")
#   source("ShotsAnalysis.R")
#   source("FoulsAnalysis.R")
#   source("PoissonModel.R")
#   source("PoissonCards.R")
#   source("PoissonCorners.R")
#   source("PoissonFouls.R")
#   source("PoissonShots.R")
#
#   # d1_totalgames_poiss <- c()
#   # d1_totalgames_poiss[1:nrow(D1)] <- paste("no",poisson_round,sep = "")
#   # d1_home_poisson <- cbind(d1_home_poisson,d1_totalgames_poiss)
#   # d1_away_poisson <- cbind(d1_away_poisson,d1_totalgames_poiss)
#   #
#   # write.csv(d1_home_poisson,paste("d1_home_poisson",ROUND_20222023,".csv",sep = "_"))
#   # write.csv(d1_away_poisson,paste("d1_away_poisson",ROUND_20222023,".csv",sep = "_"))
#   HomeTeam_d1 <- rep(d1_teams, each = length(d1_teams))
#   AwayTeam_d1 <- rep(d1_teams, length(d1_teams))
#   D1_fixtures <- cbind(HomeTeam_d1,AwayTeam_d1)
#   D1_fixtures <- as.data.frame(D1_fixtures)
#   D1_fixtures <- D1_fixtures[!D1_fixtures$HomeTeam_d1 == D1_fixtures$AwayTeam_d1,]
#   rownames(D1_fixtures) <- NULL
#   D1_fixtures$Div <- "D1"
#   D1_fixtures <- D1_fixtures[,c(3,1,2)]
#
#   D1_fixtures$avg_HG_d1 <- d1_avg_HG
#
#   D1_fixtures$d1_homeas <- rep(d1_home_as,each = length(d1_teams)-1)
#
#   d1_awayds_lookup <- cbind(d1_teams,d1_away_ds)
#
#   d1_awayds_lookup <- as.data.frame(d1_awayds_lookup)
#
#   colnames(d1_awayds_lookup) <- c("AwayTeam_d1","d1_awayds")
#
#
#   require('RH2')
#   D1_fixtures$d1_awayds <- sqldf("SELECT d1_awayds_lookup.d1_awayds FROM d1_awayds_lookup INNER JOIN D1_fixtures ON d1_awayds_lookup.AwayTeam_d1 = D1_fixtures.AwayTeam_d1")
#
#   D1_fixtures$avg_AG_d1 <- d1_avg_AG
#
#   d1_awayas_lookup <- cbind(d1_teams,d1_away_as)
#
#   d1_awayas_lookup <- as.data.frame(d1_awayas_lookup)
#
#   colnames(d1_awayas_lookup) <- c("AwayTeam_d1","d1_awayas")
#
#
#   D1_fixtures$d1_awayas <- sqldf("SELECT d1_awayas_lookup.d1_awayas FROM d1_awayas_lookup INNER JOIN D1_fixtures ON d1_awayas_lookup.AwayTeam_d1 = D1_fixtures.AwayTeam_d1")
#
#   D1_fixtures$d1_homeds <- rep(d1_home_ds,each = length(d1_teams)-1)
#
#   D1_fixtures$d1_awayds <- as.numeric(unlist(D1_fixtures$d1_awayds))
#   #xGH
#   D1_fixtures$d1_xGH <- D1_fixtures$avg_HG_d1 * D1_fixtures$d1_homeas * D1_fixtures$d1_awayds
#
#   #xGA
#
#   D1_fixtures$d1_awayas <- as.numeric(unlist(D1_fixtures$d1_awayas))
#
#   D1_fixtures$d1_xGA <- D1_fixtures$avg_AG_d1 * D1_fixtures$d1_awayas * D1_fixtures$d1_homeds
#
#   D1_fixtures$d1_0_0 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_0 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_1 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_1 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_0 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_2 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_2 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_1 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_2 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_3 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_0 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_1 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_2 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_3 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_3 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_3 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_4 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_0 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_1 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_2 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_3 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_4 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_4 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_4 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_4 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_5 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_0 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_1 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_2 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_3 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_4 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_5 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_5 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_5 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_5 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_5 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_6 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_0 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(0,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_1 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(1,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_2 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(2,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_3 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(3,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_4 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(4,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_6_5 <- round(stats::dpois(6,D1_fixtures$d1_xGH) * stats::dpois(5,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_0_6 <- round(stats::dpois(0,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_1_6 <- round(stats::dpois(1,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_2_6 <- round(stats::dpois(2,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_3_6 <- round(stats::dpois(3,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_4_6 <- round(stats::dpois(4,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   D1_fixtures$d1_5_6 <- round(stats::dpois(5,D1_fixtures$d1_xGH) * stats::dpois(6,D1_fixtures$d1_xGA), digits = 4)
#   #Home win
#   D1_fixtures$d1_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 + D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5
#   )
#
#   D1_fixtures$d1_H <- percent(D1_fixtures$d1_H, accuracy = 0.1)
#
#   #Draw
#   D1_fixtures$d1_D <- (
#
#     D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 + D1_fixtures$d1_4_4 +
#       D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6
#   )
#
#   D1_fixtures$d1_D <- percent(D1_fixtures$d1_D, accuracy = 0.1)
#
#   #Away
#
#   D1_fixtures$d1_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 + D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6
#   )
#
#   D1_fixtures$d1_A <- percent(D1_fixtures$d1_A, accuracy = 0.1)
#
#   #ov25
#   D1_fixtures$d1_ov25 <- (
#     D1_fixtures$d1_2_1 + D1_fixtures$d1_1_2 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 + D1_fixtures$d1_2_3 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 + D1_fixtures$d1_0_4 +
#       D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 + D1_fixtures$d1_4_4 + D1_fixtures$d1_5_0 +
#       D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 + D1_fixtures$d1_0_5 +
#       D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 + D1_fixtures$d1_5_5 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5 + D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 +
#       D1_fixtures$d1_4_6 + D1_fixtures$d1_5_6 + D1_fixtures$d1_6_6
#   )
#   #un25
#   D1_fixtures$d1_un25 <- (
#     D1_fixtures$d1_0_0 + D1_fixtures$d1_1_0 + D1_fixtures$d1_0_1 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_0 + D1_fixtures$d1_0_2
#   )
#   #odds
#   D1_fixtures$d1_ov25_odds <- round((1/D1_fixtures$d1_ov25),digits = 2)
#   D1_fixtures$d1_un25_odds <- round((1/D1_fixtures$d1_un25),digits = 2)
#
#   D1_fixtures$d1_ov25_odds
#   D1_fixtures$d1_un25_odds
#   ###############################################################################
#   ###BTTS########################################################################
#   #BTTSY
#   D1_fixtures$d1_BTTSY <- (
#     D1_fixtures$d1_1_1 + D1_fixtures$d1_2_1 + D1_fixtures$d1_1_2 + D1_fixtures$d1_3_1 + D1_fixtures$d1_3_2 +
#       D1_fixtures$d1_2_2 + D1_fixtures$d1_1_3 + D1_fixtures$d1_2_3 + D1_fixtures$d1_3_3 + D1_fixtures$d1_4_4 +
#       D1_fixtures$d1_4_1 + D1_fixtures$d1_4_3 + D1_fixtures$d1_4_2 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 +
#       D1_fixtures$d1_3_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 +
#       D1_fixtures$d1_5_4 + D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_6_6 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6
#   )
#   #BTTSN
#   D1_fixtures$d1_BTTSN <- (
#     D1_fixtures$d1_0_0 + D1_fixtures$d1_1_0 + D1_fixtures$d1_0_1 + D1_fixtures$d1_2_0 + D1_fixtures$d1_0_2 +
#       D1_fixtures$d1_3_0 + D1_fixtures$d1_0_3 + D1_fixtures$d1_4_0 + D1_fixtures$d1_0_4 + D1_fixtures$d1_5_0 +
#       D1_fixtures$d1_0_5 + D1_fixtures$d1_6_0 + D1_fixtures$d1_0_6
#   )
#
#   D1_fixtures$d1_BTTSY_odds <- round((1/D1_fixtures$d1_BTTSY),digits = 2)
#   D1_fixtures$d1_BTTSN_odds <- round((1/D1_fixtures$d1_BTTSN),digits = 2)
#
#   D1_fixtures$d1_BTTSY <- percent(D1_fixtures$d1_BTTSY, accuracy = 0.1)
#   D1_fixtures$d1_BTTSN <- percent(D1_fixtures$d1_BTTSN, accuracy = 0.1)
#   #odds
#   D1_fixtures$d1_BTTSY_odds
#   D1_fixtures$d1_BTTSN_odds
#   ########Asian Handicaps##########################################################################################################
#   ##########################################################################
#   #AH(0)
#   #AH_0_H
#   D1_fixtures$d1_AH_0_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 +D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6
#   )
#   #AH_0_A
#   D1_fixtures$d1_AH_0_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 +D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6
#   )
#
#   #odds
#   D1_fixtures$d1_AH_0_H_odds <- round((1/D1_fixtures$d1_AH_0_H),digits = 2)
#   D1_fixtures$d1_AH_0_A_odds <- round((1/D1_fixtures$d1_AH_0_A),digits = 2)
#
#   D1_fixtures$d1_AH_0_H_odds
#   D1_fixtures$d1_AH_0_A_odds
#   #percentages
#   D1_fixtures$d1_AH_0_H <- percent(D1_fixtures$d1_AH_0_H, accuracy = 0.1)
#   D1_fixtures$d1_AH_0_A <- percent(D1_fixtures$d1_AH_0_A, accuracy = 0.1)
#   ####################################################################################
#   ##########################################################################
#   #AH(-0.75)
#   #AH_n075_H
#   D1_fixtures$d1_AH_n075_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 +D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5
#   )
#   #AH_n075_A
#   D1_fixtures$d1_AH_n075_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 +D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6
#   )
#
#   #odds
#   D1_fixtures$d1_AH_n075_H_odds <- round((1/D1_fixtures$d1_AH_n075_H),digits = 2)
#   D1_fixtures$d1_AH_n075_A_odds <- round((1/D1_fixtures$d1_AH_n075_A),digits = 2)
#
#   D1_fixtures$d1_AH_n075_H_odds
#   D1_fixtures$d1_AH_n075_A_odds
#   #percentages
#   D1_fixtures$d1_AH_n075_H <- percent(D1_fixtures$d1_AH_n075_H, accuracy = 0.1)
#   D1_fixtures$d1_AH_n075_A <- percent(D1_fixtures$d1_AH_n075_A, accuracy = 0.1)
#   ##########################################################################
#   #AH(0.75)
#   #AH_075_H
#   D1_fixtures$d1_AH_075_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 +D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6 + D1_fixtures$d1_0_1 + D1_fixtures$d1_1_2 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_3_4 + D1_fixtures$d1_4_5 + D1_fixtures$d1_5_6
#   )
#   #AH_075_A
#   D1_fixtures$d1_AH_075_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 +D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6 + D1_fixtures$d1_1_0 + D1_fixtures$d1_2_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_3 + D1_fixtures$d1_5_4 + D1_fixtures$d1_6_5
#   )
#
#   #odds
#   D1_fixtures$d1_AH_075_H_odds <- round((1/D1_fixtures$d1_AH_075_H),digits = 2)
#   D1_fixtures$d1_AH_075_A_odds <- round((1/D1_fixtures$d1_AH_075_A),digits = 2)
#
#   D1_fixtures$d1_AH_075_H_odds
#   D1_fixtures$d1_AH_075_A_odds
#   #percentages
#   D1_fixtures$d1_AH_075_H <- percent(D1_fixtures$d1_AH_075_H, accuracy = 0.1)
#   D1_fixtures$d1_AH_075_A <- percent(D1_fixtures$d1_AH_075_A, accuracy = 0.1)
#   ####################################################################################
#   #AH(-1.25)
#   #AH_n125_H
#   D1_fixtures$d1_AH_n125_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 +D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5
#   )
#   #AH_n125_A
#   D1_fixtures$d1_AH_n125_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 +D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6
#   )
#
#   #odds
#   D1_fixtures$d1_AH_n125_H_odds <- round((1/D1_fixtures$d1_AH_n125_H),digits = 2)
#   D1_fixtures$d1_AH_n125_A_odds <- round((1/D1_fixtures$d1_AH_n125_A),digits = 2)
#
#   D1_fixtures$d1_AH_n125_H_odds
#   D1_fixtures$d1_AH_n125_A_odds
#   #percentages
#   D1_fixtures$d1_AH_n125_H <- percent(D1_fixtures$d1_AH_n125_H, accuracy = 0.1)
#   D1_fixtures$d1_AH_n125_A <- percent(D1_fixtures$d1_AH_n125_A, accuracy = 0.1)
#
#   ####################################################################################
#   ##########################################################################
#   #AH(1.25)
#   #AH_125_H
#   D1_fixtures$d1_AH_125_H <- (
#     D1_fixtures$d1_1_0 + D1_fixtures$d1_2_0 + D1_fixtures$d1_2_1 + D1_fixtures$d1_3_0 + D1_fixtures$d1_3_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_0 + D1_fixtures$d1_4_1 + D1_fixtures$d1_4_2 + D1_fixtures$d1_4_3 +
#       D1_fixtures$d1_5_0 +D1_fixtures$d1_5_1 + D1_fixtures$d1_5_2 + D1_fixtures$d1_5_3 + D1_fixtures$d1_5_4 +
#       D1_fixtures$d1_6_0 + D1_fixtures$d1_6_1 + D1_fixtures$d1_6_2 + D1_fixtures$d1_6_3 + D1_fixtures$d1_6_4 +
#       D1_fixtures$d1_6_5 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6 + D1_fixtures$d1_0_1 + D1_fixtures$d1_1_2 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_3_4 + D1_fixtures$d1_4_5 + D1_fixtures$d1_5_6
#   )
#   #AH_125_A
#   D1_fixtures$d1_AH_125_A <- (
#     D1_fixtures$d1_0_1 + D1_fixtures$d1_0_2 + D1_fixtures$d1_1_2 + D1_fixtures$d1_0_3 + D1_fixtures$d1_1_3 +
#       D1_fixtures$d1_2_3 + D1_fixtures$d1_0_4 + D1_fixtures$d1_1_4 + D1_fixtures$d1_2_4 + D1_fixtures$d1_3_4 +
#       D1_fixtures$d1_0_5 +D1_fixtures$d1_1_5 + D1_fixtures$d1_2_5 + D1_fixtures$d1_3_5 + D1_fixtures$d1_4_5 +
#       D1_fixtures$d1_0_6 + D1_fixtures$d1_1_6 + D1_fixtures$d1_2_6 + D1_fixtures$d1_3_6 + D1_fixtures$d1_4_6 +
#       D1_fixtures$d1_5_6 + D1_fixtures$d1_0_0 + D1_fixtures$d1_1_1 + D1_fixtures$d1_2_2 + D1_fixtures$d1_3_3 +
#       D1_fixtures$d1_4_4 + D1_fixtures$d1_5_5 + D1_fixtures$d1_6_6 + D1_fixtures$d1_1_0 + D1_fixtures$d1_2_1 +
#       D1_fixtures$d1_3_2 + D1_fixtures$d1_4_3 + D1_fixtures$d1_5_4 + D1_fixtures$d1_6_5
#   )
#
#   #odds
#   D1_fixtures$d1_AH_125_H_odds <- round((1/D1_fixtures$d1_AH_125_H),digits = 2)
#   D1_fixtures$d1_AH_125_A_odds <- round((1/D1_fixtures$d1_AH_125_A),digits = 2)
#
#   D1_fixtures$d1_AH_125_H_odds
#   D1_fixtures$d1_AH_125_A_odds
#   #percentages
#   D1_fixtures$d1_AH_125_H <- percent(D1_fixtures$d1_AH_125_H, accuracy = 0.1)
#   D1_fixtures$d1_AH_125_A <- percent(D1_fixtures$d1_AH_125_A, accuracy = 0.1)
#   ####################################################################################
#   ########Asian Handicaps######################################################################################################
#   #percentages
#   D1_fixtures$d1_ov25 <- percent(D1_fixtures$d1_ov25, accuracy = 0.1)
#
#   D1_fixtures$d1_un25 <- percent(D1_fixtures$d1_un25, accuracy = 0.1)
#   D1_fixtures$d1_pscore <- paste(round(D1_fixtures$d1_xGH,digits = 0),round(D1_fixtures$d1_xGA,digits = 0),sep = "-")
#  ###########################################################################################################################
#   #D1
#   D1_fixtures$Hometeam_d1_index <- match(D1_fixtures$HomeTeam_d1,d1_teams)
#   D1_fixtures$Awayteam_d1_index <- match(D1_fixtures$AwayTeam_d1,d1_teams)
#   d1_prediction <- c()
#   d1_HWM <- c()
#   d1_AWM <- c()
#   d1_HWMLM <- c()
#   d1_AWMLM <- c()
#   d1_HY <- c()
#   d1_AY <- c()
#   d1_HCO <- c()
#   d1_ACO <- c()
#   d1_HXSC <- c()
#   d1_AXSC <- c()
#   d1_HYCPF <- c()
#   d1_AYCPF <- c()
#   for(d1_row in 1:nrow(D1_fixtures))
#   {
#
#     d1_hometeamindex <- D1_fixtures[d1_row,"Hometeam_d1_index"]
#     d1_awayteamindex <- D1_fixtures[d1_row,"Awayteam_d1_index"]
#     #analyse team form
#     #home team
#     d1_form_vec_ht <- as.vector(d1_form_h[d1_hometeamindex,])
#     d1_form_vec_ht[is.na(d1_form_vec_ht)] <- ""
#     d1_form_vec_ht <- d1_form_vec_ht[d1_form_vec_ht != ""]
#     d1_form_vec_ht  <-tail(d1_form_vec_ht,6)
#     d1_ht_numberof_wins <- length(which(d1_form_vec_ht == "W"))
#     d1_ht_numberof_draws <- length(which(d1_form_vec_ht == "D"))
#     d1_ht_numberof_loss <- length(which(d1_form_vec_ht == "L"))
#     #awayteam
#     d1_form_vec_at <- as.vector(d1_form_h[d1_awayteamindex,])
#     d1_form_vec_at[is.na(d1_form_vec_at)] <- ""
#     d1_form_vec_at <- d1_form_vec_at[d1_form_vec_at != ""]
#     d1_form_vec_at  <-tail(d1_form_vec_at,6)
#     d1_at_numberof_wins <- length(which(d1_form_vec_at == "W"))
#     d1_at_numberof_draws <- length(which(d1_form_vec_at == "D"))
#     d1_at_numberof_loss <- length(which(d1_form_vec_at == "L"))
#
#     ######################################################################
#     #analyse goals scored
#     #hometeam
#     d1_goalscored_vec_ht <- as.vector(d1_goalscored_h[d1_hometeamindex,])
#     d1_goalscored_vec_ht[is.na(d1_goalscored_vec_ht)] <- ""
#     d1_goalscored_vec_ht <- d1_goalscored_vec_ht[d1_goalscored_vec_ht != ""]
#     d1_goalscored_vec_ht  <-tail(d1_goalscored_vec_ht,6)
#     d1_goalscored_vec_ht  <- as.numeric(d1_goalscored_vec_ht)
#     d1_ht_totalgoalscored <- sum(d1_goalscored_vec_ht)
#     d1_ht_matches_scoring <- length(which(d1_goalscored_vec_ht > 0))
#     d1_ht_matches_without_scoring <- length(which(d1_goalscored_vec_ht == "0"))
#     #awayteam
#     d1_goalscored_vec_at <- as.vector(d1_goalscored_h[d1_awayteamindex,])
#     d1_goalscored_vec_at[is.na(d1_goalscored_vec_at)] <- ""
#     d1_goalscored_vec_at <- d1_goalscored_vec_at[d1_goalscored_vec_at != ""]
#     d1_goalscored_vec_at  <-tail(d1_goalscored_vec_at,6)
#     d1_goalscored_vec_at  <- as.numeric(d1_goalscored_vec_at)
#     d1_at_totalgoalscored <- sum(d1_goalscored_vec_at)
#     d1_at_matches_scoring <- length(which(d1_goalscored_vec_at > 0))
#     d1_at_matches_without_scoring <- length(which(d1_goalscored_vec_at == "0"))
#     #####################################################################################
#     #analyse goals conceded
#     #hometeam
#     d1_goalconceded_vec_ht <- as.vector(d1_goalconceded_h[d1_hometeamindex,])
#     d1_goalconceded_vec_ht[is.na(d1_goalconceded_vec_ht)] <- ""
#     d1_goalconceded_vec_ht <- d1_goalconceded_vec_ht[d1_goalconceded_vec_ht != ""]
#     d1_goalconceded_vec_ht  <-tail(d1_goalconceded_vec_ht,6)
#     d1_goalconceded_vec_ht  <- as.numeric(d1_goalconceded_vec_ht)
#     d1_goalconceded_vec_ht
#     d1_ht_totalgoalconceded <- sum(d1_goalconceded_vec_ht)
#     d1_ht_matches_concede <- length(which(d1_goalconceded_vec_ht > 0))
#     d1_ht_matches_without_concede <- length(which(d1_goalconceded_vec_ht == "0"))
#     #awayteam
#     d1_goalconceded_vec_at <- as.vector(d1_goalconceded_h[d1_awayteamindex,])
#     d1_goalconceded_vec_at[is.na(d1_goalconceded_vec_at)] <- ""
#     d1_goalconceded_vec_at <- d1_goalconceded_vec_at[d1_goalconceded_vec_at != ""]
#     d1_goalconceded_vec_at  <-tail(d1_goalconceded_vec_at,6)
#     d1_goalconceded_vec_at  <- as.numeric(d1_goalconceded_vec_at)
#     d1_at_totalgoalconceded <- sum(d1_goalconceded_vec_at)
#     d1_at_matches_concede <- length(which(d1_goalconceded_vec_at > 0))
#     d1_at_matches_without_concede <- length(which(d1_goalconceded_vec_at == "0"))
#
#     ####################################################################################
#     #analyse total combined goals
#     #hometeam
#     d1_totalgoals_vec_ht <- as.vector(d1_totalgoals_h[d1_hometeamindex,])
#     d1_totalgoals_vec_ht[is.na(d1_totalgoals_vec_ht)] <- ""
#     d1_totalgoals_vec_ht <- d1_totalgoals_vec_ht[d1_totalgoals_vec_ht != ""]
#     d1_totalgoals_vec_ht  <-tail(d1_totalgoals_vec_ht,6)
#     d1_totalgoals_vec_ht  <- as.numeric(d1_totalgoals_vec_ht)
#     d1_totalgoals_vec_ht
#     d1_ht_totalgoals <- sum(d1_totalgoals_vec_ht)
#     d1_ht_avgtotalgoals <- (d1_ht_totalgoals/6)
#     d1_ht_no_of_ov25 <- length(which(d1_totalgoals_vec_ht >= 3))
#     d1_ht_no_of_un25 <- length(which(d1_totalgoals_vec_ht <= 2))
#     #awayteam
#     d1_totalgoals_vec_at <- as.vector(d1_totalgoals_h[d1_awayteamindex,])
#     d1_totalgoals_vec_at[is.na(d1_totalgoals_vec_at)] <- ""
#     d1_totalgoals_vec_at <- d1_totalgoals_vec_at[d1_totalgoals_vec_at != ""]
#     d1_totalgoals_vec_at  <-tail(d1_totalgoals_vec_at,6)
#     d1_totalgoals_vec_at  <- as.numeric(d1_totalgoals_vec_at)
#     d1_totalgoals_vec_at
#     d1_at_totalgoals <- sum(d1_totalgoals_vec_at)
#     d1_at_avgtotalgoals <- (d1_at_totalgoals/6)
#     d1_at_no_of_ov25 <- length(which(d1_totalgoals_vec_at >= 3))
#     d1_at_no_of_un25 <- length(which(d1_totalgoals_vec_at <= 2))
#     ################################################################################
#     #analyse win margin
#     #hometeam
#     d1_winmargin_vec_ht <- as.vector(d1_winmargin_h[d1_hometeamindex,])
#     d1_winmargin_vec_ht[is.na(d1_winmargin_vec_ht)] <- ""
#     d1_winmargin_vec_ht <- d1_winmargin_vec_ht[d1_winmargin_vec_ht != ""]
#     d1_winmargin_vec_ht  <-tail(d1_winmargin_vec_ht,6)
#     d1_winmargin_vec_ht  <- as.numeric(d1_winmargin_vec_ht)
#
#     d1_ht_totalwinmargin <- sum(d1_winmargin_vec_ht)
#     d1_ht_no_of_winmargin_ov0 <- length(which(d1_winmargin_vec_ht >= 0))
#     d1_ht_no_of_winmargin_ov1 <- length(which(d1_winmargin_vec_ht >= 1))
#     d1_ht_no_of_winmargin_un0 <- length(which(d1_winmargin_vec_ht <= 0))
#     d1_ht_no_of_winmargin_un1 <- length(which(d1_winmargin_vec_ht <= 1))
#     #awayteam
#     d1_winmargin_vec_at <- as.vector(d1_winmargin_h[d1_awayteamindex,])
#     d1_winmargin_vec_at[is.na(d1_winmargin_vec_at)] <- ""
#     d1_winmargin_vec_at <- d1_winmargin_vec_at[d1_winmargin_vec_at != ""]
#     d1_winmargin_vec_at  <-tail(d1_winmargin_vec_at,6)
#     d1_winmargin_vec_at  <- as.numeric(d1_winmargin_vec_at)
#
#     d1_at_totalwinmargin <- sum(d1_winmargin_vec_at)
#     d1_at_no_of_winmargin_ov0 <- length(which(d1_winmargin_vec_at >= 0))
#     d1_at_no_of_winmargin_ov1 <- length(which(d1_winmargin_vec_at >= 1))
#     d1_at_no_of_winmargin_un0 <- length(which(d1_winmargin_vec_at <= 0))
#     d1_at_no_of_winmargin_un1 <- length(which(d1_winmargin_vec_at <= 1))
#     ##################################################################################
#     #very last win margin
#     #hometeam
#     d1_winmargin_vec_ht_lm <- as.vector(d1_winmargin_h[d1_hometeamindex,])
#     d1_winmargin_vec_ht_lm[is.na(d1_winmargin_vec_ht_lm)] <- ""
#     d1_winmargin_vec_ht_lm <- d1_winmargin_vec_ht_lm[d1_winmargin_vec_ht_lm != ""]
#     d1_winmargin_vec_ht_lm  <-tail(d1_winmargin_vec_ht_lm,1)
#     #awayteam
#     d1_winmargin_vec_at_lm <- as.vector(d1_winmargin_h[d1_awayteamindex,])
#     d1_winmargin_vec_at_lm[is.na(d1_winmargin_vec_at_lm)] <- ""
#     d1_winmargin_vec_at_lm <- d1_winmargin_vec_at_lm[d1_winmargin_vec_at_lm != ""]
#     d1_winmargin_vec_at_lm  <-tail(d1_winmargin_vec_at_lm,1)
#     #################################################################################
#     #pick average yellow cards
#     #hometeam
#     d1_yellowtotals_vec_ht <- as.vector(d1_yellowtotalsv2[d1_hometeamindex,])
#     d1_yellowtotals_vec_ht[is.na(d1_yellowtotals_vec_ht)] <- ""
#     d1_yellowtotals_vec_ht <- d1_yellowtotals_vec_ht[d1_yellowtotals_vec_ht != ""]
#     d1_yellowtotals_vec_ht  <-tail(d1_yellowtotals_vec_ht,1)
#     #awayteam
#     d1_yellowtotals_vec_at <- as.vector(d1_yellowtotalsv2[d1_awayteamindex,])
#     d1_yellowtotals_vec_at[is.na(d1_yellowtotals_vec_at)] <- ""
#     d1_yellowtotals_vec_at <- d1_yellowtotals_vec_at[d1_yellowtotals_vec_at != ""]
#     d1_yellowtotals_vec_at  <-tail(d1_yellowtotals_vec_at,1)
#
#     #################################################################################
#     #pick average corners
#     #hometeam
#     d1_cornertotals_vec_ht <- as.vector(d1_cornertotalsv2[d1_hometeamindex,])
#     d1_cornertotals_vec_ht[is.na(d1_cornertotals_vec_ht)] <- ""
#     d1_cornertotals_vec_ht <- d1_cornertotals_vec_ht[d1_cornertotals_vec_ht != ""]
#     d1_cornertotals_vec_ht  <-tail(d1_cornertotals_vec_ht,1)
#     #awayteam
#     d1_cornertotals_vec_at <- as.vector(d1_cornertotalsv2[d1_awayteamindex,])
#     d1_cornertotals_vec_at[is.na(d1_cornertotals_vec_at)] <- ""
#     d1_cornertotals_vec_at <- d1_cornertotals_vec_at[d1_cornertotals_vec_at != ""]
#     d1_cornertotals_vec_at  <-tail(d1_cornertotals_vec_at,1)
#     #################################################################################
#     #pick xpected shots conversion
#     #hometeam
#     d1_xshotsconversion_vec_ht <- as.vector(d1_shots_analysis[d1_hometeamindex,])
#     d1_xshotsconversion_vec_ht[is.na(d1_xshotsconversion_vec_ht)] <- ""
#     d1_xshotsconversion_vec_ht <- d1_xshotsconversion_vec_ht[d1_xshotsconversion_vec_ht != ""]
#     d1_xshotsconversion_vec_ht  <-tail(d1_xshotsconversion_vec_ht,1)
#     #awayteam
#     d1_xshotsconversion_vec_at <- as.vector(d1_shots_analysis[d1_awayteamindex,])
#     d1_xshotsconversion_vec_at[is.na(d1_xshotsconversion_vec_at)] <- ""
#     d1_xshotsconversion_vec_at <- d1_xshotsconversion_vec_at[d1_xshotsconversion_vec_at != ""]
#     d1_xshotsconversion_vec_at  <-tail(d1_xshotsconversion_vec_at,1)
#     #################################################################################
#     #pick yellow cards per foul
#     #hometeam
#     d1_fouls_conversion_vec_ht <- as.vector(d1_fouls_conversion[d1_hometeamindex,])
#     d1_fouls_conversion_vec_ht[is.na(d1_fouls_conversion_vec_ht)] <- ""
#     d1_fouls_conversion_vec_ht <- d1_fouls_conversion_vec_ht[d1_fouls_conversion_vec_ht != ""]
#     d1_fouls_conversion_vec_ht  <-tail(d1_fouls_conversion_vec_ht,1)
#     #awayteam
#     d1_fouls_conversion_vec_at <- as.vector(d1_fouls_conversion[d1_awayteamindex,])
#     d1_fouls_conversion_vec_at[is.na(d1_fouls_conversion_vec_at)] <- ""
#     d1_fouls_conversion_vec_at <- d1_fouls_conversion_vec_at[d1_fouls_conversion_vec_at != ""]
#     d1_fouls_conversion_vec_at  <-tail(d1_fouls_conversion_vec_at,1)
#     #################################################################################
#
#     ####we need to decide ############
#     #winner goals
#     d1_ht_last6points <- d1_ht_numberof_wins*3 + d1_ht_numberof_draws*1
#     d1_at_last6points <- d1_at_numberof_wins*3 + d1_at_numberof_draws*1
#
#     if(d1_ht_last6points > d1_at_last6points) {d1_3waypick <- "1"}  else {d1_3waypick <- "X2"}
#
#     if(d1_at_last6points > d1_ht_last6points ) {d1_3waypick <- "2"} else {d1_3waypick <- "1X"}
#
#     if(d1_ht_no_of_ov25 + d1_at_no_of_ov25 >= 6) {d1_goalspick <- "ov25"} else {d1_goalspick <- "un25"}
#
#     if(d1_ht_no_of_un25 + d1_at_no_of_un25 >= 6) {d1_goalspick <- "un25"} else {d1_goalspick <- "ov25"}
#
#     if(d1_ht_matches_scoring >= 4 && d1_at_matches_scoring >=4) {d1_btts <- "BTTS-Y"} else {d1_btts <- "BTTS-N"}
#
#
#     d1_prediction[d1_row] <- rbind(paste(d1_3waypick,d1_goalspick,d1_btts,sep = ","))
#     d1_HWM[d1_row] <- d1_ht_totalwinmargin
#     d1_AWM[d1_row] <- d1_at_totalwinmargin
#
#     d1_HWMLM[d1_row] <- d1_winmargin_vec_ht_lm
#     d1_AWMLM[d1_row] <- d1_winmargin_vec_at_lm
#
#     d1_HY[d1_row] <- d1_yellowtotals_vec_ht
#     d1_AY[d1_row] <- d1_yellowtotals_vec_at
#
#     d1_HCO[d1_row] <- d1_cornertotals_vec_ht
#     d1_ACO[d1_row] <- d1_cornertotals_vec_at
#
#     d1_HXSC[d1_row] <- d1_xshotsconversion_vec_ht
#     d1_AXSC[d1_row] <- d1_xshotsconversion_vec_at
#
#     d1_HYCPF[d1_row] <- d1_fouls_conversion_vec_ht
#     d1_AYCPF[d1_row] <- d1_fouls_conversion_vec_at
#   }
#
#   d1_prediction <- as.data.frame(d1_prediction)
#   colnames(d1_prediction) <- "prediction"
#
#   d1_HWM <- as.data.frame(d1_HWM)
#   colnames(d1_HWM) <- "HWM"
#
#   d1_AWM <- as.data.frame(d1_AWM)
#   colnames(d1_AWM) <- "AWM"
#
#   d1_HWMLM <- as.data.frame(d1_HWMLM)
#   colnames(d1_HWMLM) <- "HWMLM"
#
#   d1_AWMLM <- as.data.frame(d1_AWMLM)
#   colnames(d1_AWMLM) <- "AWMLM"
#
#   d1_HY <- as.data.frame(d1_HY)
#   colnames(d1_HY) <- "AVGHY"
#
#   d1_AY <- as.data.frame(d1_AY)
#   colnames(d1_AY) <- "AVGAY"
#
#   d1_HCO <- as.data.frame(d1_HCO)
#   colnames(d1_HCO) <- "AVGHCO"
#
#   d1_ACO <- as.data.frame(d1_ACO)
#   colnames(d1_ACO) <- "AVGACO"
#
#   d1_HXSC <- as.data.frame(d1_HXSC)
#   colnames(d1_HXSC) <- "HXSC"
#
#   d1_AXSC <- as.data.frame(d1_AXSC)
#   colnames(d1_AXSC) <- "AXSC"
#
#   d1_HYCPF <- as.data.frame(d1_HYCPF)
#   colnames(d1_HYCPF) <- "HYCPF"
#
#   d1_AYCPF <- as.data.frame(d1_AYCPF)
#   colnames(d1_AYCPF) <- "AYCPF"
#
#   d1_picks <- cbind(D1_fixtures$Div,D1_fixtures$HomeTeam_d1,D1_fixtures$AwayTeam_d1,d1_prediction,d1_HWM,d1_AWM,d1_HWMLM,d1_AWMLM,d1_HY,d1_AY,d1_HCO,d1_ACO,d1_HXSC,d1_AXSC,d1_HYCPF,d1_AYCPF)
#
#   colnames(d1_picks)[1] <- "picks_Div"
#   colnames(d1_picks)[2] <- "picks_HomeTeam"
#   colnames(d1_picks)[3] <- "picks_AwayTeam"
#   d1_picks$matchid <- paste(d1_picks$picks_HomeTeam,d1_picks$picks_AwayTeam,sep = "-")
# ###############################################################################################################################
#   D1_fixtures_clone_final <- D1_fixtures_clone[,-c(8,9,10,27)]
#   D1_fixtures_clone_final[,'sep'] <- ''
#
#   d1_dmprediction <-  d1_picks[,c(4,5,6,7,8)]
#   d1_dmprediction[,'sep2'] <- ''
#
#   d1_avgyellow <- d1_picks[,c(9,10)]
#   d1_avgyellow[,'sep3'] <- ''
#
#   d1_avgcorners <- d1_picks[,c(11,12)]
#   d1_avgcorners[,'sep4'] <- ''
#
#   d1_goals <- D1_fixtures[,c(10,11)]
#   d1_goals$d1_xGH <- round(d1_goals$d1_xGH, digits = 2)
#   d1_goals$d1_xGA <- round(d1_goals$d1_xGA, digits = 2)
#   d1_goals$d1_TxG <- d1_goals$d1_xGH + d1_goals$d1_xGA
#   d1_goals[,'sep5'] <- ''
#
#   d1_shots <- D1_fixtures_sot[,c(10,11)]
#   d1_shots$d1_xHST <- round(d1_shots$d1_xHST, digits = 2)
#   d1_shots$d1_xAST <- round(d1_shots$d1_xAST, digits = 2)
#   d1_shots$TxSOT <- d1_shots$d1_xHST + d1_shots$d1_xAST
#   d1_shots[,'sep6'] <- ''
#
#   d1_fouls <- D1_fixtures_fo[,c(10,11)]
#   d1_fouls$d1_xHF <- round(d1_fouls$d1_xHF, digits = 2)
#   d1_fouls$d1_xAF <- round(d1_fouls$d1_xAF, digits = 2)
#   d1_fouls$d1_TxF <- d1_fouls$d1_xHF + d1_fouls$d1_xAF
#
#   d1_ycpf <- d1_picks[,c(15,16)]
#   d1_fouls <- cbind(d1_fouls,d1_ycpf)
#   d1_fouls$HYCPF <- as.numeric(d1_fouls$HYCPF)
#   d1_fouls$AYCPF <- as.numeric(d1_fouls$AYCPF)
#   d1_fouls$x_hyc <- (d1_fouls$d1_xHF) * (d1_fouls$HYCPF)
#   d1_fouls$x_ayc <- (d1_fouls$d1_xAF) * (d1_fouls$AYCPF)
#   d1_fouls$x_TYC <- round((d1_fouls$x_hyc + d1_fouls$x_ayc),digits = 2)
#   d1_fouls[,'sep7'] <- ''
#
#   d1_bookings <- D1_fixtures_yc[,c(10,11)]
#   d1_bookings$d1_xHYC <- round(d1_bookings$d1_xHYC, digits = 2)
#   d1_bookings$d1_xAYC <- round(d1_bookings$d1_xAYC, digits = 2)
#   d1_bookings$d1_TYcards <- d1_bookings$d1_xHYC + d1_bookings$d1_xAYC
#   d1_bookings[,'sep8'] <- ''
#
#   d1_corners <- D1_fixtures_co[,c(10,11)]
#   d1_corners$d1_xHCOC <- round(d1_corners$d1_xHCOC, digits = 2)
#   d1_corners$d1_xACOC <- round(d1_corners$d1_xACOC, digits = 2)
#   d1_corners$d1_TCOs <- d1_corners$d1_xHCOC + d1_corners$d1_xACOC
#   d1_corners[,'sep9'] <- ''
#
#   d1_shotsconversion <- d1_picks[,c(13,14)]
#   d1_shotsconversion <- cbind(d1_shotsconversion,d1_shots)
#   d1_shotsconversion$HXSC <- as.numeric(d1_shotsconversion$HXSC)
#   d1_shotsconversion$AXSC <- as.numeric(d1_shotsconversion$AXSC)
#   d1_shotsconversion$d1_hXgoals <- round((d1_shotsconversion$HXSC * d1_shotsconversion$d1_xHST), digits = 2)
#   d1_shotsconversion$d1_aXgoals <- round((d1_shotsconversion$AXSC * d1_shotsconversion$d1_xAST), digits = 2)
#   d1_shotsconversion$Xgoals <- d1_shotsconversion$d1_hXgoals + d1_shotsconversion$d1_aXgoals
#
#   D1_all <- cbind(D1_fixtures_clone_final,d1_dmprediction,d1_avgyellow,d1_avgcorners,d1_goals,d1_shots,d1_fouls,d1_bookings,d1_corners,d1_shotsconversion)
#   D1_alleventsimprep[[ROUND_20222023]] <- D1_all
#
#   ######################################################################################################################################################################################
#   #D1_eventsim <- rbind(D1_alleventsimprep[])
#
#   # allhomepoisson <- rbind(b1_home_poisson,d1_home_poisson,d2_home_poisson,e0_home_poisson,e1_home_poisson,e2_home_poisson,
#   #                          e3_home_poisson,ec_home_poisson,f1_home_poisson,f2_home_poisson,g1_home_poisson,i1_home_poisson,
#   #                          i2_home_poisson,n1_home_poisson,p1_home_poisson,sp1_home_poisson,sp2_home_poisson,sc0_home_poisson,
#   #                          sc1_home_poisson,sc2_home_poisson,sc3_home_poisson,t1_home_poisson)
#   #
#   #
#   #
#   # allawaypoisson <- rbind(b1_away_poisson,d1_away_poisson,d2_away_poisson,e0_away_poisson,e1_away_poisson,e2_away_poisson,
#   #                         e3_away_poisson,ec_away_poisson,f1_away_poisson,f2_away_poisson,g1_away_poisson,i1_away_poisson,
#   #                         i2_away_poisson,n1_away_poisson,p1_away_poisson,sp1_away_poisson,sp2_away_poisson,sc0_away_poisson,
#   #                         sc1_away_poisson,sc2_away_poisson,sc3_away_poisson,t1_away_poisson)
#
#   poisson_round <- poisson_round + 6
#
# }
#
# nrow(D1_eventsim)
# View(D1_eventsim)
#
# D1_eventsim <- do.call(rbind,D1_alleventsimprep)
#
# View(D1_alleventsimprep)
# # unlink("allhomepoisson.csv")
# # unlink("allawaypoisson.csv")
# #
# # write.csv(allhomepoisson,"allhomepoisson.csv")
# # write.csv(allawaypoisson,"allawaypoisson.csv")
#
# View(D1_all)
#
#
