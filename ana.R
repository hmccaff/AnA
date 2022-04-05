

battle <-
  function(
    #air/land
    Ainf = 0,
    Aart = 0,
    Atnk = 0,
    Afig = 0,
    Abom = 0,
    
    #sea
    Absh = 0,
    Acrs = 0,
    Ades = 0,
    Asub = 0,
    Acar = 0,
    
    #amphibious
    Absh_bbard = 0,
    Acrs_bbard = 0,
    
    #attacker sacrifice planes to take land
    sacrifice.planes = T,
    
    
    Dinf = 0,
    Dart = 0,
    Dtnk = 0,
    Dfig = 0,
    Dbom = 0,
    Daa = 0,
    
    Dbsh = 0,
    Dcrs = 0,
    Ddes = 0,
    Dsub = 0,
    Dcar = 0,
    
    niter = 10000
  ){

  res <- character()
  rd <- numeric(0)
  ra <- numeric(0)
    
  for(i in 1:niter){
    
    #resets unit counts each iter
    ainf <- Ainf
    aart <- Aart
    atnk <- Atnk
    afig <- Afig
    abom <- Abom
    
    absh = Absh
    abshp = Absh*2
    acrs = Acrs
    ades = Ades
    asub = Asub
    acar = Acar
    
    absh_bbard = Absh_bbard
    acrs_bbard = Acrs_bbard
    
    dinf <- Dinf
    dart <- Dart
    dtnk <- Dtnk
    dfig <- Dfig
    dbom <- Dbom
    daa <- Daa
    
    dbsh = Dbsh
    dbshp = Dbsh*2
    dcrs = Dcrs
    ddes = Ddes
    dsub = Dsub
    dcar = Dcar
    
    
    #bombardments at beginning of combat
    bbhits <- rbinom(1, absh_bbard, 4/6) + rbinom(1, acrs_bbard, 3/6)
    
    
    #antiaircraft roll at beginning of combat
    if(afig + abom <= 3*daa){
      aahits <- rbinom(1, afig + abom, 1/6)
    }else{
        aahits <- rbinom(1, 3*daa, 1/6)
    }
    
    if(afig <= aahits){
      aahits <- aahits - afig
      afig <-0
    }else{
        afig <- afig - aahits
        aahits <- 0
    }
    
    if(abom <= aahits){
      abom <-0
    }else{
        abom <- abom - aahits
    }
    
    
    na <- ainf + aart + atnk + afig + abom + absh + acrs + ades + asub + acar
    nd <- dinf + dart + dtnk + dfig + dbom  + dbsh + dcrs + ddes + dsub + dcar + daa
    
    #repeat rounds until at least one player has 0 units
    while( (na > 0) & (nd > 0)){
      
      
      #pair infantry up with artillery each round
      ainf_nonart <- 0
      ainf_art <- 0
      
      if(aart >= ainf){
        ainf_art <- ainf
        ainf_nonart <- 0
      }
      if(aart < ainf){
        ainf_art <- aart
        ainf_nonart <- abs(ainf - aart)
      }
      
      
      
      #sneak attack
      if((ddes ==0)&(asub > 0)){
        ashits <- rbinom(1, asub, 2/6)
        
        #take hit on battleship first if it has hitpoints available
        while((dbshp > dbsh)&(ashits>0)){
          dbshp <- dbshp - 1
          ashits <- ashits -1
        }
        while((dsub > 0)&(ashits>0)){
          dsub <- dsub - 1
          ashits <- ashits -1
        }
        while((dcrs > 0)&(ashits>0)){
          dcrs <- dcrs - 1
          ashits <- ashits -1
        }
        while((dcar > 0)&(ashits>0)){
          dcar <- dcar - 1
          ashits <- ashits -1
        }
        while((dbshp > 0)&(ashits>0)){
          dbsh <- dbsh - 1
          dbshp <- dbshp - 1
          ashits <- ashits -1
        }
      }
      
      
      if((ades ==0)&(dsub > 0)){
        dshits <- rbinom(1, dsub, 1/6)
        
        while((abshp > absh)&(dshits>0)){
          abshp <- abshp - 1
          dshits <- dshits -1
        }
        while((asub > 0)&(dshits>0)){
          asub <- asub - 1
          dshits <- dshits -1
        }
        while((acrs > 0)&(dshits>0)){
          acrs <- acrs - 1
          dshits <- dshits -1
        }
        while((acar > 0)&(dshits>0)){
          acar <- acar - 1
          dshits <- dshits -1
        }
        while((abshp > 0)&(dshits>0)){
          absh <- absh - 1
          abshp <- abshp - 1
          dshits <- dshits -1
        }
        }
        
      
      ahits <- 
        rbinom(1, ainf_nonart, 1/6) + 
        rbinom(1, ainf_art, 2/6) + 
        rbinom(1, aart, 2/6) + 
        rbinom(1, atnk, 3/6) + 
        rbinom(1, afig, 3/6) +
        rbinom(1, abom, 4/6) +
        bbhits 
      
      aseahits <-
        rbinom(1, ades, 2/6)+
        rbinom(1, acrs, 3/6)+
        rbinom(1, acar, 1/6)+
        rbinom(1, absh, 4/6)
        
      #subs attack in this phase if destroyers present
      if(ddes > 0){
        asubhits <- rbinom(1, asub, 2/6)
      }else{asubhits <- 0}
        
      #set bombard hits to 0 for remaining rounds
      bbhits <- 0
      
      dhits <- 
        rbinom(1, dinf, 2/6) + 
        rbinom(1, dart, 2/6) + 
        rbinom(1, dtnk, 3/6) + 
        rbinom(1, dfig, 4/6) +
        rbinom(1, dbom, 1/6) 
      
      dseahits <-
        rbinom(1, ddes, 2/6)+
        rbinom(1, dcrs, 3/6)+
        rbinom(1, dcar, 2/6)+
        rbinom(1, dbsh, 4/6)
      
      
      if(ades > 0){
        dsubhits <- rbinom(1, dsub, 1/6)
      }else{dsubhits <- 0}
      
      
      #defender takes losses
      
      #destroy AA first if necessary to preserve another unit to hold territory
      if(ahits == nd - 1){
        while((daa > 0)&(ahits>0)){
          daa <- daa - 1
          ahits <- ahits -1
        }
      }
      while((dinf > 0)&(ahits>0)){
        dinf <- dinf - 1
        ahits <- ahits -1
      }
      
      while((dart > 0)&(ahits>0)){
        dart <- dart - 1
        ahits <- ahits -1
      }
      
      while((daa > 0)&(ahits>0)){
        daa <- daa - 1
        ahits <- ahits -1
      }
      
      while((dtnk > 0)&(ahits>0)){
        dtnk <- dtnk - 1
        ahits <- ahits -1
      }
      
      
      while((dbshp > dbsh)&(asubhits > 0)){
        dbshp <- dbshp - 1
        asubhits <- asubhits - 1
      }
      while((dbshp > dbsh)&(aseahits > 0)){
        dbshp <- dbshp - 1
        aseahits <- aseahits - 1
      }
      while((dbshp > dbsh)&(ahits > 0)){
        dbshp <- dbshp - 1
        ahits <- ahits - 1
      }
      
      
      while((dsub > 0)&(asubhits > 0)){
        dsub <- dsub - 1
        asubhits <- asubhits - 1
      }
      while((dsub > 0)&(aseahits > 0)){
        dsub <- dsub - 1
        aseahits <- aseahits - 1
      }
      if(ades > 0){
        while((dsub > 0)&(ahits > 0)){
          dsub <- dsub - 1
          ahits <- ahits - 1
        }
      }
      
      
      while((ddes > 0)&(asubhits > 0)){
        ddes <- ddes - 1
        asubhits <- asubhits - 1
      }
      while((ddes > 0)&(aseahits > 0)){
        ddes <- ddes - 1
        aseahits <- aseahits - 1
      }
      while((ddes > 0)&(ahits > 0)){
        ddes <- ddes - 1
        ahits <- ahits - 1
      }
      
      
      while((dfig > 0)&(aseahits>0)){
        dfig <- dfig - 1
        aseahits <- aseahits -1
      }
      while((dfig > 0)&(ahits>0)){
        dfig <- dfig - 1
        ahits <- ahits -1
      }
      
      while((dbom > 0)&(ahits>0)){
        dbom <- dbom - 1
        ahits <- ahits -1
      }
      
      while((dcrs > 0)&(asubhits > 0)){
        dcrs <- dcrs - 1
        asubhits <- asubhits - 1
      }
      while((dcrs > 0)&(aseahits > 0)){
        dcrs <- dcrs - 1
        aseahits <- aseahits - 1
      }
      while((dcrs > 0)&(ahits > 0)){
        dcrs <- dcrs - 1
        ahits <- ahits - 1
      }
      
      
      while((dcar > 0)&(asubhits > 0)){
        dcar <- dcar - 1
        asubhits <- asubhits - 1
      }
      while((dcar > 0)&(aseahits > 0)){
        dcar <- dcar - 1
        aseahits <- aseahits - 1
      }
      while((dcar > 0)&(ahits > 0)){
        dcar <- dcar - 1
        ahits <- ahits - 1
      }
      
      
      while((dbshp > 0)&(asubhits>0)){
        dbsh <- dbsh - 1
        dbshp <- dbshp - 1
        asubhits <- asubhits -1
      }
      while((dbshp > 0)&(aseahits>0)){
        dbsh <- dbsh - 1
        dbshp <- dbshp - 1
        aseahits <- aseahits -1
      }
      while((dbshp > 0)&(ahits>0)){
        dbsh <- dbsh - 1
        dbshp <- dbshp - 1
        ahits <- ahits -1
      }
      
      
      
      #attacker takes losses
      
      #remove land units until 1 remains, then begin removing planes
      if(sacrifice.planes & (dhits >= ainf + aart + atnk) & (dhits < na)){
        while((ainf + aart + atnk > 1)&(ainf > 0)&(dhits>0)){
          ainf <- ainf - 1
          dhits <- dhits -1
        }
        while((ainf + aart + atnk > 1)&(aart > 0)&(dhits>0)){
          aart <- aart - 1
          dhits <- dhits -1
        }
        while((ainf + aart + atnk > 1)&(atnk > 0)&(dhits>0)){
          atnk <- atnk - 1
          dhits <- dhits -1
        }
        while((afig > 0)&(dhits>0)){
          afig <- afig - 1
          dhits <- dhits -1
        }
        while((abom > 0)&(dhits>0)){
          abom <- abom - 1
          dhits <- dhits -1
        }
      }
      
      while((ainf > 0)&(dhits>0)){
        ainf <- ainf - 1
        dhits <- dhits -1
      }
      while((aart > 0)&(dhits>0)){
        aart <- aart - 1
        dhits <- dhits -1
      }
      while((atnk > 0)&(dhits>0)){
        atnk <- atnk - 1
        dhits <- dhits -1
      }
      
      
      while((abshp > absh)&(dsubhits > 0)){
        abshp <- abshp - 1
        dsubhits <- dsubhits - 1
      }
      while((abshp > absh)&(dseahits > 0)){
        abshp <- abshp - 1
        dseahits <- dseahits - 1
      }
      while((abshp > absh)&(dhits > 0)){
        abshp <- abshp - 1
        dhits <- dhits - 1
      }
      
      
      while((asub > 0)&(dsubhits > 0)){
        asub <- asub - 1
        dsubhits <- dsubhits - 1
      }
      while((asub > 0)&(dseahits > 0)){
        asub <- asub - 1
        dseahits <- dseahits - 1
      }
      if(ddes > 0){
        while((asub > 0)&(dhits > 0)){
          asub <- asub - 1
          dhits <- dhits - 1
        }
      }
      
      
      while((ades > 0)&(dsubhits > 0)){
        ades <- ades - 1
        dsubhits <- dsubhits - 1
      }
      while((ades > 0)&(dseahits > 0)){
        ades <- ades - 1
        dseahits <- dseahits - 1
      }
      while((ades > 0)&(dhits > 0)){
        ades <- ades - 1
        dhits <- dhits - 1
      }
      
      
      while((afig > 0)&(dseahits>0)){
        afig <- afig - 1
        dseahits <- dseahits -1
      }
      while((afig > 0)&(dhits>0)){
        afig <- afig - 1
        dhits <- dhits -1
      }
      
      
      while((acrs > 0)&(dsubhits > 0)){
        acrs <- acrs - 1
        dsubhits <- dsubhits - 1
      }
      while((acrs > 0)&(dseahits > 0)){
        acrs <- acrs - 1
        dseahits <- dseahits - 1
      }
      while((acrs > 0)&(dhits > 0)){
        acrs <- acrs - 1
        dhits <- dhits - 1
      }
      
      
      while((abom > 0)&(dseahits>0)){
        abom <- abom - 1
        dseahits <- dseahits -1
      }
      while((abom > 0)&(dhits>0)){
        abom <- abom - 1
        dhits <- dhits -1
      }
      
      
      while((acar > 0)&(dsubhits > 0)){
        acar <- acar - 1
        dsubhits <- dsubhits - 1
      }
      while((acar > 0)&(dseahits > 0)){
        acar <- acar - 1
        dseahits <- dseahits - 1
      }
      while((acar > 0)&(dhits > 0)){
        acar <- acar - 1
        dhits <- dhits - 1
      }

      
      while((abshp > 0)&(dsubhits>0)){
        absh <- absh - 1
        abshp <- abshp - 1
        dsubhits <- dsubhits -1
      }
      while((abshp > 0)&(dseahits>0)){
        absh <- absh - 1
        abshp <- abshp - 1
        dseahits <- dseahits -1
      }
      while((abshp > 0)&(dhits>0)){
        absh <- absh - 1
        abshp <- abshp - 1
        dhits <- dhits -1
      }
      
      
      
      na <- ainf + aart + atnk + afig + abom + absh + acrs + ades + asub + acar
      nd <- dinf + dart + dtnk + dfig + dbom  + dbsh + dcrs + ddes + dsub + dcar + daa
      
      #AA gun cannot hold territory
      if(nd == daa){nd <- 0}
      
      #break if only planes/subs remain
      if(((na == asub)&(nd == dfig + dbom))|((nd == dsub)&(na == afig + abom))){
        break()
      }
    }
    
    if((nd==0)&(ainf + aart + atnk + absh + asub + acrs + acar + ades >0)){res <- c(res, 'A wins')}
    if((nd>0)&(na==0)){res <- c(res, 'D wins')}
    if((nd == 0)&(na <= afig + abom)){res <- c(res, 'D holds')}
    if((nd>0)&(na>0)){res <- c(res, 'stalemate')}
    
    rd <- c(rd, nd)
    ra <- c(ra, na)
    
  }
  
  df <- data.frame(result = res, remain_def = rd, remain_att = ra)
  
  Result <- res
  x<- (round(table(Result)/100,1))
  print(x)
  
  cat('\nAttacker Units Remaining (% prob >=)\n')
  x<-rev(round(cumsum(rev(table(df$remain_att)/length(df$result)*100)),1))
  print(x)
  
  cat('\nDefender Units Remaining (% prob >=)\n')
  x<-rev(round(cumsum(rev(table(df$remain_def)/length(df$result)*100)),1))
  print(x)
  }


#land/air
battle(
  Ainf = 1,
  Aart = 1,
  Atnk = 0,
  Afig = 1,
  Abom = 0,
  
  #amphibious
  Absh_bbard = 0,
  Acrs_bbard = 0,
  
  #attacker sacrifice planes to take land
  sacrifice.planes = T,
  
  Dinf = 3,
  Dart = 0,
  Dtnk = 0,
  Dfig = 0,
  Dbom = 0,
  Daa = 0
)


#sea
battle(  
  Absh = 0,
  Acrs = 0,
  Ades = 0,
  Asub = 3,
  Acar = 0,
  Afig = 1,
  Abom = 0,
  sacrifice.planes = F,
  
  Dbsh = 1,
  Dcrs = 0,
  Ddes = 0,
  Dsub = 1,
  Dcar = 0,
  Dfig = 0
)
      