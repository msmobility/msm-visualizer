## File loader
loadfiles <- reactive({
  if (global$datapath == getwd()){}
  else {
    print(global$datapath)
    
    ## Define all tables
    o_hhsize <- read.csv(paste(global$datapath,"aveHhSize.csv",sep="/", collapse = NULL))
    o_c_owne <- read.csv(paste(global$datapath,"carOwnership.csv",sep="/", collapse = NULL))
    o_com_di <- read.csv(paste(global$datapath,"commutingDistance.csv",sep="/", collapse = NULL))
    o_dwelli <- read.csv(paste(global$datapath,"dwellings.csv",sep="/", collapse = NULL))
    o_eventc <- read.csv(paste(global$datapath,"eventCounts.csv",sep="/", collapse = NULL))
    o_hhAvIn <- read.csv(paste(global$datapath,"hhAveIncome.csv",sep="/", collapse = NULL))
    o_hhSatR <- read.csv(paste(global$datapath,"hhSatisfactionByRegion.csv",sep="/", collapse = NULL))
    o_hhSize <- read.csv(paste(global$datapath,"hhSize.csv",sep="/", collapse = NULL))
    o_hhType <- read.csv(paste(global$datapath,"hhType.csv",sep="/", collapse = NULL))
    o_laPaRa <- read.csv(paste(global$datapath,"labourParticipationRate.csv",sep="/", collapse = NULL))
    o_lanReg <- read.csv(paste(global$datapath,"landRegions.csv",sep="/", collapse = NULL))
    o_popYea <- read.csv(paste(global$datapath,"popYear.csv",sep="/", collapse = NULL))

    
  }
})