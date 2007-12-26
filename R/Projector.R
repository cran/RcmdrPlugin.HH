Projector <- function() {
  command <-
    'options(Rcmdr=list(
           suppress.menus=TRUE,
           RExcelSupport=TRUE,
           plugins="RcmdrPlugin.HH",
           log.font.size = 15,
           log.width = 54,
           log.height = 6, 
           log.commands = TRUE,
           output.height = 18,
           console.output = FALSE, 
           contrasts = c("contr.Treatment", "contr.poly"),
           grab.focus = TRUE, 
           double.click = TRUE,
           sort.names = TRUE,
           scale.factor = 1.4))'
  doItAndPrint(command)

  command <- 'trellis.par.set(list(
                 superpose.symbol=list(pch=rep(16, length(trellis.par.get("superpose.symbol")$pch))),
                 plot.symbol=list(pch=16)))'
  doItAndPrint(command)

  command <- 'par(pch=16)'
  doItAndPrint(command)

  command <- '{
tkmessageBox("-message", "Click the Blue R on the Excel Rcmdr menu\nRCommander: Update menus")
putRcmdr("autoRestart", TRUE)
closeCommander(ask=FALSE)
Commander()
}'
  if (!is.na(options()$Rcmdr$RExcelSupport) & options()$Rcmdr$RExcelSupport)
    doItAndPrint(command)
}

## source("~/HH-R.package/RcmdrPlugin.HH/R/Projector.R")
