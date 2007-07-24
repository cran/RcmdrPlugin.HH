normalHypothesesPlot <- function(){
    initializeDialog(title=gettextRcmdr("Normal Distribution"))
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    nVar <- tclVar("")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    criticalLowVar <- tclVar("")
    criticalLowEntry <- tkentry(top, width="6", textvariable=criticalLowVar)
    criticalHighVar <- tclVar("1.645")
    criticalHighEntry <- tkentry(top, width="6", textvariable=criticalHighVar)
    obsValueVar <- tclVar("")
    obsValueEntry <- tkentry(top, width="6", textvariable=obsValueVar)
    muAltVar <- tclVar("")
    muAltEntry <- tkentry(top, width="6", textvariable=muAltVar)
    ymaxVar <- tclVar("")
    ymaxEntry <- tkentry(top, width="6", textvariable=ymaxVar)
    onOK <- function(){
      closeDialog()
      mu <- as.numeric(tclvalue(muVar))
      sigma <- as.numeric(tclvalue(sigmaVar))
      n <- as.numeric(tclvalue(nVar))
      criticalLow <- as.numeric(tclvalue(criticalLowVar))
      criticalHigh <- as.numeric(tclvalue(criticalHighVar))
      obsValue <- as.numeric(tclvalue(obsValueVar))
      muAlt <- as.numeric(tclvalue(muAltVar))
      ymax <- as.numeric(tclvalue(ymaxVar))
      
      ## norm.setup(mean=mu, se=sigma)
      ## norm.curve(mean=mu, se=sigma, critical=mean+critical*se, shade="right", col="black", axis.name="z")
      
      command <- "old.par <- par(oma=c(4,0,2,5), mar=c(7,7,4,2)+.1)"
      justDoIt(command)
      logger(command)
      
      if (is.na(sigma)) sigma <- 1
      if (is.na(mu)) mu <- 0
      if (is.na(n) && sigma==1)
        command.se <- ""
      if (is.na(n)) n <- 1
      if (!(exists("command.se") && command.se == ""))
        command.se <- paste(", se=",  sigma, "/sqrt(", n, ")", sep="")
      se <- sigma/sqrt(n)
      
      command.xlim <-
        if (mu !=0 || sigma != 1 || !is.na(muAlt) || !is.na(obsValue)) {
          xlim <-
            if (is.na(muAlt) && is.na(obsValue))
              mu + c(-1,1) * 2.5 * se
            else
              c(min(mu, muAlt, obsValue, na.rm=TRUE) - 2.5 * se,
                max(mu, muAlt, obsValue, na.rm=TRUE) + 2.5 * se)
          paste(", xlim=c(", xlim[1], ", ", xlim[2], ")", sep="")
        } else ""
      
      command.ylim <-
        if (!is.na(ymax)) paste(", ylim=c(0, ", ymax, ")", sep="")
        else ""
      
      command <- paste("norm.setup(mean=", mu,
                       command.se,
                       command.xlim,
                       command.ylim,
                       ")", sep="")
      justDoIt(command)
      logger(command)
      
      critical <- c(criticalLow, criticalHigh)
      critical <- critical[!is.na(critical)]
      shade <- "right"
      if (length(critical)==0) {
        critical <- mu + 20*se  ## big number, out of range of plot
      }
      else
        if (length(critical)==2) {
          critical <- paste("c(", critical[1], ",", critical[2], ")")
          shade="outside"
        }
      command.critical <- paste(", critical=", mu, "+", critical, "*", se)
      
      ## Plot Alternate Hypothesis first
      if (!is.na(muAlt)) {
        command <- paste("norm.curve(mean=", muAlt,
                         command.se,
                         command.critical,
                         ", shade='", "left", "', col='red', axis.name='z1'", ")", sep="")
        doItAndPrint(command)
      }
      
      ## Plot Null Hypothesis second
      command <- paste("norm.curve(mean=", mu,
                       command.se,
                       command.critical,
                       ", shade='", shade, "', col='black', axis.name='z'", ")", sep="")
      doItAndPrint(command)

      ## Observed Value
      if (!is.na(obsValue)) {
        command <- paste("norm.observed(", obsValue, ")", sep="")
        doItAndPrint(command)
      }

      command <- "par(old.par)"
      justDoIt(command)
      logger(command)
      
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="dnorm")
    tkgrid(tklabel(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("n (sample size)")), nEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("z_alpha (critical value)")), criticalLowEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("-z_alpha (critical value)")), criticalHighEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Observed Value")), obsValueEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("mu (Alternate Hypothesis)")), muAltEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("ymax (right-hand side)")), ymaxEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(nEntry, sticky="w")
    tkgrid.configure(criticalLowEntry, sticky="w")
    tkgrid.configure(criticalHighEntry, sticky="w")
    tkgrid.configure(obsValueEntry, sticky="w")
    tkgrid.configure(muAltEntry, sticky="w")
    tkgrid.configure(ymaxEntry, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=muEntry)
    }
