# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotAlternativesValues.R
# example: 
#  R --slave --vanilla --args "${PWD}/plotAlternativesValuesInFiles" "${PWD}/plotAlternativesValuesOutFiles" < plotAlternativesValues.R

rm(list=ls())

library(RXMCDA)

errFile<-NULL
errData<-NULL
errCalc<-NULL

execFlag<-FALSE

# get the in and out directories from the arguments

inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

# set the working directory as the in directory

setwd(inDirectory)

# try to load the mandatory input files

tmpErr<-try(
		{
			treeAlternatives<-xmlTreeParse("alternatives.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the alternatives file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesValues<-xmlTreeParse("alternativesValues.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives values file."
	}
}

if (is.null(errFile)){
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeAlternatives)==0)
	{
		errFile<-"Alternatives file is not XMCDA valid."	
	}
	if (is.null(errFile)){
		if (checkXSD(treeAlternativesValues)==0)
		{
			errFile<-"Alternatives values file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valid according to xsd
	
	errData<-NULL
	flag<-TRUE
	
	numberOfAlternatives<-getNumberOfAlternatives(treeAlternatives)
	
	if (numberOfAlternatives$status == "OK") 
	{
		n<-numberOfAlternatives[[1]]
		alternativesIDs<-getAlternativesIDs(treeAlternatives)
	}
	else {
		errData <- numberOfAlternatives$status
		flag<-FALSE
	}
	
	if(flag){
		if ((alternativesIDs$status == "OK")) 
		{
			altIDs<-alternativesIDs[[1]]
			altVals<-getAlternativesValues(treeAlternativesValues, altIDs)
		}
		else
		{
			errData <- alternativesIDs$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (altVals$status == "OK") 
		{
			alternativesValues<-altVals[[1]]
			if (is.null(alternativesValues))
			{
				errData<- "Could not read values on the alternatives."
				flag<-FALSE
			}
		}
		else
		{
			errData <- altVals$status
			flag <- FALSE	
		}
	}
	
	# calculate the number of alternatives left after the filtering process of altIDs on alternativesValues
	
	if (flag){
		if(dim(alternativesValues)[1] != 0)
		{
			n <- dim(alternativesValues)[1]
		}
		else
		{
			errData <- "No alternatives left to plot values."
		}
	}
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					setwd(outDirectory)
					
					pdf("out.pdf", width=n*1.5)
#					bitmap("out.png",type="pdfwrite", units="px", width=150*n, height=600, pointsize=18)
					
					barplot(alternativesValues[,2], 
							names.arg=altIDs[alternativesValues[,1]], density=5*(1:length(alternativesValues[,2])), cex.axis=1.5, cex.names=1.5, ylim=c(min(0,min(alternativesValues[,2])),max(alternativesValues[,2])), space=0.05)
					dev.off()
					
					system("convert out.pdf out.png")
					
					system("base64 -w0 out.png > out.base64")
					tmp<-readLines(file("out.base64","rt"))
					system("rm out.base64 out.png out.pdf")
					
					closeAllConnections()
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot plot alternatives values."
		} else {
			execFlag<-TRUE
		}
	}
}

if (execFlag){
	
	# execution was successfull and we can write the output files
	# Note however that: 
	# the optimization method might find no solution
	# in that case, errData is not null, and it is not possible to write the solutions
	
	setwd(outDirectory)
	
	# first the result file
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesPlot(outTree, tmp, altIDs[alternativesValues[,1]], mcdaConcept=names(altVals)[1])
	
	status<-saveXML(outTree, file="alternativesValuesPlot.xml")
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	status<-saveXML(outTree2, file="messages.xml")
}

if (!is.null(errCalc)){
	
	# something went wrong at the calculation step
	
	setwd(outDirectory)
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putErrorMessage(outTree2, errCalc, name="Error")
	
	status<-saveXML(outTree2, file="messages.xml")
	
}

if (!is.null(errData)){
	
	# something went wrong at the data extraction steps
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errData, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}


if (!is.null(errFile)){
	
	# something went wrong while loading the files
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errFile, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}
