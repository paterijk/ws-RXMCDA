# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < alternativesValuesKendall.R
# example: 
#  R --slave --vanilla --args "${PWD}/alternativesValuesKendallInFiles" "${PWD}/alternativesValuesKendallOutFiles" < alternativesValuesKendall.R

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

# try to load the input files

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
				treeAlternativesValues1<-xmlTreeParse("alternativesValues1.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the first alternatives' values file."
	}
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesValues2<-xmlTreeParse("alternativesValues2.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the second alternatives' values file."
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
		if (checkXSD(treeAlternativesValues1)==0)
		{
			errFile<-"First alternatives' values file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeAlternativesValues2)==0)
		{
			errFile<-"Second alternatives' values file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valide according to xsd
	
	errData<-NULL
	
	flag<-TRUE
	
	alternativesIDs <- getAlternativesIDs(treeAlternatives)
	
	if (alternativesIDs$status == "OK") 
	{
		altIDs<-alternativesIDs[[1]]
		numberOfAlternatives<-getNumberOfAlternatives(treeAlternatives)
	}
	else {
		errData <- alternativesIDs$status
		flag<-FALSE
	}
	
	if (flag){
		if (numberOfAlternatives$status == "OK") 
		{
			numAlt<-numberOfAlternatives[[1]]
			altValues1<-getAlternativesValues(treeAlternativesValues1, altIDs)
		}
		else {
			errData <- numberOfAlternatives$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if ((altValues1$status == "OK")) 
		{
			altVals1<-altValues1[[1]]
			altValues2<-getAlternativesValues(treeAlternativesValues2, altIDs)
		}
		else
		{
			errData <- altValues1$status
			flag<-FALSE
		}
	}	
	
	if (flag){
		if ((altValues2$status == "OK")) 
		{
			altVals2<-altValues2[[1]]
		}
		else
		{
			errData <- altValues2$status
			flag<-FALSE
		}
	}	
	
	if (is.null(errData))
	{
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					orderedAltVals1<-altVals1[order(altVals1[,1],decreasing=FALSE),]
					orderedAltVals2<-altVals2[order(altVals2[,1],decreasing=FALSE),]
					kendall<-cor(orderedAltVals1[,2], orderedAltVals2[,2], method="kendall")
				}
		)
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot calculate Kendall's tau on given alternatives' values."
		} else {
			execFlag<-TRUE
		}
	}
}

if (execFlag){
	
	# execution was successfull and we can write the output files
	
	setwd(outDirectory)
	
	# first the result file
	
	outTree = newXMLDoc()
		
	newXMLNode("xmcda:XMCDA", 
				attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
				suppressNamespaceWarning=TRUE, 
				namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
				parent=outTree)
		
	status<-putAlternativeValue(outTree, kendall, altIDs[altVals1[,1]], "kendallCorrelation")
		
	status<-saveXML(outTree, file="kendall.xml")
	
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

