# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < alternativesValuesKendall.R
# example: 
#  R --slave --vanilla --args "${PWD}/multipleAlternativesValuesKendallsInFiles" "${PWD}/multipleAlternativesValuesKendallsOutFiles" < multipleAlternativesValuesKendalls.R



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

# all the files from the in directory

files<-list.files(inDirectory, pattern = "\\.xml$")

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
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeAlternatives)==0)
		{
			errFile<-"Alternatives file is not XMCDA valid."	
		}
}

treeList <- list()

for (i in 1:(length(files)-1)){
	# we start at two to get the alternativesValues files
	
	err1<-NULL
	tmpErr<-NULL
	tmpErr<-try(
			{
				tree<-xmlTreeParse(files[i],useInternalNodes=TRUE)
			}
	)
	
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read an xml file."
	} else {
		# check if files are valid according to xsd
		if (checkXSD(tree)==0){
			errFile<-"At least one input file containing a ranking is not valid according to the XMCDA XML schema."
		}
		# add the tree to the list of trees
		treeList <- c(treeList, list(tree))
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
		}
		else {
			errData <- numberOfAlternatives$status
			flag<-FALSE
		}
	}

###############
	if (flag){
		kendallValues<-c()
		rankingLabels<-c()
		k<-1

		for (i in 1:(length(files)-2)){
			for (j in (i+1):(length(files)-1)){
	
				altValues1<-getAlternativesValues(treeList[[i]], altIDs)
				altValues2<-getAlternativesValues(treeList[[j]], altIDs)
				print(paste(i,j))
				print(altValues1)
				print(altValues2)
				if (altValues1$status == "OK") 
				{
					altVals1<-altValues1[[1]]
				}
				else
				{
					errData <- altValues1$status
					flag<-FALSE
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

				if (flag){
					# all data has been extracted successfully, now we can get to the calculation
					tmpErr<-try(
							{
								orderedAltVals1<-altVals1[order(altVals1[,1],decreasing=FALSE),]
								orderedAltVals2<-altVals2[order(altVals2[,1],decreasing=FALSE),]
								kendallValues<-rbind(kendallValues,c(k,cor(orderedAltVals1[,2], orderedAltVals2[,2], method="kendall")))
								k<-k+1
								rankingLabels<-c(rankingLabels, paste(i, j, sep=","))
							}
					)				
				}
				
				if (inherits(tmpErr, 'try-error')){
					errCalc<-"Cannot calculate Kendall's tau on some of the alternatives' values."
				} else {
					execFlag<-TRUE
				}
	
			}
		}
	}
}

if (execFlag){
	
	# execution was successfull and we can write the output files
	
	setwd(outDirectory)
	
	# first the result file
	
	outTree = newXMLDoc()
		
	newXMLNode("xmcda:XMCDA", 
				attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
				suppressNamespaceWarning=TRUE, 
				namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"), 
				parent=outTree)
		
	status<-putAlternativesValues(outTree, kendallValues, rankingLabels, "kendallCorrelation")
		
	status<-saveXML(outTree, file="kendall.xml")
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	status<-saveXML(outTree2, file="messages.xml")
}
	
if (!is.null(errCalc)){
	
	# something went wrong at the calculation step
	
	setwd(outDirectory)
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"), 
			parent=outTree2)
	
	status<-putErrorMessage(outTree2, errCalc, name="Error")
	
	status<-saveXML(outTree2, file="messages.xml")
	
}

if (!is.null(errData)){
	
	# something went wrong at the data extraction steps
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errData, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}


if (!is.null(errFile)){
	
	# something went wrong while loading the files
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.1.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.1.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.1.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errFile, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}

