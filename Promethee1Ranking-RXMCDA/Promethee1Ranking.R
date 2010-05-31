# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < Promethee1Ranking.R
# example: 
#  R --slave --vanilla --args "${PWD}/Promethee1RankingInFiles" "${PWD}/Promethee1RankingOutFiles" < Promethee1Ranking.R

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
				treeAlternativesValues1<-xmlTreeParse("positiveFlows.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the positive flows file."
	}
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesValues2<-xmlTreeParse("negativeFlows.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the negative flows file."
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
			errFile<-"Positive flows file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeAlternativesValues2)==0)
		{
			errFile<-"Negative flows file is not XMCDA valid."	
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
			positiveFlows<-altValues1[[1]]
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
			negativeFlows<-altValues2[[1]]
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
					
					
					# we first check whether both flow vectors contain the same alternatives 
					if ((length(setdiff(positiveFlows[,1],negativeFlows[,1])) == 0) & (length(setdiff(negativeFlows[,1],positiveFlows[,1])) == 0)){
						
						# we order the positiveFlows and negativeFlows matrixes according to the indexes of the alternatives
						orderedPositiveFlows<-positiveFlows[order(positiveFlows[,1],decreasing=FALSE),]
						orderedNegativeFlows<-negativeFlows[order(negativeFlows[,1],decreasing=FALSE),]
						
						# we define the matrix containing the alternatives comparisons (the P1 ranking)
						
						alternativesComp <- matrix(nrow=0,ncol=2)
						
						# then we apply the formula to determine the promethee 1 ranking
						# (we can do this simply on the ordered flows because we have tested that the indexes are the same)
						for (i in 1:(length(orderedPositiveFlows[,1]))){
							for (j in i:(length(orderedPositiveFlows[,1]))){
								stop<-FALSE
								
								
								if (((orderedPositiveFlows[i,2] > orderedPositiveFlows[j,2]) & (orderedNegativeFlows[i,2] < orderedNegativeFlows[j,2]))
										| ((orderedPositiveFlows[i,2] == orderedPositiveFlows[j,2]) & (orderedNegativeFlows[i,2] < orderedNegativeFlows[j,2]))
										| ((orderedPositiveFlows[i,2] > orderedPositiveFlows[j,2]) & (orderedNegativeFlows[i,2] == orderedNegativeFlows[j,2]))) {
									# strict preference i>j
									alternativesComp <-  rbind(alternativesComp,c(altIDs[orderedPositiveFlows[i,1]],altIDs[orderedPositiveFlows[j,1]]))
									stop<-TRUE
								}
								if(!stop){
									if (((orderedPositiveFlows[i,2] == orderedPositiveFlows[j,2]) & (orderedNegativeFlows[i,2] == orderedNegativeFlows[j,2]))){
										# indifference i=j
										if (orderedPositiveFlows[i,1] == orderedPositiveFlows[j,1]){
											# if it is twice the same alternative, only one loop is added
											alternativesComp <-  rbind(alternativesComp,c(altIDs[orderedPositiveFlows[i,1]],altIDs[orderedPositiveFlows[j,1]]))
										}
										else{
											alternativesComp <-  rbind(alternativesComp,c(altIDs[orderedPositiveFlows[i,1]],altIDs[orderedPositiveFlows[j,1]]))
											alternativesComp <-  rbind(alternativesComp,c(altIDs[orderedPositiveFlows[j,1]],altIDs[orderedPositiveFlows[i,1]]))	
										}
										stop<-TRUE
									}
								}
								if(!stop){
									# check if strict preference j>i
									if (((orderedPositiveFlows[j,2] > orderedPositiveFlows[i,2]) & (orderedNegativeFlows[j,2] < orderedNegativeFlows[i,2]))
											| ((orderedPositiveFlows[j,2] == orderedPositiveFlows[i,2]) & (orderedNegativeFlows[j,2] < orderedNegativeFlows[i,2]))
											| ((orderedPositiveFlows[j,2] > orderedPositiveFlows[i,2]) & (orderedNegativeFlows[j,2] == orderedNegativeFlows[i,2]))) {
										alternativesComp <-  rbind(alternativesComp,c(altIDs[orderedPositiveFlows[j,1]],altIDs[orderedPositiveFlows[i,1]]))
										stop<-TRUE
									}
								}
								# and the remaining comparisons represent incomparabilities
							}
						}
						
					}				
				}
		)
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot calculate Promethee 1 ranking on given alternatives' values."
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
	
	status<-putAlternativesComparisonsLabels(outTree, alternativesComp, "Promethee1")
	
	status<-saveXML(outTree, file="promethee1.xml")
	
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

