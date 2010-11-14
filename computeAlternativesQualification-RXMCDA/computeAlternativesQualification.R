# usage:
# R --slave --vanilla --file=computeAlternativesQualification.R --args "[inDirectory]" "[outDirectory]" "[alternativesFile]" "[outrankingRelationFile]" "[weaknessFile]" "[strengthFile]" "[qualificationFile]" "[messagesFile]"
# example: 
# R --slave --vanilla --file=computeAlternativesQualification.R --args "${PWD}/computeAlternativesQualificationInFiles" "${PWD}/computeAlternativesQualificationOutFiles" "alternatives.xml" "alternativesComparisons.xml" "alternativesWeakness.xml" "alternativesStrength.xml" "alternativesQualification.xml" "messages.xml"

rm(list=ls())

library(RXMCDA)

errFile<-NULL
errData<-NULL
errCalc<-NULL

execFlag<-FALSE



# get the in and out directories from the arguments

inDirectory <- commandArgs(trailingOnly=TRUE)[1]
outDirectory <- commandArgs(trailingOnly=TRUE)[2]
alternativesFile <- commandArgs(trailingOnly=TRUE)[3]
outrankingRelationFile <- commandArgs(trailingOnly=TRUE)[4]
weaknessFile <- commandArgs(trailingOnly=TRUE)[5]
strengthFile <- commandArgs(trailingOnly=TRUE)[6]
qualificationFile <- commandArgs(trailingOnly=TRUE)[7]
messagesFile <- commandArgs(trailingOnly=TRUE)[8]

# set the working directory as the in directory

setwd(inDirectory)

# try to load the mandatory input files

tmpErr<-try(
		{
			treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the alternatives file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesComparisons<-xmlTreeParse(outrankingRelationFile,useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives comparisons file."
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
		if (checkXSD(treeAlternativesComparisons)==0)
		{
			errFile<-"Alternatives comparisons file is not XMCDA valid."	
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
		numAlt<-numberOfAlternatives[[1]]
		
		if (numAlt == 1){
			errData <- "At least 2 alternatives required."
			flag<-FALSE
		}
		else 
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
			alternativesComparisonsLabels<-getAlternativesComparisonsLabels(treeAlternativesComparisons,altIDs)
		}
		else
		{
			errData <- alternativesIDs$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (alternativesComparisonsLabels$status == "OK") 
		{
			alternativesComparisons<-alternativesComparisonsLabels[[1]]
			if (is.null(alternativesComparisons))
			{
				errData<- "Could not read any comparisons on the alternatives."
				flag<-FALSE
			}
		}
		else
		{
			errData <- alternativesComparisonsLabels$status
			flag <- FALSE	
		}
	}
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		tmpErr<-try(
				{
					# let us first calculate the strength
					# we need a valued outranking relation with 0s and 1s (which means a cut relation)! 

					altIDsInRelation <- intersect(altIDs,c(alternativesComparisons[,1], alternativesComparisons[,2]))

					strength <-c()
					for (i in 1:length(altIDsInRelation)){
						strength<-rbind(strength, c(i,0))
					}
		
					for (i in 1:(length(altIDsInRelation))){
						for (j in 1:(dim(alternativesComparisons)[1])){
							if (!is.na(alternativesComparisons[j,3])){
								# if the value is not loaded, then we have an NA in the third column. 
								if( (alternativesComparisons[j,1]==altIDsInRelation[i]) & (alternativesComparisons[j,3]==1)){
									strength[i,2] <- strength[i,2]+1
								}
							}
						}
					}

					# let us now calculate the weakness
					# we need a valued outranking relation with 0s and 1s (which means a cut relation)! 
					weakness <-c()
					for (i in 1:length(altIDsInRelation)){
						weakness<-rbind(weakness, c(i,0))
					}
		
					for (i in 1:(length(altIDsInRelation))){
						for (j in 1:(dim(alternativesComparisons)[1])){
							if (!is.na(alternativesComparisons[j,3])){
								# if the value is not loaded, then we have an NA in the third column. 
								if( (alternativesComparisons[j,2]==altIDsInRelation[i]) & (alternativesComparisons[j,3]==1)){
									weakness[i,2] <- weakness[i,2]+1
								}
							}
						}
					}

					# let us now calculate the qualification

					qualification <-c()
					for (i in 1:length(altIDsInRelation)){
						qualification<-rbind(qualification, c(i,0))
					}
			
					qualification[,2] <- strength[,2] - weakness[,2]
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot compute qualification."
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
	
	# first the result files
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesValues(outTree, strength, altIDsInRelation, "strength")
	
	status<-saveXML(outTree, file=strengthFile)

	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesValues(outTree, weakness, altIDsInRelation, "weakness")
	
	status<-saveXML(outTree, file=weaknessFile)

	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesValues(outTree, qualification, altIDsInRelation, "qualification")
	
	status<-saveXML(outTree, file=qualificationFile)
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	status<-saveXML(outTree2, file=messagesFile)
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
	
	status<-saveXML(outTree2, file=messagesFile)
	
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
	
	status<-saveXML(outTree, file=messagesFile)
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
	
	status<-saveXML(outTree, file=messagesFile)
}
