# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < choquetIntegral.R
# example: 
# R --slave --vanilla --args "${PWD}/choquetIntegralInFiles" "${PWD}/choquetIntegralOutFiles" < choquetIntegral.R

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
			treeKAdditivity<-xmlTreeParse("kAdditivity.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the parameters file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeCriteria<-xmlTreeParse("criteria.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the criteria file."
	}
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeMobiusCapacity<-xmlTreeParse("mobiusCapacity.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the Mobius capacity file."
	}
}	
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternatives<-xmlTreeParse("alternatives.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives file."
	}
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treePerformanceTable<-xmlTreeParse("performanceTable.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the performance table file."
	}
}

if (is.null(errFile)){
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeKAdditivity)==0)
	{
		errFile<-"Parameters file is not XMCDA valid."	
	}
	if (is.null(errFile)){
		if (checkXSD(treeCriteria)==0)
		{
			errFile<-"Criteria file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeMobiusCapacity)==0)
		{
			errFile<-"Mobius capacity file is not XMCDA valid."	
		}
	}	
	if (is.null(errFile)){
		if (checkXSD(treeAlternatives)==0)
		{
			errFile<-"Alternatives file is not XMCDA valid."	
		}
	}	
	if (is.null(errFile)){
		if (checkXSD(treePerformanceTable)==0)
		{
			errFile<-"Performance table file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valid according to xsd
	
	errData<-NULL
	
	flag<-TRUE
	
	numberOfCriteria<-getNumberOfCriteria(treeCriteria)
	
	
	if (numberOfCriteria$status == "OK") 
	{
		n<-numberOfCriteria[[1]]
		criteriaIDs<-getCriteriaIDs(treeCriteria)
	}
	else {
		errData <- numberOfCriteria$status
		flag<-FALSE
	}
	if (flag){
		if ((criteriaIDs$status == "OK")) 
		{
			critIDs<-criteriaIDs[[1]]
			options<-getParameters(treeKAdditivity, "kAdditivity")
		}
		else
		{
			errData <- criteriaIDs$status
			flag<-FALSE
		}
	}
	if (flag){
		if ((options$status == "OK"))
		{
			k<-options[[1]]
		}
		else
		{
			errData<-options$status
			flag<-FALSE
		}
	}
	if (flag){
		if ((k>=1)&(k<=n))
		{
			mobiusCapacities<-getMobiusCapacities(treeMobiusCapacity, critIDs, n, k)
		}
		else 
		{
			errData <- "Invalid value for kAdditivity."
			flag <- FALSE
		}
	}
	if (flag){
		if (mobiusCapacities$status == "OK")
		{
			m<-mobiusCapacities[[1]]
			alternativesIDs<-getAlternativesIDs(treeAlternatives)
		}
		else 
		{
			errData<-mobiusCapacities$status
			flag<-FALSE
		} 	
	}
	if (flag){
		if (alternativesIDs$status == "OK")
		{
			altIDs<-alternativesIDs[[1]]
			performanceTables<-getPerformanceTables(treePerformanceTable,altIDs=altIDs, critIDs=critIDs)
		}
		else 
		{
			errData<-alternativesIDs$status
			flag<-FALSE
		} 	
	}
	if (flag){
		if (performanceTables$status == "OK")
		{
			perfTable<-performanceTables[[1]]
		}
		else 
		{
			errData<-performanceTables$status	
		} 	
	}
	
	if (is.null(errData))
	{
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					altVals<-c()
					for (i in 1:dim(perfTable)[1])
					{
						altVals<-rbind(altVals,c(i,Choquet.integral(m,perfTable[i,])))	
					}
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot calculate Choquet integral."
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
	
	status<-putAlternativesValues(outTree, altVals, rownames(perfTable), "overallValues")
	
	status<-saveXML(outTree, file="overallValues.xml")
	
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

