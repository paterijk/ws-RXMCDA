# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < lsRankingCapaIdent.R
# example: 
#  R --slave --vanilla --args "${PWD}/lsRankingCapaIdentInFiles" "${PWD}/lsRankingCapaIdentOutFiles" < lsRankingCapaIdent.R


rm(list=ls())
library(kappalab)

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
			treeKAdditivity<-xmlTreeParse("kAdditivity.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the k-additivity parameters file."
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
				treeAlternativesPreorder<-xmlTreeParse("alternativesPreorder.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives preorder file."
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
	tmpErr<-try(
			{
				treeSeparation<-xmlTreeParse("separationThreshold.xml", useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the separation threshold parameters file."
	}
}

if (is.null(errFile)){
	
	# mandatory files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeKAdditivity)==0)
	{
		errFile<-"k-additivity parameters file is not XMCDA valid."	
	}
	if (is.null(errFile)){
		if (checkXSD(treeCriteria)==0)
		{
			errFile<-"Criteria file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeAlternativesPreorder)==0)
		{
			errFile<-"Alternatives preorder file is not XMCDA valid."	
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
	if (is.null(errFile)){
		if (checkXSD(treeSeparation)==0)
		{
			errFile<-"Separation threshold parameters file is not XMCDA valid."	
		}
	}
}

if (is.null(errFile)){
	
	# let us now load the optional xml files
	# if an error occurs, we suppose that the file was not present (optional files!!)
	
	treeShapleyPreorder<-NULL
	treeShapleyInterval<-NULL
	treeInteractionPreorder<-NULL 
	treeInteractionInterval<-NULL
	
	tmpErr<-try(
			{
				treeShapleyPreorder<-xmlTreeParse("shapleyPreorder.xml",useInternalNodes=TRUE)
			}
	)
	tmpErr<-try(
			{
				treeShapleyInterval<-xmlTreeParse("shapleyInterval.xml",useInternalNodes=TRUE)
			}
	)
	tmpErr<-try(
			{
				treeInteractionPreorder<-xmlTreeParse("interactionPreorder.xml",useInternalNodes=TRUE)
			}
	)
	tmpErr<-try(
			{
				treeInteractionInterval<-xmlTreeParse("interactionInterval.xml",useInternalNodes=TRUE)
			}
	)
}

if (is.null(errFile)){
	
	# we must now check if the optional files are XMCDA valid
	# for the optional files we first check whether a tree has been loaded
	
	if ((!is.null(treeShapleyPreorder))){
		if (checkXSD(treeShapleyPreorder)==0)
		{
			errFile<-"Shapley preorder file is not XMCDA valid."	
		}
	}	
	if (is.null(errFile)){
		if ((!is.null(treeShapleyInterval))){
			if (checkXSD(treeShapleyInterval)==0)
			{
				errFile<-"Shapley interval file is not XMCDA valid."	
			}
		}
	}
	if (is.null(errFile)){
		if ((!is.null(treeInteractionPreorder))){
			if (checkXSD(treeInteractionPreorder)==0)
			{
				errFile<-"Interaction preorder file is not XMCDA valid."	
			}
		}	
	}
	if (is.null(errFile)){
		if ((!is.null(treeInteractionInterval))){
			if (checkXSD(treeInteractionInterval)==0)
			{
				errFile<-"Interaction intervals file is not XMCDA valid."	
			}
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valid according to xsd
	
	errData<-NULL
	
	numberOfCriteria<-getNumberOfCriteria(treeCriteria)
	
	flag<-TRUE
	
	if (numberOfCriteria$status == "OK") 
	{
		n<-numberOfCriteria[[1]]
		if (is.null(n))
		{
			errData<- "Could not read number of criteria."
			flag<-FALSE
		}
		criteriaIDs<-getCriteriaIDs(treeCriteria)
	}
	else
	{
		errData <- numberOfCriteria$status
		flag <- FALSE
	}
	
	if (flag){
		if ((criteriaIDs$status == "OK")) 
		{
			critIDs<-criteriaIDs[[1]]
			if (is.null(critIDs))
			{
				errData<- "Could not read any criteria IDs."
				flag<-FALSE
			}
			alternativesIDs<-getAlternativesIDs(treeAlternatives)
		}
		else
		{
			errData <- criteriaIDs$status
			flag <- FALSE
		}
	}
	
	if (flag){
		if ((alternativesIDs$status == "OK")) 
		{
			altIDs<-alternativesIDs[[1]]
			if (is.null(altIDs))
			{
				errData<- "Could not read any alternatives IDs."
				flag<-FALSE
			}
			performanceTables<-getPerformanceTables(treePerformanceTable, altIDs=altIDs,critIDs=critIDs)
		}
		else
		{
			errData<-alternativesID$status
			flag<-FALSE
		}
	}
	if (flag){
		if ((performanceTables$status == "OK")) 
		{
			perfTable<-performanceTables[[1]]
			if (is.null(perfTable))
			{
				errData<- "Could not read any performance table."
				flag<-FALSE
			}
			options<-getParameters(treeKAdditivity, "kAdditivity")
		}
		else
		{
			errData<-performanceTables$status
			flag<-FALSE
		}
	}
	if (flag){
		if ((options$status == "OK"))
		{
			k<-options[[1]]
			if (is.null(k))
			{
				errData<- "Could not read value for the k-additivity."
				flag<-FALSE
			}
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
			alternativesComparisonsLabels<-getAlternativesComparisonsLabels(treeAlternativesPreorder,altIDs)
		}
		else
		{
			errData <- "Invalid value for kAdditivity."
			flag <- FALSE
		}
	}
	if (flag){
		if (alternativesComparisonsLabels$status == "OK") 
		{
			alternativesPreorder<-alternativesComparisonsLabels[[1]]
			if (is.null(alternativesPreorder))
			{
				errData<- "Could not read alternatives preorder."
				flag<-FALSE
			}
			epsilons<-getParameters(treeSeparation, "separationThreshold")
		}
		else
		{
			errData <- alternativesComparisons$status
			flag <- FALSE
		}
	}
	if (flag){
		if (epsilons$status == "OK") 
		{
			epsilon<-epsilons[[1]]
			if (is.null(epsilon))
			{
				errData<- "Could not read minimal separation threshold."
				flag<-FALSE
			}
		}
		else
		{
			errData <- epsilons$status
			flag <- FALSE
		}
	}
	
	# for the optional files we must also check whether a tree has been loaded
	
	shapleyPreorder<-NULL
	if (flag){
		if ((!is.null(treeShapleyPreorder)))
		{
			criteriaComparisons<-getCriteriaComparisons(treeShapleyPreorder, critIDs)
			if (criteriaComparisons$status == "OK") 
			{
				shapleyPreorder<-criteriaComparisons[[1]]
			}
			else
			{
				errData <- criteriaComparisons$status
				flag <- FALSE
			}
		}
	}
	shapleyInterval<-NULL
	if (flag){	
		if ((!is.null(treeShapleyInterval)))
		{
			criteriaIntervals<-getCriteriaIntervalValues(treeShapleyInterval, critIDs)
			if (criteriaIntervals$status == "OK") 
			{
				shapleyInterval<-criteriaIntervals[[1]]
			}
			else
			{
				errData <- criteriaIntervals$status
				flag <- FALSE
			}
		}
	}
	interactionPreorder<-NULL
	if (flag){	
		if ((!is.null(treeInteractionPreorder)))
		{
			interactionPreorders<-getCriteriaPairsComparisons(treeInteractionPreorder, critIDs)
			if (interactionPreorders$status == "OK") 
			{
				interactionPreorder<-interactionPreorders[[1]]
			}
			else
			{
				errData <- interactionPreorders$status
				flag <- FALSE
			}
		}
	}
	interactionInterval<-NULL
	if (flag){	
		if ((!is.null(treeInteractionInterval)))
		{
			interactionIntervals<-getCriteriaPairsIntervalValues(treeInteractionInterval, critIDs)
			if (interactionIntervals$status == "OK") 
			{
				interactionInterval<-interactionIntervals[[1]]
			}
			else
			{
				errData <- interactionIntervals$status
				flag <- FALSE
			}
		}
	}
	
	if (is.null(errData))
	{
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		# we need to create the following matrix by hand, because in case of only one
		# constraint, R considers that it is no matrix (how dumb)
		
		altPreorder<-matrix(nrow=0,ncol=2)
		for (i in 1:dim(alternativesPreorder)[1]){
			altPreorder<-rbind(altPreorder,alternativesPreorder[i,c(1,2)])
		}
		
		tmpErr<-try(
				{	
					output<-ls.ranking.capa.ident(n,
							k,
							perfTable,
							altPreorder,
							epsilon,
							A.Shapley.preorder=shapleyPreorder,
							A.interaction.preorder=interactionPreorder,
							A.Shapley.interval = shapleyInterval,
							A.interaction.interval = interactionInterval,
							sigf = 5
					)
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"No solution found."
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
	
	status<-putCapacity(outTree, output$solution, critIDs, "mobiusCapacity")
	
	status<-saveXML(outTree, file="mobiusCapacity.xml")
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	status<-putLogMessage(outTree2, output$how, name="solverStatus")
	
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


