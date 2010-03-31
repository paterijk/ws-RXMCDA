# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < convertAlternativesRanksToAlternativesComparisons.R
# example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < convertAlternativesRanksToAlternativesComparisons.R

rm(list=ls())

# on commence par charger la librairie XMCDA qui permet de parser des fichiers XMCDA

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
				treeAlternativesRanks<-xmlTreeParse("alternativesRanks.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives ranks file."
	}
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeSeparationThreshold<-xmlTreeParse("separationThreshold.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the parameters file."
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
		if (checkXSD(treeAlternativesRanks)==0)
		{
			errFile<-"Alternatives ranks file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeSeparationThreshold)==0)
		{
			errFile<-"Separation threshold file is not XMCDA valid."	
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
		alternativesIDs<-getAlternativesIDs(treeAlternatives)
	}
	else {
		errData <- numberOfAlternatives$status
		flag<-FALSE
	}
	if (flag){
		if (alternativesIDs$status == "OK")
		{
			altIDs<-alternativesIDs[[1]]
			alternativesRanks<-getAlternativesValues(treeAlternativesRanks, altIDs)
		}
		else 
		{
			errData<-alternativesIDs$status
			flag<-FALSE
		} 	
	}
	if (flag){
		if (alternativesRanks$status == "OK")
		{
			altRanks<-alternativesRanks[[1]]
			separationThresholds<-getParameters(treeSeparationThreshold, "separationThreshold")
		}
		else 
		{
			errData<-alternativesRanks$status
			flag<-FALSE
		} 	
	}
	if (flag){
		if (separationThresholds$status == "OK")
		{
			sepThr<-separationThresholds[[1]]
		}
		else 
		{
			errData<-separationThresholds$status
			flag<-FALSE
		} 	
	}
	
	
	if (is.null(errData))
	{
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					
					orderedAltRanks <- altRanks[order(altRanks[,2],decreasing=TRUE),]
					order<-c()
					for (i in 1:(dim(orderedAltRanks)[1]-1)){
						order<-rbind(order,c(altIDs[orderedAltRanks[i,1]], altIDs[orderedAltRanks[i+1,1]], sepThr))		
					}
					
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot convert ranks to comparisons."
		} else {
			execFlag<-TRUE
		}
	}
}

# Si le calcul s'est bien passé, on doit écrire les fichiers de sortie:
# - overallValues.xml contenant la valeur de chaque alternative après Choquet,
# - messages.xml contenant l'information sur la bonne exécution du calcul.

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
	
	status<-putAlternativesComparisonsLabels(outTree, order, "alternativesOrder")
	
	status<-saveXML(outTree, file="alternativesOrder.xml")
	
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

# Si tout à l'heure le calcul s'est mal passé, il faut l'indiquer. Dans ce cas, on ne renvoit qu'un fichier: 
# messages.xml, qui contient une description de l'erreur. 

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

# Si tout à l'heure l'extraction de l'info des arbres XML s'est mal passé, il faut l'indiquer. Dans ce cas, on ne renvoit qu'un fichier: 
# messages.xml, qui contient une description de l'erreur. 

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

# Si tout à l'heure le chargement des fichiers XML s'est mal passé, il faut l'indiquer. Dans ce cas, on ne renvoit qu'un fichier: 
# messages.xml, qui contient une description de l'erreur. 

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

