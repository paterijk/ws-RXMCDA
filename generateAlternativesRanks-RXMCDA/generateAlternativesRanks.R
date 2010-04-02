# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < generateAlternativesRanks.R
# example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < generateAlternativesRanks.R

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
	
	# mandatory files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeAlternatives)==0)
	{
		errFile<-"Alternatives file is not XMCDA valid."	
	}
}

# let us now load the optional files

if (is.null(errFile)){
	
	# let us now load the optional xml files
	# if an error occurs, we suppose that the file was not present (optional files!!)
	
	treeSeed <- NULL
	
	tmpErr<-try(
			{
				treeSeed<-xmlTreeParse("seed.xml",useInternalNodes=TRUE)
			}
	)
}

# let us now check if the optional files are valid

if (is.null(errFile)){
	
	# we must now check if the optional files are XMCDA valid
	# for the optional files we first check whether a tree has been loaded
	
	if ((!is.null(treeSeed))){
		if (checkXSD(treeSeed)==0)
		{
			errFile<-"Seed file is not XMCDA valid."	
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
		}
		else 
		{
			errData<-alternativesIDs$status
			flag<-FALSE
		} 	
	}
	
	# for the optional files we must also check whether a tree has been loaded
	
	seed<-NULL
	if (flag){
		if ((!is.null(treeSeed)))
		{
			seedParam<-getParameters(treeSeed, "seed")
			if (seedParam$status == "OK") 
			{
				seed<-seedParam[[1]]
			}
			else
			{
				errData <- seedParam$status
				flag <- FALSE
			}
		}
	}
	
	
	if (is.null(errData))
	{
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					library(gtools)
					
					if (!is.null(seed)){
						set.seed(seed)
					}
					
					#dimension du vecteur de permutation. 
					
					x<-1:numAlt
					
					ordre<-permute(x)
					
					altVals<-c()
					for (i in 1:numAlt)
					{
						altVals<-rbind(altVals,c(i,ordre[i]))	
					}
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot generate ranks."
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
	
	status<-putAlternativesValues(outTree, altVals, altIDs, "alternativesRanks")
	
	status<-saveXML(outTree, file="alternativesRanks.xml")
	
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

