# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < generateCriteriaWeights.R
# example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < generateCriteriaWeights.R

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
			treeCriteria<-xmlTreeParse("criteria.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the criteria file."
}

if (is.null(errFile)){
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeCriteria)==0)
	{
		errFile<-"Criteria file is not XMCDA valid."	
	}
}

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
	
	numberOfCriteria<-getNumberOfCriteria(treeCriteria)
	
	
	if (numberOfCriteria$status == "OK") 
	{
		numCrit<-numberOfCriteria[[1]]
		criteriaIDs<-getCriteriaIDs(treeCriteria)
	}
	else {
		errData <- numberOfCriteria$status
		flag<-FALSE
	}
	if (flag){
		if (criteriaIDs$status == "OK")
		{
			critIDs<-criteriaIDs[[1]]
		}
		else 
		{
			errData<-criteriaIDs$status
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
					
					if (!is.null(seed)){
						set.seed(seed)
					}
					
					#génération des poids (aléatoire)
#					pds<-c(runif(numCrit,0,1))
#					
#					somm<-sum(pds)
#					
#					#Normalisation des poids
#					pds_norm<-pds
#					for (i in 1:numCrit) pds_norm[i]<-pds[i]/somm
					pds <- c(runif(numCrit - 1, 0, 1))
					pds <- append(pds, 0)
					pds <- append(pds, 1)
					pds <- sort(pds, decreasing = TRUE)
					for (i in 1:numCrit)
						pds[i] <- pds[i] - pds[i + 1]
					
					critVals<-c()
					for (i in 1:numCrit)
					{
#						critVals<-rbind(critVals,c(i,pds_norm[i]))
						critVals<-rbind(critVals,c(i,pds[i]))
					}
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot generate criteria weights."
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
	
	status<-putCriteriaValues(outTree, critVals, critIDs, "criteriaWeights")
	
	status<-saveXML(outTree, file="criteriaWeights.xml")
	
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

