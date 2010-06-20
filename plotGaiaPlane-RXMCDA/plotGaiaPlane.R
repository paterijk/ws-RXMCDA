# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotGaiaPlane.R
# example: 
#  R --slave --vanilla --args "${PWD}/plotGaiaPlaneInFiles" "${PWD}/plotGaiaPlaneOutFiles" < plotGaiaPlane.R


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
				treeCriteriaValues<-xmlTreeParse("criteriaWeights.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the criteria values file."
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
		if (checkXSD(treeCriteria)==0)
		{
			errFile<-"Criteria file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treePerformanceTable)==0)
		{
			errFile<-"Performance table file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeCriteriaValues)==0)
		{
			errFile<-"Criteria values file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valid according to xsd
	
	errData<-NULL
	flag<-TRUE
	
	criteriaIDs<-getCriteriaIDs(treeCriteria)
	
	if(flag){
		if ((criteriaIDs$status == "OK")) 
		{
			critIDs<-criteriaIDs[[1]]
			alternativesIDs<-getAlternativesIDs(treeAlternatives)
		}
		else
		{
			errData <- criteriaIDs$status
			flag<-FALSE
		}
	}
	
	if(flag){
		if ((alternativesIDs$status == "OK")) 
		{
			altIDs<-alternativesIDs[[1]]
			performanceTables<-getPerformanceTables(treePerformanceTable,altIDs=altIDs, critIDs=critIDs)
		}
		else
		{
			errData <- alternativesIDs$status
			flag<-FALSE
		}
	}
	
	if(flag){
		if ((performanceTables$status == "OK")) 
		{
			perfTable<-performanceTables[[1]]
			critVals<-getCriteriaValues(treeCriteriaValues, critIDs)
		}
		else
		{
			errData <- performanceTables$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (critVals$status == "OK") 
		{
			criteriaValues<-critVals[[1]]
			if (is.null(criteriaValues))
			{
				errData<- "Could not read weights of the criteria."
				flag<-FALSE
			}
		}
		else
		{
			errData <- critVals$status
			flag <- FALSE	
		}
	}
	
	if(flag){
		n<-length(colnames(perfTable))
	}
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		tmpErr<-try(
				{
					library(pcaMethods)
					
					setwd(outDirectory)
					
					pdf("out.pdf", width=n*2 + 2)
					
					pc <- pca(perfTable, scale="none", method="svd")
					
					# determine the coordinates of the unit vectors of the criteria in the PCA plane
					# first calculate the unit vectors of the criteria
				
					criteria <- matrix(nrow=dim(perfTable)[2], ncol=dim(perfTable)[2], rep(0,dim(perfTable)[2]*dim(perfTable)[2]))
					
					dimnames(criteria) <- list(dimnames(perfTable)[[2]],dimnames(perfTable)[[2]])
					
					for (i in 1:dim(criteria)[1])
						criteria[i,i] = 1
					
					# then project them in the PCA plane
			
					projectedCriteria <- matrix(nrow=dim(perfTable)[2], ncol=2)
					
					dimnames(projectedCriteria) <- list(dimnames(perfTable)[[2]],c("PC1", "PC2"))
					
					for (i in 1:(dim(projectedCriteria)[1])){
						projectedCriteria[i,1] <- scale(criteria,center=TRUE,scale=FALSE)[i,]%*%pc@loadings[,1]
						projectedCriteria[i,2] <- scale(criteria,center=TRUE,scale=FALSE)[i,]%*%pc@loadings[,2]
					}
					
					# let us now plot the alternatives in the PCA plane
					
					plot(pc@scores, xlab = "", ylab = "", ylim=c(1.1*min(pc@scores,projectedCriteria),1.1*max(pc@scores,projectedCriteria)), xlim=c(1.1*min(pc@scores,projectedCriteria),1.1*max(pc@scores,projectedCriteria)))

					labels<-dimnames(perfTable)[[1]]
					
					for (i in 1:(dim(perfTable)[1])){
						text(pc@scores[i,1],pc@scores[i,2], labels=labels[i], pos=4) 
					}
					
					
					# let us now plot the unit vectors of the criteria in the PCA plane
					
					labels<-dimnames(projectedCriteria)[[1]]
					
					for (i in 1:(dim(projectedCriteria)[1])){
						text(projectedCriteria[i,1],projectedCriteria[i,2], labels=labels[i], pos=4) 
					}
					
					par(lty=2)
					
					for (i in 1:(dim(projectedCriteria)[1]))
						segments(0,0,projectedCriteria[i,1],projectedCriteria[i,2])
					
					
					# the weights should sum up to one, if not, we renormalize them

					if (sum(criteriaValues[,2] != 1)){
						criteriaValues[,2] <- criteriaValues[,2] / sum(criteriaValues[,2])
					}
					
					# first reorder the criteriaweights correctly so that the scalar product will be correct
					
					criteriaValuesReordered<-rep(0,length(dimnames(perfTable)[[2]]))

					for (i in 1:length(dimnames(perfTable)[[2]]))
					{
						criteriaValuesReordered[which(dimnames(pc@loadings)[[1]] == critIDs[criteriaValues[i,1]])] = criteriaValues[i,2]
					}
					
					# then project this weights vector in the PCA plane for the Pi decision axis
					
					projectedWeights <- c(t(scale(criteriaValuesReordered,center=TRUE,scale=FALSE))%*%pc@loadings[,1], t(scale(criteriaValuesReordered,center=TRUE,scale=FALSE))%*%pc@loadings[,2])
					
					par(lty=1)
					
					segments(0,0,projectedWeights[1],projectedWeights[2])
					
					text(projectedWeights[1],projectedWeights[2], labels="Pi", pos=4)
					
					points(projectedWeights[1],projectedWeights[2])
					
					# write the cumulated variance of the two components of the PCA

					text(min(pc@scores,projectedCriteria),min(pc@scores,projectedCriteria),labels = paste("delta = ",pc@R2cum[2]))
					
					dev.off()
					
					#############
					
					system("convert out.pdf out.png")
					
					system("base64 -w0 out.png > out.base64")
					tmp<-readLines(file("out.base64","rt"))
					system("rm out.base64 out.png out.pdf")
					
					closeAllConnections()
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot plot Gaia plane."
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
	
	status<-putAlternativesPlot(outTree, tmp, rownames(perfTable), mcdaConcept="Gaia plane")
	
	status<-saveXML(outTree, file="GaiaPlot.xml")
	
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

