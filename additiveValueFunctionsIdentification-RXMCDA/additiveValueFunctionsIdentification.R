# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < additiveValueFunctionsIdentification.R
# example: 
#  R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < additiveValueFunctionsIdentification.R


rm(list=ls())

library(RXMCDA)
library(lpSolve)

errFile<-NULL
errData<-NULL
errCalc<-NULL

execFlag<-FALSE

# get the in and out directories from the arguments

inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

# inDirectory <- "in"
# outDirectory <- "out"

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
				treeSegments<-xmlTreeParse("segments.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the number of segments file."
	}
}

if (is.null(errFile)){
	
	# mandatory files were correctly loaded
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
		if (checkXSD(treeAlternativesRanks)==0)
		{
			errFile<-"Alternatives ranks file is not XMCDA valid."	
		}
	}	
	if (is.null(errFile)){
		if (checkXSD(treeSegments)==0)
		{
			errFile<-"Segments file is not XMCDA valid."	
		}
	}	
	if (is.null(errFile)){
		if (checkXSD(treePerformanceTable)==0)
		{
			errFile<-"Performance table file is not XMCDA valid."	
		}
	}
	if (is.null(errFile)){
		if (checkXSD(treeSeparationThreshold)==0)
		{
			errFile<-"Separation threshold parameters file is not XMCDA valid."	
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
			separationThresholds<-getParameters(treeSeparationThreshold, "separationThreshold")
		}
		else
		{
			errData<-performanceTables$status
			flag<-FALSE
		}
	}
	if (flag){
		if ((separationThresholds$status == "OK"))
		{
			sepThr<-separationThresholds[[1]]
			criteriaSegments<-getParameters(treeSegments, "criteriaSegments")
			
		}
		else
		{
			errData<-separationThresholds$status
			flag<-FALSE
		}
	}
	if (flag){
		if (criteriaSegments$status == "OK") 
		{
			segs<-criteriaSegments[[1]]
			alternativesRanks <- getAlternativesValues(treeAlternativesRanks, altIDs)
		}
		else
		{
			errData <- criteriaSegments$status
			flag <- FALSE
		}
	}
	if (flag){
		if (alternativesRanks$status == "OK") 
		{
			ranks<-alternativesRanks[[1]]
		}
		else
		{
			errData <- alternativesRanks$status
			flag <- FALSE
		}
	}
	
	
	
	
	if (is.null(errData))
	{
		
		tmpErr<-try(
				{	
					
# Change names of variables to fit Helene's code
					
					Nb_mor <- segs
					na<- dim(perfTable)[1]
					nc<-dim(perfTable)[2]
					delta.C <- sepThr
						
					# the best alternative has the lowest rank
					ordre <- ranks[order(ranks[,2],decreasing=FALSE),1]
					
					M<-perfTable
					
					if (Nb_mor == na) Nb_mor <- Nb_mor-1
					
#**************************************************************
# g  est la matrice qui contient les points de cassure
#Dans le cas où le nombre de morceaux = na (le nombre d'alternatives), les colonnes de la matrice g
#correspondent aux colonnes de la matrice de départ (ici M) triée dans l'ordre croissant
#En effet les points de cassure correspondent aux évaluations des alternatives sur les différents critères.
					g<-function(h) {
						if(Nb_mor != na){
							g<-matrix(c(rep(0,nc*(Nb_mor+1))),Nb_mor+1,nc)
							i<-1
							while(i <= nc){
								min<-min(M[,i])
								max<-max(M[,i])
								j<-1	
								while (j <= (Nb_mor+1)){
									g[j,i]<-min+(((j-1)/Nb_mor)*(max-min))
									j<-j+1
								}
								i<-i+1
							}
						}else {
							g<-matrix(c(rep(0,nc*Nb_mor)),Nb_mor,nc)
							for(i in 1:nc)g[,i]<-sort(M[,i])
						}
						return(g)	
					}
					
#*******************************************************************************************
#Calcule de la matrice des interpolations linéaires sur base des données de la marice g
#voir formule dans texte TFE Chapitre sur les fonctions additives ou les articles de réféérence
#Jaquet-Lagrèse (82) ou Greco (08)
					M_additive<-function(M,MM) {
						M_additive<-matrix(rep(0,na*nc*(Nb_mor+1)),na,nc*(Nb_mor+1))
						for(i in 1:na){
							for(j in 1:nc){
								for(k in 1:Nb_mor){
									if(M[i,j] == min(M[,j])){
										coef_ang<-0
										M_additive[i,((Nb_mor+1)*(j-1))+k]<-coef_ang
									}else if(MM[k,j]<= M[i,j]&& M[i,j] < MM[k+1,j]){
										coef_ang<-(M[i,j]-MM[k,j])/(MM[k+1,j]-MM[k,j])
										M_additive[i,((Nb_mor+1)*(j-1))+k+1]<-coef_ang
										M_additive[i,((Nb_mor+1)*(j-1))+k]<-1-coef_ang
										if(k==1)
										{
											M_additive[i,((Nb_mor+1)*(j-1))+k]<-0
										}
									}else if(M[i,j] == max(M[,j])&& k==Nb_mor){
										coef_ang<-1	
										M_additive[i,((Nb_mor+1)*(j-1))+k+1]<-coef_ang
									}						
								}
							}
						}
						return(M_additive)
					}
					
#*********************************************************************************************
# Matrice exprimant la condition de monotonie : fonctions additives  monotones croissantes.
#
					mon_crst<-function(h) {
						mon_crst<-matrix(rep(0,nc*Nb_mor*nc*(Nb_mor+1)),nc*Nb_mor,nc*(Nb_mor+1))
						ii<-1
						while(ii <= nc){
							jj<-1
							while(jj <=Nb_mor){
								kk<-((ii-1)*Nb_mor)+jj
								mon_crst[kk,]<-c(rep(0,(jj-1)+(ii-1)*(Nb_mor+1)),-1,1,rep(0,((Nb_mor+1)*(nc-ii))+((Nb_mor-1)-(jj-1))))
								jj<-jj+1
							}
							ii<-ii+1
						}
						return(mon_crst)
					}
					
#*******************************************************************************************
#Contraintes stipulant que la valeur des fonctions additives sur l'abcisse minimum de chaque segment est nulle
					val_min<-function(h) {
						val_min<-matrix(rep(0,nc*nc*(Nb_mor+1)),nc,nc*(Nb_mor+1))
						t<-1
						while(t <=nc){
							val_min[t,]<-c(rep(0,((t-1)*(Nb_mor+1))),1,rep(0,Nb_mor+((nc-t)*(Nb_mor+1))))
							t<-t+1
						}
						return(val_min)
					}
					
#*******************************************************************************************
#contrainte exprimant que la somme des utilités maximales vaut un : ici les coefficients
#des abscisses les plus élevées sur chaque segment seront égales à 1
					val_max<-function(h) {
						val_max<-matrix(rep(c(rep(0,Nb_mor),1),nc),1,nc*(Nb_mor+1))
						return(val_max)
					}
#*******************************************************************************************
#La variable supplémentaire est un vecteur contenant autant de lignes qu'il n'y en a
#dans la matrice des contraintes à transmettre à la routine de programmation linéaire.
#Elle est utilisée pour stoker les coefficients de la variable
#correspondant à l'objectif "delta"(qui est à maximiser). Les composantes valent -1 aux lignes 
# correspondant aux contraintes sur le classement des alternatives et 0 ailleurs
					var_suppl<-function(h) {
						ligne<-(na-1)+nc+1+(nc*Nb_mor)
						var_suppl<-matrix(c(rep(-1,na-1),rep(0,nc+1+(nc*Nb_mor))),ligne,1)
						return(var_suppl)
					}
#*******************************************************************************************
#Matrice contenant les contraintes sur le classemnet des alternatives en fonction du
#vecteur "ordre" généré par les permutations successives.
#En effet ordre[lg] donne la composante du vecteur ordre (le classement),et le numéro de la ligne
# de la matrice M_additive (aussi appelée M3) s'obtient par M3[ordre[lg],] 
#Dans le cas des fonctions additive, les suil de discrimination doit être introduit lors de 
#la définition du vecteur membre de droite (rhs1) à faire passer à la routine de programmation
#linéaire.
					diff_alt<-function(M3,ordre){
						diff_alt<-matrix(rep(0,(na-1)*nc*(Nb_mor+1)),na-1,nc*(Nb_mor+1))
						lg<-1
						while(lg <= na-1){
							diff_alt[lg,]<-M3[ordre[lg],]- M3[ordre[lg+1],]
							lg<-lg+1
						}
						return(diff_alt)
					}
					
					
					MM<-g(M)
					M3<-M_additive(M,MM)
					M4<-mon_crst(h)
					M5<-val_min(h)
					M6<-val_max(h)
					M7<-var_suppl(h)
					
#définition des paramètres direction, membre de droite et objectif pour la fonction
#de programation linéaire (lp) de LpSolve. 
					dir1<-c(rep(">=",(na-1)),rep("=",nc),"=", rep(">=",(nc*Nb_mor)+1))
					rhs1<-c(rep(0.0,(na-1)),rep(0,nc),1, rep(0,nc*Nb_mor),delta.C)	
					obj1<- matrix(c(rep(0,(nc*(Nb_mor+1))),1),1, (nc*(Nb_mor+1))+1)
					obj_positif<- matrix(c(rep(0,(nc*(Nb_mor+1))),1),1, (nc*(Nb_mor+1))+1)
					
					M8<-diff_alt(M3,ordre)
					
#Fin du calcul de la matrice des contraintes pour les fonctions additives. C'est la concaténation
#des différentes matrices précédemment calculées. L'instruction de concaténation dans R est 'rbind'
#pour concaténer selon les lignes (cbind concaténation selon le colonnes)
					mat_cont<-matrix(rep(0,(na+nc+(nc*Nb_mor))*nc*(Nb_mor+1)),(na+nc+(nc*Nb_mor)),nc*(Nb_mor+1))
					mat_cont<-rbind(M8,M5,M6,M4)
					mat_cont1<-cbind(mat_cont,M7)
					mat_cont2<-rbind(mat_cont1,obj_positif)
					
#Routines de programmation linéaire pour les fonctions de valeur additive
					sol_lp<-lp("max",obj1,mat_cont2,dir1,rhs1)
					
					# if there is a solution, we build the data structure containing the points of the value functions

					if(sol_lp$status == 0){
						
						points<-list()
						for (i in 1:nc){
							tmp<-c()
							for (j in 1:dim(MM)[1]){
								
								tmp<-rbind(tmp,c(MM[j,i],sol_lp$solution[(i-1)*dim(MM)[1]+j]))
							}
							
							points<-c(points,list(tmp))
							names(points)[i]<-critIDs[i]
						}
					}
					
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Calculation error."
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
	
	# first the result file, if solver status == 0
	
	if (sol_lp$status == 0){
		
		outTree = newXMLDoc()
		
		newXMLNode("xmcda:XMCDA", 
				attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
				suppressNamespaceWarning=TRUE, 
				namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
				parent=outTree)
		
		status<-putPointsCriterionFunction(outTree, points, mcdaConcept="valueFunctions")
		
		status<-saveXML(outTree, file="valueFunctions.xml")
		
	}	
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	if (sol_lp$status == 0){
		solstatus = "Success"
		status<-putLogMessage(outTree2, sol_lp$objval, name="objectiveFunction")
	}
	else{
		solstatus = "No feasible solution"
	}
	
	status<-putLogMessage(outTree2, solstatus, name="solverStatus")
	
	
	
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


