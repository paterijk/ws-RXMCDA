library(bnlearn)

########################################################################################
#######################Creation d'un model graphe XMCDA en matrice d'adjacence###########

convert.graphe.adj<-function(critIDs,numcrit,graph)
{
  adjacency <- matrix(0,ncol=numCrit,nrow=numCrit)
  rownames(adjacency) <- critIDs
  colnames(adjacency) <- critIDs
  
  for (i in 1:dim(graph)[1])
  {
    if(graph[i,3]!=0)
    {	adjacency[graph[i,1],graph[i,2]] = 1
      
    }
  }
  return(adjacency)
}

##############################################################################

test.vect<-function(vect)
{
  ans<-FALSE
  size<-length(vect)
  for(v in vect)
  {
    if(v==TRUE)
    {
      ans<-TRUE
    }
  }
  return(ans);
}


##############################################################################
##################convertiseur d'une matrice Adj en modelstring###############

is.vect.null<-function(vect)
{
  rep<-TRUE
  for(i in 1:length(vect))
  {
    if(vect[i]!=0)
    {
      rep<-FALSE
    }
  }
  return(rep);
}
####################################################################################
nbre.de.un<-function(vect)
{	size<-length(vect)
  compt<-0
  
  for(i in 1:size)
  {
    if(vect[i]==1)
    {
      compt<-compt+1	
      
    }
    
    
  }
  
  return(compt)
  
}

position<-function(vect)
{
  size.vect<-length(vect)
  size.position<-nbre.de.un(vect)
  
  position<-rep(NA,size.position)
  compt<-1
  for(i in 1:size.vect)
  {
    if(vect[i]==1)
    {
      position[compt]<-letters[i]
      compt<-compt+1
    }
    
    
  }
  
  
  
  
  return(position)
  
}
###############################################################################################
convert.str.adj<-function(mat.adj)
{
  size<-ncol(mat.adj)
  
  modelstring<-"NA"
  
  
  for(ncol in 1:size)
  {
    
    nbre.de.un<-nbre.de.un(mat.adj[,ncol])
    position<-position(mat.adj[,ncol])
    
    if(nbre.de.un==0)
    {
      modelstring<-c(modelstring,c("[",letters[ncol],"]"))
      
      
      
    }
    if(nbre.de.un==1)
    {
      modelstring<-c(modelstring,c("[",letters[ncol],"|",position,"]"))
      
      
      
    }
    if(nbre.de.un>1)
    {
      
      
      string.node<-c("[",letters[ncol],"|",position[1])
      
      for(i in 2:nbre.de.un)
      {
        string.node<-c(string.node,":",position[i])
      }
      modelstring<-c(modelstring,string.node,"]")
      
    }
    
    
    
    
  }
  
  size.modelstring<-length(modelstring)
  modelstring<-toString(modelstring[2:size.modelstring])
  return(modelstring)
  
}

#################################################################################################
######################Cr?ation des vecteurs moyennes et ecart-types ? des input#####################

create.mean.sd<-function(critIDs,numCrit,critMeans,critSD)
{
  
  
  mean<-rep(NA,numCrit)
  
  sigma<-rep(NA,numCrit)
  
  for(i in 1:numCrit) ###On met dans l ordreles valeurs de moyennes##########
{
  
  index.mean<-critMeans[i,1]
  
  mean[index.mean]<-critMeans[i,2]
  
  index.sigma<-critSD[i,1]
  
  sigma[index.sigma]<-critSD[i,2]
  
  
  
  
}
  
  return(list(Mean=mean,Sigma=sigma))
  
  
  
  
}
############################################################################################################################
#####################cr?ation du vecteur moyenne et ?cart-type par ordre topologique#####################################
create.mean.sd.topo<-function(modelstring,mean,sigma)
{
  network<-model2network(modelstring)
  
  order_topo<-node.ordering(network)	
  order_alphabetic<-nodes(network)
  
  nbre.nodes<-length(order_topo)
  
  
  mean.topo<-rep(NA,nbre.nodes)
  sigma.topo<-rep(NA,nbre.nodes)
  
  
  index.topo<-rep(NA,nbre.nodes)
  
  for(compt_topo in 1:nbre.nodes)
  {
    
    for(compt_alpha in 1:nbre.nodes)
    {
      
      if(order_topo[compt_topo]==order_alphabetic[compt_alpha])
      {
        
        mean.topo[compt_topo]<-mean[compt_alpha]
        sigma.topo[compt_topo]<-sigma[compt_alpha]
      }
      
    }
    
    
  }
  
  
  
  
  return(list(Mean.topo=mean.topo,Sigma.topo=sigma.topo))
  
  
}
##########################################################################################
##########################################################################################
create.mean.mat.topo<-function(mean.topo,sigma.topo,mat.adj,order_topo)
{
  
  nbre.nodes<-length(mean.topo)		
  mat.adjency.topo<-matrix(0,nrow=nbre.nodes,ncol=nbre.nodes)
  
  compt.line<-1
  compt.col<-1
  for(node.line in order_topo)
  {		
    
    for(node.col in order_topo)
    {		
      child12<-children(network,node.line)
      vect.bool.child12<-(node.col==child12)
      condi1<-test.vect(vect.bool.child12)
      
      child21<-children(network,node.col)
      vect.bool.child21<-(node.line==child21)
      condi2<-test.vect(vect.bool.child21)
      
      if(condi1||condi2)
      {
        mat.adjency.topo[compt.line,compt.col]<-1
        
        
      }
      
      compt.col<-compt.col+1
    }
    
    compt.line<-compt.line+1
    compt.col<-1
  }
  
  
  
  
  for(i in 1:length(order_topo))
  {
    
    
    mat.adjency.topo[i,i]<-sigma.topo[i]^2
    
    
    
  }
  
  mean.nodes<-as.list(rep(NA,nbre.nodes))
  names(mean.nodes)<-order_topo
  
  for(i in 1:nbre.nodes)
  {
    mean.nodes[[i]]<-mean.topo[i]
  }
  
  return(list(Mean=mean.nodes,mat.adj=mat.adjency.topo));
  
}

#######################################################################################################################################################################################

is.vect.null<-function(vect)
{
  rep<-TRUE
  for(i in 1:length(vect))
  {
    if(vect[i]!=0)
    {
      rep<-FALSE
    }
  }
  return(rep);
}
###########################################################################
create.bloc.nodes<-function(mat.adjency.topo,order_topo)
{
  nbre.nodes<-length(order_topo)
  list.nodes<-as.list(rep(NA,nbre.nodes))
  names(list.nodes)<-order_topo
  
  list.nodes[[1]]<-mat.adjency.topo[1,1];
  
  for(i in 2:nbre.nodes)
  {
    mat.adj<-mat.adjency.topo[i,1:i-1]
    
    if(is.vect.null(mat.adj))
    {
      list.nodes[[i]]<-mat.adjency.topo[i,i]
    }
    
    
    else
    {
      
      
      mat.bloc.initial<-mat.adjency.topo[1:i-1,1:i-1]
      
      vect.parcour<-mat.adjency.topo[i,1:i-1]
      
      nbre.different<-length(vect.parcour[vect.parcour!=0])    ###demande d'element different de 0####
      if(nbre.different==1)
      {
        vect.bloc<-rep(NA,nbre.different)
        
      }	
      else	
      {	vect.bloc<-rep(NA,2*nbre.different)
      }
      
      
      
      compt<-1
      
      
      
      for(line in 1:length(vect.parcour))
      {
        if(vect.parcour[line]!=0)
        {
          for(col in 1:length(vect.parcour))
          {
            if(vect.parcour[col]!=0)
            {
              
              
              vect.bloc[compt]<-mat.adjency.topo[line,col]
              compt<-compt+1
              
              
            }
            
            
            
            
          }
        }		
        
      }
      
      mat.bloc<-t(matrix(vect.bloc,ncol=nbre.different))
      
      list.nodes[[i]]<-mat.bloc
    }
  }
  
  return(list.nodes);
}
####################################Fonction transforme en alphabet###########################
transform.alpha<-function(node,network)
{
  
  nodes<-nodes(network)
  compt<-1
  for(n in nodes)
  {
    if(node==n)
    {
      alpha<-letters[compt]		
      
      
    }	
    compt<-compt+1
    
  }
  
  return(alpha)
  
  
  
}
################################################################################################
##########################Cherche la bonne corr?lation dans le tableau graphe##################

schearch.good.correlation<-function(critIDs,graph,node.parent,node.child,network)    
{
  nodes<-nodes(network)
  nodeParent_alpha<-transform.alpha(node.parent,network)
  
  nodeChild_alpha<-transform.alpha(node.child,network)
  
  
  
  
  ascii <- sapply(1:127,function(i) parse(text=paste("\"\\",structure(i,class="octmode"),"\"",sep=""))[[1]]) ###Passage par le code ascii#####
  
  index.parent<-which(ascii==nodeParent_alpha)-96 
  
  index.child<-which(ascii== nodeChild_alpha)-96
  
  nodeParentgraphe<-critIDs[index.parent]
  nodeChildgraphe<-critIDs[index.child]
  
  size.graphe<-nrow(graph)		
  for(i in 1:size.graphe)
  {
    if((nodeParentgraphe==graph[i,1])&&(nodeChildgraphe==graph[i,2]))			
    {
      cor<-as.double(graph[i,3])
      
    }
    
    
    
  }
  
  return(cor)
  
}
########################################################################################################################################################################################
#######################Fonction qui retourne les bornes ainsi que le param?tre K d ajustement###########

ajustement.correlation<-function(mat.adjency.topo,bloc,tY,compt)###la variable compt est utilis?e pour le sigma.courant#### 
{
  size<-nrow(bloc)
  Y<-t(t(tY))
  Ay<-bloc[1:size-1,1:size-1]
  at<-bloc[1:size-1,size]
  a<-t(t(at))
  ax<-bloc[size,size]
  
  rho<-(at%*%Y)^2-ax*((tY%*%Ay%*%Y)-1)
  borne1<-(1/sqrt(mat.adjency.topo[compt-1,compt-1]))*(-(at%*%Y)+sqrt(rho))/ax
  borne2<-(1/sqrt(mat.adjency.topo[compt-1,compt-1]))*(-(at%*%Y)-sqrt(rho))/ax
  K<-borne1
  
  rho<-((at%*%Y)*K)^2-ax*K^2*((tY%*%Ay%*%Y)-1)
  Newborne1<-(1/sqrt(mat.adjency.topo[compt-1,compt-1]))*(-((at%*%Y)*K)+sqrt(rho))/(ax*K^2)
  Newborne2<-(1/sqrt(mat.adjency.topo[compt-1,compt-1]))*(-((at%*%Y)*K)-sqrt(rho))/(ax*K^2)
  
  
  return(list(Parametre.ajust=K,Borne1=Newborne1,Borne2=Newborne2))
  
}
#############################################################################################################
#####################################################################################################
insert.cor.param<-function(critIDs,graph,list.nodes.bloc,mean.nodes,mat.adjency.topo,network)
{		
  
  nodes<-nodes(network) ####Important dans la conversion en alphabet#########		
  
  order_topo<-node.ordering(network)
  
  nbre.nodes<-length(order_topo)
  
  list.dist<-names(as.list(rep(NA,nbre.nodes)))
  
  list.nodes<-as.list(rep(NA,nbre.nodes))
  
  names(list.nodes)<-order_topo
  
  for(compt in  1:nbre.nodes)
    
  {			
    
    
    parents.nodes<-parents(network,order_topo[compt])
    parents.nodes<-order_topo[order_topo%in%parents.nodes]
    
    nbre.parents<-length(parents.nodes)
    
    
    if(length(parents.nodes)==0)
    {
      
      coeff<-rep(NA,1)
      names(coeff)<-c("(Intercept)")
      list.dist[[compt]]<-list(coef=coeff,sd=NA)
      
      
      
      list.dist[[compt]]$sd<-sqrt(list.nodes.bloc[[compt]])
      
      list.dist[[compt]]$coef<-mean.nodes[[compt]] ###?????mean####
      
      list.nodes[[compt]]<-list.dist[[compt]]
      
    }
    
    
    
    
    if(length(parents.nodes)==1)
    {
      
      ####################################################calcul l'indice du parent##################################
      for(i in 1:length(order_topo))
      {
        if(parents.nodes==order_topo[i])
        {
          indice.parents<-i;
          
        }
        
      }
      
      ###################################################################################################
      
      
      
      coeff<-rep(NA,2)
      names(coeff)<-c("(Intercept)",parents.nodes)
      list.dist[[compt]]<-list(coef=coeff,sd=NA)
      
      
      
      cor<-schearch.good.correlation(critIDs,graph,parents.nodes,order_topo[compt],network)
      
      sigma.courant<-sqrt(mat.adjency.topo[compt,compt])
      sd<-sigma.courant*sqrt(1-cor^2)
      
      alpha<-cor*sigma.courant/sqrt(list.nodes.bloc[[compt]])
      list.dist[[compt]]$sd<-sd
      
      list.dist[[compt]]$coef[2]<-alpha
      
      
      
      
      list.dist[[compt]]$coef[1]<-mean.nodes[[compt]]-alpha*mean.nodes[[indice.parents]]#####mean???###
      
      
      
      list.nodes[[compt]]<-list.dist[[compt]]
      
      ##################modifier la liste bloc################################################
      mat.adjency.topo[indice.parents,compt]<-cor*sigma.courant*sqrt(list.nodes.bloc[[compt]]);
      mat.adjency.topo[compt,indice.parents]<-mat.adjency.topo[indice.parents,compt];
      list.nodes.bloc<-create.bloc.nodes(mat.adjency.topo,order_topo)
      #########################################################################################
      
      
      
    }
    
    if(length(parents.nodes)>1)
    {	
      ####################calcul du vecteur indices des parents##########################################
      indice.parents<-rep(NA,length(parents.nodes))
      index<-1					
      for(p in parents.nodes)
      {
        for(j in 1:nbre.nodes)
        {	
          
          if(p==order_topo[j])
          {
            indice.parents[index]<-j
            index<-index+1;			
            
          }							
        }
        
      }
      ##################################################################################################
      
      
      
      sigma.courant<-sqrt(mat.adjency.topo[compt,compt])
      mat.bloc<-list.nodes.bloc[[compt]]
      size<-nrow(mat.bloc)
      
      bloc<-solve(mat.bloc)
      
      
      
      Ay<-bloc[1:(size-1),(1:size-1)]
      at<-bloc[(1:size-1),size]
      a<-t(t(at))
      ax<-bloc[size,size]
      tX<-rep(0,size)
      
      value<-1
      
      
      for(p in parents.nodes[1:(length(parents.nodes)-1)])
      {
        
        
        cor<-schearch.good.correlation(critIDs,graph,p,order_topo[compt],network)
        tX[value]<-sqrt(mat.adjency.topo[indice.parents[value],indice.parents[value]])*cor
        
        
        
        mat.adjency.topo[indice.parents[value],compt]<-cor*sigma.courant*sqrt(mat.adjency.topo[indice.parents[value],indice.parents[value]]);
        
        mat.adjency.topo[compt,indice.parents[value]]<-mat.adjency.topo[indice.parents[value],compt];
        
        list.nodes.bloc<-create.bloc.nodes(mat.adjency.topo,order_topo)
        mat.adjency.topo[indice.parents[size],compt]<-cor*sigma.courant*sqrt(mat.adjency.topo[indice.parents[size],indice.parents[size]]);
        mat.adjency.topo[compt,indice.parents[size]]<-mat.adjency.topo[indice.parents[size],compt];
        list.nodes.bloc<-create.bloc.nodes(mat.adjency.topo,order_topo)
        
        value<-value+1
        
        
        
        
        
        
      }
      tY<-tX[1:(length(parents.nodes)-1)]
      
      K<-ajustement.correlation(mat.adjency.topo,bloc,tY,compt)[[1]]
      borne1<-ajustement.correlation(mat.adjency.topo,bloc,tY,compt)[[2]]
      borne2<-ajustement.correlation(mat.adjency.topo,bloc,tY,compt)[[3]]
      
      
      
      
      cor<-schearch.good.correlation(critIDs,graph,parents.nodes[nbre.parents],order_topo[compt],network)
      tX[nbre.parents]<-sqrt(mat.adjency.topo[indice.parents[value],indice.parents[value]])*cor
      X<-t(t(tX))
      
      
      ######################################################################################################################################################################					
      ###############################################Trouve la position de l erreur et propose des bornes###################################################################
      
      quadratic<-tX%*%bloc%*%X
      
      if(quadratic<1)####Si la forme quadratique ets bonne , on parametrise#################
{
  state.parametrisation<-TRUE	
  message.error<-"The parametrization went well"
  Newborne1<-NA
  Newborne2<-NA
  
  correlation.error<-c(NA,NA)
  
  
  coeff<-rep(NA,length(parents.nodes)+1)
  names(coeff)<-c("(Intercept)",parents.nodes)
  list.dist[[compt]]<-list(coef=coeff,sd=NA)
  
  alpha<-solve(mat.bloc,sigma.courant*X)
  sd<-sqrt(sigma.courant^2-t(alpha)%*%mat.bloc%*%alpha)
  list.dist[[compt]]$sd<-sd
  
  P.param<-mean.nodes[[compt]]
  
  
  for(i in 1:length(parents.nodes))					
  {	
    list.dist[[compt]]$coef[i+1]<-alpha[i]
    P.param<-P.param-alpha[i]*mean.nodes[[indice.parents[i]]]
    
    
  }
  
  list.dist[[compt]]$coef[1]<-P.param ####mean???###################
  list.nodes[[compt]]<-list.dist[[compt]]
  
  
}
      else ##Dans ces cas une erreus est produite et il faut detecter la mauvaise corr?lation et propos? des bornes######
{
  state.parametrisation<-FALSE
  
  message.error<-"Error in the parametrization."
  
  tvect.test<-rep(0,nrow(bloc))
  tvect.test[1]<-tX[1]
  num.cor<-1
  
  position.mistake<-0
  error<-FALSE
  while(!error)
  {
    num.cor<-num.cor+1
    tvect.test[num.cor]<-X[num.cor]
    vect.test<-t(t(tvect.test))
    quadratic.courant<-tvect.test%*%bloc%*%vect.test
    
    
    if(quadratic.courant>1)
    {
      error<-TRUE
      position.mistake<-num.cor
    }
  }
  Bloc<-bloc[1:num.cor,1:num.cor]
  size.Bloc<-nrow(Bloc)
  ty<-tvect.test[1:(num.cor-1)]
  y<-t(t(ty))
  Ay.Bloc<-Bloc[1:(num.cor-1),1:(num.cor-1)]
  at.Bloc<-Bloc[1:(num.cor-1),num.cor]
  a.Bloc<-t(t(at.Bloc))
  ax.Bloc<-Bloc[num.cor,num.cor]
  
  rho<-(at.Bloc%*%y)^2-ax.Bloc*((ty%*%Ay.Bloc%*%y)-1)
  borne1.Bloc<-(1/sqrt(mat.adjency.topo[num.cor,num.cor]))*(-(at.Bloc%*%y)+sqrt(rho))/(ax.Bloc)
  borne2<-(1/sqrt(mat.adjency.topo[num.cor,num.cor]))*(-(at.Bloc%*%y)-sqrt(rho))/(ax.Bloc)
  K<-borne1
  rho<-((at.Bloc%*%y)*K)^2-ax.Bloc*K^2*((ty%*%Ay.Bloc%*%y)-1)
  Newborne1<-(1/sqrt(mat.adjency.topo[num.cor,num.cor]))*(-((at.Bloc%*%y)*K)+sqrt(rho))/(ax.Bloc*K^2)
  Newborne2<-(1/sqrt(mat.adjency.topo[num.cor,num.cor]))*(-((at.Bloc%*%y)*K)-sqrt(rho))/(ax.Bloc*K^2)
  
  
  index.parents.mistake<-which(nodes==parents.nodes[position.mistake])
  index.child.mistake<-which(nodes==order_topo[compt])
  correlation.error<-c(critIDs[index.parents.mistake],critIDs[index.child.mistake])
  
  
  
  
  
}
      
      
      
    }####Fin du if(parents>1)####
    
  }####Fin compt #########
  
  return(list(List.nodes=list.nodes,Mat.adjency.topo=mat.adjency.topo,State.parametrisation=state.parametrisation,Correlation.error=correlation.error,Borne.correlation=c(Newborne1,Newborne2),Message.error=message.error))
  
  
  
}



############################################################################
############################################################################
############################################################################

