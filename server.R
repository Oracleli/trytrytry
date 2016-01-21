library('shiny')
library('stringi')
library('dplyr')
library('igraph')
library('sna')
load(file='mole_twomode.rda')
recipe_list=read.csv('recipe_list.csv')
recipe_list=recipe_list[,-1]


######################################
## recipe plot
rmrep=rep(F,200)
for(i in 1:200){
  if(match(rownames(all_rec_onemode)[i],rownames(all_rec_onemode))!=i){
    rmrep[i]=T
  }
}

######################################
### ingredient plot
a=NULL
for (i in 1:nrow(coldif)){
  a[i]=sum(coldif[i,]=='1')
}
b=NULL
for (i in 1:nrow(coldif)){
  b[i]=sum(coldif[i,]=='2')
}
##
ha=mole_ing_onemode
ha=ha[order(b/(a+b)),]
ha=ha[,order(b/(a+b))]
edco=NULL
edwi=NULL
for(i in 1:nrow(mole_ing_onemode)){
  for(j in 1:nrow(mole_ing_onemode)){
    if(ha[i,j]>=ha[j,i]&ha[i,j]>0){
      edwi=c(edwi,ha[i,j]+ha[j,i])
      edco=c(edco,ha[i,j]/(ha[i,j]+ha[j,i]))
      ha[i,j]=1
      ha[j,i]=0
    }
  }
}

g2=graph.adjacency(ha)

V(g2)$color = rgb(red = 1, green = 0, blue = (b/(a+b))[order(b/(a+b))], alpha = .7) # clubs
V(g2)$size <- (5*diag(all_ing_onemode) %>%sqrt)[order(b/(a+b))]
V(g2)$label <- rownames(all_ing_onemode)[order(b/(a+b))]
V(g2)$label.color <- 'black'

E(g2)$color <-  rgb(red = 0, green = 1.4-1.2*edco, blue = 0, alpha = 1)


##############################
## ingredient by recipe
## first 5 centrality each courses
## mole_ing_onemode in courses


rehiin=list()
rehiout=list()
rehiboth=list()
cahiin=list()
cahiout=list()
cahiboth=list()

for(l in 1:5){
  aa=coldif[,as.numeric(recipe_list[,2])==l]
  bb=matrix(0,ncol=nrow(aa),nrow=nrow(aa))
  for(i in 1:nrow(aa)){
    for(j in 1:nrow(aa)){
      for(k in 1:ncol(aa)){
        if(aa[i,k]!=0&aa[j,k]!=0){
          if(aa[i,k]<aa[j,k]){
            bb[i,j] <-bb[i,j] + 1
          }
        }
      }
    }
    #cat(i/nrow(a),'\n')
  }
  #cat(l,'is finished')
  cahiin[[l]]=order(sna::degree(bb, cmode = "indegree"),decreasing = T)[1:5]
  cahiout[[l]]=order(sna::degree(bb, cmode = "outdegree"),decreasing = T)[1:5]
  pr= bb %>% graph.adjacency %>% page_rank
  cahiboth[[l]]= order(pr$vector,decreasing = T)[1:5]
}

for(l in 1:4){
  aa=coldif[,((l*50)-49):(l*50)]
  bb=matrix(0,ncol=nrow(aa),nrow=nrow(aa))
  for(i in 1:nrow(aa)){
    for(j in 1:nrow(aa)){
      for(k in 1:ncol(aa)){
        if(aa[i,k]!=0&aa[j,k]!=0){
          if(aa[i,k]<aa[j,k]){
            bb[i,j] <-bb[i,j] + 1
          }
        }
      }
    }
    #cat(i/nrow(a),'\n')
  }
  #cat(l,'is finished')
  rehiin[[l]]=order(sna::degree(bb, cmode = "indegree"),decreasing = T)[1:5]
  rehiout[[l]]=order(sna::degree(bb, cmode = "outdegree"),decreasing = T)[1:5]
  pr= bb %>% graph.adjacency %>% page_rank
  rehiboth[[l]]= order(pr$vector,decreasing = T)[1:5]
}






shinyServer(function(input, output) {
  
  mysize1 <- function(){input$plotsize1}
  mysize2 <- function(){input$plotsize2}
  mysize3 <- function(){input$plotsize3}
  
  formulaText1 <- reactive({
    if (input$reca1=='region'){
      c('  Central Europe：green\n',
         'Northern Europe：blue\n',
         'Southern Europe：light blue\n',
         ' Western Europe：yellow')
    }else if (input$reca1=='category'){
      c(" dessert：green\n",
         "  dish ：blue\n",
         "  meal ：light blue\n",
         " salad ：pink\n",
         "  soup ：yellow")
    }
  })
  output$legend1 <- renderText({
    formulaText1()
  })
  
  
  formulaText3 <- reactive({
    if (input$reca3=='region'){
      c('  Central Europe：green\n',
         'Northern Europe：blue\n',
         'Southern Europe：light blue\n',
         ' Western Europe：yellow\n',
         '   Ingredient  ：red')
    }else if (input$reca3=='category'){
      c("   dessert ：green\n",
         "    dish  ：blue\n",
         "    meal  ：light blue\n",
         "   salad  ：pink\n",
         "    soup  ：yellow\n",
         "Ingredient：red")
    }
  })
  
  output$legend3 <- renderText({
    formulaText3()
  })
  
 
  output$recPlot <- renderPlot({
    g1=all_rec_onemode
    diag(g1)=0
    g1[g1<input$nd1]=0
    g1=graph.adjacency(g1) 
    g1=as.undirected(g1)
    if (input$reca1=='region'){
      V(g1)$color[1:50] = rgb(red = 0, green = 1, blue = 0, alpha = .5) 
      V(g1)$color[51:100] = rgb(red = 0, green = 0, blue = 1, alpha = .5) 
      V(g1)$color[101:150] = rgb(red = 0, green = 1, blue = 1, alpha = .5) 
      V(g1)$color[151:200] = rgb(red = 1, green = 1, blue = 0, alpha = .5) 
    }else if (input$reca1=='category'){
      V(g1)$color[as.numeric(recipe_list[,2])+2==3] = rgb(red = 0, green = 1, blue = 0, alpha = .5) 
      V(g1)$color[as.numeric(recipe_list[,2])+2==4] = rgb(red = 0, green = 0, blue = 1, alpha = .5) 
      V(g1)$color[as.numeric(recipe_list[,2])+2==5] = rgb(red = 0, green = 1, blue = 1, alpha = .5) 
      V(g1)$color[as.numeric(recipe_list[,2])+2==6] =  rgb(red = 1, green = 0, blue = 1, alpha = .5)
      V(g1)$color[as.numeric(recipe_list[,2])+2==7] = rgb(red = 1, green = 1, blue = 0, alpha = .5)
    }
    V(g1)$label <- rownames(all_rec_onemode)
    V(g1)$label.cex <- input$labelsize1
    V(g1)$size <- input$vertexsize1
    
    g1 = delete.vertices(g1, V(g1)[rmrep])
    g1 = delete.vertices(g1, V(g1)[igraph::degree(g1)==0 ])
    
    plot(g1, layout=layout.kamada.kawai)
    
    
  },height=mysize1,width=mysize1)
  
  
  output$ingPlot <- renderPlot({
    
    V(g2)$label.cex <- input$labelsize2
    E(g2)$weight <- edwi*input$edwid2
    
    g2 = delete.vertices(g2, 
                         V(g2)[(diag(all_ing_onemode)<input$nd2)
                                [order(b/(a+b))]])
    
    par(mar=c(0,0,0,0),oma=c(0,0,0,0),mai=c(0,0,0,0))
    plot(g2 ,edge.width= E(g2)$weight ,
         edge.arrow.size=input$arrsize2,
         layout=layout.grid)
  },height=mysize2,width=mysize2)
  
  
  output$ingrecPlot <- renderPlot({
    
    if (input$reca3=='region'){
      forra=1:4
    }else if (input$reca3=='category'){
      forra=1:5
    }
      
    if (input$reca3=='region'){
      if (input$inoupr3=='in'){
        unhi=rehiin %>% unlist %>% unique
      }else if (input$inoupr3=='ou'){
        unhi=rehiout %>% unlist %>% unique
      }else if (input$inoupr3=='pr'){
        unhi=rehiboth %>% unlist %>% unique
      }
      }else if (input$reca3=='category'){
        if (input$inoupr3=='in'){
          unhi=cahiin %>% unlist %>% unique
        }else if (input$inoupr3=='ou'){
          unhi=cahiout %>% unlist %>% unique
        }else if (input$inoupr3=='pr'){
          unhi=cahiboth %>% unlist %>% unique
        }
      }
    
    
    
    hehe=matrix(0,ncol=length(forra),nrow=unhi %>% length)
    for(i in forra){
      for(j in 1:nrow(hehe)){
        if(unhi[j] %in% 
           if (input$reca3=='region'){
             if (input$inoupr3=='in'){
               rehiin[[i]]
             }else if (input$inoupr3=='ou'){
               rehiout[[i]]
             }else if (input$inoupr3=='pr'){
               rehiboth[[i]]
             }
           }else if (input$reca3=='category'){
             if (input$inoupr3=='in'){
               cahiin[[i]]
             }else if (input$inoupr3=='ou'){
               cahiout[[i]]
             }else if (input$inoupr3=='pr'){
               cahiboth[[i]]
             }
           }
           
        ){
          hehe[j,i]=1
        }
      }
    }
    
    
    
    haha <- graph.incidence(hehe)
    
    
    if (input$reca3=='region'){
      V(haha)$color[1:nrow(hehe)] = rgb(red = 1, green = 0, blue = 0, alpha = .8) 
      V(haha)$color[nrow(hehe)+1] = rgb(red = 0, green = 1, blue = 0, alpha = .8) 
      V(haha)$color[nrow(hehe)+2] = rgb(red = 0, green = 0, blue = 1, alpha = .8) 
      V(haha)$color[nrow(hehe)+3] = rgb(red = 0, green = 1, blue = 1, alpha = .8) 
      V(haha)$color[nrow(hehe)+4] = rgb(red = 1, green = 1, blue = 0, alpha = .8) 
      V(haha)$label = c(rownames(coldif)[unhi],c("Central Europe",
                                                   "Northern Europe",
                                                   "Southern Europe" ,
                                                   "Western Europe"))
    }else if (input$reca3=='category'){
      
      V(haha)$color[1:nrow(hehe)] = rgb(red = 1, green = 0, blue = 0, alpha = .8) 
      V(haha)$color[nrow(hehe)+1] = rgb(red = 0, green = 1, blue = 0, alpha = .8) 
      V(haha)$color[nrow(hehe)+2] = rgb(red = 0, green = 0, blue = 1, alpha = .8) 
      V(haha)$color[nrow(hehe)+3] = rgb(red = 0, green = 1, blue = 1, alpha = .8) 
      V(haha)$color[nrow(hehe)+4] =  rgb(red = 1, green = 0, blue = 1, alpha = .8)
      V(haha)$color[nrow(hehe)+5] = rgb(red = 1, green = 1, blue = 0, alpha = .8)
      V(haha)$label = c(rownames(coldif)[unhi],c("dessert",
                                                     "dish",
                                                     "meal",
                                                     "salad",
                                                     "soup"))
      
    }
    V(haha)$label.color = rgb(0,0,.2,.8)
    V(haha)$label.cex = input$labelsize3
    V(haha)$size = input$invesi3
    V(haha)$size[nrow(hehe)+forra]=input$revesi3
    V(haha)$frame.color = V(haha)$color
    E(haha)$color = "black"
    plot(haha, layout=layout.fruchterman.reingold)
    
  },height=mysize3,width=mysize3)

})