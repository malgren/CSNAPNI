#cropalloc_model.r
#This script uses domestic crop/feed production totals, crop allocation fractions between animals and humans,
  #crop N and P contents, crop forage/feed classifications, animal forage/grain ratios, 
  #animal diet N and P requirements, and DDGS consumption (market share) estimations
  #to model allocations of crops/feeds to animals

if(print_tags == 1){
  print("CreateInputsSubs/cropalloc_model.R")
}

model_yrs = c("97","02","07","12","17")  #data years to import

#Calculate crops available for domestic use
cropusedom = cropprodtotal-exports

#Load crop data for 
#crop allocation fractions between animals and humans ([4] prop_human, [5] prop_anim)
#crop N and P contents ([1] prop_dm, [2] N/dm, [3] P/dm)
#crop forage/grain classifications ([10] grain?)
#citations in cropdata_master.xlsx
#key in cropdata_key.txt
cropdata = t(array(scan("InputFiles/cropdata.txt"), c(18,19)))

#Load anim data (calculated for silage as grain) for
#animal forage/grain ratios ([3] prop N from grain,	[4] prop N from forage,	[5] prop P from grain,	[6] prop P from forage)
#animal diet N and P requirements ([1] N requirement (kg/year/animal),	[2] P requirement (kg/year/animal))
#citations in animdata_master.xlsx
#key in animdata_alloc_key.txt
animdata_alloc = t(array(scan("InputFiles/animfeedallocdataSG.txt"), c(6,19)))

#Read DGS consumption estimations
#data comes from RFA sources (cited in DGSalloc_master.xlsx)
#key in DGSalloc_key.txt
DGSalloc = t(array(scan("InputFiles/DGSalloc.txt"), c(6,19)))

#CROPS
dmprop=cropdata[,1]
Nperdm=cropdata[,2]
Pperdm=cropdata[,3]
prop_to_humans=cropdata[,4]
crop_type=cropdata[,10] #0=forage, 1=grain, 2=other

Nincrop=dmprop*Nperdm #N/mass crop
Pincrop=dmprop*Pperdm #P/mass crop

#allocate space to arrays
cropsforfeed=array(0, c(n_crops,data_yrs))
totalNincrop=array(0, c(n_crops,data_yrs))
totalPincrop=array(0, c(n_crops,data_yrs))
totalgrainN=array(0, data_yrs)
totalgrainP=array(0, data_yrs)
totalforageN=array(0, data_yrs)
totalforageP=array(0, data_yrs)
cropgrainNprop=array(0, c(n_crops,data_yrs)) #this crop's proportional contribtution to total N in grain
cropforageNprop=array(0, c(n_crops,data_yrs))
cropgrainPprop=array(0, c(n_crops,data_yrs))
cropforagePprop=array(0, c(n_crops,data_yrs))

#Calculate crop N and P availability for animals and sort into forage and grain categories
for(n in 1:data_yrs){
  cropsforfeed[,n]=cropusedom[,n]*(1-prop_to_humans)
  totalNincrop[,n]=cropsforfeed[,n]*Nincrop
  totalPincrop[,n]=cropsforfeed[,n]*Pincrop
  totalgrainN[n]=0
  totalgrainP[n]=0
  totalforageN[n]=0
  totalforageP[n]=0
  #calculate total N and P in grains and forages
  for(i in 1:n_crops){
    if(crop_type[i]==1 && i<17){  #if it's a grain and not an etoh coproduct
      totalgrainN[n]=totalgrainN[n]+totalNincrop[i,n]
      totalgrainP[n]=totalgrainP[n]+totalPincrop[i,n]
    }
    if(crop_type[i]==0){ #if it's a forage
      totalforageN[n]=totalforageN[n]+totalNincrop[i,n]
      totalforageP[n]=totalforageP[n]+totalPincrop[i,n]
    }
  }
  for(i in 1:n_crops){
    if(crop_type[i]==1 && i<17){  #if it's a grain and not an etoh coproduct
      cropgrainNprop[i,n] = totalNincrop[i,n]/totalgrainN[n]
      cropforageNprop[i,n] = 0
      cropgrainPprop[i,n] = totalPincrop[i,n]/totalgrainP[n]
      cropforagePprop[i,n] = 0
    } 
    if(crop_type[i]==0){ #if it's a forage
      cropgrainNprop[i,n] = 0
      cropforageNprop[i,n] = totalNincrop[i,n]/totalforageN[n]
      cropgrainPprop[i,n] = 0
      cropforagePprop[i,n] = totalPincrop[i,n]/totalforageP[n]
    } 
    if(crop_type[i]==2){ #if it's neither forage nor grain
      cropgrainNprop[i,n] = 0
      cropforageNprop[i,n] = 0
      cropgrainPprop[i,n] = 0
      cropforagePprop[i,n] = 0
    }
  }
}

#ANIMALS
#get livestock N and P requirements and diet propgrain and propforage
animNreq = animdatadyn[,8]
animPreq = animdatadyn[,9]
propNfromgrain = animdata_alloc[,3] #proportion of each animal's required N that comes from grain
propNfromforage = animdata_alloc[,4] #proportion of each animal's required N that comes from forage
propPfromgrain = animdata_alloc[,5] #same for P
propPfromforage = animdata_alloc[,6] #same for P
coprod_prop = DGSalloc/100

#ALLOCATION
#allocate space to arrays
cropNtoanim=array(0, c(n_anims,n_crops,data_yrs)) #N from each crop per animal in each livestock type
cropPtoanim=array(0, c(n_anims,n_crops,data_yrs)) #P from each crop per animal in each livestock type
cropNtoanimtotal=array(0, c(n_anims,n_crops,data_yrs)) #total N from each crop to each livestock type in each year
cropPtoanimtotal=array(0, c(n_anims,n_crops,data_yrs)) #total P from each crop to each livestock type in each year
animgrainN=array(0, c(n_anims,data_yrs)) #animal N requirement to be satisfied by grain after DGS allocation
animgrainP=array(0, c(n_anims,data_yrs)) #animal P requirement to be satisfied by grain after DGS allocation
animforageN=array(0, c(n_anims,data_yrs)) #animal N requirement to be satisfied by forage
animforageP=array(0, c(n_anims,data_yrs)) #animal P requirement to be satisfied by forage
cropkgtoanimtotal_N=array(0, c(n_anims,n_crops,data_yrs)) #modeled allocation of each crop to each animal type in each year (N-based)
cropkgtoanimtotal_P=array(0, c(n_anims,n_crops,data_yrs)) #modeled allocation of each crop to each animal type in each year (P-based)
cropkgtoanim = array(0, c(n_anims,n_crops,data_yrs)) #modeled allocation of each crop per 1 animal in each year (N-based)

for(n in 1:data_yrs){
  #allocate DGS and other etoh coproducts first
  for(c in 17:19){
    cropNtoanimtotal[,c,n]=coprod_prop[,n]*totalNincrop[c,n]*.95 #assumed 5% losses of etoh coproducts (we overproduce grains by 12%, based on total animal N requirements and total N in grains produced)
    cropPtoanimtotal[,c,n]=coprod_prop[,n]*totalPincrop[c,n]*.95 #assumed 5% losses of etoh coproducts (we overproduce grains by 12%)
    cropNtoanim[,c,n]=cropNtoanimtotal[,c,n]/animpoptotal[,n]
    cropPtoanim[,c,n]=cropPtoanimtotal[,c,n]/animpoptotal[,n]
    #fix NaN problem with animal populations of zero
    zero_pops=which(animpoptotal[,n] %in% 0)
    for(i in 1:length(zero_pops)){
      cropNtoanim[zero_pops[i],c,n]=0
      cropPtoanim[zero_pops[i],c,n]=0
    }
      cropkgtoanimtotal_N[,c,n]=cropNtoanimtotal[,c,n]/Nincrop[c] #kg crop/animal type/year, based on N requirement allocation
      cropkgtoanimtotal_P[,c,n]=cropPtoanimtotal[,c,n]/Pincrop[c] #kg crop/animal type/year, based on P requirement allocation
  }
  #subtract N and P in allocated coproducts from the total grain N
  #and P requirements for each livestock category
  animgrainN[,n]=animNreq*animpoptotal[,n]*propNfromgrain-rowSums(cropNtoanimtotal[,17:19,n])
  animgrainP[,n]=animPreq*animpoptotal[,n]*propPfromgrain-rowSums(cropPtoanimtotal[,17:19,n])
  negativesN = which(animgrainN[,n]<0)
  negativesP = which(animgrainP[,n]<0)
  for(i in negativesN){
    animgrainN[i,n] = 0
  }
  for(i in negativesP){
    animgrainP[i,n] = 0
  }
  animforageN[,n]=animNreq*animpoptotal[,n]*propNfromforage
  animforageP[,n]=animPreq*animpoptotal[,n]*propPfromforage
  for(c in 1:16){ #each crop except for etoh coproducts
    cropNtoanimtotal[,c,n]=animgrainN[,n]*cropgrainNprop[c,n]+animforageN[,n]*cropforageNprop[c,n]
    cropPtoanimtotal[,c,n]=animgrainP[,n]*cropgrainPprop[c,n]+animforageP[,n]*cropforagePprop[c,n]
    cropNtoanim[,c,n]=cropNtoanimtotal[,c,n]/animpoptotal[,n]
    cropPtoanim[,c,n]=cropPtoanimtotal[,c,n]/animpoptotal[,n]
    #fix NaN problem with animal populations of zero
    zero_pops=which(animpoptotal[,n] %in% 0)
    for(i in 1:length(zero_pops)){
        cropNtoanim[zero_pops[i],c,n]=0
        cropPtoanim[zero_pops[i],c,n]=0
    }
    cropkgtoanimtotal_N[,c,n]=cropNtoanimtotal[,c,n]/Nincrop[c] #kg crop/animal type/year
    cropkgtoanimtotal_P[,c,n]=cropPtoanimtotal[,c,n]/Pincrop[c] #kg crop/animal type/year
  }
  
  #since the N-based allocation model is to be used, need to use cropkgtoanimtotal_N to re-calculate cropPtoanimtotal
  #crop P to anim = total kg of crop allocated * P content of the crop
  for(i in 1:length(Pincrop)){
    cropPtoanimtotal[,i,] = cropkgtoanimtotal_N[,i,] * Pincrop[i]
    for(j in 1:length(model_yrs)){
      cropPtoanim[,i,j] = cropPtoanimtotal[,i,j]/animpoptotal[,j]
      #fix NaN problem with animal populations of zero
      zero_pops=which(animpoptotal[,j] %in% 0)
      for(k in 1:length(zero_pops)){
        cropPtoanim[zero_pops[k],i,j]=0
      }
    }
  }
  
  #Calculate kg crop/animal for each crop in each year
  cropkgtoanim[,,n] = cropkgtoanimtotal_N[,,n]/animpoptotal[,n]
  #fix NaN problem with animal populations of zero
  zero_pops=which(animpoptotal[,n] %in% 0)
  for(k in 1:length(zero_pops)){
    cropkgtoanim[zero_pops[k],,n]=0
  }
  
  #write data files
  write_name = paste("InputFiles/cropNtoanim",model_yrs[n],".txt",sep = "") #kg N by crop per animal in each livestock category
  write.table(cropNtoanim[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles/cropPtoanim",model_yrs[n],".txt",sep = "") #kg P by crop per animal in each livestock category
  write.table(cropPtoanim[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles/cropkgtoanimtotal_N",model_yrs[n],".txt",sep = "") #kg crop/animal type/year, based on N requirement allocation
  write.table(cropkgtoanimtotal_N[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles/cropkgtoanimtotal_P",model_yrs[n],".txt",sep = "") #kg crop/animal type/year, based on P requirement allocation
  write.table(cropkgtoanimtotal_P[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
#write key
write_name = "InputFileKeys/croptoanim_key.txt"
croptoanim_key = array(" ", c(n_anims+1,n_crops+1))
croptoanim_key[1,]=c(" ", cropname) #column headings
croptoanim_key[,1]=c(" ", animtyp) #row headings
write.table(croptoanim_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE, quote=FALSE)