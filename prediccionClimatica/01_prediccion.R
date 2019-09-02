
########## Functions ##########

download.cpt=function(dir_save,areas_l,n_areas_l,month,year){ 
  
  season=(month)+0:5
  if(sum(season>12)>0)season[which(season>12)]=season[which(season>12)]-12
  if(sum(season<1)>0)season[which(season<1)]=season[which(season<1)]+12
  
  season_for=season[c(1,4)]
  areas=areas_l[season_for]
  n_areas=n_areas_l[season_for]
  
  i_con=month-1
  if(i_con<=0)i_con=i_con+12
  
  for(i in 1:2){
    
    t=c(1,4)
    if(n_areas[[i]]==4){
      
      route=paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[i_con],"%201982-",year,"%29/VALUES/L/",t[i],".5/",t[i]+2,".5/RANGE/%5BL%5D//keepgrids/average/M/1/24/RANGE/%5BM%5Daverage/X/",areas[[i]][1],"/",areas[[i]][2],"/flagrange/Y/",areas[[i]][3],"/",areas[[i]][4],"/flagrange/add/1/flaggt/mul/0/setmissing_value/%5BX/Y%5D%5BS/L/add/%5Dcptv10.tsv.gz")
      
    }else{
      
      route=paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[i_con],"%201982-",year,"%29/VALUES/L/",t[i],".5/",t[i]+2,".5/RANGE/%5BL%5D//keepgrids/average/M/1/24/RANGE/%5BM%5Daverage/X/",areas[[i]][1],"/",areas[[i]][2],"/flagrange/Y/",areas[[i]][3],"/",areas[[i]][4],"/flagrange/add/1/flaggt/X/",areas[[i]][5],"/",areas[[i]][6],"/flagrange/Y/",areas[[i]][7],"/",areas[[i]][8],"/flagrange/add/1/flaggt/add/mul/0/setmissing_value/%5BX/Y%5D%5BS/L/add/%5Dcptv10.tsv.gz")
      
    }
    
    filePath <- paste(dir_save,"/",i,"_",paste(month.abb[season[t[i]:(t[i]+2)]], collapse = '_'),".tsv.gz",sep="")
    
    # ----------------------------------------------------------------------------------------------------------------------
    #  > prueba que el archivo gzip no esta dannado, sino intenta descargarlo de nuevo
    # ----------------------------------------------------------------------------------------------------------------------
    gunzipTestVal = 1
    
    while (gunzipTestVal == 1) {
      
      download.file(route, filePath) ### Realiza la descarga 
      
      gunzipTestStr <- try(system(paste("gunzip -t ", filePath, sep = "", collapse = NULL), intern = TRUE, ignore.stderr = FALSE))
      cat (gunzipTestStr)
      
      gunzipTestVal = length(grep('invalid compressed data--crc', gunzipTestStr))
      
      if (gunzipTestVal == 1){
        cat ('... archivo gzip corrupto, se reitentara descargar\n\n\n')
      }else {
        cat(filePath)
        cat ('\n... archivo descargado correctamente\n\n\n')
      }
    }
    # ----------------------------------------------------------------------------------------------------------------------    
    
  }
  
}

transform_raster=function(x,y){
  mapa_base=raster(,nrows=y[1], ncols=y[2])
  extent(mapa_base)=c(-0.5,359.5,-90.5,90.5)
  val=c(as.matrix(t(x),ncol=1,byrow = T))
  val=as.numeric(val)
  val[val==0]=NA
  values(mapa_base)=val
  return(mapa_base)
}

data_raster=function(dates){
  
  year_month=dates[1,][!is.na(dates[1,])]
  year=ifelse(substr(year_month[-1],6,7)=="12",substr(year_month[-1],9,12),substr(year_month[-1],1,4))
  data_cpt1=na.omit(dates)
  pos=which(data_cpt1[,1]=="")
  pos1=sort(rep(year,pos[2]-pos[1]))
  list_dates=split(data_cpt1,pos1)
  lat=as.numeric(as.character(list_dates[[1]][-1,1]))
  cos_lat=diag(sqrt(cos((pi/180)*lat)))
  tables=lapply(list_dates,"[",-1,-1)
  tables_numeric=lapply(tables,function(x)sapply(x,function(y)as.numeric(as.character(y))))
  #data_ponde=lapply(tables_numeric,function(x) cos_lat%*%x)
  n_p=dim(tables_numeric[[1]])
  all_raster=lapply(tables_numeric,transform_raster,n_p)
  layers=stack(all_raster)
  return(layers)
  
}

quarterly_data=function(data,sy_month){
  
  names_months=0
  data_out=list()
  year_out=list()
  l=(sy_month)+(-1:6)
  if(sum(l>12)>0)l[which(l>12)]=l[which(l>12)]-12
  if(sum(l<1)>0)l[which(l<1)]=l[which(l<1)]+12
  for(i in 1:6){
    pos_ini=which(data$month==l[i])
    pos_data=sort(c(pos_ini,pos_ini+1,pos_ini+2))
    data_out[[i]]=data.frame(na.omit(aggregate(data[pos_data,-1:-2],by=list(sort(rep(1:(length(pos_ini)),3))),sum))[,-1])
    names(data_out[[i]])=colnames(data)[-1:-2]
    year_out[[i]]=data[pos_ini+1,1][1:dim(data_out[[i]])[1]]
    names_months[i]=paste(month.abb[l[(0:2)+i]],collapse="_")
    rownames(data_out[[i]])=year_out[[i]]
  }
  
  names(data_out)=names_months
  names(year_out)=names_months
  
  all_output=list(data_out,year_out)
  names(all_output)=c("data_stations","year_response")
  
  return(all_output)
} 

pca_x_svd=function(x,modos,pon){
  
  n=dim(x)[1]
  p=dim(x)[2]
  #pon_matrix=diag(pon)
  pon_matrix=matrix(rep(pon,n),n,p,byrow = TRUE)
  x0=scale(x)#*(sqrt(n)/sqrt(n-1))
  #x_pon=t(pon_matrix%*%t(x0))
  x_pon=pon_matrix*x0
  svd_o <- fast.svd(x_pon)
  comp  <- svd_o$u[,1:modos,drop=F]%*%diag(svd_o$d[1:modos],length(svd_o$d[1:modos]),length(svd_o$d[1:modos])) 
  vect  <- svd_o$v[,1:modos]
  output <- list(comp,vect)
  return(output)
  
}

pca_y_svd=function(x,modos){
  
  n=dim(x)[1]
  x0=scale(x)#*(sqrt(n)/sqrt(n-1))
  svd_o <- fast.svd(x0)
  comp  <- svd_o$u[,1:modos,drop=F]%*%diag(svd_o$d[1:modos],length(svd_o$d[1:modos]),length(svd_o$d[1:modos])) 
  vect  <- svd_o$v[,1:modos]
  output <- list(comp,vect)
  return(output)
  
}

selection_area=function(x,y,ponde,confi){
  
  mode_x=as.numeric(confi[1])
  mode_y=as.numeric(confi[2]) ; if(dim(y)[2] < mode_y)mode_y=dim(y)[2]
  
  y_pca=pca_y_svd(y,modos=mode_y)[[1]]
  x_pca=pca_x_svd(x,modos=mode_x,ponde)[[1]]
  all_cor=matrix(NA,mode_x*mode_y,dim(x)[2])
  count=0
  
  for(i in 1:mode_x){
    
    for(j in 1:mode_y){
      
      canonico=cancor(x_pca[,1:i],y_pca[,1:j,drop=F])
      x_center=scale(x_pca[,1:i],scale = F)
      y_center=scale(y_pca[,1:j],scale = F)  
      com_x=x_center%*%canonico$xcoef
      com_y=y_center%*%canonico$ycoef
      mode1=cbind(com_x[,1],com_y[,1])
      cor_tsm=cor(x,mode1[,1])
      count=count+1
      all_cor[count,]=cor_tsm[,1]
      
    }
    
  }
  print("Finalizo la selección del area para un mes")
  cor_mean=apply(abs(all_cor),2,mean)
  return(cor_mean)
}

plots=function(path,cor,q,name){
  
  Loadings_map=data_x_r[[1]][[1]]
  pos_cor=!is.na(Loadings_map[])
  Loadings_map[pos_cor]=cor
  q1=quantile(cor,q[4])
  jBrewColors <- brewer.pal(n = 100, name = "Reds")
  tiff(paste0(path,"/",name,".tiff"),compression = 'lzw',height = 6.5,width = 5.7,units="in", res=150)
  par(mfrow=c(2,1))
  plot(Loadings_map,main="Correlación promedio",col=jBrewColors,colNA="gray",legend.width=1,legend.shrink=1)
  plot(Loadings_map >= q1,main="Pixeles seleccionados",colNA="gray",legend=F,col=jBrewColors)
  dev.off()
  
}

files_x=function(raster,cor,na,years){
  
  year_p=paste0("cpt:T=",years)
  
  for(i in seq(0,0.9,0.1)){
    
    pos_data=which(!is.na(values(raster)[,1]))
    pos_selec=cor<=quantile(cor,i)
    pos_final=pos_data*pos_selec
    val=values(raster)
    val[pos_final,]=NA
    val[which(is.na(val),arr.ind = T)]=0
    val_l=split(val,col(val))
    val_matrix=lapply(val_l,function(x)matrix(x,length(90:-90),length(0:359),byrow=TRUE,dimnames=list(c(90:-90),c(0:359))))
    
    
    p="xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
    p1="cpt:nfields=1"
    p2=paste0("cpt:field=ssta, ",year_p[1],", cpt:nrow=",181,", cpt:ncol=",360,", cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=0")
    
    name_file=paste0(main_dir,"/run_CPT/",na,"/","x_",i,".txt")
    sink(name_file)
    cat(p)
    cat("\n")
    cat(p1) 
    cat("\n")
    cat(p2) 
    cat("\n")
    u=Map(function(x,y){write.table(t(c(" ",0:359)),sep="\t",col.names=F,row.names=F,quote = F);write.table(x,sep="\t",col.names=F,row.names=T,quote = F);cat(y);cat("\n")},val_matrix,c(year_p[-1],""))
    sink()
  }
  
  return("Successful process")   
}

run_cpt=function(x,y,run,output,confi,p){
  
  modes_x=as.numeric(confi[[1]])
  mode_y=as.numeric(confi[[2]])
  if(p<10)mode_y=p
  mode_cca=as.numeric(confi[[3]])
  if(p<5)mode_cca=p
  
  t=ifelse(confi[[4]]=="si",541," ")
  
  GI=paste0(output,"GI.txt"); pear=paste0(output,"pearson.txt"); afc=paste0(output,"2afc.txt")
  prob=paste0(output,"prob.txt");roc_a=paste0(output,"roc_a.txt");roc_b=paste0(output ,"roc_b.txt")
  pca_eigen_x=paste0(output,"pca_eigen_x.txt"); pca_load_x=paste0(output,"pca_load_x.txt"); pca_scores_x=paste0(output,"pca_scores_x.txt")
  pca_eigen_y=paste0(output,"pca_eigen_y.txt"); pca_load_y=paste0(output,"pca_load_y.txt"); pca_scores_y=paste0(output,"pca_scores_y.txt")
  cca_load_x=paste0(output,"cca_load_x.txt"); cca_cc=paste0(output,"cca_cc.txt"); cca_scores_x=paste0(output,"cca_scores_x.txt")
  cca_load_y=paste0(output,"cca_load_y.txt"); cca_scores_y=paste0(output,"cca_scores_y.txt")
  
  hit_s=paste0(output,"hit_s.txt")
  hit_ss=paste0(output,"hit_ss.txt")
  
  cmd <- "@echo off
  (
  echo 611
  echo 545
  echo 1
  echo %path_x% 
  echo /
  echo /
  echo /
  echo /
  echo 1
  echo %modex%
  echo 2
  echo %path_y%
  echo 1
  echo %modey%
  echo 1
  echo %modecca%
  echo 9
  echo 1
  echo 532
  echo /
  echo /
  echo N
  echo 2
  echo 554
  echo 2
  echo %trans%
  echo 112
  echo %path_GI%
  echo 311
  echo 451
  echo 455
  echo 413
  echo 1
  echo %path_pear%
  echo 413
  echo 3
  echo %path_2afc%
  echo 413
  echo 4 
  echo %path_hit_s%
  echo 413  
  echo 5
  echo %path_hit_ss% 
  echo 413
  echo 10
  echo %path_roc_b%
  echo 413
  echo 11
  echo %path_roc_a%
  echo 111
  echo 301
  echo %path_pca_eigen_x%
  echo 302
  echo %path_pca_load_x%
  echo 303
  echo %path_pca_scores_x%
  echo 311
  echo %path_pca_eigen_y%
  echo 312
  echo %path_pca_load_y%
  echo 313
  echo %path_pca_scores_y%
  echo 401
  echo %path_cca_cc%
  echo 411
  echo %path_cca_load_x%
  echo 412
  echo %path_cca_scores_x%
  echo 421
  echo %path_cca_load_y%
  echo 422
  echo %path_cca_scores_y%
  echo 501
  echo %path_prob%
  echo 0
  echo 0
  ) | CPT_batch.exe"
  
  cmd<-gsub("%path_x%",x,cmd)
  cmd<-gsub("%path_y%",y,cmd)
  cmd<-gsub("%path_GI%",GI,cmd)
  cmd<-gsub("%path_pear%",pear,cmd)
  cmd<-gsub("%path_2afc%",afc,cmd)
  cmd<-gsub("%path_roc_b%",roc_b,cmd)
  cmd<-gsub("%path_roc_a%",roc_a,cmd)
  cmd<-gsub("%path_prob%",prob,cmd)
  cmd<-gsub("%modey%",mode_y,cmd)
  cmd<-gsub("%modex%",modes_x,cmd)
  cmd<-gsub("%modecca%",mode_cca,cmd)
  cmd<-gsub("%trans%",t,cmd)
  cmd<-gsub("%path_cca_load_x%",cca_load_x,cmd)
  cmd<-gsub("%path_cca_cc%",cca_cc,cmd)
  
  cmd<-gsub("%path_pca_eigen_x%",pca_eigen_x,cmd)
  cmd<-gsub("%path_pca_load_x%",pca_load_x,cmd)
  cmd<-gsub("%path_pca_scores_x%",pca_scores_x,cmd)
  cmd<-gsub("%path_pca_eigen_y%",pca_eigen_y,cmd)
  cmd<-gsub("%path_pca_load_y%",pca_load_y,cmd)
  cmd<-gsub("%path_pca_scores_y%",pca_scores_y,cmd)
  cmd<-gsub("%path_cca_scores_x%",cca_scores_x,cmd)
  cmd<-gsub("%path_cca_scores_y%",cca_scores_y,cmd)
  cmd<-gsub("%path_cca_load_y%",cca_load_y,cmd)
  
  cmd<-gsub("%path_hit_s%",hit_s,cmd)
  cmd<-gsub("%path_hit_ss%",hit_ss,cmd)
  
  
  write(cmd,run)
  #shell.exec(run)
  system2(run)
  
}

correl <- function(x,y){
  
  y[1,1]=""
  loadings <- na.omit(y)
  loadings[loadings==0]=NA
  pos=which(loadings[,1]=="")
  if(length(pos)==1){list_dates=list(loadings)}else{vector_split <- sort(rep(pos,pos[2]-1));list_dates <- split(loadings,vector_split)}
  tables=lapply(list_dates,"[",-1,-1)
  cor_ca=x[1:length(tables),1]
  final=Reduce("+",Map(function(x,y) abs(x)*y ,tables,cor_ca))/sum(cor_ca)
  final_vec=as.vector(as.matrix(t(final)))
  
  return(final_vec)
}

files_x=function(raster,cor,na,years){
  
  coor_min=apply(coordinates(raster),2,min) 
  coor_max=apply(coordinates(raster),2,max) 
  coor_all=cbind(coor_min,coor_max)
  
  year_p=paste0("cpt:T=",years)
  
  for(i in seq(0.1,0.9,0.1)){
    
    #pos_data=which(!is.na(values(raster)[,1]))
    pos_selec=which(cor<quantile(cor,i,na.rm=T))
    #pos_final=pos_data*pos_selec
    val=values(raster)
    val[pos_selec,]=NA
    val[which(is.na(val),arr.ind = T)]= -999
    val_l=split(val,col(val))
    
    
    lat=sort(seq(coor_all[2,1],coor_all[2,2]),decreasing = T)
    lon=sort(seq(coor_all[1,1],coor_all[1,2]))
    val_matrix=lapply(val_l,function(x)matrix(x,length(lat),length(lon),byrow=TRUE,dimnames=list(lat,lon)))
    
    
    p="xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
    p1="cpt:nfields=1"
    p2=paste0("cpt:field=ssta, ",year_p[1],", cpt:nrow=",length(lat),", cpt:ncol=",length(lon),", cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999")
    
    name_file=paste0(na,"_",i,".txt")
    sink(name_file)
    cat(p)
    cat("\n")
    cat(p1) 
    cat("\n")
    cat(p2) 
    cat("\n")
    u=Map(function(x,y){write.table(t(c(" ",lon)),sep="\t",col.names=F,row.names=F,quote = F);write.table(x,sep="\t",col.names=F,row.names=T,quote = F);cat(y);cat("\n")},val_matrix,c(year_p[-1],""))
    sink()
  }
  
  return("Successful process")   
}

files_y=function(y_d,names){
  
  y_m=paste(y_d[,1],y_d[,2],sep="-")
  p="xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
  data=y_d[,-1:-2]
  row.names(data)=y_m
  p1=paste0("cpt:field=prcp, cpt:nrow=",dim(data)[1],", cpt:ncol=",dim(data)[2],", cpt:row=T, cpt:col=index, cpt:units=mm, cpt:missing=-999.000000000")
  name_file=paste0(main_dir,"/run_CPT/",names,"/","y_",names,".txt")
  sink(name_file)
  cat(p)
  cat("\n")
  cat(p1) 
  cat("\n")
  write.table(t(c(" ",substr(names(data),10,25))),sep="\t",col.names=F,row.names=F,quote = F)
  write.table(data,sep="\t",col.names=F,row.names=T,quote = F)
  sink()
  
  return(substr(names(data),2,9))
}

run_all=function(name,set,n,year_r){
  
  modes_x=as.numeric(set[1])
  modes_y=as.numeric(set[2]);if(n < modes_y)modes_y=n
  modes_cca=as.numeric(set[3]);if(min(c(modes_x,modes_y)) < modes_cca ) modes_cca=min(c(modes_x,modes_y))
  t_gamma=gsub(" ","",set[4])
  if(t_gamma=="si"){tran=541}else(tran=" ")
  
  dpto=substr(name,1,nchar(name)-4)
  month=substr(name,nchar(name)-2,nchar(name))
  dir=paste0(main_dir,"/","run_CPT/")
  
  f=year_r[1]
  l=year_r[length(year_r)]
  if(month=="Jan")l=l-1
  
  for(i in seq(0,0.9,0.1)){
    
    x_dir=paste0(dir,name,"/x_",i,".txt")
    y_dir=paste0(dir,dpto,"/y_",dpto,".txt")
    GI_dir=paste0(dir,name,"/output/","GI_",i,".txt")
    pear_dir=paste0(dir,name,"/output/","pear_",i,".txt")
    afc_dir=paste0(dir,name,"/output/","2afc_",i,".txt")
    prob_dir=paste0(dir,name,"/output/","prob_",i,".txt")
    cc_dir=paste0(dir,name,"/output/","cc_",i,".txt")
    run_dir=paste0(dir,name,"/run_",i,".bat")
    
    CPT=run_cpt(x_dir,y_dir,GI_dir,pear_dir,afc_dir,prob_dir,cc_dir,run_dir,modes_x,modes_y,modes_cca,tran,f,l)
    
  }
  
  return(print("finalizo un mes"))
  
}

proba=function(x,y){
  
  below=read.table(paste0(x,"/output/",y,"_prob",".txt"),header=T,nrow=1,dec=".",fill=T,skip=3,check.names = FALSE)
  normal=read.table(paste0(x,"/output/",y,"_prob",".txt"),header=T,nrow=1,dec=".",fill=T,skip=6,check.names = FALSE)
  above=read.table(paste0(x,"/output/",y,"_prob",".txt"),header=T,nrow=1,dec=".",fill=T,skip=9,check.names = FALSE)
  
  data=cbind.data.frame(month=which(month.abb==basename(x)),id=names(below),below=as.matrix(below)[1,],normal=as.matrix(normal)[1,],above=as.matrix(above)[1,])
  return(data)
  
}

metricas=function(x,y){
  
  p=read.table(paste0(x,"/output/",y,"_pearson",".txt"),header=T,dec=".",skip=2,col.names = c("id","pearson"))
  k=read.table(paste0(x,"/output/",y,"_2afc",".txt"),header=T,dec=".",skip=2,col.names = c("id","kendall"))
  g=read.table(paste0(x,"/output/",y,"_GI",".txt"),header=T,dec=".",skip=5)
  goodness=g[dim(g)[1],dim(g)[2]]
  c=read.table(paste0(x,"/output/",y,"_cca_cc",".txt"),header=T,dec=".",skip=2)
  canonica=c[1,1]
  p_k=merge(p,k)
  data=cbind(month=which(month.abb==basename(x)),p_k,goodness,canonica)
  
  return(data)
}

best_GI=function(x){
  
  names=strsplit(basename(x),"_")
  all_GI=lapply(x,function(x)read.table(x,header=T,dec=".",skip=5))  
  best=lapply(all_GI,function(x) x[dim(x)[1],dim(x)[2]] )
  pos=which(unlist(best)==max(unlist(best)))[1]
  return(names[[pos]][1])
  
}

save_areas=function(ras,cor,dec,name,ext){
  
  cor_raster=ras[[1]]
  values(cor_raster)=cor
  selec_raster=cor_raster>=quantile(cor,as.numeric(dec),na.rm=T)
  raster_final=crop(selec_raster,extent(ext))
  writeRaster(raster_final,name,format="ascii")
  return(print("área seleccionada guardada en formato Raster"))
  
}

########## Run ##############
start.time <- Sys.time()
options(timeout=180)
#############################
#path_dpto <- "D:/steven/plataforma//Confi"

path_dpto=dir_response

#dir_save <- "D:/steven/plataforma/Ejemplo_descarga"

#main_dir <- "D:/steven/plataforma/"
main_dir=dirPrediccionInputs
month=as.numeric(format(Sys.Date(),"%m"))
year=format(Sys.Date(),"%Y")
season=month+c(0,3)
season[season>12]=season[season>12]-12
path_months_l=lapply(paste0(main_dir,"run_CPT","/",list.files(path_dpto)),function(x)paste0(x,"/",month.abb[season]))
path_months=unlist(path_months_l)
o_empty_4=lapply(path_months,function(x)dir.create(x,recursive = T))
o_empty_5=lapply(paste0(path_months,"/output"),dir.create)
path_down=paste(dir_save,list.files(path_dpto),sep="/")
O_empty_1=lapply(path_down,dir.create)
path_areas=list.files(path_dpto,recursive = T,pattern = "areas",full.names = T)
data_areas=lapply(path_areas,function(x)read.table(x,header=T,sep=",",dec=".",na.strings = NA))
names(data_areas)=list.files(path_dpto)
data_areas_l=lapply(data_areas,function(x)split(as.matrix(x[,-1:-2]),col(x[,-1:-2])))
areas_final=lapply(data_areas_l,function(x)lapply(x,function(x1)c(na.omit(x1))))
n_areas_l=lapply(areas_final,function(x)lapply(x,length))
O_empty_2=Map(function(x,y,z){download.cpt(x,y,z,month,year)},path_down,areas_final,n_areas_l)
O_empty_2=Map(function(x,y,z){download.cpt(x,y,z,month,year)},path_down,areas_final,n_areas_l)

cat("\n Archivos de la TSM descargados \n")

all_path_down=lapply(paste(dir_save,list.files(path_dpto),sep = "/"),function(x)list.files(x,full.names = T))
gz_o=lapply(all_path_down,function(x)lapply(x,function(x1)gunzip(x1)))
all_path_down=lapply(paste(dir_save,list.files(path_dpto),sep = "/"),function(x)list.files(x,full.names = T))
tsm_o=lapply(all_path_down,function(x)lapply(x,function(x1)read.table(x1,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)))
time=lapply(tsm_o,function(x)lapply(x,function(x1) as.character(x1[1,])[-1]))
time_sel=lapply(time,function(x)lapply(x,function(x1)x1[x1!="NA"]))

cat("\n Archivos de TSM cargados y descomprimidos \n")

data_x=lapply(tsm_o,function(x)lapply(x,data_raster))
coor_extent=lapply(areas_final,function(x1)lapply(x1,function(x) c(min(x[c(1,2,5,6)],na.rm=T)-1,max(x[c(1,2,5,6)],na.rm=T)+1,min(x[c(3,4,7,8)],na.rm=T)-1,max(x[c(3,4,7,8)],na.rm=T)+1)))
extent_season=lapply(coor_extent,function(x)x[season])
data_x_crop=Map(function(x, y) Map(function(x,y) crop(x,extent(y)),x,y),data_x,extent_season)

cat("\n Datos de la TSM organizados en formato raster")

path_stations=list.files(path_dpto,recursive = T,pattern = "stations",full.names = T)
data_y=lapply(path_stations,function(x)read.table(x,dec=".",sep = ",",header = T))
names(data_y)=list.files(path_dpto)

cat("\n Datos de precipitacion cargados \n")

part_id=Map(files_y,data_y,list.files(path_dpto))

cat("\n Archivos de las estaciones construidos para CPT \n")

path_confi=list.files(path_dpto,recursive = T,pattern = "cpt",full.names = T)
data_confi=lapply(path_confi,function(x)read.table(x,header=T,sep=",",dec=".",row.names = 1,stringsAsFactors = FALSE))
confi_selec=lapply(data_confi,function(x)x[,season])
confi_l=lapply(confi_selec,function(x) as.list(x))
p_data=lapply(data_y,function(x)dim(x)[2]-2)

cat("\n Configuración CPT cargada \n")

path_x <- lapply(list.files(dir_save,full.names = T),function(x)list.files(x,recursive = T,full.names = T))
path_zone<- list.files(paste0(main_dir,"run_CPT"),full.names = T) %>% paste0(.,"/y_",list.files(path_dpto),".txt")
path_output_pred <- lapply(path_months_l,function(x)paste0(x,"/output/0_"))
path_run <- lapply(path_months_l,function(x)paste0(x,"/run_0.bat"))
first_run <- Map(function(x,y,z,k,p1,p2)Map(run_cpt,x,y,z,k,p1,p2),path_x,path_zone,path_run,path_output_pred,confi_l,p_data)

cat("\n Primera corrida realizada")

path_cc <- lapply(path_output_pred,function(x)paste0(x,"cca_cc.txt"))
path_load <- lapply(path_output_pred,function(x)paste0(x,"cca_load_x.txt"))
cc <-  lapply(path_cc,function(x)lapply(x,function(x1)read.table(x1,sep="\t",dec=".",header = T,row.names = 1,skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)))
load <- lapply(path_load,function(x)lapply(x,function(x1)read.table(x1,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)))
cor_tsm <- Map(function(x,y)Map(correl,x,y),cc,load)

cat("\n Correlación calculada")

names_selec <-lapply(path_x,function(x)substr(x,1,nchar(x)-4))
o_empty_1=Map(function(x,y,z,r)Map(files_x,x,y,z,r),data_x,cor_tsm,names_selec,time_sel)

cat("\n Archivos de la TSM construidos por deciles para CPT \n")

path_x_2 <- lapply(list.files(dir_save,full.names = T),function(x)list.files(x,recursive = T,full.names = T,pattern = "0."))
path_output_pred_2 <- lapply(path_months_l,function(x) lapply(x, function(x)paste0(x,"/output/",seq(0.1,0.9,0.1),"_"))) %>% lapply(.,unlist)
path_run_2 <-lapply(path_months_l,function(x) lapply(x, function(x)paste0(x,"/run_",seq(0.1,0.9,0.1),".bat"))) %>% lapply(.,unlist)
second_run <- Map(function(x,y,z,k,p1,p2)Map(run_cpt,x,y,z,k,p1,p2),path_x_2,path_zone,path_run_2,path_output_pred_2,confi_l,p_data)

cat("\n Segunda corrida realizada\n")

path_out_run=lapply(path_months_l,function(x)paste0(x,"/output"))
o_e=lapply(path_out_run,function(x)lapply(x,function(x)list.files(x,full.names = T,recursive = T,pattern = "GI")))
best_decil_l=lapply(o_e,function(x)lapply(x,best_GI))
best_decil=lapply(best_decil_l,unlist)

cat("\n Mejor corrida seleccionada \n")

path_resul=path_save
o_metricas=Map(function(x,y)Map(metricas,x,y),path_months_l,best_decil)
years=format(seq(Sys.Date(), by = "month", length = 2) ,"%Y")
metricas_l=lapply(o_metricas,function(x)Map(function(x,y)cbind(year=y,x),x,years))
metricas_all=Map(function(x,y)lapply(x,function(x,y){x$id=paste0(y,x$id);x},y),metricas_l,part_id)
metricas_final=do.call("rbind",lapply(metricas_all,function(x)do.call("rbind",x)))

#write.csv(metricas_final,paste0(path_resul,"/metrics.csv"),row.names=FALSE)
tbl_df(metricas_final) %>%
  mutate(year = year, 
         month = as.integer(month)) %>%
  write_csv(paste0(path_save,"/","metrics.csv"))

cat("\n Metricas de validacion almacenadas \n")

o_prob=Map(function(x,y)Map(proba,x,y),path_months_l,best_decil)
prob_l=lapply(o_prob,function(x)Map(function(x,y)cbind(year=y,x),x,years))
prob_all=Map(function(x,y)lapply(x,function(x,y){x$id=paste0(y,x$id);x},y),prob_l,part_id)
prob_final=do.call("rbind",lapply(prob_all,function(x)do.call("rbind",x)))

#write_csv(prob_final,paste0(path_resul,"/probabilities.csv"),row.names=FALSE)
tbl_df(prob_final) %>%
  mutate(year = year,
         month =month,
         below = below / 100,
         normal = normal / 100,
         above = above /100) %>%
  write_csv(paste0(path_save,"/","probabilities.csv"))


cat("\n Pronosticos probabilisticos almacenados \n")

dir.create(paste0(main_dir,"/raster"))
o_empty_3=lapply(paste0(main_dir,"raster","/",list.files(path_dpto)),dir.create)
path_raster=lapply(paste0(main_dir,"raster","/",list.files(path_dpto)),function(x)paste0(x,"/",years,"_",month.abb[season],".asc"))
O_empty_8=Map(function(x,y,z,k,l)Map(save_areas,x,y,z,k,l),data_x,cor_tsm,best_decil,path_raster,extent_season)

cat("\n áreas almacenadas en formato Raster \n")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
