########## Functions ##########

download.cpt=function(dir_save,areas_l,n_areas_l,month,year){ 
  
  season=(month)+(-1:6)
  if(sum(season>12)>0)season[which(season>12)]=season[which(season>12)]-12
  if(sum(season<1)>0)season[which(season<1)]=season[which(season<1)]+12
  
  areas=areas_l[season[2:7]]
  n_areas=n_areas_l[season[2:7]]
  
  i_con=month-2
  if(i_con<=0)i_con=i_con+12
  
  for(i in 1:6){
    
    if(n_areas[[i]]==4){
      
      route=paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[i_con],"%201982-",year,"%29/VALUES/L/",i,".5/",i+2,".5/RANGE/%5BL%5D//keepgrids/average/M/1/24/RANGE/%5BM%5Daverage/X/",areas[[i]][1],"/",areas[[i]][2],"/flagrange/Y/",areas[[i]][3],"/",areas[[i]][4],"/flagrange/add/1/flaggt/mul/0/setmissing_value/%5BX/Y%5D%5BS/L/add/%5Dcptv10.tsv.gz")
                   
    }else{
      
      route=paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[i_con],"%201982-",year,"%29/VALUES/L/",i,".5/",i+2,".5/RANGE/%5BL%5D//keepgrids/average/M/1/24/RANGE/%5BM%5Daverage/X/",areas[[i]][1],"/",areas[[i]][2],"/flagrange/Y/",areas[[i]][3],"/",areas[[i]][4],"/flagrange/add/1/flaggt/X/",areas[[i]][5],"/",areas[[i]][6],"/flagrange/Y/",areas[[i]][7],"/",areas[[i]][8],"/flagrange/add/1/flaggt/add/mul/0/setmissing_value/%5BX/Y%5D%5BS/L/add/%5Dcptv10.tsv.gz")
      
    }
    
    filePath <- paste(dir_save,"/",i,"_",paste(month.abb[season[i:(i+2)]], collapse = '_'),".tsv.gz",sep="")
    
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
  pos=sort(rep(year,pos[2]-pos[1]))
  list_dates=split(data_cpt1,pos)
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

run_cpt=function(x,y,GI,pear,afc,prob,cc,path_run,m_x,m_y,m_cca,t,first,last){
  
  cmd <- "@echo off
  (
  echo 611
  echo 545
  echo 1
  echo %path_x%
  echo 90
  echo -90
  echo 0
  echo 359
  echo 1
  echo %mode_x%
  echo 2
  echo %path_y%
  echo 1
  echo %mode_y%
  echo 1
  echo %mode_cca%
  echo 532
  echo %first_y%
  echo %last_y%
  echo N
  echo 2
  echo 9
  echo 1
  echo 554
  echo 2
  echo %transfor%
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
  echo 111
  echo 501
  echo %path_prob%
  echo 401
  echo %path_cc%
  echo 0
  echo 0
  ) | CPT_batch.exe"
  
  cmd<-gsub("%path_x%",x,cmd)
  cmd<-gsub("%path_y%",y,cmd)
  cmd<-gsub("%path_GI%",GI,cmd)
  cmd<-gsub("%path_pear%",pear,cmd)
  cmd<-gsub("%path_2afc%",afc,cmd)
  cmd<-gsub("%path_prob%",prob,cmd)
  cmd<-gsub("%path_cc%",cc,cmd)
  cmd<-gsub("%mode_x%",m_x,cmd)
  cmd<-gsub("%mode_y%",m_y,cmd)
  cmd<-gsub("%mode_cca%",m_cca,cmd)
  cmd<-gsub("%transfor%",t,cmd)
  cmd<-gsub("%first_y%",first,cmd)
  cmd<-gsub("%last_y%",last,cmd)
  write(cmd,path_run)
  system(path_run, ignore.stdout = T, show.output.on.console = T)
  
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

metricas=function(x,y){
  
  p=read.table(paste0(x,"/output","/pear_",y,".txt"),header=T,dec=".",skip=2,col.names = c("id","pearson"))
  k=read.table(paste0(x,"/output","/2afc_",y,".txt"),header=T,dec=".",skip=2,col.names = c("id","kendall"))
  g=read.table(paste0(x,"/output","/GI_",y,".txt"),header=T,dec=".",skip=5)
  goodness=g[dim(g)[1],dim(g)[2]]
  c=read.table(paste0(x,"/output","/cc_",y,".txt"),header=T,dec=".",skip=2)
  canonica=c[1,1]
  p_k=merge(p,k)
  data=cbind(month=which(month.abb==basename(x)),p_k,goodness,canonica)
  
  return(data)
}

proba=function(x,y){
  
  below=read.table(paste0(x,"/output","/prob_",y,".txt"),header=T,nrow=1,dec=".",fill=T,skip=3,check.names = FALSE)
  normal=read.table(paste0(x,"/output","/prob_",y,".txt"),header=T,nrow=1,dec=".",fill=T,skip=6,check.names = FALSE)
  above=read.table(paste0(x,"/output","/prob_",y,".txt"),header=T,nrow=1,dec=".",fill=T,skip=9,check.names = FALSE)
  
  data=cbind.data.frame(month=which(month.abb==basename(x)),id=names(below),below=as.matrix(below)[1,],normal=as.matrix(normal)[1,],above=as.matrix(above)[1,])
  return(data)
  
}

best_GI=function(x){
  
  names=substr(basename(x),4,nchar(basename(x))-4)
  all_GI=lapply(x,function(x)read.table(x,header=T,dec=".",skip=5))  
  best=lapply(all_GI,function(x) x[dim(x)[1],dim(x)[2]] )
  pos=which(unlist(best)==max(unlist(best)))[1]
  return(names[pos])
  
}

save_areas=function(ras,cor,dec,name,ext){
  
  cor_raster=ras[[1]]
  pos=!is.na(values(cor_raster))
  values(cor_raster)[pos]=cor
  selec_raster=cor_raster>=quantile(cor,as.numeric(dec))
  raster_final=crop(selec_raster,extent(ext))
  writeRaster(raster_final,name,format="ascii")
  return(print("Área seleccionada guardada en formato Raster"))
  
}

########## Run ##############
start.time <- Sys.time()
#############################

#path_dpto="C:/Users/dagudelo/Desktop/Confi"
path_dpto=dir_response
#dir_save="C:/Users/dagudelo/Desktop/Ejemplo_descarga"
path_down=paste(dir_save,list.files(path_dpto),sep="/")
O_empty_1=lapply(path_down,dir.create)
path_areas=list.files(path_dpto,recursive = T,pattern = "areas",full.names = T)
data_areas=lapply(path_areas,function(x)read.table(x,header=T,sep=",",dec=".",na.strings = NA))
names(data_areas)=list.files(path_dpto)
data_areas_l=lapply(data_areas,function(x)split(as.matrix(x[,-1:-2]),col(x[,-1:-2])))
areas_final=lapply(data_areas_l,function(x)lapply(x,function(x1)c(na.omit(x1))))
n_areas_l=lapply(areas_final,function(x)lapply(x,length))
month=as.numeric(format(Sys.Date(),"%m"))
year=format(Sys.Date(),"%Y")
O_empty_2=Map(function(x,y,z){download.cpt(x,y,z,month,year)},path_down,areas_final,n_areas_l)

cat("\n Archivos de la TSM descargados \n")

all_path_down=lapply(paste(dir_save,list.files(path_dpto),sep = "/"),function(x)list.files(x,full.names = T))
gz_o=lapply(all_path_down,function(x)lapply(x,function(x1)gzfile(x1,'rt')))
tsm_o=lapply(all_path_down,function(x)lapply(x,function(x1)read.table(x1,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)))
time=lapply(tsm_o,function(x)lapply(x,function(x1) as.character(x1[1,])[-1]))
time_sel=lapply(time,function(x)lapply(x,function(x1)x1[x1!="NA"]))

cat("\n Archivos de TSM cargados y descomprimidos \n")

data_x=lapply(tsm_o,function(x)lapply(x,data_raster))
coor_extent=lapply(areas_final,function(x1)lapply(x1,function(x) c(min(x[c(1,2,5,6)],na.rm=T)-1,max(x[c(1,2,5,6)],na.rm=T)+1,min(x[c(3,4,7,8)],na.rm=T)-1,max(x[c(3,4,7,8)],na.rm=T)+1)))
season=month+0:5
season[season>12]=season[season>12]-12
extent_season=lapply(coor_extent,function(x)x[season])
data_x_crop=Map(function(x, y) Map(function(x,y) crop(x,extent(y)),x,y),data_x,extent_season)

cat("\n Datos de la TSM organizados en formato raster")

data_tsm=lapply(data_x,function(x)lapply(x,function(x) t(rasterToPoints(x))[-1:-2,]))
names(data_tsm)=list.files(path_dpto)
data_tsm=lapply(data_tsm,function(x){names(x)=month.abb[season];return(x)})
year_predictor=lapply(data_tsm,function(x)lapply(x,function(x) as.numeric(substr(rownames(x), 2, 5))))
lat=lapply(data_x_crop,function(x)lapply(x,function(x) t(rasterToPoints(x))[2,]))

cat("\n Datos de la TSM organizados en formato Años X Pixeles \n")

path_stations=list.files(path_dpto,recursive = T,pattern = "stations",full.names = T)
data_y=lapply(path_stations,function(x)read.table(x,dec=".",sep = ",",header = T))
names(data_y)=list.files(path_dpto)

cat("\n Datos de precipitacion cargados \n")

data_quar=lapply(data_y,quarterly_data,month)
data_quartely=unlist(lapply(data_quar,"[", 1),recursive=FALSE)
year_response=unlist(lapply(data_quar,"[", 2),recursive=FALSE)

cat("\n Datos de precipitacion organizados de forma trimestral \n")

year_model=Map(function(x,y) Map(function(x1,y1) years_model=intersect(x1,y1),x, y),year_predictor,year_response)
years_final_res=Map(function(x,y) Map(function(x1,y1) pos_x=x1%in%y1 ,x,y),year_response,year_model)
years_final_prec=Map(function(x,y) Map(function(x1,y1) pos_x=x1%in%y1 ,x,y),year_predictor,year_model)
data_tsm_final=Map(function(x,y) Map(function(x1,y1) x1[y1,] ,x,y),data_tsm,years_final_prec)
data_res_final=Map(function(x,y) Map(function(x1,y1) x1[y1,] ,x,y),data_quartely,years_final_res)

cat("\n Periodo de entrenamiento generado \n")

ponde=lapply(lat,function(x)lapply(x,function(x) sqrt(cos((pi/180)*x))))

cat("\n Ponderación PCA \n")

path_confi=list.files(path_dpto,recursive = T,pattern = "cpt",full.names = T)
data_confi=lapply(path_confi,function(x)read.table(x,header=T,sep=",",dec=".",row.names = 1,stringsAsFactors = FALSE))
confi_selec=lapply(data_confi,function(x)x[,season])
confi_l=lapply(confi_selec,function(x) as.list(x))
n_data=lapply(data_res_final,function(x)lapply(x,function(x)dim(x)[2]))

cat("\n Configuración CPT cargada \n")

cor_tsm=Map(function(x,y,z,k) Map(selection_area,x,y,z,k),data_tsm_final,data_res_final,ponde,confi_l)

cat("\n Correlación de los pixeles calculada \n")

main_dir=dirPrediccionInputs
dir.create(paste0(main_dir,"/run_CPT"))
o_empty_3=lapply(paste0(main_dir,"/run_CPT","/",list.files(path_dpto)),dir.create)
path_months=unlist(lapply(paste0(main_dir,"/run_CPT","/",list.files(path_dpto)),function(x)paste0(x,"/",month.abb[season])))
o_empty_4=lapply(path_months,dir.create)
o_empty_5=lapply(paste0(path_months,"/output"),dir.create)
names_all=lapply(list.files(path_dpto),function(x) paste0(x,"/",month.abb[season]))
o_empty_6=Map(function(x,y,z,r)Map(files_x,x,y,z,r),data_x,cor_tsm,names_all,time_sel)

cat("\n Archivos de la TSM construidos por deciles para CPT \n")

part_id=Map(files_y,data_y,list.files(path_dpto))

cat("\n Archivos de las estaciones construidos para CPT \n")

O_empty_8=Map(function(x,y,z,k)Map(run_all,x,y,z,k),names_all,confi_l,n_data,year_response)

cat("\n Batch CPT realizado \n")

path_out_run=lapply(paste0(main_dir,"/run_CPT","/",list.files(path_dpto)),function(x)paste0(x,"/",month.abb[season]))
o_e=lapply(path_out_run,function(x)lapply(x,function(x)list.files(x,full.names = T,recursive = T,pattern = "GI")))
best_decil_l=lapply(o_e,function(x)lapply(x,best_GI))
best_decil=lapply(best_decil_l,unlist)

cat("\n Mejor corrida seleccionada \n")

path_resul=path_save
o_metricas=Map(function(x,y)Map(metricas,x,y),path_out_run,best_decil)
years=format(seq(Sys.Date(), by = "month", length = 6) ,"%Y")
metricas_l=lapply(o_metricas,function(x)Map(function(x,y)cbind(year=y,x),x,years))
metricas_all=Map(function(x,y)lapply(x,function(x,y){x$id=paste0(y,x$id);x},y),metricas_l,part_id)
metricas_final=do.call("rbind",lapply(metricas_all,function(x)do.call("rbind",x)))

#write.csv(metricas_final,paste0(path_resul,"/metrics.csv"),row.names=FALSE)
tbl_df(metricas_final) %>%
  mutate(year = year, 
         month = as.integer(month)) %>%
  write_csv(paste0(path_save,"/","metrics.csv"))

cat("\n Metricas de validacion almacenadas \n")

o_prob=Map(function(x,y)Map(proba,x,y),path_out_run,best_decil)
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
o_empty_3=lapply(paste0(main_dir,"/raster","/",list.files(path_dpto)),dir.create)
path_raster=lapply(paste0(main_dir,"/raster","/",list.files(path_dpto)),function(x)paste0(x,"/",year,"_",month.abb[season],".asc"))
O_empty_8=Map(function(x,y,z,k,l)Map(save_areas,x,y,z,k,l),data_x,cor_tsm,best_decil,path_raster,extent_season)

cat("\n Áreas almacenadas en formato Raster \n")
cat("\n Áreas almacenadas en formato Raster \n")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




