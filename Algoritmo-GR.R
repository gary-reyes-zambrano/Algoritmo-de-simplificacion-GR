#=====CONGFIG===================================================================
rm(list = ls())
options( digits = 15 )

#=====FUNCIONES=================================================================

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak2 <- function(){
  if(!"RPostgres" %in% installed.packages()[, "Package"]){
    install.packages('devtools')
    install.packages('remotes')
    remotes::install_github("r-dbi/RPostgres")
    install.packages('RPostgres')
  }
  library(DBI)
  library(RPostgres)
}

get_algoritmo <- function(algoritmo){
  return("GR")
  return("Error")
}

get_dataset <- function(selected_dataset, inicial, muestra){
  # Credenciales de la base de datos ####
  dbdriver <- "PostgreSQL"
  host <- 'localhost'
  port <- '5432'
  dbname <- 'datos-gps' #'CPPP2'
  user <- 'postgres'
  pass <- 'password'
  
  # Consulta
  drv <- dbDriver(dbdriver) #RPostgres::Postgres()#
  con <- dbConnect(drv, host = host, port = port, dbname = dbname, user = user, pass = pass)
  # TRUE = YYYY-MM-DD
  # FALSE = d+
  if (selected_dataset == 1) {
    dataset <- "Mobile_Century-California"
    time_in_timestamp = 'FALSE'
    #data <- dbGetQuery(con, "SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select latitude, longitude, unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.california where concat(folder_name, ' ', file_name) in (select id from base.indices_trayectorias where db = 'mobile-century-a3' limit 50)  order by unixtime asc) s left JOIN public.\"california-1\" h ON ST_DWithin(s.the_geom, h.geog, 4) order by unixtime asc")
    data <- data.frame()
    indices <- dbGetQuery(con, "select id, cant from base.indices_trayectorias where db = 'mobile-century-a3' order by cant asc")
    k <- round(nrow(indices) / muestra, digits = 0)
    while(inicial > k){
      inicial <- inicial - k
    }
    ultim <- 0
    for(i in 1:muestra){
      id_tr <- inicial + (k * (i - 1))
      if(id_tr < nrow(indices)){
        name_tr <- indices[id_tr, "id"]
        print(paste(i,":",name_tr, id_tr))
        ultim <- i
        data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select latitude, longitude, unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.california where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' order by unixtime asc) s left JOIN public.\"california-1\" h ON ST_DWithin(s.the_geom, h.geog, 3) order by unixtime asc", sep=""))
        data <- rbind(data, data_trajectory)
      }else{
        next
      }
    }
    for(i in 1:(muestra-ultim)){
      id_tr <- inicial + 1 + (k * (i - 1))
      name_tr <- indices[id_tr, "id"]
      print(paste(ultim + i,":",name_tr, id_tr))
      data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select latitude, longitude, unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.california where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' order by unixtime asc) s left JOIN public.\"california-1\" h ON ST_DWithin(s.the_geom, h.geog, 3) order by unixtime asc", sep=""))
      data <- rbind(data, data_trajectory)
    }
    
  }else if (selected_dataset == 2) {
    dataset <- "T-Drive"
    time_in_timestamp = 'TRUE'
    #data <- dbGetQuery(con, "SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _x longitude, _y latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.t_drive where concat(folder_name, ' ', file_name) in (select id from base.indices_trayectorias where db = 't-drive-a3') and _x between '115.940505' and '117.27788' and _y between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4) order by unixtime asc")
    
    data <- data.frame()
    indices <- dbGetQuery(con, "select id, cant from base.indices_trayectorias where db = 't-drive-a3' order by cant asc")
    k <- round(nrow(indices) / muestra, digits = 0)
    while(inicial > k){
      inicial <- inicial - k
    }
    ultim <- 0
    for(i in 1:muestra){
      id_tr <- inicial + (k * (i - 1))
      if(id_tr < nrow(indices)){
        name_tr <- indices[id_tr, "id"]
        print(paste(i,":",name_tr, ":",id_tr))
        ultim <- i
        data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _x longitude, _y latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.t_drive where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' and _x between '115.940505' and '117.27788' and _y between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4.5) order by unixtime asc", sep=""))
        data <- rbind(data, data_trajectory)
      }else{
        next
      }
    }
    if(ultim < muestra){
      for(i in 1:(muestra-ultim)){
        id_tr <- inicial + 1 + (k * (i - 1))
        name_tr <- indices[id_tr, "id"]
        print(paste(ultim + i,":",name_tr, id_tr))
        data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _x longitude, _y latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.t_drive where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' and _x between '115.940505' and '117.27788' and _y between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4.5) order by unixtime asc", sep=""))
        data <- rbind(data, data_trajectory)
      }
    }
    
    
    #data <- data[order(data$unixtime, decreasing = FALSE),]
    #data_a <- data[!duplicated(data),]
  }else if (selected_dataset == 3) {
    dataset <- "Geolife"
    time_in_timestamp = 'TRUE'
    #data <- dbGetQuery(con, "SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _y longitude, _x latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.plt where concat(folder_name, ' ', file_name) in (select id from base.indices_trayectorias where db = 'geolife-a3') and _y between '115.940505' and '117.27788' and _x between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4) order by unixtime asc")
    
    data <- data.frame()
    indices <- dbGetQuery(con, "select id, cant from base.indices_trayectorias where db = 'geolife-a3' order by cant asc")
    k <- round(nrow(indices) / muestra, digits = 0)
    while(inicial > k){
      inicial <- inicial - k
    }
    ultim <- 0
    for(i in 1:muestra){
      id_tr <- inicial + (k * (i - 1))
      if(id_tr < nrow(indices)){
        name_tr <- indices[id_tr, "id"]
        print(paste(i,":",name_tr, ":",id_tr))
        ultim <- i
        data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _y longitude, _x latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.plt where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' and _y between '115.940505' and '117.27788' and _x between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4.5) order by unixtime asc", sep=""))
        data <- rbind(data, data_trajectory)
      }else{
        next
      }
    }
    if(ultim < muestra){
      for(i in 1:(muestra-ultim)){
        id_tr <- inicial + 1 + (k * (i - 1))
        name_tr <- indices[id_tr, "id"]
        print(paste(ultim + i,":",name_tr, id_tr))
        data_trajectory <- dbGetQuery(con, paste("SELECT longitude, latitude, unixtime, file_name, h.geog FROM (select _y longitude, _x latitude, date_hour unixtime, concat(folder_name, ' ', file_name) file_name, the_geom from base.plt where concat(folder_name, ' ', file_name) = \'", name_tr ,"\' and _y between '115.940505' and '117.27788' and _x between '39.30397' and '40.54583' order by unixtime asc) s left JOIN public.\"tdrive-4\" h ON ST_DWithin(s.the_geom, h.geog, 4.5) order by unixtime asc", sep=""))
        data <- rbind(data, data_trajectory)
      }
    }
  
  }
  
  array_name <- data.frame(file_name = unique(data$file_name)) 
  dbDisconnect(con)
  
  # todas <- data.frame(longitude = c(1:nrow(data)), latitude = 0, file_name = 0, unixtime = 0, geom=0) 
  # todas$longitude <- data$longitude
  # todas$latitude  <- data$latitude
  # todas$file_name <- data$file_name
  # todas$unixtime  <- if(time_in_timestamp == 'TRUE'){ as.POSIXct(data[,'unixtime']/1000, origin="1970-01-01")}else{data[,'unixtime']}
  # todas$geom      <- data$geom
  
  pto_medio <- matrix(0, nrow=1, ncol=2)
  #min_latitud <- min(todas[,'latitude']); max_latitud <- max(todas[,'latitude']); 
  #men_longitud <- min(todas[,'longitude']); max_longitud <- max(todas[,'longitude']);
  #pto_medio[2] <- (max_latitud + min_latitud)/2   
  #pto_medio[1] <- (max_longitud + men_longitud)/2
  
  result <- list(dataset, nrow(data), nrow(data),pto_medio,summary(data),data,array_name,unique(data$file_name), indices)
  names(result) <- c('dataset','originalsize','size','central_point','summary','data',"array_name","trayectorias", "indices")
  return(result)
}

get_Workspace<- function(algoritmo, dataset, kalman){
	#C:/ Compressing / Simplificacion+Fecha / DataSet / Hora+[Con-Sin] Kalman  / [Data]
  simbol<-"\\"
  root<- "C:"
  setwd(paste("C:",simbol,sep=""))
  
  dir.create("Compressing")
  setwd(paste(getwd(),simbol,"Compressing",sep=""))
  
	SF <- paste(get_algoritmo(algoritmo), Sys.Date(), sep = " - ")
  dir.create(SF)
  setwd(paste(getwd(), simbol, SF, sep=""))
  
  dir.create(dataset)
  setwd(paste(getwd(), simbol, dataset, sep=""))

  hora <- format(Sys.time(), "%H-%M-%S")
  if(kalman){
  	horak <- paste(hora, "Con Kalman", sep = " - ")
  }else{
  	horak <- paste(hora, "Sin Kalman", sep = " - ")
  }
  dir.create(horak)
  setwd(paste(getwd(), simbol, horak, sep=""))
  
  return(getwd())
}

get_muestra <- function(poblacion, confianza=0.95, error=0.05, sigma=0.5){
  tabla_confianza <- data.frame(confianza=c(0.99,0.95,0.9),error=c(0.01,0.05,0.1),Z=c(2.58,1.96,1.645))
  Z <- tabla_confianza[tabla_confianza$confianza==confianza,"Z"]
  numerador <- poblacion * (Z^2) * (sigma^2)
  denominador <- ((poblacion - 1) * (error^2)) + ((Z^2) * (sigma^2))
  muestra <-  numerador / denominador
  return(round(muestra,0))
}

get_type <- function(algoritmo){
  if(algoritmo==1){
    return(1)
  }else if(algoritmo==2){
    return(2)
  }else if(algoritmo==3){
    return(2)
  }
  return(0)
}

#=====FUNCIONES=================================================================

segmentate <- function(TR){
  return(length(TR))
}

loc_dist <-function(end, start) {
  # """ Spatial distance between two points (end-start)
  #   Args:
  #       start (:obj:`Point`)
  #       end (:obj:`Point`)
  #   Returns:
  #       float, distance in m
  #  """
  return (distance_f(end,start))
}

time.difference<- function(self, previous){
  #""" Calcultes the time difference against another point
  #     Args:
  #          previous (:obj:`Point`): Point before
  #      Returns:
  #          Time difference in seconds
  #      """
  return(abs(self[,c('unixtime')]-previous[,c('unixtime')]) )
}

time_dist<- function(end, start){
  #""" Temporal distance between two points (end-start)
  #  Args:
  #      start (:obj:`Point`)
  #      end (:obj:`Point`)
  # Returns:
  #      float, time difference in seconds
  #  """
  return(time.difference(end,start))
}

distance_f<-function(p_a, p_b){
  #  p_a=points[i,]
  #  p_b= point
  #""" Euclidean distance, between two points
  
  #  Args:
  #      p_a (:obj:`Point`)
  #      p_b (:obj:`Point`)
  #  Returns:
  #      float: distance, in degrees
  #  """
  return (sqrt((p_a$latitude-p_b$latitude)^2 + (p_a$longitude-p_b$longitude)^2) )
}

td_tr<-function(points, dist_threshold){
  #points = lista_trayectorias[[j]]; dist_threshold = dist_threshold[[j]];
  #""" Top-Down Time-Ratio Trajectory Compression Algorithm
  #Detailed in https://www.itc.nl/library/Papers_2003/peer_ref_conf/meratnia_new.pdf
  #Args:
  #   points (:obj:`list` of :obj:`Point`): trajectory or part of it
  #   dist_threshold (float): max distance error, in meters
  #Returns:
  #  :obj:`list` of :obj:`Point`, compressed trajectory
  #"""
  #http://tinyurl.com/rbd7n2r
  if (nrow(points)<=2) {
    return(points)
  }else {
    max_dist_threshold = 0
    found_index = 0
    delta_e = as.numeric(time_dist(points[nrow(points),],points[1,]), units = "secs") * I_3600 #segundos
    d_lat = points[nrow(points),][,'latitude'] - points[1,][,'latitude']
    d_lon = points[nrow(points),][,'longitude'] - points[1,][,'longitude']
    
    for (i in 2:(nrow(points)-1)) {
      delta_i= as.numeric(time_dist(points[i,],points[1,]), units = "secs")  * I_3600
      di_de = delta_i / delta_e
        if(is.nan(di_de) == FALSE){ 
          point = data.frame(longitude = points[1,]$longitude + d_lon * di_de,
                               latitude = points[1,]$latitude + d_lat * di_de,
                               unixtime=0)
            dist = loc_dist(points[i,], point)#loc_dist(points[1,], points[nrow(points),])
            if (dist > max_dist_threshold) {
              max_dist_threshold = dist
              found_index = i
            }
        }
    }
  }  
  if ( max_dist_threshold > dist_threshold ) {
    one = td_tr(points =  points[1:found_index,], dist_threshold = dist_threshold)
    two = td_tr(points =  points[found_index:nrow(points),], dist_threshold = dist_threshold)
    two=two[2:nrow(two),]
    one = rbind(one,two)
    return(one)
  }else{
    return(rbind(points[1,],points[nrow(points),]))
  }
}

point_line_distance<- function(point, start, end){
  # """ Distance from a point to a line, formed by two points
  #   Args:
  #       point (:obj:`Point`)
  #       start (:obj:`Point`): line point
  #       end (:obj:`Point`): line point
  #   Returns:
  #       float: distance to line, in degrees
  #   """
  if (isTRUE(compare(start,end))== TRUE ) {
    return(distance_f(point,start))
  }else{
    un_dist<-abs(((end$latitude-start$latitude) * (start$longitude-point$longitude))
                 - ((start$latitude-point$latitude) * (end$longitude-start$longitude)))
    n_dist<- sqrt((end$latitude-start$latitude)^2 + (end$longitude-start$longitude)^2)
    if (n_dist==0) {
      return(0)
    }else{
      return(un_dist/n_dist)
    }
  }
}

drp<-function(points, dist_threshold){
  #""" Douglas ramer peucker
  #
  #   Based on https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
  #
  #   Args:
  #      points (:obj:`list` of :obj:`Point`)
  #     dist_threshold (float): drp threshold
  # Returns:
  #    :obj:`list` of :obj:`Point`
  #"""
  #http://tinyurl.com/rbd7n2r
  dmax = 0
  index = 0
  if (nrow(points) >1) {
    for (i in 2:nrow(points)-1) {
      dist<-point_line_distance(points[i,],points[1,],points[nrow(points),])
      
      if (dist > dmax) {
        index = i
        dmax = dist
      }
    }
    if (dmax > dist_threshold) {
      one=drp(points[1:index,],dist_threshold)
      two=drp(points[index:nrow(points),],dist_threshold)
      one <- one[1:nrow(one)-1,]
      return(rbind(one, two))
    }else{
      return(rbind(points[1,],points[nrow(points),]))
    }
  }else{return(points)}
}

#=====FUNCIONES=================================================================

graficar_inicial <- function(lista_trayectoria, trayectorias_informacion){
  par(lwd=2) 
  plot(0 ,ylim=c(min_latitud , max_latitud) , xlim= c(men_longitud , max_longitud ), xlab = 'Longitud', ylab = 'Latitud',main=dataset)
  for(a in 1:length(lista_trayectoria)){
    color <- trayectorias_informacion[a,'Centroide'] 
    if(color == 0 || color == 'None'){
      color <- 'black'
    }
    tray_aux <- lista_trayectoria[[a]]
    points(tray_aux[,1:2], pch=46, col=color, cex=2.5)# color 64
  }
}

preprocesar_informacion <-  function (lista_trayectorias){
  cant_trayectorias <- length(lista_trayectorias)
  trayectorias_informacion <- data.frame(No_Trayectoria = c(1:cant_trayectorias),Cant_puntos_originales = 0,Puntos_filtro_redes_viales = 0, Cant_puntos_simplificados = 0, Proyeccion_MAE = 0, Centroide = 0, silhouette = 0,Velocidad_Prom = 0, Men_Latitud =0 ,Long_Corresp1 = 0 ,May_Latitud  =0 ,Long_Corresp2=0 ,Men_Longitud=0, Lat_Corresp1 = 0, May_Longitud=0 , Lat_Corresp2 = 0 , Dist_ancho=0, Dist_alto = 0,tiempo_recorrido=0)
  
  for(i in 1:cant_trayectorias){
    #print(i)
    tray_actual <- lista_trayectorias[[i]]
    cant_puntos <- nrow(tray_actual)
    
    ##ordeno de menor a mayor   y 
    tray_order1 <- tray_actual[order(tray_actual$latitude, decreasing=FALSE),]
    #trayectorias_informacion$Men_Latitud[i] <- tray_order1$latitude[1]
    #trayectorias_informacion$Long_Corresp1[i] <- tray_order1$longitude[1]
    ## la mayor es la ultima, pues esta ordenado de menor a mayor  
    #trayectorias_informacion$May_Latitud[i] <- tray_order1$latitude[cant_puntos]
    #trayectorias_informacion$Long_Corresp2[i] <- tray_order1$longitude[cant_puntos]
    
    ##distancia alto
    #trayectorias_informacion$Dist_alto[i] <- dist(rbind(tray_order1[1,c('longitude','latitude')], tray_order1[cant_puntos,c('longitude','latitude')]), method = "euclidean")
    
    ##ordeno de menor a mayor la longitud  x
    #tray_order2 <- tray_actual[order(tray_actual$longitude,decreasing=FALSE),]
    #trayectorias_informacion$Men_Longitud[i] <- tray_order2$longitude[1]
    #trayectorias_informacion$Lat_Corresp1[i] <- tray_order2$latitude[1]
    ##ordeno de mayor a menor la longitud  x
    #trayectorias_informacion$May_Longitud[i] <- tray_order2$longitude[cant_puntos]
    #trayectorias_informacion$Lat_Corresp2[i] <- tray_order2$latitude[cant_puntos]
    
    ##distancia ancho
    #trayectorias_informacion$Dist_ancho[i] <- dist(rbind(tray_order2[1,c('longitude','latitude')], tray_order2[cant_puntos,c('longitude','latitude')]), method = "euclidean")
    ##almaceno la cant de puntos de la tray
    trayectorias_informacion$Cant_puntos_originales[i] <- cant_puntos
    
    #trayectorias_informacion$Velocidad_Prom[i] <- mean(tray_actual$unixtime)
    
    #trayectorias_informacion$tiempo_recorrido[i] <- as.numeric(time_dist(tray_actual[1,],tray_actual[cant_puntos,]), units = "secs") 
  }
  return (trayectorias_informacion)
}

calcular_metricas <- function(){
  datos <-data.frame ( Grupos = c(1:(length(unique(trayectorias_informacion$Centroide))-1)), Cantidad_Trayectorias = 0, silhouette = 0) 
  for(i in 1:(length(unique(trayectorias_informacion$Centroide))-1)){
    grupo_informacion <- subset(trayectorias_informacion , Centroide  == i )
    datos[i,'Cantidad_Trayectorias'] <- nrow(grupo_informacion)
    datos[i,'silhouette'] <- mean(grupo_informacion$silhouette)
  }
  print("Datos de los grupos formados:")
  print(datos)
  return(datos)
}

calcular_angulo <-  function (coord1 , coord2){
  ## angulo en grados
  anguloGrados <- (atan2(coord2[1,2] - coord1[1,2] , coord2[1,1] -coord1[1,1]) * 180) / pi
  return (anguloGrados)
}


nileBuild <- function(par) {
  dlmModPoly(order = 1, dV = exp(par[1]), dW = exp(par[2]))
}

apply_kalman <- function(frame_tray){
  tryCatch({
    if(nrow(frame_tray) <= 5){
      new_frame_kalman <- frame_tray
    }else{
      matrix1 <- frame_tray
      
      nileMLE1 <- dlmMLE(matrix1[,1],parm = c(1, 1),nileBuild)
      nileMLE2 <- dlmMLE(matrix1[,2],parm = c(1, 1),nileBuild)
      
      nileMod1 <- nileBuild(nileMLE1$par)
      nileMod2 <- nileBuild(nileMLE2$par)
      
      nileFilt1 <- dlmFilter(matrix1[,1], nileMod1)
      nileFilt2 <- dlmFilter(matrix1[,2], nileMod2)
      
      nileSmooth1 <- dlmSmooth(nileFilt1)
      nileSmooth2 <- dlmSmooth(nileFilt2)
      
      tamano_kalman <- length(nileSmooth1$s)
      new_frame_kalman <- data.frame(longitude = nileSmooth1$s[2:tamano_kalman], 
                                     latitude = nileSmooth2$s[2:(tamano_kalman)], 
                                     unixtime = frame_tray[,3], 
                                     file_name = frame_tray[,4],
                                     geog = frame_tray[,5])
    }
    return(new_frame_kalman)
  }, error = function(e) {
    return(frame_tray)
  })
}

gr<-function(points, dist_threshold){
  points = apply_kalman(points)

  if (nrow(points)<=2) {
    return(points)
  }else {
    max_dist_threshold = 0
    found_index = 0
    delta_e = as.numeric(time_dist(points[nrow(points),],points[1,]), units = "secs") * I_3600 #segundos
    d_lat = points[nrow(points),][,'latitude'] - points[1,][,'latitude']
    d_lon = points[nrow(points),][,'longitude'] - points[1,][,'longitude']
    
    for (i in 2:(nrow(points)-1)) {
      delta_i = as.numeric(time_dist(points[i,], points[1,]), units = "secs")  * I_3600
      di_de   = delta_i / delta_e
      
      if(points[i, "geog"]){
        if(is.nan(di_de) == FALSE){ 
          point = data.frame(longitude = points[1,]$longitude + d_lon * di_de,
                             latitude = points[1,]$latitude + d_lat * di_de,
                             unixtime=0)
          dist  = loc_dist(points[i,], point)
          
          if (dist > max_dist_threshold) {
            max_dist_threshold = dist
            found_index = i
          }
        }
      }
    }
    
  }  
  if ( max_dist_threshold > dist_threshold ) {
    segment_1 = points[1:found_index,] #apply_kalman()
    one = gr(points =  segment_1, dist_threshold = dist_threshold)
    segment_2 = points[found_index:nrow(points),] #apply_kalman()
    two = gr(points =  segment_2, dist_threshold = dist_threshold)
    two = two[2:nrow(two),]
    one = rbind(one,two)
    return(one)
  }else{
    return(rbind(points[1,],points[nrow(points),]))
  }
}


#=====IMPORTS===================================================================

packages <- c("dlm","RPostgreSQL","rlang", "ggplot2", "caret", "class", "mapview", "compare", "pracma" , "stringr","SpatialTools","matlib", "dplyr","chron","lubridate","zoom","RgoogleMaps","ggmap","xlsx") #librerias 
ipak(packages)
#ipak2()


#===============================================================================
# Parametros de configuracion
#===============================================================================

I_3600 = 1 / 3600.0
Epsilon <- 0.00075

#' @param dataset_seleccionado
#' 1. California 
#' 2. T-drive
#' 3. Geolife
#' @param algoritmo 
#' 1. GR
#' @param kalman 
#' TRUE  Aplica kalman
#' FALSE No aplica Kalman

dataset_seleccionado <- 3
algoritmo <- 1
redesv <- TRUE

#======VARIABLES Y ENTORNO======================================================
filtro_red <- list()
result<-list()                                                              #listas de datos que contendra los resultados por iteracion del algortimo
time_proc<- list()                                                          #tiempo del proceso por cada iteracion
dist_threshold<-list()                                                      #lista de las distancias limite / umbral para cada experimento / iteracion 
lista_trayectorias <- list () 						                                  #lista que almacena todas las trayectorias
lista_trayectorias_sinprocesar <- list ()




#=======CONSULTA================================================================

if(dataset_seleccionado == 1){
  # California
  muestra <- 340
}else if(dataset_seleccionado == 2){
  # T-Drive
  muestra <- 259
}else if(dataset_seleccionado == 3){
  # Geolife
  muestra <- 376
}
trayectoria_inicial <- 1


data_dataset <- get_dataset(dataset_seleccionado, trayectoria_inicial, muestra)


data_variabilidad <- data.frame(
  #Media
  data_media = mean(data_dataset$indices[,"cant"]),
  #Varianza
  data_varianza1 = var(data_dataset$indices[,"cant"]),
  data_varianza2 = sd(data_dataset$indices[,"cant"]) ^ 2,
  # Desviación típica
  data_desviacion1 = sd(data_dataset$indices[,"cant"]),
  data_desviacion2 = sqrt(var(data_dataset$indices[,"cant"])),
  #Coeficiente variacion
  coef_variacion = sd(data_dataset$indices[,"cant"])/mean(data_dataset$indices[,"cant"])
)


# get_Workspace(algoritmo = algoritmo, dataset = data_dataset$dataset , kalman = akalman)
# folder_result <- getwd() 
# save.image(file="RData-Dataset.RData")

#=====REDES====================================================================
if(redesv){
  #data_dataset$data <- data_dataset$data[!duplicated(data_dataset$data[c("unixtime","the_geom")]),]
  data_dataset$data <- data_dataset$data[!duplicated(data_dataset$data[c("latitude", "longitude", "unixtime", "file_name")]),]
  data_dataset$size <- nrow(data_dataset$data)
  data_dataset$data[!is.na(data_dataset$data[,"geog"]),"geog"] <- TRUE
  data_dataset$data[is.na(data_dataset$data[,"geog"]),"geog"] <- FALSE
}
save.image(file="RData-Dataset-R.RData")
#===============================================================================

poblacion_tr <- length(data_dataset$trayectorias)
tamano_muestra <- poblacion_tr#get_muestra(poblacion=poblacion_tr, confianza=0.95, error=0.05, sigma=0.5)
print(paste("Muestra:",tamano_muestra))
tr_seleccionadas <- data_dataset$trayectorias[1:tamano_muestra]#sample(x=data_dataset$trayectorias, size=tamano_muestra, replace = FALSE, prob = NULL)
tr_seleccionadas <- tr_seleccionadas[order(tr_seleccionadas)]
#save.image(file="RData-Muestra.RData")

#===============================================================================

info_tr <- data_dataset$data
for(count in 1:tamano_muestra){
  tray_corresp <- subset(info_tr, info_tr$file_name == tr_seleccionadas[count])		##divido por trayectorias
  lista_trayectorias_sinprocesar[[count]] <-  tray_corresp
}
rm(info_tr)

lista_trayectorias <- lista_trayectorias_sinprocesar
cant_trayectorias <- length(lista_trayectorias)
print(paste('Cantidad de trayectorias : ', cant_trayectorias, sep=""))

trayectorias_informacion <- preprocesar_informacion(lista_trayectorias = lista_trayectorias)



#=====KALMAN====================================================================

tiempos_kalman <- data.frame(id=1:length(lista_trayectorias),tiempos=0)
df_kalman_gen <- data.frame()



print("Cuantas veces quieres ejecutar el algortimo?")
cant_ejecuciones <- length(lista_trayectorias)

# 
# print("Ingresa el Epsilon para cada experimento\n")
# for (i in 1:as.numeric(cant_ejecuciones) ) {
#   dist_threshold[i]<-Epsilon
# }

print("Ingresa el Epsilon para cada experimento\n")
for (i in 1:as.numeric(cant_ejecuciones) ) {
  trayectoria <- lista_trayectorias[[i]]
  sdev <- 0
  dist_tray <- c()
  if(nrow(trayectoria) > 1){
    for (j in 1:(nrow(trayectoria)-1)) {
      dist_euclidean <- distance_f(
        p_a = trayectoria[j, c('longitude', 'latitude')],
        p_b = trayectoria[j + 1, c('longitude', 'latitude')]
      )
      dist_tray <- c(dist_tray, dist_euclidean)
    }
  }else{
    dist_tray <- c(dist_tray, 0)
  }
  sdev <- mean(dist_tray) 
  #sdev <- sd(dist_tray) 
  #sdev <- mean(dist_tray) + sd(dist_tray) 
  if(is.na(sd)){
    sdev <- 0
  }else if(sdev < 0.00005){
    sdev <- 0.00005
  }
  #print(sdev)
  dist_threshold[i] <- sdev
}
epsilon_mean <- mean(do.call(c, dist_threshold))
epsilon_desv <- sd(do.call(c, dist_threshold))
print(epsilon_mean)
print(epsilon_desv)




#========Simplificacion=========================================================

#Aplicacion del Algoritmo  de simplificacion

for (j in 1:as.numeric(cant_ejecuciones)) {
  input_points <- lista_trayectorias[[j]]
  print(paste(get_algoritmo(algoritmo)," Trayectoria ",j ," de ", cant_trayectorias, " : ", nrow(input_points),sep = ""))

  ini_iter<- as.POSIXct(Sys.time()) 
  result[[j]] <-  gr(points = input_points, dist_threshold = dist_threshold[[j]])
  end_iter <- as.POSIXct(Sys.time())
  time_proc[[j]] <- c(ini_iter,end_iter)

}
#name <- paste("EX ",inicio_d,"-",fin_d,".RData",sep="")
#save.image(file=name)
save.image(file="RData-Compresion.RData")


#Remover duplicados
for (k in 1:as.numeric(cant_ejecuciones)) {
  result[[k]]<- result[[k]][!duplicated(result[[k]]),]
}


#Actualizacion de la trayectoria informacion
for (p in  1:as.numeric(cant_ejecuciones)) {
  #trayectorias_informacion$Puntos_filtro_redes_viales[p] <- nrow(filtro_red[[p]])
  aux <- nrow(result[[p]])
  trayectorias_informacion$Cant_puntos_simplificados[p] <- aux
}





#=====GUARDADO DE TR RESULTANTES================================================

#save.image(file="RData-Simpli.RData")

write.table(trayectorias_informacion, file ="trayectoria informacion.csv" , sep = ";", row.names = FALSE, col.names = TRUE)
write.table(do.call(rbind,lista_trayectorias)[,c("longitude","latitude","unixtime","file_name")], file ="lista_trayectorias.csv" , sep = ";", row.names = FALSE, col.names = TRUE)
write.table(do.call(rbind,result)[,c("longitude","latitude","unixtime","file_name")], file ="lista_trayectorias_simplificada.csv" , sep = ";", row.names = FALSE, col.names = TRUE)
#write.table(do.call(rbind,filtro_red)[,c("longitude","latitude","unixtime","file_name")], file ="lista_filtrada_red_vial.csv" , sep = ";", row.names = FALSE, col.names = TRUE)




#===============================================================================
# Métricas complementarias
#===============================================================================



#=====GRAFICOS==================================================================
setwd(folder_result)

dir.create(paste(get_algoritmo(algoritmo)," Graficos tras Simplificacion",sep=''))
setwd(paste(folder_result,'/',get_algoritmo(algoritmo)," Graficos tras Simplificacion",sep=''))
#Graficas cada trauectoria (original y simplificada)

for (count in trayectorias_informacion$No_Trayectoria) {
  original<- lista_trayectorias[[count]]
  simplificada<-result[[count]]
  png(paste("Trayectoria-",count,".png",sep = ""), width = 1024, height = 720, units = 'px', pointsize = 20 ,res = NA)
  plot(original[,1:2], type="b", cex=1.2,col=1, main=paste("Trayectoria_",count," - ",lista_trayectorias[[count]][1,"file_name"], sep = ""))
  points(simplificada[,1:2], type="b", cex=1.2,col=2)
  legend("bottomright",legend=c("Original","Simplificada"),col=c(1,2), pch=1,bty="n",ncol=1,cex=1,pt.cex=1)
  dev.off()
}
setwd(folder_result)

save.image(file="RData-Graficos.RData")

#tabla de tabulacion
list_size<- list()
for (a in 1:cant_ejecuciones) {
  list_size[[a]]<-nrow(result[[a]]) 
}

#=====RAZON DE COMPRESION=======================================================

##Razon de compresion 
values_z<-data.frame(valor_z=c(1.28,1.44,1.65,1.96,2.58),valor_confianza=c("80%","85%","90%","95%","99%"))

print("Indique el valor de confianza:")
print(values_z)
opcion<-1.96

rc<- list()
for (count in 1:as.numeric(cant_ejecuciones)) {
  rc[[count]] <- (1 - trayectorias_informacion$Cant_puntos_simplificados[count]/ trayectorias_informacion$Cant_puntos_originales[count]) *100
  #rc[[count]]<-(1 - (list_size[[count]]/nrow(todas)))*100
}

#=====ME===========================================================

setwd(folder_result)

margen_e<- list()
type_m_e <- get_type(algoritmo)
for (count in 1:as.numeric(cant_ejecuciones)){
  margen_e[[count]] <- 0
}

#=============== MARGEN DE ERROR =======================
setwd(folder_result)
cant_ejecuciones <- cant_trayectorias

margen_SED <- list()
margen_ED <- list()

print("Error SED")
for (count in 1:as.numeric(cant_ejecuciones)){
  #print(count)
  dist_or <- lista_trayectorias[[count]]
  dist_si <- result[[count]]
  if(algoritmo == 3) colnames(dist_si) <- c("longitude","latitude", "file_name", "unixtime")
  or <- c()
  si <- c()
  if (nrow(dist_or) <= 2){
    #return(dist_or)
    P_dist_euclidean_or <- 0
    A <- 0
  }else{
    delta_e = as.numeric(time_dist(dist_or[nrow(dist_or),], dist_or[1,]), units = "secs") * I_3600
    d_lat = dist_or[nrow(dist_or),][,'latitude'] - dist_or[1,][,'latitude']
    d_lon = dist_or[nrow(dist_or),][,'longitude'] - dist_or[1,][,'longitude']
    dist_sed <- 0
    for (i in 2:(nrow(dist_or))) {
      delta_i = as.numeric(time_dist(dist_or[i,], dist_or[1,]), units = "secs")  * I_3600
      di_de = delta_i / delta_e
      prim = data.frame(longitude = dist_or[1,]$longitude + d_lon * di_de,
                        latitude = dist_or[1,]$latitude + d_lat * di_de,
                        unixtime=0)
      dist_sed <- dist_sed + loc_dist(dist_or[i,], prim)
      or <- c(or, loc_dist(dist_or[i,], prim))
    }
    A <- dist_sed
    P_dist_euclidean_or <- dist_sed / nrow(dist_or)
  }
  
  if (nrow(dist_si) <= 2) {
    #return(dist_si)
    P_dist_euclidean_sim <- 0
    B <- 0
  }else{
    delta_e = as.numeric(time_dist(dist_si[nrow(dist_si),], dist_si[1,]), units = "secs") * I_3600
    d_lat = dist_si[nrow(dist_si),][,'latitude'] - dist_si[1,][,'latitude']
    d_lon = dist_si[nrow(dist_si),][,'longitude'] - dist_si[1,][,'longitude']
    dist_sed <- 0
    
    for (i in 2:(nrow(dist_si))) {
      delta_i= as.numeric(time_dist(dist_si[i,],dist_si[1,]), units = "secs")  * I_3600
      di_de = delta_i / delta_e
      prim = data.frame(longitude = dist_si[1,]$longitude + d_lon * di_de,
                        latitude = dist_si[1,]$latitude + d_lat * di_de,
                        unixtime = 0)
      dist_sed <- dist_sed + loc_dist(dist_si[i,], prim)
      si <- c(si, loc_dist(dist_si[i,], prim))
    }
    B <- dist_sed
    P_dist_euclidean_sim <- dist_sed/nrow(dist_si)
  }
  res <- abs(B - A)
  if(is.nan(res)){
    res <- 0
  }
  margen_SED[[count]] <- res
}

print("Error ED")
for (count in 1:as.numeric(cant_ejecuciones)){
  #print(count)
  trayectoria <- lista_trayectorias[[count]]
  trsim <- result[[count]]
  dat_GPS1 <- data.frame(longitude = trayectoria$longitude, latitude = trayectoria$latitude)
  simplificada <- data.frame(longitude = trsim$longitud, latitude = trsim$latitud)
  
  if (nrow(dat_GPS1) < 2){
    P_dist_euclidean_or <- 0
    A <- 0
  }else{
    dist_or <- dat_GPS1
    dist_euclidean <- 0
    for (i in 1:(nrow(dist_or)-1)) {
      dist_euclidean <- dist_euclidean + distance_f(p_a = dist_or[i,c('longitude','latitude')], p_b = dist_or[i+1,c('longitude','latitude')])
    }
    A <- dist_euclidean
    P_dist_euclidean_or <- dist_euclidean / nrow(dist_or)
  }
  
  if (nrow(simplificada) < 2) {
    P_dist_euclidean_sim <- 0
    B <- 0
  }else{
    dist_si <- simplificada
    dist_euclidean <- 0
    for (i in 1:(nrow(dist_si)-1)) {
      dist_euclidean <- dist_euclidean + distance_f(p_a = dist_si[i,c('longitude','latitude')], p_b = dist_si[i+1,c('longitude','latitude')])
    }
    B <- dist_euclidean
    P_dist_euclidean_sim  <- dist_euclidean / nrow(dist_si)
  }
  
  margen_ED[[count]] <- abs(B - A)#abs(P_dist_euclidean_sim - P_dist_euclidean_or)
}


Error <- data.frame(Dataset = data_dataset$dataset, 
                    Algoritmo = get_algoritmo(algoritmo),
                    SED = mean(do.call(rbind, margen_SED)),
                    ED = mean(do.call(rbind, margen_ED)))

write.table(t(Error), file = "MArgen de Error.txt", sep = ";", row.names = TRUE, col.names = FALSE)


save.image(file="RData-ME.RData")

#=====DETALLES==================================================================

detalles <- data.frame(n_experimento= 1:as.numeric(cant_ejecuciones), 
                      trayectoria_inicial= trayectorias_informacion$Cant_puntos_originales,
                      filtro_redes = trayectorias_informacion$Puntos_filtro_redes_viales,
                      epsilon= as.vector(unlist(dist_threshold)), 
                      trayectoria_final= trayectorias_informacion$Cant_puntos_simplificados,
                      tiempo_proceso_Segundos= do.call(rbind,time_proc)[,2]-do.call(rbind,time_proc)[,1],
                      margen_Error=do.call(rbind,margen_e),
                      #razon_compresion=paste(do.call(rbind,rc),sep = ""))
                      razon_compresion=do.call(rbind,rc) )


write.table(detalles, file = "Resultados de los experimentos.csv", sep = ";", row.names = FALSE, col.names = TRUE)


resumen <- data.frame(puntos_originales=data_dataset$originalsize,
                      puntos_no_repetidos=data_dataset$size,
                      puntos_antes_simplificacion = sum(detalles$trayectoria_inicial), 
                      puntos_filtro_redes = sum(detalles$filtro_redes),
                      puntos_despues_simplificacion=sum(detalles$trayectoria_final),
                      trayectorias_antes_simp=length(data_dataset$trayectorias),
                      trayectorias_despues_simp=length(lista_trayectorias),
                      tiempo_kalman=sum(tiempos_kalman[,"tiempos"]),
                      tiempo_algoritmo=sum(detalles$tiempo_proceso_Segundos),
                      razon_compresion_media=0,
                      razon_compresion_conjunto=0,
                      margen_error=0)
resumen$razon_compresion_conjunto <- round((1-(resumen$puntos_despues_simplificacion/resumen$puntos_antes_simplificacion))*100,3)
resumen$margen_error <- mean(do.call(rbind,margen_e))
temp <- do.call(rbind,rc)
resumen$razon_compresion_media <-  mean(temp[temp[,1]!=0],na.rm=TRUE)


#medias <- summary(detalles)
medias <- c(sapply(detalles,mean))
sumatorias <- sapply(detalles,sum)

write.table(t(resumen), file = "Resumen Metrica total puntos.txt", sep = ";", row.names = TRUE, col.names = FALSE)
write.table(medias, file = "Metricas Medias.txt", sep = ";", row.names = TRUE, col.names = TRUE)
write.table(sumatorias, file = "Metricas Sumatorias.txt", sep = ";", row.names = TRUE, col.names = TRUE)
#epsilon, 
#summary(detalles)
#info <- sapply(detalles,mean)
#mean(detalles$razon_compresion)
#mean(detalles[,"razon_compresion"])
#detalles$razon_compresion


setwd(folder_result)
save.image(file="RData.RData")

files_list <- c()
for(i in 1:length(result)){
  files_list <- c(files_list, result[[i]][1,"file_name"])
}




detalles_2 <- data.frame(n_experimento= 1:as.numeric(cant_ejecuciones),
                         file_name = files_list,
                       trayectoria_inicial= trayectorias_informacion$Cant_puntos_originales,
                       filtro_redes = trayectorias_informacion$Puntos_filtro_redes_viales,
                       epsilon= as.vector(unlist(dist_threshold)), 
                       trayectoria_final= trayectorias_informacion$Cant_puntos_simplificados,
                       tiempo_proceso_Segundos= do.call(rbind,time_proc)[,2]-do.call(rbind,time_proc)[,1],
                       margen_Error_SED=do.call(rbind,margen_SED),
                       margen_Error_ED=do.call(rbind,margen_ED),
                       razon_compresion=do.call(rbind,rc) )


write.table(detalles_2, file = "Resultados de los experimentos_2.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.xlsx2(detalles_2,file = "Resultados de los experimentos_3.xlsx", sheetName = "Resultados",col.names = TRUE,append = TRUE)





# SED-ASED por trayectoria

setwd(folder_result)
print("Error SED-ASED")
error_ASED <- data.frame()
for (count in 1:as.numeric(cant_ejecuciones)){
  print(count)
  tray_or <- lista_trayectorias[[count]]
  tray_si <- result[[count]]
  
  tray_SED <- c()
  
  #if(nrow(tray_si)>1){
    
  #}
  init_or <- 1
  end_or <- 1
  for(j in 1:(nrow(tray_si) - 1)){
    if(tray_or[init_or, "unixtime"] == tray_si[j, "unixtime"]){
      while(tray_or[end_or, "unixtime"] != tray_si[j + 1, "unixtime"]){
        end_or <- end_or + 1
      }
      #print(paste("Calcular SED: OR", init_or, end_or, " OR:", j , (j+1)))
      dist_or <- tray_or[init_or:end_or, ]
      #dist_si <- tray_si
      segm_SED <- c()
      
      if (nrow(dist_or) > 2){
        delta_e = as.numeric(time_dist(dist_or[nrow(dist_or),], dist_or[1,]), units = "secs") * I_3600
        d_lat = dist_or[nrow(dist_or),][,'latitude'] - dist_or[1,][,'latitude']
        d_lon = dist_or[nrow(dist_or),][,'longitude'] - dist_or[1,][,'longitude']
        
        for (i in 2:(nrow(dist_or)-1)) {
          delta_i = as.numeric(time_dist(dist_or[i,], dist_or[1,]), units = "secs")  * I_3600
          di_de = delta_i / delta_e
          prim = data.frame(
            longitude = dist_or[1,]$longitude + d_lon * di_de,
            latitude = dist_or[1,]$latitude + d_lat * di_de
          )
          segm_SED <- c(segm_SED, loc_dist(dist_or[i,], prim))
        }
      }
      
      tray_SED <- c(tray_SED, segm_SED)
      init_or <- end_or
    }
  }
  df_sed_tr <- data.frame(
    "trayectoria" = count,
    "sed_total" = sum(tray_SED),
    "mean_sed" = mean(tray_SED),
    "segmentos" = nrow(tray_si) - 1,
    "ased" = sum(tray_SED) / (nrow(tray_si) - 1)
  )
  error_ASED <- rbind(error_ASED, df_sed_tr)
}

general_sed_ased <- data.frame(
  "mean_sed_total" = mean(error_ASED$sed_total),
  "mean_sed_mean" = mean(error_ASED$mean_sed),
  "mean_ased" = mean(error_ASED$ased)
)


write.table(error_ASED,file = "error_ased.csv",sep = ";",row.names = FALSE)
# write.xlsx2(error_ASED, file = "error_ased.xlsx", )
write.table(t(general_sed_ased),file = "general_sed_ased.txt",sep = ";",row.names = TRUE,col.names = FALSE)


