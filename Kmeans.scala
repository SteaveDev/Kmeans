package TP
import scala.util.Random
import scala.collection.mutable.ListBuffer;

object KMeans extends App {
  var n_limite = 5000
  var k = 10
  var dimension = 2
  var random = scala.util.Random
  
  println("\n===============CREATION DES POINTS===================")
  var mesPoints: ListBuffer[ListBuffer[Double]] = ListBuffer()
  for(i<-0 to n_limite-1){
    var mesPoints_temp: ListBuffer[Double] = ListBuffer()
    for(j<-1 to dimension) mesPoints_temp.append(random.nextInt(n_limite) + random.nextDouble())
    mesPoints.append(mesPoints_temp)
  }

  println("\n==============TABLEAU DE CLUSTER====================")
  var mesClusters: ListBuffer[ListBuffer[Double]] = ListBuffer()
  for(i<-1 to k) mesClusters.append(mesPoints(random.nextInt(n_limite-1)))
  for(i<-0 to mesClusters.size-1) println("Clusters n°" + i + " avec la valeur : " + mesClusters(i))
    
  println("\n==============TABLEAU DANS CHAQUE CLUSTER====================")
  
  var test_continuerCalculCluster = -1
  var nombre = 1
  while(test_continuerCalculCluster!=0){
    println("Boucle n°" + nombre)
    test_continuerCalculCluster = 0
    
    var tab_calc_temp : ListBuffer[ListBuffer[ListBuffer[Double]]] = ListBuffer()
    for(i<-0 to k-1) tab_calc_temp.append(ListBuffer());
  
    println("\n=============ECART ENTRE POINT ET CLUSTER POUR INSERER DANS TAB=====================")
    for(i<-0 to mesPoints.length-1){
      var indice_cluster = 0
      var ecart_cluster = 3141592d
      for(j<-0 to mesClusters.length-1){
        var racine = math.sqrt({var somme_tab = 0d; for(nEff<-0 to dimension-1) somme_tab+=math.pow(mesPoints(i)(nEff)-mesClusters(j)(nEff),2); somme_tab})
        if(ecart_cluster>racine){
          ecart_cluster = racine
          indice_cluster = j
        }
      }
      tab_calc_temp(indice_cluster).append(mesPoints(i))
    }
    
    println("\n===============AFFICHAGE DES K TABLEAUX TRIES===================")
    for(i<-0 to mesClusters.size-1) println("mesClusters n°" + i + " : " + mesClusters(i))
    for(i<-0 to tab_calc_temp.size-1){
      print("Tableau n°" + i + " : ")
      for(j<-0 to tab_calc_temp(i).size-1)
        print(tab_calc_temp(i)(j) + "\t")
      println()
    }

    println("\n===============TROUVER CENTRE CLUSTER===================")
    var recentrer_mesCluster : ListBuffer[ListBuffer[Double]] = ListBuffer()
    for(i<-0 to k-1){
      var temp : ListBuffer[Double] = ListBuffer()
      for(temporaire<-0 to dimension-1)temp.append(0)
      for(j<-0 to tab_calc_temp(i).size-1)
        for(nEff<-0 to tab_calc_temp(i)(j).size-1)
          temp(nEff) += tab_calc_temp(i)(j)(nEff)
      recentrer_mesCluster.append(temp) 
    }
    
    for(i<-0 to k-1){
      println("NOMBRES D'ELEMENTS : " + tab_calc_temp(i).size + "\t" + recentrer_mesCluster(i))
      if(tab_calc_temp(i).size>0)
        for(j<-0 to recentrer_mesCluster(i).size-1){
          recentrer_mesCluster(i)(j) = recentrer_mesCluster(i)(j) / tab_calc_temp(i).size
          if(recentrer_mesCluster(i)(j)!=mesClusters(i)(j))
            test_continuerCalculCluster = test_continuerCalculCluster + 1
        }
      println("\tCLUSTER n° " + (i+1) +" AVANT : " + mesClusters(i) + "\tAPRES : " + recentrer_mesCluster(i))
      mesClusters(i) = recentrer_mesCluster(i)
    }
    
    if(test_continuerCalculCluster!=0)
      println("Le centre a ete modifie : " + test_continuerCalculCluster + " fois\n\n")
    nombre = nombre + 1 
  }
  
  
  println("\n=================================================\n|\tTERMINE : Plus de modification !\t|\n|\t\tFait en " + nombre + " boucles\t\t|\n=================================================") 
}