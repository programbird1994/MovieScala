import Model.Movie
import scala.collection.mutable.ListBuffer
import Model.RatingUserMovie
import Model.Ratings
import scala.collection.mutable.Buffer

object MovieParser extends App {
  def getWholetable(moviefilePtah: String, ratingFilePath: String, userFilePath: String): ListBuffer[RatingUserMovie] = {

    // scala.io.Source.fromFile(ratingFilePath).getLines().buffered.foreach { x => println(x) }
    val list = ListBuffer[List[String]]()
    val list2 = ListBuffer[List[String]]()
    val list3 = ListBuffer[List[String]]()
    val list4 = ListBuffer[List[String]]()
    var size1 = 0
    var size2 = 0
    var size3 = 0
    for (x <- scala.io.Source.fromFile(ratingFilePath).getLines().toList) {
      //size1+=1
      list2 += x.split("::").toList
    }

    for (x <- scala.io.Source.fromFile(moviefilePtah).getLines().toList) {
      // size2+=1
      list3 += x.split("::").toList
    }

    for (x <- scala.io.Source.fromFile(userFilePath).getLines().toList) {
      //size3+=1
      list4 += x.split("::").toList
    }

    return list2.map { x => x ++ list3.find { y => y(0).equals(x(1)) }.getOrElse(List("", "", "")) }.map { x => x ++ list4.find { y => y(0).equals(x(0)) }.getOrElse(List("", "", "", "", "")) }.map { x => { RatingUserMovie(x(1), x(0), x(6), x(5), x(9), x(8), x(2).toInt) } }

    //    val src = scala.io.Source.fromFile(ratingFilePath).getLines().buffered.map {  x=>x.split("::").map { x => x.trim() }.toList }.map { y =>
    //      y ++
    //        scala.io.Source.fromFile(moviefilePtah).getLines().buffered.map { x => x.split("::").map { x => x.trim() }.toBuffer }.find { x => x(0).equals(y(1)) }.getOrElse(Buffer("","",""))
    //    }.map { y =>
    //      y ++
    //        scala.io.Source.fromFile(userFilePath).getLines().buffered.map { x => x.split("::").map { x => x.trim() }.toBuffer }.find { x => x(0).equals(y(0)) }.getOrElse(Buffer("","","","",""))
    //    }.map { x => RatingUserMovie(x(1), x(0), x(11), x(10), x(6), x(5), x(2).toInt) }.toList
    //    return src
  }

  val list = List("under 18", "18-24", "25-34", "35-39", "40-44", "45-49", "50-55", "56+")
  def f(a: String, list: List[String]): String = {
    a match {
      case "1"  => list(0)
      case "18" => list(1)
      case "25" => list(2)
      case "35" => list(3)
      case "40" => list(4)
      case "45" => list(5)
      case "50" => list(6)
      case "56" => list(7)
      case _    => "unknown"
    }
  }
  def getMostPopularFileTypeGroupByAge(movie: List[String], user: List[String], rating: List[String]): Unit = {
    val userage = user.map { x => { val list = x.split("::"); (list(0), list(2)) } }.toMap
    val movietypes = movie.map { x => { val list = x.split("::"); (list(0), list(2).split("\\|")) } }.toMap
    val table = rating.map { x => { val list = x.split("::"); (userage.get(list(0)), movietypes.get(list(1)), list(2)) } }
    table.groupBy(x => x._1).mapValues(x => x.map(x => (x._2.getOrElse(Array("")), x._3.toInt)).toList).mapValues(x=>f((x))).foreach(println(_))
  }
  
  def f(list:List[(Array[String],Int)]):(String,Double)={
 val result= list.flatMap(x=>{for(i<-0 until x._1.length)yield(x._1(i),x._2)}).groupBy(x=>x._1).mapValues(x=>x.map(x=>x._2).reduce(_+_)/x.count(x=>true).toDouble).maxBy(_._2)
  return result
  }

  def getMostpopularMovieGroupByGender(src: ListBuffer[RatingUserMovie]): Unit = {
    src.groupBy { x => x.userGender }.map(x =>
      (x._1, x._2.groupBy { x => x.movieTitle }.map(x =>
        (x._1, (x._2.map { x => x.rating }.reduce(_ + _) / x._2.count { x => true }).toDouble)).maxBy(_._2))).map(x => (x._1, x._2._1)).foreach(println(_))
  }

  def getMostUnPopularType(src: ListBuffer[RatingUserMovie]): Unit = {
    val a = src.groupBy { x => x.movieType }.map(x => (x._1, (x._2.map { x => x.rating }.reduce(_ + _) / x._2.count { x => true }).toDouble)).minBy(_._2)._1
    println(a)
  }

  val movieFile = "C:\\Users\\MSI\\Desktop\\movies.txt"
  val ratingFile = "C:\\Users\\MSI\\Desktop\\ratings.txt"
  val userFile = "C:\\Users\\MSI\\Desktop\\users.txt"
  val movie = scala.io.Source.fromFile(movieFile).getLines().toList
  val rating = scala.io.Source.fromFile(ratingFile).getLines().toList
  val user = scala.io.Source.fromFile(userFile).getLines().toList
  //getWholetable(movieFile,ratingFile,userFile).foreach { println(_) }
  // val src=getWholetable(movieFile, ratingFile, userFile)
  getMostPopularFileTypeGroupByAge(movie,user,rating)
//  println("=======")
//  getMostpopularMovieGroupByGender(src)
//  println("=======")
//  getMostUnPopularType(src)
}
