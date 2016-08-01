import scala.collection.immutable.List


object Test {
 
  
  def main(args:Array[String])={
    println("hello scala")
     val records=List(1,2,3)
  val a=records.map { x => x+1 }.groupBy { 
    case x =>(x%2)}
  println(a)
  }
  
  
}