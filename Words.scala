import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import java.io.File
import Lines._
import scala.util.matching.Regex

object Words {

  def apply2(file: String) : Iterator[String] =  {
    val f = new File(file)
    scala.io.Source.fromFile(f).getLines()
  }

  def apply(file: String) : Iterator[String] =  {
    val f = new File(file)
    scala.io.Source.fromFile(f).getLines().map(_.toLowerCase)
  }
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
    var hashmap = new HashMap[B, Int]
    for(x <- xs){
      hashmap += (f(x) -> (hashmap.getOrElse(
        f(x),
        0)+1))
    }

    hashmap
 }
  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 


  def sizeFreq(file: String): HashMap[Int, Int] = {
    //var fmap = new HashMap[Int, Int]
    var fmap = groupFreq(apply(file), (str:String) => str.length())

    fmap
  }

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars   = apply(file).flatMap((str : String) => str.toCharArray )
    val grouper = ((ch : Char) => ch) 
    groupFreq(chars, grouper) 
  }

  def candWordsNoVowels(file: String) : Iterator[String] = {
    var re = new Regex("""^.{6,8}$""")
    apply(file).filter((str:String) =>
          (str.contains('a') &&
           str.contains('e') &&
           str.contains('i') &&
           str.contains('o') &&
           str.contains('u'))).filter((str:String) => ((re findFirstIn str).mkString("") == str))
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = {
   apply(file).filter((str:String) => (str.length == size))
  }

  def wordsWithAllVowels(file: String): Iterator[String] = {
    apply(file).filter((str:String) => str.contains('a') &&
                                       str.contains('e') &&
                                       str.contains('i') &&
                                       str.contains('o') &&
                                       str.contains('u'))
  }
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
    apply(file).filter((str:String) => !(str.contains('a') ||
                                       str.contains('e') ||
                                       str.contains('i') ||
                                       str.contains('o') ||
                                       str.contains('u')))
 
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = 
    apply(file).filter((str:String) => ((re findFirstIn str).mkString("") == str))

}

// vim: set ts=2 sw=2 et:

