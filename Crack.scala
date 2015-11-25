import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter
import java.io.File
case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  
  def apply(line: String) : Entry = {
    val fields = line.split(":")
    new Entry(fields(0),
            fields(1),
            fields(2).toInt,
            fields(3).toInt, 
            fields(4), 
            fields(5), 
            fields(6))
  }
}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    if(w == null){
      Iterator()
    } else{
      Iterator(w,w.reverse)
    }
  }

  def transformCapitalize(w: String) : Iterator[String] = {
    if (w == "" || w == null) { Iterator("") } else {
      var char1 = w.charAt(0)
      if(char1.isLetter){    
        for (c <- Iterator(char1.toUpper, char1.toLower);
             s <- transformCapitalize(w.substring(1)))
        yield(c + s)
      } else {
        for (s <- transformCapitalize(w.substring(1)))
        yield(char1 + s)
      }
    }    
  }

  // def transformCapitalize(w: String) : Iterator[String] = {
  //   if (w == "") { Iterator("") } else {
  //     var char1 = w.charAt(0)    
  //     for (c <- Iterator(char1.toUpper, char1.toLower)){
  //       for(s <- transformCapitalize(w.substring(1)))
  //     }
  //     yield(c + s)

  //   }    
  // }

  def checkMapping(c: Char) = c match {
    case 'o' => '0' 
    case 'z' | 'Z' =>'2' 
    case 'a' | 'A'  => '4' 
    case 'g' | 'q' | 'G' | 'Q' => '9'
    case 'i' | 'l' | 'I' | 'L' => '1' 
    case 'e' | 'E' =>'3' 
    case 's' | 'S' =>'5'
    case 't' | 'T' =>'7'
    case _         => null
    
  }
  
  def transformDigits(w:String) : Iterator[String] = {
    if (w == "") { Iterator("") } else {
      var char1 = w.charAt(0)
      if(char1 == 'b' || char1 == 'B'){
        for (c <- Iterator(char1, '6', '8');
           s <- transformDigits(w.substring(1)))
        yield(c + s)
      } else {
        var check = checkMapping(char1)
        if(check != null){   
          for (c <- Iterator(char1, check);
             s <- transformDigits(w.substring(1)))
          yield(c + s)
        } else{
          for (c <- Iterator(char1);
               s <- transformDigits(w.substring(1)))
          yield(c+s)  
        }
      } 
    }  
  }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  def break(){
    throw new IllegalStateException("Found PW Match");
  }

  def remove(account: Entry, list: List[Entry]) = list diff List(account)

  // scala> //
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    // Create entry objects for each line in the pwdfile
    //val t0 = System.nanoTime() // start time
    var accounts = (for(account <- Words.apply2(pwdFile))yield(Entry.apply(account))).toList

    var cracked = List[String]()
    val pw = new PrintWriter(new File(outFile))
    var words = (for(word <- candidateWords(wordsFile); mWords <- transformReverse(word))yield(mWords)).toList
    // for(word <- words; mutWord <- transformCapitalize(word))pw.write(mutWord + '\n')
    // pw.close()
    println("************ Finished Creating words *************")
    for(account <- accounts){ 
      var bool = false
      try{
        
        for(word <- words){
          if(checkPassword(word, account.password)){
            //println(account.account + ":" + word)
            pw.write((account.account + "=" + word) + '\n')
            bool = true
            accounts = accounts.filter(_.uid != account.uid).toList
            break
          }
        }
      } catch {
        case e: IllegalStateException    => println("Found PW")
      }
      if(!bool){ 
        //unCrackedAcct = account :: unCrackedAcct

      }
    }
    //for(account <- accounts)println(account)
    //var cWords = (for(word <- words; mutWord <- transformCapitalize(word))yield(mutWord)).toList
    for(account <- accounts){
      var bool = false
      try {
        for(word <- words){
          for(mutWord <- transformDigits(word)){ 
             if (checkPassword(mutWord, account.password)){
              //println(account.account + ":" + word)
              pw.write((account.account + "=" + mutWord) + '\n')
              bool = true
              break
             }
            }
          
          if(!bool){
            for(mutWord <- transformCapitalize(word)){ 
             if (checkPassword(mutWord, account.password)){
              //println(account.account + ":" + word)
              pw.write((account.account + "=" + mutWord) + '\n')
              bool = true
              break
             }
            }
          }

        }
      }catch {
        case e: IllegalStateException    => println("Found PW")
      }
      if(!bool){
      println("******* COULD NOT FIND " + account + " *******") 
        //unCrackedAcct2 = account :: unCrackedAcct2
      }
    }
    pw.close()
    //val t1 = System.nanoTime() // end time
    //println("Time elapsed: " + (t1 - t0))
  }
  
  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

