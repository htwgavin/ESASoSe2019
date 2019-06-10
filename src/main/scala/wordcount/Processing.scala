package wordcount

class Processing {
   
  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *   
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    if (line == "") List()
    else line.toLowerCase.replaceAll("[^a-z]+"," ").split(" ").toList

  } 
  
  def getAllWords(l:List[(Int,String)]):List[String]={
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    l.flatMap( x => getWords(x._2))
  }
  
  def countWords(l:List[String]):List[(String,Int)]={
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    l.map( x => (x, l.count( y => x == y))).distinct
  }

  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *
   *********************************************************************************************
  */

  def mapReduce[S,B,R](mapFun:(S=>B),
      redFun:(R,B)=>R,
      base:R,
      l:List[S]):R =

  l.map(mapFun).foldLeft(base)(redFun)

  def countWordsMR(l: List[String]): List[(String, Int)] = {
    mapReduce[String,(String, Int),List[(String, Int)]](X=>(X,1),insertL,List(),l)
  }

  def insertL(l:List[(String, Int)], el:(String,Int)):List[(String, Int)] = l match {
    case Nil => List(el)
    case x::xs if (el._1.equals(x._1)) => (el._1, el._2 + x._2)::xs case x::xs => x::insertL(xs,el)
  }
  
  
  /**********************************************************************************************
   *
   *                          Aufgabe 3
   *   
   *********************************************************************************************
  */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l.flatMap( x => getWords(x._2).map( y => (x._1, y)))
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    l.groupBy(_._2).mapValues(xs => xs.map(v => v._1))
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    words.flatMap( x => invInd.getOrElse(x, List()))
  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    words.flatMap( x => invInd.getOrElse(x, List())).groupBy(identity).filter( y => y._2.size == words.size).toList.sortBy(_._1).map(_._1)
  }
}


object Processing{
  
  def getData(filename:String):List[(Int,String)]={
    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}