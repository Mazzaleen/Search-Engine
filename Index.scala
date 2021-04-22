package search.sol
import search.src.{PorterStemmer, StopWords}

import scala.collection.mutable
import scala.math.{log, sqrt}
import scala.util.matching.Regex
import scala.xml.Node



/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Index(val inputFile: String) {
  // TODO : Implement!

  /**
   * Dataclass for the pages
   *
   * @param id   -- ID number of type int corresponding to the page number
   * @param body -- body of type string corresponding to the page text
   */
  case class Page(id: Int, body: String, text: String, title: String)

  /**
   * Method that converts node from xml node sequence into a page object
   *
   * @param n -- Node from xml node sequence of type Node
   * @return a page object
   */
  def pageFromNode(n: Node): Page = {
    val id = (n \ "id").text.trim.toInt
    val body = (n \ "body").text.trim
    val text = (n  \ "text").text.trim
    val title = (n \ "title").text.trim
    Page(id, body, text, title)
  }


  /**
   * Helper method that converts xml file into a list of Pages
   * @return list with page elements
   */
  def loadpage(): List[Page] ={
    //Mazine Path:
    //     "/Users/mazinesuliman/CS0180/" +
    //     "search-msulima2-sultandaniels/src/search/src/"

    //Sultan Path:
    // "src\\search\\src\\"

    //Convert xml file to node
    val fileNode: Node = xml.XML.loadFile(
      "src\\search\\src\\"
        + inputFile)

    //generate a list of pages for each list in Node
    val pages = (fileNode \ "page").map(pageFromNode).toList

    pages

  }

  /**
   * Formats the file: extract the text of each document from the XML (Parses)
   *
   * Creates a LinkedHashMap mapping page number to the body
   *
   * (Where the body of the text is tokenized using the toTokenizeHelper
   * method)
   *
   * @return LinkedHashMap with page numbers mapped to page texts
   */
  def toFormat():
  (mutable.LinkedHashMap[Int, List[String]],
    mutable.LinkedHashMap[Int, List[String]]) = {
    //Initialize a LinkedHashMap to map each page to the words in page
    val pageText = new mutable.LinkedHashMap[Int, List[String]]()
    val linklists = new mutable.LinkedHashMap[Int, List[String]]()

    //for every page in pageList, add it to the pageText LinkedHashMap of where
    // the key is the page number and the body is the string
    for (page <- this.loadpage()) {
      val str = toTokenizeHelper(page.body + " " + page.text)
      linklists.update(page.id, listOfLinks2(page.id, str))
      pageText.update(page.id, linkCleaner(str))
    }
    (pageText, linklists)
  }

  def arrToList(strings: Array[String]): List[String] = {
    var list = List[String]()
    for (word <- strings){
      list = list ::: List(word)
    }
    list
  }

//  @Override
//  def split(regex: Regex, phrase: String): List[String] = {
//
//    // Call findAllMatchIn to get an iterator of Matches
//    val matchesIterator = regex.findAllMatchIn(phrase)
//
//    // Convert the Iterator to a List and extract the matched substrings
//    val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }
//  }

  /**
   *  Method that that cleans the link converting it to one string
   * @param strs -- list of uncleaned links
   * @return List of cleaned links
   */
  def linkCleaner(strs: List[String]): List[String] ={
    var out = List[String]()
    val regex = new Regex(".+[|]")
    for (word <- strs) {
      val check = removeBrac(word)
      val input = check.replaceAll(".+[|]", "")
      val input2 = arrToList(input.split(" "))
      out = out ::: input2
    }
    out
  }

  /**
   * Getter method that returns linkCleaner method
   */
  def getLinkCleaner(strs: List[String]):  List[String]  = {
    linkCleaner(strs)
  }
  /**
   * Helper method that tokenizes a string into a list of strings
   *
   * @param body -- the giant string of type String
   * @return List of strings for every word in body
   */
  def toTokenizeHelper(body: String): List[String] = {
    //Initialize regex
    val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")

    // Call findAllMatchIn to get an iterator of Matches
    val matchesIterator = regex.findAllMatchIn(body)

    // Convert the Iterator to a List and extract the matched substrings
    val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }

    // initialize new list with the stop words removed
    val matchesStopRemoved =
      matchesList.filter(words => !StopWords.isStopWord(words))

    //remove stems of every word in the list
    matchesStopRemoved.map { words => PorterStemmer.stem(words).toLowerCase }
  }

  // TERM FREQUENCY
  /**
   * Method that finds the mode term in each page
   * @return  Hashmap mapping each page to the frequency of mode
   */
  private def termMostFreq(): mutable.LinkedHashMap[Int, Double] = {

    val idtoMax = new mutable.LinkedHashMap[Int, Double]
    val wiki = toFormat()._1
    //lowercase every word in the list
    for (pair <- wiki) {
      for (word <- pair._2) {
        word.toLowerCase()
      }
      //count frequency of every word in list
      idtoMax.update(pair._1,
        pair._2.groupBy(identity).maxBy(key => key._2.size)._2.size)
    }
    idtoMax
  }

  /**
   * Getter method that returns getTermMostFreq method
   *
   */
  def getTermMostFreq(): mutable.LinkedHashMap[Int, Double]  = {
    termMostFreq()
  }


  /**
   *  Helper Method that finds the  frequency of each string in a list
   * (Used to find the frequency of each word in a page idf)
   * @param wds -- list of strings
   * @return LinkedHashMap that maps each string to the  frequency in page
   *         we have this returning a linked LinkedHashMap (for testing)
   *         if  extra traversal gives  runtime issues we can output mapped
   */
  private def frequency(wds: List[String]):
  mutable.LinkedHashMap[String, Double] = {
    val counts = new mutable.LinkedHashMap[String, Double]()

    if(wds.equals(List(""))){
      counts
    }

    else {
      val grouped = wds.groupBy(identity)
      val mapped = grouped.map(group => (group._1, group._2.size.toDouble ) )
      for (pair <- mapped) {
        counts.update(pair._1, pair._2)
      }
      counts
    }
  }

  /**
   * Getter method that returns frequency method
   */
  def getFrequency(wds: List[String]):
  mutable.LinkedHashMap[String, Double] = {
    frequency(wds: List[String])
  }

  /**
   * Method that calculates the "term frequency" for every word in document
   * This method normalizes the amount of times a word appears in a document
   * @param wds -- text
   * @return LinkedHashMap mapping each word to term frequency
   *         we have this returning a linked LinkedHashMap (for testing)
   *         if  extra traversal gives  runtime issues we can output mapped
   */
  private def termFreq(wds: List[String]):
  mutable.LinkedHashMap[String, Double] = {
    val termFreqHash = new mutable.LinkedHashMap[String, Double]

    //find use term counts but divide everything by mode

    if(wds.equals(List("")) | wds.isEmpty){
      termFreqHash
    }

    else {
      val freqs  = frequency(wds)
      //find mode
      val mode = freqs.maxBy(key => key._2)._2
      //val grouped = wds.groupBy(identity)
      val mapped = freqs.map(group => (group._1, group._2/mode ) )
      for (pair <- freqs) {
        termFreqHash.update(pair._1, pair._2 / mode)
      }
      termFreqHash
    }

  }

  /**
   * Getter method that returns termFreq method
   */
  def getTermFreq(wds: List[String]):
  mutable.LinkedHashMap[String, Double] = {
    termFreq(wds)
  }


  //INVERSE DOCUMENT FREQUENCY
  /**
   * Method that finds inverse document frequency for every word
   * @param data -- LinkedHashMap mapping page ids (type int) to body ( string)
   * @return LinkedHashMap mapping strings to the inverse doc frequency value
   */
  private def termInvDocFreq(data: mutable.LinkedHashMap[Int, List[String]]):
  mutable.LinkedHashMap[String, Double] ={
    //number of docs
    val numDocs = data.size

    //new LinkedHashMap to hold # of docs containing term
    val numContains = new mutable.LinkedHashMap[String, Int]()
    for (page <- data){
      val counts = frequency(page._2) //LinkedHashMap of term freqs in a page
      for (term <- counts){
        if(numContains.contains(term._1)){

          //if term is already in LinkedHashMap add one to its frequency
          //add one to the # of docs if has it
          numContains.update(term._1, numContains(term._1) + 1)
        } else{
          numContains.update(term._1, 1) //initialize with one if 1st instance
        }
      }
    }
    numContains.map(pair => (pair._1, log(numDocs/pair._2)))
  }

  /**
   * Getter method that returns termInvDocFreq method
   */
  def getTermInvDocFreq
  (data: mutable.LinkedHashMap[Int, List[String]]):
  mutable.LinkedHashMap[String, Double] ={
    termInvDocFreq(data)
  }

  /**
   * @return the LinkedHashMap of words to a LinkedHashMap
   * of the document Ids they appear in along with their frequency in that
   * document
   */
  private def wordsToDocumentFrequencies():
  mutable.LinkedHashMap[String, mutable.LinkedHashMap[Int, Double]] ={
    val wiki = toFormat()._1
    //new LinkedHashMap to hold # of docs containing term
    val numContains = new mutable.LinkedHashMap[String, mutable.LinkedHashMap[Int, Double]]()
    for (page <- wiki){
      val counts = frequency(page._2) //LinkedHashMap of term freqs in a page
      for (term <- counts){
        if (numContains.contains(term._1)) {
          numContains.update(term._1, numContains(term._1) += (page._1 ->
            term._2))
        }
        else{
          val idtofreq = new mutable.LinkedHashMap[Int, Double]
          numContains.update(term._1, idtofreq += (page._1 ->
            term._2))
        }
      }
    }
    numContains
  }

  /**
   * Getter method that returns wordsToDocumentFrequencies method
   */
  def getWordsToDocumentFrequencies():
  mutable.LinkedHashMap[String, mutable.LinkedHashMap[Int, Double]] ={
    wordsToDocumentFrequencies()
  }

  /**
   * Method that maps each page id to it's title
   * @return LinkedHashMap mapping page id to title
   */
  private def idstoTitles(): mutable.LinkedHashMap[Int, String] = {

    //Initialize a LinkedHashMap to map each page to the title
    val idstoTitles = new mutable.LinkedHashMap[Int, String]

    for (page <- this.loadpage()) {
      idstoTitles.update(page.id, page.title)
    }
    idstoTitles

  }

  /**
   * Getter method that returns idstoTitles method
   */
  def getIdstoTitles(): mutable.LinkedHashMap[Int, String]  ={
    idstoTitles()
  }

  /**
   * Helper method that removes the brackets of a string (for links)
   * @param word -- link of type string
   * @return removed brackets
   */
  def removeBrac(word: String): String ={
    var wordFixed = word.replace("[", "")
    wordFixed =  wordFixed.replace("]", "")

    wordFixed
  }



  /**
   * takes in list of strings and an id for a page and outputs
   *  a list of links for a certain page
   * (Ignores duplicate links and links to itself)
   * @return List of strings that are links for a page
   */
  private def listOfLinks2(id: Int, strs: List[String]): List[String] = {
    val idtoTitle = this.idstoTitles()
    val titletoId = idtoTitle.map(_.swap)

    var links = List[String]()
    for (word <- strs) {
      if (word.length > 1) { //loop through words in page text body lists
        val firstchar = word.charAt(0).toString
        val secondchar = word.charAt(1).toString
        //chick if it is a link
        if (firstchar == "[" &&
          secondchar == "[") {
          val newWord = removeBrac(word)
          //check if duplicate link
          val testing = idtoTitle(id)
          if ((!links.contains(newWord)) && (!(newWord == idtoTitle(id)))) {
            //check if in corpus
            if (titletoId.contains(newWord)) {
              links = links ::: List(newWord)
            }
          }
        }
      }
    }
    if (links.isEmpty){ //add every single page
      for (pair <- titletoId){
        links ::: List(pair._1)
      }
    }
    links
  }

  /**
   * Getter method that returns listOfLinks2 method
   */
  def getlistOfLinks2(id: Int, strs: List[String]): List[String] ={
    listOfLinks2(id,strs)
  }




  /**
   * Helper method that maps each page id to a list of links
   * (Ignores duplicate links and links to itself)
   * @return LinkedHashMap mapping page id to the links
   */
  private def listOfLinks(): mutable.LinkedHashMap[Int, List[String]] = {
    val pagesLinks = new mutable.LinkedHashMap[Int, List[String]]()
    val wikiHash = this.toFormat()._1
    val idtoTitle = this.idstoTitles()
    val titletoId = idtoTitle.map(_.swap)

    for (page <- this.loadpage()) {
      pagesLinks.update(page.id, listOfLinks2(page.id,
        toTokenizeHelper(page.text + " " + page.body)))
    }
    pagesLinks
  }

  /**
   * Getter method that returns listOfLinks method
   */
  def getlistOfLinks(): mutable.LinkedHashMap[Int, List[String]] ={
    listOfLinks()
  }





  /**
   * Helper method that calculates equation in handout
   * @param links -- list of links
   * @return double representing value
   */
  private def calcLink(links: List[String]): Double ={
    val nk = links.size
    val n = this.toFormat()._1.size
    val eps = 0.15
    if (n == 0){
      0
    } else if (nk == 0){
      0
    } else {
      eps / n + (1 - eps) * (1 / nk)
    }
  }

  /**
   * Getter method that returns calcLink method
   */
  def getCalcLink(links: List[String]): Double ={
    calcLink(links)
  }


  private def weights(): Array[Array[Double]] ={
    val n = this.toFormat()._1.size
    val eps = 0.15
    val weightMatrix = Array.ofDim[Double](n,n)
    val listolinks = this.listOfLinks()
    val idstits = this.idstoTitles()

    var jkey = listolinks.head._1; //initialize first id
    for (j <- 0 to n-1){
      var kkey = listolinks.head._1; //initialize first id
      for(k <- 0 to n-1){

        if(listolinks(kkey).contains(idstits(jkey))){
          weightMatrix(j)(k) = calcLink(listolinks(kkey))
        } else{
          weightMatrix(j)(k) = eps/n

        }
        kkey = kkey + 1
      }
      jkey = jkey + 1
    }

    weightMatrix

  }

  /**
   * Getter method that returns weights method
   */
  def getweights(): Array[Array[Double]] ={  weights()}


  /**
   * Helper method that calculates the distance between rankings
   * @param r -- r
   * @param rp -- r prime
   * @return Euclidean distance
   */
  private def distance(r: Array[Double], rp: Array[Double]): Double = {
    val n = r.size
    val np = rp.size
    var summation = 0.0

    if (r.isEmpty | rp.isEmpty){
      return 0.0
    }
    if (n != np){
      return 0.0
    }
    //Summation
    for (i <- 0 to n-1) {
      summation = summation + math.pow((rp(i) - r(i)),2)

    }
    sqrt(summation)

  }

  /**
   * Getter method that returns distance method
   */
  def getDistances(r: Array[Double], rp: Array[Double]): Double =
  {return distance(r,rp)}

  private def pageRank():  mutable.LinkedHashMap[Int, Double] = {
    val pageRankHash = new mutable.LinkedHashMap[Int, Double]
    val wiki = idstoTitles()
    val n = wiki.size
    val w = weights()
    var r = new Array[Double](n)
    val rp = new Array[Double](n)

    //initialize r to 0
    //initialize rp to 1/n

    for (i <- 0 to n - 1) {
      r(i) = 0
      rp(i) = 1 / n.toDouble
    }

    while (distance(r, rp) > 0.001) {
      r = rp.clone()
      //reset rp to 0
      for (i <- 0 to n - 1) {
        rp(i) = 0
      }
      for (j <- 0 to n - 1) {
        for (k <- 0 to n - 1) {
          rp(j) = rp(j) + w(j)(k) * r(k)
        }
      }
    }
    var id = wiki.head._1
    for (l <- 0 to n-1) {
      pageRankHash.update(id, rp(l))
      id = id + 1
    }
    //    pageRankHash foreach println
    pageRankHash

  }

  /**
   * Getter method that returns distance method
   */
  def getPageRanks(): mutable.LinkedHashMap[Int, Double] ={  pageRank()}



}






object Index {

  def main(args: Array[String]) {

  }
}
