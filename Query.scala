package search.sol

import search.src.{FileIO, PorterStemmer, StopWords}

import java.io._
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import scala.math.log
import scala.util.matching.Regex

/**
 * Represents a query REPL built off of a specified index
 *
 * @paramtitleIndex    - the filename of the title index
 * @paramdocumentIndex - the filename of the document index
 * @param wordIndex     - the filename of the word index 
 * @param usePageRank   - true if page rank is to be incorporated into scoring
 */
class Query(titleIndex: String, documentIndex: String, wordIndex: String,
            usePageRank: Boolean) {

  val indexer = new Index("SRC.xml")

//    try {
//      val idtitlefile = FileIO.printTitleFile(titleIndex, indexer.getIdstoTitles())
//      val docfile = FileIO.printDocumentFile(documentIndex, indexer.getTermMostFreq(), indexer.getPageRanks())
//      val wordfile = FileIO.printWordsFile(wordIndex, indexer.getWordsToDocumentFrequencies())
//    } catch {
//      case e: FileNotFoundException => println("Invalid Filename")
//    }


  // Maps the document ids to the title for each document
  private val idsToTitle = new LinkedHashMap[Int, String]

  // Maps the document ids to the euclidean normalization for each document
  private val idsToMaxFreqs = new LinkedHashMap[Int, Double]

  // Maps the document ids to the page rank for each document
  private val idsToPageRank = new LinkedHashMap[Int, Double]

  // Maps each word to a map of document IDs and frequencies of documents that
  // contain that word
  private val wordsToDocumentFrequencies =
  new LinkedHashMap[String, LinkedHashMap[Int, Double]]

  readFiles()


  /**
   * Helper method that converts a query to a list of strings
    * (removing whitespaces and stop words)
   * @param userQuery -- string representing search
   * @return list of strings representing every word in search
   */
  def searchCleaner(userQuery: String): List[String] = {

    //initalize regex
    val regex = new Regex("""[^\W_]+'[^\W_]+|[^\W_]+""")
    // Call findAllMatchIn to get an iterator of Matches
    val matchesIterator = regex.findAllMatchIn(userQuery)
    // Convert the Iterator to a List and extract the matched substrings
    val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched}

    // initialize new list with the stop words removed
    val matchesStopRemoved =
      matchesList.filter(words => !StopWords.isStopWord(words))

    //remove stems of every word in the search
    val out = matchesStopRemoved.map { words => PorterStemmer.stem(words) }
    out.distinct
  }


  /**
   * Handles a single query and prints out results
   *
   * @param userQuery - the query text
   */
  private def query(userQuery: String) {
    //convert search  to list of strings
    val queryList = searchCleaner(userQuery)

    if (queryList.isEmpty) {
      println("Your search did not match any documents.")
    } else {
      try {
        if (usePageRank) {
          var scores = new mutable.LinkedHashMap[Int, Double]
          for (word <- queryList) {
            scores = scores ++ withRanks(word).map{case (k,v) => k ->
              (v + scores.getOrElse(k, 0.0))}
            scores = LinkedHashMap(scores.toSeq.sortWith(_._2 > _._2):_*)
          }
          printResults(scores)
        } else {
          var scores = new mutable.LinkedHashMap[Int, Double]
          for (word <- queryList) {
            scores = scores ++ relevance(word).map{case (k,v) => k ->
              (v + scores.getOrElse(k, 0.0))}
            scores = LinkedHashMap(scores.toSeq.sortWith(_._2 > _._2):_*)
          }
          printResults(scores)
        }
      } catch {
        case e:
          NoSuchElementException =>
          println("Your search did not match any documents.")
      }
    }
  }

  /**
   *
   * @param word
   * @return a double of the inverse document frequency
   */
  def invDocFreq(word: String): Double = {
    val n = this.idsToTitle.size.toDouble
    val ni = this.wordsToDocumentFrequencies(word).size.toDouble
    log(n / ni)

  }

//  def combineScores(word: String): LinkedHashMap[Int, Double] = {
//    relevance(word) ++
//  }

  /**
   * computes relevance values
   * @param word term that has been queried
   * @return a sorted hashmap with the id of the documents matched to
    *         the relevance value
   */
  def relevance(word: String): LinkedHashMap[Int, Double] = {
    val n = this.idsToTitle.size
    val maxfreqs = getMaxFreqs()
    val idf = invDocFreq(word)
    val rel = new LinkedHashMap[Int, Double]
    val freqs = this.wordsToDocumentFrequencies(word)

    for (pair <- maxfreqs){
      if(freqs.contains(pair._1)) {
        rel.update(pair._1, (freqs(pair._1) / pair._2) * idf)
      } else {
        rel.update(pair._1, 0)
      }
    }
    rel
  }

  /**
   * Helper Method that returns idsToMaxFreqs field
   *
   * @return idsToMaxFreqs field
   */
  def getMaxFreqs(): LinkedHashMap[Int, Double] = {
    this.idsToMaxFreqs
  }




  /**
   * Helper Method that returns idsToTitle field
   *
   * @return idsToTitle field
   */
  def getidsToTitle(): LinkedHashMap[Int, String] = {
    this.idsToTitle
  }





  /**
   * Helper Method that returns idsToPageRank field
   *
   * @return idsToPageRank field
   */
  def getidsToPageRank(): LinkedHashMap[Int, Double] = {
    this.idsToPageRank
  }

  def withRanks(word: String):LinkedHashMap[Int, Double] = {
    val rels = relevance(word)
    val ranks = this.idsToPageRank
    val out = new LinkedHashMap[Int, Double]

    for (pair <- rels){
      out.update(pair._1, pair._2 * ranks(pair._1))
    }
    out
  }
  /**
   * Format and print up to 10 results from the results list
   *
   * @param results - an array of all results to be printed
   */
  private def printResults(results: LinkedHashMap[Int, Double]) {

    val res = results.keySet.toArray

    for (i <- 0 until Math.min(10, results.size)) {
      println((i + 1) + " " + idsToTitle(res(i)))
    }
  }

  /*
   * Reads in the text files.
   */
  def readFiles(): Unit = {
    FileIO.readTitles(titleIndex, idsToTitle)
    FileIO.readDocuments(documentIndex, idsToMaxFreqs, idsToPageRank)
    FileIO.readWords(wordIndex, wordsToDocumentFrequencies)
  }

  /**
   * Starts the read and print loop for queries
   */
  def run() {
    val inputReader = new BufferedReader(new InputStreamReader(System.in))

    // Print the first query prompt and read the first line of input
    print("search> ")
    var userQuery = inputReader.readLine()

    // Loop until there are no more input lines (EOF is reached)
    while (userQuery != null) {
      // If ":quit" is reached, exit the loop
      if (userQuery == ":quit") {
        inputReader.close()
        return
      }

      // Handle the query for the single line of input
      query(userQuery)

      // Print next query prompt and read next line of input
      print("search> ")
      userQuery = inputReader.readLine()
    }

    inputReader.close()
  }
}

object Query {
  def main(args: Array[String]) { //This was string-Mazine
    try {
      // Run queries with page rank
      var pageRank = false
      var titleIndex = 0
      var docIndex = 1
      var wordIndex = 2
      if (args.size == 4 && args(0) == "--pagerank") {
        pageRank = true;
        titleIndex = 1
        docIndex = 2
        wordIndex = 3
      } else if (args.size != 3) {
        println("Incorrect arguments. Please use [--pagerank] <titleIndex> "
          + "<documentIndex> <wordIndex>")
        System.exit(1)
      }
      val query: Query =
        new Query(args(titleIndex), args(docIndex), args(wordIndex), pageRank)
      println(query.readFiles())
      query.run()
    } catch {
      case _: FileNotFoundException =>
        println("One (or more) of the files were not found")
      case _: IOException => println("Error: IO Exception")
    }
  }
}
