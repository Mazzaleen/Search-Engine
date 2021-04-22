package search.sol

import tester.Tester


import scala.collection.mutable
import scala.math.log


class SearchTest {

  def testToFormat(t: Tester) {
    val indexer = new Index("rapWiki.xml")
    t.checkExpect(indexer.toFormat()._1(0), List("bodi", "text",
      "finna", "go", "crazi","he'", "wildin", "rn", "spit", "bar", "rn"))
    t.checkExpect(indexer.toFormat()._1(1), List("new", "york", "milli",
      "rock", "sock", "runnin", "op", "cuz", "run", "thei", "block"))
    t.checkExpect(indexer.toFormat()._1(2), List("ridin", "top", "boi", "better",
      "come", "get", "man", "pop", "yah", "yah", "yah", "yah"))

    val indexer2 = new Index("stopWordsWiki.xml")
    t.checkExpect(indexer2.toFormat()._1(0),List())
    t.checkExpect(indexer2.toFormat()._1(1),List())
    t.checkExpect(indexer2.toFormat()._1(2),List())

    val indexer3 = new Index("numbersWiki.xml")
    t.checkExpect(indexer3.toFormat()._1(0), List("1", "2", "3",
      "4", "5", "6", "7", "8", "9", "10"))
    t.checkExpect(indexer3.toFormat()._1(1), List("02912"))
    t.checkExpect(indexer3.toFormat()._1(2), List("401", "123", "5686"))
    t.checkExpect(indexer3.toFormat()._1(3), List("401", "123345", "7890"))
    t.checkExpect(indexer3.toFormat()._1(4), List("1", "1", "2"))
  }

  def testToTokenizeHelper(t: Tester): Unit = {
    val indexer = new Index("rapWiki.xml")
    val edgecase = "This is the body text! " +
      "This finna go crazy. He's wildin out rn. he spitting bars rn:"

    t.checkExpect(indexer.toTokenizeHelper(edgecase),
      List("bodi", "text"
        , "finna", "go", "crazi","he'", "wildin", "rn", "spit", "bar", "rn"))

    t.checkExpect(indexer.toTokenizeHelper(
      "in new york i milly roc" +
        "k had it in my sock runnin from the op cuz i run they block"),
      List("new", "york", "milli", "rock",
        "sock", "runnin", "op", "cuz", "run", "thei", "block"))

    t.checkExpect(indexer.toTokenizeHelper(""),
      List())

    t.checkExpect(indexer.toTokenizeHelper("1 + 1 = 2!"),
      List("1", "1", "2"))
  }

  def testTermMostFreq(t: Tester): Unit = {
    val indexer = new Index("rapWiki.xml")

    t.checkExpect(indexer.getTermMostFreq()(0), 2.0)
    t.checkExpect(indexer.getTermMostFreq()(1), 1.0)
    t.checkExpect(indexer.getTermMostFreq()(2), 4.0)

    val indexer2 = new Index("numbersWiki.xml")

    t.checkExpect(indexer2.getTermMostFreq()(0), 1.0)
    t.checkExpect(indexer2.getTermMostFreq()(1), 1.0)
    t.checkExpect(indexer2.getTermMostFreq()(2), 1.0)
    t.checkExpect(indexer2.getTermMostFreq()(3), 1.0)
    t.checkExpect(indexer2.getTermMostFreq()(4), 2.0)
  }

  def testfrequency(t: Tester): Unit = {
    val indexer = new Index("rapWiki.xml")

    t.checkExpect(indexer.getFrequency(
      List("pop", "yah", "yah", "yah", "yah"))("pop"),
      1)
    t.checkExpect(indexer.getFrequency(
      List("pop", "yah", "yah", "yah", "yah"))("yah"),
      4)

    t.checkExpect(indexer.getFrequency(List("bit", "byte",
      "nibble", "byte", "batch"))("batch"), 1)
    t.checkExpect(indexer.getFrequency(List("bit", "byte",
      "nibble", "byte", "batch"))("bit"), 1)
    t.checkExpect(indexer.getFrequency(List("bit", "byte",
      "nibble", "byte", "batch"))("byte"), 2)

    t.checkExpect(indexer.getFrequency(List("belinda",
      "melinda", "belin", "melinda", "bel"))("belinda"), 1)
    t.checkExpect(indexer.getFrequency(List("belinda",
      "melinda", "belin", "melinda", "bel"))("melinda"), 2)
    t.checkExpect(indexer.getFrequency(List("belinda",
      "melinda", "belin", "melinda", "bel"))("belin"), 1)
    t.checkExpect(indexer.getFrequency(List("belinda",
      "melinda", "belin", "melinda", "bel"))("bel"), 1)

    t.checkExpect(indexer.getFrequency(List("Ax", "axe", "ax"))("Ax"),
      1)
    t.checkExpect(indexer.getFrequency(List("Ax", "axe", "ax"))("axe"),
      1)
    t.checkExpect(indexer.getFrequency(List("Ax", "axe", "ax"))("ax"),
      1)
    t.checkExpect(indexer.getFrequency(List("")),
      mutable.LinkedHashMap(), "counts empty hash")
    t.checkExpect(indexer.getFrequency(List()),
      mutable.LinkedHashMap(), "counts empty list")
  }

  def testTermFreq(t: Tester): Unit = {
    val indexer = new Index("rapWiki.xml")

    t.checkExpect(indexer.getTermFreq(List("pop", "yah", "yah", "yah", "yah"))("pop"),
      0.25)
    t.checkExpect(indexer.getTermFreq(List("pop", "yah", "yah", "yah", "yah"))("yah"),
      1)

    t.checkExpect(indexer.getTermFreq(List("bit", "byte",
      "nibble", "byte", "batch"))("batch"), 1.0/2.0)
    t.checkExpect(indexer.getTermFreq(List("bit", "byte",
      "nibble", "byte", "batch"))("bit"), 1.0/2.0)
    t.checkExpect(indexer.getTermFreq(List("bit", "byte",
      "nibble", "byte", "batch"))("byte"), 1.0)

    t.checkExpect(indexer.getTermFreq(List("belinda",
      "melinda", "belin", "melinda", "bel"))("belinda"), 1.0/2.0)
    t.checkExpect(indexer.getTermFreq(List("belinda",
      "melinda", "belin", "melinda", "bel"))("melinda"), 1.0)
    t.checkExpect(indexer.getTermFreq(List("belinda",
      "melinda", "belin", "melinda", "bel"))("belin"), 1.0/2.0)
    t.checkExpect(indexer.getTermFreq(List("belinda",
      "melinda", "belin", "melinda", "bel"))("bel"), 1.0/2.0)

    t.checkExpect(indexer.getTermFreq(List("Ax", "axe", "ax"))("Ax"),
      1.0)
    t.checkExpect(indexer.getTermFreq(List("Ax", "axe", "ax"))("axe"),
      1.0)
    t.checkExpect(indexer.getTermFreq(List("Ax", "axe", "ax"))("ax"),
      1.0)
    t.checkExpect(indexer.getTermFreq(List("")),
      mutable.LinkedHashMap(), "counts empty hash")
    t.checkExpect(indexer.getTermFreq(List()),
      mutable.LinkedHashMap(), "counts empty list")
  }

  def testTermInvDocFreq(t: Tester) {

    val indexer = new Index("numbersWiki.xml")
    val expectedFormat = mutable.LinkedHashMap[Int, List[String]]()
    expectedFormat.update(0,List("1", "2", "3"))
    expectedFormat.update(1,List("02912"))
    expectedFormat.update(2,List("401", "123345", "7890"))
    expectedFormat.update(3,List("1", "1", "2"))

    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("1"), log(4/2))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("2"), log(4/2))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("3"), log(4/1))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("02912"), log(4/1))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("401"), log(4/1))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("123345"), log(4/1))
    t.checkExpect(indexer.getTermInvDocFreq(expectedFormat)("7890"), log(4/1))

    val indexer2 = new Index("numbersWiki.xml")
    val expectedFormat2 = mutable.LinkedHashMap[Int, List[String]]()
    expectedFormat2.update(0,List("hat", "hat", "hat"))
    expectedFormat2.update(1,List("hat", "hat"))
    expectedFormat2.update(2,List("bar", "bar"))
    expectedFormat2.update(3,List("bar", "hat"))

    t.checkExpect(indexer2.getTermInvDocFreq(expectedFormat2)("hat"), log(4/3))
    t.checkExpect(indexer2.getTermInvDocFreq(expectedFormat2)("bar"), log(4/2))

    val indexer3 = new Index("numbersWiki.xml")
    val expectedFormat3 = mutable.LinkedHashMap[Int, List[String]]()
    expectedFormat3.update(0,List("hat", "hat", "hat"))
    expectedFormat3.update(1,List("hat", "hat"))

    t.checkExpect(indexer3.getTermInvDocFreq(expectedFormat3)("hat"), log(4/3))
  }

  def testListOfLinks(t: Tester) {
    val indexer = new Index("oneLink.xml")
    t.checkExpect(indexer.getlistOfLinks()(0),  List("page2"))

    val indexer2 = new Index("dupLinks.xml")
    t.checkExpect(indexer2.getlistOfLinks()(0).head, "page 2 mac")
    t.checkExpect(indexer2.getlistOfLinks()(2).head, "page 2 mac"  )

    val indexer3 = new Index("multipleLinks.xml")
    t.checkExpect(indexer3.getlistOfLinks()(0).head,
      "laptops2")
    t.checkExpect(indexer3.getlistOfLinks()(1).head,
      "laptops3")
    t.checkExpect(indexer3.getlistOfLinks()(2).head,
      "laptops")


    val indexer4 = new Index("emptyWiki.xml")
    t.checkExpect(indexer4.getlistOfLinks(), mutable.LinkedHashMap())
  }

  def testlistOfLinks2(t: Tester) {
    val indexer = new Index("oneLink.xml")
    t.checkExpect(indexer.getlistOfLinks2(0, List("[[Category: Macbook]]", "[[page2]]")),  List("page2"))
    t.checkExpect(indexer.getlistOfLinks2(1, List("[[page2]]", "I", "love", "Computers", "so", "much")),  List())
    t.checkExpect(indexer.getlistOfLinks2(2, List("I", "use", "macbook", "computers", "all", "day")),  List())

    val indexer2 = new Index("dupLinks.xml")
    t.checkExpect(indexer2.getlistOfLinks2(0,
      List("[[Category: Macbook]]","[[Category: Macbook]]",
        "[[Category: Macbook]]","[[page 2 mac]]","[[page 2 mac]]","[[page 2 mac]]")), List("page 2 mac"))
    t.checkExpect(indexer2.getlistOfLinks2(1, List("[[Category: Macbook]]","where are my usb cables")), List())
    t.checkExpect(indexer2.getlistOfLinks2(2,
      List("I", "use", "macbook", "computers", "all", "day","[[Category: Macbook]]","[[Category: Macbook]]",
        "[[Category: Macbook]]","[[page 2 mac]]")), List("page 2 mac"))

    val indexer3 = new Index("multipleLinks.xml")
    t.checkExpect(indexer3.getlistOfLinks2(0,List("[[Category: Macbook]]","[[Category: XPS]]","[[laptops2]]")), List("laptops2"))
    t.checkExpect(indexer3.getlistOfLinks2(1,List("I","love","Computers","so","much","[[laptops3]]")), List("laptops3"))
    t.checkExpect(indexer3.getlistOfLinks2(2,List("I","use","macbook","computers","all","day","[[Category: Samsung]]","[[laptops]]")),
      List("laptops"))


    val indexer4 = new Index("emptyWiki.xml")
    t.checkExpect(indexer4.getlistOfLinks2(0, List()), List())
  }

  def testIDstoTitles (t: Tester): Unit = {
    val indexer = new Index("oneLink.xml")
    t.checkExpect(indexer.getIdstoTitles()(0),"page1")
    t.checkExpect(indexer.getIdstoTitles()(1),"page2")
    t.checkExpect(indexer.getIdstoTitles()(2),"page3")

    val indexer2 = new Index("dupLinks.xml")
    t.checkExpect(indexer2.getIdstoTitles()(0),"macpage")
    t.checkExpect(indexer2.getIdstoTitles()(1),"page 2 mac")
    t.checkExpect(indexer2.getIdstoTitles()(2),"pages231dfa3")

    val indexer3 = new Index("multipleLinks.xml")
    t.checkExpect(indexer3.getIdstoTitles()(0),"laptops")
    t.checkExpect(indexer3.getIdstoTitles()(1),"laptops2")
    t.checkExpect(indexer3.getIdstoTitles()(2),"laptops3")

    val indexer4 = new Index("emptyWiki.xml")
    t.checkExpect(indexer4.getIdstoTitles()(0),"null")
    t.checkExpect(indexer4.getIdstoTitles()(1),"also null")
    t.checkExpect(indexer4.getIdstoTitles()(2),"also also null")

  }

  def testCalcLink(t: Tester) {
    val indexer = new Index("oneLink.xml")
    t.checkExpect(indexer.getCalcLink(List("hae", "yo", "hi")),
      0.15/3 + (1-0.15)*(1/3))

    val indexer2 = new Index("oneLink.xml")
    t.checkExpect(indexer2.getCalcLink(List("bruv", "innit", "hi", "yess")),
      0.15/3 + (1-0.15)*(1/4))

    val indexer3 = new Index("oneLink.xml")
    t.checkExpect(indexer3.getCalcLink(List()), 0)

  }

  def testDistance(t: Tester) {
    val indexer = new Index("oneLink.xml")
    t.checkExpect(indexer.getDistances(Array(1,1,1), Array(1)),0.0)
    t.checkExpect(indexer.getDistances(Array(1,1,1), Array(1,1,1)),0.0)
    t.checkExpect(indexer.getDistances(Array(3, 4), Array(0,0)), 5.0)
    t.checkExpect(indexer.getDistances(Array(6, 16), Array(3,12)), 5.0)
    t.checkExpect(indexer.getDistances(Array(), Array()), 0.0)
    t.checkExpect(indexer.getDistances(Array(0), Array(0)), 0.0)
  }

  def testWeights(t: Tester) {
    val eps = 0.15
    val indexer = new Index("oneLink.xml")
    var weightMatrix1 = Array.ofDim[Double](3,3)
    weightMatrix1 = Array(Array(0.05, .05, .05),
      Array(eps/3 + (1-eps)*(1/1), 0.05), Array(0.05, .05, .05))

    for (i <- 0 to 2) {
      t.checkExpect(indexer.getweights()(0)(i), eps / 3)
    }
    t.checkExpect(indexer.getweights()(1)(0), eps/3 + (1-eps)*(1/1))
  }

  def testLinkCleaner(t: Tester) {
    val indexer = new Index("oneLink.xml")

    t.checkExpect(
      indexer.linkCleaner
      (List("[[Category:Macbook]]","[[Iphone]]")),
      List("Category:Macbook","Iphone"))

    t.checkExpect(indexer.linkCleaner
    (List("[[Category|Macbook]]","[[Iphone|XR]]")),
      List("Macbook","XR"))

    //not sure if correct
    t.checkExpect(indexer.linkCleaner
    (List()),
      List())

    t.checkExpect(indexer.linkCleaner
    (List("","")),
      List("",""))

    t.checkExpect(indexer.linkCleaner
    (List("a|")),
      List(""))


  }

  def testwordsToDocumentFrequencies(t: Tester) {

    val indexer = new Index("numbersWiki.xml")

    t.checkExpect(indexer.getWordsToDocumentFrequencies()("1")(0), 1)
    t.checkExpect(indexer.getWordsToDocumentFrequencies()("2")(0), 1)
    t.checkExpect(indexer.getWordsToDocumentFrequencies()("3")(0), 1)
    t.checkExpect(indexer.getWordsToDocumentFrequencies()("02912")(1), 1)
    t.checkExpect(indexer.getWordsToDocumentFrequencies()("401")(2), 1)
    t.checkExpect(indexer.getWordsToDocumentFrequencies()("401")(3), 1)

    val indexer2 = new Index("rapWiki.xml")

    t.checkExpect(indexer2.getWordsToDocumentFrequencies()("yah")(2), 4)
    t.checkExpect(indexer2.getWordsToDocumentFrequencies()("top")(2), 1)

    val indexer3= new Index("emptyWiki.xml")

    t.checkExpect(indexer3.getWordsToDocumentFrequencies(), mutable.LinkedHashMap())
  }


  def testInvDocFreq(t: Tester) {

    val Indexer = new Index("numbersWiki.xml")

    val query = new Query(
      "titleIndex",
      "documentIndex",
      "wordIndex",
      false)

    query.readFiles()

    t.checkExpect(query.invDocFreq("1"), log(5.0/2.0))
    t.checkExpect(query.invDocFreq("2"), log(5.0/2.0))
    t.checkExpect(query.invDocFreq("3"), log(5.0/1.0))
    t.checkExpect(query.invDocFreq("02912"), log(5.0/1.0))
    t.checkExpect(query.invDocFreq("123345"), log(5.0/1.0))
    t.checkExpect(query.invDocFreq("7890"), log(5.0/1.0))

  }

  def testRelevance(t: Tester) {
    val Indexer = new Index("numbersWiki.xml")

    val query = new Query("titleIndex", "documentIndex", "wordIndex", false)

    query.readFiles()

    t.checkExpect(query.relevance("1")(0).round, 1 / query.getMaxFreqs()(0))
    t.checkExpect(query.relevance("1")(4).round, 1 / query.getMaxFreqs()(0))
    t.checkExpect(query.relevance("1")(1).round, 0)
    t.checkExpect(query.relevance("1")(2).round, 0)

  }

  def testSearchCleaner(t: Tester) {
    val Indexer = new Index("numbersWiki.xml")
    val query = new Query("titleIndex",
      "documentIndex",
      "wordIndex",
      false)

    query.readFiles()


    t.checkExpect(query.searchCleaner("jordan"),
      List("jordan"))

    t.checkExpect(query.searchCleaner("jordan is goat"),
      List("jordan","goat"))

    t.checkExpect(query.searchCleaner("jordan jumped jumping"),
      List("jordan","jump"))

    t.checkExpect(query.searchCleaner(""),
      List())

  }


  //  //TODO: not necessary?
  //  def testPageRank(t: Tester) {
  //    val indexer = new Index("PageRankWiki.xml")
  //    indexer.pageRank() foreach println
  //  }


}



object SearchTest extends App {
  Tester.run(new SearchTest)
}