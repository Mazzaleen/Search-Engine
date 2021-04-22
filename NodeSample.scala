package search.src


object NodeSample {


  val mainNode =
    <xml>
      <page>
        <id>0</id>
        <body>
          This is the body text!
          This finna go crazy.
          He's wildin out rn.
        </body>
      </page>
      <page>
        <id>1</id>
        <body>
          This is the body text!
          This finna go crazy.
          He's wildin out rn.
        </body>
      </page>
    </xml>

  def main(args: Array[String]) {

    // These are both the string "This is the body text!"
    val page = (mainNode \ "page").text
    println(page)


    //val pageid = (otherNode \ "page" \ "id").text
    //println(pageid)

    val page0Body = ((mainNode \ "page") \ "body").text
    val page0AltBody = (mainNode \\ "body").text
    println(page0Body)
    println(page0AltBody)
  }

}
