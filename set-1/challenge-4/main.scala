import scala.collection.immutable.ListMap

object app {
  
  def main(args: Array[String]): Unit = {
    try {

      // Get input numbers
      val encodedHex = io.Source.fromFile("encrypted_lines.txt").mkString
      val encodedLines = encodedHex.split("\n")
      var lineCount = 1
      encodedLines.foreach(encodedLine => {
        // Convert input from base 16 to Bytes then to String
        val encodedByteArray = hex2bytes(encodedLine)
        val encodedString = new String(encodedByteArray);
       
        // Count most frequent chars in the input
        val mostFreqChars = countChars(encodedString)
        
        // Get the possible solutions
        val possibleSolutions = crackerAlgorithm(encodedString, mostFreqChars)
        
        prettyPrintSolutions(possibleSolutions, lineCount)
        lineCount += 1
      })
    } catch {
      case e : Exception => println(e)
    }
  }

  def prettyPrintSolutions(possibleSolutions : Seq[(String, Char)], lineNumber: Int) : Unit = {
    if (possibleSolutions.length > 0) {
      println(s"Solutions for line ${lineNumber}: ") 
      var count = 1
      possibleSolutions map (solution => {
        println(s"\t ${count}: ${solution._1}\t Encoded with: ${solution._2}\n")
        count += 1
      })
    } else {
      println(s"No Solutions for line ${lineNumber}")
    }
  }

  def xorTwoChars(char1: Char, char2: Char) : Int = {
    char1.toString.getBytes("UTF-8")(0) ^ char2.toString.getBytes("UTF-8")(0)
  }

  def hex2bytes(hex: String): Array[Byte] = {
    if(hex.contains(" ")){
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if(hex.contains("-")){
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  def xorDecodeString(encodedMessage: String, decodeChar: Char) : String = { 
    encodedMessage map (
      encodedChar => xorTwoChars(encodedChar, decodeChar).toChar
    )
  }
  
  def guessXORMappingChar(input : String, mostFreqChars: Seq[Char], guesses: (Char, Char)) : (Boolean, Char) = {
    val xorGuess1 = xorTwoChars(mostFreqChars(0), guesses._1)
    val xorGuess2 = xorTwoChars(mostFreqChars(1), guesses._2)
    val matchSuccess = xorGuess1 == xorGuess2
    var charGuess = new String(Array(xorGuess1.toByte)).head 
    (matchSuccess, charGuess)
  }

  def crackerAlgorithm(input : String, mostFreqChars: Seq[Char]) : Seq[(String, Char)] = {
    val highFreqChars = "EeTtAaOoIiNn SsHhRrDdLlUu"

    val allGuesses = 
      for (highFreqChar1 <- highFreqChars; 
           highFreqChar2 <- highFreqChars 
           if highFreqChar1 != highFreqChar2) yield 
        guessXORMappingChar(input, mostFreqChars, (highFreqChar1, highFreqChar2))
    
    val matchedGuesses = allGuesses
      .filter(matchChar => matchChar._1)
      .map(matchChar => matchChar._2)
    
      matchedGuesses.map(guess => (xorDecodeString(input, guess), guess))
  }

  def countChars(inputString: String): Seq[Char] = {
    val unsortedCharFreq = inputString.groupBy(_.toChar)
                              .map(p => (p._1, p._2.length))
    
    ListMap(unsortedCharFreq.toSeq.sortBy(_._2).reverse:_*)
      .take(2)
      .map(charMapToCount => charMapToCount._1).toSeq
  }

}
