import scala.collection.immutable.ListMap

object app {
  
  def main(args: Array[String]): Unit = {
    try {

      // Get input numbers
      val number1 = args(0)
      //val xorChar = args(1)

      // Convert input from base 16 to Bytes
      val buffer = new java.math.BigInteger(number1, 16).toByteArray()
      val xorChar = "a".getBytes("UTF-8")(0)

      val outputBuffer = xorBufferAndByte(buffer, xorChar) 

      // encode output as hex
      val inputStr = new String(buffer);
      val charFreq = countChars(inputStr)
      crackerAlgorithm(inputStr, (charFreq.head._1, charFreq.tail.head._1))
     // val decodeChar = guessXORMappingChar(charFreq)
     // println("Decode Char: " + decodeChar)
      //val decodedString = inputStr map (xorTwoChars('', _))

      //decodedString map print
    } catch {
      case e : Exception => println(e)
    }
  }

  def xorTwoChars(char1: Char, char2: Char) : String = {
    val output = char1.toString.getBytes("UTF-8")(0) ^ char2.toString.getBytes("UTF-8")(0)
    new String(Array(output.toByte))
  }
  
  def xorBufferAndByte(buffer: Array[Byte], byte: Byte): Array[Byte] = {
    
    val outputBuffer = new Array[Byte](buffer.length)    
    
    for ( i <- 0 to buffer.length - 1) {
      outputBuffer(i) = (buffer(i) ^ byte).toByte
    }

    outputBuffer
  }

  def guessXORMappingChar(input : String, mostFreqChars: (Char,Char), guesses: (Char, Char)) : Char = {
    val xorGuess1 = xorTwoChars(mostFreqChars._1, guesses._1)
    val xorGuess2 = xorTwoChars(mostFreqChars._2, guesses._2)
    val matchSuccess = xorGuess2 == xorGuess1
    if (matchSuccess && xorGuess1.length == 1) {
      println(xorGuess1.head)
      println("Char 1: " + mostFreqChars._1 + " -> " + guesses._1
      + " Char 2: " + mostFreqChars._2 + " -> " + guesses._2
      + " XORGuess: " + xorGuess1
      + " Success: " + matchSuccess); 
      val decodedString = input map (xorTwoChars(xorGuess1.head, _))
      decodedString map print
    }
    'a'
  }

  def crackerAlgorithm(input : String, mostFreqChars: (Char,Char)) : Char = {
    val highFreqChars = "EeTtAaOoIiNn SsHhRrDdLlUu"
    for (highFreqChar1 <- highFreqChars;
         highFreqChar2 <- highFreqChars 
         if highFreqChar1 != highFreqChar2) yield 
      guessXORMappingChar(input, mostFreqChars, (highFreqChar1, highFreqChar2))
    'a'
  }

  def countChars(inputString: String): Map[Char, Int] = {
    val unsortedCharFreq = inputString.groupBy(_.toChar)
                              .map(p => (p._1, p._2.length))
    
    val sortedCharFreq = ListMap(unsortedCharFreq.toSeq.sortBy(_._2).reverse:_*)
    //printMap(sortedCharFreq) 
    sortedCharFreq
  }

  def printMap(map : Map[Char, Int]) = {
     val mapString = map.view map {
          case (key, value) => "\tKey: " + "%04X".format(key.toInt) + "\tValue: " + value
      } mkString ("", "\n", "\n")

      println("myMap:\n" + mapString)
  }

  // Function to convert bytes to base 16 string
  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

}
