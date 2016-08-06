object app {
  
  def main(args: Array[String]): Unit = {
    try {

      // Get input numbers
      val number1 = args(0)
      val xorChar = args(1)

      // Convert input from base 16 to Bytes
      val buffer1 = new java.math.BigInteger(number1, 16).toByteArray()
      val xorByte = xorChar.getBytes("UTF-8")(0)
      
      // XOR buffer1 and buffer2
      val outputBuffer = new Array[Byte](buffer1.length)    
      for ( i <- 0 to buffer1.length - 1) {
        val xorValue = buffer1(i) ^ xorByte
        outputBuffer(i) = xorValue.toByte
      }
      
      // encode output as hex
      val inputStr = new String(buffer1, "US-ASCII");
      val charFreq = countChars(inputStr)

      val s = charFreq.view map {
          case (key, value) => "Key: " + key + "\nValue: " + value
      } mkString ("", "\n", "\n")

      println("myMap" + s)
      println(f"Input: ${inputStr}")
      val ans = new String(outputBuffer, "US-ASCII");
      println(f"Output: ${ans}")

    } catch {
      case e : Exception => println(e)
    }
  }
  
  def countChars(inputString: String): Map[Char, Int] = {
    inputString.groupBy(_.toChar).map(p => (p._1, p._2.length))
  }
  // Function to convert bytes to base 16 string
  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

}
