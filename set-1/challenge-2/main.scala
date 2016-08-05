object app {
  
  def main(args: Array[String]): Unit = {
    try {
      // Get input numbers
      val number1 = args(0)
      val number2 = args(1)

      // Convert input from base 16 to Bytes
      val buffer1 = new java.math.BigInteger(number1, 16).toByteArray()
      val buffer2 = new java.math.BigInteger(number2, 16).toByteArray()
      
      // Input buffers must be the same length
      if (buffer1.length != buffer2.length)
        throw new Exception("Hex values must be the same length")

      // XOR buffer1 and buffer2
      val outputBuffer = new Array[Byte](buffer1.length)    
      for ( i <- 0 to buffer1.length - 1) {
        val xorValue = buffer1(i) ^ buffer2(i)
        outputBuffer(i) = xorValue.toByte
      }
      
      // encode output as hex
      val ans = bytes2hex(outputBuffer) 
      println(ans)

    } catch {
      case e : Exception => println(e)
    }
  }
  
  // Function to convert bytes to base 16 string
  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

}
