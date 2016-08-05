object app {
  def main(args: Array[String]): Unit = {
    try {
      val input = args(0)
      val encoder = java.util.Base64.getEncoder()

      // Convert input from base 16 to Bytes
      val bigInt = new java.math.BigInteger(input, 16).toByteArray()

      // encode bytes as base64 str
      val ans = encoder.encodeToString(bigInt);

      println(ans)
    } catch {
      case e : Exception => println(e)
    }
  }
}
