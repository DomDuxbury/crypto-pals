object app {
  def main(args: Array[String]): Unit = {
    try {
      val base16 = args(0)
      val value = Integer.parseInt(base16, 16)
      val base64 = value.toHexString(64)
      println(base64)
    } catch {
      case e : Exception => println(e)
    }
  }
}
