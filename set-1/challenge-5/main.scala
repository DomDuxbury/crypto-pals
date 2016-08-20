import java.io._

object app {
  
  def main(args: Array[String]): Unit = {
    try {
      val fileInput = args(0)
      val fileOutput = args(1)
      val encryptKey = args(2)

      // Get input txt
      val plainText = io.Source.fromFile(fileInput).mkString
      
      // Encrypt contents with repeatingXOR
      val output = repeatingXOR(plainText, encryptKey)
      
      // Open output file connection
      val outputFile = new PrintWriter(new File(fileOutput)) 
      
      outputFile.write(output)
      outputFile.close()
    } catch {
      case e : Exception => println(e)
    }
  }

  def repeatingXOR(text: String, key: String) : String = {
    
    val textBytes = text.getBytes("UTF-8")
    val keyBytes = key.getBytes("UTF-8")
    var index = 0

    val encryptedBytes = textBytes map (byte => {
      val encryptedByte = byte ^ keyBytes(index)
      index += 1
      index = index % keyBytes.length 
      encryptedByte.toByte
    })
    
    bytes2hex(encryptedBytes)
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

  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
    case None =>  bytes.map("%02x".format(_)).mkString
    case _ =>  bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }  
  
}
