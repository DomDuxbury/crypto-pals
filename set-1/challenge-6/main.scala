import java.io._
import sun.misc.{BASE64Encoder, BASE64Decoder}
import freqAnalysis._

object RepeatingXor {
  
  def main(args: Array[String]): Unit = {
    try {
      val fileInput = args(0)
      //val fileOutput = args(1)
      //val encryptKey = args(2)
      val decoder = new BASE64Decoder()

      val keysize = 2
      val score = freqAnalysis.scoreText("ETOIN")
      println(score)

      // Get input txt
      val inputText = io.Source.fromFile(fileInput).mkString
      val bytes = decoder.decodeBuffer(inputText)
      val likelyKeysizes = getLikelyKeysizes(bytes)
      likelyKeysizes map println
      //val output = repeatingXOR(plainText, encryptKey)
      //
      //// Open output file connection
      //val outputFile = new PrintWriter(new File(fileOutput)) 
      //
      //outputFile.write(output)
      //outputFile.close()
      //val plainText = io.Source.fromFile(fileInput).mkString
    } catch {
      case e : Exception => println(e)
    }
  }

  def getLikelyKeysizes(bytes : Array[Byte]) : List[Int] = {
    
    val hDistances = for {
      keysize <- 2 to 40
      (firstBytes, secondBytes) = bytes.splitAt(keysize)
      distance = hammingDistance(firstBytes, secondBytes) / keysize
    } yield (keysize, distance) 
    
    hDistances.sortWith(_._2 < _._2).take(4).map(_._1).toList 
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
    
    HexConverter.bytes2hex(encryptedBytes)
  } 
  
  def hammingDistance(bytes1: Array[Byte], bytes2: Array[Byte]): Int  = { 

    def toBinaryStr(bytes :Array[Byte]): String = 
      bytes
        .map(byte => byte.toInt.toBinaryString)
        .reduce(_ + _)

    val zippedStrings = toBinaryStr(bytes1) zip toBinaryStr(bytes2)
    zippedStrings.map(zip => zip._1 ^ zip._2).sum
  }
}

object HexConverter {

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
