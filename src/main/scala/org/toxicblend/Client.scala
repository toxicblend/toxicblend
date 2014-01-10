package org.toxicblend

import java.io.IOException
import java.io.DataOutputStream
import java.io.DataInputStream
import java.net.{InetAddress,Socket,SocketException}
import java.util.Random
import java.nio.charset.Charset
import com.google.protobuf.{CodedInputStream,CodedOutputStream}
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import org.toxicblend.tests.Test1

/** 
 * Simple client/server application using Java sockets. 
 * 
 * The server simply generates random integer values and 
 * the clients provide a filter function to the server 
 * to get only values they interested in (eg. even or 
 * odd values, and so on). 
 */
object Client {
  private val charset = Charset.forName("UTF-8")

  def main(args: Array[String]) {
    try {
      val rand = new Random(System.currentTimeMillis());
      val stringData = "Client Test data, a lot of test data. U getting this?? ????????????1234567890"
      //val byteData = stringData.getBytes(charset)
      val ia = InetAddress.getByName("localhost")
      val socket = new Socket(ia, 9999)
      val out = new DataOutputStream(socket.getOutputStream())
      val in = new DataInputStream(socket.getInputStream())
      var inputData:Array[Byte] = new Array[Byte](100)

      for (i <- 0 until 2) {
        
        // Send data to server
        val output = Test1.buildRandomMessage("clientSent").build().toByteArray()
        
        out.writeInt(output.length)
        out.write(output)
        out.flush()
        println("Client sent: " + output.length + " bytes")
        
        // Receive data from server
        val inLength = in.readInt()
        if (inLength > inputData.length) {
          inputData = new Array[Byte](inLength)
        }
        in.readFully(inputData, 0, inLength)
        val codedInputStream = CodedInputStream.newInstance(inputData,0,inLength);
        val inMessage = Message.parseFrom(codedInputStream)
        println("Client received: " + inLength + " bytes ")
        println(inMessage)
        println("End Client received: " )
        
      }
      out.close()
      in.close()
      socket.close()
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

