package org.toxicblend

import java.io.IOException
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import java.net.SocketException
import java.nio.charset.Charset
import java.nio.ByteBuffer
import com.google.protobuf.{CodedInputStream,CodedOutputStream}
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model

object Server {
  
  def main(args: Array[String]): Unit = {
    val port = 9999
    try {
      val listener = new ServerSocket(port);
      println("Started service, listening to port " + port );
      while (true) {
        val aSocket:Socket = listener.accept()
        println("Opening connection to " + aSocket.getPort() + "->" + aSocket.getLocalPort());
        new ServerThread(aSocket).start()
      }
      listener.close()
      System.err.println("Closing connection to ." + listener.getLocalSocketAddress());
    }
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: " + port);
        System.exit(-1)
    }
  }
}