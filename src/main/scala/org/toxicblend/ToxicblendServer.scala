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
import java.util.Scanner
import java.io.DataInputStream
import java.net.SocketTimeoutException 

object Server {
  class SocketListener(val port:Int) extends Thread("SocketListener") {
    var quit = false
    var isRunning = true
   
    override def run(): Unit = {
      try {
        val socket:ServerSocket = new ServerSocket(port)
        socket.setSoTimeout(1000)
        println("Started service, listening to port " + port)
        while (!quit) {
          try {
            val aSocket:Socket = socket.accept
            aSocket.setSoTimeout(0)
            println("Opening connection to " + aSocket.getPort() + "->" + aSocket.getLocalPort)
            new ServerThread(aSocket).start 
          } catch {
            case e: SocketTimeoutException => // println("ignoring " + e)
          }
        }
        if (!socket.getLocalSocketAddress.toString.startsWith("0.0.0.0/0.0.0.0")) {
          println("Closing connection to ." + socket.getLocalSocketAddress)
        }
        socket.close
      } catch {
        case e: IOException =>
          System.err.println("Could not listen on port: " + port);
      }
      isRunning = false
    }
  }
  
  def main(args: Array[String]): Unit = {
    val port = 9999
    //val keyboard = new Scanner(System.in)
    val keyboard=new DataInputStream(System.in);
    
    val socketListener = new SocketListener(port)
    socketListener.start
    var inputChar= ' '
    while (socketListener.isRunning && inputChar!='q' && inputChar!='Q' ) {      
      inputChar = keyboard.readByte.toChar
    }
    if (socketListener.isRunning ) {
      println("Existing, waiting for socket threads to terminate")
      socketListener.quit = true
      socketListener.join
    }
  }
}