package org.toxicblend

import java.io.IOException
import java.net.ServerSocket
import java.net.Socket
import java.io.DataInputStream
import java.net.SocketTimeoutException
import akka.actor.ActorSystem

/**
 * The main class for the toxic blend server
 */
object ToxicblendServer {

  class SocketListener(val actorSystem: ActorSystem, val port: Int) extends Thread("SocketListener") {
    var quit = false
    var isRunning = true

    override def run(): Unit = {
      try {
        val socket: ServerSocket = new ServerSocket(port)
        socket.setSoTimeout(1000)
        println("Started service, listening to port " + port)
        while (!quit) {
          try {
            val aSocket: Socket = socket.accept
            aSocket.setSoTimeout(0)
            println("Opening connection to " + aSocket.getRemoteSocketAddress + "->" + aSocket.getLocalPort)
            new ServerThread(actorSystem, aSocket).start
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
    val keyboard = new DataInputStream(System.in);
    val system = ActorSystem("ToxicBlend")

    val socketListener = new SocketListener(system, port)
    socketListener.start
    var inputChar = ' '
    while (socketListener.isRunning && inputChar != 'q' && inputChar != 'Q') {
      inputChar = keyboard.readByte.toChar
    }
    if (socketListener.isRunning) {
      println("Existing, waiting for socket threads to terminate")
      socketListener.quit = true
      socketListener.join
    }
  }
}