package org.toxicblend

import java.nio.charset.Charset
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import java.io.{DataOutputStream,DataInputStream,EOFException,IOException}
import com.google.protobuf.{CodedInputStream,CodedOutputStream}
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import toxi.geom.AABB
import toxi.geom.Vec3D
import toxi.geom.AABB
import org.toxicblend.operations.volumetricrender.VolumetricRenderProcessor
import org.toxicblend.operations.projectionoutline.ProjectionOutlineProcessor
import org.toxicblend.operations.boostmedianaxis.MedianAxisProcessor
import org.toxicblend.operations.dragoncurve.DragonCurveProcessor
import org.toxicblend.operations.boostsimplify.BoostSimplify
import org.toxicblend.operations.simplegcode.SimpleGcodeOperation
import org.toxicblend.operations.gcodeparse.ParseGcodeOperation
import org.toxicblend.operations.saveobj.SaveObjOperation

object ServerThread {
    private val CHARSET = Charset.forName("UTF-8")
}

case class ServerThread(socket: Socket) extends Thread("ServerThread") {
  
  override def run(): Unit = {
    val rand = new Random(System.currentTimeMillis());
    var commandsReceived = 0 // the number of messages sent from the client on this specific connection
    
    try {
      
      val out = new DataOutputStream(socket.getOutputStream())
      val in = new DataInputStream(socket.getInputStream())
      var inputData:Array[Byte] = new Array[Byte](100)
      
      while (true) {
        do {
          // Receive data from client
          val inLength = in.readInt()
          if (inLength > inputData.length) {
            inputData = new Array[Byte](inLength)
          }
          
          in.readFully(inputData, 0, inLength)
          val codedInputStream = CodedInputStream.newInstance(inputData,0,inLength);
          val inMessage = Message.parseFrom(codedInputStream)
          println("Server received command: \"" + inMessage.getCommand() + "\" of " + inLength + " bytes." )
          //println(inMessage)
          
          val outMessage = {
            val processor:CommandProcessorTrait = inMessage.getCommand() match {
              case "OBJECT_OT_toxicblend_volume" => new VolumetricRenderProcessor
              case "OBJECT_OT_toxicblend_add_dragon_curve" => new DragonCurveProcessor
              case "OBJECT_OT_toxicblend_projection_outline" => new ProjectionOutlineProcessor
              case "OBJECT_OT_toxicblend_medianaxis" => new MedianAxisProcessor
              case "OBJECT_OT_toxicblend_boostsimplify" => new BoostSimplify
              case "OBJECT_OT_toxicblend_simplegcodegenerator" => new SimpleGcodeOperation     
              case "OBJECT_OT_toxicblend_simplegcodeviewer" => new ParseGcodeOperation
              case "OBJECT_OT_toxicblend_saveobj" => new SaveObjOperation
              case s:String => System.err.println("Unknown command: " + s); new EchoProcessor
            }
            processor.processInput(inMessage)
          }
          Option(outMessage.getCommand()).foreach(_ => outMessage.setCommand("None"))
          val output = outMessage.build.toByteArray()
 
          out.writeInt(output.length)
          out.write(output)
          out.flush()
          println("Response sent: " + output.length + " bytes")
          commandsReceived += 1
        } while (true)
          
        Thread.sleep(100)
      }
      out.close()
      in.close()
      socket.close()
    }
    catch {
      case e: SocketException =>
        () // avoid stack trace when stopping a client with Ctrl-C
      case e: EOFException =>
        if (commandsReceived==0){
           e.printStackTrace();
        }
        println("Closing connection to " + socket.getPort() + "->" + socket.getLocalPort());
      case e: IOException =>
        e.printStackTrace();
    }
  }
}