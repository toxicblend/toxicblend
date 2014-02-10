package org.toxicblend

import java.nio.charset.Charset
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random
import java.io.{DataOutputStream,DataInputStream,EOFException,IOException}

import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.{Option => MessageOption}
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import org.toxicblend.operations.volumetricrender.VolumetricRenderProcessor
import org.toxicblend.operations.projectionoutline.ProjectionOutlineProcessor
import org.toxicblend.operations.boostmedianaxis.MedianAxisProcessor
import org.toxicblend.operations.dragoncurve.DragonCurveProcessor
import org.toxicblend.operations.boostsimplify.BoostSimplify
import org.toxicblend.operations.simplegcodegenerator.SimpleGcodeGeneratorOperation
import org.toxicblend.operations.simplegcodeparse.SimpleGcodeParseOperation
import org.toxicblend.operations.saveobj.SaveObjOperation

import com.google.protobuf.{CodedInputStream,CodedOutputStream}
import toxi.geom.AABB
import toxi.geom.Vec3D

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
              case "OBJECT_OT_toxicblend_simplegcodegenerator" => new SimpleGcodeGeneratorOperation     
              case "OBJECT_OT_toxicblend_simplegcodeviewer" => new SimpleGcodeParseOperation
              case "OBJECT_OT_toxicblend_saveobj" => new SaveObjOperation
              case s:String => System.err.println("Unknown command: " + s); new EchoProcessor
            }
            try {
              processor.processInput(inMessage)
            } catch {
              case e:Exception => {
                val message = Message.newBuilder()
                val optionBuilder = MessageOption.newBuilder()
                optionBuilder.setKey("ERROR")
                optionBuilder.setValue(e.toString)
                message.addOptions(optionBuilder)
                e.printStackTrace()
                message
              }
            }
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