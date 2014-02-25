package org.toxicblend.tests

import java.util.Random
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Option
import org.toxicblend.protobuf.ToxicBlendProtos.Vertex
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import org.toxicblend.protobuf.ToxicBlendProtos.Matrix4x4
import toxi.geom.Matrix4f
import toxi.geom.Vec3D

object Test1 {
  
  val rand = new Random(System.currentTimeMillis());
  
  def randomOption(rnd:Random) = {
    val optionBuilder = Option.newBuilder()
    optionBuilder.setKey("key" + rand.nextInt(100))
    optionBuilder.setValue("value" + rand.nextInt(100))
    optionBuilder
  }
  
  def randomVertex(rnd:Random) = {
     val vertexBuilder = Vertex.newBuilder()
     vertexBuilder.setId(rand.nextInt(100))
     vertexBuilder.setX(rand.nextFloat()*10.0f)
     vertexBuilder.setY(rand.nextFloat()*10.0f)
     vertexBuilder.setZ(rand.nextFloat()*10.0f)
     vertexBuilder
  } 
  
  def randomFace(rnd:Random) = {
     val faceBuilder = Face.newBuilder()
     faceBuilder.addVertices(rand.nextInt(100))
     faceBuilder.addVertices(rand.nextInt(100))
     faceBuilder.addVertices(rand.nextInt(100))
     faceBuilder.addVertices(rand.nextInt(100))
     faceBuilder
  }
  
  def randomWorldOrientation(rnd:Random) = {
     val matrixBuilder = Matrix4x4.newBuilder()
     matrixBuilder.setM00(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM01(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM02(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM03(rand.nextFloat()*20.f-10.f)

     matrixBuilder.setM10(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM11(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM12(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM13(rand.nextFloat()*20.f-10.f)
     
     matrixBuilder.setM20(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM21(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM22(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM23(rand.nextFloat()*20.f-10.f)

     matrixBuilder.setM30(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM31(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM32(rand.nextFloat()*20.f-10.f)
     matrixBuilder.setM33(rand.nextFloat()*20.f-10.f)
     matrixBuilder
  }
  
  def randomModel(rnd:Random, name:String) = {
    val objBuilder = Model.newBuilder()
    
    objBuilder.setName(name+rand.nextInt(100))
    objBuilder.setWorldOrientation(randomWorldOrientation(rnd))
    objBuilder.addVertices(randomVertex(rnd))
    objBuilder.addVertices(randomVertex(rnd))
    objBuilder.addVertices(randomVertex(rnd))
    objBuilder.addVertices(randomVertex(rnd))
    objBuilder.addVertices(randomVertex(rnd))
    objBuilder.addVertices(randomVertex(rnd))

    objBuilder.addFaces(randomFace(rnd))
    objBuilder.addFaces(randomFace(rnd))
    objBuilder.addFaces(randomFace(rnd))
  }
  
  def buildRandomMessage(name:String) = {
    val msgBuilder = Message.newBuilder()
    msgBuilder.setCommand("TestCommand")
    msgBuilder.addOptions(randomOption(rand))
    msgBuilder.addOptions(randomOption(rand))
    msgBuilder.addOptions(randomOption(rand))

    msgBuilder.addModels(randomModel(rand,name)) 
    msgBuilder
  }
  
  def main(args: Array[String]): Unit = {

    /*val bytes = buildRandomMessage("test").build.toByteArray
    println("Message size " + bytes.length + " bytes")
    val inMessage = Message.parseFrom(bytes)
    println(inMessage)*/
    val v = new Vec3D(1,2,3)
    val m = new Matrix4f
    m.setIdentity()
    m.set(0.1f)
    //m.rotX(1.2354f)
    val mi = new Matrix4f(m)
    mi.invert
    
    println(v)
    m.transformOne(v)
    println(v)
    mi.transformOne(v)     
    println(v)
  }
}

