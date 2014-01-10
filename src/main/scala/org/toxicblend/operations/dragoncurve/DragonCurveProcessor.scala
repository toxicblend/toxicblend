package org.toxicblend.operations.dragoncurve

import org.toxicblend.CommandProcessorTrait
import scala.collection.mutable.ArrayBuffer
import toxi.geom.{Vec3D,LineStrip3D}
import scala.collection.JavaConversions._
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.Regex

class DragonCurveProcessor extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    val iterations = options.getOrElse("iterations", "9") match {
      case Regex.INT_REGEX(limit) => limit.toInt
      case s:String => System.err.println("BoostSimplify: unrecognizable 'iterations' property value: " +  s ); 9
    }
    val edgeLength = options.getOrElse("edgeLength", "1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("BoostSimplify: unrecognizable 'edgeLength' property value: " +  s ); 1f
    }
    //println(options.options)
    val outMessage = Message.newBuilder()
    outMessage.addModels(generate(iterations, edgeLength))
    outMessage
  }
  
  def generate(iterations:Int, edgeLength:Float) = {
    val dragon = DragonCurveProcessor.draw(DragonCurveProcessor.generateData(iterations), edgeLength)
    val model = LineStripConverter(dragon,"dragon curve") 
    model.center().toPBModel(false)
  }
}

object DragonCurveProcessor {
  /**
   * start with original string="1"
   * add 1 to the right hand side of original string
   * add the original string with the middle digit reversed to get the new string
   * original string = new string
   * repeat
   */
  def generateData(folds:Int) = {
    var a = new ArrayBuffer[Boolean];
    a.append(true)
    for(count <- 0 until folds ){
      val b=a.slice(0,a.length/2);
      val x=a.slice(a.length/2+1,a.length);
      val z= !a(a.length/2)
      a.append(true)
      a ++= b
      a += z
      a ++= x
    }
    a
  }
  
  def draw(data:ArrayBuffer[Boolean], edgeLength:Float) = {
    val rv = new LineStrip3D
    var x1 = 0f;
    var y1 = 0f;
    var x2 = 0f;
    var y2 = 1f;
    var x3 = 0f;
    var y3 = 1f;
    
    // read through generator string and
    // move left or right according to each digit
    // and current direction

    data.foreach(z => {
      if((y2-y1 ==  1) && !z){ x3=x2-1 }
      if((y2-y1 == -1) && !z){ x3=x2+1;}
      if((x2-x1 ==  1) && !z){ y3=y2+1;}
      if((x2-x1 == -1) && !z){ y3=y2-1;}
      if((y2-y1 ==  1) &&  z){ x3=x2+1;}
      if((y2-y1 == -1) &&  z){ x3=x2-1;}
      if((x2-x1 ==  1) &&  z){ y3=y2-1;}
      if((x2-x1 == -1) &&  z){ y3=y2+1;}
      rv.add(x3*edgeLength,y3*edgeLength, 0f)
      x1=x2
      y1=y2
      x2=x3
      y2=y3
    })
    rv
  }
  
  def println(ls:LineStrip3D) {
    ls.getVertices().foreach(v => print(v))
      System.out.println("")
  }
  
  def main(args: Array[String]): Unit = {
   println(draw(generateData(1), 1.0f))
   //println(draw(generateData(2)))
   val m1=LineStripConverter(Array(draw(generateData(1), 1.0f)))
   val pbm=m1.toPBModel(false).build()
   val m2=LineStripConverter(pbm)
   m1.println
   m2.println
   m2.lineStrips.foreach(println)
  }
}