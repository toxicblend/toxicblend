package org.toxicblend.typeconverters

import org.toxicblend.operations.simplegcodeparse.GcodeLines
import org.toxicblend.protobuf.ToxicBlendProtos.Message.Builder
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem

object GCodeConverter {
  
  def writeGCode(parsed:GcodeLines, options:OptionConverter, returnMessageBuilder:Builder) {
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("GCodeConverter: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("GCodeConverter 'unitSystem' property value: " +  s ); None
    }
    if (unitIsMetric != UnitSystem.Metric) {
      System.err.println("GCodeConverter::processInput only metric is supported for now");
    }
    val segments = parsed.getSegments(0.001f/unitScale) // convert mm to meter
    val splitG0andG1 = true
    
    val g0Model = if (splitG0andG1) new Mesh3DConverter(name="GCODE-G0") else new Mesh3DConverter(name="GCODE")
    val g1Model = if (splitG0andG1) new Mesh3DConverter(name="GCODE-G1") else g0Model
    
    segments.foreach(segment => {
      segment.command match {
        case "G0" => g0Model.addEdges(segment.p0, segment.p1)
        case "G1" => g1Model.addEdges(segment.p0, segment.p1)
        case _  => // ignore
       }
    })
    returnMessageBuilder.addModels(g0Model.toPBModel(None,None))
    if (splitG0andG1) 
      returnMessageBuilder.addModels(g1Model.toPBModel(None,None)) 
  }
}