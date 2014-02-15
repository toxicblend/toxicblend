package org.toxicblend.typeconverters

import toxi.geom.Matrix4x4
import org.toxicblend.protobuf.ToxicBlendProtos.{Matrix4x4=>PbMatrix4x4}
import org.toxicblend.protobuf.ToxicBlendProtos
import org.toxicblend.geometry.Matrix4x4Implicit._

class Matrix4x4Converter (val matrix:Matrix4x4) {
  /**
   * Create a packet buffer matrix from this Matrix4x4.
   */  
  def toPBModel = {
    val matrixBuilder = ToxicBlendProtos.Matrix4x4.newBuilder;

    var v = matrix.matrix(0)   
    matrixBuilder.setM00(v(0).floatValue)
    matrixBuilder.setM01(v(1).floatValue)
    matrixBuilder.setM02(v(2).floatValue)
    matrixBuilder.setM03(v(3).floatValue)

    v = matrix.matrix(1)
    matrixBuilder.setM10(v(0).floatValue)
    matrixBuilder.setM11(v(1).floatValue)
    matrixBuilder.setM12(v(2).floatValue)
    matrixBuilder.setM13(v(3).floatValue)
 
    v = matrix.matrix(2)
    matrixBuilder.setM20(v(0).floatValue)
    matrixBuilder.setM21(v(1).floatValue)
    matrixBuilder.setM22(v(2).floatValue)
    matrixBuilder.setM23(v(3).floatValue)
  
    v = matrix.matrix(3)
    matrixBuilder.setM30(v(0).floatValue)
    matrixBuilder.setM31(v(1).floatValue)
    matrixBuilder.setM32(v(2).floatValue)
    matrixBuilder.setM33(v(3).floatValue)
  }
}

object Matrix4x4Converter {
  /** 
   * Constructs from a packet buffer matrix
   * Make sure that you test .hasWorldOrientation() on the pb model before you call this,
   * or the .getMxx() methods will all default to 0 values if the matrix is absent
   */
  def apply(pbMatrix:PbMatrix4x4):Matrix4x4Converter = {
    val m = new Matrix4x4(pbMatrix.getM00(), pbMatrix.getM01(), pbMatrix.getM02(), pbMatrix.getM03(),
                          pbMatrix.getM10(), pbMatrix.getM11(), pbMatrix.getM12(), pbMatrix.getM13(),
                          pbMatrix.getM20(), pbMatrix.getM21(), pbMatrix.getM22(), pbMatrix.getM23(),
                          pbMatrix.getM30(), pbMatrix.getM31(), pbMatrix.getM32(), pbMatrix.getM33())
    new Matrix4x4Converter(m) 
  }
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbmodel:org.toxicblend.protobuf.ToxicBlendProtos.Model):Matrix4x4Converter = {
    if (pbmodel.hasWorldOrientation()) {
      Matrix4x4Converter(pbmodel.getWorldOrientation)
    } else {
      Matrix4x4Converter()
    }
  }
  
  /**
   * Constructs an identity matrix
   */
  def apply(): Matrix4x4Converter = {
    new Matrix4x4Converter(new Matrix4x4().setIdentity) 
  }
}