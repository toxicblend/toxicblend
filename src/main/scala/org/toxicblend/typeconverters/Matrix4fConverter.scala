package org.toxicblend.typeconverters

import toxi.geom.Matrix4f
import org.toxicblend.protobuf.ToxicBlendProtos.Matrix4x4
import org.toxicblend.protobuf.ToxicBlendProtos

class Matrix4fConverter (val matrix:Matrix4f) {
  /**
   * Create a packet buffer matrix from this Matrix4f.
   */  
  def toPBModel = {
    val matrixBuilder = ToxicBlendProtos.Matrix4x4.newBuilder()
    matrixBuilder.setM00(matrix.getM00())
    matrixBuilder.setM01(matrix.getM01())
    matrixBuilder.setM02(matrix.getM02())
    matrixBuilder.setM03(matrix.getM03())
    
    matrixBuilder.setM10(matrix.getM10())
    matrixBuilder.setM11(matrix.getM11())
    matrixBuilder.setM12(matrix.getM12())
    matrixBuilder.setM13(matrix.getM13())
    
    matrixBuilder.setM20(matrix.getM20())
    matrixBuilder.setM21(matrix.getM21())
    matrixBuilder.setM22(matrix.getM22())
    matrixBuilder.setM23(matrix.getM23())
    
    matrixBuilder.setM30(matrix.getM30())
    matrixBuilder.setM31(matrix.getM31())
    matrixBuilder.setM32(matrix.getM32())
    matrixBuilder.setM33(matrix.getM33())
  }
}

object Matrix4fConverter {
  /** 
   * Constructs from a packet buffer matrix
   * Make sure that you test .hasWorldOrientation() on the pb model before you call this,
   * or the .getMxx() methods will all default to 0 values if the matrix is absent
   */
  def apply(pbMatrix:Matrix4x4):Matrix4fConverter = {
    val m = new Matrix4f(pbMatrix.getM00(), pbMatrix.getM01(), pbMatrix.getM02(), pbMatrix.getM03(),
                         pbMatrix.getM10(), pbMatrix.getM11(), pbMatrix.getM12(), pbMatrix.getM13(),
                         pbMatrix.getM20(), pbMatrix.getM21(), pbMatrix.getM22(), pbMatrix.getM23(),
                         pbMatrix.getM30(), pbMatrix.getM31(), pbMatrix.getM32(), pbMatrix.getM33())
    new Matrix4fConverter(m) 
  }
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbmodel:org.toxicblend.protobuf.ToxicBlendProtos.Model):Matrix4fConverter = {
    if (pbmodel.hasWorldOrientation()) {
      Matrix4fConverter(pbmodel.getWorldOrientation)
    } else {
      Matrix4fConverter()
    }
  }
  
  /**
   * Constructs an identity matrix
   */
  def apply(): Matrix4fConverter = {
    val m = new Matrix4f()
    m.setIdentity()
    new Matrix4fConverter(m) 
  }
}