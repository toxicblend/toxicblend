package org.toxicblend.typeconverters

import javax.vecmath.Matrix4d
import org.toxicblend.protobuf.ToxicBlendProtos.Matrix4x4
import org.toxicblend.protobuf.ToxicBlendProtos

class Matrix4dConverter (val matrix:Matrix4d) {
  /**
   * Create a packet buffer matrix from this Matrix4x4.
   */  
  def toPBModel = {
    val matrixBuilder = ToxicBlendProtos.Matrix4x4.newBuilder;

    matrixBuilder.setM00(matrix.m00.toFloat)
    matrixBuilder.setM01(matrix.m01.toFloat)
    matrixBuilder.setM02(matrix.m02.toFloat)
    matrixBuilder.setM03(matrix.m03.toFloat)

    matrixBuilder.setM10(matrix.m10.toFloat)
    matrixBuilder.setM11(matrix.m11.toFloat)
    matrixBuilder.setM12(matrix.m12.toFloat)
    matrixBuilder.setM13(matrix.m13.toFloat)
 
    matrixBuilder.setM20(matrix.m20.toFloat)
    matrixBuilder.setM21(matrix.m21.toFloat)
    matrixBuilder.setM22(matrix.m22.toFloat)
    matrixBuilder.setM23(matrix.m23.toFloat)
  
    matrixBuilder.setM30(matrix.m30.toFloat)
    matrixBuilder.setM31(matrix.m31.toFloat)
    matrixBuilder.setM32(matrix.m32.toFloat)
    matrixBuilder.setM33(matrix.m33.toFloat)
  }
}

object Matrix4dConverter {
  /** 
   * Constructs from a packet buffer matrix
   * Make sure that you test .hasWorldOrientation() on the pb model before you call this,
   * or the .getMxx() methods will all default to 0 values if the matrix is absent
   */
  def apply(pbMatrix:Matrix4x4):Matrix4dConverter = {
    val m = new Matrix4d(pbMatrix.getM00(), pbMatrix.getM01(), pbMatrix.getM02(), pbMatrix.getM03(),
                         pbMatrix.getM10(), pbMatrix.getM11(), pbMatrix.getM12(), pbMatrix.getM13(),
                         pbMatrix.getM20(), pbMatrix.getM21(), pbMatrix.getM22(), pbMatrix.getM23(),
                         pbMatrix.getM30(), pbMatrix.getM31(), pbMatrix.getM32(), pbMatrix.getM33())
    new Matrix4dConverter(m) 
  }
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbmodel:org.toxicblend.protobuf.ToxicBlendProtos.Model):Matrix4dConverter = {
    if (pbmodel.hasWorldOrientation()) {
      Matrix4dConverter(pbmodel.getWorldOrientation)
    } else {
      Matrix4dConverter()
    }
  }
  
  /**
   * Constructs an identity matrix
   */
  def apply(): Matrix4dConverter = {
    val m = new Matrix4d
    m.setIdentity
    new Matrix4dConverter(m) 
  }
}