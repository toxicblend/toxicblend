package org.toxicblend.util

object Managed {
  /**
   * from http://stackoverflow.com/questions/17627599/using-a-variable-in-finally-block
   */
  def managed[T <: AutoCloseable](resource:T) = new Traversable[T] {
    def foreach[U](f:T=>U) {
      try {
        f(resource)
      } finally {
        resource.close()
      }
    }
  }
}