/*******************************************************************************
 * Copyright 2011 See AUTHORS file. (http://libgdx.badlogicgames.com)
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package org.toxicblend.util

import scala.collection.mutable.HashSet
import java.io.File
import java.io.FileOutputStream;
import org.toxicblend.ToxicblendException

/*
 * Cherry picked and minimal Scala port of com.badlogic.gdx.utils.SharedLibraryLoader 
 * http://libgdx.badlogicgames.com
 */
object SharedLibraryLoader {
  
  val isWindows = System.getProperty("os.name").contains("Windows")
  val isLinux = System.getProperty("os.name").contains("Linux")
  val isMac = System.getProperty("os.name").contains("Mac")
  val isARM = System.getProperty("os.arch").startsWith("arm")
  val is64Bit = System.getProperty("os.arch").equals("amd64")
  // JDK 8 only.
  val abi = if (System.getProperty("sun.arch.abi") != null) System.getProperty("sun.arch.abi")  else ""
  
  protected val loadedLibraries = new HashSet[String]
  
  /** 
   * Maps a platform independent library name to a platform dependent name. 
   */
  protected def mapLibraryName (libraryName:String) :String = {
    if (isWindows) libraryName + { if(is64Bit) "64.dll" else ".dll" }
    else if (isLinux) "lib" + libraryName + {if (isARM) "arm" + abi else ""} + {if(is64Bit) "64.so" else ".so"}
    else if (isMac) "lib" + libraryName + ".dylib"
    else libraryName
  }
  
  /**
   * Extracts a named file from the classpath into a temp file
   * Does System.load on that file 
   */
  protected def extractAndLoad(libraryName:String) {
    val inputStream = SharedLibraryLoader.getClass.getResourceAsStream("/" + libraryName)
    if (inputStream!=null) {
      val file = File.createTempFile(libraryName, ".jni")
      val outputStream = new FileOutputStream(file)
      
      val buffer = new Array[Byte](4096)
      var length = -1
      do {
        length = inputStream.read(buffer)
        if (length != -1)
          outputStream.write(buffer, 0, length)
      } while (length != -1)
      inputStream.close()
      outputStream.close()
      System.load(file.getAbsolutePath)
    } else {
      throw new ToxicblendException("Could not find " + libraryName  + " in classpath")
    }
  }
  
  /** 
   * Loads a shared library for the platform the application is running on.
   * @param libraryName The platform independent library name. If not contain a prefix (eg lib) or suffix (eg .dll). */
  def load(inLibraryName:String):Unit = this.synchronized {
    val libraryName = mapLibraryName(inLibraryName)
    if (!loadedLibraries.contains(libraryName)) {
      extractAndLoad(libraryName)
      loadedLibraries.add(libraryName);
    }
  }
}