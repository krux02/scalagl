package glwrapper

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import java.io.{InputStreamReader, InputStream}
import scala.reflect.ClassTag

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:52
 */

object Shader {
  def apply[T <: Shader](strShaderFile:InputStream )(implicit t:ClassTag[T]):T = {
    val reader = new InputStreamReader( strShaderFile )
    val buffsize = 100000
    val data = new Array[Char](buffsize)
    val result = reader.read(data)

    assert(result < buffsize, "buffersize too small for shader")

    val shader = t.runtimeClass.getConstructor().newInstance().asInstanceOf[T]

    shader.create()
    shader.source = data
    shader.compile()
    shader
  }

  def apply[T <: Shader:ClassTag](name:String):T = {
    apply( getClass.getClassLoader.getResourceAsStream("shaders/" + name) )
  }
}

class ShaderCompileError(message:String) extends Exception(message)
class ShaderLinkError(message:String) extends Exception(message)

abstract class Shader {
  var id = 0

  def create() {
    id = glCreateShader(shaderType)
  }

  def delete() {
    glDeleteShader(id)
    id = 0
  }

  def shaderType:Int

  def compile() {
    assert(id != 0)
    glCompileShader(id)
    checkCompileStatus()
  }

  def source = ???

  def source_=(src: CharSequence) {
    glShaderSource(id, src)
  }

  def checkCompileStatus() {
    val status = glGetShaderi(id, GL_COMPILE_STATUS)
    val infoLogLength = glGetShaderi(id, GL_INFO_LOG_LENGTH)
    val strInfoLog = if( infoLogLength > 0 ) glGetShaderInfoLog(id, infoLogLength) else ""
    if( infoLogLength > 0 ){
      println(strInfoLog)
    }
    if (status == GL_FALSE)
    {
      val strShaderType = shaderType match {
        case GL_VERTEX_SHADER   => "vertex"
        //      case GL_GEOMETRY_SHADER => "geometry"
        case GL_FRAGMENT_SHADER => "fragment"
      }

      throw new ShaderCompileError(s"$strShaderType shader:\n$strInfoLog\n")
    }
  }

  override def finalize() {
    if(id != 0)
      delete()
  }
}

class VertexShader extends Shader {
  def shaderType = GL_VERTEX_SHADER
}

class FragmentShader extends Shader {
  def shaderType = GL_FRAGMENT_SHADER
}
