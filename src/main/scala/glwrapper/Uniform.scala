package glwrapper


import org.lwjgl.opengl._
import GL11._
import GL13._
import GL20._
import GL30._
import GL31._
import org.lwjgl.BufferUtils

import simplex3d.math.floatx._

import glwrapper.util.{putMat4f, sharedFloatBuffer}
import simplex3d.math._

class UniformConfig(
  val program:Program,
  val binding:Binding,
  val location:Int,
  val name:CharSequence,
  val glType:Int,
  val size:Int
)

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:43
 */

object Uniform {
  def apply(program:Program, binding:Binding, name:String, location:Int, ttype:Int, size:Int, nextSampler: () => Int) : Uniform[_] = {
    val config = new UniformConfig(
        program = program,
        binding = binding,
        name = name,
        location = location,
        glType = ttype,
        size = size
      )

      ttype match {
        case GL_SAMPLER_2D_RECT => new UniformSampler2DRect(nextSampler(), config)
        case GL_SAMPLER_1D => new UniformSampler1D(nextSampler(), config)
        case GL_SAMPLER_1D_ARRAY => new UniformSampler1DArray(nextSampler(), config)
        case GL_SAMPLER_2D => new UniformSampler2D(nextSampler(), config)
        case GL_SAMPLER_2D_ARRAY => new UniformSampler2DArray(nextSampler(), config)
        case GL_SAMPLER_CUBE => new UniformSamplerCube(nextSampler(), config)
        case GL_FLOAT => new UniformFloat(config)
        case GL_FLOAT_VEC2 => new UniformVec2f(config)
        case GL_FLOAT_VEC3 => new UniformVec3f(config)
        case GL_FLOAT_VEC4 => new UniformVec4f(config)
        case GL_FLOAT_MAT4 => new UniformMat4f(config)
        case GL_BOOL => new UniformBool(config)
        case GL_BOOL_VEC2 => new UniformBool(config)
        case GL_BOOL_VEC3 => new UniformBool(config)
        case GL_BOOL_VEC4 => new UniformBool(config)
        case _ => throw new NotImplementedError("currently not supported uniform type: "+Program.shaderTypeString(ttype) )
    }
  }
}


abstract class Uniform[T](config:UniformConfig) extends AddString {
  val program:Program   = config.program
  val binding:Binding   = config.binding
  val location:Int      = config.location
  val name:CharSequence = config.name
  val glType:Int        = config.glType
  val size:Int          = config.size

  def :=(v:T)

  def get:T

  /// stores the saved data
  def writeData()

  override def addString(sb:StringBuilder) =
    sb append s"layout(location = $location) uniform ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"

}

class UniformFake[T](program:Program, binding:Binding, _name:String) extends Uniform[T](
  new UniformConfig(program, binding, 0, _name, 0, 1)) {

  println(this)

  def := (v:T) { }
  def get = null.asInstanceOf[T]

  def writeData() {
    assert(false, "should not call this method")
  }

  override def addString( sb:StringBuilder) =
    sb append "unbound uniform " append name
}

class UniformBool(config:UniformConfig) extends Uniform[Boolean](config) {
  private[this] var data = false

  def :=(v:Boolean) {
    data = v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = glwrapper.util.sharedIntBuffer(1)
    glGetUniform(program.id, location, data) != 0
  }

  def writeData() {
    glUniform1ui(location, if(data) 1 else 0)
  }
}


class UniformVec2b(config:UniformConfig) extends Uniform[ReadVec2b](config) {
  private[this] val data = Vec2b(false)

  def :=(v:ReadVec2b) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = glwrapper.util.sharedIntBuffer(2)
    glGetUniform(program.id, location, data)
    ConstVec2b( (data get 0) != 0, (data get 1) != 0)
  }

  def writeData() {
    glUniform2ui(location, if(data.x) 1 else 0, if(data.y) 1 else 0)
  }
}

class UniformVec3b(config:UniformConfig) extends Uniform[ReadVec3b](config) {

  private[this] val data = Vec3b(false)

  def :=(v:ReadVec3b) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = glwrapper.util.sharedIntBuffer(3)
    glGetUniform(program.id, location, data)
    ConstVec3b( (data get 0) != 0, (data get 1) != 0, (data get 2) != 0)
  }

  def writeData() {
    glUniform3ui(location, if(data.x) 1 else 0, if(data.y) 1 else 0, if(data.z) 1 else 0)
  }
}

class UniformVec4b(config:UniformConfig) extends Uniform[ReadVec4b](config) {
  private[this] val data = Vec4b(false)

  def :=(v:ReadVec4b) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = glwrapper.util.sharedIntBuffer(4)
    glGetUniform(program.id, location, data)
    ConstVec4b( (data get 0) != 0, (data get 1) != 0, (data get 2) != 0, (data get 3) != 0)
  }

  def writeData() {
    glUniform4ui(location, if(data.x) 1 else 0, if(data.y) 1 else 0, if(data.z) 1 else 0, if(data.w) 1 else 0)
  }
}



class UniformFloat(config:UniformConfig) extends Uniform[Float](config) {
  private[this] var data:Float = 0

  def :=(v:Float) {
    data = v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(1)
    glGetUniform(program.id, location, data)
    data get 0
  }

  def writeData() {
    glUniform1f(location, data)
  }
}

class UniformVec2f(config:UniformConfig) extends Uniform[ReadVec2f](config) {
  private[this] val data = Vec2f(0)

  def :=(v:ReadVec2f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(2)
    glGetUniform(program.id, location, data)
    ConstVec2f(data get 0, data get 1)
  }

  def writeData() {
    glUniform2f(location, data.x, data.y)
  }
}

class UniformVec3f(config:UniformConfig) extends Uniform[ReadVec3f](config) {

  private[this] val data = Vec3f(0)

  def :=(v:ReadVec3f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(3)
    glGetUniform(program.id, location, data)
    ConstVec3f(data get 0, data get 1, data get 2)
  }

  def writeData() {
    glUniform3f(location, data.x, data.y, data.z)
  }
}

class UniformVec4f(config:UniformConfig) extends Uniform[ReadVec4f](config) {
  private[this] val data = Vec4f(0)

  def :=(v:ReadVec4f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(4)
    glGetUniform(program.id, location, data)
    ConstVec4f(data get 0, data get 1, data get 2, data get 3)
  }

  def writeData() {
    glUniform4f(location, data.x.toFloat, data.y.toFloat, data.z.toFloat, data.w.toFloat)
  }
}

abstract class UniformSampler[T <: Texture](config:UniformConfig) extends Uniform[T](config) {
  def position:Int
  protected var texture:T = _

  def := (v:T) {
    texture = v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniform1i(location, position)
    glActiveTexture(GL_TEXTURE0 + position)
    texture.bind()
  }

  def get = ???

}

class UniformSampler1D(val position:Int, config:UniformConfig) extends UniformSampler[Texture1D](config)
class UniformSampler1DArray(val position:Int, config:UniformConfig) extends UniformSampler[Texture1DArray](config)
class UniformSampler2D(val position:Int, config:UniformConfig) extends UniformSampler[Texture2D](config)
class UniformSampler2DArray(val position:Int, config:UniformConfig) extends UniformSampler[Texture2DArray](config)
class UniformSampler3D(val position:Int, config:UniformConfig) extends UniformSampler[Texture3D](config)
class UniformSamplerCube(val position:Int, config:UniformConfig) extends UniformSampler[TextureCube](config)
class UniformSampler2DRect(val position:Int, config:UniformConfig) extends UniformSampler[TextureRectangle](config)

class UniformMat4f(config:UniformConfig) extends Uniform[ReadMat4f](config) {
  val buffer = BufferUtils.createFloatBuffer(16)

  def :=(m:ReadMat4f) {
    putMat4f(buffer,m)
    buffer.flip()
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(16)
    glGetUniform(program.id, location, data)
    ConstMat4f(data get  0, data get  1, data get  2, data get   3,
               data get  4, data get  5, data get  6, data get   7,
               data get  8, data get  9, data get 10, data get  11,
               data get 12, data get 13, data get 14, data get  15)
  }

  def writeData() {
    glUniformMatrix4(location, false, buffer)
  }
}
