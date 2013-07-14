package glwrapper

import java.nio.ByteBuffer
import simplex3d.math.floatx.{Vec2f, Vec3f, Vec4f}
import simplex3d.math._
import org.lwjgl.opengl._

/**
 * User: arne
 * Date: 12.07.13
 * Time: 15:52
 */

import GL11._
import GL20._

object ShaderData {

  implicit object Vec4fIsShaderData extends ShaderData[Vec4f] {
    def put(data:ByteBuffer, value:Vec4f) = util.putVec4f(data, value)
    def get(data:ByteBuffer, orig:Vec4f):Vec4f = util.getVec4f(data, orig)
    def uniform(location:Int, value: Vec4f) = glUniform4f(location, value.x, value.y, value.z, value.w)
    def assign(target:Vec4f, value:Vec4f) = {target := value; target}
    def default = Vec4f(0)
    val byteStride:Int = util.sizeOf[Vec4f]
  }

  implicit object Vec3fIsShaderData extends ShaderData[Vec3f] {
    def put(data:ByteBuffer, value:Vec3f)       = util.putVec3f(data, value)
    def get(data:ByteBuffer, value:Vec3f):Vec3f = util.getVec3f(data, value)
    def uniform(location:Int, value: Vec3f) = glUniform3f(location, value.x, value.y, value.z)
    def assign(target:Vec3f, value:Vec3f) = {target := value; target}
    def default = Vec3f(0)
    val byteStride:Int = util.sizeOf[Vec3f]
  }

  implicit object Vec2fIsShaderData extends ShaderData[Vec2f] {
    def put(data:ByteBuffer, value:Vec2f) = util.putVec2f(data, value)
    def get(data:ByteBuffer, value:Vec2f):Vec2f = util.getVec2f(data, value)
    def uniform(location:Int, value: Vec2f) = glUniform2f(location, value.x, value.y)
    def assign(target:Vec2f, value:Vec2f) = {target := value; target}
    def default = Vec2f(0)
    val byteStride:Int = util.sizeOf[Vec2f]
  }

  implicit object FloatIsShaderData extends ShaderData[Float] {
    def put(data:ByteBuffer, value:Float) = data.putFloat(value)
    def get(data:ByteBuffer, value:Float) = data.getFloat
    def uniform(location:Int, value: Float) = glUniform1f(location, value)
    def assign(target:Float, value:Float) = value
    def default = 0
    val byteStride:Int = util.sizeOf[Float]
  }

  implicit object Vec4iIsShaderData extends ShaderData[Vec4i] {
    def put(data:ByteBuffer, value:Vec4i) = util.putVec4i(data, value)
    def get(data:ByteBuffer, orig:Vec4i):Vec4i = util.getVec4i(data, orig)
    def uniform(location:Int, value: Vec4i) = glUniform4i(location, value.x, value.y, value.z, value.w)
    def assign(target:Vec4i, value:Vec4i) = {target := value; target}
    def default = Vec4i(0)
    val byteStride:Int = util.sizeOf[Vec4i]
  }

  implicit object Vec3iIsShaderData extends ShaderData[Vec3i] {
    def put(data:ByteBuffer, value:Vec3i)       = util.putVec3i(data, value)
    def get(data:ByteBuffer, value:Vec3i):Vec3i = util.getVec3i(data, value)
    def uniform(location:Int, value: Vec3i) = glUniform3i(location, value.x, value.y, value.z)
    def assign(target:Vec3i, value:Vec3i) = {target := value; target}
    def default = Vec3i(0)
    val byteStride:Int = util.sizeOf[Vec3i]
  }

  implicit object Vec2iIsShaderData extends ShaderData[Vec2i] {
    def put(data:ByteBuffer, value:Vec2i) = util.putVec2i(data, value)
    def get(data:ByteBuffer, value:Vec2i):Vec2i = util.getVec2i(data, value)
    def uniform(location:Int, value: Vec2i) = glUniform2i(location, value.x, value.y)
    def assign(target:Vec2i, value:Vec2i) = {target := value; target}
    def default = Vec2i(0)
    val byteStride:Int = util.sizeOf[Vec2i]
  }

  implicit object IntIsShaderData extends ShaderData[Int] {
    def put(data:ByteBuffer, value:Int) = data.putInt(value)
    def get(data:ByteBuffer, value:Int) = data.getInt
    def uniform(location:Int, value: Int) = glUniform1i(location, value)
    def assign(target:Int, value:Int) = value
    def default = 0
    val byteStride:Int = util.sizeOf[Int]
  }

  def int(b:Boolean) = if(b) GL_TRUE else GL_FALSE
  def byte(b:Boolean):Byte  = if(b) 1 else 0

  implicit object Vec4bIsShaderData extends ShaderData[Vec4b] {
    def put(data:ByteBuffer, value:Vec4b) = util.putVec4b(data, value)
    def get(data:ByteBuffer, value:Vec4b):Vec4b = util.getVec4b(data, value)
    def uniform(location:Int, value: Vec4b) = glUniform4i(location, int(value.x), int(value.y), int(value.z), int(value.w))
    def assign(target:Vec4b, value:Vec4b) = {target := value; target}
    def default = Vec4b(false)
    val byteStride:Int = util.sizeOf[Vec4b]
  }

  implicit object Vec3bIsShaderData extends ShaderData[Vec3b] {
    def put(data:ByteBuffer, value:Vec3b)       = util.putVec3b(data, value)
    def get(data:ByteBuffer, value:Vec3b):Vec3b = util.getVec3b(data, value)
    def uniform(location:Int, value: Vec3b) = glUniform3i(location, int(value.x), int(value.y), int(value.z))
    def assign(target:Vec3b, value:Vec3b) = {target := value; target}
    def default = Vec3b(false)
    val byteStride:Int = util.sizeOf[Vec3b]
  }

  implicit object Vec2bIsShaderData extends ShaderData[Vec2b] {
    def put(data:ByteBuffer, value:Vec2b) = util.putVec2b(data, value)
    def get(data:ByteBuffer, value:Vec2b):Vec2b = util.getVec2b(data, value)
    def uniform(location:Int, value: Vec2b) = glUniform2i(location, int(value.x), int(value.y))
    def assign(target:Vec2b, value:Vec2b) = {target := value; target}
    def default = Vec2b(false)
    val byteStride:Int = util.sizeOf[Vec2b]
  }

  implicit object BoolIsShaderData extends ShaderData[Boolean] {
    def put(data:ByteBuffer, value:Boolean) = data.put(byte(value))
    def get(data:ByteBuffer, value:Boolean) = data.get == GL_TRUE
    def uniform(location:Int, value: Boolean) = glUniform1i(location, int(value))
    def assign(target:Boolean, value:Boolean) = value
    def default = false
    val byteStride:Int = util.sizeOf[Boolean]
  }
}

trait ShaderData[T] {
  def put(data:ByteBuffer, value:T)
  def get(data:ByteBuffer, orig:T):T
  def assign(target:T, value:T):T
  def uniform(location:Int, value: T)
  def byteStride:Int
  def default:T

  implicit class Ops(lhs:ByteBuffer) {
    def putT(rhs:T):Unit = put(lhs,rhs)
  }

}
