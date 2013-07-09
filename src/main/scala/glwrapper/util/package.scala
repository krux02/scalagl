package glwrapper


import org.lwjgl.BufferUtils
import java.nio.{ByteBuffer, IntBuffer, FloatBuffer}

import simplex3d.math.float._
import simplex3d.math.floatx._

import simplex3d.math.double._
import simplex3d.math.doublex._

trait AddString {
  override def toString = addString(new StringBuilder).result

  def addString(sb:StringBuilder):StringBuilder
}

package object util {

  private[this] val sharedFloatBufferInstance = new ThreadLocal[FloatBuffer]{ override def initialValue = BufferUtils.createFloatBuffer(16)}
  private[this] val sharedIntBufferInstance = new ThreadLocal[IntBuffer]{ override def initialValue = BufferUtils.createIntBuffer(16)}
  private[this] val sharedByteBufferInstance = new ThreadLocal[ByteBuffer]{ override def initialValue = BufferUtils.createByteBuffer(16)}

  def sharedFloatBuffer(capacity:Int) = {
    var buffer = sharedFloatBufferInstance.get

    if(buffer.capacity() < capacity) {
      buffer = BufferUtils.createFloatBuffer(capacity)
      sharedFloatBufferInstance set buffer
    }
    else {
      buffer.clear()
      buffer.limit(capacity)
    }

    buffer
  }

  def sharedIntBuffer(capacity:Int) = {
    var buffer = sharedIntBufferInstance.get

    if(buffer.capacity() < capacity) {
      buffer = BufferUtils.createIntBuffer(capacity)
      sharedIntBufferInstance set buffer
    }
    else {
      buffer.clear()
      buffer.limit(capacity)
    }

    buffer
  }

  /// thread local byte buffer that can be reused whenever needed
  // TODO create one instance per thread (needed as soon as multithreaded rendering is used)
  def sharedByteBuffer(capacity:Int) = {
    var buffer = sharedByteBufferInstance.get

    if(buffer.capacity() < capacity) {
      buffer = BufferUtils.createByteBuffer(capacity)
      sharedByteBufferInstance set buffer
    }
    else {
      buffer.clear()
      buffer.limit(capacity)
    }

    buffer
  }

  def putVec2f( buffer:FloatBuffer, v:Vec2f ) {
    putVec2f(buffer, v.x, v.y)
  }

  def putVec2f( buffer:FloatBuffer, f1:Float, f2:Float ) {
    buffer.put(f1).put(f2)
  }

  def putVec2f( buffer:ByteBuffer, v:Vec2f ) {
    putVec2f(buffer, v.x, v.y)
  }

  def putVec2f( buffer:ByteBuffer, f1:Float, f2:Float ) {
    buffer.putFloat(f1).putFloat(f2)
  }

  def putVec3f( buffer:FloatBuffer, v:Vec3f ) {
    putVec3f(buffer, v.x, v.y, v.z)
  }

  def putVec3f( buffer:FloatBuffer, f1:Float, f2:Float, f3:Float ) {
    buffer.put(f1).put(f2).put(f3)
  }

  def putVec3f( buffer:ByteBuffer, v:Vec3f ) {
    putVec3f(buffer, v.x, v.y, v.z)
  }

  def putVec3f( buffer:ByteBuffer, f1:Float, f2:Float, f3:Float ) {
    buffer.putFloat(f1).putFloat(f2).putFloat(f3)
  }

  def putVec4f( buffer:FloatBuffer, v:Vec4f ) {
    putVec4f(buffer, v.x, v.y, v.z, v.w)
  }

  def putVec4f( buffer:FloatBuffer, f1:Float, f2:Float, f3:Float, f4:Float ) {
    buffer.put(f1).put(f2).put(f3).put(f4)
  }

  def putVec4f( buffer:ByteBuffer, v:Vec4f ) {
    putVec4f(buffer, v.x, v.y, v.z, v.w)
  }

  def putVec4f( buffer:ByteBuffer, f1:Float, f2:Float, f3:Float, f4:Float ) {
    buffer.putFloat(f1).putFloat(f2).putFloat(f3).putFloat(f4)
  }

  def putMat4f( buffer:ByteBuffer, m:ReadMat4f ) {
    buffer.putFloat(m.m00)
    buffer.putFloat(m.m01)
    buffer.putFloat(m.m02)
    buffer.putFloat(m.m03)

    buffer.putFloat(m.m10)
    buffer.putFloat(m.m11)
    buffer.putFloat(m.m12)
    buffer.putFloat(m.m13)

    buffer.putFloat(m.m20)
    buffer.putFloat(m.m21)
    buffer.putFloat(m.m22)
    buffer.putFloat(m.m23)

    buffer.putFloat(m.m30)
    buffer.putFloat(m.m31)
    buffer.putFloat(m.m32)
    buffer.putFloat(m.m33)
  }

  def putMat4f( buffer:FloatBuffer, m:ReadMat4f ) {
    buffer.put(m.m00)
    buffer.put(m.m01)
    buffer.put(m.m02)
    buffer.put(m.m03)

    buffer.put(m.m10)
    buffer.put(m.m11)
    buffer.put(m.m12)
    buffer.put(m.m13)

    buffer.put(m.m20)
    buffer.put(m.m21)
    buffer.put(m.m22)
    buffer.put(m.m23)

    buffer.put(m.m30)
    buffer.put(m.m31)
    buffer.put(m.m32)
    buffer.put(m.m33)
  }

  def projectionD(l:Double,r:Double,b:Double,t:Double,n:Double,f:Double):Mat4d = {
    val v1 = Vec4d( (2*n)/(r-l),0,0,0 )
    val v2 = Vec4d( 0,(2*n)/(t-b),0,0 )
    val v3 = Vec4d( (r+l)/(r-l),(t+b)/(t-b),(f+n)/(n-f), -1 )
    val v4 = Vec4d( 0,0,2*f*n/(n-f),0 )

    Mat4d(v1,v2,v3,v4)
  }

  def projectionD(n:Double,f:Double,v:Double):Mat4d = {
    val n = 0.05     // near
    val f = 1000.0   // far
    val l = - v * n  // left
    val r =   v * n  // right
    val t =  n       // top
    val b = -n       // bottom

    projectionD(l,r,b,t,n,f)
  }

  def projectionF(l:Float,r:Float,b:Float,t:Float,n:Float,f:Float):Mat4f = {
    val v1 = Vec4f( (2*n)/(r-l),0,0,0 )
    val v2 = Vec4f( 0,(2*n)/(t-b),0,0 )
    val v3 = Vec4f( (r+l)/(r-l),(t+b)/(t-b),(f+n)/(n-f), -1 )
    val v4 = Vec4f( 0,0,2*f*n/(n-f),0 )

    Mat4f(v1,v2,v3,v4)
  }

  def projectionF(n:Float,f:Float,v:Float):Mat4f = {
    val n = 0.05f     // near
    val f = 1000.0f   // far
    val l = - v * n  // left
    val r =   v * n  // right
    val t =  n       // top
    val b = -n       // bottom

    projectionF(l,r,b,t,n,f)
  }

  import scala.reflect.runtime.universe._

  def sizeOf[T : TypeTag](x:T):Int = sizeOf[T]

  def sizeOf[T : TypeTag] = {
    val t = typeOf[T]
         if( t =:= typeOf[Int]   || t =:= typeOf[Float]  ) 4
    else if( t =:= typeOf[Long]  || t =:= typeOf[Double] ) 8
    else if( t =:= typeOf[Short] || t =:= typeOf[Char]   ) 2
    else if( t =:= typeOf[Byte] ) 1
    else if( t =:= typeOf[Vec4d] ) 4*8
    else if( t =:= typeOf[Vec3d] ) 3*8
    else if( t =:= typeOf[Vec2d] ) 2*8
    else if( t =:= typeOf[Mat4d] ) 4*4*8
    else if( t =:= typeOf[Mat3d] ) 3*3*8
    else if( t =:= typeOf[Mat2d] ) 2*2*8
    else if( t =:= typeOf[Vec4f] ) 4*4
    else if( t =:= typeOf[Vec3f] ) 3*4
    else if( t =:= typeOf[Vec2f] ) 2*4
    else if( t =:= typeOf[Mat4f] ) 4*4*4
    else if( t =:= typeOf[Mat3f] ) 3*3*4
    else if( t =:= typeOf[Mat2f] ) 2*2*4
    else if( t =:= typeOf[ReadVec4d] ) 4*8
    else if( t =:= typeOf[ReadVec3d] ) 3*8
    else if( t =:= typeOf[ReadVec2d] ) 2*8
    else if( t =:= typeOf[ReadMat4d] ) 4*4*8
    else if( t =:= typeOf[ReadMat3d] ) 3*3*8
    else if( t =:= typeOf[ReadMat2d] ) 2*2*8
    else if( t =:= typeOf[ReadVec4f] ) 4*4
    else if( t =:= typeOf[ReadVec3f] ) 3*4
    else if( t =:= typeOf[ReadVec2f] ) 2*4
    else if( t =:= typeOf[ReadMat4f] ) 4*4*4
    else if( t =:= typeOf[ReadMat3f] ) 3*3*4
    else if( t =:= typeOf[ReadMat2f] ) 2*2*4
    else ???
  }
}
