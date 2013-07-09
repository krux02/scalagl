package glwrapper

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL21._
import org.lwjgl.opengl.GL30.{glGetInteger => _, _}
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.GL40._
import org.lwjgl.opengl.GL42._
import org.lwjgl.opengl.GL43._

import java.nio.{IntBuffer, FloatBuffer, ByteBuffer}

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:50
 */

abstract class GlBuffer() extends GlObject {
  var id:Int = 0
  var hasData = false

  def create():this.type = {
    id = glGenBuffers()
    this
  }

  def bindBase(index:Int) = {
    glBindBufferBase(target, index, id)
  }

  def target:Int
  def binding:Int
  var usage = GL_STATIC_DRAW

  def bind[T]( block: => T ) = {
    require(id != 0)
    val outer = glGetInteger( binding )
    glBindBuffer(target, id)
    val v = block
    glBindBuffer(target, outer)
    v
  }

  def delete() {
    glDeleteBuffers(id)
    hasData = false
    id = 0
  }

  def putData( buffer:ByteBuffer ) {
    require( buffer.position() == 0, "forgot to flip the buffer?" )
    hasData = true
    glBufferData(target, buffer, usage)
  }

  def putData( buffer:FloatBuffer ) {
    require( buffer.position() == 0, "forgot to flip the buffer?" )
    hasData = true
    glBufferData(target, buffer, usage)
  }


  def getData( buffer:ByteBuffer, offset:Int = 0 ) = {
    require( buffer.position() == 0, "forgot to flip the buffer?" )
    glGetBufferSubData(target, offset, buffer)
    buffer
  }

  def size = glGetBufferParameteri(target, GL_BUFFER_SIZE)

  override def toString = s"GlBuffer($id)"
}

class ArrayBuffer extends GlBuffer {
  def target = GL_ARRAY_BUFFER
  def binding = GL_ARRAY_BUFFER_BINDING
}

class AtomicCounterBuffer extends GlBuffer {
  def target = GL_ATOMIC_COUNTER_BUFFER
  def binding = GL_ATOMIC_COUNTER_BUFFER_BINDING
}

class CopyReadBuffer extends GlBuffer {
  def target = GL_COPY_READ_BUFFER
  def binding = GL_COPY_READ_BUFFER_BINDING
}
class CopyWriteBuffer extends GlBuffer {
  def target = GL_COPY_WRITE_BUFFER
  def binding = GL_COPY_WRITE_BUFFER_BINDING
}

class DrawIndirectBuffer extends GlBuffer {
  def target = GL_DRAW_INDIRECT_BUFFER
  def binding = GL_DRAW_INDIRECT_BUFFER_BINDING
}

class DispatchIndirectBuffer extends GlBuffer {
  def target = GL_DISPATCH_INDIRECT_BUFFER
  def binding = GL_DISPATCH_INDIRECT_BUFFER_BINDING
}

class ElementArrayBuffer extends GlBuffer {
  def target = GL_ELEMENT_ARRAY_BUFFER
  def binding = GL_ELEMENT_ARRAY_BUFFER_BINDING
}

class PixelPackBuffer extends GlBuffer {
  def target = GL_PIXEL_PACK_BUFFER
  def binding = GL_PIXEL_PACK_BUFFER_BINDING
}

class PixelUnpackBuffer extends GlBuffer {
  def target = GL_PIXEL_UNPACK_BUFFER
  def binding = GL_PIXEL_UNPACK_BUFFER_BINDING
}

class ShaderStorageBuffer extends GlBuffer {
  def target = GL_SHADER_STORAGE_BUFFER
  def binding = GL_SHADER_STORAGE_BUFFER_BINDING
}

class TransformFeedback extends GlBuffer {
  def target = GL_TRANSFORM_FEEDBACK_BUFFER
  def binding = GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
}

class UniformBuffer extends GlBuffer {
  def target = GL_UNIFORM_BUFFER
  def binding = GL_UNIFORM_BUFFER_BINDING
}



