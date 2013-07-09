package glwrapper

import glwrapper.util.sharedIntBuffer
import org.lwjgl.opengl.{GL11, GL30}

/**
 * User: doering
 * Date: 7/4/13
 * Time: 1:59 PM
 */

object VertexArrayObject {
  def create = (new VertexArrayObject).create()

  def create(count:Int) = {
    val buffer = sharedIntBuffer(count)
    GL30.glGenVertexArrays(buffer)
    val arrays = Array.fill(count)(new VertexArrayObject)
    for(i <- 0 until count) {
      arrays(i).id = buffer.get(i)
    }
    arrays
  }
}

class VertexArrayObject extends GlObject {
  var id = 0

  def create():this.type = {
    id = GL30.glGenVertexArrays()
    this
  }

  def delete() {
    GL30.glDeleteVertexArrays(id)
    id = 0
  }

  def bind[T](block: => T) = {
    val outer = GL11.glGetInteger( GL30.GL_VERTEX_ARRAY_BINDING )
    GL30.glBindVertexArray(id)
    val v = block
    GL30.glBindVertexArray(outer)
    v
  }
}
