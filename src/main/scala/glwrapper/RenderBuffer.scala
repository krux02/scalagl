package glwrapper

import org.lwjgl.opengl._
import GL30._
import GL31._
import simplex3d.math.Vec2i

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/8/13
 * Time: 6:25 PM
 * To change this template use File | Settings | File Templates.
 */
class RenderBuffer extends GlObject {
  var id = 0
  def create() = { id = glGenRenderbuffers(); this }
  def delete() = { glDeleteRenderbuffers(id); id = 0 }

  def target = GL_RENDERBUFFER
  def binding = GL_RENDERBUFFER_BINDING

  def bind[T](block: => T) = {
    require( id != 0 )
    val outer = GL11.glGetInteger(binding)
    glBindRenderbuffer(target, id)
    val v = block
    glBindRenderbuffer(target, outer)
    v
  }

  def internalStorage( internalFormat:Int, size:Vec2i) {
    internalStorage(internalFormat, size.x, size.y )
  }

  def internalStorage( internalFormat:Int, width:Int, height:Int ){
    glRenderbufferStorage(target, internalFormat, width, height)
  }

  def width          = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_WIDTH )
  def height         = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_HEIGHT )
  def internalFormat = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_INTERNAL_FORMAT )
  def redSize        = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_RED_SIZE )
  def greenSize      = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_GREEN_SIZE )
  def blueSize       = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_BLUE_SIZE )
  def alphaSize      = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_ALPHA_SIZE )
  def depthSize      = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_DEPTH_SIZE )
  def stencilSize    = glGetRenderbufferParameteri( target, GL_RENDERBUFFER_STENCIL_SIZE )
}
