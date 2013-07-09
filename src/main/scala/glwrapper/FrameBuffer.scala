package glwrapper

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/8/13
 * Time: 4:36 PM
 * To change this template use File | Settings | File Templates.
 */

import org.lwjgl.opengl._
import GL30._
import GL32.glFramebufferTexture

object FrameBuffer {
  def frameBuffer = (new FrameBuffer).create()
  def readFrameBuffer = (new ReadFrameBuffer).create()
  def wrideFrameBuffer = (new DrawFrameBuffer).create()

  val MaxColorAttachments = GL11.glGetInteger(GL_MAX_COLOR_ATTACHMENTS)

  lazy val statusString = Map(
    GL_FRAMEBUFFER_UNDEFINED -> "undefined",
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT ->  "incomplete attachment",
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> "incomplete missing attachment",
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER -> "incomplete draw buffer",
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER -> "incomplete read buffer",
    GL_FRAMEBUFFER_UNSUPPORTED -> "unsupported",
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE -> "incomplete multisample",
    GL32.GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS -> "incomplete layer targets"
  )
}

abstract class AbstractFrameBuffer extends GlObject {
  var id = 0;
  def create() = {id = glGenFramebuffers(); this}

  def target : Int
  def binding : Int

  def textureColor( texture:Texture, colorAttachment:Int = 0 ) {
    require( 0 < colorAttachment && colorAttachment < FrameBuffer.MaxColorAttachments )
    glFramebufferTexture( target, GL_COLOR_ATTACHMENT0 + colorAttachment, texture.id, 0 )
  }

  def renderBufferStencil( renderBuffer:RenderBuffer ) {
    glFramebufferRenderbuffer(target, GL_STENCIL_ATTACHMENT, renderBuffer.target, renderBuffer.id)
  }

  def renderBufferDepth( renderBuffer:RenderBuffer ) {
    glFramebufferRenderbuffer(target, GL_STENCIL_ATTACHMENT, renderBuffer.target, renderBuffer.id)
  }

  def renderBufferColor( renderBuffer:RenderBuffer, colorAttachment:Int = 0 ) {
    glFramebufferRenderbuffer(target, GL_COLOR_ATTACHMENT0 + colorAttachment, renderBuffer.target, renderBuffer.id)
  }

  def checkStatus = FrameBuffer.statusString( glCheckFramebufferStatus(target) )

  def blit(src:Rect, dst:Rect, mask:Int, filter:Int) {
    glBlitFramebuffer(src.x, src.y, src.x + src.w, src.y + src.h, dst.x, dst.y, dst.x + dst.w, dst.y + dst.h, mask, filter )
  }

  def bind[T](block: => T):T = {
    require( id != 0 )
    val outer = GL11.glGetInteger(binding)
    glBindFramebuffer(target, id)
    val v = block
    glBindFramebuffer(target, outer)
    v
  }
  def delete() { glDeleteFramebuffers(id); id = 0 }
}

class FrameBuffer extends AbstractFrameBuffer {
  override def target = GL_FRAMEBUFFER
  override def binding =  GL_FRAMEBUFFER_BINDING
}

class ReadFrameBuffer extends AbstractFrameBuffer {
  override def target = GL_READ_FRAMEBUFFER
  override def binding = GL_READ_FRAMEBUFFER_BINDING
}

class DrawFrameBuffer extends AbstractFrameBuffer {
  override def target = GL_DRAW_FRAMEBUFFER
  override def binding = GL_DRAW_FRAMEBUFFER_BINDING
}