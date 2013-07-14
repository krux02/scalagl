package glwrapper

import org.lwjgl.opengl._
import GL11._
import GL13._
import GL20._

/**
 * User: arne
 * Date: 14.07.13
 * Time: 21:45
 */

object ActiveTexture {
  val MaxCombinedTextureImageUnits = glGetInteger( GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS )
}

class ActiveTexture(val id:Int) {
  require( GL_TEXTURE0 <= id && id < GL_TEXTURE0 + ActiveTexture.MaxCombinedTextureImageUnits )

  def bind[U](block: => U):U = {
    val outer = glGetInteger(GL_ACTIVE_TEXTURE)
    glActiveTexture(id)
    val v = block
    glActiveTexture(outer)
    v
  }
}
