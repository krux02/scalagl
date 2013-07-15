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
  var textureCounter = GL_TEXTURE1

  def create[T <: Texture]( texture:T )( params: T#Parameter => Unit ) = {
    val at = new ActiveTexture[T](textureCounter)
    at.setTexture(texture, params)
    textureCounter += 1
    at
  }
}



class ActiveTexture[T <: Texture](val id:Int) {
  require( GL_TEXTURE0 <= id && id < GL_TEXTURE0 + ActiveTexture.MaxCombinedTextureImageUnits )



  def setTexture( texture:T ) {
    val outer = glGetInteger(GL_ACTIVE_TEXTURE)
    glActiveTexture(id)
    texture.bind()
    params( texture.parameter )
    glActiveTexture(outer)
  }
}


