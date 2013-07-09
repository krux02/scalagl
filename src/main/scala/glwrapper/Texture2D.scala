package glwrapper

import java.nio.ByteBuffer

import org.lwjgl.opengl._
import GL11._
import GL12._
import GL13._
import GL30._
import GL31._
import org.lwjgl.BufferUtils
import glwrapper.util.sharedByteBuffer

object Texture {

  val InternalFormat = GL_RGBA8
  val Format = GL_BGRA
  val DataType = GL_UNSIGNED_BYTE

  private def defaultParameters(texture:Texture) = {
    texture.parameter(GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE)
    texture.parameter(GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE)
    texture.parameter(GL_TEXTURE_MAG_FILTER,GL_LINEAR)
    texture.parameter(GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
  }

  def create1DArray(surfaces:Array[Surface]):Texture1DArray = {
    require( surfaces(0).width == 1 || surfaces(0).height == 1 )
    val vertical = surfaces(0).width == 1

    val width = if(vertical) surfaces(0).height else surfaces(0).width
    val data = sharedByteBuffer(width*4*surfaces.length)
    (data.asIntBuffer /: surfaces)( _ put _.data )
    create1DArray(width, surfaces.length, data)
  }

  def create1DArray(width:Int, levelCount:Int, pixels:ByteBuffer):Texture1DArray = {
    require( ((width - 1) & width) == 0 )

    val texture = (new Texture1DArray).create()

    texture.bind {
      defaultParameters(texture)
      glTexImage2D(GL_TEXTURE_1D_ARRAY, 0, InternalFormat, width, levelCount, 0, Format, DataType, pixels)
      texture.generateMipmap()
    }
  }

  def create1D(surface:Surface):Texture1D = {
    require( surface.width == 1 || surface.height == 1 )
    val width = surface.width max surface.height
    val data = sharedByteBuffer(width*4)
    data.asIntBuffer().put(surface.data)
    create1D(width, data)
  }

  def create1D(width:Int, pixels:ByteBuffer):Texture1D = {
    require( ((width - 1) & width) == 0 )

    val texture = (new Texture1D).create()

    texture.bind {
      texture.parameter(GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_MAG_FILTER,GL_LINEAR)
      texture.parameter(GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)

      glTexImage1D(texture.target, 0, InternalFormat, width, 0, Format, DataType, pixels)

      texture.generateMipmap()
    }
  }

  def create2DArray(surfaces:Array[Surface]):Texture2DArray = {
    val width = surfaces(0).width
    val height = surfaces(0).height
    require( ((width - 1) & width) == 0 )
    require( ((height - 1) & height) == 0 )
    for( s <- surfaces ){
      require(s.width == width && s.height == height)
    }

    val data = sharedByteBuffer(width*height*4*surfaces.length)
    (data.asIntBuffer /: surfaces)( _ put _.data )

    create2DArray(width, height, surfaces.length, data)
  }

  def create2DArray(width:Int, height:Int, levelCount:Int, pixels:ByteBuffer):Texture2DArray = {

    val texture = (new Texture2DArray).create()

    texture.bind {
      defaultParameters(texture)
      glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, InternalFormat, width, height, levelCount, 0, Format, DataType, pixels)
      texture.generateMipmap()
    }
  }

  def create2D(surface:Surface):Texture2D = {
    val width = surface.width
    val height = surface.height
    val data = sharedByteBuffer(width*height*4)
    data.asIntBuffer.put(surface.data)
    create2D(width,height,data)
  }

  def create2D(width:Int, height:Int, pixels:ByteBuffer) = {
    require( ((width - 1) & width) == 0 )
    require( ((height - 1) & height) == 0 )

    val texture = (new Texture2D).create()

    texture.bind {
      defaultParameters(texture)
      glTexImage2D(texture.target, 0, InternalFormat, width, height, 0, Format, DataType, pixels)
      texture.generateMipmap()
    }
  }

  def createRectangle(width:Int, height:Int, pixels:ByteBuffer) = {
    val texture = (new TextureRectangle).create()

    texture.bind {
      texture.parameter(GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      texture.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, InternalFormat, width, height, 0, Format, DataType, pixels)
    }
    texture
  }

  def createCube(posX:Surface,
                 negX:Surface,
                 posY:Surface,
                 negY:Surface,
                 posZ:Surface,
                 negZ:Surface):TextureCube = {
    val size = posX.width
    require( ((size - 1) & size ) == 0 )
    for( s <- List(posX, negX, posY, negY, posZ, negZ) ) {
      require(s.width == size && s.height == size)
    }

    val d1 = BufferUtils.createByteBuffer(size*size*4)
    val d2 = BufferUtils.createByteBuffer(size*size*4)
    val d3 = BufferUtils.createByteBuffer(size*size*4)
    val d4 = BufferUtils.createByteBuffer(size*size*4)
    val d5 = BufferUtils.createByteBuffer(size*size*4)
    val d6 = BufferUtils.createByteBuffer(size*size*4)

    d1.asIntBuffer().put( posX.data )
    d2.asIntBuffer().put( negX.data )
    d3.asIntBuffer().put( posY.data )
    d4.asIntBuffer().put( negY.data )
    d5.asIntBuffer().put( posZ.data )
    d6.asIntBuffer().put( negZ.data )

    Texture.createCube(size,d1,d2,d3,d4,d5,d6)
  }

  def createCube(width:Int,
                positiveX:ByteBuffer,
                negativeX:ByteBuffer,
                positiveY:ByteBuffer,
                negativeY:ByteBuffer,
                positiveZ:ByteBuffer,
                negativeZ:ByteBuffer):TextureCube = {
    val texture = (new TextureCube).create()

    texture.bind {
      defaultParameters(texture)
      texture.parameter(GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)

      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, InternalFormat, width, width, 0, Format, DataType, positiveX)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, InternalFormat, width, width, 0, Format, DataType, negativeX)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, InternalFormat, width, width, 0, Format, DataType, positiveY)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, InternalFormat, width, width, 0, Format, DataType, negativeY)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, InternalFormat, width, width, 0, Format, DataType, positiveZ)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, InternalFormat, width, width, 0, Format, DataType, negativeZ)

      texture.generateMipmap()
    }
  }
}

abstract class Texture extends GlObject {
  var id = 0

  def target:Int
  def binding:Int

  def create() = { id = glGenTextures(); this }
  def delete() = { glDeleteTextures(id); id = 0 }

  def bind() {
    glBindTexture(target, id)
  }

  def bind[T](block: => T) = {
    require( id != 0 )
    val outer = GL11.glGetInteger(binding)
    glBindTexture(target, id)
    val v = block
    glBindTexture(target, outer)
    v
  }

  def generateMipmap():this.type = {
    GL30.glGenerateMipmap(target)
    this
  }

  def parameter(name:Int,param:Int):this.type = {
    glTexParameteri(target, name, param)
    this
  }
}


class Texture1D extends Texture {
  def target = GL_TEXTURE_1D
  def binding = GL_TEXTURE_BINDING_1D
}

class Texture1DArray extends Texture {
  def target = GL_TEXTURE_1D_ARRAY
  def binding = GL_TEXTURE_BINDING_1D_ARRAY
}

class Texture2D extends Texture {
  def target   = GL_TEXTURE_2D
  def binding  = GL_TEXTURE_BINDING_2D
}

class Texture2DArray extends Texture {
  def target = GL_TEXTURE_2D_ARRAY
  def binding = GL_TEXTURE_BINDING_2D_ARRAY
}

class Texture3D extends Texture {
  def target = GL_TEXTURE_3D
  def binding = GL_TEXTURE_BINDING_3D
}

class TextureCube extends Texture {
	def target   = GL_TEXTURE_CUBE_MAP
  def binding  = GL_TEXTURE_BINDING_CUBE_MAP
}

class TextureRectangle extends Texture {
  def target = GL_TEXTURE_RECTANGLE
  def binding = GL_TEXTURE_BINDING_RECTANGLE
}