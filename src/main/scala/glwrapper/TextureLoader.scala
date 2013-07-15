package glwrapper

import javax.imageio.ImageIO
import java.awt.image.DataBufferByte
import org.lwjgl.BufferUtils

import simplex3d.math.double._

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/8/13
 * Time: 6:18 PM
 * To change this template use File | Settings | File Templates.
 */
class TextureLoader {
  private def createBuffer( pixels:Array[Byte], hasAlphaChannel:Boolean ) = {
    val buffer = BufferUtils.createByteBuffer(pixels.length)
    if( hasAlphaChannel ) {
      // the default pixel format has its alpha channel at the wrong position for OpenGl Loading. So we need to convert the pixels
      for( Array(a,b,g,r) <- pixels.grouped(4) ) {
        buffer put b
        buffer put g
        buffer put r
        buffer put a
      }
    }
    else {
      buffer.put(pixels)
    }
    buffer.rewind()
    buffer
  }

  case class QuadTexCoords(v1:Vec2, v2:Vec2, v3:Vec2, v4:Vec2)


  def createTextureAtlas( names:Seq[String] ) : Surface = {

    var maxWidth = 0
    var maxHeight = 0

    val images =
    for(name <- names) yield {
      val image = readImage(name)
      val width = image.getWidth
      val height = image.getHeight

      if(width > maxWidth)
        maxWidth = width
      if(height > maxHeight)
        maxHeight = height

      val pixels = image.getRGB(0,0,width, height, null, 0, width)
      new Surface( width, height, pixels )
    }

    require( ((maxWidth - 1) & maxWidth) == 0 )
    require( ((maxHeight - 1) & maxHeight) == 0 )

    val numImages = images.size

    var flip = true
    var sizeX, sizeY = 1

    while( numImages > sizeX * sizeY ) {
      if(flip)
        sizeX *= 2
      else
        sizeY *= 2
      flip = !flip
    }

    Surface.concatVertical( images.grouped(sizeX).map( Surface.concatHorizontal _ ).toSeq )
  }

  private def readImage(filename:String) = {
    val is = getClass.getClassLoader.getResourceAsStream(filename)
    if(is == null)
      throw new java.io.FileNotFoundException("that resource is not available: " + filename)
    ImageIO.read(is)
  }

  def readImageRaster( filename:String ) : Surface = {
    val image = readImage(filename)
    import image.{getWidth => w, getHeight => h}
    val pixels = image.getRGB(0,0,w, h, null, 0, w)
    new Surface(w, h, pixels)
  }

  def loadAsSkybox(filename:String, fileEnding:String):TextureCube = {
    val posX = readImageRaster(filename+"_positiveX." + fileEnding)
    val negX = readImageRaster(filename+"_negativeX." + fileEnding)
    val posY = readImageRaster(filename+"_positiveY." + fileEnding)
    val negY = readImageRaster(filename+"_negativeY." + fileEnding)
    val posZ = readImageRaster(filename+"_positiveZ." + fileEnding)
    val negZ = readImageRaster(filename+"_negativeZ." + fileEnding)
    Texture.createCube(posX,negX,posY,negY,posZ,negZ)
  }

  def loadAsTexture(filename:String):Texture2D = {
    val raster = readImageRaster(filename:String)
    val buffer = BufferUtils.createByteBuffer(raster.width*raster.height*4)
    buffer.asIntBuffer.put(raster.data)
    Texture.create2D(raster.width, raster.height, buffer)
  }
}
