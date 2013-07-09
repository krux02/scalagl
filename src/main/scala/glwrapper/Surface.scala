package glwrapper

/**
 * User: arne
 * Date: 29.04.13
 * Time: 21:56
 */

object Surface {
  def concatHorizontal(images:Seq[Surface]):Surface = {
    val height = images.head.height
    require( (true /: images) ( _ && _.height == height ) )
    val width = (0 /: images) ( _ + _.width )

    val data = new Array[Int](width*height)

    var offset = 0
    for(line <- 0 until height) {
      for(img <- images) {
        Array.copy(img,line*img.width, data, offset, img.width)
        offset += img.width
      }
    }

    new Surface(width, height, data)
  }

  def concatVertical(images:Seq[Surface]):Surface = {
    val width = images.head.width
    require( (true /: images) ( _ && _.height == width ) )
    val height = (0 /: images) ( _ + _.height )

    val data = Array.concat( images.map(_.data):_* )

    new Surface(width, height, data)
  }

  def blit(src:Surface, screen:Surface) {
    blit(src, screen, new Rect(0,0,-1,-1) )
  }

  def blit(src:Surface, screen:Surface, dstRect:Rect) {
    blit(src, new Rect(0,0,src.width,src.height), screen, dstRect)
  }

  // same as SDL_BlitSurface but in software and overwriting alpha at the moment
  def blit(src:Surface, srcRect:Rect, dst:Surface, dstRect:Rect) {
    require( srcRect.x >= 0 && srcRect.y >= 0)
    require( srcRect.x >= 0 && srcRect.y >= 0)

    dstRect.w = srcRect.w
    dstRect.h = srcRect.h

    // out of target

    if ( dstRect.x >= dst.width )
      return
    if ( dstRect.y >= dst.height )
      return
    if ( dstRect.x + dstRect.w <= 0 )
      return
    if ( dstRect.y + dstRect.h <= 0 )

    // clipping

    if( dstRect.x < 0 ) {
      srcRect.x - dstRect.x
      srcRect.w + dstRect.x
      dstRect.x = 0
    }

    if( dstRect.y < 0 ) {
      srcRect.y - dstRect.y
      srcRect.h + dstRect.y
      dstRect.y = 0
    }

    if( dstRect.x + srcRect.w >= dst.width ) {
      dstRect.w = dst.width - dstRect.x
      srcRect.w = dstRect.w
    }

    if( dstRect.y + srcRect.h >= dst.height ) {
      dstRect.h = dst.height - dstRect.y
      srcRect.h = dstRect.h
    }

    // some checks that need to be true now

    assert( src checkBounds srcRect )
    assert( dst checkBounds dstRect )
    assert( srcRect.w == dstRect.w && srcRect.h == dstRect.h)

    for( y <- 0 until srcRect.h ) {
      val srcY = srcRect.y + y
      val dstY = dstRect.y + y
      System.arraycopy( src.data, src.indexOf(srcRect.x, srcY), dst.data, dst.indexOf(dstRect.x, dstY), srcRect.w )
    }
  }
}



final class Surface(val width:Int, val height:Int, val data:Array[Int] ) {
  require( ((width - 1) & width) == 0 )
  require( ((height - 1) & height) == 0 )
  require( data.length == width * height, "data length is %d , but should be %d, width: %d, height: %d".format(data.length,width*height,width,height) )

  def this(width:Int,height:Int) = this(width,height, new Array[Int](width*height))

  def apply(x:Int,y:Int) = data( indexOf(x,y) )

  def update(x:Int,y:Int, color:Int) {
    data( indexOf(x,y) ) = color
  }

  def checkBounds( rect:Rect ) = {
    0 <= rect.x &&
    0 <= rect.y &&
    rect.x + rect.w <= width &&
    rect.y + rect.h <= height
  }

  def indexOf(x:Int, y:Int) = {
    require(0 <= x && x < width)
    require(0 <= y && y < height)
    y * width + x
  }
}
