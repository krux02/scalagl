package glwrapper

import simplex3d.math.{ConstVec2i, ReadVec2i}

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/8/13
 * Time: 6:18 PM
 * To change this template use File | Settings | File Templates.
 */
final class Rect(var x:Int, var y:Int, var w:Int, var h:Int) {
  def this(pos:ReadVec2i, size:ReadVec2i) = this(pos.x, pos.y, size.x, size.y)

  def pos:ReadVec2i = ConstVec2i(x,y)
  def pos_=(v:ReadVec2i) {
    x = v.x
    y = v.y
  }

  def size:ReadVec2i = ConstVec2i(w,h)
  def size_=(v:ReadVec2i) {
    w = v.x
    y = v.y
  }

}
