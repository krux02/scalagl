package glwrapper

import scala.collection.mutable
import simplex3d.math.floatx._

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:45
 */

abstract class Binding( val program:Program ) extends AddString {
  val attributes:Seq[Attribute[_]]
  val uniforms:Seq[Uniform[_]]
  val transformFeedback:Seq[Attribute[_]]

  val changedUniforms = mutable.Queue[Uniform[_]]()

  def enableAttributes() {
    for( attrib <- attributes ){
      attrib.enable()
    }
  }

  def disableAttributes() {
    for( attrib <- attributes ) {
      attrib.disable()
    }
  }

  def writeAllUniforms() {
    writeUniforms(uniforms)
  }


  /*
    Uniforms are bound to the program, not the vertex Array object, that's bad
   */
  def writeChangedUniforms() {
    writeUniforms( changedUniforms.dequeueAll(_ => true) )
  }

  private def writeUniforms( uniforms:Seq[Uniform[_]] ) {
    require(program.isActive, "program must be active before data is written")
    for( binding <- uniforms ) {
      binding.writeData()
    }
  }

  def setAttributePointers() {
    val groupedAttributes = attributes.groupBy( _.bufferBinding.buffer )
    for( (buffer, atList) <- groupedAttributes ) {
      buffer.bind {
        for( binding <- atList ) {
          binding.setPointer()
        }
      }
    }
  }

  def addString(sb:StringBuilder) = {
    sb append program
    attributes.addString(sb,"\n\t","\n\t","\n\t")
    uniforms.addString(sb,"\n\t","\n\t","\n\t")
    transformFeedback.addString(sb,"\n\t","\n\t","\n\t")
    sb append "\n"
    sb
  }

  //def transformFeedbackVec4f(name:String) = transformFeedback.collect{ case at:AttributeVec4f => at }.head

  def attributeInt(name:String)   = attributes.collect{ case at:AttributeInt   if at.name == name => at }.headOption.getOrElse(new AttributeFake[Int](program, this, name))
  def attributeFloat(name:String) = attributes.collect{ case at:AttributeFloat if at.name == name => at }.headOption.getOrElse(new AttributeFake[Float](program, this, name))
  def attributeVec2f(name:String) = attributes.collect{ case at:AttributeVec2f if at.name == name => at }.headOption.getOrElse(new AttributeFake[Vec2f](program, this, name))
  def attributeVec3f(name:String) = attributes.collect{ case at:AttributeVec3f if at.name == name => at }.headOption.getOrElse(new AttributeFake[Vec3f](program, this, name))
  def attributeVec4f(name:String) = attributes.collect{ case at:AttributeVec4f if at.name == name => at }.headOption.getOrElse(new AttributeFake[Vec4f](program, this, name))

  def uniformFloat(name:String) = uniforms.collect{ case uf:UniformFloat if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Float](program,this,name))
  def uniformVec2f(name:String) = uniforms.collect{ case uf:UniformVec2f if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Vec2f](program,this,name))
  def uniformVec3f(name:String) = uniforms.collect{ case uf:UniformVec3f if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Vec3f](program,this,name))
  def uniformVec4f(name:String) = uniforms.collect{ case uf:UniformVec4f if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Vec4f](program,this,name))
  def uniformMat4f(name:String) = uniforms.collect{ case uf:UniformMat4f if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Mat4f](program,this,name))
  def uniformSampler1D(name:String) = uniforms.collect{ case uf:UniformSampler1D if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Texture1D](program,this,name))
  def uniformSampler1DArray(name:String) = uniforms.collect{ case uf:UniformSampler1DArray if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Texture1DArray](program,this,name))
  def uniformSampler2D(name:String) = uniforms.collect{ case uf:UniformSampler2D if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Texture2D](program,this,name))
  def uniformSampler2DArray(name:String) = uniforms.collect{ case uf:UniformSampler2DArray if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Texture2DArray](program,this,name))
  def uniformSampler3D(name:String) = uniforms.collect{ case uf:UniformSampler3D if uf.name == name => uf }.headOption.getOrElse(new UniformFake[Texture2D](program,this,name))
  def uniformSampler2DRect(name:String) = uniforms.collect{ case uf:UniformSampler2DRect if uf.name == name => uf }.headOption.getOrElse(new UniformFake[TextureRectangle](program,this,name))
  def uniformSamplerCube(name:String) = uniforms.collect{ case uf:UniformSamplerCube if uf.name == name => uf }.headOption.getOrElse(new UniformFake[TextureCube](program,this,name))
}
