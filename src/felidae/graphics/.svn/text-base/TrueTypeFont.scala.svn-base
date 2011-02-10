/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics

import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Graphics2D
import java.awt.GraphicsEnvironment
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.image.DataBuffer
import java.awt.image.DataBufferByte
import java.awt.image.DataBufferInt
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

import util.control.Breaks.breakable
import util.control.Breaks.break

/**
 * A TrueType font implementation originally for Slick, edited for Bobjob's Engine
 *
 * @original author James Chambers (Jimmy)
 * @original author Jeremy Adams (elias4444)
 * @original author Kevin Glass (kevglass)
 * @original author Peter Korzuszek (genail)
 *
 * @new version edited by David Aaron Muhar (bobjob)
 */

case class IntObject(width : Int, height : Int, storedX : Int, storedY : Int)

object TrueTypeFont {
    val ALIGN_LEFT = 0
	val ALIGN_RIGHT = 1
	val ALIGN_CENTER = 2
	
	def getFontImage(character : Char, fontMetrics : FontMetrics, fontSize : Int, antiAlias : Boolean, font : Font) : BufferedImage = {
	    val charwidth = if(fontMetrics.charWidth(character)+8 > 0) fontMetrics.charWidth(character)+8 else 7
	    val charheight = if(fontMetrics.getHeight+3 > 0) fontMetrics.getHeight+3 else fontSize

		val fontImage = new BufferedImage(charwidth, charheight, BufferedImage.TYPE_INT_ARGB)
		val gt = fontImage.getGraphics.asInstanceOf[Graphics2D]
		if(antiAlias) gt.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
		gt.setFont(font);

		gt.setColor(Color.WHITE)
		gt.drawString(character.toString, 3, 1 + fontMetrics.getAscent)

		return fontImage
	}
	
	def loadImage(bufferedImage : BufferedImage) : Int = {
	    try{
	        val width = bufferedImage.getWidth
		    val height = bufferedImage.getHeight
		    val bpp = bufferedImage.getColorModel.getPixelSize
		    val db : DataBuffer = bufferedImage.getData.getDataBuffer
		    val byteBuffer : ByteBuffer = {
                if (db.isInstanceOf[DataBufferInt]) {
                    val intI : Array[Int] = db.asInstanceOf[DataBufferInt].getData
                    val newI : Array[Byte] = new Array[Byte](intI.length*4)
                    for(i <- 0 until intI.length) {
                        val newIndex = i*4;
                        newI(newIndex)   = (intI(i) >>> 16).asInstanceOf[Byte]
                        newI(newIndex+1) = (intI(i) >>> 8).asInstanceOf[Byte]
                        newI(newIndex+2) = (intI(i) >>> 8).asInstanceOf[Byte]
                        newI(newIndex+3) = (intI(i) >>> 24).asInstanceOf[Byte]
                    }
                    ByteBuffer.allocateDirect(width*height*(bpp/8)).order(ByteOrder.nativeOrder).put(newI)
                } else {
                    ByteBuffer.allocateDirect(width*height*(bpp/8)).order(ByteOrder.nativeOrder).put(db.asInstanceOf[DataBufferByte].getData)
                }
            }
		    byteBuffer.flip
		    
			val textureId = BufferUtils.createIntBuffer(1)
			GL11.glGenTextures(textureId)
			GL11.glBindTexture(GL11.GL_TEXTURE_2D, textureId.get(0))

			GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, GL11.GL_CLAMP)
			GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, GL11.GL_CLAMP)

			GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MAG_FILTER, GL11.GL_LINEAR)
			GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MIN_FILTER, GL11.GL_LINEAR)

			GL11.glTexEnvf(GL11.GL_TEXTURE_ENV, GL11.GL_TEXTURE_ENV_MODE, GL11.GL_MODULATE)

			GLU.gluBuild2DMipmaps(GL11.GL_TEXTURE_2D, GL11.GL_RGBA8, width, height, GL11.GL_RGBA, GL11.GL_UNSIGNED_BYTE, byteBuffer)
			return textureId.get(0)
		} catch {
		    case e => {e.printStackTrace();System.exit(-1);}
	    }
		return -1
	}
	
	def apply(font : Font, antiAlias : Boolean, additionalChars : Array[Char] = null) : TrueTypeFont = {
	    val fontMetrics = {
	        val tempfontImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
            val g = tempfontImage.getGraphics.asInstanceOf[Graphics2D]
            if(antiAlias) g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g.setFont(font);
            g.getFontMetrics
	    }
	    
	    var textureWidth = 512
	    var textureHeight = if (additionalChars != null && additionalChars.length > 0) 1024 else 512
	    var customChars : Map[Char, IntObject] = null
	    var fontTextureID = 0
	    var fontSize = font.getSize+3
	    var fontHeight = 0
	    var charArray = new Array[IntObject](256)
	    
		try {
		    val imgTemp = new BufferedImage(textureWidth, textureHeight, BufferedImage.TYPE_INT_ARGB)
			val g =  imgTemp.getGraphics.asInstanceOf[Graphics2D]

			g.setColor(new Color(0,0,0,1))
			g.fillRect(0,0,textureWidth,textureHeight)

			var rowHeight = 0
			var positionX = 0
			var positionY = 0

			val additionalCharsLength = if(additionalChars != null) additionalChars.length else 0

			var additionalMappings : List[(Char, IntObject)] = Nil
			for(i <- 0 until 256+additionalCharsLength){
			    val ch : Char = if(i < 256) i.toChar else additionalChars(i-256)
			    val fontImage = getFontImage(ch, fontMetrics, fontSize, antiAlias, font)
			    
			    if (positionX + fontImage.getWidth >= textureWidth) {
			        positionX = 0
			        positionY += rowHeight
			        rowHeight = 0
			    }
			    
			    if (fontImage.getHeight > fontHeight) fontHeight = fontImage.getHeight
				if (fontImage.getHeight > rowHeight) rowHeight = fontImage.getHeight
				
				g.drawImage(fontImage, positionX, positionY, null)
				
				val newIntObject = IntObject(fontImage.getWidth, fontImage.getHeight, positionX, positionY)
				if( i < 256 ) charArray(i) = newIntObject else additionalMappings = ch -> newIntObject :: additionalMappings
				positionX += fontImage.getWidth
			}
			if(additionalMappings != Nil) customChars = Map.empty[Char, IntObject] ++ additionalMappings
			fontTextureID = loadImage(imgTemp)
		} catch {
		    case e => {
		        System.err.println("Failed to create font.")
		        e.printStackTrace
		        return null
		    }
		}
		
		fontHeight -= 1
	    if (fontHeight <= 0) fontHeight = 1
		
	    return new TrueTypeFont(font, antiAlias, fontMetrics, customChars, fontSize, fontHeight, textureWidth, textureHeight, fontTextureID, charArray)
	}
}

class TrueTypeFont(val font : Font, 
                   val antiAlias : Boolean, 
                   val fontMetric : FontMetrics, 
                   val customChars : Map[Char, IntObject], 
                   val fontSize : Int,
                   val fontHeight : Int,
                   val textureWidth : Int,
                   val textureHeight : Int,
                   val fontTextureID : Int,
                   val charArray : Array[IntObject]) {
    import TrueTypeFont._
    
    var correctL = 9
    var correctR = 8
    def correction_=(on : Boolean) = if(on) {correctL = 2;correctR = 1;} else {correctL = 0;correctR = 0;}
	
	def drawQuad(drawX : Float, drawY : Float, drawX2 : Float, drawY2 : Float, srcX : Float, srcY : Float, srcX2 : Float, srcY2 : Float) {
		val DrawWidth = drawX2 - drawX
		val DrawHeight = drawY2 - drawY
		val TextureSrcX = srcX / textureWidth
		val TextureSrcY = srcY / textureHeight
		val SrcWidth = srcX2 - srcX
		val SrcHeight = srcY2 - srcY
		val RenderWidth = (SrcWidth / textureWidth)
		val RenderHeight = (SrcHeight / textureHeight)

		GL11.glTexCoord2f(TextureSrcX, TextureSrcY);
		GL11.glVertex2f(drawX, drawY);
		GL11.glTexCoord2f(TextureSrcX, TextureSrcY + RenderHeight);
		GL11.glVertex2f(drawX, drawY + DrawHeight);
		GL11.glTexCoord2f(TextureSrcX + RenderWidth, TextureSrcY + RenderHeight);
		GL11.glVertex2f(drawX + DrawWidth, drawY + DrawHeight);
		GL11.glTexCoord2f(TextureSrcX + RenderWidth, TextureSrcY);
		GL11.glVertex2f(drawX + DrawWidth, drawY);
	}
	
	def volume(input : String, scaleX : Float = 1, scaleY : Float = 1, startIndex : Int = 0, stopIndex : Int = -1, format : Int = ALIGN_LEFT) : Rectangle = {
	    val endIndex = if (stopIndex < 0) input.length-1 else stopIndex
	    var result = new Rectangle(0, 0, 0, 0)

		var totalwidth = 0
		var i = startIndex
		var d = 0
		var c = 0
		var startY = 0f
		
		format match {
		    case ALIGN_RIGHT => {
		        d = -1
				c = correctR
				while (i < endIndex) {
					if (input(i) == '\n') startY -= fontHeight
					i+=1
				}
			}
			case ALIGN_CENTER => {
			    breakable {
                    for(l <- startIndex to endIndex) {
                        val charCurrent = input(l)
                        if (charCurrent == '\n') break
                        val intObject = if (charCurrent < 256) charArray(charCurrent) else customChars(charCurrent)
                        totalwidth += intObject.width-correctL
                    }
                    totalwidth /= -2
                    d = 1
                    c = correctL
                }
			}
			case ALIGN_LEFT => {d = 1;c = correctL}
		}

		while (i >= startIndex && i <= endIndex) {
			val charCurrent = input(i)
			val intObject = if (charCurrent < 256) charArray(charCurrent) else customChars(charCurrent)

			if( intObject != null ) {
				if (d < 0) totalwidth += (intObject.width-c) * d
				if (charCurrent == '\n') {
					startY -= fontHeight * d
					totalwidth = 0
					if (format == ALIGN_CENTER) {
					    breakable {
                            for(l <- i+1 to endIndex) {
                                val charNext = input(l)
                                if (charNext == '\n') break
                                val nextIntObject = if (charNext < 256) charArray(charNext) else customChars(charNext)
                                totalwidth += nextIntObject.width-correctL;
                            }
                        }
						totalwidth /= -2
					}
				} else {
					result += new Rectangle(totalwidth * scaleX, startY * scaleY, intObject.width * scaleX, intObject.height * scaleY)
					if (d > 0) totalwidth += (intObject.width-c) * d
				}
				i += d
			}
		}
		return new Rectangle(0, 0, result.x + result.width, result.y + result.height)
	}
    
    def render(input : String, x : Float, y : Float, scaleX : Float = 1, scaleY : Float = 1, startIndex : Int = 0, stopIndex : Int = -1, format : Int = ALIGN_LEFT) : Unit = {
        val endIndex = if (stopIndex < 0) input.length-1 else stopIndex
		var totalwidth = 0
		var i = startIndex
		var d = 0
		var c = 0
		var startY = 0f
		
		format match {
		    case ALIGN_RIGHT => {
		        d = -1
				c = correctR
				while (i < endIndex) {
					if (input(i) == '\n') startY -= fontHeight
					i+=1
				}
			}
			case ALIGN_CENTER => {
			    breakable {
                    for(l <- startIndex to endIndex) {
                        val charCurrent = input(l);
                        if (charCurrent == '\n') break
                        val intObject = if (charCurrent < 256) charArray(charCurrent) else customChars(charCurrent)
                        totalwidth += intObject.width-correctL
                    }
                    totalwidth /= -2
                    d = 1
                    c = correctL
                }
			}
			case ALIGN_LEFT => {d = 1;c = correctL}
		}
		
		GL11.glBindTexture(GL11.GL_TEXTURE_2D, fontTextureID)
		GL11.glBegin(GL11.GL_QUADS)
		while (i >= startIndex && i <= endIndex) {
			val charCurrent = input(i)
			val intObject = if (charCurrent < 256) charArray(charCurrent) else customChars(charCurrent)

			if( intObject != null ) {
				if (d < 0) totalwidth += (intObject.width-c) * d
				if (charCurrent == '\n') {
					startY -= fontHeight * d
					totalwidth = 0
					if (format == ALIGN_CENTER) {
					    breakable {
                            for(l <- i+1 to endIndex) {
                                val charNext = input(l)
                                if (charNext == '\n') break
                                val nextIntObject = if (charNext < 256) charArray(charNext) else customChars(charNext)
                                totalwidth += nextIntObject.width-correctL;
                            }
                        }
						totalwidth /= -2;
					}
				} else {
					drawQuad((totalwidth + intObject.width) * scaleX + x, startY * scaleY + y,
							totalwidth * scaleX + x,
							(startY + intObject.height) * scaleY + y, intObject.storedX + intObject.width,
							intObject.storedY + intObject.height,intObject.storedX,
							intObject.storedY)
					if (d > 0) totalwidth += (intObject.width-c) * d
				}
				i += d
			}
		}
		GL11.glEnd();
    }
    
    def destroy : Unit = {
		val scratch = BufferUtils.createIntBuffer(1)
		scratch.put(0, fontTextureID)
		GL11.glBindTexture(GL11.GL_TEXTURE_2D, 0)
		GL11.glDeleteTextures(scratch)
	}
}
