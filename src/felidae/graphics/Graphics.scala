/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics

import de.matthiasmann.twl.DesktopArea
import de.matthiasmann.twl.GUI
import de.matthiasmann.twl.renderer.lwjgl.LWJGLRenderer
import de.matthiasmann.twl.theme.ThemeManager
import felidae.Game
import felidae.Utils
import felidae.Utils
import felidae.Utils
import java.awt.Color
import java.awt.Font
import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import javax.imageio.ImageIO
import net.java.games.input.Component.Identifier
import net.java.games.input.Controller
import net.java.games.input.ControllerEnvironment
import net.java.games.input.Event
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.common.Vec2
import org.jbox2d.common.XForm
import org.jbox2d.dynamics.Body
import org.lwjgl.input.Mouse
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.DisplayMode
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.PixelFormat
import scala.collection.immutable.HashMap
import scala.concurrent.ops.future
import scala.concurrent.ops.spawn
import scala.concurrent.MailBox
import scala.io.Source.fromFile
import scala.xml.XML
import java.lang.Math.max
import java.lang.Math.cos
import java.lang.Math.sin
import java.lang.Math.PI
import java.io.IOException

class Texture(val id : Int, val width : Int, val height : Int)
class Cursor(val screenPoint : Vec2, val worldPoint : Vec2)
class Input(val mouse : Cursor, val event : Event, val controller : Controller)
class Rectangle(val x : Float = 0, val y : Float = 0, val width : Float = 0, val height : Float = 0) {
    def +(that : Rectangle) : Rectangle = {
        val newX = if(that.x < this.x) that.x else this.x
        val newY = if(that.y < this.y) that.y else this.y
        new Rectangle(newX,
                  newY,
                  (if(that.x + that.width < this.x + this.width) this.x + this.width else that.x + that.width) - newX,
                  (if(that.y + that.height < this.y + this.height) this.y + this.height else that.y + that.height) - newY)
    }
    override def toString = "x = " + x + ", y = " + y + ", width = " + width + ", height = " + height
}

object Effects {
    val clamp : Int = 0
    val repeat : Int = 1
}
class Effects(val xform : XForm = XForm.identity,
              val wrapping : Int = Effects.clamp,
              val color : Color = Color.white,
              val flipHorizontal : Boolean = false,
              val flipVertical : Boolean = false,
              val doTile : Boolean = false,
              val tile : Vec2 = new Vec2(1, 1))

class Graphics(val game : Game) {
    var loadedTextures : Map[String, Texture] = new HashMap[String, Texture]
    var fonts : Map[String, TrueTypeFont] = new HashMap[String, TrueTypeFont]
    var currentFont : TrueTypeFont = null
    var fullScreen : Boolean = true
    var active : Boolean = false
    var running : Boolean = true
    var done : Boolean = false
    var initialized : Boolean = false
    var _displayMode : DisplayMode = null
    var gui : GUI = null
    var guiRenderer : LWJGLRenderer = null
    var theme : ThemeManager = null
    var desktop : DesktopArea = null
    val mailbox = new MailBox
    mailbox send None
    private var _viewPort : Rectangle = new Rectangle(0, 0, 1, 1)
    var zPosition = 0f
    var currentColor = Color.white
    
    var error : Throwable = null
    spawn{update}
    synchronized {
        while(!done) wait
    }
    
    if(error != null) throw error

    def loadFont(fileName : String) : Boolean = {
        try {
            val is = new FileInputStream(new File(fileName));
            fonts = fonts.updated(fileName.substring(0, fileName.indexOf(".ttf")), TrueTypeFont(Font.createFont(Font.TRUETYPE_FONT, is), true))
        } catch {
            case e : Exception => {
                e.printStackTrace
                return false
            }
        }
        return true
    }

    def setCurrentFont(font : String) = currentFont = fonts(font)

    def drawString(text : Any, x : Float, y : Float) : Unit = {
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        currentFont.render(if (text == null) "null" else text.toString, x, y, viewPort.width/windowWidth, viewPort.height/windowHeight)
        GL11.glDisable(GL11.GL_TEXTURE_2D)
    }

    def drawStringOnView(text : Any, x : Float, y : Float) : Unit = {
        drawString(text, x + viewPort.x, y + viewPort.y)
    }

    def getStringVolume(text : Any) : Rectangle = {
        currentFont.volume(if (text == null) "null" else text.toString, viewPort.width/windowWidth, viewPort.height/windowHeight)
    }

    def drawStringInCenter(text : Any) : Unit = {
        val bounds = getStringVolume(text)
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        currentFont.render(if (text == null) "null" else text.toString, 
                               viewPort.x + (viewPort.width - bounds.width)/2,
                               viewPort.y + (viewPort.height - bounds.height)/2,
                               viewPort.width/windowWidth,
                               viewPort.height/windowHeight)
    }

    def findLowestDisplayMode : DisplayMode = {
        val modes = Display.getAvailableDisplayModes
        val start : DisplayMode = null
        return (start /: modes){(m1, m2) => 
            if(m1 == null)
                return m2
            else 
                return if(m2.getWidth <= m1.getWidth && m2.getHeight <= m1.getHeight && m2.isFullscreenCapable) m2 else m1
        }
    }

    def findDisplayMode(width : Int, height : Int) : Option[DisplayMode] = {
        try {
            val modes = Display.getAvailableDisplayModes
            for(m <- modes) if(m.getWidth == width && m.getHeight == height && m.isFullscreenCapable) return Some(m)
            return None
        } catch {
            case e : Exception => {
                e.printStackTrace
                return None
            }
        }
    }

    def clear = GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_STENCIL_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)

    def clearMemory : Unit = {
        val temp = BufferUtils.createIntBuffer(loadedTextures.size)
        for(t <- loadedTextures.values) temp.put(t.id)
        GL11.glDeleteTextures(temp)
        loadedTextures = loadedTextures.empty
    }

    def loadTexture(fileName : String) : Texture = if(loadedTextures.contains(fileName)) {
        return loadedTextures(fileName) 
    }else{
        val newTexture = loadTexture(ImageIO.read(new File(fileName)), new Effects)
        loadedTextures += fileName -> newTexture
        return newTexture
    }
    def loadTexture(fileName : String, effects : Effects) : Texture = if(loadedTextures.contains(fileName)) {
        return loadedTextures(fileName) 
    }else{
        val newTexture = loadTexture(ImageIO.read(new File(fileName)), effects)
        loadedTextures += fileName -> newTexture
        return newTexture
    }
    def loadTexture(name : String, image : BufferedImage) : Texture = if(loadedTextures.contains(name)){
        return loadedTextures(name) 
    }else{
        val newTexture = loadTexture(image, new Effects())
        loadedTextures += name -> newTexture
        return newTexture
    }
    def loadTexture(image : BufferedImage) : Texture = loadTexture(image, new Effects())
    def loadTexture(image : BufferedImage, effects : Effects) : Texture = {
        val rawData = image.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
        val data = BufferUtils.createByteBuffer(4*image.getWidth*image.getHeight)
        var offset = 0
        image.getType match {
            case BufferedImage.TYPE_4BYTE_ABGR => {
                while(data.hasRemaining){
                    data.put(rawData, offset+3, 1);
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset, 1);
                    offset+=4;
                }
            }
            case BufferedImage.TYPE_INT_ARGB => {
                while(data.hasRemaining){
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+3, 1);
                    data.put(rawData, offset, 1);
                    offset+=4;
                }
            }
            case BufferedImage.TYPE_3BYTE_BGR => {
                while(data.hasRemaining){
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset, 1);
                    data.put(255.asInstanceOf[Byte]);
                    offset+=3;
                }
            }
            case BufferedImage.TYPE_INT_BGR => {
                while(data.hasRemaining){
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset, 1);
                    data.put(255.asInstanceOf[Byte]);
                    offset+=4;
                }
            }
            case BufferedImage.TYPE_INT_RGB => {
                while(data.hasRemaining){
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset, 1);
                    data.put(255.asInstanceOf[Byte]);
                    offset+=4;
                }
            }
            case BufferedImage.TYPE_CUSTOM => {
                while(data.hasRemaining){
                    data.put(rawData, offset, 1);
                    data.put(rawData, offset+1, 1);
                    data.put(rawData, offset+2, 1);
                    data.put(rawData, offset+3, 1);
                    offset+=4;
                }
            }
        }
        data.rewind()
        val scratch = BufferUtils.createIntBuffer(1)
        GL11.glGenTextures(scratch)
        GL11.glBindTexture(GL11.GL_TEXTURE_2D, scratch.get(0))
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MIN_FILTER, GL11.GL_LINEAR)
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MAG_FILTER, GL11.GL_LINEAR)
        effects.wrapping match {
            case Effects.clamp => {
                GL11.glTexParameterf( GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, GL11.GL_CLAMP )
                GL11.glTexParameterf( GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, GL11.GL_CLAMP )
            }
            case Effects.repeat => {
                GL11.glTexParameterf( GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, GL11.GL_REPEAT )
                GL11.glTexParameterf( GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, GL11.GL_REPEAT )
	        }
        }
        GL11.glTexImage2D(GL11.GL_TEXTURE_2D, 0, 4, image.getWidth, image.getHeight, 0, GL11.GL_RGBA, GL11.GL_UNSIGNED_BYTE, data)

        return new Texture(scratch.get(0), image.getWidth, image.getHeight)
    }

    def clearColor = {
        val result = BufferUtils.createFloatBuffer(16)
        GL11.glGetFloat(GL11.GL_COLOR_CLEAR_VALUE, result)
        new Color(result.get(0), result.get(1), result.get(2), result.get(3))
    }
    def clearColor_=(color : Color) = GL11.glClearColor(color.getRed()/255f, color.getGreen()/255f, color.getBlue()/255f, color.getAlpha()/255f)

    def destroy = {
        this execute (() => {
                println("destroy called from Graphics")
                clearMemory
                if(gui != null) gui.destroy
                if(theme != null) theme.destroy
                if(Display.isCreated) Display.destroy
                running = false
        })
    }
    
    def displayMode = _displayMode
    def displayMode_=(displayMode : DisplayMode) : Unit = {
        try {
            _displayMode = displayMode
            val mappings = ("ScreenWidth", displayMode.getWidth.toString) ::
                       ("ScreenHeight", displayMode.getHeight.toString) ::
                       ("FullScreen", fullScreen.toString) :: Nil
            XML.save("Settings" + File.separator + "Graphics.xml", <Graphics>{"\n"}{Utils.packageCode(Map.empty[String, String] ++ mappings, "    ")}</Graphics>, "UTF-8", true)
        } catch {
            case ex : Exception =>
                System.err.println(ex)
        }
        try {
            Display.setDisplayMode(displayMode)
            Display.setTitle(game.title)
            Display.setFullscreen(fullScreen)
            if (Display.isCreated()) {
                viewPort = new Rectangle(0, 0, 1, 1)
                GL11.glViewport(0, 0, Display.getDisplayMode.getWidth, Display.getDisplayMode.getHeight)
            }
        } catch {
            case e : Exception => {
                e.printStackTrace
            }
        }
    }

    def viewPort = _viewPort
    
    def viewPort_=(viewPort : Rectangle) : Unit = {
        _viewPort = viewPort
        GL11.glMatrixMode(GL11.GL_PROJECTION)
        GL11.glLoadIdentity
        GL11.glOrtho(viewPort.x, viewPort.x + viewPort.width, viewPort.y, viewPort.y + viewPort.height, -1.0, 1.0)
    }

    def aspectRatio = windowWidth.asInstanceOf[Float]/windowHeight
    def windowWidth = displayMode.getWidth
    def windowHeight = displayMode.getHeight
    def windowDimensions = new Vec2(windowWidth, windowHeight)

    def draw(texture : Texture, destination : Rectangle) : Unit = draw(texture, destination, new Rectangle(0, 0, 1, 1), new Effects)
    def draw(texture : Texture, destination : Rectangle, effects : Effects) : Unit = draw(texture, destination, new Rectangle(0, 0, 1, 1), effects)
    def draw(texture : Texture, destination : Rectangle, source : Rectangle) : Unit = draw(texture, destination, source, new Effects)
    def draw(texture : Texture, destination : Rectangle, source : Rectangle, effects : Effects) : Unit = {
        var left = source.x
        var right = source.x + source.width
        var top = source.y + source.height
        var bottom = source.y

        if(effects.doTile){
            right = source.x + source.width*destination.width/effects.tile.x
            top =  source.y + source.height*destination.height/effects.tile.y
        }
        if(effects.flipHorizontal){
            val temp = left
            left = right
            right = temp
        }


        GL11.glColor4f(effects.color.getRed, effects.color.getGreen, effects.color.getBlue, effects.color.getAlpha)
        GL11.glBindTexture(GL11.GL_TEXTURE_2D, texture.id)

        var temp : Vec2 = new Vec2
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        GL11.glBegin(GL11.GL_QUADS)
            GL11.glTexCoord2f(left, top)
            temp = XForm.mul(effects.xform, new Vec2(destination.x, destination.y))
            GL11.glVertex3f(temp.x, temp.y, zPosition)
            GL11.glTexCoord2f(right, top)
            temp = XForm.mul(effects.xform, new Vec2(destination.x + destination.width, destination.y))
            GL11.glVertex3f(temp.x, temp.y, zPosition)
            GL11.glTexCoord2f(right, bottom)
            temp = XForm.mul(effects.xform, new Vec2(destination.x + destination.width, destination.y + destination.height))
            GL11.glVertex3f(temp.x, temp.y, zPosition)
            GL11.glTexCoord2f(left, bottom)
            temp = XForm.mul(effects.xform, new Vec2(destination.x, destination.y + destination.height))
            GL11.glVertex3f(temp.x, temp.y, zPosition)
        GL11.glEnd()
        GL11.glDisable(GL11.GL_TEXTURE_2D)

    }

    def displayPeriod : Float = 1f/Display.getDisplayMode.getFrequency

    def draw(body : Body) : Unit = {
        var s = body.getShapeList
        while(s != null){
            if(s.isInstanceOf[PolygonShape]){
                val s2 = s.asInstanceOf[PolygonShape]
                GL11.glBegin(GL11.GL_LINE_STRIP)
                    for(v <- 0 to s2.getVertexCount) {
                        val temp = XForm.mul(body.getXForm, s2.m_vertices(v % s2.getVertexCount))
                        GL11.glVertex3f(temp.x, temp.y, zPosition)
                    }
                GL11.glEnd
            }else if(s.isInstanceOf[CircleShape]){
                val s2 = s.asInstanceOf[CircleShape]
                val transform = XForm.mul(body.getXForm, s2.getLocalPosition)
                GL11.glBegin(GL11.GL_LINE_STRIP)
                    (0 to 100) foreach {(x) =>
                        GL11.glVertex3d(transform.x + cos(2*PI*x/100)*s2.m_radius, transform.y + sin(2*PI*x/100)*s2.m_radius, zPosition)
                    }
                GL11.glEnd
            }
            s = s.getNext
        }
    }

    def color = currentColor
    def color_=(input : Color) = {
        currentColor = input
        GL11.glColor4f(color.getRed/255f, color.getGreen/255f, color.getBlue/255f, color.getAlpha/255f)
    }

    def draw(from : Vec2, to : Vec2) : Unit = {
        GL11.glBegin(GL11.GL_LINES)
            GL11.glVertex3f(from.x, from.y, zPosition)
            GL11.glVertex3f(to.x, to.y, zPosition)
        GL11.glEnd
    }

    def draw(lines : Seq[Vec2]) : Unit = {
        GL11.glBegin(GL11.GL_LINE_STRIP)
            lines foreach (point => GL11.glVertex3f(point.x, point.y, zPosition))
        GL11.glEnd
    }

    def draw(rectangle : Rectangle) : Unit = {
        GL11.glBegin(GL11.GL_LINE_STRIP)
            GL11.glVertex3f(rectangle.x, rectangle.y, zPosition)
            GL11.glVertex3f(rectangle.x + rectangle.width, rectangle.y, zPosition)
            GL11.glVertex3f(rectangle.x + rectangle.width, rectangle.y + rectangle.height, zPosition)
            GL11.glVertex3f(rectangle.x, rectangle.y + rectangle.height, zPosition)
            GL11.glVertex3f(rectangle.x, rectangle.y, zPosition)
        GL11.glEnd
    }
    /*
    def vertexShader_=(input : String) = {
        val fileLoader = GLShaders.class.getClassLoader();
		val fileInputStream = fileInputStream = new FileInputStream(filename)
		val dataStream = new DataInputStream(fileInputStream)
		val shaderCode = new Array[Byte](fileInputStream.available)
		dataStream.readFully(shaderCode)
		fileInputStream.close
		dataStream.close
		
		val shaderPro = BufferUtils.createByteBuffer(shaderCode.length)
 
		shaderPro.put(shaderCode)
		shaderPro.flip

		val vertexShader = ARBShaderObjects.glCreateShaderObjectARB(ARBVertexShader.GL_VERTEX_SHADER_ARB)
		ARBShaderObjects.glShaderSourceARB(vertexShader, shaderPro)
		ARBShaderObjects.glCompileShaderARB(vertexShader)
		
		val programObject  = ARBShaderObjects.glCreateProgramObjectARB
		ARBShaderObjects.glAttachObjectARB(programObject, vertexShader)
		
		ARBShaderObjects.glLinkProgramARB(programObject)
		ARBShaderObjects.glValidateProgramARB(programObject)
		
		ARBShaderObjects.glUseProgramObjectARB(programObject)
    }*/

    type Action = () => Unit
    
    def execute(function : Action) = {
        mailbox send Some(function)
        function.synchronized {function.wait}
    }

    def update : Unit = {
        val (targetWidth, targetHeight) = try {
            val code = Utils.parseCode(XML.loadFile("Settings" + File.separator + "Graphics.xml"))
            for((key,value) <- code) println(key + ", " + value)
            val targetWidth = code("ScreenWidth").toInt
            val targetHeight = code("ScreenHeight").toInt
            fullScreen = code("FullScreen").toBoolean
            (code("ScreenWidth").toInt, code("ScreenHeight").toInt)
        } catch {
            case e : IOException => {
                println("Could not load Graphics.cfg file.")
                val temp = findLowestDisplayMode
                (temp.getWidth, temp.getHeight)
            }
            case e => {
                done = true
                error = e
                synchronized {notify}
                return
            }
        }
        
        displayMode = findDisplayMode(targetWidth, targetHeight) match {
            case Some(d) => d
            case None => {
                done = true
                error = new Exception("Could not initialize minimal display.")
                synchronized {notify}
                return
            }
        }
        
        try {
            Display.create(new PixelFormat(0, 8, 1))
            viewPort = new Rectangle(0, 0, 1, 1)
            GL11.glViewport(0, 0, Display.getDisplayMode().getWidth(), Display.getDisplayMode().getHeight())
        } catch {
            case e => {
                done = true
                error = e
                synchronized {notify}
                return
            }
        }

        GL11.glTexEnvi(GL11.GL_TEXTURE_ENV, GL11.GL_TEXTURE_ENV_MODE, GL11.GL_BLEND)

        GL11.glEnable(GL11.GL_BLEND)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

        GL11.glClearDepth(1.0f)
        GL11.glEnable(GL11.GL_DEPTH_TEST)
        GL11.glDepthFunc(GL11.GL_LEQUAL)

        GL11.glEnable(GL11.GL_STENCIL_TEST)
        GL11.glStencilFunc(GL11.GL_EQUAL, 1, 1)
        GL11.glStencilOp(GL11.GL_KEEP, GL11.GL_KEEP, GL11.GL_KEEP)
        GL11.glClearStencil(1)

        GL11.glDisable(GL11.GL_COLOR_MATERIAL)

        GL11.glLineWidth(1)

        fonts = Map.empty[String, TrueTypeFont]
        currentFont = TrueTypeFont(new Font(Font.SERIF, Font.PLAIN, 20), true)
        fonts = fonts.updated("Default", currentFont)
        try {
            guiRenderer = new LWJGLRenderer
        } catch {
            case e => {
                done = true
                error = e
                synchronized {notify}
                return
            }
        }
        desktop = new DesktopArea
        desktop.setTheme("")
        gui = new GUI(desktop, guiRenderer)
        println(gui)
        try {
            theme = ThemeManager.createThemeManager(new File("GUI" + File.separator + "guiTheme.xml").toURI().toURL(), guiRenderer)
        } catch {
            case e => {
                done = true
                error = e
                synchronized {notify}
                return
            }
        }

        gui.applyTheme(theme)

        done = true
        synchronized {notify}
        
        synchronized {while(!game.running) wait}

        try {
            while(running){
                val time = System.currentTimeMillis

                gui.update
                Display.update
                active = Display.isActive
                if(active){
                    clear
                    val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
                    if(controllers.length == 0) return Some(new Exception("Found no controllers."))

                    for(controller <- controllers) {
                        controller.poll()
                        val queue = controller.getEventQueue
                        var event : Event = new Event
                        while(queue.getNextEvent(event)) {
    //                        if(controller.getType == Controller.Type.KEYBOARD && event.getComponent.getIdentifier == Identifier.Key.GRAVE){
    //                            if(event.getValue == 1){
    //                                game.scripting.console.setVisible(!game.scripting.console.isVisible)
    //                            }else if(game.scripting.console.isVisible){
    //                                game.scripting.console.requestFocus
    //                            }
    //                        }
                            game.gameState match {
                                case Some(state) => {
                                        val worldX = viewPort.x + Mouse.getX.toFloat/windowWidth*viewPort.width
                                        val worldY = viewPort.y + Mouse.getY.toFloat/windowHeight*viewPort.height
                                        state.processInput(new Input(new Cursor(new Vec2(Mouse.getX, Mouse.getY), new Vec2(worldX, worldY)), event, controller))
                                }
                                case None => {}
                            }
                        }
                    }

                    game.gameState match {
                        case Some(state) => state.render
                        case None => {}
                    }
                    drawStringOnView(String.valueOf(game.FPS), 0, 5)

                    if(Display.isCloseRequested) {
                        println("Close Requested")
                        game.running = false
                    }

                    done = false
                    while(!done) {
                        mailbox receive {
                            case Some(function) => {
                                function.asInstanceOf[Action].apply
                                function.asInstanceOf[Action].synchronized {function.asInstanceOf[Action].notify}
                            }
                            case None => {
                                done = true
                                mailbox send None
                            }
                        }
                    }
                }
                val sleepTime = max(0, 1000/60 - (System.currentTimeMillis - time))
                Thread.sleep(sleepTime)
            }
        } catch {
            case e => {
                error = e
            }
        }
    }
}
