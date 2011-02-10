package felidae.scripting;

import java.awt.Color;

import org.lwjgl.input.Keyboard;

import de.matthiasmann.twl.DialogLayout;
import de.matthiasmann.twl.EditField;
import de.matthiasmann.twl.ResizableFrame;
import de.matthiasmann.twl.ScrollPane;
import de.matthiasmann.twl.TextArea;
import de.matthiasmann.twl.Timer;
import de.matthiasmann.twl.model.AutoCompletionDataSource;
import de.matthiasmann.twl.model.AutoCompletionResult;
import de.matthiasmann.twl.textarea.HTMLTextAreaModel;
import felidae.Game;

class Console(val game : Game) extends ResizableFrame {
    setTitle("Console")
    val output = new TextArea(new HTMLTextAreaModel())
    val input = new EditField()
    //input.setAutoCompletion(new ConsoleAutoCompletionDataSource)
    var outputString = ""
    var history : List[String] = Nil
    var past : List[String] = Nil
    var current : String = ""
    var future : List[String] = Nil
    var color : Color = Color.white
    var currentMax = 0
    
    input.addCallback(new EditField.Callback {
        def callback(key : Int) {
            if(key == Keyboard.KEY_RETURN) {
                color = Color.yellow
                println(input.getText)
                color = Color.white
                game.scripting.runString(input.getText)
                input.setText("")
            }else if(key == Keyboard.KEY_UP){
                past match {
                    case x :: xs => {
                        future = current :: future
                        current = x
                        past = xs
                        input.setText(current)
                    }
                    case Nil => {}
                }
            }else if(key == Keyboard.KEY_DOWN){
                future match {
                    case x :: xs => {
                        past = current :: past
                        current = x
                        future = xs
                        input.setText(current)
                    }
                    case Nil => {}
                }
            }
        }
    });

    val scroller = new Timer(game.graphics.gui)
    scroller.setContinuous(true)
    scroller.setDelay(100)
    scroller.setCallback(new Runnable {
        def run {
            if(currentMax != scrollPane.getMaxScrollPosY){
                scrollPane.setScrollPositionY(scrollPane.getMaxScrollPosY)
                scroller.stop
            }
        }
    });

    val scrollPane = new ScrollPane(output)
    scrollPane.setFixed(ScrollPane.Fixed.HORIZONTAL)
    val l = new DialogLayout
    l.setTheme("content")
    l.setHorizontalGroup(l.createParallelGroup(scrollPane, input))
    l.setVerticalGroup(l.createSequentialGroup(scrollPane, input))
    add(l)

    def requestFocus = input.requestKeyboardFocus
    
    def print(rawInput : String) = {
        val input = rawInput.replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br/>").replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;").replace(" ", "&nbsp;")
        
        var colorString = ""
        colorString += "%02x".format(color.getRed)
        colorString += "%02x".format(color.getGreen)
        colorString += "%02x".format(color.getBlue)
        val finalString = "<div style=\"font:" + colorString + "\">" + input + "</div><br/>"

        outputString += finalString
        output.getModel.asInstanceOf[HTMLTextAreaModel].setHtml(outputString)
        currentMax = scrollPane.getMaxScrollPosY
        scroller.start
    }
}
