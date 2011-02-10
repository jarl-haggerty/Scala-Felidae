package felidae.logging;

import java.awt.Color
import java.io.IOException
import java.io.OutputStream
import java.io.PrintStream

import felidae.scripting.Console

class FelidaeStream(val systemStream : PrintStream, val logger : Logger, var color : Color) extends PrintStream(new OutputStream{override def write(b : Int) = {}}) {
    var buffer : String = ""
    private var _console : Console = null
    
    override def write(b : Array[Byte], offset : Int, len : Int) = {
        val input = new String(b).substring(offset, offset+len)
        logger.write(input)
        
        if(_console != null) {
            _console.color = color
            _console.print(input)
        } else {
            buffer += input
        }
        
        systemStream.write(b, offset, len)
    }
    
    def console = _console
    def console_=(console : Console) = {
        _console = console
        console.color = color
        console.color = color
        console.print(buffer)
        buffer = ""
    }
    
    def println(input : String, color : Color) : Unit = {
        val temp = this.color
        this.color = color
        println(input)
        this.color = temp
    }
}