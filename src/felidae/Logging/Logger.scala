package felidae.logging;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Level;

import javax.swing.JOptionPane;

class Logger {
    val out = new FelidaeStream(System.out, this, Color.white)
    val err = new FelidaeStream(System.out, this, Color.red)
    System.setOut(out)
    System.setErr(err)
    val writer = try{
        new FileWriter("Felidae.log")
    }catch{
        case e : Exception => {
            var trace = e.toString + "\n" + {for(elem <- e.getStackTrace) yield elem.toString + "\n"}
            JOptionPane.showMessageDialog(null, "Failed to create Felidae.log:\n" + trace, "Error", JOptionPane.ERROR_MESSAGE)
        }
        throw new Exception("Failed to create Felidae.log")
    }

    def write(input : String) = {
        try {
            writer.write(input)
        } catch {
            case e : IOException => e.printStackTrace
        }
    }

    def destroy = {
        try {
            if(writer != null) writer.close
        } catch {
            case e : IOException => e.printStackTrace
        }
    }
}
