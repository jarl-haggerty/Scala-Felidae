/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.audio

import java.io.File
import java.io.FileNotFoundException
import java.io.IOException

import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.Clip
import javax.sound.sampled.DataLine
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.UnsupportedAudioFileException
import scala.concurrent.ops.spawn

class Audio {
    var loadedSounds : Map[String, Sound] = Map.empty[String, Sound]
    var streaming : Set[Stream] = Set.empty[Stream]
    var running : Boolean = true
    spawn{update}
    
    def clearMemory : Boolean = {
        loadedSounds = loadedSounds.empty
        this.synchronized {
            for(s <- streaming){
                s.stop
            }
        	streaming = streaming.empty
        }
        return true
    }
    
    def streamCount : Int = streaming.size

    def destroy : Unit = {
    	clearMemory
    	running = false
    }
    
    def update : Unit = {
        while(running){
            this.synchronized {
                var toRemove : List[Stream] = Nil
                for(s <- streaming) {
                    if(!s.update) toRemove = s :: toRemove
                }
                streaming = streaming -- toRemove
            }
        }
        println("Audio Done")
    }

    def pause : Unit = for(s <- streaming) s.stop
    
    def loadSound(soundName : String) : Sound = {
        if (loadedSounds contains soundName) {
            return loadedSounds(soundName)
        }else{
            try {
				val sound = AudioSystem.getAudioInputStream(new File(soundName))
				val info = new DataLine.Info(classOf[Clip], sound.getFormat)
				val clip = AudioSystem.getLine(info).asInstanceOf[Clip]
				clip.open(sound)
				return new Sound(clip)
			} catch {
			    case e => {e.printStackTrace();return null;}
			}
        }
    }
    
    def play(sound : Sound) = sound.start
    
    def loadStream(streamName : String) : Stream = {
        try {
            return new Stream(streamName)
        } catch {
            case e => {e.printStackTrace();return null;}
        }
    }
    
    def play(stream : Stream, loop : Boolean = true) : Boolean = {
        try {
            stream.loop = loop
            stream.play
            streaming = streaming + stream
            return true
        } catch {
            case e : Exception => {
                e.printStackTrace
                return false
            }
        }
    }

    def stop(stream : Stream) : Boolean = {
        streaming = streaming - stream
        stream.stop
        return true
    }
}

