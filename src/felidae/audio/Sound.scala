package felidae.audio;

import java.nio.IntBuffer;
import javax.sound.sampled.Clip;
import org.lwjgl.BufferUtils;

class Sound(val clip : Clip) {
    def start() : Unit = {
    	if(clip.isActive) clip.setFramePosition(0)
    	clip.start
    }
}

