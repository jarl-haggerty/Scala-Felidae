/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;

import org.lwjgl.opengl.ARBShaderObjects;

/**
 *
 * @author Jarl
 */
public class Shader {
    int program;
    Map<String, Integer> locations;

    public Shader(int program){
        this.program = program;
        locations = new HashMap<String, Integer>();
    }

    public void initializeUniform(String name){
        if(locations.containsKey(name)){
            return;
        }

        ByteBuffer buff = ByteBuffer.allocate(name.length() + 1);
        buff.put(name.getBytes());
        buff.put((byte)0);
        buff.rewind();

        int temp = ARBShaderObjects.glGetUniformLocationARB(program, buff);
        locations.put(name, temp);
    }

    public void setUnitform1f(String name, float input){
        ARBShaderObjects.glUniform1fARB(locations.get(name), input);
    }
}
