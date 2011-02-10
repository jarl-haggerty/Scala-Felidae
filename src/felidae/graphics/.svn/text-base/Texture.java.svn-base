/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics;

import java.nio.ByteBuffer;

/**
 *
 * @author Jarl
 */
public class Texture {
    public int id, width, height;
    protected boolean loaded;
    public String fileName;
    protected ByteBuffer data;

    public Texture(int id, int width, int height){
        this.id = id;
        this.width = width;
        this.height = height;
        this.loaded = true;
    }

    public Texture(int id, String fileName) {
        this.loaded = false;
        this.id = id;
        this.fileName = fileName;
    }

    /**
     * @return the width
     */
    public int getWidth() {
        return width;
    }

    /**
     * @param width the width to set
     */
    public void setWidth(int width) {
        this.width = width;
    }

    /**
     * @return the height
     */
    public int getHeight() {
        return height;
    }

    /**
     * @param height the height to set
     */
    public void setHeight(int height) {
        this.height = height;
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * @param id the id to set
     */
    public void setId(int id) {
        this.id = id;
    }
}
