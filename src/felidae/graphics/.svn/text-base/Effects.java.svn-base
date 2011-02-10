/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics;

import java.awt.Color;

import org.jbox2d.common.Mat22;
import org.jbox2d.common.Vec2;
import org.jbox2d.common.XForm;


/**
 *
 * @author Jarl
 */
public class Effects {
    public boolean flipHorizontal;
    public XForm xform;
    public Color color;
    public boolean doTile;
    public Vec2 tile;
    public Wrapping wrapping;
    
    public enum Wrapping{
    	Clamp,
    	Repeat
    }

    public Effects(){
        flipHorizontal = false;
        color = new Color(1, 1, 1, 1);
        xform = new XForm(new Vec2(), new Mat22(1, 0, 0, 1));
        doTile = false;
        tile = new Vec2();
        wrapping = Wrapping.Clamp;
    }

    /**
     * @return the flipHorizontal
     */
    public boolean isFlipHorizontal() {
        return flipHorizontal;
    }

    /**
     * @param flipHorizontal the flipHorizontal to set
     */
    public void setFlipHorizontal(boolean flipHorizontal) {
        this.flipHorizontal = flipHorizontal;
    }
}
