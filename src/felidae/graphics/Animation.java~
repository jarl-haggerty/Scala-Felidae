/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.graphics;

import java.awt.Rectangle;

import felidae.Game;

/**
 *
 * @author Jarl
 */
public class Animation {
    public Texture sheet;
    public int numRows, numColumns, numCells, cell;
    public float time, period, cellWidth, cellHeight;
    public Effects effects;
    
    public Animation(String sheetFileName, int numRows, int numColumns, int numCells, float period){
        this(sheetFileName, numRows, numColumns, numCells, period, 0);
    }

    public Animation(String sheetFileName, int numRows, int numColumns, int numCells, float period, int startCell){
        sheet = Graphics.loadTexture(sheetFileName);
        this.numRows = numRows;
        this.numColumns = numColumns;
        this.numCells = numCells;
        this.period = period;

        cellWidth = 1f/numColumns;
        cellHeight = 1f/numRows;

        effects = new Effects();

        time = 0;
        cell = startCell;
    }

    public void update(){
        time += Game.delta;
        int periodsPassed = (int) (time / period);
        time = time % period;
        cell = (cell + periodsPassed) % numCells;
    }

    public void draw(Rectangle.Float volume){
        draw(volume, 0);
    }
    
    public void draw(Rectangle.Float volume, int cellOffset){
        int x = ((cell+cellOffset) % numCells) % numColumns, y = ((cell+cellOffset) % numCells) / numColumns;

        //Graphics.drawString(String.valueOf(x) + ", " + String.valueOf(y), 0, 150);

        Rectangle.Float source = new Rectangle.Float(cellWidth*x, cellHeight*y, cellWidth, cellHeight);

        Graphics.drawTexture(sheet, volume, source, effects);
    }

    /**
     * @return the cell
     */
    public int getCell() {
        return cell;
    }

    /**
     * @param cell the cell to set
     */
    public void setCell(int cell) {
        this.cell = cell;
    }

    /**
     * @return the flipHorizontal
     */
    public boolean isFlipHorizontal() {
        return effects.isFlipHorizontal();
    }

    /**
     * @param flipHorizontal the flipHorizontal to set
     */
    public void setFlipHorizontal(boolean flipHorizontal) {
        effects.setFlipHorizontal(flipHorizontal);
    }
}
