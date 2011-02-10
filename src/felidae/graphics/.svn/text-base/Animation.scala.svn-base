package felidae.graphics

import felidae.Game

class Animation(val game : Game, val sheetFileName : String, val numRows : Int, val numColumns : Int, val numCells : Int, val period : Float, var cell : Int = 0) {
    val sheet = game.graphics.loadTexture(sheetFileName)
    var time : Float = 0
    val cellWidth = 1f / numColumns
    val cellHeight = 1f / numRows
    val effects = new Effects
    
    def update : Unit = {
        time += game.delta
        val periodsPassed = (time / period).asInstanceOf[Int]
        time = time % period;
        cell = (cell + periodsPassed) % numCells
    }
    
    def draw(volume : Rectangle, cellOffset : Int = 0) : Unit = {
        val x = ((cell+cellOffset) % numCells) % numColumns
        val y = ((cell+cellOffset) % numCells) / numColumns

        game.graphics.drawTexture(sheet, volume, new Rectangle(cellWidth*x, cellHeight*y, cellWidth, cellHeight), effects)
    }
}
