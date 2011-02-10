/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.agents


import felidae.GameState

class AgentLoader {
    def load(code : Map[String, String], gameState : GameState) : Agent = {
        val stackTrace = Thread.currentThread().getStackTrace()
        val mainClass = stackTrace(stackTrace.length-1).getClassName
        val theClass = Class.forName(mainClass.substring(0, mainClass.indexOf(".")) + ".agents." + code("Role"))
        val constructor = theClass.getConstructor(classOf[Map[String, String]], classOf[GameState])
        return constructor.newInstance(code, gameState).asInstanceOf[Agent]
    }
    
    def base(gameState : GameState) : List[Agent] = Nil
}
