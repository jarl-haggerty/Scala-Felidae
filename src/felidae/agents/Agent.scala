/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.agents

import felidae.GameState
import felidae.graphics.Input
import org.jbox2d.collision.shapes.Shape
import org.jbox2d.common.Vec2
import org.jbox2d.dynamics.BodyDef
import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.contacts.ContactPoint

object Agent{
    var anonymousID = -1
}

abstract class Agent(code : Map[String, String], val gameState : GameState) {
    var zPosition = 0f
    val bodyDef : BodyDef = new BodyDef
    var body : Body = null
    bodyDef.userData = this
    if (code == null || !(code.contains("X") && code.contains("Y"))) {
        bodyDef.position = new Vec2()
    }else {
        bodyDef.position = new Vec2(code("X").toFloat, code("Y").toFloat)
    }
    var dead : Boolean = false
    var initialized = false
    var dying : Boolean = false
    var name : String = if(code == null || code("Name").isEmpty){
        Agent.anonymousID += 1
        "Anonymous " + Agent.anonymousID
    }else{
        code("Name")
    }

    def initialize : Unit = {}
    def loadGraphics(code : Map[String, String]) : Unit = {}
    def update : Unit = {}
    def render : Unit = {}
    def processInput(input : Input) : Unit = {}
    def handleCollision(point : ContactPoint) : Unit = {}
    def handleSeparation(point : ContactPoint) : Unit = {}
    def verifyCollision(who : Shape) : Boolean = {true}
    def verifyRaycast(who : Agent) : Boolean = {true}
    def destroy : Unit = {
        gameState.roster -= name
        gameState.simulation.destroyBody(body)
    }

    override def toString = name
}
