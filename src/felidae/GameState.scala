/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae

import felidae.agents.AgentLoader
import felidae.agents.Agent
import felidae.physics.WorldFilter
import felidae.physics.WorldListener

import java.awt.Color
import org.jbox2d.common.Vec2
import org.jbox2d.dynamics.World
import org.jbox2d.collision.AABB
import scala.xml.XML.loadFile
import scala.xml.Elem
import java.io.File

import felidae.graphics.Input

object GameState {
    def apply(name : String, agentLoader : AgentLoader, game : Game) : GameState = {
        val xml = loadFile("Levels" + File.separator + name)
        val code : Map[String, String] = Utils.parseCode((xml \ "Level")(0))
        val temp = code("BackgroundColor").split(",")
        val clearColor = new Color(temp(0).toInt, temp(1).toInt, temp(2).toInt)
        game.graphics execute {() => game.graphics.clearColor = clearColor}
        val result = new GameState(name, game, code("PixelsPerMeter").toFloat, new Vec2(code("Width").toFloat, code("Height").toFloat), clearColor, Map.empty[String, Agent])
        
        var agents : List[(String, Agent)] = Nil
        var count = 1f
        for(x <- xml \ "Element") {
            if(x.isInstanceOf[Elem]){
                val nextCode = Utils.parseCode(x)
                val nextAgent = agentLoader.load(nextCode, result)
                nextAgent.zPosition = 1/count
                count += 1
                println(nextAgent.name + ", " + nextAgent)
                agents = (nextAgent.name -> nextAgent) :: agents
                game.graphics execute {() => nextAgent.loadGraphics(nextCode)}
            }
        }
        result.roster = result.roster ++ (agents ::: (agentLoader.base(result) map (agent => (agent.name, agent))))
        for(agent <- result.roster.values) agent.body = result.simulation.createBody(agent.bodyDef)
        return result
    }
}

class GameState(val name : String, val game : Game, val pixelsPerMeter : Float, val dimensions : Vec2, val clearColor : Color, var roster : Map[String, Agent]) {
    var updateHogs = Set.empty[Agent]
    var inputHogs = Set.empty[Agent]
    
    val simulationListener = new WorldListener
    val simulationFilter = new WorldFilter
    val simulation : World = {
        val temp = new World(new AABB(new Vec2(-1000, -1000), dimensions.add(new Vec2(1000, 1000))), new Vec2(), true)
        temp.setContactListener(simulationListener)
        temp.setContactFilter(simulationFilter)
        temp
    }
    
    def processInput(input : Input) : Unit = {
        //println("Processing input: " + input)
        if(inputHogs.isEmpty){
            //if(!game.scripting.console.isVisible){
                for(agent <- roster.values) agent.processInput(input)
            //}
        }else{
            //if(!game.scripting.console.isVisible){
                for(agent <- inputHogs) agent.processInput(input)
            //}
        }
    }
        
    def initialize : Unit = {
        for(agent <- roster.values) {
            agent.initialize
            agent.initialized = true
        }
        val starter = new File("Levels" + File.separator + name.substring(0, name.indexOf(".")) + ".py")
        //if(starter.isFile) game.scripting.exec(starter)
    }
    
    def update : Unit = {
        var deadAgents : List[String] = Nil
        for(agent <- roster.values) {
            if(!agent.initialized) {
                agent.body = simulation.createBody(agent.bodyDef)
                agent.initialize
                agent.initialized = true
            }
            agent.update
        }
        simulation.step(game.delta, 6)
    }
    
    def render = for(agent <- roster.values if agent.initialized) {
        game.graphics.zPosition = agent.zPosition
        agent.render
    }
    def apply(name : String) = roster(name)
    def reset = game.gameState = name
}
