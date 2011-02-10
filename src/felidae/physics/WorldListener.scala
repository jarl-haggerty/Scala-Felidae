package felidae.physics;

import felidae.agents.Agent
import org.jbox2d.dynamics.ContactListener
import org.jbox2d.dynamics.contacts.ContactPoint
import org.jbox2d.dynamics.contacts.ContactResult

object WorldListener {
    val attraction = true
    val seperation = false
    
    def cleanMap(map : Map[Agent, Set[Agent]], subscriber : Agent, publisher : Agent) {
        val result = map(subscriber) - publisher
        if(result.isEmpty){
            return map - subscriber
        }else{
            return map.updated(subscriber, result)
        }
    }
    
    def reverse(point : ContactPoint) : ContactPoint = {
        val result = new ContactPoint
        result.friction = point.friction
        result.normal.set(point.normal.mul(-1))
        result.position.set(point.position)
        result.restitution = point.restitution
        result.separation = point.separation
        result.shape1 = point.shape2
        result.shape2 = point.shape1
        result.velocity.set(point.velocity.mul(-1))
        return result
    }
}

class WorldListener extends ContactListener{
    var addMap : Map[Agent, Set[Agent]] = Map.empty[Agent, Set[Agent]]
    var adders : Set[Agent] = Set.empty[Agent]
    var persistMap : Map[Agent, Set[Agent]] = Map.empty[Agent, Set[Agent]]
    var persisters : Set[Agent] = Set.empty[Agent]

    def subscribe(subscriber : Agent, constant : Boolean = false) = if(constant) persisters = persisters + subscriber else adders = adders + subscriber
    
    override def add(point : ContactPoint) = contact(point, true)
    override def remove(point : ContactPoint) = contact(point, false)
    
    def contact(point : ContactPoint, seperation : Boolean) = {
        val reversePoint = WorldListener.reverse(point)
        
        if(adders contains point.shape1.getBody.getUserData.asInstanceOf[Agent]) {
            if(seperation) point.shape1.getBody.getUserData.asInstanceOf[Agent].handleSeparation(point) else point.shape1.getBody.getUserData.asInstanceOf[Agent].handleCollision(point)
        }
    
        if(adders contains point.shape2.getBody.getUserData.asInstanceOf[Agent]) {
            if(seperation) point.shape2.getBody.getUserData.asInstanceOf[Agent].handleSeparation(reversePoint) else point.shape2.getBody.getUserData.asInstanceOf[Agent].handleCollision(reversePoint)
        }
    }
        
    override def persist(point : ContactPoint) = {
        val reversePoint = WorldListener.reverse(point)
        
        try{
            val temp = persistMap(point.shape1.getBody.getUserData.asInstanceOf[Agent])
            if(temp.contains(point.shape2.getBody.getUserData.asInstanceOf[Agent]) || persisters.contains(point.shape1.getBody.getUserData.asInstanceOf[Agent])) {
                point.shape1.getBody.getUserData.asInstanceOf[Agent] handleCollision point
            }
        }catch{
            case e : Exception => {}
        }
        
        try{
            val temp = persistMap(point.shape2.getBody.getUserData.asInstanceOf[Agent])
            if(temp.contains(point.shape1.getBody.getUserData.asInstanceOf[Agent]) || persisters.contains(point.shape2.getBody.getUserData.asInstanceOf[Agent])) {
                point.shape2.getBody.getUserData.asInstanceOf[Agent] handleCollision point
            }
        }catch{
            case e : Exception => {}
        }
    }
    
    override def result(contact : ContactResult) = {}
}
