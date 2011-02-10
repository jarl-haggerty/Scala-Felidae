/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae.physics;

import org.jbox2d.collision.shapes.Shape;
import org.jbox2d.dynamics.ContactFilter;
import org.jbox2d.dynamics.DefaultContactFilter;

import felidae.agents.Agent;

class WorldFilter extends ContactFilter {
    var verifyingCollisions : Set[Agent] = Set.empty[Agent]
    var verifyingRaycasts : Set[Agent] = Set.empty[Agent]
    
    def verifyCollisions(verifyer : Agent) = verifyingCollisions = verifyingCollisions + verifyer
    def dontVerifyCollisions(verifyer : Agent) = verifyingCollisions = verifyingCollisions - verifyer
    def verifyRaycasts(verifyer : Agent) = verifyingCollisions = verifyingCollisions + verifyer
    def dontVerifyRaycasts(verifyer : Agent) = verifyingCollisions = verifyingCollisions - verifyer

    override def shouldCollide(shape1 : Shape, shape2 : Shape) : Boolean = {
        var result = true
        if(verifyingCollisions contains shape1.getBody.getUserData.asInstanceOf[Agent] ){
            result = result && shape1.getBody.getUserData.asInstanceOf[Agent].verifyCollision(shape2)
        }
        if(verifyingCollisions contains shape2.getBody.getUserData.asInstanceOf[Agent] ){
            result = result && shape2.getBody.getUserData.asInstanceOf[Agent].verifyCollision(shape1)
        }
//        if(filteringCollisions.contains((Actor)shape2.getBody().getUserData())){
//           result &= ((Actor) shape2.getBody().getUserData()).filterCollision(shape1);
//        }
        return result
    }

    def rayCollide(userData : Object, shape : Shape) : Boolean = {
        var result = true
        if(verifyingRaycasts contains userData.asInstanceOf[Agent]){
            result = result && userData.asInstanceOf[Agent].verifyRaycast(shape.getBody.getUserData.asInstanceOf[Agent])
        }
//        if(filteringRaycasts.contains((Actor)shape.getBody().getUserData())){
//            result &= ((Actor)shape.getBody().getUserData()).filterRaycast((Actor)userData);
//        }
        return result
    }
}
