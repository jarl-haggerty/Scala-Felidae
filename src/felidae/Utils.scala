/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package felidae

import scala.collection.immutable.HashMap
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML.loadString
import scala.xml.Elem

object Utils {
    def parseCode(root : Node) : Map[String, String] = {
        var mappings : List[(String, String)] = Nil
        for(a <- root.child if a.label == "Property"){
            mappings ::= (a \ "@Key" text) -> (a \ "@Value" text)
        }
        Map.empty[String, String] ++ mappings
    }

    def parseCode(lines : Iterator[String]) : Map[String, String] = {
        var mappings : List[(String, String)] = Nil
        for(a <- lines){
            val temp = a.split("=")
            (temp(0), temp(1)) :: mappings
        }
        Map.empty[String, String] ++ mappings
    }
    
    def packageCode(code : Map[String, String], indentation : String) : NodeSeq = {
        var bundle : List[NodeSeq] = Nil
        for((key, value) <- code) {
            bundle ::=
<Temp>{indentation}<Property Key={key} Value={value}/>
</Temp>.child
        }
<Temp>{bundle}</Temp>.child
    }

    def implies(x : Boolean, y : Boolean) = !x || y
    def validKey(map : Map[String, String], key : String) = map.contains(key) && map(key).nonEmpty
}
