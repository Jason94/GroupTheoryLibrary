package bench

import net.groups._
import net.groups.groups._
import net.groups.groups.Sn._
import net.groups.lib.MathHelpers._

import GroupImplicits._

object worksheet {

	def printOrders(grp: Group[Int]) { grp.orderOf.toSeq.sortBy(_._1).foreach(println(_)) }
                                                  //> printOrders: (grp: net.groups.Group[Int])Unit
    /*
		val z12 = Zn(12)
    val g3 = z12.cylcicSubgroupOf(3)
    z12.leftCosets(g3) == z12.rightCosets(g3)

		implicit val dSpace = 5
		val d5 = Dn(dSpace)
    val x2y = DnElement(2,1)
    val gx2y = d5.cylcicSubgroupOf(x2y)
    d5.leftCosets(gx2y)
    d5.rightCosets(gx2y)
    (1 to 5).permutations.size
    */
    
    val G = Sn(3).cartesianProduct(Dn(2))
    G.elts.size
    2+2
    //G.subgroups
}