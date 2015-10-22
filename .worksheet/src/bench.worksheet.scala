package bench

import net.groups._
import net.groups.groups._
import net.groups.groups.Sn._
import net.groups.lib.MathHelpers._

import GroupImplicits._

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(262); 

	def printOrders(grp: Group[Int]) { grp.orderOf.toSeq.sortBy(_._1).foreach(println(_)) };System.out.println("""printOrders: (grp: net.groups.Group[Int])Unit""");$skip(361); 
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
    
    val G = Sn(3).cartesianProduct(Dn(2));System.out.println("""G  : net.groups.Group[(net.groups.groups.Sn.Cycle, net.groups.groups.DnElement)] = """ + $show(G ));$skip(16); val res$0 = 
    G.elts.size;System.out.println("""res0: Int = """ + $show(res$0));$skip(8); val res$1 = 
    2+2;System.out.println("""res1: Int(4) = """ + $show(res$1))}
    //G.subgroups
}
