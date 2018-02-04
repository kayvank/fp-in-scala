package ch3.datastructure

import org.specs2._
import ch3.datastructure.Tree

class DatastructureSpec extends Specification { def is = s2"""
 Chapter 3 Tree specifications
  Tree EX  3.25 size imp $e1
  Tree EX  3.26 max imp $e2
  Tree EX  3.27 depth imp $e3
"""

  def e1 = Tree.size(
    Branch(left=Leaf(1),
           right= Branch(left=Leaf(1),
                         right= Branch(
                           left=Leaf(2), right=Leaf(6)))))  must_== 4

  def e2 = Tree.maximum(
    Branch(left=Leaf(1),
           right= Branch(left=Leaf(1),
                         right= Branch(
                           left=Leaf(2), right=Leaf(6)))))  must_== 6

  def e3 = Tree.depth(
    Branch(
      left=Leaf(1), right=
        Branch(left=Leaf(1), right=
                 Branch(left=Leaf(2), right=
                          Branch(
                            left=Leaf(3), right=Leaf(6))))))  must_== 5
}
