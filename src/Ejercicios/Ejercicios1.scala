package Ejercicios

import scala.math.max

object Problem0 extends App {
    def drop[A](list: List[A], n: Int): List[A] = {
        @scala.annotation.tailrec
        def dropAux(aux: Int, list: List[A], acc: List[A]): List[A] =
            aux match {
                case 0 => acc
                case _ => list match {
                    case Nil => Nil
                    case head::tail => dropAux(aux-1, tail, head::acc)
                }
            }

        /* Solution */
        n match {
            case 0 => list
            case _ if n >= list.length => List()
            case _ if n < list.length => dropAux(n, list, List())
        }
    }

    println(drop(List(true, false), 0))
    println(drop(List(true, false, false, true), 2))
    println(drop(List(true, false, true, true, false, true), 3))
    println(drop(List(), 0))
    println(drop(List(true, false, true, true, false, true), 6))
    println(drop(List(), 2))
    println(drop(List(true, false, true, true, false, true), 8))
}

object Problem1 extends App {
    def occurrences[A](list: List[A], a: String, tailRec: Boolean): Int = {
        def occurrencesR(list: List[A], a: String): Int =
            list match {
                case Nil => 0
                case `a` :: tail => 1 + occurrencesR(tail, a)
                case _ :: tail => occurrencesR(tail, a)
            }
        @scala.annotation.tailrec
        def occurrencesTR(list: List[A], a: String, acc: Int): Int =
            list match {
                case Nil => acc
                case `a` :: tail => occurrencesTR(tail, a, acc + 1)
                case _ :: tail => occurrencesTR(tail, a, acc)
            }

        /* Solution */
        if (!tailRec) occurrencesR(list, a) else occurrencesTR(list, a, 0)
    }

    println("Part a: without Tail-recursion")
    println(occurrences(List("1","1","1"), "1", tailRec = false))
    println(occurrences(List("1","2","3"), "2", tailRec = false))
    println(occurrences(List(), "3", tailRec = false))
    println(occurrences(List("1","2","3"), "5", tailRec = false))

    println("Part b: Tail-recursion")
    println(occurrences(List("1","1","1"), "1", tailRec = true))
    println(occurrences(List("1","2","3"), "2", tailRec = true))
    println(occurrences(List(), "3", tailRec = true))
    println(occurrences(List("1","2","3"), "5", tailRec = true))
}

object Utils {
    def reverse[A](list: List[A]): List[A] = {
        @scala.annotation.tailrec
        def reverseAux(acc: List[A], list: List[A]): List[A] =
            list match {
                case Nil => acc
                case head :: tail =>
                    reverseAux(head :: acc, tail)
            }
        reverseAux(List(), list)
    }
}

object Problem2 extends App {
    def take[A](list: List[A], n: Int): List[A] = {
//        @scala.annotation.tailrec
//        def takeAux(_list: List[A], aux: Int, acc: List[A]) : List[A] =
//            aux match {
//                case 0 => acc
//                case _ => _list match {
//                    case Nil => Nil
//                    case head::tail => takeAux(tail, aux-1, head::acc)
//                }
//            }
//
//        /* Solution */
//        n match {
//            case 0 => List()
//            case _ if n > list.length => List()
//            case _ if n == list.length => list
//            case _ if n < list.length => Utils.reverse(takeAux(list, n, List()))
//        }
        list match {
            case head::tail if n > 0 => head::take(tail, n-1)
            case _ => List()
        }

    }

    println(take(List(), 0))
    println(take(List(), 5))
    println(take(List('1','2','3'), 0))
    println(take(List('1','2','3'), 2))
    println(take(List('1','2','3'), 3))
    println(take(List('1','2','3'), 10))
}

object Problem3 extends App {

    def partitionEvenOdd[int](list: List[Int]): (List[Int], List[Int]) =
        list match {
            case Nil => (Nil, Nil)
            case head::tail if head % 2 == 0 =>
                (partitionEvenOdd(tail)._1, head::partitionEvenOdd(tail)._2)
            case head::tail =>
                (head::partitionEvenOdd(tail)._1, partitionEvenOdd(tail)._2)
        }

    println(
        partitionEvenOdd(List()) + "\n" +
        partitionEvenOdd(List(1,3,5)) + "\n" +
        partitionEvenOdd(List(0,2,4,6)) + "\n" +
        partitionEvenOdd(List(1,2,3,4,5))
    )
}

object Problem4 extends App {
    def sum(list: List[(Int, Int)]): List[Int] =
        list match {
            case Nil => Nil
            case head::tail => (head._1 + head._2)::sum(tail)
        }

    println(
        sum(List()) + "\n" +
        sum(List((0,0))) + "\n" +
        sum(List((1,2), (3,4), (5,6)))
    )
}

object Problem5 extends App {
    def zip[A, B](list1: List[A], list2: List[B]): List[(A, B)] =
        (list1, list2) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (head1::tail1, head2::tail2)
                => (head1, head2)::zip(tail1, tail2)
        }

    println(
        zip(List(), List()) + "\n" +
        zip(List(), List('a','b')) + "\n" +
        zip(List(1,2,3), List()) + "\n" +
        zip(List(1,2,3), List('a','b','c')) + "\n" +
        zip(List(1,2), List('a','b','c')) + "\n" +
        zip(List(1,2,3), List('a','b'))
    )

}

object Problem6 extends App {

    def greatest(list: List[Int], tailRec: Boolean): Option[Int] = {
        def greatestR(_list: List[Int]): Option[Int] =
            _list match {
                case Nil => None
                case head::tail => Some(max(head, greatestR(tail).getOrElse(0)))
            }

        @scala.annotation.tailrec
        def greatestTR(_list: List[Int]): Option[Int] =
            _list match {
                case Nil => None
                case List(x : Int) => Some(x)
                case fst::snd::rest => greatestTR(max(fst, snd):: rest)
            }

        if (!tailRec) greatestR(list) else greatestTR(list)
    }

    println(
        "Part a: without Tail-recursion"+ "\n" +
        greatest(List(), tailRec = false) + "\n" +
        greatest(List(1,2,3), tailRec = false) + "\n" +
        greatest(List(3,2,1), tailRec = false) + "\n" +
        greatest(List(1), tailRec = false)
    )
    println(
        "Part b: Tail-recursion" + "\n" +
        greatest(List(), tailRec = true) + "\n" +
        greatest(List(1,2,3), tailRec = true) + "\n" +
        greatest(List(3,2,1), tailRec = true) + "\n" +
        greatest(List(1), tailRec = true)
    )
}

object Problem7 extends App {
    def conc(list : List[Either[String, Int]]): String =
        list match {
            case Nil => ""
            case head::tail => head.left.getOrElse("") + conc(tail)
        }

    println(
        conc(List()) + "\n" +
        conc(List(Right(1), Right(2), Right(3))) + "\n" +
        conc(List(Left("hello"), Left(", "), Left("world!"))) + "\n" +
        conc(List(Right(1), Left("hello"), Right(2), Left(", "), Left("world!"), Right(5)))
    )
}

// =========================  Trees  ================================
sealed abstract class Tree[A]
case class Empty[A]() extends Tree[A]
case class Node[A](left: Tree[A], root: A, right: Tree[A]) extends Tree[A]
object Tree{
    def void[A]: Tree[A] = Empty()
    def leaf[A](a: A): Node[A] = Node(Empty(), a, Empty())
    def right[A](a: A, tree: Tree[A]): Node[A] = Node(Empty(), a, tree)
    def left[A](tree: Tree[A], a: A): Node[A] = Node(tree, a, Empty())
    def node[A](left: Tree[A], a: A, right: Tree[A]): Node[A] = Node(left, a, right)
}

import Tree._

object Problem8 extends App {

    def numNodes[A](tree: Tree[A]): Int =
        tree match {
            case Empty() => 0
            case Node(left, _, right) => 1 + numNodes(left) + numNodes(right)
        }

    println(
        numNodes(void) + "\n" +
        numNodes(leaf(1)) + "\n" +
        numNodes(left(leaf(1), 2)) + "\n" +
        numNodes(node(leaf(1), 2, leaf(3)))
    )
}

object Problem9 extends App {

    def height[A](tree: Node[A]): Int = {
        def loop(_tree: Tree[A]): Int =
            _tree match {
                case Empty() => 0
                case Node(left, _, right) => 1 + max(loop(left), loop(right))
            }
        loop(tree)-1
    }


    println(
        height(leaf(1)) + "\n" +
        height(left(leaf(1),2)) + "\n" +
        height(node(leaf(1), 2, leaf(3))) + "\n" +
        height(left(left(leaf(3),2),1))
    )
}

object Problem10 extends App {

    @scala.annotation.tailrec
    def isDegenerate[A](tree: Tree[A]): Boolean =
        tree match {
            case Empty() => true
            case Node(Empty(), _, right) => isDegenerate(right)
            case Node(left, _, Empty()) => isDegenerate(left)
            case _ => false
        }

    println(
        isDegenerate(void) + "\n" +
        isDegenerate(leaf(1)) + "\n" +
        isDegenerate(left(leaf(1), 2)) + "\n" +
        isDegenerate(right(2, leaf(1)))  + "\n" +
        isDegenerate(node(leaf(1), 2, leaf(3))) + "\n" +
        isDegenerate(left(left(leaf(3), 2), 1)) + "\n" +
        isDegenerate(left(right(2, left(right(4, leaf(5)), 3)),1)) + "\n" +
        isDegenerate(left(node(leaf(3), 2, leaf(3)), 1))
    )

}

object Problem11 extends App {

    def leaves[A](tree: Tree[A]): List[A] =
        tree match {
            case Empty() => List()
            case Node(Empty(), root, Empty()) => List(root)
            case Node(left, _, right) => leaves(left) ++ leaves(right)
        }

    println(
        leaves(void) + "\n" +
        leaves(leaf(1)) + "\n" +
        leaves(left(leaf(1), 2)) + "\n" +
        leaves(right(2, leaf(1)))  + "\n" +
        leaves(node(leaf(1), 2, leaf(3))) + "\n" +
        leaves(left(left(leaf(3), 2), 1)) + "\n" +
        leaves(left(right(2, left(right(4, leaf(5)), 3)),1)) + "\n" +
        leaves(left(node(leaf(3), 2, leaf(3)), 1)) + "\n" +
        leaves(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7))))
    )

}

object Problem12 extends App {

    def preorder[A](tree: Tree[A]): List[A] =
        tree match {
            case Empty() => List()
            case Node(left, root, right) => List(root) ++ preorder(left) ++ preorder(right)
        }

    println("Part a) Write a function that creates the pre-order of a binary tree.")
    println(
        preorder(void) + " shouldBe List()\n" +
        preorder(leaf(1)) + " shouldBe List(1)\n" +
        preorder(left(leaf(1), 2)) + " shouldBe List(2,1)\n"+
        preorder(right(2, leaf(1))) + " shouldBe List(2,1)\n"+
        preorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(2,1,3)\n"+
        preorder(left(left(leaf(3), 2), 1)) + " shouldBe List(1,2,3)\n"+
        preorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(1,2,3,4,5)\n"+
        preorder(left(node(leaf(3), 2, leaf(4)), 1)) + " shouldBe List(1,2,3,4)\n"+
        preorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7))))+ " shouldBe List(4,2,1,3,6,5,7)"
    )

    def inorder[A](tree: Tree[A]): List[A] =
        tree match {
            case Empty() => List()
            case Node(left, root, right) => inorder(left) ++ List(root) ++ inorder(right)
        }

    println("Part b) Write a function that returns the in-order of a binary tree.")
    println(
        inorder(void) + " shouldBe List()\n" +
        inorder(leaf(1)) + " shouldBe List(1)\n" +
        inorder(left(leaf(1), 2)) + " shouldBe List(1,2)\n" +
        inorder(right(2, leaf(1)))  + " shouldBe List(2,1)\n" +
        inorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(1,2,3)\n" +
        inorder(left(left(leaf(3), 2), 1)) + " shouldBe List(3,2,1)\n" +
        inorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(2,4,5,3,1)\n" +
        inorder(left(node(leaf(3), 2, leaf(4)), 1))+ " shouldBe List(3,2,4,1)\n" +
        inorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7)))) + " shouldBe List(1,2,3,4,5,6,7)"
    )

    def postorder[A](tree: Tree[A]): List[A] =
        tree match {
            case Empty() => List()
            case Node(left, root, right) => postorder(left) ++ postorder(right) ++ List(root)
        }

    println("Part c) Write a function that returns post-order of a binary tree")
    println(
        postorder(void) + " shouldBe List()\n" +
        postorder(leaf(1)) + " shouldBe List(1)\n" +
        postorder(left(leaf(1), 2))+ " shouldBe List(1,2)\n" +
        postorder(right(2, leaf(1)))  + " shouldBe List(1,2)\n" +
        postorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(1,3,2)\n" +
        postorder(left(left(leaf(3), 2), 1)) + " shouldBe List(3,2,1)\n" +
        postorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(5,4,3,2,1)\n" +
        postorder(left(node(leaf(3), 2, leaf(4)), 1)) + " shouldBe List(3,4,2,1)\n" +
        postorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7)))) + " shouldBe List(1,3,2,5,7,6,4)"
    )
}

object Problem13 extends App {

    def sum(tree: Tree[(Int, Int)]): Tree[Int] =
        tree match {
            case Empty() => void
            case Node(Empty(), a, Empty()) => leaf(a._1 + a._2)
            case Node(l, a, Empty()) => left(sum(l), a._1 + a._2)
            case Node(Empty(), a, r) => right(a._1 + a._2, sum(r))
            case Node(left, a, right) => node(sum(left), a._1 + a._2, sum(right))
        }

    println(
        sum(void) +" shouldBe void\n"+
        sum(leaf((1,1))) +" shouldBe leaf(2)\n"+
        sum(left(leaf((1,3)), (2,5))) +" shouldBe left(leaf(4), 7)\n"+
        sum(right((0,2), leaf((-1,2)))) +" shouldBe right(2, leaf(1))\n"+
        sum(left(left(leaf((-3,6)), (2,0)), (-5,6))) +" shouldBe left(left(leaf(3), 2), 1)"
    )
}