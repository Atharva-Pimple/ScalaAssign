package Comparator_Search_Sort

import scala.annotation.tailrec

object ComparatorProblem extends App {

  // b. Define a generic Comparator class Comparator<T, T> with one method - "compare(T o1, T o2)" that returns -1, 0 and 1 depending on whether o1 is less than, equal to or greater than o2.
  //b-> type class
  trait Comparator[T] {
    def compare(o1: T, o2: T): Int
  }
  //b-> type class Instances
  object Comparator{
    implicit val intComparator: Comparator[Int]= new Comparator[Int] {

      override def compare(o1: Int, o2: Int): Int = o1.compareTo(o2)
    }

    implicit val stringComparator: Comparator[String]= new Comparator[String] {
      override def compare(o1: String, o2: String): Int = o1.compareTo(o2)
    }

    implicit val longComparator: Comparator[Long]= new Comparator[Long] {
      override def compare(o1: Long, o2: Long): Int = o1.compareTo(o2)
    }

    implicit val floatComparator: Comparator[Float]= new Comparator[Float] {
      override def compare(o1: Float, o2: Float): Int = o1.compareTo(o2)
    }
  }

  implicit class RichList[T](l: List[T]) {

    //a. Implement a method "l.size()" that returns the size of the list using Recursion (don't use built in length method)
    def cSize(): Int={
      def loop(li: List[T], acc: Int): Int={
        if(li.isEmpty) acc
        else loop(li.tail,acc+1)
      }

      loop(l,0)
    }

    /*
      c. Implement a method named "l.sort(Comparator<T, T> comparator)" that returns a sorted list
     based on the passed in comparator. Do not use any built in sort method of the list class or
     any SDK sorting method. You may implement bubble sort or selection sort.
     */
    def sort(implicit comparator: Comparator[T]): List[T]={
      def findMin(li: List[T]): (T, List[T])={
        if (li.tail.isEmpty) (li.head,li.tail)
        else{
          val (min,rest)=findMin(li.tail)
          if(comparator.compare(li.head,min) <=0) (li.head,li.tail)
          else (min,li.head :: rest)
        }
      }

      @tailrec
      def selectionSort(li: List[T],acc: List[T]): List[T]={
        if (li.isEmpty) acc.reverse
        else{
          val (min,rest)= findMin(li)
          selectionSort(rest,min :: acc)
        }
      }

      selectionSort(l,List())
    }

    /*
      d. Implement a method named "l.binarySearch(Comparator<T, T> comparator, T)" to perform a
      binary search within the list. This method will return an Option[int] which will have a
      value of the value of the index where the element is found or None in case it is not found.
       Do not use any builtin search method.
     */
    def binarySearch(key: T)(implicit comparator: Comparator[T]): Option[Int]={
      def loop(start: Int, end: Int): Option[Int]={
        if (start>end) None
        else{
          val mid=start + (end-start)/2
          val midValue = l(mid)
          comparator.compare(key,midValue) match {
            case 0 => Some(mid)
            case -1 => loop(start,mid-1)
            case 1 => loop(mid+1,end)
          }
        }
      }

      loop(0,l.cSize()-1)
    }

  }
  val l=List(1,9,4,7,5,2)

  println(l.cSize())
  println(l.sort)
  println(l.sort.binarySearch(7))


}
