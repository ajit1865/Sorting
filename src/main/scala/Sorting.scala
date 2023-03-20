object Sorting extends App{
  def selectionSort(unsort: List[Int]): List[Int] ={
    def sort(unsort: List[Int], sort1: List[Int]): List[Int] = {
      if(unsort.isEmpty) sort1
      else {
        val pass = unsort.min
        val newList = unsort.filterNot(_ == pass)
        sort(newList , sort1 :+ pass)
      }
    }
    sort(unsort, List.empty[Int])
  }

//  val first = List(5,4,3,2,8,1)
//    println(selectionSort(first))

  def insertionSort(unsortList: List[Int]): List[Int] ={
    def insertion(sortedList: List[Int],unsorted: List[Int] ): List[Int]={
      if(unsorted.isEmpty) sortedList
      else{
        val min = unsorted.head
        val (left, right)= sortedList.span(_ <= min)
        insertion(left ::: min :: right , unsorted.tail)
      }
    }
    insertion( List.empty[Int], unsortList)
  }

  val first = List(5, 4, 3, 2, 8, 1)
  println(insertionSort(first))
}
