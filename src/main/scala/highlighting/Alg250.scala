package highlighting

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging

/**
 * Created by mattia on 28.08.15.
 */
object Alg250 extends LazyLogging{

  case class StatMethod(minIndex:Int, maxIndex:Int, children:List[StatMethod], instructions: List[PDFHighlightInstruction])

  def combine(myList: List[StatMethod]) : List[StatMethod] = {
    if(myList.length>= 2){
      val zipped1 = myList.zipWithIndex.filter(m => m._2 % 2 == 0)
      val zipped2 = myList.zipWithIndex.filter(m => m._2 % 2 == 1)

      val newList : List[StatMethod] =
        zipped1 zip zipped2 flatMap {
          case (left, right) => mergeIfMergeable(left._1, right._1)
        }

      if(newList.length>=2){
        newList.splitAt(newList.length-2)._1 ::: mergeIfMergeable(newList(newList.length-2), newList.last)
      }else {
        newList
      }
    }else {
      myList
    }
  }

  def mergeIfMergeable(method1: StatMethod, method2: StatMethod) : List[StatMethod] = {
    if(areMergeable(method1,method2)) {
      List[StatMethod](StatMethod(
        Math.min(method1.minIndex, method2.minIndex),
        Math.max(method1.maxIndex, method2.maxIndex),
        List[StatMethod](method1, method2) ::: method1.children ::: method2.children, method1.instructions ::: method2.instructions))
    }
    else {
      List[StatMethod](method1, method2)
    }
  }

  def areMergeable(method1: StatMethod, method2: StatMethod): Boolean = {
    method1.maxIndex > method2.minIndex
  }

  def iterateAlgorithm(stop: Boolean, mergedMethods: List[StatMethod]) : List[StatMethod] = {
    if(stop){
      mergedMethods
    }else {
      val tmpList = Alg250.combine(mergedMethods)
      iterateAlgorithm(tmpList equals mergedMethods, tmpList)
    }
  }

  def mergeMethods(methodsToMerge: List[StatMethod], method: String, permuter: PDFPermuter, methodAndSynonyms: StatisticalMethod, pdfName: String) : (List[StatMethod], List[PDFHighlightInstruction]) = {

    val mergedMethods = iterateAlgorithm(stop = false, methodsToMerge)

    logger.debug(s"Result after merging method: $method => ${mergedMethods.length} different groups for paper $pdfName.")

    if(mergedMethods.nonEmpty) {
      val assumptionsForMethod : List[String] = methodAndSynonyms.assumptions.flatMap(assumption => {
        List[String](assumption.name) ::: assumption.synonym
      })
      val assumptionsList = permuter.getUniqueStringsForSearchTerms(Map(Color.green -> assumptionsForMethod)).toList

      if(assumptionsList.nonEmpty) {
        logger.debug(s"There are: ${assumptionsList.length} different matches for the assumptions of method $method.")
        (mergedMethods, assumptionsList)
      }else {
        (List.empty[StatMethod], List.empty[PDFHighlightInstruction])
      }
    }else{
      (List.empty[StatMethod], List.empty[PDFHighlightInstruction])
    }
  }


}
